//! Code Generator Module
//!
//! This module is responsible for generating LLVM IR from the Mux AST.
//! It implements Phase 1 and Phase 2 of the implementation plan:
//! - Type system infrastructure with concrete LLVM types
//! - Expression and statement compilation
//!
//! Design Principles:
//! 1. Static First: All type decisions made at compile time
//! 2. Zero-Cost Abstractions: Generics compile to efficient concrete code
//! 3. Memory Safety: Use type system to prevent runtime errors
//! 4. Performance: Concrete types eliminate boxing overhead

use crate::parser::{
    AstNode, BinaryOp, EnumVariant, ExpressionKind, ExpressionNode, FunctionNode, LiteralNode,
    Param, PrimitiveType, StatementKind, StatementNode, TypeKind, TypeNode, UnaryOp,
};
use crate::semantics::{SemanticAnalyzer, Symbol, SymbolKind, Type};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::FloatPredicate;
use std::collections::HashMap;
use std::path::Path;

/// Error type for code generation failures
#[derive(Debug)]
pub struct CodeGenError {
    pub message: String,
}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CodeGen Error: {}", self.message)
    }
}

impl std::error::Error for CodeGenError {}

impl CodeGenError {
    pub fn new(message: impl Into<String>) -> Self {
        CodeGenError {
            message: message.into(),
        }
    }
}

type CodeGenResult<T> = Result<T, CodeGenError>;

/// Stores information about a compiled function
#[derive(Clone)]
struct CompiledFunction<'ctx> {
    function: FunctionValue<'ctx>,
    return_type: Type,
}

/// Stores information about a variable in scope
#[derive(Clone)]
struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    type_: Type,
}

/// The main code generator struct
pub struct CodeGenerator<'a, 'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    analyzer: &'a mut SemanticAnalyzer,

    /// Symbol table mapping variable names to their LLVM pointers and types
    variables: HashMap<String, Variable<'ctx>>,

    /// Function table mapping function names to their LLVM function values
    functions: HashMap<String, CompiledFunction<'ctx>>,

    /// Runtime function declarations (external functions from mux_runtime)
    runtime_functions: HashMap<String, FunctionValue<'ctx>>,

    /// The current function being compiled
    current_function: Option<FunctionValue<'ctx>>,

    /// Struct type cache for user-defined types
    struct_types: HashMap<String, StructType<'ctx>>,

    /// Enum type cache - stores (struct_type, variant_info)
    /// where variant_info maps variant name to (discriminant, field_types)
    enum_types: HashMap<String, EnumTypeInfo<'ctx>>,

    /// String constant cache
    string_constants: HashMap<String, PointerValue<'ctx>>,

    /// Counter for generating unique names
    unique_counter: usize,

    /// Loop context stack for break/continue statements
    /// Each entry contains (continue_block, break_block)
    loop_stack: Vec<(inkwell::basic_block::BasicBlock<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)>,
}

/// Information about a compiled enum type
#[derive(Clone)]
struct EnumTypeInfo<'ctx> {
    /// The LLVM struct type representing this enum (discriminant + max-size payload)
    struct_type: StructType<'ctx>,
    /// Map from variant name to (discriminant_value, optional field types)
    variants: HashMap<String, (u32, Vec<Type>)>,
}

impl<'a, 'ctx> CodeGenerator<'a, 'ctx> {
    /// Create a new CodeGenerator
    pub fn new(context: &'ctx Context, analyzer: &'a mut SemanticAnalyzer) -> Self {
        let module = context.create_module("mux_program");
        let builder = context.create_builder();

        let mut codegen = CodeGenerator {
            context,
            module,
            builder,
            analyzer,
            variables: HashMap::new(),
            functions: HashMap::new(),
            runtime_functions: HashMap::new(),
            current_function: None,
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            string_constants: HashMap::new(),
            unique_counter: 0,
            loop_stack: Vec::new(),
        };

        codegen.declare_runtime_functions();
        codegen
    }

    /// Generate a unique name for temporaries
    fn unique_name(&mut self, prefix: &str) -> String {
        self.unique_counter += 1;
        format!("{}_{}", prefix, self.unique_counter)
    }

    // ========================================================================
    // Phase 1.1: Type System Infrastructure
    // ========================================================================

    /// Convert a Mux Type to an LLVM BasicTypeEnum
    fn type_to_llvm(&self, type_: &Type) -> CodeGenResult<BasicTypeEnum<'ctx>> {
        match type_ {
            Type::Primitive(prim) => self.primitive_to_llvm(prim),
            Type::Void => {
                // Void is not a basic type, but we can use i8 as a placeholder
                Ok(self.context.i8_type().into())
            }
            Type::Reference(inner) => {
                // References are represented as pointers
                let _inner_type = self.type_to_llvm(inner)?;
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::List(_) => {
                // Lists are represented as opaque pointers to runtime structs
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Map(_, _) => {
                // Maps are represented as opaque pointers to runtime structs
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Set(_) => {
                // Sets are represented as opaque pointers to runtime structs
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Optional(_) => {
                // Optionals are represented as tagged unions (discriminant + value)
                // For now, use a pointer type
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Named(name, _) => {
                // User-defined types (classes, enums) are represented as pointers to structs
                if let Some(_struct_type) = self.struct_types.get(name) {
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                } else {
                    // If not yet defined, use opaque pointer
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                }
            }
            Type::Function { .. } => {
                // Function types are represented as function pointers
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Tuple(elements) => {
                // Tuples are represented as anonymous structs
                let field_types: Vec<BasicTypeEnum> = elements
                    .iter()
                    .map(|t| self.type_to_llvm(t))
                    .collect::<CodeGenResult<Vec<_>>>()?;
                let struct_type = self.context.struct_type(&field_types, false);
                Ok(struct_type.into())
            }
            Type::Variable(_) | Type::Generic(_) | Type::Instantiated(_, _) => {
                // Generic types should be monomorphized before code generation
                Err(CodeGenError::new(
                    "Generic types should be monomorphized before code generation",
                ))
            }
            Type::EmptyList | Type::EmptyMap | Type::EmptySet => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Never => {
                // Never type is used for functions that don't return
                Ok(self.context.i8_type().into())
            }
        }
    }

    /// Convert a primitive type to LLVM type
    fn primitive_to_llvm(&self, prim: &PrimitiveType) -> CodeGenResult<BasicTypeEnum<'ctx>> {
        match prim {
            PrimitiveType::Int => Ok(self.context.i64_type().into()),
            PrimitiveType::Float => Ok(self.context.f64_type().into()),
            PrimitiveType::Bool => Ok(self.context.bool_type().into()),
            PrimitiveType::Char => Ok(self.context.i32_type().into()),
            PrimitiveType::Str => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            PrimitiveType::Void => Ok(self.context.i8_type().into()),
            PrimitiveType::Auto => Err(CodeGenError::new(
                "Auto type should be resolved before code generation",
            )),
        }
    }

    /// Convert a TypeNode from the AST to a Type
    fn resolve_type_node(&self, type_node: &TypeNode) -> CodeGenResult<Type> {
        match &type_node.kind {
            TypeKind::Primitive(prim) => Ok(Type::Primitive(prim.clone())),
            TypeKind::Named(name, args) => {
                let resolved_args: Vec<Type> = args
                    .iter()
                    .map(|arg| self.resolve_type_node(arg))
                    .collect::<CodeGenResult<Vec<_>>>()?;
                Ok(Type::Named(name.clone(), resolved_args))
            }
            TypeKind::List(inner) => {
                let inner_type = self.resolve_type_node(inner)?;
                Ok(Type::List(Box::new(inner_type)))
            }
            TypeKind::Map(key, value) => {
                let key_type = self.resolve_type_node(key)?;
                let value_type = self.resolve_type_node(value)?;
                Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
            }
            TypeKind::Set(inner) => {
                let inner_type = self.resolve_type_node(inner)?;
                Ok(Type::Set(Box::new(inner_type)))
            }
            TypeKind::Tuple(elements) => {
                let element_types: Vec<Type> = elements
                    .iter()
                    .map(|e| self.resolve_type_node(e))
                    .collect::<CodeGenResult<Vec<_>>>()?;
                Ok(Type::Tuple(element_types))
            }
            TypeKind::Reference(inner) => {
                let inner_type = self.resolve_type_node(inner)?;
                Ok(Type::Reference(Box::new(inner_type)))
            }
            TypeKind::Function { params, returns } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| self.resolve_type_node(p))
                    .collect::<CodeGenResult<Vec<_>>>()?;
                let return_type = self.resolve_type_node(returns)?;
                Ok(Type::Function {
                    params: param_types,
                    returns: Box::new(return_type),
                })
            }
            TypeKind::TraitObject(_) => Err(CodeGenError::new("Trait objects not yet supported")),
            TypeKind::Auto => Ok(Type::Primitive(PrimitiveType::Auto)),
        }
    }

    // ========================================================================
    // Phase 1.2: Runtime Function Declarations
    // ========================================================================

    /// Declare external functions from the mux_runtime library
    fn declare_runtime_functions(&mut self) {
        // String functions
        self.declare_runtime_fn("mux_string_concat", &["ptr", "ptr"], "ptr");
        self.declare_runtime_fn("mux_string_length", &["ptr"], "i64");
        self.declare_runtime_fn("mux_string_from_cstr", &["ptr"], "ptr");
        self.declare_runtime_fn("mux_string_to_cstr", &["ptr"], "ptr");
        self.declare_runtime_fn("mux_string_equals", &["ptr", "ptr"], "i1");

        // Type conversion functions
        self.declare_runtime_fn("mux_int_to_string", &["i64"], "ptr");
        self.declare_runtime_fn("mux_float_to_string", &["f64"], "ptr");
        self.declare_runtime_fn("mux_bool_to_string", &["i1"], "ptr");
        self.declare_runtime_fn("mux_float_to_int", &["f64"], "i64");
        self.declare_runtime_fn("mux_int_to_float", &["i64"], "f64");

        // I/O functions
        self.declare_runtime_fn("mux_print", &["ptr"], "void");
        self.declare_runtime_fn("mux_println", &["ptr"], "void");
        self.declare_runtime_fn("mux_read_line", &[], "ptr");

        // List functions
        self.declare_runtime_fn("mux_new_list", &[], "ptr");
        self.declare_runtime_fn("mux_list_push", &["ptr", "ptr"], "void");
        self.declare_runtime_fn("mux_list_get", &["ptr", "i64"], "ptr");
        self.declare_runtime_fn("mux_list_length", &["ptr"], "i64");

        // Map functions
        self.declare_runtime_fn("mux_new_map", &[], "ptr");
        self.declare_runtime_fn("mux_map_put", &["ptr", "ptr", "ptr"], "void");
        self.declare_runtime_fn("mux_map_get", &["ptr", "ptr"], "ptr");

        // Set functions
        self.declare_runtime_fn("mux_new_set", &[], "ptr");
        self.declare_runtime_fn("mux_set_add", &["ptr", "ptr"], "void");
        self.declare_runtime_fn("mux_set_contains", &["ptr", "ptr"], "i1");

        // Object/class functions
        self.declare_runtime_fn("mux_alloc_object", &["i64"], "ptr");
        self.declare_runtime_fn("mux_free_object", &["ptr"], "void");

        // Range function
        self.declare_runtime_fn("mux_range", &["i64", "i64"], "ptr");
    }

    /// Helper to declare a runtime function
    fn declare_runtime_fn(&mut self, name: &str, param_types: &[&str], return_type: &str) {
        let params: Vec<BasicMetadataTypeEnum> = param_types
            .iter()
            .map(|t| self.str_to_llvm_type(t).into())
            .collect();

        let fn_type: FunctionType = match return_type {
            "void" => self.context.void_type().fn_type(&params, false),
            _ => self.str_to_llvm_type(return_type).fn_type(&params, false),
        };

        let function = self.module.add_function(name, fn_type, None);
        self.runtime_functions.insert(name.to_string(), function);
    }

    /// Convert a string type name to LLVM type
    fn str_to_llvm_type(&self, type_str: &str) -> BasicTypeEnum<'ctx> {
        match type_str {
            "i64" => self.context.i64_type().into(),
            "i32" => self.context.i32_type().into(),
            "i8" => self.context.i8_type().into(),
            "i1" => self.context.bool_type().into(),
            "f64" => self.context.f64_type().into(),
            "ptr" => self.context.ptr_type(AddressSpace::default()).into(),
            _ => self.context.i64_type().into(), // Default to i64
        }
    }

    // ========================================================================
    // Phase 2.1: Code Generation Entry Point
    // ========================================================================

    /// Generate LLVM IR for the entire program
    pub fn generate(&mut self, ast: &[AstNode]) -> CodeGenResult<()> {
        // First pass: collect all function, class, and enum declarations
        for node in ast {
            match node {
                AstNode::Function(func) => {
                    self.declare_function(func)?;
                }
                AstNode::Class { name, fields, .. } => {
                    self.declare_class(name, fields)?;
                }
                AstNode::Enum { name, variants, .. } => {
                    self.declare_enum(name, variants)?;
                }
                _ => {}
            }
        }

        // Second pass: generate code for all top-level constructs
        // Collect statements for main function
        let mut main_statements = Vec::new();

        for node in ast {
            match node {
                AstNode::Function(func) => {
                    self.compile_function(func)?;
                }
                AstNode::Class { methods, name, .. } => {
                    self.compile_class_methods(name, methods)?;
                }
                AstNode::Enum { name, variants, .. } => {
                    self.compile_enum_constructors(name, variants)?;
                }
                AstNode::Statement(stmt) => {
                    main_statements.push(stmt.clone());
                }
                AstNode::Interface { .. } => {
                    // Interfaces are handled at the type level
                }
            }
        }

        // Generate main function with all top-level statements
        if !main_statements.is_empty() {
            self.generate_main(&main_statements)?;
        } else {
            // Generate an empty main if there are no top-level statements
            self.generate_empty_main()?;
        }

        // Verify the module
        if let Err(msg) = self.module.verify() {
            return Err(CodeGenError::new(format!(
                "Module verification failed: {}",
                msg.to_string()
            )));
        }

        Ok(())
    }

    /// Emit the generated IR to a file
    pub fn emit_ir_to_file(&self, path: &str) -> CodeGenResult<()> {
        self.module
            .print_to_file(Path::new(path))
            .map_err(|e| CodeGenError::new(format!("Failed to write IR file: {}", e)))
    }

    // ========================================================================
    // Phase 2.2: Function Compilation
    // ========================================================================

    /// Declare a function (create the function signature without body)
    fn declare_function(&mut self, func: &FunctionNode) -> CodeGenResult<FunctionValue<'ctx>> {
        let return_type = self.resolve_type_node(&func.return_type)?;
        let params: Vec<(String, Type)> = func
            .params
            .iter()
            .map(|p| {
                let type_ = self.resolve_type_node(&p.type_)?;
                Ok((p.name.clone(), type_))
            })
            .collect::<CodeGenResult<Vec<_>>>()?;

        let param_types: Vec<BasicMetadataTypeEnum> = params
            .iter()
            .map(|(_, t)| self.type_to_llvm(t).map(|ty| ty.into()))
            .collect::<CodeGenResult<Vec<_>>>()?;

        let fn_type = match &return_type {
            Type::Void => self.context.void_type().fn_type(&param_types, false),
            _ => {
                let ret_llvm = self.type_to_llvm(&return_type)?;
                ret_llvm.fn_type(&param_types, false)
            }
        };

        let function = self.module.add_function(&func.name, fn_type, None);

        self.functions.insert(
            func.name.clone(),
            CompiledFunction {
                function,
                return_type: return_type.clone(),
            },
        );

        Ok(function)
    }

    /// Compile a function body
    fn compile_function(&mut self, func: &FunctionNode) -> CodeGenResult<()> {
        let compiled = self
            .functions
            .get(&func.name)
            .cloned()
            .ok_or_else(|| CodeGenError::new(format!("Function {} not declared", func.name)))?;

        let function = compiled.function;
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Save the previous function context
        let prev_function = self.current_function;
        self.current_function = Some(function);

        // Save previous variables and create new scope
        let prev_variables = self.variables.clone();
        self.variables.clear();

        // Allocate space for parameters and store them
        for (i, param) in func.params.iter().enumerate() {
            let param_type = self.resolve_type_node(&param.type_)?;
            let llvm_type = self.type_to_llvm(&param_type)?;
            let alloca = self.builder.build_alloca(llvm_type, &param.name)
                .map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;

            let param_value = function.get_nth_param(i as u32).ok_or_else(|| {
                CodeGenError::new(format!("Could not get parameter {} for function {}", i, func.name))
            })?;

            self.builder.build_store(alloca, param_value)
                .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

            self.variables.insert(
                param.name.clone(),
                Variable {
                    ptr: alloca,
                    type_: param_type,
                },
            );
        }

        // Compile the function body
        let mut has_terminator = false;
        for stmt in &func.body {
            self.compile_statement(stmt)?;
            // Check if current block has a terminator
            if self.builder.get_insert_block().unwrap().get_terminator().is_some() {
                has_terminator = true;
                break;
            }
        }

        // Add implicit return if needed
        if !has_terminator {
            let return_type = self.resolve_type_node(&func.return_type)?;
            match return_type {
                Type::Void => {
                    self.builder.build_return(None)
                        .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
                }
                _ => {
                    // Return a default value for non-void functions
                    let default_val = self.default_value(&return_type)?;
                    self.builder.build_return(Some(&default_val))
                        .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
                }
            }
        }

        // Restore previous context
        self.variables = prev_variables;
        self.current_function = prev_function;

        Ok(())
    }

    /// Generate a default value for a type
    fn default_value(&self, type_: &Type) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match type_ {
            Type::Primitive(PrimitiveType::Int) => {
                Ok(self.context.i64_type().const_int(0, false).into())
            }
            Type::Primitive(PrimitiveType::Float) => {
                Ok(self.context.f64_type().const_float(0.0).into())
            }
            Type::Primitive(PrimitiveType::Bool) => {
                Ok(self.context.bool_type().const_int(0, false).into())
            }
            Type::Primitive(PrimitiveType::Char) => {
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            Type::Primitive(PrimitiveType::Str) => {
                let null = self.context.ptr_type(AddressSpace::default()).const_null();
                Ok(null.into())
            }
            _ => {
                // For complex types, return null pointer
                let null = self.context.ptr_type(AddressSpace::default()).const_null();
                Ok(null.into())
            }
        }
    }

    // ========================================================================
    // Phase 2.3: Class Compilation
    // ========================================================================

    /// Declare a class (create the struct type)
    fn declare_class(
        &mut self,
        name: &str,
        fields: &[crate::parser::Field],
    ) -> CodeGenResult<StructType<'ctx>> {
        let field_types: Vec<BasicTypeEnum> = fields
            .iter()
            .map(|f| {
                let type_ = self.resolve_type_node(&f.type_)?;
                self.type_to_llvm(&type_)
            })
            .collect::<CodeGenResult<Vec<_>>>()?;

        let struct_type = self.context.opaque_struct_type(name);
        struct_type.set_body(&field_types, false);

        self.struct_types.insert(name.to_string(), struct_type);

        // Declare the constructor function (ClassName.new)
        let constructor_name = format!("{}.new", name);
        let constructor_type = self
            .context
            .ptr_type(AddressSpace::default())
            .fn_type(&[], false);
        let constructor = self.module.add_function(&constructor_name, constructor_type, None);

        // Store the constructor in functions table
        self.functions.insert(
            constructor_name,
            CompiledFunction {
                function: constructor,
                return_type: Type::Named(name.to_string(), vec![]),
            },
        );

        Ok(struct_type)
    }

    /// Compile class methods
    fn compile_class_methods(
        &mut self,
        class_name: &str,
        methods: &[FunctionNode],
    ) -> CodeGenResult<()> {
        // Get the struct type
        let struct_type = self
            .struct_types
            .get(class_name)
            .cloned()
            .ok_or_else(|| CodeGenError::new(format!("Class {} not declared", class_name)))?;

        // First, compile the constructor
        let constructor_name = format!("{}.new", class_name);
        if let Some(compiled) = self.functions.get(&constructor_name).cloned() {
            let entry = self.context.append_basic_block(compiled.function, "entry");
            self.builder.position_at_end(entry);

            // Allocate the struct
            let size = struct_type.size_of().unwrap();
            let alloc_fn = self
                .runtime_functions
                .get("mux_alloc_object")
                .ok_or_else(|| CodeGenError::new("mux_alloc_object not declared"))?;

            let ptr = self
                .builder
                .build_call(*alloc_fn, &[size.into()], "obj_ptr")
                .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                .try_as_basic_value()
                .left()
                .ok_or_else(|| CodeGenError::new("Expected return value from mux_alloc_object"))?;

            self.builder.build_return(Some(&ptr))
                .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
        }

        // Compile each method
        for method in methods {
            let method_name = format!("{}.{}", class_name, method.name);

            // Declare the method with self parameter
            let return_type = self.resolve_type_node(&method.return_type)?;
            let self_type = Type::Named(class_name.to_string(), vec![]);

            let mut all_params: Vec<(String, Type)> = vec![("self".to_string(), self_type)];
            for param in &method.params {
                let param_type = self.resolve_type_node(&param.type_)?;
                all_params.push((param.name.clone(), param_type));
            }

            let param_types: Vec<BasicMetadataTypeEnum> = all_params
                .iter()
                .map(|(_, t)| self.type_to_llvm(t).map(|ty| ty.into()))
                .collect::<CodeGenResult<Vec<_>>>()?;

            let fn_type = match &return_type {
                Type::Void => self.context.void_type().fn_type(&param_types, false),
                _ => {
                    let ret_llvm = self.type_to_llvm(&return_type)?;
                    ret_llvm.fn_type(&param_types, false)
                }
            };

            let function = self.module.add_function(&method_name, fn_type, None);
            self.functions.insert(
                method_name.clone(),
                CompiledFunction {
                    function,
                    return_type: return_type.clone(),
                },
            );

            // Compile the method body
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            let prev_function = self.current_function;
            self.current_function = Some(function);
            let prev_variables = self.variables.clone();
            self.variables.clear();

            // Add parameters to scope
            for (i, (param_name, param_type)) in all_params.iter().enumerate() {
                let llvm_type = self.type_to_llvm(param_type)?;
                let alloca = self.builder.build_alloca(llvm_type, param_name)
                    .map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;

                let param_value = function.get_nth_param(i as u32).ok_or_else(|| {
                    CodeGenError::new(format!(
                        "Could not get parameter {} for method {}",
                        i, method_name
                    ))
                })?;

                self.builder.build_store(alloca, param_value)
                    .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

                self.variables.insert(
                    param_name.clone(),
                    Variable {
                        ptr: alloca,
                        type_: param_type.clone(),
                    },
                );
            }

            // Compile the body
            let mut has_terminator = false;
            for stmt in &method.body {
                self.compile_statement(stmt)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_some() {
                    has_terminator = true;
                    break;
                }
            }

            if !has_terminator {
                match return_type {
                    Type::Void => {
                        self.builder.build_return(None)
                            .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
                    }
                    _ => {
                        let default_val = self.default_value(&return_type)?;
                        self.builder.build_return(Some(&default_val))
                            .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
                    }
                }
            }

            self.variables = prev_variables;
            self.current_function = prev_function;
        }

        Ok(())
    }

    // ========================================================================
    // Phase 3.3: Enum System
    // ========================================================================

    /// Declare an enum type (create the tagged union struct)
    fn declare_enum(
        &mut self,
        name: &str,
        variants: &[EnumVariant],
    ) -> CodeGenResult<StructType<'ctx>> {
        // Calculate the maximum size needed for any variant's payload
        let mut max_payload_size = 0u64;
        let mut variant_info: HashMap<String, (u32, Vec<Type>)> = HashMap::new();

        for (i, variant) in variants.iter().enumerate() {
            let field_types: Vec<Type> = if let Some(data) = &variant.data {
                data.iter()
                    .map(|t| self.resolve_type_node(t))
                    .collect::<CodeGenResult<Vec<_>>>()?
            } else {
                vec![]
            };

            // Calculate payload size for this variant
            let mut variant_size = 0u64;
            for field_type in &field_types {
                let llvm_type = self.type_to_llvm(field_type)?;
                variant_size += self.get_type_size(llvm_type);
            }

            if variant_size > max_payload_size {
                max_payload_size = variant_size;
            }

            variant_info.insert(variant.name.clone(), (i as u32, field_types));
        }

        // Create the enum struct type: { i32 discriminant, [N x i8] payload }
        // Use i8 array for the payload to ensure proper alignment
        let discriminant_type = self.context.i32_type();
        let payload_type = if max_payload_size > 0 {
            self.context.i8_type().array_type(max_payload_size as u32)
        } else {
            self.context.i8_type().array_type(1) // Minimum 1 byte payload
        };

        let struct_type = self.context.opaque_struct_type(name);
        struct_type.set_body(&[discriminant_type.into(), payload_type.into()], false);

        // Store enum type info
        self.enum_types.insert(
            name.to_string(),
            EnumTypeInfo {
                struct_type,
                variants: variant_info,
            },
        );

        // Also register in struct_types for type resolution
        self.struct_types.insert(name.to_string(), struct_type);

        Ok(struct_type)
    }

    /// Get the size of a type in bytes (approximate)
    fn get_type_size(&self, llvm_type: BasicTypeEnum<'ctx>) -> u64 {
        match llvm_type {
            BasicTypeEnum::IntType(t) => (t.get_bit_width() as u64 + 7) / 8,
            BasicTypeEnum::FloatType(_) => 8, // f64
            BasicTypeEnum::PointerType(_) => 8, // 64-bit pointer
            BasicTypeEnum::StructType(t) => {
                // Sum of field sizes (not accounting for padding)
                let mut size = 0u64;
                for i in 0..t.count_fields() {
                    if let Some(field) = t.get_field_type_at_index(i) {
                        size += self.get_type_size(field);
                    }
                }
                size
            }
            BasicTypeEnum::ArrayType(t) => {
                let elem_size = self.get_type_size(t.get_element_type());
                elem_size * t.len() as u64
            }
            _ => 8, // Default to 8 bytes
        }
    }

    /// Compile enum variant constructor functions
    fn compile_enum_constructors(
        &mut self,
        enum_name: &str,
        variants: &[EnumVariant],
    ) -> CodeGenResult<()> {
        let enum_info = self
            .enum_types
            .get(enum_name)
            .cloned()
            .ok_or_else(|| CodeGenError::new(format!("Enum {} not declared", enum_name)))?;

        for variant in variants {
            let (discriminant, field_types) = enum_info
                .variants
                .get(&variant.name)
                .ok_or_else(|| {
                    CodeGenError::new(format!("Variant {} not found in enum {}", variant.name, enum_name))
                })?;

            // Create constructor function: EnumName::VariantName(args...) -> EnumName
            let constructor_name = format!("{}::{}", enum_name, variant.name);

            // Build parameter types
            let param_types: Vec<BasicMetadataTypeEnum> = field_types
                .iter()
                .map(|t| self.type_to_llvm(t).map(|ty| ty.into()))
                .collect::<CodeGenResult<Vec<_>>>()?;

            // Return type is a pointer to the enum struct
            let return_type = self.context.ptr_type(AddressSpace::default());
            let fn_type = return_type.fn_type(&param_types, false);

            let function = self.module.add_function(&constructor_name, fn_type, None);

            // Store in functions table
            self.functions.insert(
                constructor_name.clone(),
                CompiledFunction {
                    function,
                    return_type: Type::Named(enum_name.to_string(), vec![]),
                },
            );

            // Also store variant name alone for pattern matching
            self.functions.insert(
                variant.name.clone(),
                CompiledFunction {
                    function,
                    return_type: Type::Named(enum_name.to_string(), vec![]),
                },
            );

            // Generate constructor body
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            // Allocate the enum struct
            let alloc_fn = self
                .runtime_functions
                .get("mux_alloc_object")
                .ok_or_else(|| CodeGenError::new("mux_alloc_object not declared"))?;

            let struct_size = enum_info.struct_type.size_of().unwrap();
            let enum_ptr = self
                .builder
                .build_call(*alloc_fn, &[struct_size.into()], "enum_ptr")
                .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                .try_as_basic_value()
                .left()
                .ok_or_else(|| CodeGenError::new("Expected return value from mux_alloc_object"))?
                .into_pointer_value();

            // Set the discriminant
            let disc_ptr = self
                .builder
                .build_struct_gep(enum_info.struct_type, enum_ptr, 0, "disc_ptr")
                .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

            let disc_val = self.context.i32_type().const_int(*discriminant as u64, false);
            self.builder
                .build_store(disc_ptr, disc_val)
                .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

            // Store the payload fields
            if !field_types.is_empty() {
                let payload_ptr = self
                    .builder
                    .build_struct_gep(enum_info.struct_type, enum_ptr, 1, "payload_ptr")
                    .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

                // Create a struct type for the payload
                let payload_field_types: Vec<BasicTypeEnum> = field_types
                    .iter()
                    .map(|t| self.type_to_llvm(t))
                    .collect::<CodeGenResult<Vec<_>>>()?;

                let payload_struct = self.context.struct_type(&payload_field_types, false);

                // Store each argument
                for (i, _field_type) in field_types.iter().enumerate() {
                    let arg = function.get_nth_param(i as u32).ok_or_else(|| {
                        CodeGenError::new(format!("Could not get parameter {}", i))
                    })?;

                    let field_ptr = self
                        .builder
                        .build_struct_gep(payload_struct, payload_ptr, i as u32, &format!("field_{}", i))
                        .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

                    self.builder
                        .build_store(field_ptr, arg)
                        .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;
                }
            }

            // Return the enum pointer
            self.builder
                .build_return(Some(&enum_ptr))
                .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
        }

        Ok(())
    }

    /// Get the discriminant value from an enum instance
    fn compile_enum_discriminant(
        &mut self,
        enum_ptr: PointerValue<'ctx>,
        enum_name: &str,
    ) -> CodeGenResult<inkwell::values::IntValue<'ctx>> {
        let enum_info = self
            .enum_types
            .get(enum_name)
            .ok_or_else(|| CodeGenError::new(format!("Enum {} not declared", enum_name)))?;

        let disc_ptr = self
            .builder
            .build_struct_gep(enum_info.struct_type, enum_ptr, 0, "disc_ptr")
            .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

        let disc_val = self
            .builder
            .build_load(self.context.i32_type(), disc_ptr, "disc_val")
            .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?
            .into_int_value();

        Ok(disc_val)
    }

    /// Extract a payload field from an enum instance
    fn compile_enum_payload_field(
        &mut self,
        enum_ptr: PointerValue<'ctx>,
        enum_name: &str,
        variant_name: &str,
        field_index: u32,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let enum_info = self
            .enum_types
            .get(enum_name)
            .cloned()
            .ok_or_else(|| CodeGenError::new(format!("Enum {} not declared", enum_name)))?;

        let (_disc, field_types) = enum_info
            .variants
            .get(variant_name)
            .ok_or_else(|| {
                CodeGenError::new(format!("Variant {} not found in enum {}", variant_name, enum_name))
            })?;

        if field_index as usize >= field_types.len() {
            return Err(CodeGenError::new(format!(
                "Field index {} out of bounds for variant {}",
                field_index, variant_name
            )));
        }

        let field_type = &field_types[field_index as usize];
        let llvm_field_type = self.type_to_llvm(field_type)?;

        let payload_ptr = self
            .builder
            .build_struct_gep(enum_info.struct_type, enum_ptr, 1, "payload_ptr")
            .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

        // Create a struct type for the payload
        let payload_field_types: Vec<BasicTypeEnum> = field_types
            .iter()
            .map(|t| self.type_to_llvm(t))
            .collect::<CodeGenResult<Vec<_>>>()?;

        let payload_struct = self.context.struct_type(&payload_field_types, false);

        let field_ptr = self
            .builder
            .build_struct_gep(payload_struct, payload_ptr, field_index, &format!("field_{}", field_index))
            .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

        let field_val = self
            .builder
            .build_load(llvm_field_type, field_ptr, "field_val")
            .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?;

        Ok(field_val)
    }

    // ========================================================================
    // Phase 2.4: Statement Compilation
    // ========================================================================

    /// Compile a statement
    fn compile_statement(&mut self, stmt: &StatementNode) -> CodeGenResult<()> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, expr) | StatementKind::TypedDecl(name, _, expr) => {
                self.compile_variable_declaration(name, expr)?;
            }
            StatementKind::ConstDecl(name, _, expr) => {
                self.compile_variable_declaration(name, expr)?;
            }
            StatementKind::Expression(expr) => {
                self.compile_expression(expr)?;
            }
            StatementKind::Return(Some(expr)) => {
                let value = self.compile_expression(expr)?;
                self.builder.build_return(Some(&value))
                    .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
            }
            StatementKind::Return(None) => {
                self.builder.build_return(None)
                    .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.compile_if_statement(cond, then_block, else_block)?;
            }
            StatementKind::While { cond, body } => {
                self.compile_while_statement(cond, body)?;
            }
            StatementKind::For {
                var, iter, body, ..
            } => {
                self.compile_for_statement(var, iter, body)?;
            }
            StatementKind::Block(stmts) => {
                for inner_stmt in stmts {
                    self.compile_statement(inner_stmt)?;
                }
            }
            StatementKind::Break => {
                if let Some((_, break_bb)) = self.loop_stack.last() {
                    self.builder.build_unconditional_branch(*break_bb)
                        .map_err(|e| CodeGenError::new(format!("Failed to build break branch: {}", e)))?;
                } else {
                    return Err(CodeGenError::new("Break statement outside of loop"));
                }
            }
            StatementKind::Continue => {
                if let Some((continue_bb, _)) = self.loop_stack.last() {
                    self.builder.build_unconditional_branch(*continue_bb)
                        .map_err(|e| CodeGenError::new(format!("Failed to build continue branch: {}", e)))?;
                } else {
                    return Err(CodeGenError::new("Continue statement outside of loop"));
                }
            }
            StatementKind::Match { expr, arms } => {
                self.compile_match_statement(expr, arms)?;
            }
            StatementKind::Import { .. } => {
                // Imports are handled at the semantic level
            }
            StatementKind::Function(func) => {
                self.declare_function(func)?;
                self.compile_function(func)?;
            }
        }
        Ok(())
    }

    /// Compile a variable declaration
    fn compile_variable_declaration(
        &mut self,
        name: &str,
        expr: &ExpressionNode,
    ) -> CodeGenResult<()> {
        let value = self.compile_expression(expr)?;
        let type_ = self.infer_expression_type(expr)?;
        let llvm_type = self.type_to_llvm(&type_)?;

        let alloca = self.builder.build_alloca(llvm_type, name)
            .map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;
        self.builder.build_store(alloca, value)
            .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

        self.variables.insert(
            name.to_string(),
            Variable {
                ptr: alloca,
                type_,
            },
        );

        Ok(())
    }

    /// Compile an if statement
    fn compile_if_statement(
        &mut self,
        cond: &ExpressionNode,
        then_block: &[StatementNode],
        else_block: &Option<Vec<StatementNode>>,
    ) -> CodeGenResult<()> {
        let cond_value = self.compile_expression(cond)?;
        let cond_bool = cond_value.into_int_value();

        let function = self.current_function.ok_or_else(|| {
            CodeGenError::new("Cannot compile if statement outside of a function")
        })?;

        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "merge");

        self.builder.build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build conditional branch: {}", e)))?;

        // Compile then block
        self.builder.position_at_end(then_bb);
        for stmt in then_block {
            self.compile_statement(stmt)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb)
                .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;
        }

        // Compile else block
        self.builder.position_at_end(else_bb);
        if let Some(else_stmts) = else_block {
            for stmt in else_stmts {
                self.compile_statement(stmt)?;
            }
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb)
                .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;
        }

        // Continue at merge block
        self.builder.position_at_end(merge_bb);

        Ok(())
    }

    /// Compile a while statement
    fn compile_while_statement(
        &mut self,
        cond: &ExpressionNode,
        body: &[StatementNode],
    ) -> CodeGenResult<()> {
        let function = self.current_function.ok_or_else(|| {
            CodeGenError::new("Cannot compile while statement outside of a function")
        })?;

        let cond_bb = self.context.append_basic_block(function, "while.cond");
        let body_bb = self.context.append_basic_block(function, "while.body");
        let end_bb = self.context.append_basic_block(function, "while.end");

        // Push loop context for break/continue
        // continue jumps to condition, break jumps to end
        self.loop_stack.push((cond_bb, end_bb));

        // Jump to condition block
        self.builder.build_unconditional_branch(cond_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;

        // Compile condition
        self.builder.position_at_end(cond_bb);
        let cond_value = self.compile_expression(cond)?;
        let cond_bool = cond_value.into_int_value();
        self.builder.build_conditional_branch(cond_bool, body_bb, end_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build conditional branch: {}", e)))?;

        // Compile body
        self.builder.position_at_end(body_bb);
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(cond_bb)
                .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;
        }

        // Pop loop context
        self.loop_stack.pop();

        // Continue at end block
        self.builder.position_at_end(end_bb);

        Ok(())
    }

    /// Compile a for statement
    fn compile_for_statement(
        &mut self,
        var: &str,
        iter: &ExpressionNode,
        body: &[StatementNode],
    ) -> CodeGenResult<()> {
        let function = self.current_function.ok_or_else(|| {
            CodeGenError::new("Cannot compile for statement outside of a function")
        })?;

        // Compile the iterator expression (should return a list)
        let list_ptr = self.compile_expression(iter)?;

        // Get the list length
        let length_fn = self
            .runtime_functions
            .get("mux_list_length")
            .ok_or_else(|| CodeGenError::new("mux_list_length not declared"))?;

        let length = self
            .builder
            .build_call(*length_fn, &[list_ptr.into()], "list_len")
            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodeGenError::new("Expected return value from mux_list_length"))?
            .into_int_value();

        // Create index variable
        let index_alloca = self.builder.build_alloca(self.context.i64_type(), "for.idx")
            .map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;
        self.builder.build_store(index_alloca, self.context.i64_type().const_int(0, false))
            .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

        let cond_bb = self.context.append_basic_block(function, "for.cond");
        let body_bb = self.context.append_basic_block(function, "for.body");
        let inc_bb = self.context.append_basic_block(function, "for.inc");
        let end_bb = self.context.append_basic_block(function, "for.end");

        // Push loop context for break/continue
        // continue jumps to increment, break jumps to end
        self.loop_stack.push((inc_bb, end_bb));

        self.builder.build_unconditional_branch(cond_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;

        // Condition: index < length
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(self.context.i64_type(), index_alloca, "idx")
            .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?
            .into_int_value();
        let cond = self.builder.build_int_compare(IntPredicate::SLT, current_idx, length, "for.cond")
            .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?;
        self.builder.build_conditional_branch(cond, body_bb, end_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build conditional branch: {}", e)))?;

        // Body: get element and bind to variable
        self.builder.position_at_end(body_bb);

        let get_fn = self
            .runtime_functions
            .get("mux_list_get")
            .ok_or_else(|| CodeGenError::new("mux_list_get not declared"))?;

        let element = self
            .builder
            .build_call(*get_fn, &[list_ptr.into(), current_idx.into()], "element")
            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodeGenError::new("Expected return value from mux_list_get"))?;

        // Create the loop variable
        let var_alloca = self.builder.build_alloca(
            self.context.ptr_type(AddressSpace::default()),
            var,
        ).map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;
        self.builder.build_store(var_alloca, element)
            .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

        // Save previous binding and add new one
        let prev_var = self.variables.remove(var);
        self.variables.insert(
            var.to_string(),
            Variable {
                ptr: var_alloca,
                type_: Type::Primitive(PrimitiveType::Int), // Assume int for now
            },
        );

        // Compile body
        for stmt in body {
            self.compile_statement(stmt)?;
        }

        // Restore previous binding
        if let Some(prev) = prev_var {
            self.variables.insert(var.to_string(), prev);
        } else {
            self.variables.remove(var);
        }

        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb)
                .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;
        }

        // Increment
        self.builder.position_at_end(inc_bb);
        let new_idx = self.builder.build_int_add(
            current_idx,
            self.context.i64_type().const_int(1, false),
            "next.idx",
        ).map_err(|e| CodeGenError::new(format!("Failed to build int add: {}", e)))?;
        self.builder.build_store(index_alloca, new_idx)
            .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;
        self.builder.build_unconditional_branch(cond_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;

        // Pop loop context
        self.loop_stack.pop();

        self.builder.position_at_end(end_bb);

        Ok(())
    }

    /// Compile a match statement
    fn compile_match_statement(
        &mut self,
        expr: &ExpressionNode,
        arms: &[crate::parser::MatchArm],
    ) -> CodeGenResult<()> {
        let function = self.current_function.ok_or_else(|| {
            CodeGenError::new("Cannot compile match statement outside of a function")
        })?;

        // Compile the match expression
        let match_value = self.compile_expression(expr)?;
        let match_type = self.infer_expression_type(expr)?;

        // Create blocks for each arm and the merge block
        let mut arm_blocks: Vec<inkwell::basic_block::BasicBlock<'ctx>> = Vec::new();
        for i in 0..arms.len() {
            arm_blocks.push(self.context.append_basic_block(function, &format!("match.arm{}", i)));
        }
        let merge_bb = self.context.append_basic_block(function, "match.merge");
        let default_bb = self.context.append_basic_block(function, "match.default");

        // For simple integer/bool matches, we can use a switch instruction
        // For more complex patterns, we fall back to if-else chains
        if self.can_use_switch(&arms) && matches!(match_type, Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Bool) | Type::Primitive(PrimitiveType::Char)) {
            // Use switch instruction for simple patterns
            self.compile_match_switch(match_value, &match_type, arms, &arm_blocks, default_bb, merge_bb)?;
        } else {
            // Use if-else chain for complex patterns
            self.compile_match_if_chain(match_value, &match_type, arms, &arm_blocks, default_bb, merge_bb)?;
        }

        // Position at merge block for continuation
        self.builder.position_at_end(merge_bb);

        Ok(())
    }

    /// Check if we can use a switch instruction for the match
    fn can_use_switch(&self, arms: &[crate::parser::MatchArm]) -> bool {
        arms.iter().all(|arm| {
            matches!(
                &arm.pattern,
                crate::parser::PatternNode::Literal(crate::parser::LiteralNode::Integer(_))
                    | crate::parser::PatternNode::Literal(crate::parser::LiteralNode::Boolean(_))
                    | crate::parser::PatternNode::Literal(crate::parser::LiteralNode::Char(_))
                    | crate::parser::PatternNode::Wildcard
                    | crate::parser::PatternNode::Identifier(_)
            ) && arm.guard.is_none()
        })
    }

    /// Compile match using LLVM switch instruction
    fn compile_match_switch(
        &mut self,
        match_value: BasicValueEnum<'ctx>,
        _match_type: &Type,
        arms: &[crate::parser::MatchArm],
        arm_blocks: &[inkwell::basic_block::BasicBlock<'ctx>],
        default_bb: inkwell::basic_block::BasicBlock<'ctx>,
        merge_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> CodeGenResult<()> {
        let int_value = match_value.into_int_value();

        // Find the default arm (wildcard or identifier pattern)
        let mut default_arm_idx: Option<usize> = None;
        let mut cases: Vec<(inkwell::values::IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            match &arm.pattern {
                crate::parser::PatternNode::Wildcard | crate::parser::PatternNode::Identifier(_) => {
                    if default_arm_idx.is_none() {
                        default_arm_idx = Some(i);
                    }
                }
                crate::parser::PatternNode::Literal(lit) => {
                    let case_value = match lit {
                        crate::parser::LiteralNode::Integer(n) => {
                            self.context.i64_type().const_int(*n as u64, true)
                        }
                        crate::parser::LiteralNode::Boolean(b) => {
                            self.context.bool_type().const_int(if *b { 1 } else { 0 }, false)
                        }
                        crate::parser::LiteralNode::Char(c) => {
                            self.context.i32_type().const_int(*c as u64, false)
                        }
                        _ => continue, // Skip non-integer literals
                    };
                    cases.push((case_value, arm_blocks[i]));
                }
                _ => {}
            }
        }

        // Build the switch instruction
        let actual_default = if let Some(idx) = default_arm_idx {
            arm_blocks[idx]
        } else {
            default_bb
        };

        let _switch = self.builder.build_switch(int_value, actual_default, &cases)
            .map_err(|e| CodeGenError::new(format!("Failed to build switch: {}", e)))?;

        // Compile each arm's body
        for (i, arm) in arms.iter().enumerate() {
            self.builder.position_at_end(arm_blocks[i]);

            // Bind identifier patterns to the match value
            if let crate::parser::PatternNode::Identifier(name) = &arm.pattern {
                let llvm_type = match_value.get_type();
                let alloca = self.builder.build_alloca(llvm_type, name)
                    .map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;
                self.builder.build_store(alloca, match_value)
                    .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

                // Save previous binding
                let prev_var = self.variables.remove(name);
                self.variables.insert(
                    name.clone(),
                    Variable {
                        ptr: alloca,
                        type_: self.infer_expression_type_from_value(match_value)?,
                    },
                );

                // Compile body
                for stmt in &arm.body {
                    self.compile_statement(stmt)?;
                }

                // Restore previous binding
                if let Some(prev) = prev_var {
                    self.variables.insert(name.clone(), prev);
                } else {
                    self.variables.remove(name);
                }
            } else {
                // Compile body without binding
                for stmt in &arm.body {
                    self.compile_statement(stmt)?;
                }
            }

            // Branch to merge if no terminator
            if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                self.builder.build_unconditional_branch(merge_bb)
                    .map_err(|e| CodeGenError::new(format!("Failed to build branch: {}", e)))?;
            }
        }

        // Default block just branches to merge
        self.builder.position_at_end(default_bb);
        self.builder.build_unconditional_branch(merge_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build branch: {}", e)))?;

        Ok(())
    }

    /// Compile match using if-else chain for complex patterns
    fn compile_match_if_chain(
        &mut self,
        match_value: BasicValueEnum<'ctx>,
        match_type: &Type,
        arms: &[crate::parser::MatchArm],
        arm_blocks: &[inkwell::basic_block::BasicBlock<'ctx>],
        default_bb: inkwell::basic_block::BasicBlock<'ctx>,
        merge_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> CodeGenResult<()> {
        let function = self.current_function.unwrap();

        // Create condition blocks for each arm
        let mut cond_blocks: Vec<inkwell::basic_block::BasicBlock<'ctx>> = Vec::new();
        for i in 0..arms.len() {
            cond_blocks.push(self.context.append_basic_block(function, &format!("match.cond{}", i)));
        }

        // Jump to first condition
        self.builder.build_unconditional_branch(cond_blocks[0])
            .map_err(|e| CodeGenError::new(format!("Failed to build branch: {}", e)))?;

        for (i, arm) in arms.iter().enumerate() {
            self.builder.position_at_end(cond_blocks[i]);

            // Check if pattern matches
            let matches = self.compile_pattern_check(&arm.pattern, match_value, match_type)?;

            // If there's a guard, check it too
            let condition = if let Some(guard) = &arm.guard {
                let guard_val = self.compile_expression(guard)?;
                self.builder.build_and(matches, guard_val.into_int_value(), "match.guard")
                    .map_err(|e| CodeGenError::new(format!("Failed to build and: {}", e)))?
            } else {
                matches
            };

            // Branch based on condition
            let next_cond = if i + 1 < cond_blocks.len() {
                cond_blocks[i + 1]
            } else {
                default_bb
            };

            self.builder.build_conditional_branch(condition, arm_blocks[i], next_cond)
                .map_err(|e| CodeGenError::new(format!("Failed to build conditional branch: {}", e)))?;

            // Compile arm body
            self.builder.position_at_end(arm_blocks[i]);

            // Bind pattern variables
            self.bind_pattern_variables(&arm.pattern, match_value, match_type)?;

            for stmt in &arm.body {
                self.compile_statement(stmt)?;
            }

            // Branch to merge if no terminator
            if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                self.builder.build_unconditional_branch(merge_bb)
                    .map_err(|e| CodeGenError::new(format!("Failed to build branch: {}", e)))?;
            }
        }

        // Default block just branches to merge (unreachable if patterns are exhaustive)
        self.builder.position_at_end(default_bb);
        self.builder.build_unconditional_branch(merge_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build branch: {}", e)))?;

        Ok(())
    }

    /// Compile a pattern check - returns bool indicating if pattern matches
    fn compile_pattern_check(
        &mut self,
        pattern: &crate::parser::PatternNode,
        value: BasicValueEnum<'ctx>,
        value_type: &Type,
    ) -> CodeGenResult<inkwell::values::IntValue<'ctx>> {
        match pattern {
            crate::parser::PatternNode::Wildcard | crate::parser::PatternNode::Identifier(_) => {
                // Always matches
                Ok(self.context.bool_type().const_int(1, false))
            }
            crate::parser::PatternNode::Literal(lit) => {
                let lit_value = self.compile_literal(lit)?;
                match value_type {
                    Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Char) | Type::Primitive(PrimitiveType::Bool) => {
                        self.builder.build_int_compare(
                            IntPredicate::EQ,
                            value.into_int_value(),
                            lit_value.into_int_value(),
                            "pattern.eq",
                        ).map_err(|e| CodeGenError::new(format!("Failed to build compare: {}", e)))
                    }
                    Type::Primitive(PrimitiveType::Float) => {
                        self.builder.build_float_compare(
                            FloatPredicate::OEQ,
                            value.into_float_value(),
                            lit_value.into_float_value(),
                            "pattern.feq",
                        ).map_err(|e| CodeGenError::new(format!("Failed to build compare: {}", e)))
                    }
                    Type::Primitive(PrimitiveType::Str) => {
                        let equals_fn = self.runtime_functions.get("mux_string_equals")
                            .ok_or_else(|| CodeGenError::new("mux_string_equals not declared"))?;
                        let result = self.builder
                            .build_call(*equals_fn, &[value.into(), lit_value.into()], "pattern.str_eq")
                            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                            .try_as_basic_value()
                            .left()
                            .ok_or_else(|| CodeGenError::new("Expected return value"))?;
                        Ok(result.into_int_value())
                    }
                    _ => Err(CodeGenError::new(format!(
                        "Cannot pattern match literal against type {:?}",
                        value_type
                    ))),
                }
            }
            crate::parser::PatternNode::Tuple(patterns) => {
                // For tuples, check each element
                if let Type::Tuple(elem_types) = value_type {
                    let mut result = self.context.bool_type().const_int(1, false);
                    for (i, (pat, elem_type)) in patterns.iter().zip(elem_types.iter()).enumerate() {
                        let elem_val = self.builder.build_extract_value(
                            value.into_struct_value(),
                            i as u32,
                            &format!("tuple.{}", i),
                        ).map_err(|e| CodeGenError::new(format!("Failed to extract value: {}", e)))?;
                        let elem_matches = self.compile_pattern_check(pat, elem_val.into(), elem_type)?;
                        result = self.builder.build_and(result, elem_matches, "pattern.and")
                            .map_err(|e| CodeGenError::new(format!("Failed to build and: {}", e)))?;
                    }
                    Ok(result)
                } else {
                    Err(CodeGenError::new("Expected tuple type for tuple pattern"))
                }
            }
            crate::parser::PatternNode::EnumVariant { name, args: _ } => {
                // For enums, check the discriminant
                if name == "None" {
                    // Check for null pointer (for Optional types)
                    let null = self.context.ptr_type(AddressSpace::default()).const_null();
                    self.builder.build_int_compare(
                        IntPredicate::EQ,
                        value.into_pointer_value(),
                        null,
                        "is_none",
                    ).map_err(|e| CodeGenError::new(format!("Failed to build compare: {}", e)))
                } else if name == "Some" {
                    // Check for non-null pointer (for Optional types)
                    let null = self.context.ptr_type(AddressSpace::default()).const_null();
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        value.into_pointer_value(),
                        null,
                        "is_some",
                    ).map_err(|e| CodeGenError::new(format!("Failed to build compare: {}", e)))
                } else {
                    // Check if this is a declared enum type
                    if let Type::Named(enum_name, _) = value_type {
                        if let Some(enum_info) = self.enum_types.get(enum_name).cloned() {
                            if let Some((discriminant, _)) = enum_info.variants.get(name) {
                                // Get the discriminant from the enum value
                                let disc_ptr = self
                                    .builder
                                    .build_struct_gep(enum_info.struct_type, value.into_pointer_value(), 0, "disc_ptr")
                                    .map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

                                let disc_val = self
                                    .builder
                                    .build_load(self.context.i32_type(), disc_ptr, "disc_val")
                                    .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?
                                    .into_int_value();

                                let expected_disc = self.context.i32_type().const_int(*discriminant as u64, false);

                                return self.builder.build_int_compare(
                                    IntPredicate::EQ,
                                    disc_val,
                                    expected_disc,
                                    &format!("is_{}", name),
                                ).map_err(|e| CodeGenError::new(format!("Failed to build compare: {}", e)));
                            }
                        }
                    }
                    // Fallback: assume it always matches (for forward compatibility)
                    Ok(self.context.bool_type().const_int(1, false))
                }
            }
        }
    }

    /// Bind pattern variables to the matched value
    fn bind_pattern_variables(
        &mut self,
        pattern: &crate::parser::PatternNode,
        value: BasicValueEnum<'ctx>,
        value_type: &Type,
    ) -> CodeGenResult<()> {
        match pattern {
            crate::parser::PatternNode::Identifier(name) => {
                let llvm_type = self.type_to_llvm(value_type)?;
                let alloca = self.builder.build_alloca(llvm_type, name)
                    .map_err(|e| CodeGenError::new(format!("Failed to build alloca: {}", e)))?;
                self.builder.build_store(alloca, value)
                    .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;
                self.variables.insert(
                    name.clone(),
                    Variable {
                        ptr: alloca,
                        type_: value_type.clone(),
                    },
                );
            }
            crate::parser::PatternNode::Tuple(patterns) => {
                if let Type::Tuple(elem_types) = value_type {
                    for (i, (pat, elem_type)) in patterns.iter().zip(elem_types.iter()).enumerate() {
                        let elem_val = self.builder.build_extract_value(
                            value.into_struct_value(),
                            i as u32,
                            &format!("tuple.{}", i),
                        ).map_err(|e| CodeGenError::new(format!("Failed to extract value: {}", e)))?;
                        self.bind_pattern_variables(pat, elem_val.into(), elem_type)?;
                    }
                }
            }
            crate::parser::PatternNode::EnumVariant { name, args } => {
                if name == "Some" && !args.is_empty() {
                    // Bind the inner value of Some(x) for Optional types
                    self.bind_pattern_variables(&args[0], value, value_type)?;
                } else if !args.is_empty() {
                    // Handle declared enum variants with payload
                    if let Type::Named(enum_name, _) = value_type {
                        if let Some(enum_info) = self.enum_types.get(enum_name).cloned() {
                            if let Some((_, field_types)) = enum_info.variants.get(name) {
                                // Extract each payload field and bind to pattern variables
                                for (i, (arg_pat, field_type)) in args.iter().zip(field_types.iter()).enumerate() {
                                    let field_val = self.compile_enum_payload_field(
                                        value.into_pointer_value(),
                                        enum_name,
                                        name,
                                        i as u32,
                                    )?;
                                    self.bind_pattern_variables(arg_pat, field_val, field_type)?;
                                }
                            }
                        }
                    }
                }
            }
            _ => {} // Wildcard and Literal don't bind variables
        }
        Ok(())
    }

    /// Helper to infer type from LLVM value (best effort)
    fn infer_expression_type_from_value(&self, value: BasicValueEnum<'ctx>) -> CodeGenResult<Type> {
        match value {
            BasicValueEnum::IntValue(iv) => {
                let bit_width = iv.get_type().get_bit_width();
                match bit_width {
                    1 => Ok(Type::Primitive(PrimitiveType::Bool)),
                    32 => Ok(Type::Primitive(PrimitiveType::Char)),
                    64 => Ok(Type::Primitive(PrimitiveType::Int)),
                    _ => Ok(Type::Primitive(PrimitiveType::Int)),
                }
            }
            BasicValueEnum::FloatValue(_) => Ok(Type::Primitive(PrimitiveType::Float)),
            BasicValueEnum::PointerValue(_) => Ok(Type::Primitive(PrimitiveType::Str)),
            BasicValueEnum::StructValue(_) => Ok(Type::Tuple(vec![])),
            _ => Ok(Type::Primitive(PrimitiveType::Int)),
        }
    }

    // ========================================================================
    // Phase 2.5: Expression Compilation
    // ========================================================================

    /// Compile an expression and return its LLVM value
    fn compile_expression(&mut self, expr: &ExpressionNode) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => self.compile_literal(lit),
            ExpressionKind::None => {
                // None is represented as a null pointer
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .const_null()
                    .into())
            }
            ExpressionKind::Identifier(name) => self.compile_identifier(name),
            ExpressionKind::Binary { left, op, right } => {
                self.compile_binary_expression(left, op, right)
            }
            ExpressionKind::Unary { op, expr, postfix } => {
                self.compile_unary_expression(op, expr, *postfix)
            }
            ExpressionKind::Call { func, args } => self.compile_call_expression(func, args),
            ExpressionKind::FieldAccess { expr, field } => {
                self.compile_field_access(expr, field)
            }
            ExpressionKind::ListAccess { expr, index } => {
                self.compile_list_access(expr, index)
            }
            ExpressionKind::ListLiteral(elements) => self.compile_list_literal(elements),
            ExpressionKind::MapLiteral { entries, .. } => self.compile_map_literal(entries),
            ExpressionKind::SetLiteral(elements) => self.compile_set_literal(elements),
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => self.compile_if_expression(cond, then_expr, else_expr),
            ExpressionKind::Lambda { params, body } => self.compile_lambda(params, body),
            ExpressionKind::GenericType(name, type_args) => {
                self.compile_generic_type_expr(name, type_args)
            }
        }
    }

    /// Compile a literal value
    fn compile_literal(&mut self, lit: &LiteralNode) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match lit {
            LiteralNode::Integer(n) => Ok(self.context.i64_type().const_int(*n as u64, true).into()),
            LiteralNode::Float(f) => Ok(self.context.f64_type().const_float(f.0).into()),
            LiteralNode::Boolean(b) => {
                Ok(self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false)
                    .into())
            }
            LiteralNode::Char(c) => {
                Ok(self.context.i32_type().const_int(*c as u64, false).into())
            }
            LiteralNode::String(s) => self.compile_string_literal(s),
        }
    }

    /// Compile a string literal
    fn compile_string_literal(&mut self, s: &str) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // Check if we've already created this string constant
        if let Some(ptr) = self.string_constants.get(s) {
            return Ok((*ptr).into());
        }

        // Create a global string constant
        let string_const = self.builder.build_global_string_ptr(s, "str")
            .map_err(|e| CodeGenError::new(format!("Failed to build global string ptr: {}", e)))?;
        let ptr = string_const.as_pointer_value();

        // Cache the string constant
        self.string_constants.insert(s.to_string(), ptr);

        Ok(ptr.into())
    }

    /// Compile an identifier reference
    fn compile_identifier(&mut self, name: &str) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // First check local variables
        if let Some(var) = self.variables.get(name) {
            let llvm_type = self.type_to_llvm(&var.type_)?;
            let value = self.builder.build_load(llvm_type, var.ptr, name)
                .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?;
            return Ok(value);
        }

        // Then check if it's a function reference
        if let Some(compiled) = self.functions.get(name) {
            return Ok(compiled.function.as_global_value().as_pointer_value().into());
        }

        Err(CodeGenError::new(format!("Unknown identifier: {}", name)))
    }

    /// Compile a binary expression
    fn compile_binary_expression(
        &mut self,
        left: &ExpressionNode,
        op: &BinaryOp,
        right: &ExpressionNode,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // Special handling for assignment
        if matches!(op, BinaryOp::Assign) {
            return self.compile_assignment(left, right);
        }

        let left_val = self.compile_expression(left)?;
        let right_val = self.compile_expression(right)?;

        let left_type = self.infer_expression_type(left)?;

        match left_type {
            Type::Primitive(PrimitiveType::Int) => {
                self.compile_int_binary_op(left_val.into_int_value(), op, right_val.into_int_value())
            }
            Type::Primitive(PrimitiveType::Float) => {
                self.compile_float_binary_op(left_val.into_float_value(), op, right_val.into_float_value())
            }
            Type::Primitive(PrimitiveType::Bool) => {
                self.compile_bool_binary_op(left_val.into_int_value(), op, right_val.into_int_value())
            }
            Type::Primitive(PrimitiveType::Str) => {
                self.compile_string_binary_op(left_val.into_pointer_value(), op, right_val.into_pointer_value())
            }
            _ => Err(CodeGenError::new(format!(
                "Binary operation not supported for type {:?}",
                left_type
            ))),
        }
    }

    /// Compile assignment expression
    fn compile_assignment(
        &mut self,
        target: &ExpressionNode,
        value: &ExpressionNode,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let value_compiled = self.compile_expression(value)?;

        match &target.kind {
            ExpressionKind::Identifier(name) => {
                if let Some(var) = self.variables.get(name) {
                    self.builder.build_store(var.ptr, value_compiled)
                        .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;
                    Ok(value_compiled)
                } else {
                    Err(CodeGenError::new(format!(
                        "Cannot assign to undefined variable: {}",
                        name
                    )))
                }
            }
            ExpressionKind::FieldAccess { expr, field } => {
                // Handle field assignment (e.g., obj.field = value)
                let obj_ptr = self.compile_expression(expr)?;
                let obj_type = self.infer_expression_type(expr)?;

                if let Type::Named(class_name, _) = obj_type {
                    if let Some(struct_type) = self.struct_types.get(&class_name) {
                        // Find field index
                        if let Some(symbol) = self.analyzer.all_symbols().get(&class_name) {
                            let field_names: Vec<&String> = symbol.fields.keys().collect();
                            if let Some(idx) = field_names.iter().position(|f| *f == field) {
                                let field_ptr = self.builder.build_struct_gep(
                                    *struct_type,
                                    obj_ptr.into_pointer_value(),
                                    idx as u32,
                                    &format!("{}.{}", class_name, field),
                                ).map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;
                                self.builder.build_store(field_ptr, value_compiled)
                                    .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;
                                return Ok(value_compiled);
                            }
                        }
                    }
                }

                Err(CodeGenError::new(format!(
                    "Cannot assign to field {} on type {:?}",
                    field, obj_type
                )))
            }
            _ => Err(CodeGenError::new(
                "Invalid assignment target".to_string(),
            )),
        }
    }

    /// Compile integer binary operations
    fn compile_int_binary_op(
        &mut self,
        left: inkwell::values::IntValue<'ctx>,
        op: &BinaryOp,
        right: inkwell::values::IntValue<'ctx>,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match op {
            BinaryOp::Add => Ok(self.builder.build_int_add(left, right, "add")
                .map_err(|e| CodeGenError::new(format!("Failed to build int add: {}", e)))?.into()),
            BinaryOp::Subtract => Ok(self.builder.build_int_sub(left, right, "sub")
                .map_err(|e| CodeGenError::new(format!("Failed to build int sub: {}", e)))?.into()),
            BinaryOp::Multiply => Ok(self.builder.build_int_mul(left, right, "mul")
                .map_err(|e| CodeGenError::new(format!("Failed to build int mul: {}", e)))?.into()),
            BinaryOp::Divide => Ok(self.builder.build_int_signed_div(left, right, "div")
                .map_err(|e| CodeGenError::new(format!("Failed to build int div: {}", e)))?.into()),
            BinaryOp::Modulo => Ok(self.builder.build_int_signed_rem(left, right, "rem")
                .map_err(|e| CodeGenError::new(format!("Failed to build int rem: {}", e)))?.into()),
            BinaryOp::Equal => Ok(self
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "eq")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            BinaryOp::NotEqual => Ok(self
                .builder
                .build_int_compare(IntPredicate::NE, left, right, "ne")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            BinaryOp::Less => Ok(self
                .builder
                .build_int_compare(IntPredicate::SLT, left, right, "lt")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            BinaryOp::LessEqual => Ok(self
                .builder
                .build_int_compare(IntPredicate::SLE, left, right, "le")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            BinaryOp::Greater => Ok(self
                .builder
                .build_int_compare(IntPredicate::SGT, left, right, "gt")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            BinaryOp::GreaterEqual => Ok(self
                .builder
                .build_int_compare(IntPredicate::SGE, left, right, "ge")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            _ => Err(CodeGenError::new(format!(
                "Unsupported binary operator for integers: {:?}",
                op
            ))),
        }
    }

    /// Compile float binary operations
    fn compile_float_binary_op(
        &mut self,
        left: inkwell::values::FloatValue<'ctx>,
        op: &BinaryOp,
        right: inkwell::values::FloatValue<'ctx>,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match op {
            BinaryOp::Add => Ok(self.builder.build_float_add(left, right, "fadd")
                .map_err(|e| CodeGenError::new(format!("Failed to build float add: {}", e)))?.into()),
            BinaryOp::Subtract => Ok(self.builder.build_float_sub(left, right, "fsub")
                .map_err(|e| CodeGenError::new(format!("Failed to build float sub: {}", e)))?.into()),
            BinaryOp::Multiply => Ok(self.builder.build_float_mul(left, right, "fmul")
                .map_err(|e| CodeGenError::new(format!("Failed to build float mul: {}", e)))?.into()),
            BinaryOp::Divide => Ok(self.builder.build_float_div(left, right, "fdiv")
                .map_err(|e| CodeGenError::new(format!("Failed to build float div: {}", e)))?.into()),
            BinaryOp::Equal => Ok(self
                .builder
                .build_float_compare(FloatPredicate::OEQ, left, right, "feq")
                .map_err(|e| CodeGenError::new(format!("Failed to build float compare: {}", e)))?
                .into()),
            BinaryOp::NotEqual => Ok(self
                .builder
                .build_float_compare(FloatPredicate::ONE, left, right, "fne")
                .map_err(|e| CodeGenError::new(format!("Failed to build float compare: {}", e)))?
                .into()),
            BinaryOp::Less => Ok(self
                .builder
                .build_float_compare(FloatPredicate::OLT, left, right, "flt")
                .map_err(|e| CodeGenError::new(format!("Failed to build float compare: {}", e)))?
                .into()),
            BinaryOp::LessEqual => Ok(self
                .builder
                .build_float_compare(FloatPredicate::OLE, left, right, "fle")
                .map_err(|e| CodeGenError::new(format!("Failed to build float compare: {}", e)))?
                .into()),
            BinaryOp::Greater => Ok(self
                .builder
                .build_float_compare(FloatPredicate::OGT, left, right, "fgt")
                .map_err(|e| CodeGenError::new(format!("Failed to build float compare: {}", e)))?
                .into()),
            BinaryOp::GreaterEqual => Ok(self
                .builder
                .build_float_compare(FloatPredicate::OGE, left, right, "fge")
                .map_err(|e| CodeGenError::new(format!("Failed to build float compare: {}", e)))?
                .into()),
            _ => Err(CodeGenError::new(format!(
                "Unsupported binary operator for floats: {:?}",
                op
            ))),
        }
    }

    /// Compile boolean binary operations
    fn compile_bool_binary_op(
        &mut self,
        left: inkwell::values::IntValue<'ctx>,
        op: &BinaryOp,
        right: inkwell::values::IntValue<'ctx>,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match op {
            BinaryOp::LogicalAnd => Ok(self.builder.build_and(left, right, "and")
                .map_err(|e| CodeGenError::new(format!("Failed to build and: {}", e)))?.into()),
            BinaryOp::LogicalOr => Ok(self.builder.build_or(left, right, "or")
                .map_err(|e| CodeGenError::new(format!("Failed to build or: {}", e)))?.into()),
            BinaryOp::Equal => Ok(self
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "eq")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            BinaryOp::NotEqual => Ok(self
                .builder
                .build_int_compare(IntPredicate::NE, left, right, "ne")
                .map_err(|e| CodeGenError::new(format!("Failed to build int compare: {}", e)))?
                .into()),
            _ => Err(CodeGenError::new(format!(
                "Unsupported binary operator for booleans: {:?}",
                op
            ))),
        }
    }

    /// Compile string binary operations
    fn compile_string_binary_op(
        &mut self,
        left: PointerValue<'ctx>,
        op: &BinaryOp,
        right: PointerValue<'ctx>,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match op {
            BinaryOp::Add => {
                // String concatenation
                let concat_fn = self
                    .runtime_functions
                    .get("mux_string_concat")
                    .ok_or_else(|| CodeGenError::new("mux_string_concat not declared"))?;

                let result = self
                    .builder
                    .build_call(*concat_fn, &[left.into(), right.into()], "concat")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value from mux_string_concat"))?;

                Ok(result)
            }
            BinaryOp::Equal => {
                // String equality comparison
                let equals_fn = self
                    .runtime_functions
                    .get("mux_string_equals")
                    .ok_or_else(|| CodeGenError::new("mux_string_equals not declared"))?;

                let result = self
                    .builder
                    .build_call(*equals_fn, &[left.into(), right.into()], "str_eq")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value from mux_string_equals"))?;

                Ok(result)
            }
            BinaryOp::NotEqual => {
                // String inequality comparison (negate the equality result)
                let equals_fn = self
                    .runtime_functions
                    .get("mux_string_equals")
                    .ok_or_else(|| CodeGenError::new("mux_string_equals not declared"))?;

                let eq_result = self
                    .builder
                    .build_call(*equals_fn, &[left.into(), right.into()], "str_eq")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value from mux_string_equals"))?;

                // Negate the result for inequality
                let negated = self
                    .builder
                    .build_not(eq_result.into_int_value(), "str_ne")
                    .map_err(|e| CodeGenError::new(format!("Failed to build not: {}", e)))?;

                Ok(negated.into())
            }
            _ => Err(CodeGenError::new(format!(
                "Unsupported binary operator for strings: {:?}",
                op
            ))),
        }
    }

    /// Compile unary expression
    fn compile_unary_expression(
        &mut self,
        op: &UnaryOp,
        expr: &ExpressionNode,
        postfix: bool,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let expr_type = self.infer_expression_type(expr)?;

        match op {
            UnaryOp::Neg => {
                let value = self.compile_expression(expr)?;
                match expr_type {
                    Type::Primitive(PrimitiveType::Int) => {
                        Ok(self.builder.build_int_neg(value.into_int_value(), "neg")
                            .map_err(|e| CodeGenError::new(format!("Failed to build int neg: {}", e)))?.into())
                    }
                    Type::Primitive(PrimitiveType::Float) => {
                        Ok(self.builder.build_float_neg(value.into_float_value(), "fneg")
                            .map_err(|e| CodeGenError::new(format!("Failed to build float neg: {}", e)))?.into())
                    }
                    _ => Err(CodeGenError::new(format!(
                        "Cannot negate type {:?}",
                        expr_type
                    ))),
                }
            }
            UnaryOp::Not => {
                let value = self.compile_expression(expr)?;
                Ok(self.builder.build_not(value.into_int_value(), "not")
                    .map_err(|e| CodeGenError::new(format!("Failed to build not: {}", e)))?.into())
            }
            UnaryOp::Ref => {
                let value = self.compile_expression(expr)?;
                // For references, we need to get the address
                // This is a placeholder - proper implementation depends on context
                Ok(value)
            }
            UnaryOp::Deref => {
                let value = self.compile_expression(expr)?;
                // Dereference a pointer
                // This is a placeholder - proper implementation depends on context
                Ok(value)
            }
            UnaryOp::Incr => {
                // Increment operator (++x or x++)
                self.compile_incr_decr(expr, &expr_type, true, postfix)
            }
            UnaryOp::Decr => {
                // Decrement operator (--x or x--)
                self.compile_incr_decr(expr, &expr_type, false, postfix)
            }
        }
    }

    /// Compile increment/decrement operations
    fn compile_incr_decr(
        &mut self,
        expr: &ExpressionNode,
        expr_type: &Type,
        is_incr: bool,
        postfix: bool,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // Get the variable pointer
        let var_ptr = match &expr.kind {
            ExpressionKind::Identifier(name) => {
                let var = self.variables.get(name).ok_or_else(|| {
                    CodeGenError::new(format!("Cannot increment/decrement undefined variable: {}", name))
                })?;
                var.ptr
            }
            _ => {
                return Err(CodeGenError::new(
                    "Increment/decrement requires a variable as operand",
                ));
            }
        };

        match expr_type {
            Type::Primitive(PrimitiveType::Int) => {
                // Load current value
                let current_val = self
                    .builder
                    .build_load(self.context.i64_type(), var_ptr, "curr")
                    .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?
                    .into_int_value();

                // Compute new value
                let one = self.context.i64_type().const_int(1, false);
                let new_val = if is_incr {
                    self.builder.build_int_add(current_val, one, "incr")
                        .map_err(|e| CodeGenError::new(format!("Failed to build int add: {}", e)))?
                } else {
                    self.builder.build_int_sub(current_val, one, "decr")
                        .map_err(|e| CodeGenError::new(format!("Failed to build int sub: {}", e)))?
                };

                // Store new value
                self.builder.build_store(var_ptr, new_val)
                    .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

                // Return old or new value depending on prefix/postfix
                if postfix {
                    Ok(current_val.into())
                } else {
                    Ok(new_val.into())
                }
            }
            Type::Primitive(PrimitiveType::Float) => {
                // Load current value
                let current_val = self
                    .builder
                    .build_load(self.context.f64_type(), var_ptr, "curr")
                    .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?
                    .into_float_value();

                // Compute new value
                let one = self.context.f64_type().const_float(1.0);
                let new_val = if is_incr {
                    self.builder.build_float_add(current_val, one, "fincr")
                        .map_err(|e| CodeGenError::new(format!("Failed to build float add: {}", e)))?
                } else {
                    self.builder.build_float_sub(current_val, one, "fdecr")
                        .map_err(|e| CodeGenError::new(format!("Failed to build float sub: {}", e)))?
                };

                // Store new value
                self.builder.build_store(var_ptr, new_val)
                    .map_err(|e| CodeGenError::new(format!("Failed to build store: {}", e)))?;

                // Return old or new value depending on prefix/postfix
                if postfix {
                    Ok(current_val.into())
                } else {
                    Ok(new_val.into())
                }
            }
            _ => Err(CodeGenError::new(format!(
                "Cannot increment/decrement type {:?}",
                expr_type
            ))),
        }
    }

    /// Compile a function call expression
    fn compile_call_expression(
        &mut self,
        func: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // Compile arguments
        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .map(|arg| {
                let val = self.compile_expression(arg)?;
                Ok(val.into())
            })
            .collect::<CodeGenResult<Vec<_>>>()?;

        match &func.kind {
            ExpressionKind::Identifier(name) => {
                self.compile_function_call(name, &compiled_args)
            }
            ExpressionKind::FieldAccess { expr, field } => {
                // Method call
                self.compile_method_call(expr, field, &compiled_args)
            }
            ExpressionKind::GenericType(name, _type_args) => {
                // Generic type constructor call (e.g., Stack<int>.new())
                let constructor_name = format!("{}.new", name);
                self.compile_function_call(&constructor_name, &compiled_args)
            }
            _ => Err(CodeGenError::new("Invalid call expression")),
        }
    }

    /// Compile a direct function call
    fn compile_function_call(
        &mut self,
        name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // Check for built-in functions first
        match name {
            "print" => {
                let print_fn = self
                    .runtime_functions
                    .get("mux_print")
                    .ok_or_else(|| CodeGenError::new("mux_print not declared"))?;
                self.builder.build_call(*print_fn, args, "print_call")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;
                // print returns void, so return a dummy value
                Ok(self.context.i64_type().const_int(0, false).into())
            }
            "println" => {
                let println_fn = self
                    .runtime_functions
                    .get("mux_println")
                    .ok_or_else(|| CodeGenError::new("mux_println not declared"))?;
                self.builder.build_call(*println_fn, args, "println_call")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;
                Ok(self.context.i64_type().const_int(0, false).into())
            }
            "range" => {
                let range_fn = self
                    .runtime_functions
                    .get("mux_range")
                    .ok_or_else(|| CodeGenError::new("mux_range not declared"))?;
                let result = self
                    .builder
                    .build_call(*range_fn, args, "range_call")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value from mux_range"))?;
                Ok(result)
            }
            "Some" => {
                // Some constructor - wrap value in optional
                // For now, just return the value
                if args.is_empty() {
                    return Err(CodeGenError::new("Some() requires an argument"));
                }
                Ok(args[0].into_pointer_value().into())
            }
            "None" => {
                // None - return null pointer
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .const_null()
                    .into())
            }
            _ => {
                // User-defined function
                if let Some(compiled) = self.functions.get(name).cloned() {
                    let call = self
                        .builder
                        .build_call(compiled.function, args, &format!("{}_call", name))
                        .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;

                    match compiled.return_type {
                        Type::Void => Ok(self.context.i64_type().const_int(0, false).into()),
                        _ => call.try_as_basic_value().left().ok_or_else(|| {
                            CodeGenError::new(format!(
                                "Expected return value from function {}",
                                name
                            ))
                        }),
                    }
                } else {
                    Err(CodeGenError::new(format!("Unknown function: {}", name)))
                }
            }
        }
    }

    /// Compile a method call
    fn compile_method_call(
        &mut self,
        receiver: &ExpressionNode,
        method: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let receiver_val = self.compile_expression(receiver)?;
        let receiver_type = self.infer_expression_type(receiver)?;

        // Check for built-in methods on primitives
        match &receiver_type {
            Type::Primitive(PrimitiveType::Int) => {
                return self.compile_int_method(receiver_val.into_int_value(), method);
            }
            Type::Primitive(PrimitiveType::Float) => {
                return self.compile_float_method(receiver_val.into_float_value(), method);
            }
            Type::Primitive(PrimitiveType::Bool) => {
                return self.compile_bool_method(receiver_val.into_int_value(), method);
            }
            Type::Primitive(PrimitiveType::Str) => {
                return self.compile_string_method(receiver_val.into_pointer_value(), method, args);
            }
            Type::List(_) => {
                return self.compile_list_method(receiver_val.into_pointer_value(), method, args);
            }
            Type::Named(class_name, _) => {
                // Class method call
                let method_name = format!("{}.{}", class_name, method);
                if self.functions.contains_key(&method_name) {
                    // Add self as first argument
                    let mut all_args = vec![receiver_val.into()];
                    all_args.extend(args.iter().cloned());
                    return self.compile_function_call(&method_name, &all_args);
                }

                // Check if it's accessing a static constructor like ClassName.new
                if method == "new" {
                    let constructor_name = format!("{}.new", class_name);
                    return self.compile_function_call(&constructor_name, args);
                }
            }
            _ => {}
        }

        Err(CodeGenError::new(format!(
            "Unknown method {} on type {:?}",
            method, receiver_type
        )))
    }

    /// Compile int methods
    fn compile_int_method(
        &mut self,
        value: inkwell::values::IntValue<'ctx>,
        method: &str,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match method {
            "to_string" => {
                let to_string_fn = self
                    .runtime_functions
                    .get("mux_int_to_string")
                    .ok_or_else(|| CodeGenError::new("mux_int_to_string not declared"))?;
                let result = self
                    .builder
                    .build_call(*to_string_fn, &[value.into()], "to_string")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value"))?;
                Ok(result)
            }
            "to_float" => {
                let result = self.builder.build_signed_int_to_float(
                    value,
                    self.context.f64_type(),
                    "to_float",
                ).map_err(|e| CodeGenError::new(format!("Failed to build int to float: {}", e)))?;
                Ok(result.into())
            }
            _ => Err(CodeGenError::new(format!(
                "Unknown method {} on int",
                method
            ))),
        }
    }

    /// Compile float methods
    fn compile_float_method(
        &mut self,
        value: inkwell::values::FloatValue<'ctx>,
        method: &str,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match method {
            "to_string" => {
                let to_string_fn = self
                    .runtime_functions
                    .get("mux_float_to_string")
                    .ok_or_else(|| CodeGenError::new("mux_float_to_string not declared"))?;
                let result = self
                    .builder
                    .build_call(*to_string_fn, &[value.into()], "to_string")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value"))?;
                Ok(result)
            }
            "to_int" => {
                let result = self.builder.build_float_to_signed_int(
                    value,
                    self.context.i64_type(),
                    "to_int",
                ).map_err(|e| CodeGenError::new(format!("Failed to build float to int: {}", e)))?;
                Ok(result.into())
            }
            _ => Err(CodeGenError::new(format!(
                "Unknown method {} on float",
                method
            ))),
        }
    }

    /// Compile bool methods
    fn compile_bool_method(
        &mut self,
        value: inkwell::values::IntValue<'ctx>,
        method: &str,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match method {
            "to_string" => {
                let to_string_fn = self
                    .runtime_functions
                    .get("mux_bool_to_string")
                    .ok_or_else(|| CodeGenError::new("mux_bool_to_string not declared"))?;
                let result = self
                    .builder
                    .build_call(*to_string_fn, &[value.into()], "to_string")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value"))?;
                Ok(result)
            }
            _ => Err(CodeGenError::new(format!(
                "Unknown method {} on bool",
                method
            ))),
        }
    }

    /// Compile string methods
    fn compile_string_method(
        &mut self,
        value: PointerValue<'ctx>,
        method: &str,
        _args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match method {
            "length" => {
                let length_fn = self
                    .runtime_functions
                    .get("mux_string_length")
                    .ok_or_else(|| CodeGenError::new("mux_string_length not declared"))?;
                let result = self
                    .builder
                    .build_call(*length_fn, &[value.into()], "length")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value"))?;
                Ok(result)
            }
            "to_string" => {
                // String.to_string returns itself
                Ok(value.into())
            }
            _ => Err(CodeGenError::new(format!(
                "Unknown method {} on string",
                method
            ))),
        }
    }

    /// Compile list methods
    fn compile_list_method(
        &mut self,
        list_ptr: PointerValue<'ctx>,
        method: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        match method {
            "push" | "push_back" => {
                let push_fn = self
                    .runtime_functions
                    .get("mux_list_push")
                    .ok_or_else(|| CodeGenError::new("mux_list_push not declared"))?;
                let mut all_args = vec![list_ptr.into()];
                all_args.extend(args.iter().cloned());
                self.builder.build_call(*push_fn, &all_args, "push")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;
                Ok(self.context.i64_type().const_int(0, false).into())
            }
            "get" => {
                let get_fn = self
                    .runtime_functions
                    .get("mux_list_get")
                    .ok_or_else(|| CodeGenError::new("mux_list_get not declared"))?;
                let mut all_args = vec![list_ptr.into()];
                all_args.extend(args.iter().cloned());
                let result = self
                    .builder
                    .build_call(*get_fn, &all_args, "get")
                    .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| CodeGenError::new("Expected return value from list.get"))?;
                Ok(result)
            }
            _ => Err(CodeGenError::new(format!(
                "Unknown method {} on list",
                method
            ))),
        }
    }

    /// Compile field access
    fn compile_field_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let obj_val = self.compile_expression(expr)?;
        let obj_type = self.infer_expression_type(expr)?;

        if let Type::Named(class_name, _) = &obj_type {
            if let Some(struct_type) = self.struct_types.get(class_name) {
                if let Some(symbol) = self.analyzer.all_symbols().get(class_name) {
                    // Find the field index
                    let field_names: Vec<&String> = symbol.fields.keys().collect();
                    if let Some(idx) = field_names.iter().position(|f| *f == field) {
                        let field_type = symbol.fields.get(field).ok_or_else(|| {
                            CodeGenError::new(format!("Field {} not found in {}", field, class_name))
                        })?;
                        let llvm_field_type = self.type_to_llvm(field_type)?;

                        let field_ptr = self.builder.build_struct_gep(
                            *struct_type,
                            obj_val.into_pointer_value(),
                            idx as u32,
                            &format!("{}.{}", class_name, field),
                        ).map_err(|e| CodeGenError::new(format!("Failed to build struct gep: {}", e)))?;

                        let value = self.builder.build_load(llvm_field_type, field_ptr, field)
                            .map_err(|e| CodeGenError::new(format!("Failed to build load: {}", e)))?;
                        return Ok(value);
                    }
                }
            }

            // If not a field, it might be a static method reference
            return Ok(obj_val);
        }

        Err(CodeGenError::new(format!(
            "Cannot access field {} on type {:?}",
            field, obj_type
        )))
    }

    /// Compile list access (indexing)
    fn compile_list_access(
        &mut self,
        expr: &ExpressionNode,
        index: &ExpressionNode,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let list_ptr = self.compile_expression(expr)?;
        let index_val = self.compile_expression(index)?;

        let get_fn = self
            .runtime_functions
            .get("mux_list_get")
            .ok_or_else(|| CodeGenError::new("mux_list_get not declared"))?;

        let result = self
            .builder
            .build_call(
                *get_fn,
                &[list_ptr.into(), index_val.into()],
                "list_access",
            )
            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodeGenError::new("Expected return value from mux_list_get"))?;

        Ok(result)
    }

    /// Compile list literal
    fn compile_list_literal(
        &mut self,
        elements: &[ExpressionNode],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let new_list_fn = self
            .runtime_functions
            .get("mux_new_list")
            .ok_or_else(|| CodeGenError::new("mux_new_list not declared"))?;

        let list_ptr = self
            .builder
            .build_call(*new_list_fn, &[], "new_list")
            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodeGenError::new("Expected return value from mux_new_list"))?;

        let push_fn = self
            .runtime_functions
            .get("mux_list_push")
            .ok_or_else(|| CodeGenError::new("mux_list_push not declared"))?;

        for elem in elements {
            let elem_val = self.compile_expression(elem)?;
            self.builder.build_call(
                *push_fn,
                &[list_ptr.into(), elem_val.into()],
                "push",
            ).map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;
        }

        Ok(list_ptr)
    }

    /// Compile map literal
    fn compile_map_literal(
        &mut self,
        entries: &[(ExpressionNode, ExpressionNode)],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let new_map_fn = self
            .runtime_functions
            .get("mux_new_map")
            .ok_or_else(|| CodeGenError::new("mux_new_map not declared"))?;

        let map_ptr = self
            .builder
            .build_call(*new_map_fn, &[], "new_map")
            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodeGenError::new("Expected return value from mux_new_map"))?;

        let put_fn = self
            .runtime_functions
            .get("mux_map_put")
            .ok_or_else(|| CodeGenError::new("mux_map_put not declared"))?;

        for (key, value) in entries {
            let key_val = self.compile_expression(key)?;
            let value_val = self.compile_expression(value)?;
            self.builder.build_call(
                *put_fn,
                &[map_ptr.into(), key_val.into(), value_val.into()],
                "put",
            ).map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;
        }

        Ok(map_ptr)
    }

    /// Compile set literal
    fn compile_set_literal(
        &mut self,
        elements: &[ExpressionNode],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let new_set_fn = self
            .runtime_functions
            .get("mux_new_set")
            .ok_or_else(|| CodeGenError::new("mux_new_set not declared"))?;

        let set_ptr = self
            .builder
            .build_call(*new_set_fn, &[], "new_set")
            .map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodeGenError::new("Expected return value from mux_new_set"))?;

        let add_fn = self
            .runtime_functions
            .get("mux_set_add")
            .ok_or_else(|| CodeGenError::new("mux_set_add not declared"))?;

        for elem in elements {
            let elem_val = self.compile_expression(elem)?;
            self.builder.build_call(
                *add_fn,
                &[set_ptr.into(), elem_val.into()],
                "add",
            ).map_err(|e| CodeGenError::new(format!("Failed to build call: {}", e)))?;
        }

        Ok(set_ptr)
    }

    /// Compile if expression (ternary)
    fn compile_if_expression(
        &mut self,
        cond: &ExpressionNode,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        let cond_value = self.compile_expression(cond)?;
        let cond_bool = cond_value.into_int_value();

        let function = self.current_function.ok_or_else(|| {
            CodeGenError::new("Cannot compile if expression outside of a function")
        })?;

        let then_bb = self.context.append_basic_block(function, "ifexpr.then");
        let else_bb = self.context.append_basic_block(function, "ifexpr.else");
        let merge_bb = self.context.append_basic_block(function, "ifexpr.merge");

        self.builder.build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build conditional branch: {}", e)))?;

        // Compile then branch
        self.builder.position_at_end(then_bb);
        let then_value = self.compile_expression(then_expr)?;
        self.builder.build_unconditional_branch(merge_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;
        let then_bb = self.builder.get_insert_block().unwrap();

        // Compile else branch
        self.builder.position_at_end(else_bb);
        let else_value = self.compile_expression(else_expr)?;
        self.builder.build_unconditional_branch(merge_bb)
            .map_err(|e| CodeGenError::new(format!("Failed to build unconditional branch: {}", e)))?;
        let else_bb = self.builder.get_insert_block().unwrap();

        // Merge
        self.builder.position_at_end(merge_bb);
        let result_type = self.infer_expression_type(then_expr)?;
        let llvm_type = self.type_to_llvm(&result_type)?;

        let phi = self.builder.build_phi(llvm_type, "ifexpr.result")
            .map_err(|e| CodeGenError::new(format!("Failed to build phi: {}", e)))?;
        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

        Ok(phi.as_basic_value())
    }

    /// Compile lambda expression
    fn compile_lambda(
        &mut self,
        params: &[Param],
        body: &[StatementNode],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // Create a unique name for the lambda
        let lambda_name = self.unique_name("lambda");

        // Create a function node to reuse existing machinery
        let func_node = FunctionNode {
            name: lambda_name.clone(),
            type_params: vec![],
            params: params.to_vec(),
            return_type: TypeNode {
                kind: TypeKind::Primitive(PrimitiveType::Void), // TODO: infer return type
                span: crate::lexer::Span::new(0, 0),
            },
            body: body.to_vec(),
            span: crate::lexer::Span::new(0, 0),
            is_common: false,
        };

        self.declare_function(&func_node)?;
        self.compile_function(&func_node)?;

        // Return a pointer to the compiled function
        let func = self
            .functions
            .get(&lambda_name)
            .ok_or_else(|| CodeGenError::new("Lambda not found after compilation"))?;

        Ok(func.function.as_global_value().as_pointer_value().into())
    }

    /// Compile generic type expression
    fn compile_generic_type_expr(
        &mut self,
        name: &str,
        _type_args: &[TypeNode],
    ) -> CodeGenResult<BasicValueEnum<'ctx>> {
        // For generic types used as expressions (e.g., Stack<int>),
        // we need to return a reference to the type's constructor or factory
        // For now, return a placeholder

        // Check if this is a class - return constructor reference
        if self.struct_types.contains_key(name) {
            let constructor_name = format!("{}.new", name);
            if let Some(compiled) = self.functions.get(&constructor_name) {
                return Ok(compiled.function.as_global_value().as_pointer_value().into());
            }
        }

        Err(CodeGenError::new(format!(
            "Cannot use generic type {} as expression",
            name
        )))
    }

    // ========================================================================
    // Phase 2.6: Main Function Generation
    // ========================================================================

    /// Generate the main function from top-level statements
    fn generate_main(&mut self, statements: &[StatementNode]) -> CodeGenResult<()> {
        // Create main function: int main(int argc, char** argv)
        let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);

        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        self.current_function = Some(main_fn);

        // Compile all statements
        for stmt in statements {
            self.compile_statement(stmt)?;
        }

        // Return 0
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_return(Some(&i32_type.const_int(0, false)))
                .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;
        }

        self.current_function = None;

        Ok(())
    }

    /// Generate an empty main function
    fn generate_empty_main(&mut self) -> CodeGenResult<()> {
        let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);

        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        self.builder.build_return(Some(&i32_type.const_int(0, false)))
            .map_err(|e| CodeGenError::new(format!("Failed to build return: {}", e)))?;

        Ok(())
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    /// Infer the type of an expression
    fn infer_expression_type(&mut self, expr: &ExpressionNode) -> CodeGenResult<Type> {
        match self.analyzer.get_expression_type(expr) {
            Ok(t) => Ok(t),
            Err(e) => Err(CodeGenError::new(format!(
                "Type inference failed: {}",
                e.message
            ))),
        }
    }
}
