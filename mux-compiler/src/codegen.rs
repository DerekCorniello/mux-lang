use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::*;
use crate::semantics::{SemanticAnalyzer, Type, Type as ResolvedType};

pub struct CodeGenerator<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    analyzer: &'a SemanticAnalyzer,
    type_map: HashMap<String, BasicTypeEnum<'a>>,
    #[allow(dead_code)]
    vtable_map: HashMap<String, PointerValue<'a>>,
    enum_variants: HashMap<String, Vec<String>>,
    field_map: HashMap<String, HashMap<String, usize>>,
    lambda_counter: usize,
    string_counter: usize,
    label_counter: usize,
    variables: HashMap<String, (PointerValue<'a>, BasicTypeEnum<'a>, ResolvedType)>,
    functions: HashMap<String, FunctionValue<'a>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(context: &'a Context, analyzer: &'a SemanticAnalyzer) -> Self {
        let module = context.create_module("mux_module");
        let builder = context.create_builder();

        // Declare runtime functions
        let void_type = context.void_type();
        let i64_type = context.i64_type();
        let f64_type = context.f64_type();
        let bool_type = context.bool_type();
        let i8_ptr = context.ptr_type(AddressSpace::default());
        let list_ptr = i8_ptr; // placeholder for *mut List

        // mux_value_from_string: (*const c_char) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_value_from_string", fn_type, None);

        // mux_new_string_from_cstr: (*const c_char) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_new_string_from_cstr", fn_type, None);

        // mux_print: (*mut Value) -> ()
        let params = &[i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_print", fn_type, None);

        // mux_string_concat: (*const c_char, *const c_char) -> *mut c_char
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_concat", fn_type, None);

        // mux_int_to_string: (i64) -> *const c_char
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_int_to_string", fn_type, None);

        // mux_float_to_string: (f64) -> *const c_char
        let params = &[f64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_float_to_string", fn_type, None);

        // mux_bool_to_string: (bool) -> *const c_char
        let params = &[bool_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_bool_to_string", fn_type, None);

        // mux_string_to_string: (*const c_char) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_to_string", fn_type, None);

        // mux_list_to_string: (*mut List) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_to_string", fn_type, None);

        // mux_map_to_string: (*mut Map) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_map_to_string", fn_type, None);

        // mux_set_to_string: (*mut Set) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_set_to_string", fn_type, None);

        // mux_value_to_string: (*mut Value) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_value_to_string", fn_type, None);

        // mux_list_value: (*mut List) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_value", fn_type, None);

        // mux_map_value: (*mut Map) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_map_value", fn_type, None);

        // mux_set_value: (*mut Set) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_set_value", fn_type, None);

        // mux_range: (i64, i64) -> *mut List
        let params = &[i64_type.into(), i64_type.into()];
        let fn_type = list_ptr.fn_type(params, false);
        module.add_function("mux_range", fn_type, None);

        // Collection constructors
        // mux_new_list: () -> *mut List
        let fn_type = list_ptr.fn_type(&[], false);
        module.add_function("mux_new_list", fn_type, None);

        // mux_new_map: () -> *mut Map
        let fn_type = list_ptr.fn_type(&[], false); // using list_ptr as generic ptr
        module.add_function("mux_new_map", fn_type, None);

        // mux_new_set: () -> *mut Set
        let fn_type = list_ptr.fn_type(&[], false);
        module.add_function("mux_new_set", fn_type, None);

        // mux_value_add: (*mut Value, *mut Value) -> *mut Value
        let fn_type = i8_ptr.fn_type(&[i8_ptr.into(), i8_ptr.into()], false);
        module.add_function("mux_value_add", fn_type, None);

        // List operations
        // mux_list_push_back: (*mut List, *mut Value) -> ()
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push_back", fn_type, None);

        // mux_list_get: (*const List, i64) -> *mut Optional
        let params = &[list_ptr.into(), i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_get", fn_type, None);

        // mux_list_length: (*const List) -> i64
        let params = &[list_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_list_length", fn_type, None);

        // mux_list_pop_back: (*mut List) -> *mut Optional
        let params = &[list_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_pop_back", fn_type, None);

        // mux_list_is_empty: (*const List) -> bool
        let params = &[list_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_list_is_empty", fn_type, None);

        // Map operations
        // mux_map_put: (*mut Map, *mut Value, *mut Value) -> ()
        let params = &[list_ptr.into(), i8_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_map_put", fn_type, None);

        // mux_map_get: (*const Map, *const Value) -> *mut Optional
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_map_get", fn_type, None);

        // mux_map_contains: (*const Map, *const Value) -> bool
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_map_contains", fn_type, None);

        // mux_map_remove: (*mut Map, *const Value) -> *mut Optional
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_map_remove", fn_type, None);

        // Set operations
        // mux_set_add: (*mut Set, *mut Value) -> ()
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_set_add", fn_type, None);

        // mux_set_contains: (*const Set, *const Value) -> bool
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_set_contains", fn_type, None);

        // mux_set_remove: (*mut Set, *const Value) -> bool
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_set_remove", fn_type, None);

        // mux_set_size: (*const Set) -> i64
        let params = &[list_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_set_size", fn_type, None);

        // mux_set_is_empty: (*const Set) -> bool
        let params = &[list_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_set_is_empty", fn_type, None);

        // Additional map operations
        // mux_map_size: (*const Map) -> i64
        let params = &[list_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_map_size", fn_type, None);

        // mux_map_is_empty: (*const Map) -> bool
        let params = &[list_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_map_is_empty", fn_type, None);

        // Value creation functions
        // mux_int_value: (i64) -> *mut Value
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_int_value", fn_type, None);

        // mux_float_value: (f64) -> *mut Value
        let params = &[context.f64_type().into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_float_value", fn_type, None);

        // mux_bool_value: (bool) -> *mut Value
        let params = &[context.bool_type().into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_bool_value", fn_type, None);

        // mux_string_value: (*const c_char) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_value", fn_type, None);

        // mux_int_to_string: (i64) -> *const c_char
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_int_to_string", fn_type, None);

        // mux_int_from_value: (*mut Value) -> i64
        let params = &[i8_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_int_from_value", fn_type, None);

        // mux_float_from_value: (*mut Value) -> f64
        let params = &[i8_ptr.into()];
        let fn_type = f64_type.fn_type(params, false);
        module.add_function("mux_float_from_value", fn_type, None);

        // mux_bool_from_value: (*mut Value) -> i1
        let params = &[i8_ptr.into()];
        let fn_type = bool_type.fn_type(params, false);
        module.add_function("mux_bool_from_value", fn_type, None);

        // mux_string_from_value: (*mut Value) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_from_value", fn_type, None);

        // Result constructors
        // mux_result_ok_int: (i64) -> *mut MuxResult
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_int", fn_type, None);

        // mux_result_err_str: (*const c_char) -> *mut MuxResult
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_err_str", fn_type, None);

        // mux_optional_some_int: (i64) -> *mut Optional
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_some_int", fn_type, None);

        // mux_optional_none: () -> *mut Optional
        let fn_type = i8_ptr.fn_type(&[], false);
        module.add_function("mux_optional_none", fn_type, None);

        // mux_result_ok_int: (i64) -> *mut MuxResult
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_int", fn_type, None);

        // mux_result_err_str: (*const c_char) -> *mut MuxResult
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_err_str", fn_type, None);

        // mux_optional_discriminant: (*mut Optional) -> i32
        let params = &[i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_optional_discriminant", fn_type, None);

        // mux_optional_data: (*mut Optional) -> *mut Value
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_data", fn_type, None);

        // mux_result_discriminant: (*mut MuxResult) -> i32
        let params = &[i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_result_discriminant", fn_type, None);

        // mux_result_data: (*mut MuxResult) -> *mut Value
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_data", fn_type, None);

        let mut type_map = HashMap::new();
        let mut enum_variants = HashMap::new();

        // Built-in enum types
        let i32_type = context.i32_type();
        let i8_ptr = context.ptr_type(AddressSpace::default());
        let struct_type = context.struct_type(&[i32_type.into(), i8_ptr.into()], false);
        type_map.insert("Optional".to_string(), struct_type.into());
        type_map.insert("Result".to_string(), struct_type.into());
        enum_variants.insert(
            "Optional".to_string(),
            vec!["Some".to_string(), "None".to_string()],
        );
        enum_variants.insert(
            "Result".to_string(),
            vec!["Ok".to_string(), "Err".to_string()],
        );

        Self {
            context,
            module,
            builder,
            analyzer,
            type_map,
            vtable_map: HashMap::new(),
            enum_variants,
            field_map: HashMap::new(),
            lambda_counter: 0,
            string_counter: 0,
            label_counter: 0,
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn generate_user_defined_types(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        // Generate LLVM types for classes, interfaces, enums
        for node in nodes {
            match node {
                AstNode::Class { name, fields, .. } => {
                    self.generate_class_type(name, fields)?;
                }
                AstNode::Interface { name, .. } => {
                    self.generate_interface_type(name)?;
                }
                AstNode::Enum { name, variants, .. } => {
                    self.generate_enum_type(name, variants)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn generate_class_type(&mut self, name: &str, fields: &[Field]) -> Result<(), String> {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();
        for (i, field) in fields.iter().enumerate() {
            let field_type = self.llvm_type_from_mux_type(&field.type_)?;
            field_types.push(field_type);
            field_indices.insert(field.name.clone(), i);
        }

        let struct_type = self.context.struct_type(&field_types, false);
        self.type_map.insert(name.to_string(), struct_type.into());
        self.field_map.insert(name.to_string(), field_indices);
        Ok(())
    }

    fn generate_interface_type(&mut self, _name: &str) -> Result<(), String> {
        // For now, interfaces are just pointers - vtable support later
        let _ptr_type = self.context.ptr_type(AddressSpace::default());
        // self.type_map.insert(name.to_string(), ptr_type.into());
        Ok(())
    }

    fn generate_enum_type(&mut self, name: &str, variants: &[EnumVariant]) -> Result<(), String> {
        // Simple tagged union: {i32 discriminant, i8* data}
        let i32_type = self.context.i32_type();
        let i8_ptr = self.context.ptr_type(AddressSpace::default());

        let mut variant_names = Vec::new();
        for variant in variants {
            variant_names.push(variant.name.clone());
        }
        self.enum_variants.insert(name.to_string(), variant_names);

        let struct_type = self
            .context
            .struct_type(&[i32_type.into(), i8_ptr.into()], false);
        self.type_map.insert(name.to_string(), struct_type.into());
        Ok(())
    }

    fn get_variant_index(&self, enum_name: &str, variant_name: &str) -> Result<usize, String> {
        if let Some(variants) = self.enum_variants.get(enum_name) {
            variants
                .iter()
                .position(|v| v == variant_name)
                .ok_or_else(|| format!("Variant {} not found in enum {}", variant_name, enum_name))
        } else {
            Err(format!("Enum {} not found", enum_name))
        }
    }

    fn generate_if_expression(
        &mut self,
        cond: &ExpressionNode,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let cond_val = self.generate_expression(cond)?;

        // Get current function
        let current_bb = self.builder.get_insert_block().unwrap();
        let function = current_bb.get_parent().unwrap();

        // Create blocks
        let then_bb = self.context.append_basic_block(function, "if_then");
        let else_bb = self.context.append_basic_block(function, "if_else");
        let merge_bb = self.context.append_basic_block(function, "if_merge");

        // Conditional branch
        self.builder
            .build_conditional_branch(cond_val.into_int_value(), then_bb, else_bb)
            .map_err(|e| e.to_string())?;

        // Then block
        self.builder.position_at_end(then_bb);
        let then_val = self.generate_expression(then_expr)?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let then_bb_end = self.builder.get_insert_block().unwrap();

        // Else block
        self.builder.position_at_end(else_bb);
        let else_val = self.generate_expression(else_expr)?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let else_bb_end = self.builder.get_insert_block().unwrap();

        // Merge with phi
        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(then_val.get_type(), "if_result")
            .map_err(|e| e.to_string())?;
        phi.add_incoming(&[(&then_val, then_bb_end), (&else_val, else_bb_end)]);

        Ok(phi.as_basic_value())
    }

    fn generate_lambda_expression(
        &mut self,
        params: &[Param],
        body: &[StatementNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // Save current insert block
        let old_bb = self.builder.get_insert_block();

        // Generate unique function name
        let func_name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;

        // Convert params to LLVM types
        let mut param_types = Vec::new();
        for param in params {
            let param_type = self.llvm_type_from_mux_type(&param.type_)?;
            param_types.push(param_type.into());
        }

        // Determine return type from body
        let return_type_opt: Option<BasicTypeEnum<'a>> = if let Some(StatementNode {
            kind: StatementKind::Return(Some(_)),
            ..
        }) = body.last()
        {
            Some(match &params[0].type_.kind {
                TypeKind::Primitive(PrimitiveType::Int) => self.context.i64_type().into(),
                TypeKind::Primitive(PrimitiveType::Float) => self.context.f64_type().into(),
                _ => unreachable!(),
            })
        } else {
            None
        };
        let fn_type = if let Some(rt) = return_type_opt {
            rt.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        // Create the function
        let function = self.module.add_function(&func_name, fn_type, None);

        // Set parameter names
        for (i, param) in params.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            arg.set_name(&param.name);
        }

        // Create entry block and set up parameters
        let entry_bb = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_bb);

        // Save current variables and create new scope
        let old_variables = self.variables.clone();
        self.variables.clear();

        // Set up parameter variables
        for (i, param) in params.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            let boxed = self.box_value(arg);
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let alloca = self
                .builder
                .build_alloca(ptr_type, &param.name)
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, boxed)
                .map_err(|e| e.to_string())?;
            let resolved_type = self
                .analyzer
                .resolve_type(&param.type_)
                .map_err(|e| e.to_string())?;
            self.variables.insert(
                param.name.clone(),
                (alloca, BasicTypeEnum::PointerType(ptr_type), resolved_type),
            );
        }

        // Generate body
        for stmt in body {
            self.generate_statement(stmt, Some(&function))?;
        }

        // Add implicit return if no explicit return
        if !body
            .last()
            .is_some_and(|s| matches!(s.kind, StatementKind::Return(_)))
        {
            self.builder.build_return(None).map_err(|e| e.to_string())?;
        }

        // Restore variables
        self.variables = old_variables;

        // Restore builder to previous block
        if let Some(bb) = old_bb {
            self.builder.position_at_end(bb);
        }

        // Return function pointer
        Ok(function.as_global_value().as_pointer_value().into())
    }

    pub fn generate(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        println!("Starting codegen with {} nodes", nodes.len());

        // Zero pass: generate LLVM types for user-defined types
        self.generate_user_defined_types(nodes)?;

        // First pass: declare all functions
        for node in nodes {
            if let AstNode::Function(func) = node {
                self.declare_function(func)?;
            }
        }

        // Second pass: generate code
        let mut top_level_statements = vec![];
        for node in nodes {
            match node {
                AstNode::Function(func) => {
                    self.generate_function(func)?;
                }
                AstNode::Statement(stmt) => {
                    top_level_statements.push(stmt.clone());
                }
                _ => {} // Skip classes, interfaces, enums for now
            }
        }

        // Generate main function for top-level statements
        if !top_level_statements.is_empty() {
            let main_type = self.context.void_type().fn_type(&[], false);
            let main_func = self.module.add_function("main", main_type, None);
            let entry = self.context.append_basic_block(main_func, "entry");
            self.builder.position_at_end(entry);
            self.variables.clear(); // Start with clean scope
            for stmt in top_level_statements {
                self.generate_statement(&stmt, Some(&main_func))?;
            }
            self.builder.build_return(None).map_err(|e| e.to_string())?;
        }

        Ok(())
    }

    fn declare_function(&mut self, func: &FunctionNode) -> Result<(), String> {
        let param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type_from_mux_type(&p.type_).map(|t| t.into()))
            .collect::<Result<_, _>>()?;

        let fn_type = if matches!(
            func.return_type.kind,
            TypeKind::Primitive(PrimitiveType::Void)
        ) {
            self.context.void_type().fn_type(&param_types, false)
        } else {
            let return_type = self.llvm_type_from_mux_type(&func.return_type)?;
            return_type.fn_type(&param_types, false)
        };

        let function = self.module.add_function(&func.name, fn_type, None);
        self.functions.insert(func.name.clone(), function);

        Ok(())
    }

    fn generate_function(&mut self, func: &FunctionNode) -> Result<(), String> {
        let function = *self
            .functions
            .get(&func.name)
            .ok_or("Function not declared")?;

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Clear variables for new scope
        self.variables.clear();

        // Set up parameter variables
        for (i, param) in func.params.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            let boxed = self.box_value(arg);
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let alloca = self
                .builder
                .build_alloca(ptr_type, &param.name)
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, boxed)
                .map_err(|e| e.to_string())?;
            let symbol = self
                .analyzer
                .symbol_table()
                .lookup(&param.name)
                .ok_or("Symbol not found")?;
            let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;
            self.variables.insert(
                param.name.clone(),
                (
                    alloca,
                    BasicTypeEnum::PointerType(ptr_type),
                    resolved_type.clone(),
                ),
            );
        }

        // Generate function body
        for stmt in &func.body {
            self.generate_statement(stmt, Some(&function))?;
        }

        // If void return, add return void if not already terminated
        if matches!(
            func.return_type.kind,
            TypeKind::Primitive(PrimitiveType::Void)
        ) {
            if let Some(block) = self.builder.get_insert_block() {
                if block.get_terminator().is_none() {
                    let _ = self.builder.build_return(None);
                }
            }
        }

        Ok(())
    }

    fn generate_expression(&mut self, expr: &ExpressionNode) -> Result<BasicValueEnum<'a>, String> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => self.generate_literal(lit),
            ExpressionKind::Identifier(name) => {
                if let Some((ptr, _, type_node)) = self.variables.get(name) {
                    let ptr_to_boxed = self
                        .builder
                        .build_load(self.context.ptr_type(AddressSpace::default()), *ptr, name)
                        .map_err(|e| e.to_string())?
                        .into_pointer_value();
                    match type_node {
                        Type::Primitive(PrimitiveType::Int) => {
                            let func = self
                                .module
                                .get_function("mux_int_from_value")
                                .ok_or("mux_int_from_value not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[ptr_to_boxed.into()], "int_from_value")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        Type::Primitive(PrimitiveType::Float) => {
                            let func = self
                                .module
                                .get_function("mux_float_from_value")
                                .ok_or("mux_float_from_value not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[ptr_to_boxed.into()], "float_from_value")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        Type::Primitive(PrimitiveType::Bool) => {
                            let func = self
                                .module
                                .get_function("mux_bool_from_value")
                                .ok_or("mux_bool_from_value not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[ptr_to_boxed.into()], "bool_from_value")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        Type::Primitive(PrimitiveType::Str) => Ok(ptr_to_boxed.into()),
                        _ => {
                            // boxed types
                            Ok(ptr_to_boxed.into())
                        }
                    }
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            ExpressionKind::Binary { left, op, right } => {
                if op.is_assignment() {
                    match op {
                        BinaryOp::Assign => {
                            let right_val = self.generate_expression(right)?;
                            if let ExpressionKind::Identifier(name) = &left.kind {
                                if let Some((ptr, _, _)) = self.variables.get(name) {
                                    let ptr_copy = *ptr;
                                    let boxed = self.box_value(right_val);
                                    self.builder
                                        .build_store(ptr_copy, boxed)
                                        .map_err(|e| e.to_string())?;
                                    Ok(right_val)
                                } else {
                                    Err(format!("Undefined variable {}", name))
                                }
                            } else {
                                Err("Assignment to non-identifier not implemented".to_string())
                            }
                        }
                        BinaryOp::AddAssign => {
                            let left_val = self.generate_expression(left)?;
                            let right_val = self.generate_expression(right)?;
                            let result = if left_val.is_int_value() {
                                self.builder
                                    .build_int_add(
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "add_assign",
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into()
                            } else if left_val.is_float_value() {
                                self.builder
                                    .build_float_add(
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fadd_assign",
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into()
                            } else {
                                return Err("Unsupported add assign operands".to_string());
                            };
                            if let ExpressionKind::Identifier(name) = &left.kind {
                                if let Some((ptr, _, _)) = self.variables.get(name) {
                                    let ptr_copy = *ptr;
                                    let boxed = self.box_value(result);
                                    self.builder
                                        .build_store(ptr_copy, boxed)
                                        .map_err(|e| e.to_string())?;
                                    Ok(result)
                                } else {
                                    Err(format!("Undefined variable {}", name))
                                }
                            } else {
                                Err("Assignment to non-identifier not implemented".to_string())
                            }
                        }
                        BinaryOp::SubtractAssign => {
                            let left_val = self.generate_expression(left)?;
                            let right_val = self.generate_expression(right)?;
                            let result = if left_val.is_int_value() {
                                self.builder
                                    .build_int_sub(
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "sub_assign",
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into()
                            } else if left_val.is_float_value() {
                                self.builder
                                    .build_float_sub(
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fsub_assign",
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into()
                            } else {
                                return Err("Unsupported sub assign operands".to_string());
                            };
                            if let ExpressionKind::Identifier(name) = &left.kind {
                                if let Some((ptr, _, _)) = self.variables.get(name) {
                                    let ptr_copy = *ptr;
                                    let boxed = self.box_value(result);
                                    self.builder
                                        .build_store(ptr_copy, boxed)
                                        .map_err(|e| e.to_string())?;
                                    Ok(result)
                                } else {
                                    Err(format!("Undefined variable {}", name))
                                }
                            } else {
                                Err("Assignment to non-identifier not implemented".to_string())
                            }
                        }
                        _ => Err("Assignment op not implemented".to_string()),
                    }
                } else {
                    let left_val = self.generate_expression(left)?;
                    let right_val = self.generate_expression(right)?;
                    Ok(self.generate_binary_op(left_val, op, right_val)?)
                }
            }
            ExpressionKind::Call { func, args } => {
                if let ExpressionKind::FieldAccess { expr, field } = &func.kind {
                    if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                        if obj_name == "self" {
                            // Method call on self
                            return self.generate_method_call_on_self(field, args);
                        } else {
                            // Method call on variable
                            if let Some((_, _, type_)) = self.variables.get(obj_name) {
                                match type_ {
                                    Type::Primitive(PrimitiveType::Int) if field == "to_string" => {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_int_to_string")
                                            .ok_or("mux_int_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "int_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    Type::Primitive(PrimitiveType::Float)
                                        if field == "to_string" =>
                                    {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_float_to_string")
                                            .ok_or("mux_float_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "float_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    Type::Primitive(PrimitiveType::Bool)
                                        if field == "to_string" =>
                                    {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_bool_to_string")
                                            .ok_or("mux_bool_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "bool_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    Type::Primitive(PrimitiveType::Str) if field == "to_string" => {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_string_to_string")
                                            .ok_or("mux_string_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "str_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    Type::List(_) if field == "to_string" => {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_value_to_string")
                                            .ok_or("mux_value_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "val_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    Type::Map(_, _) if field == "to_string" => {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_value_to_string")
                                            .ok_or("mux_value_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "val_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    Type::Set(_) if field == "to_string" => {
                                        let ptr = self.generate_expression(expr)?;
                                        let func = self
                                            .module
                                            .get_function("mux_value_to_string")
                                            .ok_or("mux_value_to_string not found")?;
                                        let call = self
                                            .builder
                                            .build_call(func, &[ptr.into()], "val_to_str")
                                            .map_err(|e| e.to_string())?;
                                        let func_new = self
                                            .module
                                            .get_function("mux_new_string_from_cstr")
                                            .ok_or("mux_new_string_from_cstr not found")?;
                                        let call2 = self
                                            .builder
                                            .build_call(
                                                func_new,
                                                &[call.try_as_basic_value().left().unwrap().into()],
                                                "new_str",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        return Ok(call2.try_as_basic_value().left().unwrap());
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Method {} not implemented for type",
                                            field
                                        ))
                                    }
                                }
                            } else {
                                return Err(format!("Undefined variable {}", obj_name));
                            }
                        }
                    } else {
                        return Err("Complex method calls not implemented".to_string());
                    }
                }
                if let ExpressionKind::Identifier(name) = &func.kind {
                    match name.as_str() {
                        "print" => {
                            if args.len() != 1 {
                                return Err("print takes 1 argument".to_string());
                            }
                            let arg_val = self.generate_expression(&args[0])?;
                            let func_print = self
                                .module
                                .get_function("mux_print")
                                .ok_or("mux_print not found")?;
                            self.builder
                                .build_call(func_print, &[arg_val.into()], "print_call")
                                .map_err(|e| e.to_string())?;
                            // Return void, but since BasicValueEnum, return a dummy
                            Ok(self.context.i32_type().const_int(0, false).into())
                        }
                        "Err" => {
                            if args.len() != 1 {
                                return Err("Err takes 1 argument".to_string());
                            }
                            let arg_val = self.generate_expression(&args[0])?;
                            let func = self
                                .module
                                .get_function("mux_result_err_str")
                                .ok_or("mux_result_err_str not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[arg_val.into()], "err_call")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        "Ok" => {
                            if args.len() != 1 {
                                return Err("Ok takes 1 argument".to_string());
                            }
                            let arg_val = self.generate_expression(&args[0])?;
                            let func = self
                                .module
                                .get_function("mux_result_ok_int")
                                .ok_or("mux_result_ok_int not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[arg_val.into()], "ok_call")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        "Some" => {
                            if args.len() != 1 {
                                return Err("Some takes 1 argument".to_string());
                            }
                            let arg_val = self.generate_expression(&args[0])?;
                            let func = self
                                .module
                                .get_function("mux_optional_some_int")
                                .ok_or("mux_optional_some_int not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[arg_val.into()], "some_call")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        "None" => {
                            if !args.is_empty() {
                                return Err("None takes no arguments".to_string());
                            }
                            let func = self
                                .module
                                .get_function("mux_optional_none")
                                .ok_or("mux_optional_none not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[], "none_call")
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        "range" => {
                            if args.len() != 2 {
                                return Err("range takes 2 arguments".to_string());
                            }
                            let start = self.generate_expression(&args[0])?;
                            let end = self.generate_expression(&args[1])?;
                            let call = self
                                .builder
                                .build_call(
                                    self.module.get_function("mux_range").unwrap(),
                                    &[start.into(), end.into()],
                                    "range_call",
                                )
                                .map_err(|e| e.to_string())?;
                            Ok(call.try_as_basic_value().left().unwrap())
                        }
                        _ => {
                            // User function
                            let func_val = if let Some(fv) = self.functions.get(name) {
                                *fv
                            } else {
                                return Err(format!("Unknown function {}", name));
                            };
                            let mut arg_vals = vec![];
                            for arg in args {
                                arg_vals.push(self.generate_expression(arg)?);
                            }
                            let arg_vals: Vec<BasicMetadataValueEnum> =
                                arg_vals.into_iter().map(|v| v.into()).collect();
                            let call = self
                                .builder
                                .build_call(func_val, &arg_vals, "call")
                                .map_err(|e| e.to_string())?;
                            if func_val.get_type().get_return_type().is_some() {
                                Ok(call.try_as_basic_value().left().unwrap())
                            } else {
                                // Void return, return dummy
                                Ok(self.context.i32_type().const_int(0, false).into())
                            }
                        }
                    }
                } else {
                    Err("Complex function calls not implemented".to_string())
                }
            }
            ExpressionKind::ListAccess { expr, index } => {
                let list_val = self.generate_expression(expr)?;
                let index_val = self.generate_expression(index)?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_get").unwrap(),
                        &[list_val.into(), index_val.into()],
                        "list_get",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            ExpressionKind::ListLiteral(elements) => {
                let list_ptr = self
                    .generate_runtime_call("mux_new_list", &[])
                    .unwrap()
                    .into_pointer_value();
                for element in elements {
                    let elem_val = self.generate_expression(element)?;
                    let elem_ptr = self.box_value(elem_val);
                    self.generate_runtime_call(
                        "mux_list_push_back",
                        &[list_ptr.into(), elem_ptr.into()],
                    );
                }
                let value_ptr = self
                    .generate_runtime_call("mux_list_value", &[list_ptr.into()])
                    .unwrap()
                    .into_pointer_value();
                Ok(value_ptr.into())
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                let map_ptr = self
                    .generate_runtime_call("mux_new_map", &[])
                    .unwrap()
                    .into_pointer_value();
                for (key, value) in entries {
                    let key_val = self.generate_expression(key)?;
                    let key_ptr = self.box_value(key_val);
                    let value_val = self.generate_expression(value)?;
                    let value_ptr = self.box_value(value_val);
                    self.generate_runtime_call(
                        "mux_map_put",
                        &[map_ptr.into(), key_ptr.into(), value_ptr.into()],
                    );
                }
                let value_ptr = self
                    .generate_runtime_call("mux_map_value", &[map_ptr.into()])
                    .unwrap()
                    .into_pointer_value();
                Ok(value_ptr.into())
            }
            ExpressionKind::SetLiteral(elements) => {
                let set_ptr = self
                    .generate_runtime_call("mux_new_set", &[])
                    .unwrap()
                    .into_pointer_value();
                for element in elements {
                    let elem_val = self.generate_expression(element)?;
                    let elem_ptr = self.box_value(elem_val);
                    self.generate_runtime_call("mux_set_add", &[set_ptr.into(), elem_ptr.into()]);
                }
                let value_ptr = self
                    .generate_runtime_call("mux_set_value", &[set_ptr.into()])
                    .unwrap()
                    .into_pointer_value();
                Ok(value_ptr.into())
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => Ok(self.generate_if_expression(cond, then_expr, else_expr)?),
            ExpressionKind::Lambda { params, body } => {
                Ok(self.generate_lambda_expression(params, body)?)
            }
            ExpressionKind::FieldAccess { expr, field } => {
                let struct_ptr = self.generate_expression(expr)?.into_pointer_value();
                if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                    if let Some(type_node) = self.variables.get(obj_name).map(|(_, _, t)| t) {
                        if let Type::Named(class_name, _) = type_node {
                            if let Some(field_indices) = self.field_map.get(class_name.as_str()) {
                                if let Some(&index) = field_indices.get(field) {
                                    let struct_type = self
                                        .type_map
                                        .get(class_name.as_str())
                                        .ok_or("Class type not found")?;
                                    if let BasicTypeEnum::StructType(st) = *struct_type {
                                        let field_ptr = self
                                            .builder
                                            .build_struct_gep(st, struct_ptr, index as u32, field)
                                            .map_err(|e| e.to_string())?;
                                        let loaded = self
                                            .builder
                                            .build_load(self.context.i64_type(), field_ptr, field)
                                            .map_err(|e| e.to_string())?;
                                        return Ok(loaded);
                                    }
                                }
                            }
                        }
                    } else if let Some((_, _, type_node)) = self.variables.get(obj_name) {
                        match type_node {
                            Type::Primitive(PrimitiveType::Int) if field == "to_string" => {
                                let ptr = self.generate_expression(expr)?;
                                let func = self
                                    .module
                                    .get_function("mux_value_to_string")
                                    .ok_or("mux_value_to_string not found")?;
                                let call = self
                                    .builder
                                    .build_call(func, &[ptr.into()], "val_to_str")
                                    .map_err(|e| e.to_string())?;
                                let func_new = self
                                    .module
                                    .get_function("mux_new_string_from_cstr")
                                    .ok_or("mux_new_string_from_cstr not found")?;
                                let call2 = self
                                    .builder
                                    .build_call(
                                        func_new,
                                        &[call.try_as_basic_value().left().unwrap().into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().unwrap());
                            }
                            Type::Primitive(PrimitiveType::Float) if field == "to_string" => {
                                let ptr = self.generate_expression(expr)?;
                                let func = self
                                    .module
                                    .get_function("mux_value_to_string")
                                    .ok_or("mux_value_to_string not found")?;
                                let call = self
                                    .builder
                                    .build_call(func, &[ptr.into()], "val_to_str")
                                    .map_err(|e| e.to_string())?;
                                let func_new = self
                                    .module
                                    .get_function("mux_new_string_from_cstr")
                                    .ok_or("mux_new_string_from_cstr not found")?;
                                let call2 = self
                                    .builder
                                    .build_call(
                                        func_new,
                                        &[call.try_as_basic_value().left().unwrap().into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().unwrap());
                            }
                            Type::Primitive(PrimitiveType::Bool) if field == "to_string" => {
                                let ptr = self.generate_expression(expr)?;
                                let func = self
                                    .module
                                    .get_function("mux_value_to_string")
                                    .ok_or("mux_value_to_string not found")?;
                                let call = self
                                    .builder
                                    .build_call(func, &[ptr.into()], "val_to_str")
                                    .map_err(|e| e.to_string())?;
                                let func_new = self
                                    .module
                                    .get_function("mux_new_string_from_cstr")
                                    .ok_or("mux_new_string_from_cstr not found")?;
                                let call2 = self
                                    .builder
                                    .build_call(
                                        func_new,
                                        &[call.try_as_basic_value().left().unwrap().into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().unwrap());
                            }
                            Type::Primitive(PrimitiveType::Str) if field == "to_string" => {
                                let ptr = self.generate_expression(expr)?;
                                let func = self
                                    .module
                                    .get_function("mux_value_to_string")
                                    .ok_or("mux_value_to_string not found")?;
                                let call = self
                                    .builder
                                    .build_call(func, &[ptr.into()], "val_to_str")
                                    .map_err(|e| e.to_string())?;
                                let func_new = self
                                    .module
                                    .get_function("mux_new_string_from_cstr")
                                    .ok_or("mux_new_string_from_cstr not found")?;
                                let call2 = self
                                    .builder
                                    .build_call(
                                        func_new,
                                        &[call.try_as_basic_value().left().unwrap().into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().unwrap());
                            }
                            _ => {}
                        }
                    }
                }
                Err("Field access not supported".to_string())
            }
            _ => Err("Expression type not implemented".to_string()),
        }
    }

    fn generate_statement(
        &mut self,
        stmt: &StatementNode,
        function: Option<&FunctionValue<'a>>,
    ) -> Result<(), String> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, expr) => {
                let value = self.generate_expression(expr)?;
                let boxed = self.box_value(value);
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let alloca = self
                    .builder
                    .build_alloca(ptr_type, name)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(alloca, boxed)
                    .map_err(|e| e.to_string())?;
                let symbol = self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .ok_or("Symbol not found")?;
                let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;
                self.variables.insert(
                    name.clone(),
                    (
                        alloca,
                        BasicTypeEnum::PointerType(ptr_type),
                        resolved_type.clone(),
                    ),
                );
            }
            StatementKind::TypedDecl(name, _type_node, expr) => {
                let value = self.generate_expression(expr)?;
                let boxed = self.box_value(value);
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let alloca = self
                    .builder
                    .build_alloca(ptr_type, name)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(alloca, boxed)
                    .map_err(|e| e.to_string())?;
                let symbol = self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .ok_or("Symbol not found")?;
                let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;
                self.variables.insert(
                    name.clone(),
                    (
                        alloca,
                        BasicTypeEnum::PointerType(ptr_type),
                        resolved_type.clone(),
                    ),
                );
            }
            StatementKind::ConstDecl(name, _type_node, expr) => {
                let value = self.generate_expression(expr)?;
                let boxed = self.box_value(value);
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let alloca = self
                    .builder
                    .build_alloca(ptr_type, name)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(alloca, boxed)
                    .map_err(|e| e.to_string())?;
                let symbol = self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .ok_or("Symbol not found")?;
                let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;
                self.variables.insert(
                    name.clone(),
                    (
                        alloca,
                        BasicTypeEnum::PointerType(ptr_type),
                        resolved_type.clone(),
                    ),
                );
            }
            StatementKind::Return(Some(expr)) => {
                let value = self.generate_expression(expr)?;
                self.builder
                    .build_return(Some(&value))
                    .map_err(|e| e.to_string())?;
            }
            StatementKind::Return(None) => {
                let _ = self.builder.build_return(None);
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let function = function.ok_or("If statement not in function")?;
                let cond_val = self.generate_expression(cond)?;
                let cond_int = cond_val.into_int_value();

                let then_bb = self.context.append_basic_block(*function, "bb1");
                let else_bb = self.context.append_basic_block(*function, "bb2");
                let merge_bb = self.context.append_basic_block(*function, "bb3");

                self.builder
                    .build_conditional_branch(cond_int, then_bb, else_bb)
                    .map_err(|e| e.to_string())?;

                // then block
                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.generate_statement(stmt, Some(function))?;
                }
                if !then_block
                    .last()
                    .is_some_and(|s| matches!(s.kind, StatementKind::Return(_)))
                {
                    self.builder
                        .build_unconditional_branch(merge_bb)
                        .map_err(|e| e.to_string())?;
                }

                // else block
                self.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.generate_statement(stmt, Some(function))?;
                    }
                }
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| e.to_string())?;

                // merge
                self.builder.position_at_end(merge_bb);
            }
            StatementKind::While { cond, body } => {
                let function = function.ok_or("While statement not in function")?;
                let header_bb = self.context.append_basic_block(*function, "while_header");
                let body_bb = self.context.append_basic_block(*function, "while_body");
                let exit_bb = self.context.append_basic_block(*function, "while_exit");

                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                // header
                self.builder.position_at_end(header_bb);
                let cond_val = self.generate_expression(cond)?;
                let cond_int = cond_val.into_int_value();
                self.builder
                    .build_conditional_branch(cond_int, body_bb, exit_bb)
                    .map_err(|e| e.to_string())?;

                // body
                self.builder.position_at_end(body_bb);
                for stmt in body {
                    self.generate_statement(stmt, Some(function))?;
                }
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                // exit
                self.builder.position_at_end(exit_bb);
            }
            StatementKind::For {
                var,
                var_type,
                iter,
                body,
            } => {
                let function = function.ok_or("For statement not in function")?;
                // Assume iter is range(start, end) or list identifier
                if let ExpressionKind::Call { func, args } = &iter.kind {
                    if let ExpressionKind::Identifier(name) = &func.kind {
                        if name == "range" && args.len() == 2 {
                            let resolved_var_type = Type::Primitive(PrimitiveType::Int);
                            let start_val = self.generate_expression(&args[0])?;
                            let end_val = self.generate_expression(&args[1])?;

                            // Create index variable
                            let index_type = self.context.i64_type();
                            let index_alloca = self
                                .builder
                                .build_alloca(index_type, "index")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(index_alloca, start_val)
                                .map_err(|e| e.to_string())?;

                            // Create loop var
                            let ptr_type = self.context.ptr_type(AddressSpace::default());
                            let var_alloca = self
                                .builder
                                .build_alloca(ptr_type, var)
                                .map_err(|e| e.to_string())?;
                            self.variables.insert(
                                var.clone(),
                                (
                                    var_alloca,
                                    BasicTypeEnum::PointerType(ptr_type),
                                    resolved_var_type.clone(),
                                ),
                            );

                            // Loop header
                            let label_id = self.label_counter;
                            self.label_counter += 1;
                            let header_bb = self
                                .context
                                .append_basic_block(*function, &format!("for_header_{}", label_id));
                            let body_bb = self
                                .context
                                .append_basic_block(*function, &format!("for_body_{}", label_id));
                            let exit_bb = self
                                .context
                                .append_basic_block(*function, &format!("for_exit_{}", label_id));

                            self.builder
                                .build_unconditional_branch(header_bb)
                                .map_err(|e| e.to_string())?;

                            // Header: check index < end
                            self.builder.position_at_end(header_bb);
                            let index_load = self
                                .builder
                                .build_load(index_type, index_alloca, "index_load")
                                .map_err(|e| e.to_string())?;
                            let cmp = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SLT,
                                    index_load.into_int_value(),
                                    end_val.into_int_value(),
                                    "cmp",
                                )
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_conditional_branch(cmp, body_bb, exit_bb)
                                .map_err(|e| e.to_string())?;

                            // Body: set var = index, then body
                            self.builder.position_at_end(body_bb);
                            let index_load2 = self
                                .builder
                                .build_load(index_type, index_alloca, "index_load2")
                                .map_err(|e| e.to_string())?;
                            let boxed = self.box_value(index_load2);
                            self.builder
                                .build_store(var_alloca, boxed)
                                .map_err(|e| e.to_string())?;
                            for stmt in body {
                                self.generate_statement(stmt, Some(function))?;
                            }
                            // Increment index
                            let one = self.context.i64_type().const_int(1, false);
                            let new_index = self
                                .builder
                                .build_int_add(index_load2.into_int_value(), one, "inc")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(index_alloca, new_index)
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_unconditional_branch(header_bb)
                                .map_err(|e| e.to_string())?;
                            self.builder.position_at_end(exit_bb);
                        } else {
                            return Err("For loop iter must be range(start, end)".to_string());
                        }
                    } else {
                        return Err("For loop iter must be range call".to_string());
                    }
                } else if let ExpressionKind::Identifier(_list_name) = &iter.kind {
                    // Iterate over list
                    let resolved_var_type = self
                        .analyzer
                        .resolve_type(var_type)
                        .map_err(|e| e.message)?;
                    let list_val = self.generate_expression(iter)?;

                    // Get length
                    let len_call = self
                        .builder
                        .build_call(
                            self.module.get_function("mux_list_length").unwrap(),
                            &[list_val.into()],
                            "list_len",
                        )
                        .map_err(|e| e.to_string())?;
                    let len_val = len_call
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();

                    // Create index variable
                    let index_type = self.context.i64_type();
                    let index_alloca = self
                        .builder
                        .build_alloca(index_type, "index")
                        .map_err(|e| e.to_string())?;
                    let zero = self.context.i64_type().const_int(0, false);
                    self.builder
                        .build_store(index_alloca, zero)
                        .map_err(|e| e.to_string())?;

                    // Create loop var
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let var_alloca = self
                        .builder
                        .build_alloca(ptr_type, var)
                        .map_err(|e| e.to_string())?;
                    self.variables.insert(
                        var.clone(),
                        (
                            var_alloca,
                            BasicTypeEnum::PointerType(ptr_type),
                            resolved_var_type.clone(),
                        ),
                    );

                    // Loop header
                    let label_id = self.label_counter;
                    self.label_counter += 1;
                    let header_bb = self
                        .context
                        .append_basic_block(*function, &format!("for_header_{}", label_id));
                    let body_bb = self
                        .context
                        .append_basic_block(*function, &format!("for_body_{}", label_id));
                    let exit_bb = self
                        .context
                        .append_basic_block(*function, &format!("for_exit_{}", label_id));

                    self.builder
                        .build_unconditional_branch(header_bb)
                        .map_err(|e| e.to_string())?;

                    // Header: check index < len
                    self.builder.position_at_end(header_bb);
                    let index_load = self
                        .builder
                        .build_load(index_type, index_alloca, "index_load")
                        .map_err(|e| e.to_string())?;
                    let cmp = self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLT,
                            index_load.into_int_value(),
                            len_val,
                            "cmp",
                        )
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_conditional_branch(cmp, body_bb, exit_bb)
                        .map_err(|e| e.to_string())?;

                    // Body: get element at index
                    self.builder.position_at_end(body_bb);
                    let index_load2 = self
                        .builder
                        .build_load(index_type, index_alloca, "index_load2")
                        .map_err(|e| e.to_string())?;
                    let get_call = self
                        .builder
                        .build_call(
                            self.module.get_function("mux_list_get").unwrap(),
                            &[list_val.into(), index_load2.into()],
                            "list_get",
                        )
                        .map_err(|e| e.to_string())?;
                    let value_ptr = get_call
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_pointer_value();
                    // Store the Value pointer directly
                    self.builder
                        .build_store(var_alloca, value_ptr)
                        .map_err(|e| e.to_string())?;

                    // Execute body
                    for stmt in body {
                        self.generate_statement(stmt, Some(function))?;
                    }

                    // Increment index
                    let one = self.context.i64_type().const_int(1, false);
                    let new_index = self
                        .builder
                        .build_int_add(index_load2.into_int_value(), one, "inc")
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(index_alloca, new_index)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_unconditional_branch(header_bb)
                        .map_err(|e| e.to_string())?;

                    // Exit
                    self.builder.position_at_end(exit_bb);
                    // For functions returning Optional, return None if loop completes without return
                    let none_call = self
                        .builder
                        .build_call(
                            self.module.get_function("mux_optional_none").unwrap(),
                            &[],
                            "none",
                        )
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_return(Some(&none_call.try_as_basic_value().left().unwrap()))
                        .map_err(|e| e.to_string())?;
                } else {
                    return Err("For loop iter must be range(...) or list identifier".to_string());
                }
            }
            StatementKind::Match { expr, arms } => {
                let function = function.ok_or("Match not in function")?;
                let expr_val = self.generate_expression(expr)?;
                let expr_ptr = expr_val.into_pointer_value();

                // Determine enum type from patterns
                let enum_name = {
                    let mut en = "Result"; // default
                    for arm in arms {
                        if let PatternNode::EnumVariant { name, .. } = &arm.pattern {
                            en = if name == "Some" || name == "None" {
                                "Optional"
                            } else {
                                "Result"
                            };
                            break;
                        }
                    }
                    en
                };

                // Get discriminant
                let discriminant_func = if enum_name == "Optional" {
                    "mux_optional_discriminant"
                } else if enum_name == "Result" {
                    "mux_result_discriminant"
                } else {
                    return Err(format!("Unknown enum {}", enum_name));
                };
                let func = self
                    .module
                    .get_function(discriminant_func)
                    .ok_or(format!("{} not found", discriminant_func))?;
                let discriminant_call = self
                    .builder
                    .build_call(func, &[expr_ptr.into()], "discriminant_call")
                    .map_err(|e| e.to_string())?;
                let discriminant = discriminant_call
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                let mut current_bb = self.builder.get_insert_block().unwrap();
                let end_bb = self.context.append_basic_block(*function, "match_end");

                for (i, arm) in arms.iter().enumerate() {
                    let arm_bb = self
                        .context
                        .append_basic_block(*function, &format!("match_arm_{}", i));
                    let next_bb = if i < arms.len() - 1 {
                        self.context
                            .append_basic_block(*function, &format!("match_next_{}", i))
                    } else {
                        end_bb
                    };

                    self.builder.position_at_end(current_bb);

                    let pattern_matches = match &arm.pattern {
                        PatternNode::EnumVariant { name, args: _ } => {
                            let enum_name = if name == "Some" || name == "None" {
                                "Optional"
                            } else if name == "Ok" || name == "Err" {
                                "Result"
                            } else {
                                return Err(format!("Unknown variant {}", name));
                            };
                            let variant_index = self.get_variant_index(enum_name, name)?;
                            let index_val = self
                                .context
                                .i32_type()
                                .const_int(variant_index as u64, false);
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    discriminant,
                                    index_val,
                                    "match_cmp",
                                )
                                .map_err(|e| e.to_string())?
                        }
                        PatternNode::Wildcard => self.context.bool_type().const_int(1, false),
                        _ => return Err("Pattern not implemented".to_string()),
                    };

                    let cond = pattern_matches;

                    self.builder
                        .build_conditional_branch(cond, arm_bb, next_bb)
                        .map_err(|e| e.to_string())?;

                    // Arm body
                    self.builder.position_at_end(arm_bb);

                    // Bind variables
                    if let PatternNode::EnumVariant { name, args } = &arm.pattern {
                        if (name == "Some" || name == "Ok" || name == "Err") && !args.is_empty() {
                            if let PatternNode::Identifier(var) = &args[0] {
                                let data_func = if enum_name == "Optional" {
                                    "mux_optional_data"
                                } else if enum_name == "Result" {
                                    "mux_result_data"
                                } else {
                                    return Err(format!("Unknown enum {}", enum_name));
                                };
                                let func = self
                                    .module
                                    .get_function(data_func)
                                    .ok_or(format!("{} not found", data_func))?;
                                let data_call = self
                                    .builder
                                    .build_call(func, &[expr_ptr.into()], "data_call")
                                    .map_err(|e| e.to_string())?;
                                let data_ptr = data_call
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_pointer_value();
                                let ptr_type = self.context.ptr_type(AddressSpace::default());
                                let alloca = self
                                    .builder
                                    .build_alloca(ptr_type, var)
                                    .map_err(|e| e.to_string())?;
                                self.builder
                                    .build_store(alloca, data_ptr)
                                    .map_err(|e| e.to_string())?;
                                let var_type = if name == "Err" {
                                    Type::Primitive(PrimitiveType::Str)
                                } else {
                                    Type::Primitive(PrimitiveType::Int)
                                };
                                self.variables.insert(
                                    var.clone(),
                                    (alloca, BasicTypeEnum::PointerType(ptr_type), var_type),
                                );
                            }
                        }
                    }

                    // Check guard
                    if let Some(guard) = &arm.guard {
                        let guard_val = self.generate_expression(guard)?;
                        let guard_pass_bb = self
                            .context
                            .append_basic_block(*function, &format!("match_guard_pass_{}", i));
                        self.builder
                            .build_conditional_branch(
                                guard_val.into_int_value(),
                                guard_pass_bb,
                                next_bb,
                            )
                            .map_err(|e| e.to_string())?;
                        self.builder.position_at_end(guard_pass_bb);
                    }

                    for stmt in &arm.body {
                        self.generate_statement(stmt, Some(function))?;
                    }
                    self.builder
                        .build_unconditional_branch(end_bb)
                        .map_err(|e| e.to_string())?;

                    current_bb = next_bb;
                }

                self.builder.position_at_end(end_bb);
            }
            StatementKind::Expression(expr) => {
                self.generate_expression(expr)?;
            }
            _ => {} // Skip other statement types for now
        }
        Ok(())
    }

    fn generate_literal(&mut self, lit: &LiteralNode) -> Result<BasicValueEnum<'a>, String> {
        match lit {
            LiteralNode::Integer(i) => {
                let val = self.context.i64_type().const_int(*i as u64, true);
                Ok(val.into())
            }
            LiteralNode::Float(f) => {
                let val = self.context.f64_type().const_float(f.into_inner());
                Ok(val.into())
            }
            LiteralNode::Boolean(b) => {
                let val = self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false);
                Ok(val.into())
            }
            LiteralNode::String(s) => {
                let name = format!("str_{}", self.string_counter);
                self.string_counter += 1;
                let bytes = s.as_bytes();
                let mut values = vec![];
                for &b in bytes {
                    values.push(self.context.i8_type().const_int(b as u64, false));
                }
                values.push(self.context.i8_type().const_int(0, false));
                let array_type = self.context.i8_type().array_type(values.len() as u32);
                let const_array = self.context.i8_type().const_array(&values);
                let global =
                    self.module
                        .add_global(array_type, Some(AddressSpace::default()), &name);
                global.set_linkage(inkwell::module::Linkage::Private);
                global.set_initializer(&const_array);
                let ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        array_type,
                        global.as_pointer_value(),
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(0, false),
                        ],
                        &name,
                    )
                }
                .map_err(|e| e.to_string())?;
                let call = self
                    .generate_runtime_call("mux_new_string_from_cstr", &[ptr.into()])
                    .unwrap();
                Ok(call.into())
            }
            _ => Err("Literal type not implemented".to_string()),
        }
    }

    fn generate_binary_op(
        &mut self,
        left: BasicValueEnum<'a>,
        op: &BinaryOp,
        right: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        // Type coercion for arithmetic
        let (left_val, right_val) = if left.is_float_value() && right.is_int_value() {
            (
                left,
                self.builder
                    .build_signed_int_to_float(
                        right.into_int_value(),
                        self.context.f64_type(),
                        "int_to_float",
                    )
                    .map_err(|e| e.to_string())?
                    .into(),
            )
        } else if left.is_int_value() && right.is_float_value() {
            (
                self.builder
                    .build_signed_int_to_float(
                        left.into_int_value(),
                        self.context.f64_type(),
                        "int_to_float",
                    )
                    .map_err(|e| e.to_string())?
                    .into(),
                right,
            )
        } else {
            (left, right)
        };

        match op {
            BinaryOp::Add => {
                if left_val.is_int_value() && right_val.is_int_value() {
                    self.builder
                        .build_int_add(left_val.into_int_value(), right_val.into_int_value(), "add")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left_val.is_float_value() && right_val.is_float_value() {
                    self.builder
                        .build_float_add(
                            left_val.into_float_value(),
                            right_val.into_float_value(),
                            "fadd",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left_val.is_pointer_value() || right_val.is_pointer_value() {
                    // Value add - ensure both are pointers
                    let left_ptr = if left_val.is_pointer_value() {
                        left_val.into_pointer_value()
                    } else {
                        self.box_value(left_val)
                    };
                    let right_ptr = if right_val.is_pointer_value() {
                        right_val.into_pointer_value()
                    } else {
                        self.box_value(right_val)
                    };
                    let call = self
                        .builder
                        .build_call(
                            self.module.get_function("mux_value_add").unwrap(),
                            &[left_ptr.into(), right_ptr.into()],
                            "add",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(call.try_as_basic_value().left().unwrap())
                } else {
                    Err("Unsupported add operands".to_string())
                }
            }
            BinaryOp::Subtract => {
                if left.is_int_value() {
                    self.builder
                        .build_int_sub(left.into_int_value(), right.into_int_value(), "sub")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_sub(left.into_float_value(), right.into_float_value(), "fsub")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported sub operands".to_string())
                }
            }
            BinaryOp::Multiply => {
                if left_val.is_int_value() {
                    self.builder
                        .build_int_mul(left_val.into_int_value(), right_val.into_int_value(), "mul")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left_val.is_float_value() {
                    self.builder
                        .build_float_mul(
                            left_val.into_float_value(),
                            right_val.into_float_value(),
                            "fmul",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported mul operands".to_string())
                }
            }
            BinaryOp::Divide => {
                if left.is_int_value() {
                    self.builder
                        .build_int_signed_div(left.into_int_value(), right.into_int_value(), "div")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_div(left.into_float_value(), right.into_float_value(), "fdiv")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported div operands".to_string())
                }
            }
            BinaryOp::Equal => {
                if left.is_int_value() {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            left.into_int_value(),
                            right.into_int_value(),
                            "eq",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            left.into_float_value(),
                            right.into_float_value(),
                            "feq",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported eq operands".to_string())
                }
            }
            BinaryOp::Less => {
                if left.is_int_value() {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLT,
                            left.into_int_value(),
                            right.into_int_value(),
                            "lt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLT,
                            left.into_float_value(),
                            right.into_float_value(),
                            "flt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported lt operands".to_string())
                }
            }
            BinaryOp::Greater => {
                if left.is_int_value() {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGT,
                            left.into_int_value(),
                            right.into_int_value(),
                            "gt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGT,
                            left.into_float_value(),
                            right.into_float_value(),
                            "fgt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported gt operands".to_string())
                }
            }
            BinaryOp::LessEqual => {
                if left.is_int_value() {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLE,
                            left.into_int_value(),
                            right.into_int_value(),
                            "le",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLE,
                            left.into_float_value(),
                            right.into_float_value(),
                            "fle",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported le operands".to_string())
                }
            }
            BinaryOp::GreaterEqual => {
                if left.is_int_value() {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGE,
                            left.into_int_value(),
                            right.into_int_value(),
                            "ge",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGE,
                            left.into_float_value(),
                            right.into_float_value(),
                            "fge",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported ge operands".to_string())
                }
            }
            BinaryOp::NotEqual => {
                if left.is_int_value() {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::NE,
                            left.into_int_value(),
                            right.into_int_value(),
                            "ne",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if left.is_float_value() {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::ONE,
                            left.into_float_value(),
                            right.into_float_value(),
                            "fne",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported ne operands".to_string())
                }
            }
            BinaryOp::LogicalAnd => {
                // For now, assume bool
                let and = self
                    .builder
                    .build_and(left.into_int_value(), right.into_int_value(), "and")
                    .map_err(|e| e.to_string())?;
                Ok(and.into())
            }
            BinaryOp::LogicalOr => {
                let or = self
                    .builder
                    .build_or(left.into_int_value(), right.into_int_value(), "or")
                    .map_err(|e| e.to_string())?;
                Ok(or.into())
            }
            BinaryOp::Modulo => {
                if left.is_int_value() {
                    self.builder
                        .build_int_signed_rem(left.into_int_value(), right.into_int_value(), "mod")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported mod operands".to_string())
                }
            }
            _ => Err("Binary op not implemented".to_string()),
        }
    }

    fn llvm_type_from_mux_type(&self, type_node: &TypeNode) -> Result<BasicTypeEnum<'a>, String> {
        match &type_node.kind {
            TypeKind::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            TypeKind::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            TypeKind::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            TypeKind::Primitive(PrimitiveType::Str) => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),
            TypeKind::Primitive(PrimitiveType::Void) => {
                Err("Void type not allowed here".to_string())
            }
            TypeKind::Primitive(PrimitiveType::Auto) => {
                Err("Auto primitive should be resolved".to_string())
            }
            TypeKind::Named(name, _generics) => {
                // For user-defined types (classes, enums), values are pointers to structs
                if let Some(_struct_type) = self.type_map.get(name) {
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                } else {
                    Err(format!("Unknown type: {}", name))
                }
            }
            TypeKind::Reference(_inner) => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::List(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Map(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Set(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Tuple(elements) => {
                let mut element_types = Vec::new();
                for elem in elements {
                    element_types.push(self.llvm_type_from_mux_type(elem)?);
                }
                let struct_type = self.context.struct_type(&element_types, false);
                Ok(struct_type.into())
            }
            TypeKind::Function {
                params: _,
                returns: _,
            } => {
                // For now, all function types are generic pointers
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::TraitObject(_) => {
                // For now, trait objects are just pointers
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::Auto => Err("Auto type should be resolved".to_string()),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn type_to_type_node(&self, type_: &Type) -> TypeNode {
        match type_ {
            Type::Primitive(p) => TypeNode {
                kind: TypeKind::Primitive(p.clone()),
                span: Span::new(0, 0),
            },
            Type::List(inner) => TypeNode {
                kind: TypeKind::List(Box::new(self.type_to_type_node(inner))),
                span: Span::new(0, 0),
            },
            Type::Map(k, v) => TypeNode {
                kind: TypeKind::Map(
                    Box::new(self.type_to_type_node(k)),
                    Box::new(self.type_to_type_node(v)),
                ),
                span: Span::new(0, 0),
            },
            Type::Set(inner) => TypeNode {
                kind: TypeKind::Set(Box::new(self.type_to_type_node(inner))),
                span: Span::new(0, 0),
            },
            Type::Tuple(elements) => TypeNode {
                kind: TypeKind::Tuple(elements.iter().map(|e| self.type_to_type_node(e)).collect()),
                span: Span::new(0, 0),
            },
            Type::Optional(inner) => TypeNode {
                kind: TypeKind::Named("Optional".to_string(), vec![self.type_to_type_node(inner)]),
                span: Span::new(0, 0),
            },
            Type::Reference(inner) => TypeNode {
                kind: TypeKind::Reference(Box::new(self.type_to_type_node(inner))),
                span: Span::new(0, 0),
            },
            Type::Void => TypeNode {
                kind: TypeKind::Primitive(PrimitiveType::Void),
                span: Span::new(0, 0),
            },
            Type::EmptyList => TypeNode {
                kind: TypeKind::List(Box::new(TypeNode {
                    kind: TypeKind::Auto,
                    span: Span::new(0, 0),
                })),
                span: Span::new(0, 0),
            },
            Type::EmptyMap => TypeNode {
                kind: TypeKind::Map(
                    Box::new(TypeNode {
                        kind: TypeKind::Auto,
                        span: Span::new(0, 0),
                    }),
                    Box::new(TypeNode {
                        kind: TypeKind::Auto,
                        span: Span::new(0, 0),
                    }),
                ),
                span: Span::new(0, 0),
            },
            Type::EmptySet => TypeNode {
                kind: TypeKind::Set(Box::new(TypeNode {
                    kind: TypeKind::Auto,
                    span: Span::new(0, 0),
                })),
                span: Span::new(0, 0),
            },
            Type::Function { params, returns } => TypeNode {
                kind: TypeKind::Function {
                    params: params.iter().map(|p| self.type_to_type_node(p)).collect(),
                    returns: Box::new(self.type_to_type_node(returns)),
                },
                span: Span::new(0, 0),
            },
            Type::Named(name, generics) => TypeNode {
                kind: TypeKind::Named(
                    name.clone(),
                    generics.iter().map(|g| self.type_to_type_node(g)).collect(),
                ),
                span: Span::new(0, 0),
            },
            Type::Variable(_) => TypeNode {
                kind: TypeKind::Auto,
                span: Span::new(0, 0),
            }, // Should not happen
        }
    }

    fn generate_runtime_call(
        &mut self,
        name: &str,
        args: &[BasicMetadataValueEnum<'a>],
    ) -> Option<BasicValueEnum<'a>> {
        let func = self.module.get_function(name).unwrap();
        let call = self.builder.build_call(func, args, "call").unwrap();
        call.try_as_basic_value().left()
    }

    fn box_value(&mut self, val: BasicValueEnum<'a>) -> PointerValue<'a> {
        if val.is_int_value() {
            let call = self
                .generate_runtime_call("mux_int_value", &[val.into()])
                .unwrap();
            call.into_pointer_value()
        } else if val.is_float_value() {
            let call = self
                .generate_runtime_call("mux_float_value", &[val.into()])
                .unwrap();
            call.into_pointer_value()
        } else if val.is_pointer_value() {
            // Assume string or already boxed
            val.into_pointer_value()
        } else {
            // For bool
            let call = self
                .generate_runtime_call("mux_bool_value", &[val.into()])
                .unwrap();
            call.into_pointer_value()
        }
    }

    fn generate_method_call_on_self(
        &mut self,
        field: &str,
        _args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // Placeholder for self.method() calls
        Err(format!("Self method calls not implemented: {}", field))
    }

    pub fn print_ir(&self) {
        println!("IR printed");
    }

    pub fn emit_ir_to_file(&self, filename: &str) -> Result<(), String> {
        self.module
            .print_to_file(filename)
            .map_err(|e| format!("Failed to write IR: {}", e))
    }
}
