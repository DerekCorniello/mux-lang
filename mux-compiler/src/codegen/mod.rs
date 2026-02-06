//! LLVM IR code generation for the Mux compiler.
//!
//! This module generates LLVM IR from the AST and semantic analysis results.
//! It has been split into submodules for better organization:
//! - classes: Class, interface, and enum type generation
//! - constructors: Constructor generation for classes and enums
//! - expressions: Expression code generation
//! - functions: Function declaration and generation
//! - generics: Generic type instantiation
//! - memory: Memory management and RC tracking
//! - methods: Method call generation
//! - operators: Binary and logical operators
//! - runtime: Runtime function boxing/unboxing
//! - statements: Statement code generation
//! - types: Type conversion functions

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{FunctionValue, PointerValue};
use std::collections::HashMap;

use crate::ast::{AstNode, Field, FunctionNode, StatementKind, StatementNode, TypeNode};
use crate::semantics::{GenericContext, SemanticAnalyzer, Type, Type as ResolvedType};

pub struct CodeGenerator<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    analyzer: &'a mut SemanticAnalyzer,
    type_map: HashMap<String, BasicTypeEnum<'a>>,
    vtable_map: HashMap<String, PointerValue<'a>>,
    vtable_type_map: HashMap<String, inkwell::types::StructType<'a>>,
    enum_variants: HashMap<String, Vec<String>>,
    enum_variant_fields: HashMap<String, HashMap<String, Vec<TypeNode>>>,
    field_map: HashMap<String, HashMap<String, usize>>,
    field_types_map: HashMap<String, Vec<BasicTypeEnum<'a>>>,
    classes: HashMap<String, Vec<Field>>,
    constructors: HashMap<String, FunctionValue<'a>>,
    lambda_counter: usize,
    string_counter: usize,
    label_counter: usize,
    variables: HashMap<String, (PointerValue<'a>, BasicTypeEnum<'a>, ResolvedType)>,
    global_variables: HashMap<String, (PointerValue<'a>, BasicTypeEnum<'a>, ResolvedType)>,
    functions: HashMap<String, FunctionValue<'a>>,
    function_nodes: HashMap<String, FunctionNode>,
    current_function_name: Option<String>,
    current_function_return_type: Option<ResolvedType>,
    generic_context: Option<GenericContext>,
    context_stack: Vec<GenericContext>,
    generated_methods: HashMap<String, bool>,
    rc_scope_stack: Vec<Vec<(String, PointerValue<'a>)>>,
}

impl<'a> CodeGenerator<'a> {
    // Helper function to sanitize module paths for use in LLVM identifiers
    fn sanitize_module_path(module_path: &str) -> String {
        module_path.replace(['.', '/'], "_")
    }

    pub fn new(context: &'a Context, analyzer: &'a mut SemanticAnalyzer) -> Self {
        let module = context.create_module("mux_module");
        let builder = context.create_builder();

        let void_type = context.void_type();
        let i64_type = context.i64_type();
        let f64_type = context.f64_type();
        let i8_ptr = context.ptr_type(AddressSpace::default());
        let list_ptr = i8_ptr;
        let map_ptr = i8_ptr;
        let set_ptr = i8_ptr;

        module.add_function(
            "mux_value_from_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );
        module.add_function(
            "mux_new_string_from_cstr",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );
        module.add_function(
            "mux_print",
            void_type.fn_type(&[i8_ptr.into()], false),
            None,
        );
        module.add_function("mux_read_line", i8_ptr.fn_type(&[], false), None);
        module.add_function(
            "exit",
            void_type.fn_type(&[context.i32_type().into()], false),
            None,
        );
        module.add_function("malloc", i8_ptr.fn_type(&[i64_type.into()], false), None);

        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_concat", fn_type, None);

        module.add_function(
            "mux_string_contains",
            context
                .bool_type()
                .fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_string_contains_char",
            context
                .bool_type()
                .fn_type(&[i8_ptr.into(), i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_string_equal",
            context
                .i32_type()
                .fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_string_not_equal",
            context
                .i32_type()
                .fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_equal",
            context
                .i32_type()
                .fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_not_equal",
            context
                .i32_type()
                .fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_from_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_int_to_string",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_int_to_float",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_float_to_int",
            i8_ptr.fn_type(&[f64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_float_to_string",
            i8_ptr.fn_type(&[f64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_bool_to_string",
            i8_ptr.fn_type(&[context.i32_type().into()], false),
            None,
        );

        module.add_function(
            "mux_bool_to_int",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_bool_to_float",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_string_to_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_string_to_int",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_string_to_float",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_char_to_int",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_char_to_string",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_list_to_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_to_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_register_object_type",
            context
                .i32_type()
                .fn_type(&[i8_ptr.into(), context.i64_type().into()], false),
            None,
        );

        module.add_function(
            "mux_alloc_object",
            i8_ptr.fn_type(&[context.i32_type().into()], false),
            None,
        );

        module.add_function(
            "mux_free_object",
            context.void_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_get_object_ptr",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_to_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_to_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_from_optional",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_list",
            list_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_map",
            map_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_set",
            set_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_concat",
            list_ptr.fn_type(&[list_ptr.into(), list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_merge",
            map_ptr.fn_type(&[map_ptr.into(), map_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_union",
            set_ptr.fn_type(&[set_ptr.into(), set_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_to_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_range",
            list_ptr.fn_type(&[i64_type.into(), i64_type.into()], false),
            None,
        );

        module.add_function("mux_new_list", list_ptr.fn_type(&[], false), None);

        module.add_function("mux_new_map", list_ptr.fn_type(&[], false), None);

        module.add_function("mux_new_set", list_ptr.fn_type(&[], false), None);

        module.add_function(
            "mux_value_add",
            i8_ptr.fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_int",
            i64_type.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_float",
            context.f64_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_bool",
            context.i32_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_get_type_tag",
            context.i32_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_free_value",
            void_type.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_push_back",
            void_type.fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_get",
            i8_ptr.fn_type(&[list_ptr.into(), i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_list_get_value",
            i8_ptr.fn_type(&[list_ptr.into(), i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_list_set",
            void_type.fn_type(&[list_ptr.into(), i64_type.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_set_value",
            void_type.fn_type(&[i8_ptr.into(), i64_type.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_length",
            i64_type.fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_contains",
            context
                .bool_type()
                .fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_list_length",
            i64_type.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_value_list_get_value",
            i8_ptr.fn_type(&[i8_ptr.into(), i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_list_pop_back",
            i8_ptr.fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_push",
            void_type.fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_pop",
            i8_ptr.fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_push_back_value",
            void_type.fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_push_value",
            void_type.fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_pop_back_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_pop_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_list_is_empty",
            context.bool_type().fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_put",
            void_type.fn_type(&[map_ptr.into(), i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_put_value",
            void_type.fn_type(&[i8_ptr.into(), i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_get",
            i8_ptr.fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_contains",
            context
                .bool_type()
                .fn_type(&[map_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_remove",
            i8_ptr.fn_type(&[map_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_remove_value",
            i8_ptr.fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_add",
            void_type.fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_add_value",
            void_type.fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_contains",
            context
                .bool_type()
                .fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_remove",
            context
                .bool_type()
                .fn_type(&[list_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_remove_value",
            context
                .bool_type()
                .fn_type(&[i8_ptr.into(), i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_size",
            i64_type.fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_set_is_empty",
            context.bool_type().fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_size",
            i64_type.fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_map_is_empty",
            context.bool_type().fn_type(&[list_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_int_value",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_float_value",
            i8_ptr.fn_type(&[context.f64_type().into()], false),
            None,
        );

        module.add_function(
            "mux_bool_value",
            i8_ptr.fn_type(&[context.i32_type().into()], false),
            None,
        );

        module.add_function(
            "mux_string_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_int_to_string",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_int_from_value",
            i64_type.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_float_from_value",
            f64_type.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_bool_from_value",
            context.i32_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_string_from_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_result_ok_int",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_result_err_str",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_some_int",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_some_float",
            i8_ptr.fn_type(&[f64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_some_bool",
            i8_ptr.fn_type(&[context.i32_type().into()], false),
            None,
        );

        module.add_function(
            "mux_optional_some_char",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_some_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_some_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function("mux_optional_none", i8_ptr.fn_type(&[], false), None);

        module.add_function(
            "mux_result_ok_int",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_result_ok_float",
            i8_ptr.fn_type(&[f64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_result_ok_bool",
            i8_ptr.fn_type(&[context.i32_type().into()], false),
            None,
        );

        module.add_function(
            "mux_result_ok_char",
            i8_ptr.fn_type(&[i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_result_ok_string",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_result_ok_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_result_err_str",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_discriminant",
            context.i32_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_data",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_is_some",
            context.bool_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_optional_get_value",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_result_discriminant",
            context.i32_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_result_data",
            i8_ptr.fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_int_pow",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_math_pow",
            f64_type.fn_type(&[f64_type.into(), f64_type.into()], false),
            None,
        );

        module.add_function(
            "mux_rc_inc",
            context.void_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        module.add_function(
            "mux_rc_dec",
            context.bool_type().fn_type(&[i8_ptr.into()], false),
            None,
        );

        let mut type_map = HashMap::new();
        let mut enum_variants = HashMap::new();

        let i32_type = context.i32_type();
        let i8_ptr = context.ptr_type(AddressSpace::default());
        let struct_type = context.struct_type(&[i32_type.into(), i8_ptr.into()], false);
        type_map.insert("Optional".to_string(), struct_type.into());
        type_map.insert("Result".to_string(), struct_type.into());

        // use BTreeMap to ensure deterministic ordering of enum variants
        use std::collections::BTreeMap;
        let mut ordered_variants = BTreeMap::new();
        ordered_variants.insert(
            "Optional".to_string(),
            vec!["Some".to_string(), "None".to_string()],
        );
        ordered_variants.insert(
            "Result".to_string(),
            vec!["Ok".to_string(), "Err".to_string()],
        );

        for (enum_name, variants) in ordered_variants {
            enum_variants.insert(enum_name, variants);
        }

        for (name, symbol) in analyzer.all_symbols() {
            if symbol.kind == crate::semantics::SymbolKind::Enum {
                let mut variants = vec![];
                for method_name in symbol.methods.keys() {
                    variants.push(method_name.clone());
                }
                enum_variants.insert(name.clone(), variants);
            }
        }

        Self {
            context,
            module,
            builder,
            analyzer,
            type_map,
            vtable_map: HashMap::new(),
            vtable_type_map: HashMap::new(),
            enum_variants,
            enum_variant_fields: HashMap::new(),
            field_map: HashMap::new(),
            field_types_map: HashMap::new(),
            classes: HashMap::new(),
            constructors: HashMap::new(),
            lambda_counter: 0,
            string_counter: 0,
            label_counter: 0,
            variables: HashMap::new(),
            global_variables: HashMap::new(),
            functions: HashMap::new(),
            function_nodes: HashMap::new(),
            current_function_name: None,
            current_function_return_type: None,
            generic_context: None,
            context_stack: Vec::new(),
            generated_methods: HashMap::new(),
            rc_scope_stack: Vec::new(),
        }
    }
    /// Create an alloca instruction in the entry block of the current function.
    /// This ensures proper LLVM dominance - allocas must be in the entry block
    /// to be used throughout the function, including in match arms and loops.
    fn create_entry_block_alloca(
        &self,
        function: FunctionValue<'a>,
        ty: BasicTypeEnum<'a>,
        name: &str,
    ) -> Result<PointerValue<'a>, String> {
        let builder = self.context.create_builder();

        let entry = function
            .get_first_basic_block()
            .expect("function should have entry block after creation");
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name).map_err(|e| e.to_string())
    }

    /// Create an alloca in the entry block of the current function (inferred from builder position).
    /// If not in a function context, creates alloca at current position.
    fn create_entry_alloca(
        &self,
        ty: BasicTypeEnum<'a>,
        name: &str,
    ) -> Result<PointerValue<'a>, String> {
        // try to get the current function from the builder's insert block
        if let Some(block) = self.builder.get_insert_block() {
            if let Some(function) = block.get_parent() {
                return self.create_entry_block_alloca(function, ty, name);
            }
        }

        // fallback: create alloca at current position (shouldn't happen in normal code)
        self.builder
            .build_alloca(ty, name)
            .map_err(|e| e.to_string())
    }

    pub fn generate(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        // Keep reference to main module nodes (for getting module name later)
        let main_module_nodes = nodes;

        // Collect all nodes including imported modules
        let mut all_nodes = Vec::new();

        // Add imported module nodes first (so they're available for main module)
        for module_nodes in self.analyzer.all_module_asts().values() {
            all_nodes.extend(module_nodes.clone());
        }

        // Add main module nodes last
        all_nodes.extend(nodes.to_vec());

        // Now process all nodes together for type generation and function declarations
        let nodes = &all_nodes;

        self.generate_user_defined_types(nodes)?;

        let imported_functions: Vec<(String, FunctionNode)> = self
            .analyzer
            .all_module_asts()
            .iter()
            .flat_map(|(module_path, module_nodes)| {
                let module_name_for_mangling = Self::sanitize_module_path(module_path);
                module_nodes
                    .iter()
                    .filter_map(|node| {
                        if let AstNode::Function(func) = node {
                            if func.type_params.is_empty() {
                                Some((module_name_for_mangling.clone(), func.clone()))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        // Declare functions from imported modules with mangled names
        for (module_name, func) in &imported_functions {
            // Store function nodes
            self.function_nodes.insert(func.name.clone(), func.clone());

            // Declare with mangled name
            let mangled_name = format!("{}!{}", module_name, func.name);
            self.declare_function_with_name(func, &mangled_name)?;
        }

        // Declare functions from main module (no mangling needed)
        for node in main_module_nodes {
            if let AstNode::Function(func) = node {
                // Store function nodes
                self.function_nodes.insert(func.name.clone(), func.clone());

                // Declare non-generic functions
                if func.type_params.is_empty() {
                    // Mangled name to avoid conflict with entry point if function is named "main"
                    let llvm_name = if func.name == "main" {
                        "!user!main".to_string()
                    } else {
                        func.name.clone()
                    };
                    self.declare_function_with_name(func, &llvm_name)?;
                }
            }
        }

        // declare class methods with prefixed names
        for node in nodes {
            if let AstNode::Class { name, methods, .. } = node {
                for method in methods {
                    let prefixed_name = format!("{}.{}", name, method.name);
                    let mut method_copy = method.clone();
                    method_copy.name = prefixed_name;
                    self.declare_function(&method_copy)?;
                }
            }
        }

        // generate vtables after all functions are declared
        for node in nodes {
            if let AstNode::Class { name, .. } = node {
                let interfaces = self
                    .analyzer
                    .all_symbols()
                    .get(name)
                    .map(|sym| sym.interfaces.clone())
                    .unwrap_or_default();
                self.generate_class_vtables(name, &interfaces)?;
            }
        }

        // generate constructor functions after vtables
        for node in nodes {
            match node {
                AstNode::Enum { name, variants, .. } => {
                    self.generate_enum_constructors(name, variants)?;
                }
                AstNode::Class { name, fields, .. } => {
                    let interfaces = self
                        .analyzer
                        .all_symbols()
                        .get(name)
                        .map(|sym| sym.interfaces.clone())
                        .unwrap_or_default();
                    self.generate_class_constructors(name, fields, &interfaces)?;
                }
                _ => {}
            }
        }

        let mut top_level_statements = vec![];
        for node in nodes {
            if let AstNode::Statement(stmt) = node {
                top_level_statements.push(stmt.clone());
            }
        }

        let mut user_functions = vec![];
        for node in main_module_nodes {
            if let AstNode::Function(func) = node {
                if func.type_params.is_empty() {
                    user_functions.push(func.clone());
                }
            }
        }

        let mut main_top_level_statements = vec![];
        for node in main_module_nodes {
            if let AstNode::Statement(stmt) = node {
                main_top_level_statements.push(stmt.clone());
            }
        }

        // First, analyze top-level statements to identify global variable declarations
        // and create LLVM global variables for them
        // Use top_level_statements (from all modules) because LLVM globals must be declared at module scope
        for stmt in &top_level_statements {
            match &stmt.kind {
                StatementKind::TypedDecl(name, type_, _)
                | StatementKind::ConstDecl(name, type_, _) => {
                    let resolved_type = self
                        .analyzer
                        .resolve_type(type_)
                        .map_err(|e| e.to_string())?;

                    // determine correct LLVM type based on whether value is boxed
                    let llvm_type = match &resolved_type {
                        Type::Primitive(_) => {
                            // primitives are boxed, use ptr type
                            self.context.ptr_type(AddressSpace::default()).into()
                        }
                        _ => {
                            // enums, classes, etc. use their actual struct type
                            self.llvm_type_from_mux_type(type_)?
                        }
                    };

                    let global = self.module.add_global(llvm_type, None, name);
                    global.set_initializer(&llvm_type.const_zero());

                    // store in global_variables for later access
                    self.global_variables.insert(
                        name.clone(),
                        (global.as_pointer_value(), llvm_type, resolved_type),
                    );
                }
                StatementKind::AutoDecl(name, _, expr) => {
                    // for auto declarations, get the inferred type from the expression
                    // (not from symbol table, since variables are not stored there to avoid collisions)
                    let resolved_type = self
                        .analyzer
                        .get_expression_type(expr)
                        .map_err(|e| format!("Failed to get type for {}: {}", name, e.message))?;

                    // determine correct LLVM type based on whether value is boxed
                    let llvm_type = match &resolved_type {
                        Type::Primitive(_) => {
                            // primitives are boxed, use ptr type
                            self.context.ptr_type(AddressSpace::default()).into()
                        }
                        _ => {
                            // enums, classes, etc. use their actual struct type
                            let type_node = self.type_to_type_node(&resolved_type);
                            self.llvm_type_from_mux_type(&type_node)?
                        }
                    };

                    let global = self.module.add_global(llvm_type, None, name);
                    global.set_initializer(&llvm_type.const_zero());

                    self.global_variables.insert(
                        name.clone(),
                        (global.as_pointer_value(), llvm_type, resolved_type.clone()),
                    );
                }
                _ => {}
            }
        }

        // Generate module initialization functions for all imported modules
        // First collect all module data to avoid borrowing conflicts
        let modules_data: Vec<(String, Vec<StatementNode>)> = self
            .analyzer
            .all_module_asts()
            .iter()
            .map(|(module_path, module_nodes)| {
                let mut module_top_level_statements = vec![];

                // Extract top-level statements from this module
                for node in module_nodes {
                    if let AstNode::Statement(stmt) = node {
                        module_top_level_statements.push(stmt.clone());
                    }
                }

                (module_path.replace('/', "_"), module_top_level_statements)
            })
            .collect();

        // Now generate init functions for each module
        for (module_name, module_top_level_statements) in modules_data {
            self.generate_module_init(&module_top_level_statements, &module_name)?;
        }

        // Generate module initialization function for the main module
        // Use main_top_level_statements (only from main module) to avoid re-initializing imported globals
        let module_name = self.get_module_name(main_module_nodes);
        self.generate_module_init(&main_top_level_statements, &module_name)?;

        // Always generate main function (even for modules without top-level statements)
        // This allows class-only files to be compiled and executed directly
        self.generate_main_function(&module_name)?;

        // Generate user-defined functions for imported modules
        for (module_name_mangled, func) in imported_functions {
            let mangled_name = format!("{}!{}", module_name_mangled, func.name);
            self.generate_function_with_llvm_name(&func, &mangled_name)?;
        }

        // Generate user-defined functions for main module (no mangling except for "main")
        for func in user_functions {
            if func.name == "main" {
                self.generate_function_with_llvm_name(&func, "!user!main")?;
            } else {
                self.generate_function(&func)?;
            }
        }

        // generate class methods with prefixed names
        for node in nodes {
            if let AstNode::Class {
                name,
                methods,
                type_params,
                ..
            } = node
            {
                // Set class-level type parameter bounds for method generation
                if !type_params.is_empty() {
                    let bounds: Vec<(String, Vec<String>)> = type_params
                        .iter()
                        .map(|(p, b)| (p.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
                        .collect();
                    self.analyzer.set_class_type_params(bounds);
                }

                for method in methods {
                    let prefixed_name = format!("{}.{}", name, method.name);
                    // generate non-generic class methods, OR
                    // generate static methods with no type parameters that DON'T use class type params
                    if type_params.is_empty() {
                        let mut method_copy = method.clone();
                        method_copy.name = prefixed_name;
                        self.generate_function(&method_copy)?;
                    } else {
                        let class_type_param_names: Vec<&str> =
                            type_params.iter().map(|(p, _)| p.as_str()).collect();
                        if method.is_common
                            && method.type_params.is_empty()
                            && !Self::method_uses_type_params(method, &class_type_param_names)
                        {
                            // static method with no type params and doesn't use class type params - can generate once
                            let mut method_copy = method.clone();
                            method_copy.name = prefixed_name;
                            self.generate_function(&method_copy)?;
                        }
                    }
                }

                // Clear class-level type params after generating all methods for this class
                if !type_params.is_empty() {
                    self.analyzer.clear_class_type_params();
                }
            }
        }

        Ok(())
    }

    pub fn emit_ir_to_file(&self, filename: &str) -> Result<(), String> {
        self.module
            .print_to_file(filename)
            .map_err(|e| format!("Failed to write IR: {}", e))
    }
}

// Re-export all submodules
mod classes;
mod constructors;
mod expressions;
mod functions;
mod generics;
mod memory;
mod methods;
mod operators;
mod runtime;
mod statements;
mod types;
