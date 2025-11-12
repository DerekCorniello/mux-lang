use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, Either};
use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::*;
use crate::semantics::{MethodSig, SemanticAnalyzer, Type, Type as ResolvedType};

pub struct CodeGenerator<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    analyzer: &'a mut SemanticAnalyzer,
    type_map: HashMap<String, BasicTypeEnum<'a>>,
    #[allow(dead_code)]
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
    functions: HashMap<String, FunctionValue<'a>>,
    current_function_name: Option<String>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(context: &'a Context, analyzer: &'a mut SemanticAnalyzer) -> Self {
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

        // mux_println_val: (*mut Value) -> ()
        let params = &[i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_println_val", fn_type, None);

        // exit: (i32) -> ()
        let params = &[context.i32_type().into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("exit", fn_type, None);

        // mux_string_concat: (*const c_char, *const c_char) -> *mut c_char
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_concat", fn_type, None);

        // mux_value_get_string: (*const Value) -> *const c_char
        let fn_type = i8_ptr.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_string", fn_type, None);

        // mux_value_from_string: (*const c_char) -> *mut Value
        let fn_type = i8_ptr.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_from_string", fn_type, None);

        // mux_int_to_string: (i64) -> *const c_char
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_int_to_string", fn_type, None);

        // mux_int_to_float: (i64) -> *mut Value
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_int_to_float", fn_type, None);

        // mux_float_to_int: (f64) -> *mut Value
        let params = &[f64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_float_to_int", fn_type, None);

        // mux_float_to_string: (f64) -> *const c_char
        let params = &[f64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_float_to_string", fn_type, None);

        // mux_bool_to_string: (bool) -> *const c_char
        let params = &[bool_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_bool_to_string", fn_type, None);

        // mux_bool_to_int: (*mut Value) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_bool_to_int", fn_type, None);

        // mux_bool_to_float: (*mut Value) -> *mut Value
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_bool_to_float", fn_type, None);

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

        // Object management functions
        // mux_register_object_type: (*const c_char, usize) -> u32
        let params = &[i8_ptr.into(), context.i64_type().into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_register_object_type", fn_type, None);

        // mux_alloc_object: (u32) -> *mut Value
        let params = &[context.i32_type().into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_alloc_object", fn_type, None);

        // mux_free_object: (*mut Value) -> ()
        let params = &[i8_ptr.into()];
        let fn_type = context.void_type().fn_type(params, false);
        module.add_function("mux_free_object", fn_type, None);

        // mux_get_object_ptr: (*const Value) -> *mut c_void
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_get_object_ptr", fn_type, None);

        // mux_set_to_string: (*mut Set) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_set_to_string", fn_type, None);

        // mux_optional_to_string: (*const Optional) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_to_string", fn_type, None);

        // mux_value_from_optional: (*mut Optional) -> *mut Value
        let fn_type = i8_ptr.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_from_optional", fn_type, None);

        // mux_value_get_list: (*mut Value) -> *mut List
        let fn_type = list_ptr.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_list", fn_type, None);

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

        // Safe value extraction functions
        // mux_value_get_int: (*const Value) -> i64
        let fn_type = i64_type.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_int", fn_type, None);

        // mux_value_get_float: (*const Value) -> f64
        let fn_type = context.f64_type().fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_float", fn_type, None);

        // mux_value_get_bool: (*const Value) -> bool
        let fn_type = context.bool_type().fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_bool", fn_type, None);

        // mux_value_get_type_tag: (*const Value) -> i32
        let fn_type = context.i32_type().fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_type_tag", fn_type, None);

        // mux_free_value: (*mut Value) -> ()
        let fn_type = void_type.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_free_value", fn_type, None);

        // List operations
        // mux_list_push_back: (*mut List, *mut Value) -> ()
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push_back", fn_type, None);

        // mux_list_get: (*const List, i64) -> *mut Optional
        let params = &[list_ptr.into(), i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_get", fn_type, None);

        // mux_list_get_value: (*const List, i64) -> *mut Value
        let params = &[list_ptr.into(), i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_get_value", fn_type, None);

        // mux_list_length: (*const List) -> i64
        let params = &[list_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_list_length", fn_type, None);

        // mux_value_list_length: (*const Value) -> i64
        let params = &[i8_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_value_list_length", fn_type, None);

        // mux_value_list_get_value: (*const Value, i64) -> *mut Value
        let params = &[i8_ptr.into(), i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_value_list_get_value", fn_type, None);

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

        // Use BTreeMap to ensure deterministic ordering of enum variants
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

        // Generate types for user-defined enums
        for (name, symbol) in analyzer.all_symbols() {
            if symbol.kind == crate::semantics::SymbolKind::Enum {
                // Assume all data are f64, max 2 fields for simplicity
                let i32_type = context.i32_type();
                let f64_type = context.f64_type();
                let struct_type = context
                    .struct_type(&[i32_type.into(), f64_type.into(), f64_type.into()], false);
                type_map.insert(name.clone(), struct_type.into());

                // Collect variants
                let mut variants = vec![];
                for (method_name, _) in &symbol.methods {
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
            functions: HashMap::new(),
            current_function_name: None,
        }
    }

    fn generate_user_defined_types(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        // Generate LLVM types for classes, interfaces, enums
        for node in nodes {
            match node {
                AstNode::Class { name, fields, .. } => {
                    let symbol = self.analyzer.all_symbols().get(name).unwrap();
                    let interfaces = symbol.interfaces.clone();
                    self.classes.insert(name.clone(), fields.clone());
                    self.generate_class_type(name, fields, &interfaces)?;
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

    fn generate_class_type(
        &mut self,
        name: &str,
        fields: &[Field],
        interfaces: &std::collections::HashMap<
            String,
            std::collections::HashMap<String, MethodSig>,
        >,
    ) -> Result<(), String> {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();

        // Add vtable fields for implemented interfaces FIRST
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        for interface_name in interfaces.keys() {
            field_types.push(ptr_type.into());
            field_indices.insert(format!("vtable_{}", interface_name), field_types.len() - 1);
        }

        // Add class fields after
        for field in fields {
            let field_type = self.llvm_type_from_mux_type(&field.type_)?;
            field_types.push(field_type);
            field_indices.insert(field.name.clone(), field_types.len() - 1);
        }

        let struct_type = self.context.struct_type(&field_types, false);
        self.type_map.insert(name.to_string(), struct_type.into());
        self.field_map.insert(name.to_string(), field_indices);
        self.field_types_map.insert(name.to_string(), field_types);

        Ok(())
    }

    fn generate_class_vtables(
        &mut self,
        class_name: &str,
        interfaces: &std::collections::HashMap<
            String,
            std::collections::HashMap<String, MethodSig>,
        >,
    ) -> Result<(), String> {
        println!("Generating vtables for class: {}", class_name);
        for (interface_name, interface_methods) in interfaces {
            println!("  Interface: {}", interface_name);
            let mut vtable_values = Vec::new();
            for (method_name, _interface_sig) in interface_methods {
                // Find the class method
                let class_method_name = format!("{}.{}", class_name, method_name);
                println!(
                    "    Method: {} -> looking for {}",
                    method_name, class_method_name
                );
                let func = self.functions.get(&class_method_name).ok_or_else(|| {
                    println!(
                        "    ERROR: Method {} not found in functions map",
                        class_method_name
                    );
                    println!(
                        "    Available functions: {:?}",
                        self.functions.keys().collect::<Vec<_>>()
                    );
                    format!(
                        "Class {} does not implement method {} for interface {}",
                        class_name, method_name, interface_name
                    )
                })?;
                vtable_values.push(func.as_global_value().as_basic_value_enum());
            }
            // Get vtable struct type
            let vtable_type = self.vtable_type_map.get(interface_name).unwrap();
            let vtable_const = vtable_type.const_named_struct(&vtable_values);
            // Create global
            let vtable_name = format!("{}_{}_vtable", class_name, interface_name);
            let global =
                self.module
                    .add_global(vtable_type.as_basic_type_enum(), None, &vtable_name);
            global.set_initializer(&vtable_const);
            self.vtable_map.insert(
                format!("{}_{}", class_name, interface_name),
                global.as_pointer_value(),
            );
        }
        Ok(())
    }

    fn generate_interface_type(&mut self, name: &str) -> Result<(), String> {
        // Generate LLVM struct for interface: { *mut vtable }
        // For simplicity, vtable is struct of void* function pointers
        let symbol = self.analyzer.all_symbols().get(name).unwrap();
        let interface_methods = symbol.interfaces.get(name).unwrap();

        // Create vtable as struct of function pointers (all (void*) -> void* for now)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[ptr_type.into()], false);
        let fn_ptr_type = ptr_type; // since fn_type.ptr_type deprecated, use ptr_type

        let mut vtable_types = vec![fn_ptr_type.into(); interface_methods.len()];

        // Vtable type: struct of function pointers
        let vtable_struct_type = self.context.struct_type(&vtable_types, false);
        self.vtable_type_map
            .insert(name.to_string(), vtable_struct_type);
        let vtable_ptr_type = self.context.ptr_type(AddressSpace::default());

        // Interface struct: { vtable_ptr }
        let interface_struct_type = self.context.struct_type(&[vtable_ptr_type.into()], false);
        self.type_map
            .insert(name.to_string(), interface_struct_type.into());

        Ok(())
    }

    fn generate_enum_type(&mut self, name: &str, variants: &[EnumVariant]) -> Result<(), String> {
        // Tagged union: {i32 discriminant, f64 data0, f64 data1} for now
        let i32_type = self.context.i32_type();
        let f64_type = self.context.f64_type();

        let mut variant_names = Vec::new();
        let mut variant_fields = HashMap::new();
        let mut max_fields = 0;
        for variant in variants {
            variant_names.push(variant.name.clone());
            let field_types = variant.data.as_ref().unwrap_or(&vec![]).clone();
            max_fields = max_fields.max(field_types.len());
            variant_fields.insert(variant.name.clone(), field_types);
        }
        self.enum_variants.insert(name.to_string(), variant_names);
        self.enum_variant_fields
            .insert(name.to_string(), variant_fields);

        // Create struct type with discriminant + maximum number of data fields
        let mut struct_fields = vec![i32_type.into()]; // discriminant first
        for _ in 0..max_fields {
            struct_fields.push(f64_type.into()); // use f64 as the largest common type
        }
        let struct_type = self.context.struct_type(&struct_fields, false);
        self.type_map.insert(name.to_string(), struct_type.into());
        Ok(())
    }

    fn generate_enum_constructors(
        &mut self,
        name: &str,
        variants: &[EnumVariant],
    ) -> Result<(), String> {
        for variant in variants {
            let variant_name = &variant.name;
            let full_name = format!("{}_{}", name, variant_name);

            // params: variant.data
            let data_types = if let Some(ref d) = variant.data {
                d
            } else {
                &vec![]
            };
            let mut param_types = vec![];
            for type_node in data_types {
                let llvm_type = self.llvm_type_from_mux_type(type_node)?;
                param_types.push(llvm_type.into());
            }

            // return type: enum struct
            let enum_type = self.type_map.get(name).ok_or("Enum type not found")?;
            let struct_type = enum_type.into_struct_type();
            let fn_type = enum_type.fn_type(&param_types, false);
            let function = self.module.add_function(&full_name, fn_type, None);

            // Generate the body
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            // Build the struct by storing to temp and loading
            let tag_index = self.get_variant_index(name, variant_name)?;
            let tag_val = self.context.i32_type().const_int(tag_index as u64, false);
            let temp_ptr = self
                .builder
                .build_alloca(struct_type, "temp_struct")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(temp_ptr, struct_type.const_zero())
                .map_err(|e| e.to_string())?;
            let tag_ptr = self
                .builder
                .build_struct_gep(struct_type, temp_ptr, 0, "tag_ptr")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(tag_ptr, tag_val)
                .map_err(|e| e.to_string())?;
            for (i, _) in data_types.iter().enumerate() {
                let arg = function.get_nth_param(i as u32).unwrap();
                let data_ptr = self
                    .builder
                    .build_struct_gep(struct_type, temp_ptr, (i + 1) as u32, "data_ptr")
                    .map_err(|e| e.to_string())?;
                // Debug: print the value being stored
                if arg.is_float_value() {
                    println!("DEBUG: Storing float value at index {}", i + 1);
                }
                self.builder
                    .build_store(data_ptr, arg)
                    .map_err(|e| e.to_string())?;
            }
            let struct_val = self
                .builder
                .build_load(struct_type, temp_ptr, "struct")
                .map_err(|e| e.to_string())?;
            // Return the struct
            self.builder
                .build_return(Some(&struct_val))
                .map_err(|e| e.to_string())?;

            // Store in constructors
            self.constructors
                .insert(format!("{}.{}", name, variant_name), function);
        }
        Ok(())
    }

    fn generate_class_constructors(
        &mut self,
        name: &str,
        fields: &[Field],
        interfaces: &std::collections::HashMap<
            String,
            std::collections::HashMap<String, MethodSig>,
        >,
    ) -> Result<(), String> {
        let full_name = format!("{}_new", name);

        // params: field types
        let mut param_types = vec![];
        for field in fields {
            let llvm_type = self.llvm_type_from_mux_type(&field.type_)?;
            param_types.push(llvm_type.into());
        }

        // return type: *mut Value (boxed object)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let fn_type = ptr_type.fn_type(&param_types, false);
        let function = self.module.add_function(&full_name, fn_type, None);

        // Generate the body
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Register the object type if not already registered
        let type_name = format!("type_name_{}", name);
        let type_name_global = self
            .builder
            .build_global_string_ptr(name, &type_name)
            .map_err(|e| e.to_string())?;
        if let Some(global) = self.module.get_global(&type_name) {
            global.set_linkage(inkwell::module::Linkage::External);
        }
        let type_size = self
            .type_map
            .get(name)
            .ok_or("Class type not found")?
            .size_of()
            .ok_or("Cannot get type size")?;
        let register_func = self
            .module
            .get_function("mux_register_object_type")
            .ok_or("mux_register_object_type not found")?;
        let type_id = self
            .builder
            .build_call(
                register_func,
                &[type_name_global.as_pointer_value().into(), type_size.into()],
                "type_id",
            )
            .map_err(|e| e.to_string())?;
        let type_id_val = type_id
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();

        // Allocate the object
        let alloc_func = self
            .module
            .get_function("mux_alloc_object")
            .ok_or("mux_alloc_object not found")?;
        let obj_ptr = self
            .builder
            .build_call(alloc_func, &[type_id_val.into()], "obj_ptr")
            .map_err(|e| e.to_string())?;
        let obj_value_ptr = obj_ptr
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        // Get the object data pointer
        let get_ptr_func = self
            .module
            .get_function("mux_get_object_ptr")
            .ok_or("mux_get_object_ptr not found")?;
        let data_ptr = self
            .builder
            .build_call(get_ptr_func, &[obj_value_ptr.into()], "data_ptr")
            .map_err(|e| e.to_string())?;
        let struct_ptr = data_ptr
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        // Cast to class struct pointer
        let class_type = self.type_map.get(name).ok_or("Class type not found")?;
        let struct_ptr_typed = self
            .builder
            .build_pointer_cast(
                struct_ptr,
                class_type.ptr_type(AddressSpace::default()),
                "struct_ptr",
            )
            .map_err(|e| e.to_string())?;

        // Set fields
        for (i, field) in fields.iter().enumerate() {
            let field_index = self.field_map.get(name).unwrap().get(&field.name).unwrap();
            let field_ptr = self
                .builder
                .build_struct_gep(
                    *class_type,
                    struct_ptr_typed,
                    *field_index as u32,
                    &field.name,
                )
                .map_err(|e| e.to_string())?;
            let arg = function.get_nth_param(i as u32).unwrap();
            self.builder
                .build_store(field_ptr, arg)
                .map_err(|e| e.to_string())?;
        }

        // Set vtable fields
        for interface_name in interfaces.keys() {
            let vtable_key = format!("{}_{}", name, interface_name);
            let vtable_ptr = self
                .vtable_map
                .get(&vtable_key)
                .ok_or(format!("Vtable not found for {}", vtable_key))?;
            let vtable_field_name = format!("vtable_{}", interface_name);
            let field_index = self
                .field_map
                .get(name)
                .unwrap()
                .get(&vtable_field_name)
                .unwrap();
            let field_ptr = self
                .builder
                .build_struct_gep(
                    *class_type,
                    struct_ptr_typed,
                    *field_index as u32,
                    &vtable_field_name,
                )
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(field_ptr, *vtable_ptr)
                .map_err(|e| e.to_string())?;
        }

        // Return the Value pointer
        self.builder
            .build_return(Some(&obj_value_ptr))
            .map_err(|e| e.to_string())?;

        // Store in constructors
        self.constructors.insert(format!("{}.new", name), function);
        Ok(())
    }

    fn get_variant_index(&self, enum_name: &str, variant_name: &str) -> Result<usize, String> {
        // Hardcode indices for built-in enums to ensure deterministic behavior
        match (enum_name, variant_name) {
            ("Optional", "Some") => Ok(0),
            ("Optional", "None") => Ok(1),
            ("Result", "Ok") => Ok(0),
            ("Result", "Err") => Ok(1),
            _ => {
                // For user-defined enums, use HashMap lookup
                if let Some(variants) = self.enum_variants.get(enum_name) {
                    variants
                        .iter()
                        .position(|v| v == variant_name)
                        .ok_or_else(|| {
                            format!("Variant {} not found in enum {}", variant_name, enum_name)
                        })
                } else {
                    Err(format!("Enum {} not found", enum_name))
                }
            }
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
        // Zero pass: generate LLVM types for user-defined types
        self.generate_user_defined_types(nodes)?;

        // First pass: declare all functions
        for node in nodes {
            if let AstNode::Function(func) = node {
                self.declare_function(func)?;
            }
        }

        // Declare class methods with prefixed names
        for node in nodes {
            if let AstNode::Class { name, methods, .. } = node {
                for method in methods {
                    let prefixed_name = format!("{}.{}", name, method.name);
                    println!("Declaring class method: {}", prefixed_name);
                    let mut method_copy = method.clone();
                    method_copy.name = prefixed_name;
                    self.declare_function(&method_copy)?;
                }
            }
        }

        // Generate vtables after all functions are declared
        for node in nodes {
            if let AstNode::Class { name, .. } = node {
                let symbol = self.analyzer.all_symbols().get(name).unwrap();
                let interfaces = symbol.interfaces.clone();
                self.generate_class_vtables(name, &interfaces)?;
            }
        }

        // Generate constructor functions after vtables
        for node in nodes {
            match node {
                AstNode::Enum { name, variants, .. } => {
                    self.generate_enum_constructors(name, variants)?;
                }
                AstNode::Class { name, fields, .. } => {
                    let symbol = self.analyzer.all_symbols().get(name).unwrap();
                    let interfaces = symbol.interfaces.clone();
                    self.generate_class_constructors(name, fields, &interfaces)?;
                }
                _ => {}
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

        // Generate class methods with prefixed names
        for node in nodes {
            if let AstNode::Class { name, methods, .. } = node {
                for method in methods {
                    let prefixed_name = format!("{}.{}", name, method.name);
                    println!("Generating class method: {}", prefixed_name);
                    let mut method_copy = method.clone();
                    method_copy.name = prefixed_name;
                    self.generate_function(&method_copy)?;
                }
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
        let mut param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type_from_mux_type(&p.type_).map(|t| t.into()))
            .collect::<Result<_, _>>()?;

        // For class methods, add implicit 'self' parameter
        let is_class_method = func.name.contains('.');
        if is_class_method {
            let class_name = func.name.split('.').next().unwrap();
            let class_type = self
                .type_map
                .get(class_name)
                .ok_or_else(|| format!("Class type {} not found", class_name))?;
            param_types.insert(0, class_type.ptr_type(AddressSpace::default()).into());
        }

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
        self.current_function_name = Some(func.name.clone());

        let function = *self
            .functions
            .get(&func.name)
            .ok_or("Function not declared")?;

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Clear variables for new scope
        self.variables.clear();

        // Set up parameter variables
        let is_class_method = func.name.contains('.');
        let mut param_index = 0;
        if is_class_method {
            let class_name = func.name.split('.').next().unwrap();
            let class_type = self.type_map.get(class_name).unwrap();
            let arg = function.get_nth_param(param_index).unwrap();
            param_index += 1;
            // Set self as variable
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let alloca = self
                .builder
                .build_alloca(ptr_type, "self")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, arg)
                .map_err(|e| e.to_string())?;
            self.variables.insert(
                "self".to_string(),
                (
                    alloca,
                    ptr_type.into(),
                    ResolvedType::Named(class_name.to_string(), vec![]),
                ),
            );
        }

        for (i, param) in func.params.iter().enumerate() {
            let arg = function
                .get_nth_param((i as u32) + (param_index as u32))
                .unwrap();
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
                        Type::Named(type_name, _) => {
                            println!("DEBUG: Loading variable of type: {}", type_name);
                            if type_name == "Optional" || type_name == "Result" {
                                // Optional/Result: return pointer
                                Ok(ptr_to_boxed.into())
                            } else if self
                                .analyzer
                                .symbol_table()
                                .lookup(type_name)
                                .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                                .unwrap_or(false)
                            {
                                // Custom enums: load as struct value
                                println!(
                                    "DEBUG: Loading custom enum {} as struct value",
                                    type_name
                                );
                                let struct_type =
                                    self.type_map.get(type_name).unwrap().into_struct_type();
                                let struct_val = self
                                    .builder
                                    .build_load(struct_type, *ptr, name)
                                    .map_err(|e| e.to_string())?;
                                Ok(struct_val.into())
                            } else {
                                // Classes and other named types: keep as pointer
                                Ok(ptr_to_boxed.into())
                            }
                        }
                        Type::Primitive(PrimitiveType::Int) => {
                            let raw_int = self.get_raw_int_value(ptr_to_boxed.into())?;
                            Ok(raw_int.into())
                        }
                        Type::Primitive(PrimitiveType::Float) => {
                            let raw_float = self.get_raw_float_value(ptr_to_boxed.into())?;
                            Ok(raw_float.into())
                        }
                        Type::Primitive(PrimitiveType::Bool) => {
                            let raw_bool = self.get_raw_bool_value(ptr_to_boxed.into())?;
                            Ok(raw_bool.into())
                        }
                        Type::Primitive(PrimitiveType::Str) => Ok(ptr_to_boxed.into()),
                        _ => {
                            // boxed types
                            Ok(ptr_to_boxed.into())
                        }
                    }
                } else {
                    if self
                        .analyzer
                        .symbol_table()
                        .lookup(name)
                        .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                        .unwrap_or(false)
                    {
                        Err(format!("Enums cannot be used as values: {}", name))
                    } else {
                        // Check if class method field access
                        if let Some(ref func_name) = self.current_function_name {
                            if func_name.contains('.') {
                                let class_name = func_name.split('.').next().unwrap();
                                if let Some((self_ptr, _, _)) = self.variables.get("self") {
                                    if let Some(field_index) = self
                                        .field_map
                                        .get(class_name)
                                        .and_then(|fields| fields.get(name))
                                    {
                                        // Extract the actual enum value from the object field
                                        // self_ptr is an alloca containing the object data pointer, so load it first
                                        let object_data_ptr_val = self
                                            .builder
                                            .build_load(
                                                self.context.ptr_type(AddressSpace::default()),
                                                *self_ptr,
                                                "object_data_ptr",
                                            )
                                            .map_err(|e| e.to_string())?;
                                        let object_data_ptr =
                                            object_data_ptr_val.into_pointer_value();

                                        // Cast to the class struct type (GenericShape)
                                        let class_type = self
                                            .type_map
                                            .get(class_name)
                                            .ok_or("Class type not found")?;
                                        let struct_ptr_typed = self
                                            .builder
                                            .build_pointer_cast(
                                                object_data_ptr,
                                                class_type.ptr_type(AddressSpace::default()),
                                                "struct_ptr_typed",
                                            )
                                            .map_err(|e| e.to_string())?;

                                        // Get pointer to the specific field (shape field should be index 1, after vtable at index 0)
                                        // Let's use hardcoded index 1 for now to test
                                        let field_ptr = self
                                            .builder
                                            .build_struct_gep(
                                                *class_type,
                                                struct_ptr_typed,
                                                1u32,
                                                "field_ptr",
                                            )
                                            .map_err(|e| e.to_string())?;

                                        // Get the field type and load the enum value
                                        let class_fields = self
                                            .classes
                                            .get(class_name)
                                            .ok_or("Class not found")?;
                                        let field = class_fields
                                            .iter()
                                            .find(|f| f.name == *name)
                                            .ok_or("Field not found")?;
                                        let field_type =
                                            self.llvm_type_from_mux_type(&field.type_)?;
                                        // Load the actual enum value from the object field
                                        let enum_val = self
                                            .builder
                                            .build_load(field_type, field_ptr, "field_enum")
                                            .map_err(|e| e.to_string())?;
                                        return Ok(enum_val.into());
                                    }
                                }
                            }
                        }
                        Err(format!("Undefined variable: {}", name))
                    }
                }
            }
            ExpressionKind::Binary { left, op, right } => {
                if op.is_assignment() {
                    match op {
                        BinaryOp::Assign => {
                            let right_val = self.generate_expression(right)?;
                            if let ExpressionKind::Identifier(name) = &left.kind {
                                if let Some((ptr, _, type_node)) = self.variables.get(name) {
                                    let ptr_copy = *ptr;
                                    // Don't box enum struct values - store them directly
                                    let value_to_store = if let Type::Named(type_name, _) =
                                        type_node
                                    {
                                        let is_enum = self
                                            .analyzer
                                            .symbol_table()
                                            .lookup(type_name)
                                            .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                                            .unwrap_or(false);
                                        println!(
                                            "DEBUG: Storing variable of type {} (is_enum: {})",
                                            type_name, is_enum
                                        );
                                        if is_enum {
                                            // For enum types, store struct value directly (don't box)
                                            right_val
                                        } else {
                                            // For class types, box the value
                                            self.box_value(right_val).into()
                                        }
                                    } else {
                                        // For primitive types, box the value
                                        self.box_value(right_val).into()
                                    };
                                    self.builder
                                        .build_store(ptr_copy, value_to_store)
                                        .map_err(|e| e.to_string())?;
                                    Ok(right_val)
                                } else {
                                    Err(format!("Undefined variable {}", name))
                                }
                            } else if let ExpressionKind::Unary {
                                op: UnaryOp::Deref,
                                expr: deref_expr,
                                postfix: _,
                            } = &left.kind
                            {
                                let ref_val = self.generate_expression(deref_expr)?;
                                let ptr = ref_val.into_pointer_value();
                                let boxed = self.box_value(right_val);
                                self.builder
                                    .build_store(ptr, boxed)
                                    .map_err(|e| e.to_string())?;
                                Ok(right_val)
                            } else {
                                Err("Assignment to non-identifier/deref not implemented"
                                    .to_string())
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
                            } else if let ExpressionKind::Unary {
                                op: UnaryOp::Deref,
                                expr: deref_expr,
                                postfix: _,
                            } = &left.kind
                            {
                                let ref_val = self.generate_expression(deref_expr)?;
                                let ptr = ref_val.into_pointer_value();
                                let boxed = self.box_value(result);
                                self.builder
                                    .build_store(ptr, boxed)
                                    .map_err(|e| e.to_string())?;
                                Ok(result)
                            } else {
                                Err("Assignment to non-identifier/deref not implemented"
                                    .to_string())
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
                            } else if let ExpressionKind::Unary {
                                op: UnaryOp::Deref,
                                expr: deref_expr,
                                postfix: _,
                            } = &left.kind
                            {
                                let ref_val = self.generate_expression(deref_expr)?;
                                let ptr = ref_val.into_pointer_value();
                                let boxed = self.box_value(result);
                                self.builder
                                    .build_store(ptr, boxed)
                                    .map_err(|e| e.to_string())?;
                                Ok(result)
                            } else {
                                Err("Assignment to non-identifier/deref not implemented"
                                    .to_string())
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


                    // Special case: method calls on 'self' (keep existing logic)
                    if let ExpressionKind::Identifier(obj_name) = &expr.kind {

                        if obj_name == "self" {
                            return self.generate_method_call_on_self(field, args);
                        }
                    }

                    // NEW: Handle method calls on ANY expression type
                    let obj_value = self.generate_expression(expr)?;
                    
                    // Use semantic analyzer for robust type inference
                    let obj_type = self.analyzer.get_expression_type(expr)
                        .map_err(|e| format!("Type inference failed: {}", e))?;
                    
                    return self.generate_method_call(obj_value, &obj_type, field, args);
                } else if let ExpressionKind::Identifier(name) = &func.kind {
                    // Handle regular function calls (non-methods)
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
                        "println" => {
                            if args.len() != 1 {
                                return Err("println takes 1 argument".to_string());
                            }
                            let arg_val = self.generate_expression(&args[0])?;
                            let func_println = self
                                .module
                                .get_function("mux_println_val")
                                .ok_or("mux_println_val not found")?;
                            self.builder
                                .build_call(func_println, &[arg_val.into()], "println_call")
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
                            let result_ptr = call.try_as_basic_value().left().unwrap();
                            // Result constructors return Value* pointers directly
                            Ok(result_ptr)
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
                            let result_ptr = call.try_as_basic_value().left().unwrap();
                            // Result constructors return Value* pointers directly
                            Ok(result_ptr)
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
                            let result_ptr = call.try_as_basic_value().left().unwrap();
                            // Optional constructors return Value* pointers directly
                            Ok(result_ptr)
                        }
                        "None" => {
                            if args.len() != 0 {
                                return Err("None takes 0 arguments".to_string());
                            }
                            let func = self
                                .module
                                .get_function("mux_optional_none")
                                .ok_or("mux_optional_none not found")?;
                            let call = self
                                .builder
                                .build_call(func, &[], "none_call")
                                .map_err(|e| e.to_string())?;
                            let result_ptr = call.try_as_basic_value().left().unwrap();
                            // Optional constructors return Value* pointers directly
                            Ok(result_ptr)
                        }
                        _ => {
                            // User-defined function calls
                            if let Some(func) = self.module.get_function(name) {
                                let mut call_args = vec![];
                                for arg in args {
                                    call_args.push(self.generate_expression(arg)?.into());
                                }
                                let call = self
                                    .builder
                                    .build_call(func, &call_args, "user_func_call")
                                    .map_err(|e| e.to_string())?;
                                // Handle void functions - return a dummy value
                                match call.try_as_basic_value().left() {
                                    Some(val) => Ok(val),
                                    None => Ok(self.context.i32_type().const_int(0, false).into()),
                                }
                            } else {
                                Err(format!("Undefined function: {}", name))
                            }
                        }
                    }
                } else {
                    Err(format!("Unsupported function call type"))
                }
            }
            ExpressionKind::ListAccess { expr, index } => {
                let list_val = self.generate_expression(expr)?;
                let index_val = self.generate_expression(index)?;
                
                // Extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_list").unwrap(),
                        &[list_val.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;
                
                // Call mux_list_get_value (returns direct value or null)
                let raw_result = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_get_value").unwrap(),
                        &[raw_list.try_as_basic_value().left().unwrap().into(), index_val.into()],
                        "list_raw",
                    )
                    .map_err(|e| e.to_string())?;

                let result_ptr = raw_result
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();

                // Check for null (out of bounds)
                let is_null = self
                    .builder
                    .build_is_null(result_ptr, "is_null")
                    .map_err(|e| e.to_string())?;

                // Get current function for basic blocks
                let current_function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .ok_or("No current function")?;

                // Create error block and continue block
                let error_bb = self
                    .context
                    .append_basic_block(current_function, "index_error");
                let continue_bb = self
                    .context
                    .append_basic_block(current_function, "index_continue");

                self.builder
                    .build_conditional_branch(is_null, error_bb, continue_bb)
                    .map_err(|e| e.to_string())?;

                // Error block: print error and exit
                self.builder.position_at_end(error_bb);
                let error_msg = self
                    .builder
                    .build_global_string_ptr("List index out of bounds", "error_msg")
                    .map_err(|e| e.to_string())?;
                let error_str = self
                    .generate_runtime_call(
                        "mux_new_string_from_cstr",
                        &[error_msg.as_pointer_value().into()],
                    )
                    .unwrap();
                self.generate_runtime_call("mux_println_val", &[error_str.into()]);
                self.generate_runtime_call(
                    "exit",
                    &[self.context.i32_type().const_int(1, false).into()],
                );
                self.builder
                    .build_unreachable()
                    .map_err(|e| e.to_string())?;

                // Continue block: return the actual value, not pointer
                self.builder.position_at_end(continue_bb);
                
                // The result_ptr is already a *mut Value pointer, which represents our value
                // In the LLVM world, we treat this as the value itself (like box_value does)
                Ok(result_ptr.into())
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
                // Convert list pointer to Value for type consistency
                let list_value = self
                    .generate_runtime_call("mux_list_value", &[list_ptr.into()])
                    .unwrap();
                Ok(list_value)
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
                let map_value = self
                    .generate_runtime_call("mux_map_value", &[map_ptr.into()])
                    .unwrap();
                Ok(map_value)
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
                let set_value = self
                    .generate_runtime_call("mux_set_value", &[set_ptr.into()])
                    .unwrap();
                Ok(set_value)
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
                                        // Check if this field is an enum type
                                        let field_types = self
                                            .field_types_map
                                            .get(class_name.as_str())
                                            .ok_or("Field types not found for class")?;
                                        if index < field_types.len() {
                                            let field_type = field_types[index];
                                            // Check if field type is a struct (enum)
                                            if let BasicTypeEnum::StructType(struct_type) =
                                                field_type
                                            {
                                                // For enum fields: load as struct value
                                                println!(
                                                    "DEBUG: Loading enum field {} as struct value",
                                                    field
                                                );
                                                let loaded = self
                                                    .builder
                                                    .build_load(struct_type, field_ptr, field)
                                                    .map_err(|e| e.to_string())?;
                                                return Ok(loaded);
                                            }
                                        }
                                        // For non-enum fields: keep existing behavior
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
                        println!("DEBUG: Variable type node: {:?}", type_node);
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
            ExpressionKind::Unary {
                op,
                expr,
                postfix: _,
            } => {
                match op {
                    UnaryOp::Ref => {
                        if let ExpressionKind::Identifier(name) = &expr.kind {
                            if let Some((ptr, _, _)) = self.variables.get(name) {
                                // For identifier references: return pointer to the alloca containing the boxed value
                                // Don't dereference - we want a reference to the variable itself
                                Ok((*ptr).into())
                            } else {
                                Err(format!("Undefined variable {}", name))
                            }
                        } else {
                            // Complex expression: evaluate, allocate temp ptr, store the result ptr
                            let expr_val = self.generate_expression(expr)?; // ptr to mux_value
                            let ptr_type = self.context.ptr_type(AddressSpace::default());
                            let temp = self
                                .builder
                                .build_alloca(ptr_type, "ref_temp")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(temp, expr_val.into_pointer_value())
                                .map_err(|e| e.to_string())?;
                            Ok(temp.into())
                        }
                    }
                    UnaryOp::Deref => self.generate_expression(expr),
                    _ => Err("Unary op not implemented".to_string()),
                }
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
                let symbol = self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .ok_or("Symbol not found")?;
                let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;
                if value.is_struct_value() {
                    let var_type = value.get_type();
                    let alloca = self
                        .builder
                        .build_alloca(var_type, name)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(alloca, value)
                        .map_err(|e| e.to_string())?;
                    self.variables
                        .insert(name.clone(), (alloca, var_type, resolved_type.clone()));
                } else {
                    let boxed = self.box_value(value);
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let alloca = self
                        .builder
                        .build_alloca(ptr_type, name)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(alloca, boxed)
                        .map_err(|e| e.to_string())?;
                    self.variables.insert(
                        name.clone(),
                        (
                            alloca,
                            BasicTypeEnum::PointerType(ptr_type),
                            resolved_type.clone(),
                        ),
                    );
                }
            }
            StatementKind::TypedDecl(name, type_node, expr) => {
                let var_type = self.llvm_type_from_mux_type(type_node)?;
                let value = self.generate_expression(expr)?;
                let symbol = self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .ok_or("Symbol not found")?;
                let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;
                if value.is_struct_value() {
                    let alloca = self
                        .builder
                        .build_alloca(var_type, name)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(alloca, value)
                        .map_err(|e| e.to_string())?;
                    self.variables
                        .insert(name.clone(), (alloca, var_type, resolved_type.clone()));
                } else {
                    let boxed = self.box_value(value);
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let alloca = self
                        .builder
                        .build_alloca(ptr_type, name)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(alloca, boxed)
                        .map_err(|e| e.to_string())?;
                    self.variables.insert(
                        name.clone(),
                        (
                            alloca,
                            BasicTypeEnum::PointerType(ptr_type),
                            resolved_type.clone(),
                        ),
                    );
                }
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

                let if_id = self.label_counter;
                self.label_counter += 1;
                let then_bb = self
                    .context
                    .append_basic_block(*function, &format!("if_then_{}", if_id));
                let else_bb = self
                    .context
                    .append_basic_block(*function, &format!("if_else_{}", if_id));
                let merge_bb = self
                    .context
                    .append_basic_block(*function, &format!("if_merge_{}", if_id));

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
                            self.module.get_function("mux_value_list_length").unwrap(),
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

                    // Create a temporary exit block for the conditional branch
                    // We'll move the actual exit block creation after body processing
                    let temp_exit_bb = self
                        .context
                        .append_basic_block(*function, &format!("temp_for_exit_{}", label_id));

                    self.builder
                        .build_conditional_branch(cmp, body_bb, temp_exit_bb)
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
                            self.module
                                .get_function("mux_value_list_get_value")
                                .unwrap(),
                            &[list_val.into(), index_load2.into()],
                            "list_get_value",
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

                    // Create the real exit block now that body is processed
                    let exit_bb = self
                        .context
                        .append_basic_block(*function, &format!("for_exit_{}", label_id));

                    // Move from temp block to real exit block
                    self.builder.position_at_end(temp_exit_bb);
                    self.builder
                        .build_unconditional_branch(exit_bb)
                        .map_err(|e| e.to_string())?;

                    // Position at real exit block
                    self.builder.position_at_end(exit_bb);

                    // Add a default return only for functions that return Optional/Result types
                    // This prevents LLVM IR syntax errors from missing returns
                    if let Some(current_func) = self.current_function_name.as_ref() {
                        if let Some(func_symbol) = self.analyzer.symbol_table().lookup(current_func)
                        {
                            if let Some(Type::Named(name, _)) = &func_symbol.type_ {
                                if name == "Optional" {
                                    // Return None for Optional functions that reach here
                                    let none_func =
                                        self.module.get_function("mux_optional_none").unwrap();
                                    let none_call = self
                                        .builder
                                        .build_call(none_func, &[], "none_default")
                                        .map_err(|e| e.to_string())?;
                                    let none_ptr = none_call.try_as_basic_value().left().unwrap();
                                    self.builder
                                        .build_return(Some(&none_ptr))
                                        .map_err(|e| e.to_string())?;
                                }
                                // Don't add default return for void functions or other types
                            } else if let Some(function) = self.functions.get(current_func) {
                                // Check if function returns a pointer (indicating Optional/Result)
                                let return_type = function.get_type().get_return_type();
                                if let Some(return_type) = return_type {
                                    if return_type.is_pointer_type() {
                                        // Function returns pointer, likely Optional/Result
                                        let none_func =
                                            self.module.get_function("mux_optional_none");
                                        if let Some(none_func) = none_func {
                                            let none_call = self
                                                .builder
                                                .build_call(none_func, &[], "none_default")
                                                .map_err(|e| e.to_string())?;
                                            let none_ptr =
                                                none_call.try_as_basic_value().left().unwrap();
                                            self.builder
                                                .build_return(Some(&none_ptr))
                                                .map_err(|e| e.to_string())?;
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    return Err("For loop iter must be range(...) or list identifier".to_string());
                }
            }
            StatementKind::Match { expr, arms } => {
                let function = function.ok_or("Match not in function")?;

                let expr_val = self.generate_expression(expr)?;
                println!("DEBUG: Match expression generated value: {:?}", expr_val);

                let enum_name = match &expr.kind {
                    ExpressionKind::Identifier(name) => {
                        if let Some(symbol) = self.analyzer.symbol_table().lookup(name) {
                            if let Some(Type::Named(n, _)) = &symbol.type_ {
                                n.clone()
                            } else {
                                return Err("Match expression must be an enum type".to_string());
                            }
                        } else {
                            return Err(format!("Symbol {} not found", name));
                        }
                    }
                    ExpressionKind::FieldAccess { expr, field } => {
                        if let ExpressionKind::Identifier(obj) = &expr.kind {
                            if obj == "self" {
                                if let Some((_, _, Type::Named(class_name, _))) =
                                    self.variables.get("self")
                                {
                                    if let Some(fields) = self.classes.get(class_name) {
                                        if let Some(f) = fields.iter().find(|f| f.name == *field) {
                                            if let crate::parser::TypeKind::Named(n, _) =
                                                &f.type_.kind
                                            {
                                                n.clone()
                                            } else {
                                                return Err(
                                                    "Match field must be enum type".to_string()
                                                );
                                            }
                                        } else {
                                            return Err(format!(
                                                "Field {} not found in class {}",
                                                field, class_name
                                            ));
                                        }
                                    } else {
                                        return Err(format!("Class {} not found", class_name));
                                    }
                                } else {
                                    return Err("Self not found".to_string());
                                }
                            } else {
                                return Err(
                                    "Match expression must be identifier or self.field".to_string()
                                );
                            }
                        } else {
                            return Err(
                                "Match expression must be identifier or self.field".to_string()
                            );
                        }
                    }
                    ExpressionKind::Call { func, .. } => {
                        // Handle constructor calls like Some(15), None, etc.
                        if let ExpressionKind::Identifier(constructor_name) = &func.kind {
                            // Map common constructor names to their enum types
                            match constructor_name.as_str() {
                                "Some" | "None" => "Optional".to_string(),
                                "Ok" | "Err" => "Result".to_string(),
                                _ => {
                                    // For other constructors, try to look up as enum type
                                    if let Some(symbol) =
                                        self.analyzer.symbol_table().lookup(constructor_name)
                                    {
                                        if let Some(Type::Named(type_name, _)) = &symbol.type_ {
                                            type_name.clone()
                                        } else {
                                            return Err("Constructor must be enum type".to_string());
                                        }
                                    } else {
                                        return Err(format!(
                                            "Constructor {} not found",
                                            constructor_name
                                        ));
                                    }
                                }
                            }
                        } else {
                            return Err(
                                "Match expression constructor calls must be simple identifiers"
                                    .to_string(),
                            );
                        }
                    }
                    _ => return Err(
                        "Match expression must be identifier, field access, or constructor call"
                            .to_string(),
                    ),
                };
                let expr_ptr_opt = if enum_name == "Optional" || enum_name == "Result" {
                    // For Optional/Result constructor calls, we need to allocate the struct and get a pointer
                    if expr_val.is_pointer_value() {
                        Some(expr_val.into_pointer_value())
                    } else {
                        // This is a struct value (from constructor call), allocate it and get pointer
                        let struct_val = expr_val.into_struct_value();
                        let ptr_type = struct_val.get_type().ptr_type(AddressSpace::default());
                        let alloca = self
                            .builder
                            .build_alloca(struct_val.get_type(), "temp_enum")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_store(alloca, struct_val)
                            .map_err(|e| e.to_string())?;
                        Some(alloca)
                    }
                } else {
                    None
                };

                // Get discriminant
                let (discriminant, temp_ptr_opt) = if let Some(expr_ptr) = expr_ptr_opt {
                    let discriminant_func = if enum_name == "Optional" {
                        "mux_optional_discriminant"
                    } else {
                        "mux_result_discriminant"
                    };
                    let func = self
                        .module
                        .get_function(discriminant_func)
                        .ok_or(format!("{} not found", discriminant_func))?;
                    let discriminant_call = self
                        .builder
                        .build_call(func, &[expr_ptr.into()], "discriminant_call")
                        .map_err(|e| e.to_string())?;
                    (
                        discriminant_call
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                            .into_int_value(),
                        None,
                    )
                } else {
                    // For custom enums, load the discriminant field
                    let struct_type = self.type_map.get(&enum_name).unwrap().into_struct_type();
                    let temp_ptr = self
                        .builder
                        .build_alloca(struct_type, "temp_struct")
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(temp_ptr, expr_val)
                        .map_err(|e| e.to_string())?;
                    let discriminant_ptr = self
                        .builder
                        .build_struct_gep(struct_type, temp_ptr, 0, "discriminant_ptr")
                        .map_err(|e| e.to_string())?;
                    let discriminant = self
                        .builder
                        .build_load(self.context.i32_type(), discriminant_ptr, "discriminant")
                        .map_err(|e| e.to_string())?
                        .into_int_value();
                    (discriminant, Some(temp_ptr))
                };

                let mut current_bb = self.builder.get_insert_block().unwrap();
                let match_id = self.label_counter;
                self.label_counter += 1;
                let end_bb = self
                    .context
                    .append_basic_block(*function, &format!("match_end_{}", match_id));

                for (i, arm) in arms.iter().enumerate() {
                    let arm_bb = self
                        .context
                        .append_basic_block(*function, &format!("match_arm_{}_{}", match_id, i));
                    let next_bb = if i < arms.len() - 1 {
                        self.context.append_basic_block(
                            *function,
                            &format!("match_next_{}_{}", match_id, i),
                        )
                    } else {
                        end_bb
                    };

                    self.builder.position_at_end(current_bb);

                    let pattern_matches = match &arm.pattern {
                        PatternNode::EnumVariant { name, args: _ } => {
                            let variant_index = self.get_variant_index(&enum_name, name)?;
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
                        PatternNode::Identifier(_) => self.context.bool_type().const_int(1, false),
                        PatternNode::Literal(_) => self.context.bool_type().const_int(1, false),
                        PatternNode::Tuple(_) => self.context.bool_type().const_int(1, false),
                        PatternNode::Wildcard => self.context.bool_type().const_int(1, false),
                    };

                    let cond = pattern_matches;

                    self.builder
                        .build_conditional_branch(cond, arm_bb, next_bb)
                        .map_err(|e| e.to_string())?;

                    // Arm body
                    self.builder.position_at_end(arm_bb);

                    // Bind variables
                    if let &PatternNode::EnumVariant {
                        name: ref name,
                        ref args,
                    } = &arm.pattern
                    {
                        if let Some(expr_ptr) = expr_ptr_opt {
                            if (*name == "Some" || *name == "Ok" || *name == "Err")
                                && !args.is_empty()
                            {
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
                                    let resolved_type = if enum_name == "Optional" {
                                        Type::Primitive(PrimitiveType::Int)
                                    } else {
                                        Type::Primitive(PrimitiveType::Str)
                                    };
                                    self.variables.insert(
                                        var.clone(),
                                        (alloca, ptr_type.into(), resolved_type),
                                    );
                                }
                            }
                        } else {
                            // Custom enum - use variant-specific field information
                            let struct_type =
                                self.type_map.get(&enum_name).unwrap().into_struct_type();
                            let field_types_clone = if let Some(variant_fields) =
                                self.enum_variant_fields.get(&enum_name)
                            {
                                if let Some(field_types) = variant_fields.get(name) {
                                    field_types.clone()
                                } else {
                                    return Err(format!(
                                        "Variant {} not found in enum {}",
                                        name, enum_name
                                    ));
                                }
                            } else {
                                return Err(format!(
                                    "No field information found for enum {}",
                                    enum_name
                                ));
                            };

                            for (i, arg) in args.iter().enumerate() {
                                if let PatternNode::Identifier(var) = arg {
                                    let data_index = i + 1; // Start after discriminant at index 0
                                    let data_ptr = self
                                        .builder
                                        .build_struct_gep(
                                            struct_type,
                                            temp_ptr_opt.unwrap(),
                                            data_index as u32,
                                            "data_ptr",
                                        )
                                        .map_err(|e| e.to_string())?;

                                    // Get the actual field type to load correctly
                                    let field_type: BasicTypeEnum<'_> =
                                        if i < field_types_clone.len() {
                                            match &field_types_clone[i].kind {
                                                TypeKind::Primitive(PrimitiveType::Float) => {
                                                    self.context.f64_type().into()
                                                }
                                                TypeKind::Primitive(PrimitiveType::Int) => {
                                                    self.context.i64_type().into()
                                                }
                                                TypeKind::Primitive(PrimitiveType::Bool) => {
                                                    self.context.bool_type().into()
                                                }
                                                TypeKind::Primitive(PrimitiveType::Str) => self
                                                    .context
                                                    .ptr_type(AddressSpace::default())
                                                    .into(),
                                                _ => self.context.f64_type().into(), // fallback to float
                                            }
                                        } else {
                                            self.context.f64_type().into() // fallback
                                        };

                                    println!(
                                        "DEBUG: Loading float value from index {} for variant {}",
                                        data_index, name
                                    );
                                    let data_val = self
                                        .builder
                                        .build_load(field_type, data_ptr, "data")
                                        .map_err(|e| e.to_string())?;
                                    // Debug: try to print the loaded value before boxing
                                    if data_val.is_float_value() {
                                        println!("DEBUG: Loaded float value before boxing");
                                    }
                                    let boxed = self.box_value(data_val);
                                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                                    let alloca = self
                                        .builder
                                        .build_alloca(ptr_type, var)
                                        .map_err(|e| e.to_string())?;
                                    self.builder
                                        .build_store(alloca, boxed)
                                        .map_err(|e| e.to_string())?;

                                    // Use the actual field type for resolved type
                                    let resolved_type = if i < field_types_clone.len() {
                                        self.analyzer
                                            .resolve_type(&field_types_clone[i])
                                            .map_err(|e| e.to_string())?
                                    } else {
                                        Type::Primitive(PrimitiveType::Float) // fallback
                                    };

                                    self.variables.insert(
                                        var.clone(),
                                        (alloca, ptr_type.into(), resolved_type),
                                    );
                                }
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

    fn generate_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        obj_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match obj_type {
            Type::Primitive(prim) => {
                self.generate_primitive_method_call(obj_value, prim, method_name, args)
            }
            Type::List(elem_type) => {
                self.generate_list_method_call(obj_value, elem_type, method_name, args)
            }
            Type::Map(key_type, value_type) => {
                self.generate_map_method_call(obj_value, key_type, value_type, method_name, args)
            }
            Type::Set(elem_type) => {
                self.generate_set_method_call(obj_value, elem_type, method_name, args)
            }
            Type::Named(name, _) => {
                // TODO: Future implementation
                Err(format!(
                    "Named type methods not yet implemented: {}",
                    method_name
                ))
            }
            Type::Optional(inner_type) => {
                self.generate_optional_method_call(obj_value, inner_type, method_name, args)
            }
            _ => Err(format!(
                "Method {} not implemented for type {:?}",
                method_name, obj_type
            )),
        }
    }

    fn generate_primitive_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        prim: &PrimitiveType,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match prim {
            PrimitiveType::Int => match method_name {
                "to_string" => {
                    let func = self
                        .module
                        .get_function("mux_value_to_string")
                        .ok_or("mux_value_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "value_to_str")
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
                    Ok(call2.try_as_basic_value().left().unwrap())
                }
                "to_float" => {
                    let func = self
                        .module
                        .get_function("mux_int_to_float")
                        .ok_or("mux_int_to_float not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "int_to_float")
                        .map_err(|e| e.to_string())?;
                    Ok(call.try_as_basic_value().left().unwrap())
                }
                "to_int" => {
                    // int.to_int() just returns itself (identity operation)
                    Ok(obj_value)
                }
                _ => Err(format!("Method {} not implemented for int", method_name)),
            },
            PrimitiveType::Float => match method_name {
                "to_string" => {
                    let func = self
                        .module
                        .get_function("mux_value_to_string")
                        .ok_or("mux_value_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "value_to_str")
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
                    Ok(call2.try_as_basic_value().left().unwrap())
                }
                "to_int" => {
                    let func = self
                        .module
                        .get_function("mux_float_to_int")
                        .ok_or("mux_float_to_int not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "float_to_int")
                        .map_err(|e| e.to_string())?;
                    Ok(call.try_as_basic_value().left().unwrap())
                }
                "to_float" => {
                    // float.to_float() just returns itself (identity operation)
                    Ok(obj_value)
                }
                _ => Err(format!("Method {} not implemented for float", method_name)),
            },
            PrimitiveType::Str => match method_name {
                "to_string" => {
                    let func = self
                        .module
                        .get_function("mux_value_to_string")
                        .ok_or("mux_value_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "value_to_str")
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
                    Ok(call2.try_as_basic_value().left().unwrap())
                }
                "length" => {
                    let func = self
                        .module
                        .get_function("mux_string_length")
                        .ok_or("mux_string_length not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "str_len")
                        .map_err(|e| e.to_string())?;
                    Ok(call.try_as_basic_value().left().unwrap())
                }
                _ => Err(format!("Method {} not implemented for string", method_name)),
            },
            PrimitiveType::Bool => match method_name {
                "to_string" => {
                    eprintln!("DEBUG: Compiling bool.to_string() method call");
                    let func = self
                        .module
                        .get_function("mux_value_to_string")
                        .ok_or("mux_value_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "value_to_str")
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
                    Ok(call2.try_as_basic_value().left().unwrap())
                }
                "to_int" => {
                    let func = self
                        .module
                        .get_function("mux_bool_to_int")
                        .ok_or("mux_bool_to_int not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "bool_to_int")
                        .map_err(|e| e.to_string())?;
                    Ok(call.try_as_basic_value().left().unwrap())
                }
                "to_float" => {
                    let func = self
                        .module
                        .get_function("mux_bool_to_float")
                        .ok_or("mux_bool_to_float not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "bool_to_float")
                        .map_err(|e| e.to_string())?;
                    Ok(call.try_as_basic_value().left().unwrap())
                }
                _ => Err(format!("Method {} not implemented for bool", method_name)),
            },
            PrimitiveType::Char => match method_name {
                "to_string" => {
                    eprintln!("DEBUG: Compiling char.to_string() method call");
                    let func = self
                        .module
                        .get_function("mux_bool_to_string")
                        .ok_or("mux_bool_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "bool_to_str")
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
                    Ok(call2.try_as_basic_value().left().unwrap())
                }
                _ => Err(format!("Method {} not implemented for char", method_name)),
            },
            _ => Err(format!(
                "Method {} not implemented for primitive type {:?}",
                method_name, prim
            )),
        }
    }

    fn generate_list_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        elem_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "to_string" => {
                let func = self
                    .module
                    .get_function("mux_value_to_string")
                    .ok_or("mux_value_to_string not found")?;
                let call = self
                    .builder
                    .build_call(func, &[obj_value.into()], "val_to_str")
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
                Ok(call2.try_as_basic_value().left().unwrap())
            }
            "get" => {
                if args.len() != 1 {
                    return Err("get() method takes exactly 1 argument".to_string());
                }
                let index_val = self.generate_expression(&args[0])?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_get").unwrap(),
                        &[obj_value.into(), index_val.into()],
                        "list_get",
                    )
                    .map_err(|e| e.to_string())?;
                // Box the Optional as a Value
                let boxed_call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_from_optional").unwrap(),
                        &[call.try_as_basic_value().left().unwrap().into()],
                        "optional_as_value",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(boxed_call.try_as_basic_value().left().unwrap())
            }
            "push_back" => {
                if args.len() != 1 {
                    return Err("push_back() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                self.generate_runtime_call(
                    "mux_list_push_back",
                    &[obj_value.into(), elem_ptr.into()],
                );
                Ok(self.context.i32_type().const_int(0, false).into()) // Return dummy value
            }
            "pop_back" => {
                if !args.is_empty() {
                    return Err("pop_back() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_pop_back").unwrap(),
                        &[obj_value.into()],
                        "list_pop_back",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_is_empty").unwrap(),
                        &[obj_value.into()],
                        "list_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "length" => {
                if !args.is_empty() {
                    return Err("length() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_length").unwrap(),
                        &[obj_value.into()],
                        "list_length",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            _ => Err(format!("Method {} not implemented for lists", method_name)),
        }
    }

    fn generate_map_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        _key_type: &Type,
        _value_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                let func = self
                    .module
                    .get_function("mux_value_to_string")
                    .ok_or("mux_value_to_string not found")?;
                let call = self
                    .builder
                    .build_call(func, &[obj_value.into()], "val_to_str")
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
                Ok(call2.try_as_basic_value().left().unwrap())
            }
            "put" => {
                if args.len() != 2 {
                    return Err("put() method takes exactly 2 arguments".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let value_val = self.generate_expression(&args[1])?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_put").unwrap(),
                        &[obj_value.into(), key_val.into(), value_val.into()],
                        "map_put",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "get" => {
                if args.len() != 1 {
                    return Err("get() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_get").unwrap(),
                        &[obj_value.into(), key_val.into()],
                        "map_get",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_contains").unwrap(),
                        &[obj_value.into(), key_val.into()],
                        "map_contains",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_size").unwrap(),
                        &[obj_value.into()],
                        "map_size",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_is_empty").unwrap(),
                        &[obj_value.into()],
                        "map_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            _ => Err(format!("Method {} not implemented for maps", method_name)),
        }
    }

    fn generate_set_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        _elem_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                let func = self
                    .module
                    .get_function("mux_value_to_string")
                    .ok_or("mux_value_to_string not found")?;
                let call = self
                    .builder
                    .build_call(func, &[obj_value.into()], "val_to_str")
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
                Ok(call2.try_as_basic_value().left().unwrap())
            }
            "add" => {
                if args.len() != 1 {
                    return Err("add() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_set_add").unwrap(),
                        &[obj_value.into(), elem_val.into()],
                        "set_add",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_set_contains").unwrap(),
                        &[obj_value.into(), elem_val.into()],
                        "set_contains",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_set_size").unwrap(),
                        &[obj_value.into()],
                        "set_size",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_set_is_empty").unwrap(),
                        &[obj_value.into()],
                        "set_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            _ => Err(format!("Method {} not implemented for sets", method_name)),
        }
    }

    fn generate_optional_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        _inner_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                // Use the standard mux_value_to_string function which handles Optional case
                let func = self
                    .module
                    .get_function("mux_value_to_string")
                    .ok_or("mux_value_to_string not found")?;
                let call = self
                    .builder
                    .build_call(func, &[obj_value.into()], "val_to_str")
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
                Ok(call2.try_as_basic_value().left().unwrap())
            }
            _ => Err(format!("Method {} not implemented for Optionals", method_name)),
        }
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
                let bool_val = self
                    .generate_runtime_call("mux_bool_value", &[val.into()])
                    .unwrap();
                Ok(bool_val.into())
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
                global.set_linkage(inkwell::module::Linkage::External);
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
                // Check for string concatenation first
                if let (Ok(left_is_string), Ok(right_is_string)) = (self.is_string_value(left), self.is_string_value(right)) {
                    if left_is_string || right_is_string {
                        // Use string concatenation
                        let left_ptr = if left.is_pointer_value() { left.into_pointer_value() } else {
                            // Convert non-pointer to string pointer
                            self.box_value(left)
                        };
                        let right_ptr = if right.is_pointer_value() { right.into_pointer_value() } else {
                            // Convert non-pointer to string pointer
                            self.box_value(right)
                        };
                        
                        // Extract C strings from Value pointers
                        let left_cstr = self.extract_c_string_from_value(left_ptr)?;
                        let right_cstr = self.extract_c_string_from_value(right_ptr)?;
                        
                        // Call string concatenation function
                        let concat_fn = self.module.get_function("mux_string_concat")
                            .ok_or("mux_string_concat not found")?;
                        let result = self.builder.build_call(concat_fn, &[left_cstr.into(), right_cstr.into()], "string_concat")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;
                        
                        // Convert result back to Value pointer
                        return self.box_string_value(result.into_pointer_value());
                    }
                }
                
                // Try arithmetic operations
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_add(left_int, right_int, "add")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_add(left_float, right_float, "fadd")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    // Try mixed type: float + int or int + float
                    if let (Ok(left_float), Ok(right_int)) = (self.get_raw_float_value(left), self.get_raw_int_value(right)) {
                        let right_float = self.builder.build_signed_int_to_float(right_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_add(left_float, right_float, "fadd_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else if let (Ok(left_int), Ok(right_float)) = (self.get_raw_int_value(left), self.get_raw_float_value(right)) {
                        let left_float = self.builder.build_signed_int_to_float(left_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_add(left_float, right_float, "fadd_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else {
                        Err("Unsupported add operands".to_string())
                    }
                }
            }
            BinaryOp::Subtract => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_sub(left_int, right_int, "sub")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_sub(left_float, right_float, "fsub")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    // Try mixed type: float - int or int - float
                    if let (Ok(left_float), Ok(right_int)) = (self.get_raw_float_value(left), self.get_raw_int_value(right)) {
                        let right_float = self.builder.build_signed_int_to_float(right_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_sub(left_float, right_float, "fsub_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else if let (Ok(left_int), Ok(right_float)) = (self.get_raw_int_value(left), self.get_raw_float_value(right)) {
                        let left_float = self.builder.build_signed_int_to_float(left_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_sub(left_float, right_float, "fsub_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else {
                        Err("Unsupported sub operands".to_string())
                    }
                }
            }
            BinaryOp::Multiply => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_mul(left_int, right_int, "mul")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_mul(left_float, right_float, "fmul")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    // Try mixed type: float * int or int * float
                    if let (Ok(left_float), Ok(right_int)) = (self.get_raw_float_value(left), self.get_raw_int_value(right)) {
                        let right_float = self.builder.build_signed_int_to_float(right_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_mul(left_float, right_float, "fmul_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else if let (Ok(left_int), Ok(right_float)) = (self.get_raw_int_value(left), self.get_raw_float_value(right)) {
                        let left_float = self.builder.build_signed_int_to_float(left_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_mul(left_float, right_float, "fmul_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else {
                        Err("Unsupported mul operands".to_string())
                    }
                }
            }
            BinaryOp::Divide => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_signed_div(left_int, right_int, "div")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_div(left_float, right_float, "fdiv")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    // Try mixed type: float / int or int / float
                    if let (Ok(left_float), Ok(right_int)) = (self.get_raw_float_value(left), self.get_raw_int_value(right)) {
                        let right_float = self.builder.build_signed_int_to_float(right_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_div(left_float, right_float, "fdiv_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else if let (Ok(left_int), Ok(right_float)) = (self.get_raw_int_value(left), self.get_raw_float_value(right)) {
                        let left_float = self.builder.build_signed_int_to_float(left_int, self.context.f64_type(), "int_to_float")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_float_div(left_float, right_float, "fdiv_mixed")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    } else {
                        Err("Unsupported div operands".to_string())
                    }
                }
            }
            BinaryOp::Equal => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            left_int,
                            right_int,
                            "eq",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            left_float,
                            right_float,
                            "feq",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported eq operands".to_string())
                }
            }
            BinaryOp::Less => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLT,
                            left_int,
                            right_int,
                            "lt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLT,
                            left_float,
                            right_float,
                            "flt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported lt operands".to_string())
                }
            }
            BinaryOp::Greater => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGT,
                            left_int,
                            right_int,
                            "gt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGT,
                            left_float,
                            right_float,
                            "fgt",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported gt operands".to_string())
                }
            }
            BinaryOp::LessEqual => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLE,
                            left_int,
                            right_int,
                            "le",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLE,
                            left_float,
                            right_float,
                            "fle",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported le operands".to_string())
                }
            }
            BinaryOp::GreaterEqual => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGE,
                            left_int,
                            right_int,
                            "ge",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGE,
                            left_float,
                            right_float,
                            "fge",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported ge operands".to_string())
                }
            }
            BinaryOp::NotEqual => {
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::NE,
                            left_int,
                            right_int,
                            "ne",
                        )
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (self.get_raw_float_value(left), self.get_raw_float_value(right)) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::ONE,
                            left_float,
                            right_float,
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
                // Try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) = (self.get_raw_int_value(left), self.get_raw_int_value(right)) {
                    self.builder
                        .build_int_signed_rem(left_int, right_int, "mod")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported mod operands".to_string())
                }
            }
            BinaryOp::In => {
                // 'in' operator - check if left is contained in right
                // For now, return false as a placeholder implementation
                // A complete implementation would require runtime functions for:
                // - List containment: iterate through elements
                // - Set containment: hash lookup
                // - Map containment: key lookup
                // - String containment: substring search
                let false_val = self.context.bool_type().const_zero();
                Ok(false_val.into())
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
                if self.enum_variants.contains_key(name) {
                    if name == "Optional" || name == "Result" {
                        // For Optional/Result: values are always Value* pointers
                        Ok(self.context.ptr_type(AddressSpace::default()).into())
                    } else {
                        // For custom enums: values are struct values
                        let struct_type = self.type_map.get(name).ok_or("Enum type not found")?;
                        Ok(*struct_type)
                    }
                } else {
                    // For classes, values are pointers to structs
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
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
        let func = match self.module.get_function(name) {
            Some(f) => f,
            None => {
                panic!("Function '{}' not found in module", name);
            }
        };
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
            // Assume string or already boxed Value (from Map/Set/List literals)
            // Map/Set/List literals already return *mut Value pointers, so just return as-is
            val.into_pointer_value()
        } else {
            // For bool
            let call = self
                .generate_runtime_call("mux_bool_value", &[val.into()])
                .unwrap();
            call.into_pointer_value()
        }
    }

    fn get_raw_int_value(&mut self, val: BasicValueEnum<'a>) -> Result<IntValue<'a>, String> {
        if val.is_int_value() {
            Ok(val.into_int_value())
        } else if val.is_pointer_value() {
            // Use safe runtime function to extract int
            let ptr = val.into_pointer_value();
            let get_int_fn = self.module.get_function("mux_value_get_int")
                .ok_or("mux_value_get_int not found")?;
            let result = self.builder.build_call(get_int_fn, &[ptr.into()], "get_int")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("Call returned no value")?;
            Ok(result.into_int_value())
        } else {
            Err("Expected int value or pointer".to_string())
        }
    }

    fn get_raw_float_value(&mut self, val: BasicValueEnum<'a>) -> Result<FloatValue<'a>, String> {
        if val.is_float_value() {
            Ok(val.into_float_value())
        } else if val.is_pointer_value() {
            // Use safe runtime function to extract float
            let ptr = val.into_pointer_value();
            let get_float_fn = self.module.get_function("mux_value_get_float")
                .ok_or("mux_value_get_float not found")?;
            let result = self.builder.build_call(get_float_fn, &[ptr.into()], "get_float")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("Call returned no value")?;
            Ok(result.into_float_value())
        } else {
            Err("Expected float value or pointer".to_string())
        }
    }

    fn get_raw_bool_value(&mut self, val: BasicValueEnum<'a>) -> Result<inkwell::values::IntValue<'a>, String> {
        if val.is_int_value() {
            Ok(val.into_int_value())
        } else if val.is_pointer_value() {
            // Use safe runtime function to extract bool
            let ptr = val.into_pointer_value();
            let get_bool_fn = self.module.get_function("mux_value_get_bool")
                .ok_or("mux_value_get_bool not found")?;
            let result = self.builder.build_call(get_bool_fn, &[ptr.into()], "get_bool")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("Call returned no value")?;
            // Convert bool to i64 for LLVM compatibility
            let extended = self.builder.build_int_z_extend(
                result.into_int_value(), 
                self.context.i64_type(), 
                "bool_to_i64"
            ).map_err(|e| e.to_string())?;
            Ok(extended)
        } else {
            Err("Expected bool value or pointer".to_string())
        }
    }

    fn is_string_value(&mut self, val: BasicValueEnum<'a>) -> Result<bool, String> {
        if val.is_pointer_value() {
            let ptr = val.into_pointer_value();
            let get_type_fn = self.module.get_function("mux_value_get_type_tag")
                .ok_or("mux_value_get_type_tag not found")?;
            let type_tag = self.builder.build_call(get_type_fn, &[ptr.into()], "get_type")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("Call returned no value")?
                .into_int_value();
            
            // Check if type tag is 3 (String)
            let is_string = self.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                type_tag,
                self.context.i32_type().const_int(3, false), // String type tag = 3
                "is_string"
            ).map_err(|e| e.to_string())?;
            
            Ok(is_string.get_zero_extended_constant() != Some(0))
        } else {
            Ok(false)
        }
    }

    fn cleanup_value(&mut self, val: BasicValueEnum<'a>) -> Result<(), String> {
        if val.is_pointer_value() {
            let ptr = val.into_pointer_value();
            let free_fn = self.module.get_function("mux_free_value")
                .ok_or("mux_free_value not found")?;
            self.builder.build_call(free_fn, &[ptr.into()], "free_value")
                .map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    fn extract_c_string_from_value(&mut self, value_ptr: PointerValue<'a>) -> Result<PointerValue<'a>, String> {
        // Call mux_value_get_string to extract C string from Value
        let get_string_fn = self.module.get_function("mux_value_get_string")
            .ok_or("mux_value_get_string not found")?;
        let cstr_ptr = self.builder.build_call(get_string_fn, &[value_ptr.into()], "get_string")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?
            .into_pointer_value();
        Ok(cstr_ptr)
    }

    fn box_string_value(&mut self, cstr_ptr: PointerValue<'a>) -> Result<BasicValueEnum<'a>, String> {
        // Call mux_value_from_string to create a Value from C string
        let from_string_fn = self.module.get_function("mux_value_from_string")
            .ok_or("mux_value_from_string not found")?;
        let value_ptr = self.builder.build_call(from_string_fn, &[cstr_ptr.into()], "from_string")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?;
        Ok(value_ptr)
    }

    fn generate_method_call_on_self(
        &mut self,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // Get self pointer
        let (self_ptr, _, _) = self
            .variables
            .get("self")
            .ok_or("Self not found in method call")?;

        // Get class name from self type
        let class_name =
            if let Some((_, _, Type::Named(class_name, _))) = self.variables.get("self") {
                class_name
            } else {
                return Err("Self type not found".to_string());
            };

        // Build method function name: {class_name}.{field}
        let method_func_name = format!("{}.{}", class_name, field);

        // Get the function
        let func_val = *self
            .functions
            .get(&method_func_name)
            .ok_or(format!("Method {} not found", method_func_name))?;

        // Build call arguments: self + args
        let mut call_args = vec![(*self_ptr).into()];
        for arg in args {
            call_args.push(self.generate_expression(arg)?.into());
        }

        // Call the method
        let call = self
            .builder
            .build_call(func_val, &call_args, &format!("call_{}", field))
            .map_err(|e| e.to_string())?;

        Ok(call.try_as_basic_value().left().unwrap())
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
