use crate::semantics::SymbolKind;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue,
    PointerValue,
};
use inkwell::AddressSpace;
use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::*;
use crate::semantics::{GenericContext, MethodSig, SemanticAnalyzer, Type, Type as ResolvedType};

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
}

impl<'a> CodeGenerator<'a> {
    pub fn new(context: &'a Context, analyzer: &'a mut SemanticAnalyzer) -> Self {
        let module = context.create_module("mux_module");
        let builder = context.create_builder();

        // declare runtime functions
        let void_type = context.void_type();
        let i64_type = context.i64_type();
        let f64_type = context.f64_type();
        let i8_ptr = context.ptr_type(AddressSpace::default());
        let list_ptr = i8_ptr; // placeholder for *mut List
        let map_ptr = i8_ptr; // placeholder for *mut Map
        let set_ptr = i8_ptr; // placeholder for *mut Set

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

        // malloc: (i64) -> *mut i8
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("malloc", fn_type, None);

        // mux_string_concat: (*const c_char, *const c_char) -> *mut c_char
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_concat", fn_type, None);

        // mux_string_equal: (*const c_char, *const c_char) -> i32
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_string_equal", fn_type, None);

        // mux_string_not_equal: (*const c_char, *const c_char) -> i32
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_string_not_equal", fn_type, None);

        // mux_value_equal: (*const Value, *const Value) -> i32
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_value_equal", fn_type, None);

        // mux_value_not_equal: (*const Value, *const Value) -> i32
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_value_not_equal", fn_type, None);

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

        // mux_bool_to_string: (i32) -> *const c_char
        let params = &[context.i32_type().into()];
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

        // mux_map_to_string: (*mut Map) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_map_to_string", fn_type, None);

        // object management functions
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

        // mux_value_get_map: (*mut Value) -> *mut Map
        let fn_type = map_ptr.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_map", fn_type, None);

        // mux_value_get_set: (*mut Value) -> *mut Set
        let fn_type = set_ptr.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_set", fn_type, None);

        // mux_list_concat: (*const List, *const List) -> *mut List
        let fn_type = list_ptr.fn_type(&[list_ptr.into(), list_ptr.into()], false);
        module.add_function("mux_list_concat", fn_type, None);

        // mux_map_merge: (*const Map, *const Map) -> *mut Map
        let fn_type = map_ptr.fn_type(&[map_ptr.into(), map_ptr.into()], false);
        module.add_function("mux_map_merge", fn_type, None);

        // mux_set_union: (*const Set, *const Set) -> *mut Set
        let fn_type = set_ptr.fn_type(&[set_ptr.into(), set_ptr.into()], false);
        module.add_function("mux_set_union", fn_type, None);

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

        // collection constructors
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

        // safe value extraction functions
        // mux_value_get_int: (*const Value) -> i64
        let fn_type = i64_type.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_int", fn_type, None);

        // mux_value_get_float: (*const Value) -> f64
        let fn_type = context.f64_type().fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_float", fn_type, None);

        // mux_value_get_bool: (*const Value) -> i32
        let fn_type = context.i32_type().fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_bool", fn_type, None);

        // mux_value_get_type_tag: (*const Value) -> i32
        let fn_type = context.i32_type().fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_value_get_type_tag", fn_type, None);

        // mux_free_value: (*mut Value) -> ()
        let fn_type = void_type.fn_type(&[i8_ptr.into()], false);
        module.add_function("mux_free_value", fn_type, None);

        // list operations
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

        // mux_list_push: (*mut List, *mut Value) -> ()
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push", fn_type, None);

        // mux_list_pop: (*mut List) -> *mut Optional
        let params = &[list_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_pop", fn_type, None);

        // mux_list_push_front: (*mut List, *mut Value) -> ()
        let params = &[list_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push_front", fn_type, None);

        // mux_list_pop_front: (*mut List) -> *mut Optional
        let params = &[list_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_pop_front", fn_type, None);

        // mux_list_push_back_value: (*mut Value, *mut Value) -> ()
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push_back_value", fn_type, None);

        // mux_list_push_value: (*mut Value, *mut Value) -> ()
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push_value", fn_type, None);

        // mux_list_push_front_value: (*mut Value, *mut Value) -> ()
        let params = &[i8_ptr.into(), i8_ptr.into()];
        let fn_type = void_type.fn_type(params, false);
        module.add_function("mux_list_push_front_value", fn_type, None);

        // mux_list_pop_back_value: (*mut Value) -> *mut Optional
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_pop_back_value", fn_type, None);

        // mux_list_pop_value: (*mut Value) -> *mut Optional
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_pop_value", fn_type, None);

        // mux_list_pop_front_value: (*mut Value) -> *mut Optional
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_list_pop_front_value", fn_type, None);

        // mux_list_is_empty: (*const List) -> bool
        let params = &[list_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_list_is_empty", fn_type, None);

        // map operations
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

        // set operations
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

        // additional map operations
        // mux_map_size: (*const Map) -> i64
        let params = &[list_ptr.into()];
        let fn_type = i64_type.fn_type(params, false);
        module.add_function("mux_map_size", fn_type, None);

        // mux_map_is_empty: (*const Map) -> bool
        let params = &[list_ptr.into()];
        let fn_type = context.bool_type().fn_type(params, false);
        module.add_function("mux_map_is_empty", fn_type, None);

        // value creation functions
        // mux_int_value: (i64) -> *mut Value
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_int_value", fn_type, None);

        // mux_float_value: (f64) -> *mut Value
        let params = &[context.f64_type().into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_float_value", fn_type, None);

        // mux_bool_value: (i32) -> *mut Value
        let params = &[context.i32_type().into()];
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

        // mux_bool_from_value: (*mut Value) -> i32
        let params = &[i8_ptr.into()];
        let fn_type = context.i32_type().fn_type(params, false);
        module.add_function("mux_bool_from_value", fn_type, None);

        // mux_string_from_value: (*mut Value) -> *const c_char
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_string_from_value", fn_type, None);

        // result constructors
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

        // mux_optional_some_float: (f64) -> *mut Optional
        let params = &[f64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_some_float", fn_type, None);

        // mux_optional_some_bool: (i32) -> *mut Optional
        let params = &[context.i32_type().into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_some_bool", fn_type, None);

        // mux_optional_some_char: (i64) -> *mut Optional
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_some_char", fn_type, None);

        // mux_optional_some_string: (*mut Value) -> *mut Optional
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_some_string", fn_type, None);

        // mux_optional_some_value: (*mut Value) -> *mut Optional
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_optional_some_value", fn_type, None);

        // mux_optional_none: () -> *mut Optional
        let fn_type = i8_ptr.fn_type(&[], false);
        module.add_function("mux_optional_none", fn_type, None);

        // mux_result_ok_int: (i64) -> *mut MuxResult
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_int", fn_type, None);

        // mux_result_ok_float: (f64) -> *mut MuxResult
        let params = &[f64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_float", fn_type, None);

        // mux_result_ok_bool: (i32) -> *mut MuxResult
        let params = &[context.i32_type().into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_bool", fn_type, None);

        // mux_result_ok_char: (i64) -> *mut MuxResult
        let params = &[i64_type.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_char", fn_type, None);

        // mux_result_ok_string: (*mut Value) -> *mut MuxResult
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_string", fn_type, None);

        // mux_result_ok_value: (*mut Value) -> *mut MuxResult
        let params = &[i8_ptr.into()];
        let fn_type = i8_ptr.fn_type(params, false);
        module.add_function("mux_result_ok_value", fn_type, None);

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

        // built-in enum types
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

        // generate types for user-defined enums
        for (name, symbol) in analyzer.all_symbols() {
            if symbol.kind == crate::semantics::SymbolKind::Enum {
                // TODO: why is this hardcoded
                // assume all data are f64, max 2 fields for simplicity
                let i32_type = context.i32_type();
                let f64_type = context.f64_type();
                let struct_type = context
                    .struct_type(&[i32_type.into(), f64_type.into(), f64_type.into()], false);
                type_map.insert(name.clone(), struct_type.into());

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

        let entry = function.get_first_basic_block().unwrap();
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

    fn generate_user_defined_types(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        // generate LLVM types for classes, interfaces, enums
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

        // Collect type parameter names for this class from the symbol table
        let type_param_names: std::collections::HashSet<String> =
            if let Some(class_symbol) = self.analyzer.all_symbols().get(name) {
                class_symbol
                    .type_params
                    .iter()
                    .map(|(param_name, _)| param_name.clone())
                    .collect()
            } else {
                std::collections::HashSet::new()
            };

        // add vtable fields for implemented interfaces FIRST
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        for interface_name in interfaces.keys() {
            field_types.push(ptr_type.into());
            field_indices.insert(format!("vtable_{}", interface_name), field_types.len() - 1);
        }

        // add class fields after
        for field in fields {
            let field_type = if let TypeNode {
                kind: TypeKind::Named(field_type_name, _),
                ..
            } = &field.type_
            {
                // Check if this field type is a type parameter of the class
                if type_param_names.contains(field_type_name) {
                    // generic fields should be pointers (boxed values)
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    // for primitive fields, use pointer type to be consistent with boxing
                    if matches!(field.type_.kind, TypeKind::Primitive(_)) {
                        self.context.ptr_type(AddressSpace::default()).into()
                    } else {
                        self.llvm_type_from_mux_type(&field.type_)?
                    }
                }
            } else {
                // for primitive fields, use pointer type to be consistent with boxing
                if matches!(field.type_.kind, TypeKind::Primitive(_)) {
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    self.llvm_type_from_mux_type(&field.type_)?
                }
            };
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
        for (interface_name, interface_methods) in interfaces {
            let mut vtable_values = Vec::new();
            for method_name in interface_methods.keys() {
                let class_method_name = format!("{}.{}", class_name, method_name);
                let func = self.functions.get(&class_method_name).ok_or_else(|| {
                    format!(
                        "Class {} does not implement method {} for interface {}",
                        class_name, method_name, interface_name
                    )
                })?;
                vtable_values.push(func.as_global_value().as_basic_value_enum());
            }
            // get vtable struct type
            let vtable_type = self.vtable_type_map.get(interface_name).unwrap();
            let vtable_const = vtable_type.const_named_struct(&vtable_values);
            // create global
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
        // generate LLVM struct for interface: { *mut vtable }
        // for simplicity, vtable is struct of void* function pointers
        let symbol = self.analyzer.all_symbols().get(name).unwrap();
        let interface_methods = symbol.interfaces.get(name).unwrap();

        // create vtable as struct of function pointers (all (void*) -> void* for now)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let fn_ptr_type = ptr_type; // since fn_type.ptr_type deprecated, use ptr_type

        let vtable_types = vec![fn_ptr_type.into(); interface_methods.len()];

        // vtable type: struct of function pointers
        let vtable_struct_type = self.context.struct_type(&vtable_types, false);
        self.vtable_type_map
            .insert(name.to_string(), vtable_struct_type);
        let vtable_ptr_type = self.context.ptr_type(AddressSpace::default());

        // interface struct: { vtable_ptr }
        let interface_struct_type = self.context.struct_type(&[vtable_ptr_type.into()], false);
        self.type_map
            .insert(name.to_string(), interface_struct_type.into());

        Ok(())
    }

    fn generate_enum_type(&mut self, name: &str, variants: &[EnumVariant]) -> Result<(), String> {
        // tagged union: {i32 discriminant, f64 data0, f64 data1} for now
        // unused field definition removed
        let i32_type = self.context.i32_type();
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

        // create struct type with discriminant + actual field types from variants
        let mut struct_fields = vec![i32_type.into()]; // discriminant first
        let union_field_types = self.get_enum_union_field_types(name);
        struct_fields.extend(union_field_types);
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

            // generate the body
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            // build the struct by storing to temp and loading
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
                self.builder
                    .build_store(data_ptr, arg)
                    .map_err(|e| e.to_string())?;
            }
            let struct_val = self
                .builder
                .build_load(struct_type, temp_ptr, "struct")
                .map_err(|e| e.to_string())?;
            // return the struct
            self.builder
                .build_return(Some(&struct_val))
                .map_err(|e| e.to_string())?;

            // store in constructors
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
        let full_name = format!("{}.new", name);

        // Constructor takes no parameters - fields are initialized separately
        let param_types = vec![];

        // return type: *mut Value (boxed object)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let fn_type = ptr_type.fn_type(&param_types, false);
        let function = self.module.add_function(&full_name, fn_type, None);

        // generate the body
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // register the object type if not already registered
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

        // allocate the object
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

        // get the object data pointer
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

        // cast to class struct pointer
        let class_type = self.type_map.get(name).ok_or("Class type not found")?;
        let class_type_clone = *class_type; // clone to avoid borrow issues
        let struct_ptr_typed = self
            .builder
            .build_pointer_cast(
                struct_ptr,
                self.context.ptr_type(AddressSpace::default()),
                "struct_ptr",
            )
            .map_err(|e| e.to_string())?;

        // set fields to default (zero) values
        for field in fields.iter() {
            let field_index = self.field_map.get(name).unwrap().get(&field.name).unwrap();
            let field_ptr = self
                .builder
                .build_struct_gep(
                    class_type_clone,
                    struct_ptr_typed,
                    *field_index as u32,
                    &field.name,
                )
                .map_err(|e| e.to_string())?;

            // Initialize primitive fields with default boxed values
            let default_value: BasicValueEnum =
                if matches!(field.type_.kind, TypeKind::Primitive(_)) {
                    // Create default value based on type
                    let llvm_type = self.llvm_type_from_mux_type(&field.type_)?;
                    let zero_val = if llvm_type.is_int_type() {
                        llvm_type.into_int_type().const_zero().into()
                    } else if llvm_type.is_float_type() {
                        llvm_type.into_float_type().const_zero().into()
                    } else {
                        // For other types, use null pointer
                        self.context
                            .ptr_type(AddressSpace::default())
                            .const_zero()
                            .into()
                    };
                    // Box the zero value
                    self.box_value(zero_val).into()
                } else {
                    // Non-primitive fields: initialize to null
                    self.context
                        .ptr_type(AddressSpace::default())
                        .const_zero()
                        .into()
                };

            self.builder
                .build_store(field_ptr, default_value)
                .map_err(|e| e.to_string())?;
        }

        // set vtable fields
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
                    class_type_clone,
                    struct_ptr_typed,
                    *field_index as u32,
                    &vtable_field_name,
                )
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(field_ptr, *vtable_ptr)
                .map_err(|e| e.to_string())?;
        }

        // return the Value pointer
        self.builder
            .build_return(Some(&obj_value_ptr))
            .map_err(|e| e.to_string())?;

        // store in constructors
        self.constructors.insert(format!("{}.new", name), function);
        Ok(())
    }

    fn get_variant_index(&self, enum_name: &str, variant_name: &str) -> Result<usize, String> {
        // hardcode indices for built-in enums to ensure deterministic behavior
        match (enum_name, variant_name) {
            ("Optional", "Some") => Ok(0),
            ("Optional", "None") => Ok(1),
            ("Result", "Ok") => Ok(0),
            ("Result", "Err") => Ok(1),
            _ => {
                // for user-defined enums, use HashMap lookup
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

    /// load the discriminant from an enum value as an i32
    /// this function centralizes discriminant loading logic and ensures type safety
    fn load_enum_discriminant(
        &self,
        enum_name: &str,
        enum_value: BasicValueEnum<'a>,
    ) -> Result<IntValue<'a>, String> {
        match enum_name {
            "Optional" | "Result" => {
                // for built-in enums, use runtime functions
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
                    .build_call(func, &[enum_value.into()], "discriminant_call")
                    .map_err(|e| e.to_string())?;

                Ok(discriminant_call
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value())
            }
            _ => {
                // for user-defined enums, load discriminant field directly
                let struct_type = self
                    .type_map
                    .get(enum_name)
                    .ok_or_else(|| format!("Enum {} not found in type map", enum_name))?
                    .into_struct_type();

                // allocate temporary storage for the enum value
                let temp_ptr = self
                    .builder
                    .build_alloca(struct_type, "temp_enum")
                    .map_err(|e| e.to_string())?;

                // store the enum value
                self.builder
                    .build_store(temp_ptr, enum_value)
                    .map_err(|e| e.to_string())?;

                // get pointer to discriminant field (index 0)
                let discriminant_ptr = self
                    .builder
                    .build_struct_gep(struct_type, temp_ptr, 0, "discriminant_ptr")
                    .map_err(|e| e.to_string())?;

                // load discriminant as i32
                let discriminant = self
                    .builder
                    .build_load(self.context.i32_type(), discriminant_ptr, "discriminant")
                    .map_err(|e| e.to_string())?
                    .into_int_value();

                Ok(discriminant)
            }
        }
    }

    /// create a type-safe comparison between discriminant and variant index
    /// this ensures both operands are i32 values and returns a boolean for branching
    fn build_discriminant_comparison(
        &self,
        discriminant: IntValue<'a>,
        variant_index: usize,
    ) -> Result<IntValue<'a>, String> {
        let index_val = self
            .context
            .i32_type()
            .const_int(variant_index as u64, false);

        let result = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                discriminant,
                index_val,
                "match_cmp",
            )
            .map_err(|e| e.to_string())?;

        Ok(result)
    }

    /// determine the union field types for an enum based on its variants
    /// this replaces the hardcoded f64 assumption with actual field types
    fn get_enum_union_field_types(&self, enum_name: &str) -> Vec<BasicTypeEnum<'a>> {
        let mut union_types = Vec::new();

        if let Some(variant_fields) = self.enum_variant_fields.get(enum_name) {
            // find the maximum number of fields across all variants
            let max_fields = variant_fields
                .values()
                .map(|fields| fields.len())
                .max()
                .unwrap_or(0);

            // for each field position, determine the appropriate union type
            for field_idx in 0..max_fields {
                let mut field_types = Vec::new();

                // collect all types used in this field position across variants
                for field_list in variant_fields.values() {
                    if field_idx < field_list.len() {
                        field_types.push(&field_list[field_idx]);
                    }
                }

                // determine the union type for this field position
                let union_type = self.determine_union_field_type(&field_types);
                union_types.push(union_type);
            }
        }

        union_types
    }

    /// determine the appropriate LLVM type for a union field position
    /// for now, use the largest common type or pointer for complex types
    fn determine_union_field_type(&self, field_types: &[&TypeNode]) -> BasicTypeEnum<'a> {
        if field_types.is_empty() {
            // no fields in this position, use i32 as default
            return self.context.i32_type().into();
        }

        // for simplicity, check if all types are the same
        let first_type = field_types[0];
        let all_same = field_types.iter().all(|t| t.kind == first_type.kind);

        if all_same {
            // all variants use the same type for this field
            // use the same types as llvm_type_from_mux_type for consistency
            match &first_type.kind {
                TypeKind::Primitive(PrimitiveType::Int) => self.context.i64_type().into(),
                TypeKind::Primitive(PrimitiveType::Float) => self.context.f64_type().into(),
                TypeKind::Primitive(PrimitiveType::Bool) => self.context.bool_type().into(),
                TypeKind::Primitive(PrimitiveType::Str) => {
                    self.context.ptr_type(AddressSpace::default()).into()
                }
                _ => self.context.ptr_type(AddressSpace::default()).into(), // default to pointer
            }
        } else {
            // mixed types - use pointer for now (could be improved with proper union types)
            self.context.ptr_type(AddressSpace::default()).into()
        }
    }

    fn generate_if_expression(
        &mut self,
        cond: &ExpressionNode,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let cond_val = self.generate_expression(cond)?;

        // get current function
        let current_bb = self.builder.get_insert_block().unwrap();
        let function = current_bb.get_parent().unwrap();

        // create blocks
        let then_bb = self.context.append_basic_block(function, "if_then");
        let else_bb = self.context.append_basic_block(function, "if_else");
        let merge_bb = self.context.append_basic_block(function, "if_merge");

        // conditional branch
        self.builder
            .build_conditional_branch(cond_val.into_int_value(), then_bb, else_bb)
            .map_err(|e| e.to_string())?;

        // then block
        self.builder.position_at_end(then_bb);
        let then_val = self.generate_expression(then_expr)?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let then_bb_end = self.builder.get_insert_block().unwrap();

        // else block
        self.builder.position_at_end(else_bb);
        let else_val = self.generate_expression(else_expr)?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let else_bb_end = self.builder.get_insert_block().unwrap();

        // merge with phi
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
        // save current insert block
        let old_bb = self.builder.get_insert_block();

        // generate unique function name
        let func_name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;

        // set current function name for proper scoping
        let old_function_name = self.current_function_name.take();
        self.current_function_name = Some(func_name.clone());

        // convert params to LLVM types
        let mut param_types = Vec::new();
        for param in params {
            let param_type = self.llvm_type_from_mux_type(&param.type_)?;
            param_types.push(param_type.into());
        }

        // determine return type from body
        let return_type_opt: Option<BasicTypeEnum<'a>> = if let Some(StatementNode {
            kind: StatementKind::Return(Some(expr)),
            ..
        }) = body.last()
        {
            // get the return type from the actual return expression
            let return_type = self
                .analyzer
                .get_expression_type(expr)
                .map_err(|e| e.to_string())?;
            // check if return type is void
            if matches!(return_type, Type::Void) {
                None
            } else {
                Some(self.llvm_type_from_resolved_type(&return_type)?)
            }
        } else if let Some(StatementNode {
            kind: StatementKind::Expression(expr),
            ..
        }) = body.last()
        {
            // if the last statement is an expression, use its type as return type
            let return_type = self
                .analyzer
                .get_expression_type(expr)
                .map_err(|e| e.to_string())?;
            // check if return type is void
            if matches!(return_type, Type::Void) {
                None
            } else {
                Some(self.llvm_type_from_resolved_type(&return_type)?)
            }
        } else {
            None
        };

        // set current function return type for proper return handling
        let resolved_return_type = if let Some(rt) = return_type_opt {
            match rt {
                BasicTypeEnum::IntType(_) => Some(ResolvedType::Primitive(PrimitiveType::Int)),
                BasicTypeEnum::FloatType(_) => Some(ResolvedType::Primitive(PrimitiveType::Float)),
                _ => None,
            }
        } else {
            // when return_type_opt is None, it's a void return
            Some(ResolvedType::Void)
        };
        let old_return_type = self.current_function_return_type.take();
        self.current_function_return_type = resolved_return_type;
        let fn_type = if let Some(rt) = return_type_opt {
            rt.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        // create the function
        let function = self.module.add_function(&func_name, fn_type, None);

        // set parameter names
        for (i, param) in params.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            arg.set_name(&param.name);
        }

        // create entry block and set up parameters
        let entry_bb = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_bb);

        // save current variables and create new scope
        let old_variables = self.variables.clone();
        self.variables.clear();

        // set up parameter variables
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

        // generate all statements
        for stmt in body {
            self.generate_statement(stmt, Some(&function))?;
        }

        // if void return, add return void if not already terminated
        if return_type_opt.is_none() {
            if let Some(block) = self.builder.get_insert_block() {
                if block.get_terminator().is_none() {
                    self.builder.build_return(None).map_err(|e| e.to_string())?;
                }
            }
        }

        // restore variables
        self.variables = old_variables;

        // restore return type
        self.current_function_return_type = old_return_type;

        // restore function name
        self.current_function_name = old_function_name;

        // restore builder to previous block
        if let Some(bb) = old_bb {
            self.builder.position_at_end(bb);
        }

        // return function pointer
        Ok(function.as_global_value().as_pointer_value().into())
    }

    /// check if a method's parameters or return type reference any of the given type parameters
    fn method_uses_type_params(method: &FunctionNode, type_param_names: &[&str]) -> bool {
        // check parameter types
        for param in &method.params {
            if Self::type_node_contains_names(&param.type_, type_param_names) {
                return true;
            }
        }
        // check return type
        if Self::type_node_contains_names(&method.return_type, type_param_names) {
            return true;
        }
        false
    }

    /// check if a TypeNode contains any of the given names (for generic type parameters)
    fn type_node_contains_names(type_node: &TypeNode, names: &[&str]) -> bool {
        match &type_node.kind {
            TypeKind::Named(n, args) => {
                if names.contains(&n.as_str()) {
                    return true;
                }
                for arg in args {
                    if Self::type_node_contains_names(arg, names) {
                        return true;
                    }
                }
                false
            }
            TypeKind::List(inner) => Self::type_node_contains_names(inner, names),
            TypeKind::Map(k, v) => {
                Self::type_node_contains_names(k, names) || Self::type_node_contains_names(v, names)
            }
            TypeKind::Set(inner) => Self::type_node_contains_names(inner, names),
            _ => false,
        }
    }

    pub fn generate(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        // zero pass: generate LLVM types for user-defined types
        self.generate_user_defined_types(nodes)?;

        // first pass: declare all non-generic functions
        for node in nodes {
            if let AstNode::Function(func) = node {
                // store function nodes for both generic and non-generic functions
                self.function_nodes.insert(func.name.clone(), func.clone());

                // only declare non-generic functions in first pass
                // generic functions will be declared when instantiated
                if func.type_params.is_empty() {
                    self.declare_function(func)?;
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
                let symbol = self.analyzer.all_symbols().get(name).unwrap();
                let interfaces = symbol.interfaces.clone();
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
                    let symbol = self.analyzer.all_symbols().get(name).unwrap();
                    let interfaces = symbol.interfaces.clone();
                    self.generate_class_constructors(name, fields, &interfaces)?;
                }
                _ => {}
            }
        }

        // second pass: collect top-level statements and function nodes
        let mut top_level_statements = vec![];
        let mut user_functions = vec![];
        for node in nodes {
            match node {
                AstNode::Function(func) => {
                    // collect non-generic functions for later generation
                    // generic functions will be generated when instantiated
                    if func.type_params.is_empty() {
                        user_functions.push(func.clone());
                    }
                }
                AstNode::Statement(stmt) => {
                    top_level_statements.push(stmt.clone());
                }
                _ => {} // skip classes, interfaces, enums for now
            }
        }

        // First, analyze top-level statements to identify global variable declarations
        // and create LLVM global variables for them
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
                StatementKind::AutoDecl(name, _, _) => {
                    // for auto declarations, get the inferred type from the symbol table
                    let symbol = self
                        .analyzer
                        .symbol_table()
                        .lookup(name)
                        .ok_or_else(|| format!("Symbol {} not found", name))?;
                    let resolved_type = symbol
                        .type_
                        .as_ref()
                        .ok_or_else(|| format!("Type not inferred for {}", name))?;

                    // determine correct LLVM type based on whether value is boxed
                    let llvm_type = match resolved_type {
                        Type::Primitive(_) => {
                            // primitives are boxed, use ptr type
                            self.context.ptr_type(AddressSpace::default()).into()
                        }
                        _ => {
                            // enums, classes, etc. use their actual struct type
                            let type_node = self.type_to_type_node(resolved_type);
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

        // generate main function to initialize global variables
        if !top_level_statements.is_empty() {
            let main_type = self.context.void_type().fn_type(&[], false);
            let main_func = self.module.add_function("main", main_type, None);
            let entry = self.context.append_basic_block(main_func, "entry");
            self.builder.position_at_end(entry);

            // copy global_variables to variables so statements can access/initialize them
            self.variables = self.global_variables.clone();

            for stmt in top_level_statements {
                self.generate_statement(&stmt, Some(&main_func))?;
            }
            self.builder.build_return(None).map_err(|e| e.to_string())?;
        }

        // now generate user-defined functions (they can access globals)
        for func in user_functions {
            self.generate_function(&func)?;
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
            }
        }

        Ok(())
    }

    fn declare_function(&mut self, func: &FunctionNode) -> Result<(), String> {
        let mut param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type_from_mux_type(&p.type_).map(|t| t.into()))
            .collect::<Result<_, _>>()?;

        // for class methods, add implicit 'self' parameter (unless static)
        let is_class_method = func.name.contains('.');
        if is_class_method && !func.is_common {
            param_types.insert(0, self.context.ptr_type(AddressSpace::default()).into());
        }

        // for specialized methods (name contains $), wrap all parameters in pointers,
        // only for instance methods, not static methods
        // static methods should use concrete types after specialization
        let is_specialized = func.name.contains('$');
        let is_static = func.is_common;
        if is_specialized && !is_static {
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            param_types = param_types
                .into_iter()
                .enumerate()
                .map(|(i, param_type)| {
                    // skip self parameter (index 0)
                    if i == 0 && is_class_method && !func.is_common {
                        param_type
                    } else {
                        // wrap non-self parameters in pointers
                        ptr_type.into()
                    }
                })
                .collect();
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
        self.function_nodes.insert(func.name.clone(), func.clone());

        Ok(())
    }

    fn generate_function(&mut self, func: &FunctionNode) -> Result<(), String> {
        self.current_function_name = Some(func.name.clone());
        self.current_function_return_type = Some(
            self.analyzer
                .resolve_type(&func.return_type)
                .map_err(|e| e.to_string())?,
        );

        let function = *self
            .functions
            .get(&func.name)
            .ok_or("Function not declared")?;

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // clear variables for new scope
        self.variables.clear();

        // set up parameter variables
        let is_class_method = func.name.contains('.');
        let mut param_index = 0;
        if is_class_method && !func.is_common {
            let class_name = func
                .name
                .split('.')
                .next()
                .or_else(|| {
                    // handle specialized method names like Box$int.to_string
                    func.name.split('$').next()
                })
                .unwrap();
            // for specialized methods like Box$int.to_string, we need just "Box"
            let base_class_name = class_name.split('$').next().unwrap_or(class_name);
            let class_type = self.type_map.get(base_class_name).unwrap();
            let arg = function.get_nth_param(param_index).unwrap();
            param_index += 1;
            // set self as variable
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let alloca = self
                .builder
                .build_alloca(ptr_type, "self")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, arg)
                .map_err(|e| e.to_string())?;
            let self_type = Type::Named(base_class_name.to_string(), vec![]);
            self.variables
                .insert("self".to_string(), (alloca, *class_type, self_type.clone()));
            // also set current_self_type in the analyzer for type checking
            self.analyzer.current_self_type = Some(self_type);
        }

        for (i, param) in func.params.iter().enumerate() {
            let arg = function.get_nth_param((i as u32) + param_index).unwrap();

            // resolve parameter type first
            let resolved_type = self
                .analyzer
                .resolve_type(&param.type_)
                .map_err(|e| e.to_string())?;

            // handle different parameter types appropriately
            let value_to_store = if matches!(resolved_type, Type::Reference(_)) {
                // for reference parameters, store pointer directly
                arg.into_pointer_value()
            } else {
                // check if this is an enum type
                let is_enum = matches!(&resolved_type, Type::Named(type_name, _) if self
                    .analyzer
                    .symbol_table()
                    .lookup(type_name)
                    .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                    .unwrap_or(false));

                if is_enum {
                    // for enum types, store struct value directly
                    let struct_type = arg.get_type();
                    let alloca = self
                        .builder
                        .build_alloca(struct_type, &param.name)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(alloca, arg)
                        .map_err(|e| e.to_string())?;
                    // for enums, store the struct type directly
                    self.variables
                        .insert(param.name.clone(), (alloca, struct_type, resolved_type));
                    continue; // skip the normal pointer wrapping
                } else if matches!(resolved_type, Type::Function { .. }) {
                    // for function type parameters, store raw function pointer directly
                    let func_ptr_type = self.context.ptr_type(AddressSpace::default());
                    let alloca = self
                        .builder
                        .build_alloca(func_ptr_type, &param.name)
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(alloca, arg)
                        .map_err(|e| e.to_string())?;

                    self.variables.insert(
                        param.name.clone(),
                        (alloca, func_ptr_type.into(), resolved_type),
                    );
                    continue; // skip the normal pointer wrapping
                } else {
                    // for class and primitive types, box the value
                    self.box_value(arg)
                }
            };

            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let alloca = self
                .builder
                .build_alloca(ptr_type, &param.name)
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, value_to_store)
                .map_err(|e| e.to_string())?;

            self.variables.insert(
                param.name.clone(),
                (alloca, BasicTypeEnum::PointerType(ptr_type), resolved_type),
            );
        }

        // generate function body
        for stmt in &func.body {
            self.generate_statement(stmt, Some(&function))?;
        }

        // if void return, add return void if not already terminated
        if matches!(
            func.return_type.kind,
            TypeKind::Primitive(PrimitiveType::Void)
        ) {
            if let Some(block) = self.builder.get_insert_block() {
                if block.get_terminator().is_none() {
                    self.builder.build_return(None).map_err(|e| e.to_string())?;
                }
            }
        }

        // clear current_self_type
        self.analyzer.current_self_type = None;

        Ok(())
    }

    fn generate_expression(&mut self, expr: &ExpressionNode) -> Result<BasicValueEnum<'a>, String> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => self.generate_literal(lit),
            ExpressionKind::None => {
                let none_call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_optional_none").unwrap(),
                        &[],
                        "none_literal",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                // Return Optional pointer directly (not wrapped in Value)
                Ok(none_call)
            }
            ExpressionKind::Identifier(name) => {
                if let Some((ptr, var_type, type_node)) = self
                    .variables
                    .get(name)
                    .or_else(|| self.global_variables.get(name))
                {
                    match type_node {
                        Type::Named(type_name, _) => {
                            if type_name == "Optional" || type_name == "Result" {
                                // optional/Result: load pointer to boxed value
                                let ptr_to_boxed = self
                                    .builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        *ptr,
                                        &format!("load_{}", name),
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into_pointer_value();
                                Ok(ptr_to_boxed.into())
                            } else if self
                                .analyzer
                                .symbol_table()
                                .lookup(type_name)
                                .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                                .unwrap_or(false)
                            {
                                // custom enums: load as struct value directly from alloca
                                if let BasicTypeEnum::StructType(st) = *var_type {
                                    let struct_val = self
                                        .builder
                                        .build_load(st, *ptr, &format!("load_{}", name))
                                        .map_err(|e| e.to_string())?;
                                    Ok(struct_val)
                                } else {
                                    Err(format!("Expected struct type for enum variable {}", name))
                                }
                            } else {
                                // not Optional/Result or enum - treat as boxed value
                                let ptr_to_boxed = self
                                    .builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        *ptr,
                                        &format!("load_{}", name),
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into_pointer_value();
                                Ok(ptr_to_boxed.into())
                            }
                        }
                        Type::Primitive(prim) => {
                            // for primitives, load the boxed pointer first
                            let ptr_to_boxed = self
                                .builder
                                .build_load(
                                    self.context.ptr_type(AddressSpace::default()),
                                    *ptr,
                                    &format!("load_{}", name),
                                )
                                .map_err(|e| e.to_string())?
                                .into_pointer_value();
                            match prim {
                                PrimitiveType::Int => {
                                    let raw_int = self.get_raw_int_value(ptr_to_boxed.into())?;
                                    Ok(raw_int.into())
                                }
                                PrimitiveType::Float => {
                                    let raw_float =
                                        self.get_raw_float_value(ptr_to_boxed.into())?;
                                    Ok(raw_float.into())
                                }
                                PrimitiveType::Bool => {
                                    let raw_bool = self.get_raw_bool_value(ptr_to_boxed.into())?;
                                    Ok(raw_bool.into())
                                }
                                PrimitiveType::Str => Ok(ptr_to_boxed.into()),
                                PrimitiveType::Char | PrimitiveType::Void | PrimitiveType::Auto => {
                                    Err(format!("Unsupported primitive type {:?}", prim))
                                }
                            }
                        }
                        Type::Function { .. } => {
                            // for function types, load and return raw function pointer
                            let func_ptr = self
                                .builder
                                .build_load(
                                    self.context.ptr_type(AddressSpace::default()),
                                    *ptr,
                                    &format!("load_{}", name),
                                )
                                .map_err(|e| e.to_string())?
                                .into_pointer_value();
                            Ok(func_ptr.into())
                        }
                        _ => {
                            // boxed types
                            let ptr_to_boxed = self
                                .builder
                                .build_load(
                                    self.context.ptr_type(AddressSpace::default()),
                                    *ptr,
                                    &format!("load_{}", name),
                                )
                                .map_err(|e| e.to_string())?
                                .into_pointer_value();
                            Ok(ptr_to_boxed.into())
                        }
                    }
                } else if self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                    .unwrap_or(false)
                {
                    Err(format!("Enums cannot be used as values: {}", name))
                } else {
                    // check if class method field access
                    if let Some(ref func_name) = self.current_function_name {
                        if func_name.contains('.') {
                            let class_name = func_name.split('.').next().unwrap();
                            if let Some((self_ptr, _, _)) = self
                                .variables
                                .get("self")
                                .or_else(|| self.global_variables.get("self"))
                            {
                                if self
                                    .field_map
                                    .get(class_name)
                                    .and_then(|fields| fields.get(name))
                                    .is_some()
                                {
                                    // extract the actual enum value from the object field
                                    // self_ptr is an alloca containing the object data pointer, so load it first
                                    let object_data_ptr_val = self
                                        .builder
                                        .build_load(
                                            self.context.ptr_type(AddressSpace::default()),
                                            *self_ptr,
                                            "object_data_ptr",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    let object_data_ptr = object_data_ptr_val.into_pointer_value();

                                    // cast to the class struct type (GenericShape)
                                    let class_type = self
                                        .type_map
                                        .get(class_name)
                                        .ok_or("Class type not found")?;
                                    let struct_ptr_typed = self
                                        .builder
                                        .build_pointer_cast(
                                            object_data_ptr,
                                            self.context.ptr_type(AddressSpace::default()),
                                            "struct_ptr_typed",
                                        )
                                        .map_err(|e| e.to_string())?;

                                    // get the correct field index from field_map
                                    let field_indices =
                                        self.field_map.get(class_name).ok_or_else(|| {
                                            format!("Field map not found for class {}", class_name)
                                        })?;
                                    let field_index = field_indices.get(name).ok_or_else(|| {
                                        format!("Field {} not found in class {}", name, class_name)
                                    })?;

                                    let field_ptr = self
                                        .builder
                                        .build_struct_gep(
                                            *class_type,
                                            struct_ptr_typed,
                                            *field_index as u32,
                                            "field_ptr",
                                        )
                                        .map_err(|e| e.to_string())?;

                                    // get the field type and load the enum value
                                    let class_fields =
                                        self.classes.get(class_name).ok_or("Class not found")?;
                                    let field = class_fields
                                        .iter()
                                        .find(|f| f.name == *name)
                                        .ok_or("Field not found")?;
                                    let field_type = self.llvm_type_from_mux_type(&field.type_)?;
                                    // load the actual enum value from the object field
                                    let enum_val = self
                                        .builder
                                        .build_load(field_type, field_ptr, "field_enum")
                                        .map_err(|e| e.to_string())?;
                                    return Ok(enum_val);
                                }
                            }
                        }
                    }
                    // check if we're in a method and this is a field access
                    if let Some(ref func_name) = self.current_function_name {
                        if func_name.contains('.') {
                            let class_name = func_name.split('.').next().unwrap();
                            if self.classes.contains_key(class_name) {
                                if let Some(field_index) = self
                                    .field_map
                                    .get(class_name)
                                    .and_then(|fields| fields.get(name))
                                {
                                    // this is a field access on self
                                    if let Some((self_ptr, _, _)) = self
                                        .variables
                                        .get("self")
                                        .or_else(|| self.global_variables.get("self"))
                                    {
                                        // load the object pointer from the alloca
                                        let self_value_ptr = self
                                            .builder
                                            .build_load(
                                                self.context.ptr_type(AddressSpace::default()),
                                                *self_ptr,
                                                "load_self_for_field_access",
                                            )
                                            .map_err(|e| e.to_string())?
                                            .into_pointer_value();

                                        // get the raw data pointer from the boxed Value
                                        let get_ptr_func = self
                                            .module
                                            .get_function("mux_get_object_ptr")
                                            .ok_or("mux_get_object_ptr not found")?;
                                        let data_ptr = self
                                            .builder
                                            .build_call(
                                                get_ptr_func,
                                                &[self_value_ptr.into()],
                                                "get_data_ptr",
                                            )
                                            .map_err(|e| e.to_string())?
                                            .try_as_basic_value()
                                            .left()
                                            .unwrap()
                                            .into_pointer_value();

                                        // get the struct type and field pointer
                                        let struct_type = self.type_map.get(class_name).unwrap();
                                        let field_ptr = self
                                            .builder
                                            .build_struct_gep(
                                                *struct_type,
                                                data_ptr,
                                                *field_index as u32,
                                                &format!("{}_ptr", name),
                                            )
                                            .map_err(|e| e.to_string())?;

                                        // load the field value
                                        let field_types =
                                            self.field_types_map.get(class_name).unwrap();
                                        let field_type = field_types[*field_index];
                                        let loaded = self
                                            .builder
                                            .build_load(field_type, field_ptr, name)
                                            .map_err(|e| e.to_string())?;

                                        // return the boxed value directly (all fields are stored as Value*)
                                        return Ok(loaded);
                                    }
                                }
                            }
                        }
                    }

                    // check if this is a global function reference
                    if let Some(func) = self.module.get_function(name) {
                        // return function as a pointer value
                        Ok(func.as_global_value().as_pointer_value().into())
                    } else {
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
                                // check if this is a field assignment (bare identifier in method)
                                if let Some(ref func_name) = self.current_function_name {
                                    if func_name.contains('.') {
                                        let class_name = func_name.split('.').next().unwrap();
                                        if let Some(field_index) = self
                                            .field_map
                                            .get(class_name)
                                            .and_then(|fields| fields.get(name))
                                        {
                                            // this is a field assignment on self
                                            if let Some((self_ptr, _, _)) = self
                                                .variables
                                                .get("self")
                                                .or_else(|| self.global_variables.get("self"))
                                            {
                                                // load the object pointer from the alloca
                                                let self_value_ptr = self
                                                    .builder
                                                    .build_load(
                                                        self.context
                                                            .ptr_type(AddressSpace::default()),
                                                        *self_ptr,
                                                        "load_self_for_field_assign",
                                                    )
                                                    .map_err(|e| e.to_string())?
                                                    .into_pointer_value();

                                                // get the raw data pointer from the boxed Value
                                                let get_ptr_func = self
                                                    .module
                                                    .get_function("mux_get_object_ptr")
                                                    .ok_or("mux_get_object_ptr not found")?;
                                                let data_ptr = self
                                                    .builder
                                                    .build_call(
                                                        get_ptr_func,
                                                        &[self_value_ptr.into()],
                                                        "get_data_ptr",
                                                    )
                                                    .map_err(|e| e.to_string())?
                                                    .try_as_basic_value()
                                                    .left()
                                                    .unwrap()
                                                    .into_pointer_value();

                                                // get the struct type and field pointer
                                                let struct_type =
                                                    self.type_map.get(class_name).unwrap();
                                                let field_ptr = self
                                                    .builder
                                                    .build_struct_gep(
                                                        *struct_type,
                                                        data_ptr,
                                                        *field_index as u32,
                                                        &format!("{}_ptr", name),
                                                    )
                                                    .map_err(|e| e.to_string())?;

                                                // store the value
                                                self.builder
                                                    .build_store(field_ptr, right_val)
                                                    .map_err(|e| e.to_string())?;
                                                return Ok(right_val);
                                            }
                                        }
                                    }
                                }

                                if let Some((ptr, _, type_node)) = self
                                    .variables
                                    .get(name)
                                    .or_else(|| self.global_variables.get(name))
                                {
                                    let ptr_copy = *ptr;
                                    // don't box enum struct values - store them directly
                                    let value_to_store = if let Type::Named(type_name, _) =
                                        type_node
                                    {
                                        let is_enum = self
                                            .analyzer
                                            .symbol_table()
                                            .lookup(type_name)
                                            .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                                            .unwrap_or(false);
                                        if is_enum {
                                            // for enum types, store struct value directly (don't box)
                                            right_val
                                        } else {
                                            // for class types, box the value
                                            self.box_value(right_val).into()
                                        }
                                    } else {
                                        // for primitive types, box the value
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
                            } else if let ExpressionKind::FieldAccess { expr, field } = &left.kind {
                                // handle field assignment
                                let mut struct_ptr =
                                    if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                                        if obj_name == "self" {
                                            // special case: accessing field of 'self'
                                            if let Some((self_ptr, _, _)) = self
                                                .variables
                                                .get("self")
                                                .or_else(|| self.global_variables.get("self"))
                                            {
                                                let self_value_ptr = self
                                                    .builder
                                                    .build_load(
                                                        self.context
                                                            .ptr_type(AddressSpace::default()),
                                                        *self_ptr,
                                                        "load_self_for_field_assign",
                                                    )
                                                    .map_err(|e| e.to_string())?
                                                    .into_pointer_value();

                                                // get the raw data pointer from the boxed Value
                                                let get_ptr_func = self
                                                    .module
                                                    .get_function("mux_get_object_ptr")
                                                    .ok_or("mux_get_object_ptr not found")?;
                                                let data_ptr = self
                                                    .builder
                                                    .build_call(
                                                        get_ptr_func,
                                                        &[self_value_ptr.into()],
                                                        "self_data_ptr_assign",
                                                    )
                                                    .map_err(|e| e.to_string())?
                                                    .try_as_basic_value()
                                                    .left()
                                                    .unwrap()
                                                    .into_pointer_value();
                                                data_ptr
                                            } else {
                                                return Err("Self not found in field assignment"
                                                    .to_string());
                                            }
                                        } else {
                                            self.generate_expression(expr)?.into_pointer_value()
                                        }
                                    } else {
                                        self.generate_expression(expr)?.into_pointer_value()
                                    };

                                // for non-self class objects, get the data pointer
                                if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                                    if obj_name != "self" {
                                        if let Some(Type::Named(_, _)) = self
                                            .variables
                                            .get(obj_name)
                                            .or_else(|| self.global_variables.get(obj_name))
                                            .map(|(_, _, t)| t)
                                        {
                                            let get_ptr_func = self
                                                .module
                                                .get_function("mux_get_object_ptr")
                                                .ok_or("mux_get_object_ptr not found")?;
                                            struct_ptr = self
                                                .builder
                                                .build_call(
                                                    get_ptr_func,
                                                    &[struct_ptr.into()],
                                                    "data_ptr_assign",
                                                )
                                                .map_err(|e| e.to_string())?
                                                .try_as_basic_value()
                                                .left()
                                                .unwrap()
                                                .into_pointer_value();
                                        }
                                    }
                                }

                                if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                                    if let Some(type_node) = self
                                        .variables
                                        .get(obj_name)
                                        .or_else(|| self.global_variables.get(obj_name))
                                        .map(|(_, _, t)| t)
                                    {
                                        if let Type::Named(class_name, _) = type_node {
                                            if let Some(field_indices) =
                                                self.field_map.get(class_name.as_str())
                                            {
                                                if let Some(&index) = field_indices.get(field) {
                                                    let struct_type = self
                                                        .type_map
                                                        .get(class_name.as_str())
                                                        .ok_or("Class type not found")?;
                                                    if let BasicTypeEnum::StructType(st) =
                                                        *struct_type
                                                    {
                                                        let field_ptr = self
                                                            .builder
                                                            .build_struct_gep(
                                                                st,
                                                                struct_ptr,
                                                                index as u32,
                                                                field,
                                                            )
                                                            .map_err(|e| e.to_string())?;
                                                        // check if this is an enum field - don't box enum values
                                                        let field_type_node = self
                                                            .classes
                                                            .get(class_name.as_str())
                                                            .and_then(|fields| {
                                                                fields
                                                                    .iter()
                                                                    .find(|f| f.name == *field)
                                                            })
                                                            .map(|f| &f.type_);

                                                        let value_to_store = if let Some(
                                                            field_type,
                                                        ) = field_type_node
                                                        {
                                                            if let TypeNode {
                                                                kind:
                                                                    TypeKind::Named(field_type_name, _),
                                                                ..
                                                            } = field_type
                                                            {
                                                                let is_enum = self.analyzer.symbol_table()
                                                                       .lookup(field_type_name)
                                                                       .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                                                                       .unwrap_or(false);
                                                                if is_enum {
                                                                    // for enum fields, store struct value directly
                                                                    right_val
                                                                } else {
                                                                    // for other fields, box the value
                                                                    self.box_value(right_val).into()
                                                                }
                                                            } else {
                                                                // for non-named types, box the value
                                                                self.box_value(right_val).into()
                                                            }
                                                        } else {
                                                            // fallback: box the value
                                                            self.box_value(right_val).into()
                                                        };

                                                        self.builder
                                                            .build_store(field_ptr, value_to_store)
                                                            .map_err(|e| e.to_string())?;
                                                        Ok(right_val)
                                                    } else {
                                                        Err("Struct type expected".to_string())
                                                    }
                                                } else {
                                                    Err(format!("Field {} not found", field))
                                                }
                                            } else {
                                                Err("Field map not found".to_string())
                                            }
                                        } else {
                                            Err("Named type expected".to_string())
                                        }
                                    } else {
                                        Err("Variable type not found".to_string())
                                    }
                                } else {
                                    Err("Field access on complex expression not supported for assignment".to_string())
                                }
                            } else {
                                Err("Assignment to non-identifier/deref/field not implemented"
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
                                if let Some((ptr, _, _)) = self
                                    .variables
                                    .get(name)
                                    .or_else(|| self.global_variables.get(name))
                                {
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
                                if let Some((ptr, _, _)) = self
                                    .variables
                                    .get(name)
                                    .or_else(|| self.global_variables.get(name))
                                {
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
                    // Special handling for short-circuit logical operators
                    if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
                        return self.generate_short_circuit_logical_op(left, op, right);
                    }

                    // Regular binary operations - evaluate both operands
                    let left_val = self.generate_expression(left)?;
                    let right_val = self.generate_expression(right)?;
                    Ok(self.generate_binary_op(left, left_val, op, right, right_val)?)
                }
            }
            ExpressionKind::Call { func, args } => {
                if let ExpressionKind::Identifier(_name) = &func.kind {}
                if let ExpressionKind::FieldAccess { expr, field } = &func.kind {
                    // special case: method calls on 'self' (keep existing logic)
                    if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                        if obj_name == "self" {
                            return self.generate_method_call_on_self(field, args);
                        }
                    }

                    // handle field access on class fields within method context,
                    // transform items.push_back(item) -> self.items.push_back(item)
                    if let ExpressionKind::Identifier(field_name) = &expr.kind {
                        if let Some(current_function) = &self.current_function_name {
                            if current_function.contains('.') {
                                // we're in a method, check if field_name is a field of current class
                                let class_name = current_function.split('.').next().unwrap();
                                if let Some(class_fields) = self.classes.get(class_name) {
                                    if class_fields.iter().any(|f| f.name == *field_name) {
                                        // get the field type before borrowing self mutably
                                        let field_type = class_fields
                                            .iter()
                                            .find(|f| f.name == *field_name)
                                            .unwrap()
                                            .type_
                                            .clone();
                                        let resolved_field_type =
                                            self.analyzer.resolve_type(&field_type).map_err(
                                                |e| format!("Type resolution failed: {}", e),
                                            )?;

                                        // transform to self.field access
                                        let self_field_expr = ExpressionNode {
                                            kind: ExpressionKind::FieldAccess {
                                                expr: Box::new(ExpressionNode {
                                                    kind: ExpressionKind::Identifier(
                                                        "self".to_string(),
                                                    ),
                                                    span: expr.span,
                                                }),
                                                field: field_name.clone(),
                                            },
                                            span: func.span,
                                        };
                                        // generate method call on the transformed expression
                                        let obj_value =
                                            self.generate_expression(&self_field_expr)?;
                                        return self.generate_method_call(
                                            obj_value,
                                            &resolved_field_type,
                                            field,
                                            args,
                                        );
                                    }
                                }
                            }
                        }
                    }

                    // handle method calls - prioritize variable resolution over class lookup
                    match &expr.kind {
                        ExpressionKind::Identifier(name) => {
                            // first check if this is a variable in current scope
                            if let Some((_, _, var_type)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                // this is an instance method call on a variable
                                let var_type_clone = var_type.clone();
                                let obj_value = self.generate_expression(expr)?;
                                return self.generate_method_call(
                                    obj_value,
                                    &var_type_clone,
                                    field,
                                    args,
                                );
                            } else {
                                // not a variable, check if it's a class or enum
                                if let Some(symbol) = self.analyzer.symbol_table().lookup(name) {
                                    if symbol.kind == crate::semantics::SymbolKind::Class {
                                        // handle constructor/static method calls
                                        if let Some(method) = symbol.methods.get(field) {
                                            if !method.is_static {
                                                return Err(format!(
                                                    "Method {} on class {} is not static",
                                                    field, name
                                                ));
                                            }
                                            // generate static method call (no self parameter)
                                            let mut call_args = vec![];
                                            for arg in args {
                                                call_args
                                                    .push(self.generate_expression(arg)?.into());
                                            }
                                            let call = self
                                                .builder
                                                .build_call(
                                                    self.module
                                                        .get_function(&format!(
                                                            "{}.{}",
                                                            name, field
                                                        ))
                                                        .unwrap(),
                                                    &call_args,
                                                    &format!("{}.{}_call", name, field),
                                                )
                                                .map_err(|e| e.to_string())?;
                                            return Ok(call.try_as_basic_value().left().unwrap());
                                        } else {
                                            return Err(format!(
                                                "Method {} not found on class {}",
                                                field, name
                                            ));
                                        }
                                    } else if symbol.kind == crate::semantics::SymbolKind::Enum {
                                        // handle enum constructor calls like Shape.Circ
                                        let constructor_name = format!("{}_{}", name, field);
                                        if let Some(constructor_func) =
                                            self.module.get_function(&constructor_name)
                                        {
                                            let mut call_args = vec![];
                                            for arg in args {
                                                call_args
                                                    .push(self.generate_expression(arg)?.into());
                                            }
                                            let call = self
                                                .builder
                                                .build_call(
                                                    constructor_func,
                                                    &call_args,
                                                    &format!("{}_call", constructor_name),
                                                )
                                                .map_err(|e| e.to_string())?;
                                            return Ok(call.try_as_basic_value().left().unwrap());
                                        } else {
                                            return Err(format!(
                                                "Enum variant {} not found in enum {}",
                                                field, name
                                            ));
                                        }
                                    }
                                }
                                // not a variable or class - fall through to general expression handling
                            }
                        }
                        ExpressionKind::GenericType(class_name, type_args) => {
                            // convert type arguments to Type
                            let resolved_type_args = type_args
                                .iter()
                                .map(|arg| self.type_node_to_type(arg))
                                .collect::<Vec<_>>();

                            // check if this is a constructor call
                            if field == "new" {
                                // special case: constructor call with type arguments
                                // resolve the type arguments in case they are generic parameters
                                let concrete_type_args = resolved_type_args
                                    .iter()
                                    .map(|arg| self.resolve_type(arg))
                                    .collect::<Result<Vec<_>, _>>()?;
                                return self.generate_constructor_call_with_types(
                                    class_name,
                                    &concrete_type_args,
                                    args,
                                );
                            }

                            // check for static methods on the class
                            if let Some(class_symbol) =
                                self.analyzer.symbol_table().lookup(class_name)
                            {
                                if let Some(method) = class_symbol.methods.get(field) {
                                    if method.is_static {
                                        // set up generic context for static method call
                                        let context = GenericContext {
                                            type_params: self.build_type_param_map(
                                                class_name,
                                                &resolved_type_args,
                                            )?,
                                        };
                                        self.generic_context = Some(context);

                                        // save variables and current builder state before generating specialized methods
                                        let saved_variables = self.variables.clone();
                                        let saved_insert_block = self.builder.get_insert_block();

                                        // generate specialized methods for this class variant if not already generated
                                        if !resolved_type_args.is_empty() {
                                            self.generate_specialized_methods(
                                                class_name,
                                                &resolved_type_args,
                                            )?;
                                        }

                                        // restore variables and builder state after generating specialized methods
                                        self.variables = saved_variables;
                                        if let Some(block) = saved_insert_block {
                                            self.builder.position_at_end(block);
                                        }

                                        // generate static method call - prioritize specialized methods
                                        let mut call_args = vec![];
                                        for arg in args.iter() {
                                            let arg_val = self.generate_expression(arg)?;
                                            call_args.push(arg_val.into());
                                        }

                                        // try specialized method first
                                        let specialized_method_name = self
                                            .create_specialized_method_name(
                                                class_name,
                                                &resolved_type_args,
                                                field,
                                            );
                                        let function_name = if self
                                            .module
                                            .get_function(&specialized_method_name)
                                            .is_some()
                                        {
                                            specialized_method_name
                                        } else {
                                            format!("{}.{}", class_name, field)
                                        };

                                        let call = self
                                            .builder
                                            .build_call(
                                                self.module.get_function(&function_name).ok_or(
                                                    format!("Method '{}' not found", function_name),
                                                )?,
                                                &call_args,
                                                &format!(
                                                    "{}_call",
                                                    function_name.replace('.', "_")
                                                ),
                                            )
                                            .map_err(|e| e.to_string())?;

                                        // clear generic context after call
                                        self.generic_context = None;

                                        return Ok(call.try_as_basic_value().left().unwrap());
                                    } else {
                                        return Err(format!(
                                            "Method {} on class {} is not static",
                                            field, class_name
                                        ));
                                    }
                                }
                            }

                            return Err(format!(
                                "Method {} not found on class {}",
                                field, class_name
                            ));
                        }
                        _ => {
                            // complex expression, handle below
                        }
                    }

                    // handle method calls on complex expressions (not simple identifiers)
                    let obj_value = self.generate_expression(expr)?;

                    // prefer the codegen-local variable type map (it reflects monomorphized
                    // generics), and fall back to the semantic analyzer for complex expressions.
                    let obj_type = if let ExpressionKind::Identifier(name) = &expr.kind {
                        self.variables
                            .get(name)
                            .map(|(_, _, t)| t.clone())
                            .ok_or_else(|| format!("Unknown variable: {}", name))?
                    } else {
                        self.analyzer
                            .get_expression_type(expr)
                            .map_err(|e| format!("Type inference failed: {}", e))?
                    };

                    self.generate_method_call(obj_value, &obj_type, field, args)
                } else if let ExpressionKind::Identifier(name) = &func.kind {
                    // handle regular function calls (non-methods)
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
                            // return void, but since BasicValueEnum, return a dummy
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
                            // return void, but since BasicValueEnum, return a dummy
                            Ok(self.context.i32_type().const_int(0, false).into())
                        }
                        "Err" => {
                            if args.len() != 1 {
                                return Err("Err takes 1 argument".to_string());
                            }
                            if let ExpressionKind::Literal(LiteralNode::String(s)) = &args[0].kind {
                                // generate null-terminated string pointer
                                let name = format!("str_{}", self.string_counter);
                                self.string_counter += 1;
                                let bytes = s.as_bytes();
                                let mut values = vec![];
                                for &b in bytes {
                                    values.push(self.context.i8_type().const_int(b as u64, false));
                                }
                                values.push(self.context.i8_type().const_int(0, false));
                                let array_type =
                                    self.context.i8_type().array_type(values.len() as u32);
                                let const_array = self.context.i8_type().const_array(&values);
                                let global = self.module.add_global(
                                    array_type,
                                    Some(AddressSpace::default()),
                                    &name,
                                );
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
                                let func = self
                                    .module
                                    .get_function("mux_result_err_str")
                                    .ok_or("mux_result_err_str not found")?;
                                let call = self
                                    .builder
                                    .build_call(func, &[ptr.into()], "err_call")
                                    .map_err(|e| e.to_string())?;
                                let result_ptr = call.try_as_basic_value().left().unwrap();
                                Ok(result_ptr)
                            } else {
                                return Err("Err argument must be a string literal".to_string());
                            }
                        }
                        "Ok" => {
                            if args.len() != 1 {
                                return Err("Ok takes 1 argument".to_string());
                            }
                            let arg_expr = &args[0];
                            let arg_type = self
                                .analyzer
                                .get_expression_type(arg_expr)
                                .map_err(|e| format!("Type inference failed: {}", e))?;
                            let arg_val = self.generate_expression(arg_expr)?;
                            let func_name = match arg_type {
                                Type::Primitive(PrimitiveType::Int) => "mux_result_ok_int",
                                Type::Primitive(PrimitiveType::Float) => "mux_result_ok_float",
                                Type::Primitive(PrimitiveType::Bool) => "mux_result_ok_bool",
                                Type::Primitive(PrimitiveType::Char) => "mux_result_ok_char",
                                Type::Primitive(PrimitiveType::Str)
                                | Type::List(_)
                                | Type::Map(_, _)
                                | Type::Set(_)
                                | Type::Named(_, _)
                                | Type::Tuple(_)
                                | Type::Instantiated(_, _) => "mux_result_ok_value",
                                _ => {
                                    return Err(format!(
                                        "Ok() not supported for type {:?}",
                                        arg_type
                                    ));
                                }
                            };
                            let func = self
                                .module
                                .get_function(func_name)
                                .ok_or(format!("{} not found", func_name))?;
                            let call = self
                                .builder
                                .build_call(func, &[arg_val.into()], "ok_call")
                                .map_err(|e| e.to_string())?;
                            let result_ptr = call.try_as_basic_value().left().unwrap();
                            // result constructors return Value* pointers directly
                            Ok(result_ptr)
                        }
                        "Some" => {
                            if args.len() != 1 {
                                return Err("Some takes 1 argument".to_string());
                            }
                            let arg_expr = &args[0];
                            let arg_type = self
                                .analyzer
                                .get_expression_type(arg_expr)
                                .map_err(|e| format!("Type inference failed: {}", e))?;
                            let arg_val = self.generate_expression(arg_expr)?;
                            let func_name = match arg_type {
                                Type::Primitive(PrimitiveType::Int) => "mux_optional_some_int",
                                Type::Primitive(PrimitiveType::Float) => "mux_optional_some_float",
                                Type::Primitive(PrimitiveType::Bool) => "mux_optional_some_bool",
                                Type::Primitive(PrimitiveType::Char) => "mux_optional_some_char",
                                Type::Primitive(PrimitiveType::Str)
                                | Type::List(_)
                                | Type::Map(_, _)
                                | Type::Set(_)
                                | Type::Named(_, _)
                                | Type::Tuple(_)
                                | Type::Instantiated(_, _) => "mux_optional_some_value",
                                _ => {
                                    return Err(format!(
                                        "Some() not supported for type {:?}",
                                        arg_type
                                    ));
                                }
                            };
                            let func = self
                                .module
                                .get_function(func_name)
                                .ok_or(format!("{} not found", func_name))?;
                            let call = self
                                .builder
                                .build_call(func, &[arg_val.into()], "some_call")
                                .map_err(|e| e.to_string())?;
                            let result_ptr = call.try_as_basic_value().left().unwrap();
                            // optional constructors return Value* pointers directly
                            Ok(result_ptr)
                        }
                        "None" => {
                            if args.is_empty() {
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
                            // optional constructors return Value* pointers directly
                            Ok(result_ptr)
                        }
                        _ => {
                            // first check if this is a function pointer variable
                            if let Some((ptr, _, var_type)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                let var_type_clone = var_type.clone();
                                if matches!(var_type, Type::Function { .. }) {
                                    // load function pointer
                                    let func_ptr = self
                                        .builder
                                        .build_load(
                                            self.context.ptr_type(AddressSpace::default()),
                                            *ptr,
                                            name,
                                        )
                                        .map_err(|e| e.to_string())?
                                        .into_pointer_value();

                                    // generate arguments
                                    let mut call_args = vec![];
                                    for arg in args {
                                        call_args.push(self.generate_expression(arg)?.into());
                                    }

                                    // get function type from resolved type
                                    let func_type = if let Type::Function { params, returns } =
                                        var_type_clone
                                    {
                                        // convert parameter types to LLVM types
                                        let mut param_types = Vec::new();
                                        for param in params {
                                            let type_node = self.type_to_type_node(&param);
                                            param_types.push(
                                                self.llvm_type_from_mux_type(&type_node)?.into(),
                                            );
                                        }
                                        // handle return type - check for void first
                                        if matches!(*returns, Type::Void) {
                                            self.context.void_type().fn_type(&param_types, false)
                                        } else {
                                            // convert return type to LLVM type
                                            let return_type_node = self.type_to_type_node(&returns);
                                            let return_type =
                                                self.llvm_type_from_mux_type(&return_type_node)?;
                                            return_type.fn_type(&param_types, false)
                                        }
                                    } else {
                                        return Err("Expected function type".to_string());
                                    };

                                    // make indirect call through function pointer
                                    let call = self
                                        .builder
                                        .build_indirect_call(
                                            func_type,
                                            func_ptr,
                                            &call_args,
                                            "indirect_func_call",
                                        )
                                        .map_err(|e| e.to_string())?;

                                    // handle return value
                                    return match call.try_as_basic_value().left() {
                                        Some(val) => Ok(val),
                                        None => {
                                            Ok(self.context.i32_type().const_int(0, false).into())
                                        }
                                    };
                                } else {
                                    // not a function pointer, try global function lookup
                                    if let Some(func) = self.module.get_function(name) {
                                        // print some info about the found function
                                        let mut call_args = vec![];
                                        for arg in args {
                                            call_args.push(self.generate_expression(arg)?.into());
                                        }
                                        let call = self
                                            .builder
                                            .build_call(func, &call_args, "user_func_call")
                                            .map_err(|e| e.to_string())?;
                                        match call.try_as_basic_value().left() {
                                            Some(val) => Ok(val),
                                            None => Ok(self
                                                .context
                                                .i32_type()
                                                .const_int(0, false)
                                                .into()),
                                        }
                                    } else {
                                        Err(format!("Undefined function: {}", name))
                                    }
                                }
                            } else {
                                // check if this is a generic function that needs instantiation
                                if let Some(func_symbol) = self.analyzer.symbol_table().lookup(name)
                                {
                                    if let SymbolKind::Function = func_symbol.kind {
                                        if let Some(func_node) = self.function_nodes.get(name) {
                                            // check if this function has truly generic type parameters (not concrete types)
                                            let has_generic_params =
                                                !func_node.type_params.is_empty()
                                                    && func_node.type_params.iter().any(
                                                        |(param_name, _)| {
                                                            // a parameter is generic if it's not a concrete type name
                                                            // generic parameters are typically single uppercase letters or descriptive names
                                                            // concrete types are lowercase like "int", "string", etc.
                                                            param_name
                                                                .chars()
                                                                .next()
                                                                .unwrap_or(' ')
                                                                .is_uppercase()
                                                                || param_name.len() > 3
                                                            // heuristic: generic names are usually short
                                                        },
                                                    );

                                            if has_generic_params {
                                                // this is a generic function call - instantiate it
                                                return self
                                                    .generate_generic_function_call(name, args);
                                            }
                                        }
                                    }
                                }

                                // not a generic function, try global function lookup
                                if let Some(func) = self.module.get_function(name) {
                                    let mut call_args = vec![];
                                    for arg in args {
                                        call_args.push(self.generate_expression(arg)?.into());
                                    }
                                    let call = self
                                        .builder
                                        .build_call(func, &call_args, "user_func_call")
                                        .map_err(|e| e.to_string())?;
                                    match call.try_as_basic_value().left() {
                                        Some(val) => Ok(val),
                                        None => {
                                            Ok(self.context.i32_type().const_int(0, false).into())
                                        }
                                    }
                                } else {
                                    Err(format!("Undefined function: {}", name))
                                }
                            }
                        }
                    }
                } else {
                    Err("Unsupported function call type".to_string())
                }
            }
            ExpressionKind::ListAccess { expr, index } => {
                let list_val = self.generate_expression(expr)?;
                let index_val = self.generate_expression(index)?;

                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_list").unwrap(),
                        &[list_val.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                // call mux_list_get_value (returns direct value or null)
                let raw_result = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_get_value").unwrap(),
                        &[
                            raw_list.try_as_basic_value().left().unwrap().into(),
                            index_val.into(),
                        ],
                        "list_raw",
                    )
                    .map_err(|e| e.to_string())?;

                let result_ptr = raw_result
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();

                // check for null (out of bounds)
                let is_null = self
                    .builder
                    .build_is_null(result_ptr, "is_null")
                    .map_err(|e| e.to_string())?;

                // get current function for basic blocks
                let current_function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .ok_or("No current function")?;

                // create error block and continue block
                let error_bb = self
                    .context
                    .append_basic_block(current_function, "index_error");
                let continue_bb = self
                    .context
                    .append_basic_block(current_function, "index_continue");

                self.builder
                    .build_conditional_branch(is_null, error_bb, continue_bb)
                    .map_err(|e| e.to_string())?;

                // error block: print error and exit
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

                // continue block: extract the actual primitive value from the Value pointer
                self.builder.position_at_end(continue_bb);

                // assumes Int type for now, should infer from context
                let get_int_func = self
                    .module
                    .get_function("mux_value_get_int")
                    .ok_or("mux_value_get_int not found")?;
                let int_val = self
                    .builder
                    .build_call(get_int_func, &[result_ptr.into()], "extracted_int")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Ok(int_val)
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
                // convert list pointer to Value for type consistency
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
                let mut struct_ptr = if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                    if obj_name == "self" {
                        // special case: accessing field of 'self' - load actual object pointer from alloca first
                        if let Some((self_ptr, _, _)) = self
                            .variables
                            .get("self")
                            .or_else(|| self.global_variables.get("self"))
                        {
                            let self_value_ptr = self
                                .builder
                                .build_load(
                                    self.context.ptr_type(AddressSpace::default()),
                                    *self_ptr,
                                    "load_self_for_field_access",
                                )
                                .map_err(|e| e.to_string())?
                                .into_pointer_value();

                            // get the raw data pointer from the boxed Value
                            let get_ptr_func = self
                                .module
                                .get_function("mux_get_object_ptr")
                                .ok_or("mux_get_object_ptr not found")?;
                            let data_ptr = self
                                .builder
                                .build_call(get_ptr_func, &[self_value_ptr.into()], "self_data_ptr")
                                .map_err(|e| e.to_string())?
                                .try_as_basic_value()
                                .left()
                                .unwrap()
                                .into_pointer_value();
                            data_ptr
                        } else {
                            return Err("Self not found in field access".to_string());
                        }
                    } else {
                        self.generate_expression(expr)?.into_pointer_value()
                    }
                } else {
                    self.generate_expression(expr)?.into_pointer_value()
                };

                // for non-self class objects, get the data pointer
                if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                    if obj_name != "self" {
                        if let Some(Type::Named(_, _)) = self
                            .variables
                            .get(obj_name)
                            .or_else(|| self.global_variables.get(obj_name))
                            .map(|(_, _, t)| t)
                        {
                            let get_ptr_func = self
                                .module
                                .get_function("mux_get_object_ptr")
                                .ok_or("mux_get_object_ptr not found")?;
                            struct_ptr = self
                                .builder
                                .build_call(get_ptr_func, &[struct_ptr.into()], "data_ptr")
                                .map_err(|e| e.to_string())?
                                .try_as_basic_value()
                                .left()
                                .unwrap()
                                .into_pointer_value();
                        }
                    }
                }

                if let ExpressionKind::Identifier(obj_name) = &expr.kind {
                    if let Some(type_node) = self
                        .variables
                        .get(obj_name)
                        .or_else(|| self.global_variables.get(obj_name))
                        .map(|(_, _, t)| t)
                    {
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

                                        // check if this field is an enum type
                                        let field_types = self
                                            .field_types_map
                                            .get(class_name.as_str())
                                            .ok_or("Field types not found for class")?;
                                        if index < field_types.len() {
                                            let field_type = field_types[index];

                                            // check if field type is a struct (enum)
                                            if let BasicTypeEnum::StructType(struct_type) =
                                                field_type
                                            {
                                                // for enum fields: load as struct value
                                                let loaded = self
                                                    .builder
                                                    .build_load(struct_type, field_ptr, field)
                                                    .map_err(|e| e.to_string())?;
                                                return Ok(loaded);
                                            }
                                        }
                                        // for non-enum fields: check if it's a generic field
                                        let field_type_node = &field_types[index];
                                        if *field_type_node == self.context.i64_type().into() {
                                            // this might be a generic field (T), load as pointer (boxed value)
                                            let loaded = self
                                                .builder
                                                .build_load(
                                                    self.context.ptr_type(AddressSpace::default()),
                                                    field_ptr,
                                                    field,
                                                )
                                                .map_err(|e| e.to_string())?;
                                            return Ok(loaded);
                                        } else {
                                            // regular non-enum field
                                            // get the field's mux type to determine if it needs unboxing
                                            let class_fields = self
                                                .classes
                                                .get(class_name.as_str())
                                                .ok_or("Class fields not found")?;
                                            let field_def = class_fields
                                                .iter()
                                                .find(|f| f.name == *field)
                                                .ok_or("Field not found")?;
                                            let resolved_field_type = self
                                                .analyzer
                                                .resolve_type(&field_def.type_)
                                                .map_err(|e| e.to_string())?;

                                            // load the field value (all fields stored as Value*)
                                            let loaded = self
                                                .builder
                                                .build_load(*field_type_node, field_ptr, field)
                                                .map_err(|e| e.to_string())?;

                                            // handle unboxing for primitive fields
                                            match &resolved_field_type {
                                                Type::Primitive(PrimitiveType::Int) => {
                                                    let raw_int = self.get_raw_int_value(loaded)?;
                                                    return Ok(raw_int.into());
                                                }
                                                Type::Primitive(PrimitiveType::Float) => {
                                                    let raw_float =
                                                        self.get_raw_float_value(loaded)?;
                                                    return Ok(raw_float.into());
                                                }
                                                Type::Primitive(PrimitiveType::Bool) => {
                                                    let raw_bool =
                                                        self.get_raw_bool_value(loaded)?;
                                                    return Ok(raw_bool.into());
                                                }
                                                Type::Named(name, _type_args) => {
                                                    // check if this is a substituted generic type
                                                    if let Some(context) = &self.generic_context {
                                                        if let Some(concrete_type) =
                                                            context.type_params.get(name)
                                                        {
                                                            // recursively handle the concrete type
                                                            match concrete_type {
                                                                Type::Primitive(
                                                                    PrimitiveType::Int,
                                                                ) => {
                                                                    let raw_int = self
                                                                        .get_raw_int_value(
                                                                            loaded,
                                                                        )?;
                                                                    return Ok(raw_int.into());
                                                                }
                                                                Type::Primitive(
                                                                    PrimitiveType::Float,
                                                                ) => {
                                                                    let raw_float = self
                                                                        .get_raw_float_value(
                                                                            loaded,
                                                                        )?;
                                                                    return Ok(raw_float.into());
                                                                }
                                                                Type::Primitive(
                                                                    PrimitiveType::Bool,
                                                                ) => {
                                                                    let raw_bool = self
                                                                        .get_raw_bool_value(
                                                                            loaded,
                                                                        )?;
                                                                    return Ok(raw_bool.into());
                                                                }
                                                                _ => {} // for other concrete types, continue with default handling
                                                            }
                                                        }
                                                    }
                                                    // if no generic context match, continue with default handling
                                                }
                                                _ => {} // for non-primitives, return the loaded pointer
                                            }

                                            return Ok(loaded);
                                        }
                                    }
                                }
                            }
                        }
                    } else if let Some((_, _, type_node)) = self
                        .variables
                        .get(obj_name)
                        .or_else(|| self.global_variables.get(obj_name))
                    {
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
                                let float_val = self.generate_expression(expr)?;
                                // call mux_float_to_string directly on the raw float
                                let func = self
                                    .module
                                    .get_function("mux_float_to_string")
                                    .ok_or("mux_float_to_string not found")?;
                                let call = self
                                    .builder
                                    .build_call(func, &[float_val.into()], "float_to_str")
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
                            if let Some((ptr, _, _)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                // for identifier references: return pointer to the alloca containing the boxed value
                                // don't dereference - we want a reference to the variable itself
                                Ok((*ptr).into())
                            } else {
                                Err(format!("Undefined variable {}", name))
                            }
                        } else {
                            // complex expression: evaluate, allocate temp ptr, store the result
                            let expr_val = self.generate_expression(expr)?;
                            // box the value if it's not already a pointer
                            let boxed_val = if expr_val.is_pointer_value() {
                                expr_val.into_pointer_value()
                            } else {
                                self.box_value(expr_val)
                            };
                            let ptr_type = self.context.ptr_type(AddressSpace::default());
                            let temp = self
                                .builder
                                .build_alloca(ptr_type, "ref_temp")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(temp, boxed_val)
                                .map_err(|e| e.to_string())?;
                            Ok(temp.into())
                        }
                    }
                    UnaryOp::Deref => {
                        let ref_val = self.generate_expression(expr)?;
                        // load the pointer to the boxed value from the reference
                        let boxed_ptr = self
                            .builder
                            .build_load(
                                self.context.ptr_type(AddressSpace::default()),
                                ref_val.into_pointer_value(),
                                "boxed_ptr",
                            )
                            .map_err(|e| e.to_string())?;
                        // now we need to extract the actual value from the boxed pointer
                        // this depends on the type of the referenced variable
                        if let ExpressionKind::Identifier(name) = &expr.kind {
                            if let Some((_, _, var_type)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                match var_type {
                                    Type::Reference(inner_type) => match inner_type.as_ref() {
                                        Type::Primitive(PrimitiveType::Int) => {
                                            let raw_int = self.get_raw_int_value(boxed_ptr)?;
                                            Ok(raw_int.into())
                                        }
                                        Type::Primitive(PrimitiveType::Float) => {
                                            let raw_float = self.get_raw_float_value(boxed_ptr)?;
                                            Ok(raw_float.into())
                                        }
                                        Type::Primitive(PrimitiveType::Bool) => {
                                            let raw_bool = self.get_raw_bool_value(boxed_ptr)?;
                                            Ok(raw_bool.into())
                                        }
                                        _ => Ok(boxed_ptr),
                                    },
                                    _ => Ok(boxed_ptr),
                                }
                            } else {
                                Ok(boxed_ptr)
                            }
                        } else {
                            Ok(boxed_ptr)
                        }
                    }
                    UnaryOp::Incr => {
                        // Load current value, add 1, store back
                        if let ExpressionKind::Identifier(name) = &expr.kind {
                            if let Some((ptr, _, _)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                let ptr_copy = *ptr;
                                // Load the mux_value* pointer
                                let value_ptr = self
                                    .builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        ptr_copy,
                                        &format!("{}_load", name),
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into_pointer_value();

                                // Extract the i64 from the mux_value using the runtime function
                                let get_int_func = self
                                    .module
                                    .get_function("mux_value_get_int")
                                    .ok_or("mux_value_get_int not found")?;
                                let current_val = self
                                    .builder
                                    .build_call(
                                        get_int_func,
                                        &[value_ptr.into()],
                                        &format!("{}_get_int", name),
                                    )
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_int_value();

                                // Add 1
                                let one = self.context.i64_type().const_int(1, false);
                                let new_val = self
                                    .builder
                                    .build_int_add(current_val, one, "incr_result")
                                    .map_err(|e| e.to_string())?;

                                // Box it back into a mux_value*
                                let boxed_val = self.box_value(new_val.into());

                                // Store back to the variable
                                self.builder
                                    .build_store(ptr_copy, boxed_val)
                                    .map_err(|e| e.to_string())?;

                                Ok(new_val.into())
                            } else {
                                Err(format!("Undefined variable {}", name))
                            }
                        } else {
                            Err("Increment operator only supports simple variables for now"
                                .to_string())
                        }
                    }
                    UnaryOp::Decr => {
                        // Load current value, subtract 1, store back
                        if let ExpressionKind::Identifier(name) = &expr.kind {
                            if let Some((ptr, _, _)) = self.variables.get(name) {
                                let ptr_copy = *ptr;
                                // Load the mux_value* pointer
                                let value_ptr = self
                                    .builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        ptr_copy,
                                        &format!("{}_load", name),
                                    )
                                    .map_err(|e| e.to_string())?
                                    .into_pointer_value();

                                // Extract the i64 from the mux_value using the runtime function
                                let get_int_func = self
                                    .module
                                    .get_function("mux_value_get_int")
                                    .ok_or("mux_value_get_int not found")?;
                                let current_val = self
                                    .builder
                                    .build_call(
                                        get_int_func,
                                        &[value_ptr.into()],
                                        &format!("{}_get_int", name),
                                    )
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_int_value();

                                // Subtract 1
                                let one = self.context.i64_type().const_int(1, false);
                                let new_val = self
                                    .builder
                                    .build_int_sub(current_val, one, "decr_result")
                                    .map_err(|e| e.to_string())?;

                                // Box it back into a mux_value*
                                let boxed_val = self.box_value(new_val.into());

                                // Store back to the variable
                                self.builder
                                    .build_store(ptr_copy, boxed_val)
                                    .map_err(|e| e.to_string())?;

                                Ok(new_val.into())
                            } else {
                                Err(format!("Undefined variable {}", name))
                            }
                        } else {
                            Err("Decrement operator only supports simple variables for now"
                                .to_string())
                        }
                    }
                    UnaryOp::Not => {
                        // Evaluate the expression to get a bool value
                        let expr_val = self.generate_expression(expr)?;

                        // Get raw bool value (i1) - handles both raw i1 and boxed bool
                        let bool_val = self.get_raw_bool_value(expr_val)?;

                        // Use LLVM's build_not to invert the i1 value
                        let not_result = self
                            .builder
                            .build_not(bool_val, "not")
                            .map_err(|e| e.to_string())?;

                        Ok(not_result.into())
                    }
                    UnaryOp::Neg => {
                        // Negate a number (int or float)
                        let expr_val = self.generate_expression(expr)?;

                        if expr_val.is_int_value() {
                            let int_val = expr_val.into_int_value();
                            let neg = self
                                .builder
                                .build_int_neg(int_val, "neg")
                                .map_err(|e| e.to_string())?;
                            Ok(neg.into())
                        } else if expr_val.is_float_value() {
                            let float_val = expr_val.into_float_value();
                            let neg = self
                                .builder
                                .build_float_neg(float_val, "neg")
                                .map_err(|e| e.to_string())?;
                            Ok(neg.into())
                        } else {
                            // Try to extract from boxed value
                            if let Ok(int_val) = self.get_raw_int_value(expr_val) {
                                let neg = self
                                    .builder
                                    .build_int_neg(int_val, "neg")
                                    .map_err(|e| e.to_string())?;
                                Ok(neg.into())
                            } else if let Ok(float_val) = self.get_raw_float_value(expr_val) {
                                let neg = self
                                    .builder
                                    .build_float_neg(float_val, "neg")
                                    .map_err(|e| e.to_string())?;
                                Ok(neg.into())
                            } else {
                                Err("Negation only works on int or float values".to_string())
                            }
                        }
                    }
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

                // check if this variable already exists in current scope (e.g., pre-created global)
                if let Some((existing_ptr, _, _)) = self.variables.get(name).cloned() {
                    // variable already exists - just store to it (this handles globals in main)
                    if value.is_struct_value() {
                        self.builder
                            .build_store(existing_ptr, value)
                            .map_err(|e| e.to_string())?;
                    } else {
                        let boxed = self.box_value(value);
                        self.builder
                            .build_store(existing_ptr, boxed)
                            .map_err(|e| e.to_string())?;
                    }
                } else {
                    // create new local variable
                    if value.is_struct_value() {
                        let var_type = value.get_type();
                        let alloca = if let Some(func) = function {
                            // in a function - create alloca in entry block
                            self.create_entry_block_alloca(*func, var_type, name)?
                        } else {
                            // top-level (shouldn't happen as globals are pre-created)
                            self.builder
                                .build_alloca(var_type, name)
                                .map_err(|e| e.to_string())?
                        };
                        self.builder
                            .build_store(alloca, value)
                            .map_err(|e| e.to_string())?;
                        self.variables
                            .insert(name.clone(), (alloca, var_type, resolved_type.clone()));
                    } else {
                        let boxed = self.box_value(value);
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let alloca = if let Some(func) = function {
                            // in a function - create alloca in entry block
                            self.create_entry_block_alloca(*func, ptr_type.into(), name)?
                        } else {
                            // top-level (shouldn't happen as globals are pre-created)
                            self.builder
                                .build_alloca(ptr_type, name)
                                .map_err(|e| e.to_string())?
                        };
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

                // check if this variable already exists in current scope (e.g., pre-created global)
                if let Some((existing_ptr, _, _)) = self.variables.get(name).cloned() {
                    // variable already exists - just store to it (this handles globals in main)
                    if value.is_struct_value() {
                        self.builder
                            .build_store(existing_ptr, value)
                            .map_err(|e| e.to_string())?;
                    } else {
                        let boxed = self.box_value(value);
                        self.builder
                            .build_store(existing_ptr, boxed)
                            .map_err(|e| e.to_string())?;
                    }
                } else {
                    // create new local variable (this handles new locals in functions)
                    if value.is_struct_value() {
                        let alloca = if let Some(func) = function {
                            // in a function - create alloca in entry block
                            self.create_entry_block_alloca(*func, var_type, name)?
                        } else {
                            // top-level (shouldn't happen as globals are pre-created)
                            self.builder
                                .build_alloca(var_type, name)
                                .map_err(|e| e.to_string())?
                        };
                        self.builder
                            .build_store(alloca, value)
                            .map_err(|e| e.to_string())?;
                        self.variables
                            .insert(name.clone(), (alloca, var_type, resolved_type.clone()));
                    } else {
                        let boxed = self.box_value(value);
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let alloca = if let Some(func) = function {
                            // in a function - create alloca in entry block
                            self.create_entry_block_alloca(*func, ptr_type.into(), name)?
                        } else {
                            // top-level (shouldn't happen as globals are pre-created)
                            self.builder
                                .build_alloca(ptr_type, name)
                                .map_err(|e| e.to_string())?
                        };
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
            }
            StatementKind::ConstDecl(name, _, expr) => {
                let value = self.generate_expression(expr)?;
                let boxed = self.box_value(value);
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let symbol = self
                    .analyzer
                    .symbol_table()
                    .lookup(name)
                    .ok_or("Symbol not found")?;
                let resolved_type = symbol.type_.as_ref().ok_or("Type not resolved")?;

                // check if this constant already exists in current scope (e.g., pre-created global)
                if let Some((existing_ptr, _, _)) = self.variables.get(name).cloned() {
                    // constant already exists - just store to it
                    self.builder
                        .build_store(existing_ptr, boxed)
                        .map_err(|e| e.to_string())?;
                } else {
                    // create new local constant
                    let alloca = if let Some(func) = function {
                        // in a function - create alloca in entry block
                        self.create_entry_block_alloca(*func, ptr_type.into(), name)?
                    } else {
                        // top-level (shouldn't happen as globals are pre-created)
                        self.builder
                            .build_alloca(ptr_type, name)
                            .map_err(|e| e.to_string())?
                    };
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
            StatementKind::Return(Some(expr)) => {
                // special handling for boolean literals in boolean functions
                if let ExpressionKind::Literal(LiteralNode::Boolean(b)) = &expr.kind {
                    if let Some(ResolvedType::Primitive(PrimitiveType::Bool)) =
                        &self.current_function_return_type
                    {
                        // return boolean literal directly as i1
                        let bool_val = self
                            .context
                            .bool_type()
                            .const_int(if *b { 1 } else { 0 }, false);
                        self.builder
                            .build_return(Some(&bool_val))
                            .map_err(|e| e.to_string())?;
                        return Ok(());
                    }
                }

                let value = self.generate_expression(expr)?;
                // check if we need to return raw primitive or boxed value
                if let Some(return_type) = &self.current_function_return_type {
                    match return_type {
                        ResolvedType::Primitive(PrimitiveType::Int) => {
                            // for int, unbox if necessary
                            let raw_int = self.get_raw_int_value(value)?;
                            self.builder
                                .build_return(Some(&raw_int))
                                .map_err(|e| e.to_string())?;
                        }
                        ResolvedType::Primitive(PrimitiveType::Float) => {
                            // for float, unbox if necessary
                            let raw_float = self.get_raw_float_value(value)?;
                            self.builder
                                .build_return(Some(&raw_float))
                                .map_err(|e| e.to_string())?;
                        }
                        ResolvedType::Primitive(PrimitiveType::Bool) => {
                            // for bool, unbox if necessary and return i1
                            if value.is_int_value() {
                                let int_val = value.into_int_value();
                                // check if we need to truncate i32/i64 to i1
                                if int_val.get_type().get_bit_width() == 1 {
                                    // already i1, return directly
                                    self.builder
                                        .build_return(Some(&value))
                                        .map_err(|e| e.to_string())?;
                                } else {
                                    // truncate i32/i64 to i1
                                    let bool_val = self
                                        .builder
                                        .build_int_truncate(
                                            int_val,
                                            self.context.bool_type(),
                                            "int_to_i1",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    self.builder
                                        .build_return(Some(&bool_val))
                                        .map_err(|e| e.to_string())?;
                                }
                            } else if value.is_pointer_value() {
                                // extract from boxed value
                                let ptr = value.into_pointer_value();
                                let get_bool_fn = self
                                    .module
                                    .get_function("mux_value_get_bool")
                                    .ok_or("mux_value_get_bool not found")?;
                                let result = self
                                    .builder
                                    .build_call(get_bool_fn, &[ptr.into()], "get_bool")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .ok_or("Call returned no value")?;
                                // convert i32 to i1 for return
                                let bool_val = self
                                    .builder
                                    .build_int_truncate(
                                        result.into_int_value(),
                                        self.context.bool_type(),
                                        "i32_to_i1",
                                    )
                                    .map_err(|e| e.to_string())?;
                                self.builder
                                    .build_return(Some(&bool_val))
                                    .map_err(|e| e.to_string())?;
                            } else {
                                return Err("Expected bool value or pointer".to_string());
                            }
                        }
                        ResolvedType::List(_) => {
                            // for list types, return the wrapped Value* directly
                            // the function signature should expect wrapped pointers for lists
                            if value.is_pointer_value() {
                                self.builder
                                    .build_return(Some(&value))
                                    .map_err(|e| e.to_string())?;
                            } else {
                                return Err("Expected pointer value for list return".to_string());
                            }
                        }
                        _ => {
                            // for complex types, ensure it's boxed
                            let boxed = match value {
                                BasicValueEnum::PointerValue(_) => value, // already boxed
                                _ => self.box_value(value).into(), // box it and convert to BasicValueEnum
                            };
                            self.builder
                                .build_return(Some(&boxed))
                                .map_err(|e| e.to_string())?;
                        }
                    }
                } else {
                    // fallback: assume boxed
                    let boxed = self.box_value(value);
                    self.builder
                        .build_return(Some(&boxed))
                        .map_err(|e| e.to_string())?;
                }
            }
            StatementKind::Return(None) => {
                self.builder.build_return(None).map_err(|e| e.to_string())?;
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

                // check if we need a merge block
                let then_ends_with_return = then_block
                    .last()
                    .is_some_and(|s| matches!(s.kind, StatementKind::Return(_)));
                let else_ends_with_return = if let Some(else_stmts) = &else_block {
                    else_stmts
                        .last()
                        .is_some_and(|s| matches!(s.kind, StatementKind::Return(_)))
                } else {
                    false
                };
                let needs_merge = !then_ends_with_return || !else_ends_with_return;

                let merge_bb = if needs_merge {
                    Some(
                        self.context
                            .append_basic_block(*function, &format!("if_merge_{}", if_id)),
                    )
                } else {
                    None
                };

                self.builder
                    .build_conditional_branch(cond_int, then_bb, else_bb)
                    .map_err(|e| e.to_string())?;

                // then block
                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.generate_statement(stmt, Some(function))?;
                }
                if !then_ends_with_return {
                    if let Some(merge_bb) = merge_bb {
                        self.builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(|e| e.to_string())?;
                    }
                }

                // else block
                self.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.generate_statement(stmt, Some(function))?;
                    }
                }
                if !else_ends_with_return {
                    if let Some(merge_bb) = merge_bb {
                        self.builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(|e| e.to_string())?;
                    }
                }

                // merge
                if let Some(merge_bb) = merge_bb {
                    self.builder.position_at_end(merge_bb);
                }
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
                // assume iter is range(start, end) or list identifier
                if let ExpressionKind::Call { func, args } = &iter.kind {
                    if let ExpressionKind::Identifier(name) = &func.kind {
                        if name == "range" && args.len() == 2 {
                            let resolved_var_type = Type::Primitive(PrimitiveType::Int);
                            let start_val = self.generate_expression(&args[0])?;
                            let end_val = self.generate_expression(&args[1])?;

                            // create index variable
                            let index_type = self.context.i64_type();
                            let index_alloca = self
                                .builder
                                .build_alloca(index_type, "index")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(index_alloca, start_val)
                                .map_err(|e| e.to_string())?;

                            // create loop var
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

                            // loop header
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

                            // header: check index < end
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

                            // body: set var = index, then body
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
                            // increment index
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
                } else if let ExpressionKind::Identifier(_) = &iter.kind {
                    // iterate over list
                    let resolved_var_type = self
                        .analyzer
                        .resolve_type(var_type)
                        .map_err(|e| e.message)?;
                    let list_val = self.generate_expression(iter)?;

                    // get length
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

                    // create index variable
                    let index_type = self.context.i64_type();
                    let index_alloca = self
                        .builder
                        .build_alloca(index_type, "index")
                        .map_err(|e| e.to_string())?;
                    let zero = self.context.i64_type().const_int(0, false);
                    self.builder
                        .build_store(index_alloca, zero)
                        .map_err(|e| e.to_string())?;

                    // create loop var
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

                    // loop header
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

                    // header: check index < len
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

                    // body: get element at index
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
                    // store the Value pointer directly
                    self.builder
                        .build_store(var_alloca, value_ptr)
                        .map_err(|e| e.to_string())?;

                    // execute body
                    for stmt in body {
                        self.generate_statement(stmt, Some(function))?;
                    }

                    // increment index
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

                    // create continuation block for code after the loop
                    let continue_bb = self
                        .context
                        .append_basic_block(*function, &format!("for_continue_{}", label_id));

                    // position exit block to branch to continuation
                    self.builder.position_at_end(exit_bb);
                    self.builder
                        .build_unconditional_branch(continue_bb)
                        .map_err(|e| e.to_string())?;

                    // position at continuation block for code after loop
                    self.builder.position_at_end(continue_bb);
                } else {
                    return Err("For loop iter must be range(...) or list identifier".to_string());
                }
            }
            StatementKind::Match { expr, arms } => {
                let function = function.ok_or("Match not in function")?;

                // check if match expression is complex (not simple identifier/constructor/field access)
                let (expr_val, match_expr) = if matches!(
                    &expr.kind,
                    ExpressionKind::Identifier(_) | ExpressionKind::FieldAccess { .. }
                ) || matches!(&expr.kind, ExpressionKind::Call { func, .. } if matches!(func.kind, ExpressionKind::Identifier(_)))
                {
                    // simple expressions - use directly
                    (self.generate_expression(expr)?, expr.clone())
                } else {
                    // complex expression - evaluate first and store in temporary
                    let temp_val = self.generate_expression(expr)?;
                    let temp_name = format!("match_temp_{}", self.label_counter);
                    self.label_counter += 1;

                    // create temporary variable to hold the result
                    let temp_type = self.context.ptr_type(AddressSpace::default());
                    let temp_alloca = self
                        .builder
                        .build_alloca(temp_type, &temp_name)
                        .map_err(|e| e.to_string())?;

                    // store the result
                    self.builder
                        .build_store(temp_alloca, temp_val)
                        .map_err(|e| e.to_string())?;

                    // get the actual type for the temporary variable
                    let actual_type = self
                        .analyzer
                        .get_expression_type(expr)
                        .map_err(|e| format!("Type inference failed: {}", e))?;

                    // add to variables for pattern matching
                    self.variables.insert(
                        temp_name.clone(),
                        (
                            temp_alloca,
                            BasicTypeEnum::PointerType(temp_type),
                            actual_type,
                        ),
                    );

                    // create synthetic identifier expression for the temporary
                    let temp_expr = ExpressionNode {
                        kind: ExpressionKind::Identifier(temp_name),
                        span: expr.span,
                    };

                    (temp_val, temp_expr)
                };

                // get the full type of the match expression
                let match_expr_type = match &match_expr.kind {
                    ExpressionKind::Identifier(name) => {
                        if name == "self" {
                            if let Some((_, _, var_type)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                var_type.clone()
                            } else {
                                return Err("Self not found".to_string());
                            }
                        } else if name.starts_with("match_temp_") {
                            // temporary variable created during codegen
                            if let Some((_, _, var_type)) = self
                                .variables
                                .get(name)
                                .or_else(|| self.global_variables.get(name))
                            {
                                var_type.clone()
                            } else {
                                return Err(format!("Temporary variable {} not found", name));
                            }
                        } else {
                            self.analyzer
                                .get_expression_type(&match_expr)
                                .map_err(|e| format!("Type inference failed: {}", e))?
                        }
                    }
                    ExpressionKind::FieldAccess { expr, field } => {
                        if let ExpressionKind::Identifier(obj) = &expr.kind {
                            if obj == "self" {
                                if let Some((_, _, Type::Named(class_name, _))) = self
                                    .variables
                                    .get("self")
                                    .or_else(|| self.global_variables.get("self"))
                                {
                                    if let Some(fields) = self.classes.get(class_name) {
                                        if let Some(f) = fields.iter().find(|f| f.name == *field) {
                                            self.analyzer.resolve_type(&f.type_).map_err(|e| {
                                                format!("Type resolution failed: {}", e)
                                            })?
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
                                self.analyzer
                                    .get_expression_type(&match_expr)
                                    .map_err(|e| format!("Type inference failed: {}", e))?
                            }
                        } else {
                            self.analyzer
                                .get_expression_type(&match_expr)
                                .map_err(|e| format!("Type inference failed: {}", e))?
                        }
                    }
                    _ => self
                        .analyzer
                        .get_expression_type(&match_expr)
                        .map_err(|e| format!("Type inference failed: {}", e))?,
                };

                let enum_name = match &match_expr.kind {
                    ExpressionKind::Identifier(name) => {
                        // first check if this is a temporary variable created for complex expressions
                        if name.starts_with("match_temp_") {
                            if let Some((_, _, var_type)) = self.variables.get(name) {
                                match var_type {
                                    Type::Named(n, _) => n.clone(),
                                    Type::Optional(_) => "Optional".to_string(),
                                    _ => {
                                        return Err(format!(
                                            "Match expression must be an enum type, got {:?}",
                                            var_type
                                        ));
                                    }
                                }
                            } else {
                                return Err(format!("Temporary variable {} not found", name));
                            }
                        } else if let Some(symbol) = self.analyzer.symbol_table().lookup(name) {
                            if let Some(symbol_type) = &symbol.type_ {
                                match symbol_type {
                                    Type::Named(n, _) => n.clone(),
                                    Type::Optional(_) => "Optional".to_string(),
                                    _ => {
                                        return Err(
                                            "Match expression must be an enum type".to_string()
                                        );
                                    }
                                }
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
                                // handle self.field
                                if let Some((_, _, Type::Named(class_name, _))) = self
                                    .variables
                                    .get("self")
                                    .or_else(|| self.global_variables.get("self"))
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
                                // handle obj.field where obj is not self
                                if let Some((_, _, var_type)) = self
                                    .variables
                                    .get(obj)
                                    .or_else(|| self.global_variables.get(obj))
                                {
                                    if let Type::Named(class_name, _) = var_type {
                                        if let Some(fields) = self.classes.get(class_name) {
                                            if let Some(f) =
                                                fields.iter().find(|f| f.name == *field)
                                            {
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
                                        return Err(format!(
                                            "Variable {} is not a class instance",
                                            obj
                                        ));
                                    }
                                } else {
                                    return Err(format!("Variable {} not found", obj));
                                }
                            }
                        } else {
                            return Err(
                                "Match expression must be identifier, self.field, or obj.field"
                                    .to_string(),
                            );
                        }
                    }
                    ExpressionKind::Call { func, .. } => {
                        // handle constructor calls like Some(15), None, etc.
                        if let ExpressionKind::Identifier(constructor_name) = &func.kind {
                            // map common constructor names to their enum types
                            match constructor_name.as_str() {
                                "Some" | "None" => "Optional".to_string(),
                                "Ok" | "Err" => "Result".to_string(),
                                _ => {
                                    // for other constructors, try to look up as enum type
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
                    // for Optional/Result constructor calls, we need to allocate the struct and get a pointer
                    if expr_val.is_pointer_value() {
                        Some(expr_val.into_pointer_value())
                    } else {
                        // this is a struct value (from constructor call), allocate it and get pointer
                        let struct_val = expr_val.into_struct_value();
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

                // get discriminant using centralized function
                let discriminant = self.load_enum_discriminant(&enum_name, expr_val)?;

                // create temporary pointer for field extraction in match arms
                let temp_ptr_opt = if expr_val.is_pointer_value() {
                    Some(expr_val.into_pointer_value())
                } else {
                    // for struct values, allocate temporary storage
                    let struct_type = self
                        .type_map
                        .get(&enum_name)
                        .ok_or_else(|| format!("Enum {} not found in type map", enum_name))?
                        .into_struct_type();
                    let temp_ptr = self
                        .builder
                        .build_alloca(struct_type, "temp_enum_struct")
                        .map_err(|e| e.to_string())?;
                    self.builder
                        .build_store(temp_ptr, expr_val)
                        .map_err(|e| e.to_string())?;
                    Some(temp_ptr)
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
                            self.build_discriminant_comparison(discriminant, variant_index)?
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

                    // arm body
                    self.builder.position_at_end(arm_bb);

                    // bind variables
                    if let PatternNode::EnumVariant { name, args } = &arm.pattern {
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

                                    // extract the actual value based on the variant type
                                    let (data_val, resolved_type) = if enum_name == "Optional" {
                                        if let Type::Optional(inner_type) = &match_expr_type {
                                            self.extract_value_from_ptr(
                                                data_ptr, inner_type, "Some",
                                            )?
                                        } else {
                                            return Err(format!(
                                                "Type mismatch: expected Optional, got {:?}",
                                                match_expr_type
                                            ));
                                        }
                                    } else if enum_name == "Result" {
                                        if let Type::Named(_, generics) = &match_expr_type {
                                            if generics.len() != 2 {
                                                return Err(format!(
                                                    "Result must have 2 type parameters, got {}",
                                                    generics.len()
                                                ));
                                            }
                                            let target_type = if *name == "Ok" {
                                                &generics[0]
                                            } else {
                                                &generics[1]
                                            };
                                            let variant_name =
                                                if *name == "Ok" { "Ok" } else { "Err" };
                                            self.extract_value_from_ptr(
                                                data_ptr,
                                                target_type,
                                                variant_name,
                                            )?
                                        } else {
                                            return Err(format!(
                                                "Type mismatch: expected Result, got {:?}",
                                                match_expr_type
                                            ));
                                        }
                                    } else {
                                        return Err(format!(
                                            "Unknown enum {} for value extraction",
                                            enum_name
                                        ));
                                    };

                                    let boxed = self.box_value(data_val);
                                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                                    let alloca = self.create_entry_alloca(ptr_type.into(), var)?;
                                    self.builder
                                        .build_store(alloca, boxed)
                                        .map_err(|e| e.to_string())?;

                                    self.variables.insert(
                                        var.clone(),
                                        (alloca, ptr_type.into(), resolved_type),
                                    );
                                }
                            }
                        } else {
                            // custom enum - use variant-specific field information
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
                                    let data_index = i + 1; // start after discriminant at index 0
                                    let data_ptr = self
                                        .builder
                                        .build_struct_gep(
                                            struct_type,
                                            temp_ptr_opt.unwrap(),
                                            data_index as u32,
                                            "data_ptr",
                                        )
                                        .map_err(|e| e.to_string())?;

                                    // get the actual field type to load correctly
                                    let field_type: BasicTypeEnum<'_> = if i < field_types_clone
                                        .len()
                                    {
                                        self.type_kind_to_llvm_type(&field_types_clone[i].kind)?
                                    } else {
                                        return Err(format!(
                                                "Field index {} out of bounds for enum variant {}.{} (has {} fields)",
                                                i, enum_name, name, field_types_clone.len()
                                            ));
                                    };

                                    let data_val = self
                                        .builder
                                        .build_load(field_type, data_ptr, "data")
                                        .map_err(|e| e.to_string())?;
                                    let boxed = self.box_value(data_val);
                                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                                    let alloca = self
                                        .builder
                                        .build_alloca(ptr_type, var)
                                        .map_err(|e| e.to_string())?;
                                    self.builder
                                        .build_store(alloca, boxed)
                                        .map_err(|e| e.to_string())?;

                                    // use the actual field type for resolved type
                                    let resolved_type = if i < field_types_clone.len() {
                                        self.analyzer
                                            .resolve_type(&field_types_clone[i])
                                            .map_err(|e| e.to_string())?
                                    } else {
                                        return Err(format!(
                                            "Field index {} out of bounds for enum variant {}.{} during type resolution (has {} fields)",
                                            i, enum_name, name, field_types_clone.len()
                                        ));
                                    };

                                    self.variables.insert(
                                        var.clone(),
                                        (alloca, ptr_type.into(), resolved_type),
                                    );
                                }
                            }
                        }
                    }

                    // check guard
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

                // clean up temporary variables created for complex match expressions
                if let ExpressionKind::Identifier(temp_name) = &match_expr.kind {
                    if temp_name.starts_with("match_temp_") {
                        self.variables.remove(temp_name);
                    }
                }
            }
            StatementKind::Expression(expr) => {
                if let ExpressionKind::Identifier(_name) = &expr.kind {
                } else if let ExpressionKind::Call { func, args: _args } = &expr.kind {
                    if let ExpressionKind::Identifier(_name) = &func.kind {}
                }
                self.generate_expression(expr)?;
            }
            _ => {} // skip other statement types for now
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
        let resolved_obj_type = self.resolve_type(obj_type).map_err(|e| {
            format!(
                "Unresolved receiver type for method '{}': {}",
                method_name, e
            )
        })?;

        match &resolved_obj_type {
            Type::Primitive(prim) => {
                self.generate_primitive_method_call(obj_value, prim, method_name)
            }
            Type::List(_) => self.generate_list_method_call(obj_value, method_name, args),
            Type::Map(_, _) => self.generate_map_method_call(obj_value, method_name, args),
            Type::Set(_) => self.generate_set_method_call(obj_value, method_name, args),
            Type::Named(name, type_args) => {
                if let Some(class) = self.analyzer.symbol_table().lookup(name) {
                    if let Some(method) = class.methods.get(method_name) {
                        if method.is_static {
                            return Err(format!(
                                "Cannot call static method {} on instance",
                                method_name
                            ));
                        }

                        // generate instance method call - prioritize specialized methods
                        let specialized_method_name =
                            self.create_specialized_method_name(name, type_args, method_name);
                        let method_func_name =
                            if self.module.get_function(&specialized_method_name).is_some() {
                                specialized_method_name.clone()
                            } else {
                                format!("{}.{}", name, method_name)
                            };

                        // Specialized methods (containing '$') expect boxed parameters
                        // Non-specialized methods expect concrete types
                        let is_specialized = method_func_name.contains('$');

                        let mut call_args = vec![obj_value.into()]; // self
                        for arg in args {
                            let arg_val = self.generate_expression(arg)?;
                            if is_specialized {
                                call_args.push(self.box_value(arg_val).into());
                            } else {
                                call_args.push(arg_val.into());
                            }
                        }
                        let call = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function(&method_func_name)
                                    .ok_or(format!("Method '{}' not found", method_func_name))?,
                                &call_args,
                                &format!("{}_call", method_func_name.replace('.', "_")),
                            )
                            .map_err(|e| e.to_string())?;
                        match call.try_as_basic_value().left() {
                            Some(value) => Ok(value),
                            None => {
                                // check if method returns void
                                if method.return_type == Type::Void {
                                    // void method - this is expected, return a placeholder
                                    // the method was executed, we just don't have a return value
                                    Ok(self.context.i32_type().const_int(0, false).into())
                                } else {
                                    // non-void method returning None - this is an error
                                    Err("Method call failed to return value".to_string())
                                }
                            }
                        }
                    } else {
                        Err(format!(
                            "Method {} not found on class {}",
                            method_name, name
                        ))
                    }
                } else {
                    Err(format!("Class {} not found", name))
                }
            }
            Type::Optional(_) => self.generate_optional_method_call(obj_value, method_name, args),
            _ => Err(format!(
                "Method {} not implemented for type {:?}",
                method_name, resolved_obj_type
            )),
        }
    }

    fn generate_primitive_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        prim: &PrimitiveType,
        method_name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        match prim {
            PrimitiveType::Int => match method_name {
                "to_string" => {
                    let func = self
                        .module
                        .get_function("mux_int_to_string")
                        .ok_or("mux_int_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "int_to_str")
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
                        .get_function("mux_float_to_string")
                        .ok_or("mux_float_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "float_to_str")
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
                    let bool_value: BasicValueEnum<'a>;
                    if obj_value.is_int_value() {
                        // already a primitive boolean (i1), convert to i32 for mux_bool_to_string
                        let i1_val = obj_value.into_int_value();
                        bool_value = self
                            .builder
                            .build_int_z_extend(i1_val, self.context.i32_type(), "i1_to_i32")
                            .map_err(|e| e.to_string())?
                            .into();
                    } else if obj_value.is_pointer_value() {
                        // boxed boolean, extract the value
                        let extract_func = self
                            .module
                            .get_function("mux_value_get_bool")
                            .ok_or("mux_value_get_bool not found")?;
                        bool_value = self
                            .builder
                            .build_call(extract_func, &[obj_value.into()], "extract_bool")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;
                    } else {
                        return Err("Invalid boolean value type".to_string());
                    }

                    // call mux_bool_to_string with the i32 value
                    let func = self
                        .module
                        .get_function("mux_bool_to_string")
                        .ok_or("mux_bool_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[bool_value.into()], "bool_to_str")
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
                    let func = self
                        .module
                        .get_function("mux_float_to_string")
                        .ok_or("mux_float_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "float_to_str")
                        .map_err(|e| e.to_string())?;
                    let func_new = self
                        .module
                        .get_function("mux_new_string_from_cstr")
                        .ok_or("mux_new_string_from_cstr not found")?;
                    // handle pointer return value
                    let call2 = self
                        .builder
                        .build_call(
                            func_new,
                            &[call.try_as_basic_value().left().unwrap().into()],
                            "new_str",
                        )
                        .map_err(|e| e.to_string())?;
                    // mux_new_string_from_cstr returns a pointer
                    let result = call2.try_as_basic_value().left().unwrap();
                    Ok(result)
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
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "get" => {
                if args.len() != 1 {
                    return Err("get() method takes exactly 1 argument".to_string());
                }
                let index_val = self.generate_expression(&args[0])?;

                // extract raw List pointer from Value (same as direct access)
                let raw_list = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_list").unwrap(),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_get").unwrap(),
                        &[
                            raw_list.try_as_basic_value().left().unwrap().into(),
                            index_val.into(),
                        ],
                        "list_get",
                    )
                    .map_err(|e| e.to_string())?;
                // box the Optional as a Value
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
                    "mux_list_push_back_value",
                    &[obj_value.into(), elem_ptr.into()],
                );
                Ok(self.context.i32_type().const_int(0, false).into()) // return dummy value
            }
            "pop_back" => {
                if !args.is_empty() {
                    return Err("pop_back() method takes no arguments".to_string());
                }

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_pop_back_value").unwrap(),
                        &[obj_value.into()],
                        "list_pop_back",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "push" => {
                if args.len() != 1 {
                    return Err("push() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);

                self.generate_runtime_call(
                    "mux_list_push_value",
                    &[obj_value.into(), elem_ptr.into()],
                );
                Ok(self.context.i32_type().const_int(0, false).into()) // return dummy value
            }
            "pop" => {
                if !args.is_empty() {
                    return Err("pop() method takes no arguments".to_string());
                }

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_pop_value").unwrap(),
                        &[obj_value.into()],
                        "list_pop",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "push_front" => {
                if args.len() != 1 {
                    return Err("push_front() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);

                self.generate_runtime_call(
                    "mux_list_push_front_value",
                    &[obj_value.into(), elem_ptr.into()],
                );
                Ok(self.context.i32_type().const_int(0, false).into()) // return dummy value
            }
            "pop_front" => {
                if !args.is_empty() {
                    return Err("pop_front() method takes no arguments".to_string());
                }

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_pop_front_value")
                            .unwrap(),
                        &[obj_value.into()],
                        "list_pop_front",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }

                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_list").unwrap(),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_is_empty").unwrap(),
                        &[raw_list.try_as_basic_value().left().unwrap().into()],
                        "list_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "length" => {
                if !args.is_empty() {
                    return Err("length() method takes no arguments".to_string());
                }

                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_list").unwrap(),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_length").unwrap(),
                        &[raw_list.try_as_basic_value().left().unwrap().into()],
                        "list_length",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_list").unwrap(),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_list_to_string").unwrap(),
                        &[raw_list.try_as_basic_value().left().unwrap().into()],
                        "list_to_str",
                    )
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
            _ => Err(format!("Method {} not implemented for lists", method_name)),
        }
    }

    fn generate_map_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
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
                let extract_map = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_map").unwrap(),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_put").unwrap(),
                        &[extract_map.into(), key_val.into(), value_val.into()],
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
                let extract_map = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_map").unwrap(),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let map_get_result = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_get").unwrap(),
                        &[extract_map.into(), key_val.into()],
                        "map_get",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let optional_value = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_from_optional").unwrap(),
                        &[map_get_result.into()],
                        "optional_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Ok(optional_value)
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let extract_map = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_map").unwrap(),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_contains").unwrap(),
                        &[extract_map.into(), key_val.into()],
                        "map_contains",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_map").unwrap(),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_size").unwrap(),
                        &[extract_map.into()],
                        "map_size",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_map").unwrap(),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_is_empty").unwrap(),
                        &[extract_map.into()],
                        "map_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call.try_as_basic_value().left().unwrap())
            }
            "remove" => {
                if args.len() != 1 {
                    return Err("remove() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let extract_map = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_get_map").unwrap(),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let map_remove_result = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_map_remove").unwrap(),
                        &[extract_map.into(), key_val.into()],
                        "map_remove",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let optional_value = self
                    .builder
                    .build_call(
                        self.module.get_function("mux_value_from_optional").unwrap(),
                        &[map_remove_result.into()],
                        "optional_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Ok(optional_value)
            }
            _ => Err(format!("Method {} not implemented for maps", method_name)),
        }
    }

    fn generate_set_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
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
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                // use the standard mux_value_to_string function which handles Optional case
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
            _ => Err(format!(
                "Method {} not implemented for Optionals",
                method_name
            )),
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
                // Generate i1 (bool) value directly, like int/float literals
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
                Ok(call)
            }
            LiteralNode::Char(c) => {
                // Chars are stored as i64 for compatibility with int comparisons
                let val = self.context.i64_type().const_int(*c as u64, false);
                Ok(val.into())
            }
        }
    }

    fn generate_short_circuit_logical_op(
        &mut self,
        left_expr: &ExpressionNode,
        op: &BinaryOp,
        right_expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        // Get the current function from the current basic block
        let current_bb = self
            .builder
            .get_insert_block()
            .ok_or("No current basic block for short-circuit logical operation")?;
        let current_fn = current_bb
            .get_parent()
            .ok_or("No current function for short-circuit logical operation")?;

        match op {
            BinaryOp::LogicalAnd => {
                // Create basic blocks for control flow
                let eval_right_bb = self
                    .context
                    .append_basic_block(current_fn, "and_eval_right");
                let merge_bb = self.context.append_basic_block(current_fn, "and_merge");

                // Evaluate left operand
                let left_val = self.generate_expression(left_expr)?;
                let left_bool = self.get_raw_bool_value(left_val)?;
                let left_bb = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block after evaluating left operand")?;

                // If left is false, skip to merge with false result
                // If left is true, evaluate right operand
                self.builder
                    .build_conditional_branch(left_bool, eval_right_bb, merge_bb)
                    .map_err(|e| e.to_string())?;

                // eval_right_bb: evaluate right operand
                self.builder.position_at_end(eval_right_bb);
                let right_val = self.generate_expression(right_expr)?;
                let right_bool = self.get_raw_bool_value(right_val)?;
                let right_bb = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block after evaluating right operand")?;
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| e.to_string())?;

                // merge_bb: phi node combines results
                self.builder.position_at_end(merge_bb);
                let phi = self
                    .builder
                    .build_phi(self.context.bool_type(), "and_result")
                    .map_err(|e| e.to_string())?;

                let false_val = self.context.bool_type().const_zero();
                phi.add_incoming(&[
                    (&false_val, left_bb),   // Left was false, return false
                    (&right_bool, right_bb), // Left was true, return right
                ]);

                Ok(phi.as_basic_value())
            }
            BinaryOp::LogicalOr => {
                // Create basic blocks for control flow
                let eval_right_bb = self.context.append_basic_block(current_fn, "or_eval_right");
                let merge_bb = self.context.append_basic_block(current_fn, "or_merge");

                // Evaluate left operand
                let left_val = self.generate_expression(left_expr)?;
                let left_bool = self.get_raw_bool_value(left_val)?;
                let left_bb = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block after evaluating left operand")?;

                // If left is true, skip to merge with true result
                // If left is false, evaluate right operand
                self.builder
                    .build_conditional_branch(left_bool, merge_bb, eval_right_bb)
                    .map_err(|e| e.to_string())?;

                // eval_right_bb: evaluate right operand
                self.builder.position_at_end(eval_right_bb);
                let right_val = self.generate_expression(right_expr)?;
                let right_bool = self.get_raw_bool_value(right_val)?;
                let right_bb = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block after evaluating right operand")?;
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| e.to_string())?;

                // merge_bb: phi node combines results
                self.builder.position_at_end(merge_bb);
                let phi = self
                    .builder
                    .build_phi(self.context.bool_type(), "or_result")
                    .map_err(|e| e.to_string())?;

                let true_val = self.context.bool_type().const_int(1, false);
                phi.add_incoming(&[
                    (&true_val, left_bb),    // Left was true, return true
                    (&right_bool, right_bb), // Left was false, return right
                ]);

                Ok(phi.as_basic_value())
            }
            _ => Err(
                "generate_short_circuit_logical_op called with non-logical operator".to_string(),
            ),
        }
    }

    fn generate_binary_op(
        &mut self,
        left_expr: &ExpressionNode,
        left: BasicValueEnum<'a>,
        op: &BinaryOp,
        _right_expr: &ExpressionNode,
        right: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        match op {
            BinaryOp::Add => {
                // Get the semantic type to determine what kind of addition to perform
                let left_type = self
                    .analyzer
                    .get_expression_type(left_expr)
                    .map_err(|e| format!("Failed to get left operand type: {}", e))?;

                // Semantics already validated both types are the same, so just check left
                match &left_type {
                    // String concatenation
                    Type::Primitive(PrimitiveType::Str) => {
                        let left_ptr = if left.is_pointer_value() {
                            left.into_pointer_value()
                        } else {
                            self.box_value(left)
                        };
                        let right_ptr = if right.is_pointer_value() {
                            right.into_pointer_value()
                        } else {
                            self.box_value(right)
                        };

                        let left_cstr = self.extract_c_string_from_value(left_ptr)?;
                        let right_cstr = self.extract_c_string_from_value(right_ptr)?;

                        let concat_fn = self
                            .module
                            .get_function("mux_string_concat")
                            .ok_or("mux_string_concat not found")?;
                        let result = self
                            .builder
                            .build_call(
                                concat_fn,
                                &[left_cstr.into(), right_cstr.into()],
                                "string_concat",
                            )
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?
                            .into_pointer_value();

                        self.box_string_value(result)
                    }

                    // List concatenation
                    Type::List(_) => {
                        // Extract List pointers from Value wrappers
                        let left_list = self.extract_list_from_value(left.into_pointer_value())?;
                        let right_list =
                            self.extract_list_from_value(right.into_pointer_value())?;

                        // Call mux_list_concat
                        let concat_fn = self
                            .module
                            .get_function("mux_list_concat")
                            .ok_or("mux_list_concat not found")?;
                        let result_list = self
                            .builder
                            .build_call(
                                concat_fn,
                                &[left_list.into(), right_list.into()],
                                "list_concat",
                            )
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?
                            .into_pointer_value();

                        // Wrap in Value
                        let list_value_fn = self
                            .module
                            .get_function("mux_list_value")
                            .ok_or("mux_list_value not found")?;
                        let result = self
                            .builder
                            .build_call(list_value_fn, &[result_list.into()], "list_value")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        Ok(result)
                    }

                    // Map merge
                    Type::Map(_, _) => {
                        let left_map = self.extract_map_from_value(left.into_pointer_value())?;
                        let right_map = self.extract_map_from_value(right.into_pointer_value())?;

                        let merge_fn = self
                            .module
                            .get_function("mux_map_merge")
                            .ok_or("mux_map_merge not found")?;
                        let result_map = self
                            .builder
                            .build_call(merge_fn, &[left_map.into(), right_map.into()], "map_merge")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?
                            .into_pointer_value();

                        let map_value_fn = self
                            .module
                            .get_function("mux_map_value")
                            .ok_or("mux_map_value not found")?;
                        let result = self
                            .builder
                            .build_call(map_value_fn, &[result_map.into()], "map_value")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        Ok(result)
                    }

                    // Set union
                    Type::Set(_) => {
                        let left_set = self.extract_set_from_value(left.into_pointer_value())?;
                        let right_set = self.extract_set_from_value(right.into_pointer_value())?;

                        let union_fn = self
                            .module
                            .get_function("mux_set_union")
                            .ok_or("mux_set_union not found")?;
                        let result_set = self
                            .builder
                            .build_call(union_fn, &[left_set.into(), right_set.into()], "set_union")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?
                            .into_pointer_value();

                        let set_value_fn = self
                            .module
                            .get_function("mux_set_value")
                            .ok_or("mux_set_value not found")?;
                        let result = self
                            .builder
                            .build_call(set_value_fn, &[result_set.into()], "set_value")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        Ok(result)
                    }

                    // Numeric addition (int)
                    Type::Primitive(PrimitiveType::Int) => {
                        let left_int = self.get_raw_int_value(left)?;
                        let right_int = self.get_raw_int_value(right)?;
                        self.builder
                            .build_int_add(left_int, right_int, "add")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }

                    // Numeric addition (float)
                    Type::Primitive(PrimitiveType::Float) => {
                        let left_float = self.get_raw_float_value(left)?;
                        let right_float = self.get_raw_float_value(right)?;
                        self.builder
                            .build_float_add(left_float, right_float, "fadd")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }

                    _ => Err(format!(
                        "Add operation not supported for type: {:?}",
                        left_type
                    )),
                }
            }
            BinaryOp::Subtract => {
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_sub(left_int, right_int, "sub")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
                    self.builder
                        .build_float_sub(left_float, right_float, "fsub")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported sub operands".to_string())
                }
            }
            BinaryOp::Multiply => {
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_mul(left_int, right_int, "mul")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
                    let result = self
                        .builder
                        .build_float_mul(left_float, right_float, "fmul")
                        .map_err(|e| e.to_string())?;
                    Ok(result.into())
                } else {
                    Err("Unsupported mul operands".to_string())
                }
            }
            BinaryOp::Divide => {
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_signed_div(left_int, right_int, "div")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
                    self.builder
                        .build_float_div(left_float, right_float, "fdiv")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else {
                    Err("Unsupported div operands".to_string())
                }
            }
            BinaryOp::Equal => {
                // Get the semantic type to determine what kind of comparison to perform
                let left_type = self
                    .analyzer
                    .get_expression_type(left_expr)
                    .map_err(|e| format!("Failed to get left operand type: {}", e))?;

                match &left_type {
                    // String comparison
                    Type::Primitive(PrimitiveType::Str) => {
                        let left_ptr = if left.is_pointer_value() {
                            left.into_pointer_value()
                        } else {
                            self.box_value(left)
                        };
                        let right_ptr = if right.is_pointer_value() {
                            right.into_pointer_value()
                        } else {
                            self.box_value(right)
                        };

                        let left_cstr = self.extract_c_string_from_value(left_ptr)?;
                        let right_cstr = self.extract_c_string_from_value(right_ptr)?;

                        let equal_fn = self
                            .module
                            .get_function("mux_string_equal")
                            .ok_or("mux_string_equal not found")?;
                        let result = self
                            .builder
                            .build_call(
                                equal_fn,
                                &[left_cstr.into(), right_cstr.into()],
                                "string_equal",
                            )
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        // Convert i32 result to i1 bool
                        let result_i32 = result.into_int_value();
                        let zero = self.context.i32_type().const_zero();
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                result_i32,
                                zero,
                                "to_bool",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // Int/Char comparison
                    Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Char) => {
                        let left_int = self.get_raw_int_value(left)?;
                        let right_int = self.get_raw_int_value(right)?;
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::EQ, left_int, right_int, "eq")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // Bool comparison
                    Type::Primitive(PrimitiveType::Bool) => {
                        let left_bool = self.get_raw_bool_value(left)?;
                        let right_bool = self.get_raw_bool_value(right)?;
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::EQ,
                                left_bool,
                                right_bool,
                                "eq",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // Float comparison
                    Type::Primitive(PrimitiveType::Float) => {
                        let left_float = self.get_raw_float_value(left)?;
                        let right_float = self.get_raw_float_value(right)?;
                        self.builder
                            .build_float_compare(
                                inkwell::FloatPredicate::OEQ,
                                left_float,
                                right_float,
                                "feq",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // List/Map/Set comparison using Value equality
                    Type::List(_)
                    | Type::Map(_, _)
                    | Type::Set(_)
                    | Type::EmptyList
                    | Type::EmptyMap
                    | Type::EmptySet => {
                        let left_ptr = if left.is_pointer_value() {
                            left.into_pointer_value()
                        } else {
                            self.box_value(left)
                        };
                        let right_ptr = if right.is_pointer_value() {
                            right.into_pointer_value()
                        } else {
                            self.box_value(right)
                        };

                        let equal_fn = self
                            .module
                            .get_function("mux_value_equal")
                            .ok_or("mux_value_equal not found")?;
                        let result = self
                            .builder
                            .build_call(
                                equal_fn,
                                &[left_ptr.into(), right_ptr.into()],
                                "value_equal",
                            )
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        // Convert i32 result to i1 bool
                        let result_i32 = result.into_int_value();
                        let zero = self.context.i32_type().const_zero();
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                result_i32,
                                zero,
                                "to_bool",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    _ => Err(format!(
                        "Equality comparison not supported for type: {:?}",
                        left_type
                    )),
                }
            }
            BinaryOp::Less => {
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::SLT, left_int, right_int, "lt")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
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
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::SGT, left_int, right_int, "gt")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
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
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::SLE, left_int, right_int, "le")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
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
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::SGE, left_int, right_int, "ge")
                        .map_err(|e| e.to_string())
                        .map(|v| v.into())
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
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
                // Get the semantic type to determine what kind of comparison to perform
                let left_type = self
                    .analyzer
                    .get_expression_type(left_expr)
                    .map_err(|e| format!("Failed to get left operand type: {}", e))?;

                match &left_type {
                    // String comparison
                    Type::Primitive(PrimitiveType::Str) => {
                        let left_ptr = if left.is_pointer_value() {
                            left.into_pointer_value()
                        } else {
                            self.box_value(left)
                        };
                        let right_ptr = if right.is_pointer_value() {
                            right.into_pointer_value()
                        } else {
                            self.box_value(right)
                        };

                        let left_cstr = self.extract_c_string_from_value(left_ptr)?;
                        let right_cstr = self.extract_c_string_from_value(right_ptr)?;

                        let not_equal_fn = self
                            .module
                            .get_function("mux_string_not_equal")
                            .ok_or("mux_string_not_equal not found")?;
                        let result = self
                            .builder
                            .build_call(
                                not_equal_fn,
                                &[left_cstr.into(), right_cstr.into()],
                                "string_not_equal",
                            )
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        // Convert i32 result to i1 bool
                        let result_i32 = result.into_int_value();
                        let zero = self.context.i32_type().const_zero();
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                result_i32,
                                zero,
                                "to_bool",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // Int/Char comparison
                    Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Char) => {
                        let left_int = self.get_raw_int_value(left)?;
                        let right_int = self.get_raw_int_value(right)?;
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::NE, left_int, right_int, "ne")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // Bool comparison
                    Type::Primitive(PrimitiveType::Bool) => {
                        let left_bool = self.get_raw_bool_value(left)?;
                        let right_bool = self.get_raw_bool_value(right)?;
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                left_bool,
                                right_bool,
                                "ne",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // Float comparison
                    Type::Primitive(PrimitiveType::Float) => {
                        let left_float = self.get_raw_float_value(left)?;
                        let right_float = self.get_raw_float_value(right)?;
                        self.builder
                            .build_float_compare(
                                inkwell::FloatPredicate::ONE,
                                left_float,
                                right_float,
                                "fne",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    // List/Map/Set comparison using Value inequality
                    Type::List(_)
                    | Type::Map(_, _)
                    | Type::Set(_)
                    | Type::EmptyList
                    | Type::EmptyMap
                    | Type::EmptySet => {
                        let left_ptr = if left.is_pointer_value() {
                            left.into_pointer_value()
                        } else {
                            self.box_value(left)
                        };
                        let right_ptr = if right.is_pointer_value() {
                            right.into_pointer_value()
                        } else {
                            self.box_value(right)
                        };

                        let not_equal_fn = self
                            .module
                            .get_function("mux_value_not_equal")
                            .ok_or("mux_value_not_equal not found")?;
                        let result = self
                            .builder
                            .build_call(
                                not_equal_fn,
                                &[left_ptr.into(), right_ptr.into()],
                                "value_not_equal",
                            )
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or("Call returned no value")?;

                        // Convert i32 result to i1 bool
                        let result_i32 = result.into_int_value();
                        let zero = self.context.i32_type().const_zero();
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                result_i32,
                                zero,
                                "to_bool",
                            )
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
                    _ => Err(format!(
                        "Inequality comparison not supported for type: {:?}",
                        left_type
                    )),
                }
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                // These should be handled by generate_short_circuit_logical_op
                // and should not reach here
                Err("Logical AND/OR should use short-circuit evaluation".to_string())
            }
            BinaryOp::Modulo => {
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
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
                // for now, return false as a placeholder implementation
                // a complete implementation would require runtime functions for:
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

    fn llvm_type_from_resolved_type(
        &self,
        resolved_type: &ResolvedType,
    ) -> Result<BasicTypeEnum<'a>, String> {
        match resolved_type {
            ResolvedType::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            ResolvedType::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            ResolvedType::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            ResolvedType::Primitive(PrimitiveType::Str) => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            ResolvedType::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),
            ResolvedType::Primitive(PrimitiveType::Void) => {
                Err("Void type not allowed here".to_string())
            }
            ResolvedType::Void => Err("Void type not allowed here".to_string()),
            ResolvedType::Primitive(PrimitiveType::Auto) => {
                Err("Auto type should be resolved".to_string())
            }
            ResolvedType::Named(name, args) => {
                if args.is_empty() {
                    if let Some(context) = &self.generic_context {
                        if let Some(concrete) = context.type_params.get(name) {
                            return self.llvm_type_from_resolved_type(concrete);
                        }
                    }
                }

                if self.classes.contains_key(name) {
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                } else {
                    Err(format!("Unknown type: {}", name))
                }
            }
            ResolvedType::Function {
                params: _,
                returns: _,
            } => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::List(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::Map(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::Set(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::Optional(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::Reference(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::Tuple(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::EmptyList => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::EmptyMap => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::EmptySet => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            ResolvedType::Generic(_) => Err("Generic types should be resolved".to_string()),
            ResolvedType::Instantiated(_, _) => {
                Err("Instantiated types should be resolved".to_string())
            }
            ResolvedType::Variable(name) => {
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.llvm_type_from_resolved_type(concrete);
                    }
                }
                Err(format!("Variable type '{}' should be resolved", name))
            }
            ResolvedType::Never => Err("Never type not allowed here".to_string()),
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
            TypeKind::Named(name, _) => {
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.llvm_type_from_resolved_type(concrete);
                    }
                }

                if self.enum_variants.contains_key(name) {
                    if name == "Optional" || name == "Result" {
                        // for Optional/Result: values are always Value* pointers
                        Ok(self.context.ptr_type(AddressSpace::default()).into())
                    } else {
                        // for custom enums: values are struct values
                        let struct_type = self.type_map.get(name).ok_or("Enum type not found")?;
                        Ok(*struct_type)
                    }
                } else {
                    // for classes, values are pointers to structs
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                }
            }
            TypeKind::Reference(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
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
                // for now, all function types are generic pointers
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::TraitObject(_) => {
                // for now, trait objects are just pointers
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::Auto => Err("Auto type should be resolved".to_string()),
        }
    }

    /// Maps a TypeKind to its corresponding LLVM BasicTypeEnum
    /// Used for enum variant field extraction and storage
    /// Returns an error for unsupported or invalid types instead of falling back
    fn type_kind_to_llvm_type(&self, type_kind: &TypeKind) -> Result<BasicTypeEnum<'a>, String> {
        match type_kind {
            // Primitive types
            TypeKind::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            TypeKind::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            TypeKind::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            TypeKind::Primitive(PrimitiveType::Str) => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),

            // Named types (enums, classes)
            TypeKind::Named(name, _) => {
                // Check if it's a generic type parameter
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.llvm_type_from_resolved_type(concrete);
                    }
                }

                // Check if it's an enum
                if self.enum_variants.contains_key(name) {
                    if name == "Optional" || name == "Result" {
                        // Optional/Result are Value* pointers
                        Ok(self.context.ptr_type(AddressSpace::default()).into())
                    } else {
                        // Custom enums are struct values
                        let struct_type = self
                            .type_map
                            .get(name)
                            .ok_or_else(|| format!("Enum type {} not found in type map", name))?;
                        Ok(*struct_type)
                    }
                } else {
                    // Classes are pointers
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                }
            }

            // Collection types (all pointers)
            TypeKind::List(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Map(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Set(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Reference(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),

            // Function and trait types (pointers)
            TypeKind::Function { .. } => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::TraitObject(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),

            // Tuples - not supported in enum variants (migrating away from tuples)
            TypeKind::Tuple(_) => {
                Err("Tuple types are not supported in enum variant fields".to_string())
            }

            // Invalid types
            TypeKind::Primitive(PrimitiveType::Void) => {
                Err("Void type cannot be used in enum variant fields".to_string())
            }
            TypeKind::Primitive(PrimitiveType::Auto) | TypeKind::Auto => {
                Err("Auto type should be resolved before codegen".to_string())
            }
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
            }, // should not happen
            Type::Never => TypeNode {
                kind: TypeKind::Auto, // use Auto as placeholder
                span: Span::new(0, 0),
            }, // should not happen
            Type::Generic(name) => TypeNode {
                kind: TypeKind::Named(name.clone(), vec![]),
                span: Span::new(0, 0),
            },
            Type::Instantiated(name, generics) => TypeNode {
                kind: TypeKind::Named(
                    name.clone(),
                    generics.iter().map(|g| self.type_to_type_node(g)).collect(),
                ),
                span: Span::new(0, 0),
            },
        }
    }

    fn type_node_to_type(&self, type_node: &TypeNode) -> Type {
        match &type_node.kind {
            TypeKind::Primitive(p) => Type::Primitive(p.clone()),
            TypeKind::List(inner) => Type::List(Box::new(self.type_node_to_type(inner))),
            TypeKind::Map(k, v) => Type::Map(
                Box::new(self.type_node_to_type(k)),
                Box::new(self.type_node_to_type(v)),
            ),
            TypeKind::Set(inner) => Type::Set(Box::new(self.type_node_to_type(inner))),
            TypeKind::Tuple(elements) => {
                Type::Tuple(elements.iter().map(|e| self.type_node_to_type(e)).collect())
            }
            TypeKind::TraitObject(_) => Type::Variable("trait_object".to_string()),

            TypeKind::Reference(inner) => Type::Reference(Box::new(self.type_node_to_type(inner))),
            TypeKind::Named(name, generics) => {
                if generics.is_empty() {
                    // special case for Pair.from method
                    if self.current_function_name == Some("Pair.from".to_string()) {
                        if name == "T" {
                            return Type::Named("string".to_string(), vec![]);
                        } else if name == "U" {
                            return Type::Primitive(PrimitiveType::Bool);
                        }
                    }

                    // check if this is a generic parameter by looking at the current context
                    if let Some(context) = &self.generic_context {
                        if let Some(concrete_type) = context.type_params.get(name) {
                            // return the concrete type directly
                            concrete_type.clone()
                        } else {
                            Type::Named(name.clone(), vec![])
                        }
                    } else {
                        Type::Named(name.clone(), vec![])
                    }
                } else {
                    Type::Named(
                        name.clone(),
                        generics.iter().map(|g| self.type_node_to_type(g)).collect(),
                    )
                }
            }
            TypeKind::Function { params, returns } => Type::Function {
                params: params.iter().map(|p| self.type_node_to_type(p)).collect(),
                returns: Box::new(self.type_node_to_type(returns)),
            },
            TypeKind::Auto => Type::Variable("auto".to_string()),
        }
    }

    fn resolve_type(&self, type_: &Type) -> Result<Type, String> {
        match type_ {
            Type::Primitive(_)
            | Type::Void
            | Type::Never
            | Type::EmptyList
            | Type::EmptyMap
            | Type::EmptySet => Ok(type_.clone()),
            Type::Generic(name) | Type::Variable(name) => {
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.resolve_type(concrete);
                    }
                }
                Err(format!("Unresolved generic: {}", name))
            }
            Type::Named(name, type_args) => {
                if type_args.is_empty() {
                    if let Some(context) = &self.generic_context {
                        if let Some(concrete) = context.type_params.get(name) {
                            return self.resolve_type(concrete);
                        }
                    }
                    Ok(Type::Named(name.clone(), vec![]))
                } else {
                    let resolved_args = type_args
                        .iter()
                        .map(|arg| self.resolve_type(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Type::Named(name.clone(), resolved_args))
                }
            }
            Type::Instantiated(name, type_args) => {
                let resolved_args = type_args
                    .iter()
                    .map(|arg| self.resolve_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Instantiated(name.clone(), resolved_args))
            }
            Type::List(inner) => Ok(Type::List(Box::new(self.resolve_type(inner)?))),
            Type::Set(inner) => Ok(Type::Set(Box::new(self.resolve_type(inner)?))),
            Type::Map(k, v) => Ok(Type::Map(
                Box::new(self.resolve_type(k)?),
                Box::new(self.resolve_type(v)?),
            )),
            Type::Tuple(elements) => Ok(Type::Tuple(
                elements
                    .iter()
                    .map(|e| self.resolve_type(e))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Type::Optional(inner) => Ok(Type::Optional(Box::new(self.resolve_type(inner)?))),
            Type::Reference(inner) => Ok(Type::Reference(Box::new(self.resolve_type(inner)?))),
            Type::Function { params, returns } => Ok(Type::Function {
                params: params
                    .iter()
                    .map(|p| self.resolve_type(p))
                    .collect::<Result<Vec<_>, _>>()?,
                returns: Box::new(self.resolve_type(returns)?),
            }),
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
            let int_val = val.into_int_value();
            // Check if this is a bool (i1) - LLVM considers i1 as an int type
            if int_val.get_type().get_bit_width() == 1 {
                // Bool: extend i1 to i32 for mux_bool_value
                let i32_val = self
                    .builder
                    .build_int_z_extend(int_val, self.context.i32_type(), "bool_to_i32")
                    .unwrap();
                let call = self
                    .generate_runtime_call("mux_bool_value", &[i32_val.into()])
                    .unwrap();
                call.into_pointer_value()
            } else {
                // Regular int (i64)
                let call = self
                    .generate_runtime_call("mux_int_value", &[int_val.into()])
                    .unwrap();
                call.into_pointer_value()
            }
        } else if val.is_float_value() {
            let call = self
                .generate_runtime_call("mux_float_value", &[val.into()])
                .unwrap();
            call.into_pointer_value()
        } else if val.is_pointer_value() {
            // assume string or already boxed Value (from Map/Set/List literals)
            // map/Set/List literals already return *mut Value pointers, so just return as-is
            val.into_pointer_value()
        } else {
            panic!("Unexpected value type in box_value")
        }
    }

    fn get_raw_int_value(&mut self, val: BasicValueEnum<'a>) -> Result<IntValue<'a>, String> {
        if val.is_int_value() {
            Ok(val.into_int_value())
        } else if val.is_pointer_value() {
            // use safe runtime function to extract int
            let ptr = val.into_pointer_value();
            let get_int_fn = self
                .module
                .get_function("mux_value_get_int")
                .ok_or("mux_value_get_int not found")?;
            let result = self
                .builder
                .build_call(get_int_fn, &[ptr.into()], "get_int")
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
            // use safe runtime function to extract float
            let ptr = val.into_pointer_value();
            let get_float_fn = self
                .module
                .get_function("mux_value_get_float")
                .ok_or("mux_value_get_float not found")?;
            let result = self
                .builder
                .build_call(get_float_fn, &[ptr.into()], "get_float")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("Call returned no value")?;
            Ok(result.into_float_value())
        } else {
            Err("Expected float value or pointer".to_string())
        }
    }

    fn get_raw_bool_value(
        &mut self,
        val: BasicValueEnum<'a>,
    ) -> Result<inkwell::values::IntValue<'a>, String> {
        if val.is_int_value() {
            let int_val = val.into_int_value();
            // If already i1, return as-is. If i32 (from runtime), truncate to i1
            if int_val.get_type().get_bit_width() == 1 {
                Ok(int_val)
            } else {
                // Truncate i32 to i1
                let i1_val = self
                    .builder
                    .build_int_truncate(int_val, self.context.bool_type(), "trunc_to_i1")
                    .map_err(|e| e.to_string())?;
                Ok(i1_val)
            }
        } else if val.is_pointer_value() {
            // use safe runtime function to extract bool
            let ptr = val.into_pointer_value();
            let get_bool_fn = self
                .module
                .get_function("mux_value_get_bool")
                .ok_or("mux_value_get_bool not found")?;
            let i32_result = self
                .builder
                .build_call(get_bool_fn, &[ptr.into()], "get_bool")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("Call returned no value")?
                .into_int_value();
            // Truncate i32 to i1 so callers get consistent bool type
            let i1_val = self
                .builder
                .build_int_truncate(i32_result, self.context.bool_type(), "trunc_to_i1")
                .map_err(|e| e.to_string())?;
            Ok(i1_val)
        } else {
            Err("Expected bool value or pointer".to_string())
        }
    }

    /// Extracts a value from a *mut Value pointer based on the wrapped type.
    /// Used for unwrapping Optional<T> and Result<T, E> in match statements.
    ///
    /// # Arguments
    /// * `data_ptr` - Pointer to the wrapped value (*mut Value)
    /// * `wrapped_type` - The type of the value being unwrapped
    /// * `variant_name` - Name of the variant ("Some", "Ok", "Err") for error messages
    ///
    /// # Returns
    /// A tuple of (BasicValueEnum, Type) representing the extracted value and its type
    fn extract_value_from_ptr(
        &mut self,
        data_ptr: PointerValue<'a>,
        wrapped_type: &Type,
        variant_name: &str,
    ) -> Result<(BasicValueEnum<'a>, Type), String> {
        match wrapped_type {
            // Primitive types need to be extracted from *mut Value
            Type::Primitive(PrimitiveType::Int) => {
                let get_int_func = self
                    .module
                    .get_function("mux_value_get_int")
                    .ok_or(format!(
                        "Failed to extract int from {}: mux_value_get_int not found",
                        variant_name
                    ))?;
                let val = self
                    .builder
                    .build_call(get_int_func, &[data_ptr.into()], "get_int")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_value_get_int returned no value")?;
                Ok((val, Type::Primitive(PrimitiveType::Int)))
            }
            Type::Primitive(PrimitiveType::Float) => {
                let get_float_func =
                    self.module
                        .get_function("mux_value_get_float")
                        .ok_or(format!(
                            "Failed to extract float from {}: mux_value_get_float not found",
                            variant_name
                        ))?;
                let val = self
                    .builder
                    .build_call(get_float_func, &[data_ptr.into()], "get_float")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_value_get_float returned no value")?;
                Ok((val, Type::Primitive(PrimitiveType::Float)))
            }
            Type::Primitive(PrimitiveType::Bool) => {
                let get_bool_func =
                    self.module
                        .get_function("mux_value_get_bool")
                        .ok_or(format!(
                            "Failed to extract bool from {}: mux_value_get_bool not found",
                            variant_name
                        ))?;
                let val = self
                    .builder
                    .build_call(get_bool_func, &[data_ptr.into()], "get_bool")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_value_get_bool returned no value")?;
                Ok((val, Type::Primitive(PrimitiveType::Bool)))
            }
            Type::Primitive(PrimitiveType::Char) => {
                // Char is stored as int
                let get_int_func = self
                    .module
                    .get_function("mux_value_get_int")
                    .ok_or(format!(
                        "Failed to extract char from {}: mux_value_get_int not found",
                        variant_name
                    ))?;
                let val = self
                    .builder
                    .build_call(get_int_func, &[data_ptr.into()], "get_int")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_value_get_int returned no value")?;
                Ok((val, Type::Primitive(PrimitiveType::Char)))
            }
            Type::Primitive(PrimitiveType::Str) => {
                // String needs special handling: get C string then wrap in Mux string
                let get_string_func =
                    self.module
                        .get_function("mux_value_get_string")
                        .ok_or(format!(
                            "Failed to extract string from {}: mux_value_get_string not found",
                            variant_name
                        ))?;
                let c_str = self
                    .builder
                    .build_call(get_string_func, &[data_ptr.into()], "get_string")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_value_get_string returned no value")?
                    .into_pointer_value();

                // Wrap C string back into Mux string (*mut Value)
                let new_string_func = self
                    .module
                    .get_function("mux_new_string_from_cstr")
                    .ok_or("mux_new_string_from_cstr not found")?;
                let mux_string = self
                    .builder
                    .build_call(new_string_func, &[c_str.into()], "new_string")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_new_string_from_cstr returned no value")?;

                Ok((mux_string, Type::Primitive(PrimitiveType::Str)))
            }
            Type::Primitive(PrimitiveType::Void) => Err(format!(
                "Unsupported type Void for extraction from {}",
                variant_name
            )),
            Type::Primitive(PrimitiveType::Auto) => Err(format!(
                "Unsupported type Auto for extraction from {}",
                variant_name
            )),
            // Collections, custom types, and nested Optional/Result stay as *mut Value
            Type::List(_)
            | Type::Map(_, _)
            | Type::Set(_)
            | Type::Named(_, _)
            | Type::Optional(_)
            | Type::Tuple(_)
            | Type::Instantiated(_, _) => {
                // These are already *mut Value pointers, no extraction needed
                Ok((data_ptr.into(), wrapped_type.clone()))
            }
            // Reference types - unwrap the reference
            Type::Reference(inner) => self.extract_value_from_ptr(data_ptr, inner, variant_name),
            // Other types that shouldn't appear in Optional/Result
            Type::Void | Type::Never | Type::EmptyList | Type::EmptyMap | Type::EmptySet => {
                Err(format!(
                    "Unsupported type {:?} for extraction from {}",
                    wrapped_type, variant_name
                ))
            }
            Type::Function { .. } => Err(format!(
                "Unsupported type Function for extraction from {}",
                variant_name
            )),
            Type::Variable(v) | Type::Generic(v) => Err(format!(
                "Unresolved generic type {} for extraction from {}",
                v, variant_name
            )),
        }
    }

    fn extract_c_string_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        // call mux_value_get_string to extract C string from Value
        let get_string_fn = self
            .module
            .get_function("mux_value_get_string")
            .ok_or("mux_value_get_string not found")?;
        let cstr_ptr = self
            .builder
            .build_call(get_string_fn, &[value_ptr.into()], "get_string")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?
            .into_pointer_value();
        Ok(cstr_ptr)
    }

    fn box_string_value(
        &mut self,
        cstr_ptr: PointerValue<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        // call mux_value_from_string to create a Value from C string
        let from_string_fn = self
            .module
            .get_function("mux_value_from_string")
            .ok_or("mux_value_from_string not found")?;
        let value_ptr = self
            .builder
            .build_call(from_string_fn, &[cstr_ptr.into()], "from_string")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?;
        Ok(value_ptr)
    }

    fn extract_list_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        let get_list_fn = self
            .module
            .get_function("mux_value_get_list")
            .ok_or("mux_value_get_list not found")?;
        let list_ptr = self
            .builder
            .build_call(get_list_fn, &[value_ptr.into()], "get_list")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?
            .into_pointer_value();
        Ok(list_ptr)
    }

    fn extract_map_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        let get_map_fn = self
            .module
            .get_function("mux_value_get_map")
            .ok_or("mux_value_get_map not found")?;
        let map_ptr = self
            .builder
            .build_call(get_map_fn, &[value_ptr.into()], "get_map")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?
            .into_pointer_value();
        Ok(map_ptr)
    }

    fn extract_set_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        let get_set_fn = self
            .module
            .get_function("mux_value_get_set")
            .ok_or("mux_value_get_set not found")?;
        let set_ptr = self
            .builder
            .build_call(get_set_fn, &[value_ptr.into()], "get_set")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?
            .into_pointer_value();
        Ok(set_ptr)
    }

    fn initialize_field_by_type(
        &mut self,
        field_ptr: PointerValue<'a>,
        field_type: &Type,
        is_generic_param: bool,
    ) -> Result<(), String> {
        // generic parameter fields are boxed, initialize as null pointer
        if is_generic_param {
            let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
            self.builder
                .build_store(field_ptr, null_ptr)
                .map_err(|e| e.to_string())?;
            return Ok(());
        }

        let resolved_type = self.resolve_type(field_type)?;

        match resolved_type {
            Type::Primitive(PrimitiveType::Bool) => {
                let false_val = self.context.bool_type().const_int(0, false);
                self.builder
                    .build_store(field_ptr, false_val)
                    .map_err(|e| e.to_string())?;
            }
            Type::Primitive(PrimitiveType::Int) => {
                let zero_val = self.context.i64_type().const_int(0, false);
                self.builder
                    .build_store(field_ptr, zero_val)
                    .map_err(|e| e.to_string())?;
            }
            Type::Primitive(PrimitiveType::Float) => {
                let zero_val = self.context.f64_type().const_float(0.0);
                self.builder
                    .build_store(field_ptr, zero_val)
                    .map_err(|e| e.to_string())?;
            }
            Type::Primitive(PrimitiveType::Str) => {
                // initialize with null pointer (empty string)
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder
                    .build_store(field_ptr, null_ptr)
                    .map_err(|e| e.to_string())?;
            }
            Type::List(_) => {
                // initialize list fields with empty list
                let new_list_fn = self
                    .module
                    .get_function("mux_new_list")
                    .ok_or("mux_new_list function not found")?;
                let list_ptr = self
                    .builder
                    .build_call(new_list_fn, &[], "new_list")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let list_value_fn = self
                    .module
                    .get_function("mux_list_value")
                    .ok_or("mux_list_value function not found")?;
                let list_val = self
                    .builder
                    .build_call(list_value_fn, &[list_ptr.into()], "list_value")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                self.builder
                    .build_store(field_ptr, list_val)
                    .map_err(|e| e.to_string())?;
            }
            Type::Map(_, _) => {
                // initialize map fields with empty map
                let new_map_fn = self
                    .module
                    .get_function("mux_new_map")
                    .ok_or("mux_new_map function not found")?;
                let map_ptr = self
                    .builder
                    .build_call(new_map_fn, &[], "new_map")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let map_value_fn = self
                    .module
                    .get_function("mux_map_value")
                    .ok_or("mux_map_value function not found")?;
                let map_val = self
                    .builder
                    .build_call(map_value_fn, &[map_ptr.into()], "map_value")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                self.builder
                    .build_store(field_ptr, map_val)
                    .map_err(|e| e.to_string())?;
            }
            Type::Set(_) => {
                // initialize set fields with empty set
                let new_set_fn = self
                    .module
                    .get_function("mux_new_set")
                    .ok_or("mux_new_set function not found")?;
                let set_ptr = self
                    .builder
                    .build_call(new_set_fn, &[], "new_set")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let set_value_fn = self
                    .module
                    .get_function("mux_set_value")
                    .ok_or("mux_set_value function not found")?;
                let set_val = self
                    .builder
                    .build_call(set_value_fn, &[set_ptr.into()], "set_value")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                self.builder
                    .build_store(field_ptr, set_val)
                    .map_err(|e| e.to_string())?;
            }
            Type::Named(class_name, type_args) => {
                // handle built-in types
                if class_name == "string" && type_args.is_empty() {
                    // initialize string field with null pointer
                    let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                    self.builder
                        .build_store(field_ptr, null_ptr)
                        .map_err(|e| e.to_string())?;
                } else if class_name == "bool" && type_args.is_empty() {
                    // initialize bool field with false
                    let false_val = self.context.bool_type().const_int(0, false);
                    self.builder
                        .build_store(field_ptr, false_val)
                        .map_err(|e| e.to_string())?;
                } else {
                    // recursively call constructor for nested classes
                    let nested_obj =
                        self.generate_constructor_call_with_types(&class_name, &type_args, &[])?;
                    self.builder
                        .build_store(field_ptr, nested_obj)
                        .map_err(|e| e.to_string())?;
                }
            }
            _ => return Err(format!("Unsupported field type: {:?}", resolved_type)),
        }
        Ok(())
    }

    fn generate_constructor_call_with_types(
        &mut self,
        class_name: &str,
        type_args: &[Type],
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // create generic context for this instantiation
        let context = GenericContext {
            type_params: self.build_type_param_map(class_name, type_args)?,
        };

        // push context for recursive constructor calls
        self.context_stack.push(context.clone());
        self.generic_context = Some(context);

        // generate specialized methods for this class variant if not already generated
        if !type_args.is_empty() {
            self.generate_specialized_methods(class_name, type_args)?;
        }

        // generate constructor with context
        let result = self.generate_constructor_call(class_name, args);

        // pop context after call
        self.context_stack.pop();
        self.generic_context = self.context_stack.last().cloned();

        result
    }

    #[allow(clippy::only_used_in_recursion)]
    fn sanitize_type_name(&self, type_: &Type) -> String {
        match type_ {
            Type::Primitive(PrimitiveType::Int) => "int".to_string(),
            Type::Primitive(PrimitiveType::Float) => "float".to_string(),
            Type::Primitive(PrimitiveType::Bool) => "bool".to_string(),
            Type::Primitive(PrimitiveType::Str) => "string".to_string(),
            Type::Named(name, type_args) => {
                if type_args.is_empty() {
                    name.clone()
                } else {
                    let args_str = type_args
                        .iter()
                        .map(|arg| self.sanitize_type_name(arg))
                        .collect::<Vec<_>>()
                        .join("_");
                    format!("{}_{}", name, args_str)
                }
            }
            Type::Generic(name) | Type::Variable(name) => name.clone(),
            Type::List(inner) => format!("list_{}", self.sanitize_type_name(inner)),
            Type::Map(k, v) => format!(
                "map_{}_{}",
                self.sanitize_type_name(k),
                self.sanitize_type_name(v)
            ),
            Type::Set(inner) => format!("set_{}", self.sanitize_type_name(inner)),
            Type::Tuple(elements) => {
                let elements_str = elements
                    .iter()
                    .map(|e| self.sanitize_type_name(e))
                    .collect::<Vec<_>>()
                    .join("_");
                format!("tuple_{}", elements_str)
            }
            Type::Optional(inner) => format!("optional_{}", self.sanitize_type_name(inner)),
            Type::Instantiated(name, type_args) => {
                let args_str = type_args
                    .iter()
                    .map(|arg| self.sanitize_type_name(arg))
                    .collect::<Vec<_>>()
                    .join("$");
                format!("{}${}", name, args_str)
            }
            _ => "unknown".to_string(),
        }
    }

    fn create_specialized_method_name(
        &self,
        class_name: &str,
        type_args: &[Type],
        method_name: &str,
    ) -> String {
        if type_args.is_empty() {
            format!("{}.{}", class_name, method_name)
        } else {
            let args_str = type_args
                .iter()
                .map(|t| self.sanitize_type_name(t))
                .collect::<Vec<_>>()
                .join("$");
            format!("{}${}.{}", class_name, args_str, method_name)
        }
    }

    fn generate_specialized_methods(
        &mut self,
        class_name: &str,
        type_args: &[Type],
    ) -> Result<(), String> {
        // save the current builder position so we can restore it after generating specialized methods
        let saved_insert_block = self.builder.get_insert_block();

        // check if we need to generate specialized methods for this variant
        let variant_suffix = type_args
            .iter()
            .map(|t| self.sanitize_type_name(t))
            .collect::<Vec<_>>()
            .join("$");
        let variant_key = format!("{}${}", class_name, variant_suffix);

        // skip if we've already generated methods for this variant
        if self.generated_methods.contains_key(&variant_key) {
            return Ok(());
        }

        // mark this variant as being processed to prevent infinite recursion
        self.generated_methods.insert(variant_key.clone(), true);

        // get the class symbol to access methods
        let class_symbol = self
            .analyzer
            .symbol_table()
            .lookup(class_name)
            .ok_or(format!("Class {} not found", class_name))?;

        // generate specialized methods (including static methods)
        for (method_name, method_sig) in &class_symbol.methods {
            let specialized_method_name =
                self.create_specialized_method_name(class_name, type_args, method_name);

            // skip if this specific method was already generated
            if self
                .generated_methods
                .contains_key(&specialized_method_name)
            {
                continue;
            }

            // get the original method AST node
            let original_method_name = if method_sig.is_static {
                // static methods are stored as "ClassName.method_name"
                format!("{}.{}", class_name, method_name)
            } else {
                // instance methods are stored as "ClassName.method_name" too
                format!("{}.{}", class_name, method_name)
            };

            if let Some(method_node) = self.function_nodes.get(&original_method_name) {
                // clone the method and specialize it
                let mut specialized_method = method_node.clone();
                specialized_method.name = specialized_method_name.clone();

                // create type parameter substitution map
                let type_param_map = class_symbol
                    .type_params
                    .iter()
                    .enumerate()
                    .map(|(_i, param)| (param.0.clone(), type_args[_i].clone()))
                    .collect::<HashMap<_, _>>();

                // substitute types in parameters and return type
                for param in &mut specialized_method.params {
                    param.type_ = self.substitute_types_in_type_node(&param.type_, &type_param_map);
                }
                specialized_method.return_type = self.substitute_types_in_type_node(
                    &specialized_method.return_type,
                    &type_param_map,
                );

                // substitute types in the method body
                let mut substituted_body = Vec::new();
                for stmt in &specialized_method.body {
                    substituted_body
                        .push(self.substitute_types_in_statement(stmt, &type_param_map));
                }
                specialized_method.body = substituted_body;

                // set up generic context for specialized method generation
                let specialized_context = GenericContext {
                    type_params: type_param_map,
                };
                let old_context = self.generic_context.take();
                self.generic_context = Some(specialized_context);

                // declare the specialized method first
                self.declare_function(&specialized_method)?;

                // generate the specialized method
                self.generate_function(&specialized_method)?;

                // mark as generated
                self.generated_methods.insert(specialized_method_name, true);

                // restore original context
                self.generic_context = old_context;
            }
        }

        // restore the builder position to where we were before generating specialized methods
        if let Some(block) = saved_insert_block {
            self.builder.position_at_end(block);
        }

        Ok(())
    }

    fn build_type_param_map(
        &self,
        class_name: &str,
        type_args: &[Type],
    ) -> Result<HashMap<String, Type>, String> {
        let mut type_params = HashMap::new();

        // get the class symbol to find generic parameter names
        if let Some(class_symbol) = self.analyzer.symbol_table().lookup(class_name) {
            if class_symbol.type_params.len() == type_args.len() {
                for (i, param) in class_symbol.type_params.iter().enumerate() {
                    // param is (String, Vec<String>) - first element is the parameter name
                    type_params.insert(param.0.clone(), type_args[i].clone());
                }
            } else {
                return Err(format!(
                    "Type argument count mismatch for class {}",
                    class_name
                ));
            }
        } else {
            return Err(format!("Class {} not found", class_name));
        }

        Ok(type_params)
    }

    fn generate_constructor_call(
        &mut self,
        class_name: &str,
        _args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // get the class type from our type map
        let class_type = *self
            .type_map
            .get(class_name)
            .ok_or(format!("Class '{}' not found in type map", class_name))?;

        // register the object type if not already registered
        let type_name = format!("type_name_{}", class_name);
        let type_name_global = self
            .builder
            .build_global_string_ptr(class_name, &type_name)
            .map_err(|e| e.to_string())?;
        if let Some(global) = self.module.get_global(&type_name) {
            global.set_linkage(inkwell::module::Linkage::External);
        }
        let type_size = class_type.size_of().ok_or("Cannot get type size")?;
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

        // allocate the object using runtime
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

        // get the object data pointer for field initialization
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

        // initialize fields based on their types
        if let Some(fields) = self.classes.get(class_name) {
            let fields_vec: Vec<_> = fields
                .iter()
                .enumerate()
                .map(|(i, f)| (i, f.clone()))
                .collect();
            for (i, field) in fields_vec {
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        class_type.into_struct_type(),
                        struct_ptr,
                        i as u32,
                        &format!("field_{}", i),
                    )
                    .map_err(|e| e.to_string())?;

                // convert TypeNode to Type for resolution
                let field_type = self.type_node_to_type(&field.type_);
                self.initialize_field_by_type(field_ptr, &field_type, field.is_generic_param)?;
            }
        }

        // return the allocated object as a boxed Value pointer
        Ok(obj_value_ptr.into())
    }

    fn generate_method_call_on_self(
        &mut self,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // get self pointer
        let (self_ptr, _, _) = self
            .variables
            .get("self")
            .or_else(|| self.global_variables.get("self"))
            .ok_or("Self not found in method call")?;

        // get class name from self type
        let class_name = if let Some((_, _, Type::Named(class_name, _))) = self
            .variables
            .get("self")
            .or_else(|| self.global_variables.get("self"))
        {
            class_name
        } else {
            return Err("Self type not found".to_string());
        };

        // build method function name: {class_name}.{method_name}
        let method_func_name = format!("{}.{}", class_name, method_name);

        // check if method is static
        if let Some(class) = self.analyzer.symbol_table().lookup(class_name) {
            if let Some(method) = class.methods.get(method_name) {
                if method.is_static {
                    return Err(format!(
                        "Cannot call static method {} with self",
                        method_name
                    ));
                }
            }
        }

        // get the function
        let func_val = *self
            .functions
            .get(&method_func_name)
            .ok_or(format!("Method {} not found", method_func_name))?;

        // build call arguments: self + args
        // load the actual object pointer from the alloca first
        let self_loaded = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                *self_ptr,
                "load_self_for_method_call",
            )
            .map_err(|e| e.to_string())?;
        let mut call_args = vec![self_loaded.into()];
        for arg in args {
            call_args.push(self.generate_expression(arg)?.into());
        }

        // call the method
        let call = self
            .builder
            .build_call(func_val, &call_args, &format!("call_{}", method_name))
            .map_err(|e| e.to_string())?;

        Ok(call.try_as_basic_value().left().unwrap())
    }

    fn generate_generic_function_call(
        &mut self,
        func_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // get the generic function AST node
        let func_node = self
            .function_nodes
            .get(func_name)
            .ok_or(format!("Generic function {} not found", func_name))?;

        // infer concrete types by matching function signature against arguments
        let mut type_map = std::collections::HashMap::new();

        // for each parameter in the function signature, match against the corresponding argument
        for (param_idx, param) in func_node.params.iter().enumerate() {
            if param_idx >= args.len() {
                break;
            }

            let arg_type = self
                .analyzer
                .get_expression_type(&args[param_idx])
                .map_err(|e| format!("Failed to get argument type: {}", e))?;

            // recursively match the parameter type against the argument type to infer generic parameters
            self.infer_types_from_signature(&param.type_, &arg_type, &mut type_map)?;
        }

        // convert to concrete types list in the order of type parameters
        let mut concrete_types = Vec::new();
        for (type_param_name, _) in &func_node.type_params {
            if let Some(concrete_type) = type_map.get(type_param_name) {
                concrete_types.push(concrete_type.clone());
            } else {
                return Err(format!(
                    "Could not infer concrete type for generic parameter {}",
                    type_param_name
                ));
            }
        }

        // create instantiation key
        let type_names: Vec<String> = concrete_types
            .iter()
            .map(|t| self.type_to_string(t))
            .collect();
        let instance_name = format!("{}_{}", func_name, type_names.join("_"));

        // check if already instantiated
        if self.module.get_function(&instance_name).is_none() {
            // instantiate the generic function
            self.instantiate_generic_function(func_name, &concrete_types, &instance_name)?;
        }

        // call the instantiated function
        let func = self
            .module
            .get_function(&instance_name)
            .ok_or(format!("Instantiated function {} not found", instance_name))?;

        let mut call_args = vec![];
        for arg in args {
            call_args.push(self.generate_expression(arg)?.into());
        }

        let call = self
            .builder
            .build_call(func, &call_args, "generic_func_call")
            .map_err(|e| e.to_string())?;

        match call.try_as_basic_value().left() {
            Some(val) => Ok(val),
            None => Ok(self.context.i32_type().const_int(0, false).into()),
        }
    }

    fn instantiate_generic_function(
        &mut self,
        func_name: &str,
        concrete_types: &[Type],
        instance_name: &str,
    ) -> Result<(), String> {
        let func_node = self
            .function_nodes
            .get(func_name)
            .ok_or(format!("Generic function {} not found", func_name))?;

        // create type substitution map
        let mut type_map = std::collections::HashMap::new();
        for (i, type_param) in func_node.type_params.iter().enumerate() {
            if i < concrete_types.len() {
                type_map.insert(type_param.0.clone(), concrete_types[i].clone());
            }
        }

        // clone and substitute the function
        let mut substituted_func = func_node.clone();
        substituted_func.name = instance_name.to_string();
        substituted_func.type_params.clear(); // no longer generic

        // substitute types in parameters and return type
        for param in &mut substituted_func.params {
            param.type_ = self.substitute_types_in_type_node(&param.type_, &type_map);
        }
        substituted_func.return_type =
            self.substitute_types_in_type_node(&substituted_func.return_type, &type_map);

        // substitute types in the function body
        let mut substituted_body = Vec::new();
        for stmt in &substituted_func.body {
            substituted_body.push(self.substitute_types_in_statement(stmt, &type_map));
        }
        substituted_func.body = substituted_body;

        // save current context (from calling context)
        let saved_variables = self.variables.clone();
        let saved_current_function_name = self.current_function_name.take();
        let saved_current_function_return_type = self.current_function_return_type.take();
        let saved_builder_position = self.builder.get_insert_block();

        // declare the instantiated function
        self.declare_function(&substituted_func)?;

        // generate the instantiated function
        self.generate_function(&substituted_func)?;

        // restore context (back to calling context)
        self.variables = saved_variables;
        self.current_function_name = saved_current_function_name;
        self.current_function_return_type = saved_current_function_return_type;
        if let Some(block) = saved_builder_position {
            self.builder.position_at_end(block);
        }

        Ok(())
    }

    fn substitute_types_in_type_node(
        &self,
        type_node: &TypeNode,
        type_map: &std::collections::HashMap<String, Type>,
    ) -> TypeNode {
        match &type_node.kind {
            TypeKind::Named(name, args) => {
                if type_map.contains_key(name) {
                    // this is a generic type parameter - substitute it
                    let concrete_type = &type_map[name];
                    self.type_to_type_node(concrete_type)
                } else {
                    // not a generic parameter, substitute arguments recursively
                    let substituted_args = args
                        .iter()
                        .map(|arg| self.substitute_types_in_type_node(arg, type_map))
                        .collect();
                    TypeNode {
                        kind: TypeKind::Named(name.clone(), substituted_args),
                        span: type_node.span,
                    }
                }
            }
            TypeKind::List(inner) => TypeNode {
                kind: TypeKind::List(Box::new(
                    self.substitute_types_in_type_node(inner, type_map),
                )),
                span: Span::new(0, 0),
            },
            TypeKind::Function { params, returns } => {
                let substituted_params = params
                    .iter()
                    .map(|p| self.substitute_types_in_type_node(p, type_map))
                    .collect();
                let substituted_returns = self.substitute_types_in_type_node(returns, type_map);
                TypeNode {
                    kind: TypeKind::Function {
                        params: substituted_params,
                        returns: Box::new(substituted_returns),
                    },
                    span: Span::new(0, 0),
                }
            }
            // for other types, return as-is (they don't contain generic parameters)
            _ => type_node.clone(),
        }
    }

    fn substitute_types_in_statement(
        &self,
        stmt: &StatementNode,
        type_map: &std::collections::HashMap<String, Type>,
    ) -> StatementNode {
        match &stmt.kind {
            StatementKind::TypedDecl(name, type_node, expr) => {
                let substituted_type = self.substitute_types_in_type_node(type_node, type_map);
                let substituted_expr = self.substitute_types_in_expression(expr, type_map);
                StatementNode {
                    kind: StatementKind::TypedDecl(
                        name.clone(),
                        substituted_type,
                        substituted_expr,
                    ),
                    span: stmt.span,
                }
            }
            StatementKind::AutoDecl(name, type_node, expr) => {
                let substituted_type = self.substitute_types_in_type_node(type_node, type_map);
                let substituted_expr = self.substitute_types_in_expression(expr, type_map);
                StatementNode {
                    kind: StatementKind::AutoDecl(name.clone(), substituted_type, substituted_expr),
                    span: stmt.span,
                }
            }
            StatementKind::For {
                var,
                var_type,
                iter,
                body,
            } => {
                let substituted_var_type = self.substitute_types_in_type_node(var_type, type_map);
                let substituted_iter = self.substitute_types_in_expression(iter, type_map);
                let substituted_body = body
                    .iter()
                    .map(|s| self.substitute_types_in_statement(s, type_map))
                    .collect();
                StatementNode {
                    kind: StatementKind::For {
                        var: var.clone(),
                        var_type: substituted_var_type,
                        iter: substituted_iter,
                        body: substituted_body,
                    },
                    span: stmt.span,
                }
            }
            StatementKind::Return(expr) => {
                let substituted_expr = expr
                    .as_ref()
                    .map(|e| self.substitute_types_in_expression(e, type_map));
                StatementNode {
                    kind: StatementKind::Return(substituted_expr),
                    span: stmt.span,
                }
            }
            StatementKind::Expression(expr) => {
                let substituted_expr = self.substitute_types_in_expression(expr, type_map);
                StatementNode {
                    kind: StatementKind::Expression(substituted_expr),
                    span: stmt.span,
                }
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let substituted_cond = self.substitute_types_in_expression(cond, type_map);
                let substituted_then = then_block
                    .iter()
                    .map(|s| self.substitute_types_in_statement(s, type_map))
                    .collect();
                let substituted_else = else_block.as_ref().map(|b| {
                    b.iter()
                        .map(|s| self.substitute_types_in_statement(s, type_map))
                        .collect()
                });
                StatementNode {
                    kind: StatementKind::If {
                        cond: substituted_cond,
                        then_block: substituted_then,
                        else_block: substituted_else,
                    },
                    span: stmt.span,
                }
            }
            // for other statement types, return unchanged for now
            _ => stmt.clone(),
        }
    }

    fn substitute_types_in_expression(
        &self,
        expr: &ExpressionNode,
        type_map: &std::collections::HashMap<String, Type>,
    ) -> ExpressionNode {
        match &expr.kind {
            ExpressionKind::Call { func, args } => {
                let substituted_func = self.substitute_types_in_expression(func, type_map);
                let substituted_args = args
                    .iter()
                    .map(|a| self.substitute_types_in_expression(a, type_map))
                    .collect();
                ExpressionNode {
                    kind: ExpressionKind::Call {
                        func: Box::new(substituted_func),
                        args: substituted_args,
                    },
                    span: expr.span,
                }
            }
            ExpressionKind::FieldAccess {
                expr: inner_expr,
                field,
            } => {
                let substituted_expr = self.substitute_types_in_expression(inner_expr, type_map);
                ExpressionNode {
                    kind: ExpressionKind::FieldAccess {
                        expr: Box::new(substituted_expr),
                        field: field.clone(),
                    },
                    span: expr.span,
                }
            }
            ExpressionKind::Lambda { params, body } => {
                // for lambda parameters, substitute their types
                let substituted_params = params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        type_: self.substitute_types_in_type_node(&p.type_, type_map),
                    })
                    .collect();
                // for lambda body, substitute statements
                let substituted_body = body
                    .iter()
                    .map(|s| self.substitute_types_in_statement(s, type_map))
                    .collect();
                ExpressionNode {
                    kind: ExpressionKind::Lambda {
                        params: substituted_params,
                        body: substituted_body,
                    },
                    span: expr.span,
                }
            }
            // for other expression types, return unchanged for now
            _ => expr.clone(),
        }
    }

    /// recursively match a parameter type pattern against a concrete argument type to infer generic parameters
    fn infer_types_from_signature(
        &self,
        param_type: &TypeNode,
        arg_type: &Type,
        type_map: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), String> {
        match &param_type.kind {
            TypeKind::Named(name, type_args) => {
                if type_args.is_empty() {
                    // this is a potential generic parameter or concrete type
                    if name.chars().next().unwrap_or(' ').is_uppercase() || name.len() <= 3 {
                        // likely a generic parameter - infer it from the argument type
                        if let Some(existing) = type_map.get(name) {
                            if existing != arg_type {
                                return Err(format!(
                                    "Type mismatch for generic parameter {}: expected {:?}, got {:?}",
                                    name, existing, arg_type
                                ));
                            }
                        } else {
                            type_map.insert(name.clone(), arg_type.clone());
                        }
                    } else {
                        // concrete type - should match exactly
                        let expected_concrete = self.type_node_to_type(param_type);
                        if expected_concrete != *arg_type {
                            return Err(format!(
                                "Type mismatch: expected {:?}, got {:?}",
                                expected_concrete, arg_type
                            ));
                        }
                    }
                } else {
                    // generic type with arguments (like list<T>)
                    match arg_type {
                        Type::Named(arg_name, arg_type_args) => {
                            if name != arg_name {
                                return Err(format!(
                                    "Type name mismatch: expected {}, got {}",
                                    name, arg_name
                                ));
                            }
                            if type_args.len() != arg_type_args.len() {
                                return Err(format!(
                                    "Type argument count mismatch for {}: expected {}, got {}",
                                    name,
                                    type_args.len(),
                                    arg_type_args.len()
                                ));
                            }
                            // recursively match type arguments
                            for (param_arg, arg_arg) in type_args.iter().zip(arg_type_args.iter()) {
                                self.infer_types_from_signature(param_arg, arg_arg, type_map)?;
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Expected named type with args, got {:?}",
                                arg_type
                            ));
                        }
                    }
                }
            }
            TypeKind::List(inner_param_type) => match arg_type {
                Type::List(inner_arg_type) => {
                    self.infer_types_from_signature(inner_param_type, inner_arg_type, type_map)?;
                }
                _ => return Err(format!("Expected list type, got {:?}", arg_type)),
            },
            TypeKind::Function {
                params: param_params,
                returns: param_returns,
            } => {
                match arg_type {
                    Type::Function {
                        params: arg_params,
                        returns: arg_returns,
                    } => {
                        if param_params.len() != arg_params.len() {
                            return Err(format!(
                                "Function parameter count mismatch: expected {}, got {}",
                                param_params.len(),
                                arg_params.len()
                            ));
                        }
                        // match parameter types
                        for (param_param, arg_param) in param_params.iter().zip(arg_params.iter()) {
                            self.infer_types_from_signature(param_param, arg_param, type_map)?;
                        }
                        // match return type
                        self.infer_types_from_signature(param_returns, arg_returns, type_map)?;
                    }
                    _ => return Err(format!("Expected function type, got {:?}", arg_type)),
                }
            }
            TypeKind::Primitive(primitive) => {
                let expected = match primitive {
                    PrimitiveType::Int => Type::Primitive(PrimitiveType::Int),
                    PrimitiveType::Float => Type::Primitive(PrimitiveType::Float),
                    PrimitiveType::Bool => Type::Primitive(PrimitiveType::Bool),
                    PrimitiveType::Str => Type::Primitive(PrimitiveType::Str),
                    _ => return Err(format!("Unsupported primitive type {:?}", primitive)),
                };
                if expected != *arg_type {
                    return Err(format!(
                        "Primitive type mismatch: expected {:?}, got {:?}",
                        expected, arg_type
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Unsupported type kind in signature matching: {:?}",
                    param_type.kind
                ));
            }
        }
        Ok(())
    }

    // recursion is necessary here for proper type resolution
    #[allow(clippy::only_used_in_recursion)]
    fn type_to_string(&self, type_: &Type) -> String {
        match type_ {
            Type::Primitive(PrimitiveType::Int) => "int".to_string(),
            Type::Primitive(PrimitiveType::Float) => "float".to_string(),
            Type::Primitive(PrimitiveType::Bool) => "bool".to_string(),
            Type::Primitive(PrimitiveType::Str) => "str".to_string(),
            Type::List(inner) => format!("list_{}", self.type_to_string(inner)),
            _ => "unknown".to_string(),
        }
    }

    pub fn emit_ir_to_file(&self, filename: &str) -> Result<(), String> {
        self.module
            .print_to_file(filename)
            .map_err(|e| format!("Failed to write IR: {}", e))
    }
}
