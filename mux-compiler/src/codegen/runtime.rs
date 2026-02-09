//! Runtime function calls and value boxing/unboxing.
//!
//! This module handles calling mux runtime functions and converting
//! between LLVM values and boxed Value* pointers.

use super::CodeGenerator;
use crate::ast::PrimitiveType;
use crate::semantics::Type;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue};

impl<'a> CodeGenerator<'a> {
    pub(super) fn generate_runtime_call(
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
        let call = self
            .builder
            .build_call(func, args, "call")
            .expect("build_call should always return Some");
        call.try_as_basic_value().left()
    }

    pub(super) fn box_value(&mut self, val: BasicValueEnum<'a>) -> PointerValue<'a> {
        if val.is_int_value() {
            let int_val = val.into_int_value();
            // Check if this is a bool (i1) - LLVM considers i1 as an int type
            if int_val.get_type().get_bit_width() == 1 {
                // Bool: extend i1 to i32 for mux_bool_value
                let i32_val = self
                    .builder
                    .build_int_z_extend(int_val, self.context.i32_type(), "bool_to_i32")
                    .expect("bool extension should succeed");
                let call = self
                    .generate_runtime_call("mux_bool_value", &[i32_val.into()])
                    .expect("mux_bool_value should always return a value");
                call.into_pointer_value()
            } else {
                // Regular int (i64)
                let call = self
                    .generate_runtime_call("mux_int_value", &[int_val.into()])
                    .expect("mux_int_value should always return a value");
                call.into_pointer_value()
            }
        } else if val.is_float_value() {
            let call = self
                .generate_runtime_call("mux_float_value", &[val.into()])
                .expect("mux_float_value should always return a value");
            call.into_pointer_value()
        } else if val.is_pointer_value() {
            // assume string or already boxed Value (from Map/Set/List literals)
            // map/Set/List literals already return *mut Value pointers, so just return as-is
            val.into_pointer_value()
        } else {
            panic!("Unexpected value type in box_value")
        }
    }

    pub(super) fn get_raw_int_value(
        &mut self,
        val: BasicValueEnum<'a>,
    ) -> Result<IntValue<'a>, String> {
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

    pub(super) fn get_raw_float_value(
        &mut self,
        val: BasicValueEnum<'a>,
    ) -> Result<FloatValue<'a>, String> {
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

    pub(super) fn get_raw_bool_value(
        &mut self,
        val: BasicValueEnum<'a>,
    ) -> Result<IntValue<'a>, String> {
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
    pub(super) fn extract_value_from_ptr(
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
            | Type::Tuple(_, _)
            | Type::Named(_, _)
            | Type::Optional(_)
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
            Type::Module(_) => {
                panic!("Module types should not appear in codegen - they are compile-time only")
            }
        }
    }

    /// Call a runtime getter function to extract a pointer from a Value.
    fn call_value_getter(
        &mut self,
        func_name: &str,
        value_ptr: PointerValue<'a>,
        result_name: &str,
    ) -> Result<PointerValue<'a>, String> {
        let func = self
            .module
            .get_function(func_name)
            .ok_or_else(|| format!("{} not found", func_name))?;
        self.builder
            .build_call(func, &[value_ptr.into()], result_name)
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| format!("{} returned no value", func_name))
            .map(|v| v.into_pointer_value())
    }

    pub(super) fn extract_c_string_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        self.call_value_getter("mux_value_get_string", value_ptr, "get_string")
    }

    pub(super) fn box_string_value(
        &mut self,
        cstr_ptr: PointerValue<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function("mux_value_from_string")
            .ok_or("mux_value_from_string not found")?;
        self.builder
            .build_call(func, &[cstr_ptr.into()], "from_string")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| "mux_value_from_string returned no value".to_string())
    }

    pub(super) fn extract_list_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        self.call_value_getter("mux_value_get_list", value_ptr, "get_list")
    }

    pub(super) fn extract_map_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        self.call_value_getter("mux_value_get_map", value_ptr, "get_map")
    }

    pub(super) fn extract_set_from_value(
        &mut self,
        value_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        self.call_value_getter("mux_value_get_set", value_ptr, "get_set")
    }
}
