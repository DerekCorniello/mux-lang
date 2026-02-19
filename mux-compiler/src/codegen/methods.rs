//! Method call generation for the code generator.
//!
//! This module handles:
//! - Instance method calls on user-defined classes
//! - Primitive type method calls (int, float, str, bool, char)
//! - Collection method calls (list, map, set)
//! - Optional method calls

use inkwell::values::BasicValueEnum;

use crate::ast::{ExpressionNode, PrimitiveType};
use crate::semantics::Type;

use super::CodeGenerator;

impl<'a> CodeGenerator<'a> {
    fn call_runtime_to_string(
        &self,
        value: BasicValueEnum<'a>,
        func_name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        let to_cstr = self
            .module
            .get_function(func_name)
            .ok_or(format!("{} not found", func_name))?;
        let call = self
            .builder
            .build_call(to_cstr, &[value.into()], "to_cstr")
            .map_err(|e| e.to_string())?;
        let cstr = call
            .try_as_basic_value()
            .left()
            .ok_or(format!("{} should return a basic value", func_name))?;
        let new_string = self
            .module
            .get_function("mux_new_string_from_cstr")
            .ok_or("mux_new_string_from_cstr not found")?;
        let call2 = self
            .builder
            .build_call(new_string, &[cstr.into()], "new_string")
            .map_err(|e| e.to_string())?;
        Ok(call2
            .try_as_basic_value()
            .left()
            .expect("mux_new_string_from_cstr should return a basic value"))
    }

    fn call_runtime_to_string_from_call(
        &self,
        call: inkwell::values::CallSiteValue<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let cstr = call
            .try_as_basic_value()
            .left()
            .ok_or_else(|| "Function should return a basic value".to_string())?;
        let new_string = self
            .module
            .get_function("mux_new_string_from_cstr")
            .ok_or("mux_new_string_from_cstr not found".to_string())?;
        let call2 = self
            .builder
            .build_call(new_string, &[cstr.into()], "new_string")
            .map_err(|e| e.to_string())?;
        Ok(call2
            .try_as_basic_value()
            .left()
            .expect("mux_new_string_from_cstr should return a basic value"))
    }

    pub(super) fn generate_method_call(
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
            Type::Tuple(_, _) => self.generate_tuple_method_call(obj_value, method_name, args),
            Type::Named(name, type_args) if name == "Result" && type_args.len() == 2 => {
                self.generate_result_method_call(obj_value, method_name, args)
            }
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

    fn generate_result_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match method_name {
            "is_ok" => {
                if !args.is_empty() {
                    return Err("is_ok() method takes no arguments".to_string());
                }
                let extract_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_result")
                            .expect("mux_value_get_result must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_result",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_result should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_result_is_ok")
                            .expect("mux_result_is_ok must be declared in runtime"),
                        &[extract_result.into()],
                        "is_ok",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_result_is_ok should return a basic value"))
            }
            "is_err" => {
                if !args.is_empty() {
                    return Err("is_err() method takes no arguments".to_string());
                }
                let extract_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_result")
                            .expect("mux_value_get_result must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_result",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_result should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_result_is_err")
                            .expect("mux_result_is_err must be declared in runtime"),
                        &[extract_result.into()],
                        "is_err",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_result_is_err should return a basic value"))
            }
            _ => Err(format!("Method {} not implemented for Result", method_name)),
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
                "to_string" => self.call_runtime_to_string(obj_value, "mux_int_to_string"),
                "to_float" => {
                    let float_val = self
                        .builder
                        .build_signed_int_to_float(
                            obj_value.into_int_value(),
                            self.context.f64_type(),
                            "int_to_float",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(float_val.into())
                }
                "to_int" => Ok(obj_value),
                _ => Err(format!("Method {} not implemented for int", method_name)),
            },
            PrimitiveType::Float => match method_name {
                "to_string" => self.call_runtime_to_string(obj_value, "mux_float_to_string"),
                "to_int" => {
                    let float_val = obj_value.into_float_value();
                    let int_val = self
                        .builder
                        .build_float_to_signed_int(
                            float_val,
                            self.context.i64_type(),
                            "float_to_int",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(int_val.into())
                }
                "to_float" => Ok(obj_value),
                _ => Err(format!("Method {} not implemented for float", method_name)),
            },
            PrimitiveType::Str => match method_name {
                "to_string" => self.call_runtime_to_string(obj_value, "mux_value_to_string"),
                "to_int" => {
                    // str.to_int() - call mux_string_to_int which returns Result<int, str>
                    // First convert the boxed string to a C string
                    let func_to_cstr = self
                        .module
                        .get_function("mux_value_to_string")
                        .ok_or("mux_value_to_string not found")?;
                    let cstr = self
                        .builder
                        .build_call(func_to_cstr, &[obj_value.into()], "str_to_cstr")
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_to_string should return a basic value");
                    // Now call mux_string_to_int with the C string
                    let func = self
                        .module
                        .get_function("mux_string_to_int")
                        .ok_or("mux_string_to_int not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[cstr.into()], "str_to_int")
                        .map_err(|e| e.to_string())?;
                    Ok(call
                        .try_as_basic_value()
                        .left()
                        .expect("mux_string_to_int should return a basic value"))
                }
                "to_float" => {
                    // str.to_float() - call mux_string_to_float which returns Result<float, str>
                    // First convert the boxed string to a C string
                    let func_to_cstr = self
                        .module
                        .get_function("mux_value_to_string")
                        .ok_or("mux_value_to_string not found")?;
                    let cstr = self
                        .builder
                        .build_call(func_to_cstr, &[obj_value.into()], "str_to_cstr")
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_to_string should return a basic value");
                    // Now call mux_string_to_float with the C string
                    let func = self
                        .module
                        .get_function("mux_string_to_float")
                        .ok_or("mux_string_to_float not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[cstr.into()], "str_to_float")
                        .map_err(|e| e.to_string())?;
                    Ok(call
                        .try_as_basic_value()
                        .left()
                        .expect("mux_string_to_float should return a basic value"))
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
                    Ok(call
                        .try_as_basic_value()
                        .left()
                        .expect("mux_string_length should return a basic value"))
                }
                _ => Err(format!("Method {} not implemented for string", method_name)),
            },
            PrimitiveType::Bool => match method_name {
                "to_string" => {
                    let bool_i32 = if obj_value.is_int_value() {
                        let i1_val = obj_value.into_int_value();
                        self.builder
                            .build_int_z_extend(i1_val, self.context.i32_type(), "i1_to_i32")
                            .map_err(|e| e.to_string())?
                    } else if obj_value.is_pointer_value() {
                        let extract_func = self
                            .module
                            .get_function("mux_value_get_bool")
                            .ok_or("mux_value_get_bool not found".to_string())?;
                        let extracted = self
                            .builder
                            .build_call(extract_func, &[obj_value.into()], "extract_bool")
                            .map_err(|e| e.to_string())?
                            .try_as_basic_value()
                            .left()
                            .ok_or_else(|| "Call returned no value".to_string())?;
                        extracted.into_int_value()
                    } else {
                        return Err("Invalid boolean value type".to_string());
                    };
                    let bool_func = self
                        .module
                        .get_function("mux_bool_to_string")
                        .ok_or("mux_bool_to_string not found".to_string())?;
                    let call = self
                        .builder
                        .build_call(bool_func, &[bool_i32.into()], "bool_to_str")
                        .map_err(|e| e.to_string())?;
                    self.call_runtime_to_string_from_call(call)
                }
                "to_int" => {
                    let bool_i32 = obj_value.into_int_value();
                    let int_val = self
                        .builder
                        .build_int_z_extend(bool_i32, self.context.i64_type(), "bool_to_int")
                        .map_err(|e| e.to_string())?;
                    Ok(int_val.into())
                }
                "to_float" => {
                    let bool_i32 = obj_value.into_int_value();
                    let float_val = self
                        .builder
                        .build_unsigned_int_to_float(
                            bool_i32,
                            self.context.f64_type(),
                            "bool_to_float",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(float_val.into())
                }
                _ => Err(format!("Method {} not implemented for bool", method_name)),
            },
            PrimitiveType::Char => match method_name {
                "to_string" => self.call_runtime_to_string(obj_value, "mux_char_to_string"),
                "to_int" => {
                    // char.to_int() - call mux_char_to_int which returns Result<int, str>
                    // Only works for '0'-'9', returns error for other chars
                    let func = self
                        .module
                        .get_function("mux_char_to_int")
                        .ok_or("mux_char_to_int not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "char_to_int")
                        .map_err(|e| e.to_string())?;
                    Ok(call
                        .try_as_basic_value()
                        .left()
                        .expect("mux_char_to_int should return a basic value"))
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
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_get")
                            .expect("mux_list_get must be declared in runtime"),
                        &[
                            raw_list
                                .try_as_basic_value()
                                .left()
                                .expect("mux_value_get_list should return a basic value")
                                .into(),
                            index_val.into(),
                        ],
                        "list_get",
                    )
                    .map_err(|e| e.to_string())?;
                // box the Optional as a Value
                let boxed_call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_into_value")
                            .expect("mux_optional_into_value must be declared in runtime"),
                        &[call
                            .try_as_basic_value()
                            .left()
                            .expect("mux_list_get should return a basic value")
                            .into()],
                        "optional_as_value",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(boxed_call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a basic value"))
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
                        self.module
                            .get_function("mux_list_pop_back_value")
                            .expect("mux_list_pop_back_value must be declared in runtime"),
                        &[obj_value.into()],
                        "list_pop_back",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_pop_back_value should return a basic value"))
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
                        self.module
                            .get_function("mux_list_pop_value")
                            .expect("mux_list_pop_value must be declared in runtime"),
                        &[obj_value.into()],
                        "list_pop",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_pop_value should return a basic value"))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }

                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_is_empty")
                            .expect("mux_list_is_empty must be declared in runtime"),
                        &[raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into()],
                        "list_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_is_empty should return a basic value"))
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }

                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_length")
                            .expect("mux_list_length must be declared in runtime"),
                        &[raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into()],
                        "list_size",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_length should return a basic value"))
            }
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                // extract raw List pointer from Value
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_to_string")
                            .expect("mux_list_to_string must be declared in runtime"),
                        &[raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into()],
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
                        &[call
                            .try_as_basic_value()
                            .left()
                            .expect("mux_list_to_string should return a basic value")
                            .into()],
                        "new_str",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call2
                    .try_as_basic_value()
                    .left()
                    .expect("mux_new_string_from_cstr should return a basic value"))
            }
            "sort" => {
                if !args.is_empty() {
                    return Err("sort() method takes no arguments".to_string());
                }
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                self.builder
                    .build_call(
                        self.module
                            .get_function("mux_list_sort")
                            .expect("mux_list_sort must be declared in runtime"),
                        &[raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into()],
                        "list_sort",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "reverse" => {
                if !args.is_empty() {
                    return Err("reverse() method takes no arguments".to_string());
                }
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                self.builder
                    .build_call(
                        self.module
                            .get_function("mux_list_reverse")
                            .expect("mux_list_reverse must be declared in runtime"),
                        &[raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into()],
                        "list_reverse",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_contains")
                            .expect("mux_list_contains must be declared in runtime"),
                        &[
                            raw_list
                                .try_as_basic_value()
                                .left()
                                .expect("mux_value_get_list should return a basic value")
                                .into(),
                            elem_ptr.into(),
                        ],
                        "list_contains",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_contains should return a basic value"))
            }
            "index_of" => {
                if args.len() != 1 {
                    return Err("index_of() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                let index_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_index_of")
                            .expect("mux_list_index_of must be declared in runtime"),
                        &[
                            raw_list
                                .try_as_basic_value()
                                .left()
                                .expect("mux_value_get_list should return a basic value")
                                .into(),
                            elem_ptr.into(),
                        ],
                        "list_index_of",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_index_of should return a basic value");

                let i64_type = self.context.i64_type();
                let neg_one = i64_type.const_int(-1i64 as u64, true);
                let is_found = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        index_result.into_int_value(),
                        neg_one,
                        "is_found",
                    )
                    .map_err(|e| e.to_string())?;

                let optional_none = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_none")
                            .expect("mux_optional_none must be declared in runtime"),
                        &[],
                        "optional_none",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_none should return a basic value");

                let optional_some = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_some_int")
                            .expect("mux_optional_some_int must be declared in runtime"),
                        &[index_result.into()],
                        "optional_some",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_some_int should return a basic value");

                let result = self
                    .builder
                    .build_select(is_found, optional_some, optional_none, "index_optional")
                    .map_err(|e| e.to_string())?;

                let boxed_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_into_value")
                            .expect("mux_optional_into_value must be declared in runtime"),
                        &[result.into()],
                        "optional_as_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a basic value");
                Ok(boxed_result)
            }
            "clear" => {
                if !args.is_empty() {
                    return Err("clear() method takes no arguments".to_string());
                }
                let raw_list = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_list")
                            .expect("mux_value_get_list must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_list",
                    )
                    .map_err(|e| e.to_string())?;

                self.builder
                    .build_call(
                        self.module
                            .get_function("mux_list_clear")
                            .expect("mux_list_clear must be declared in runtime"),
                        &[raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into()],
                        "list_clear",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "filter" | "map" | "reduce" | "find" => {
                self.generate_list_higher_order_method(obj_value, method_name, args)
            }
            _ => Err(format!("Method {} not implemented for lists", method_name)),
        }
    }

    fn generate_list_higher_order_method(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let raw_list = self
            .builder
            .build_call(
                self.module
                    .get_function("mux_value_get_list")
                    .expect("mux_value_get_list must be declared in runtime"),
                &[obj_value.into()],
                "extract_list",
            )
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .expect("mux_value_get_list should return a basic value");

        let list_ptr = raw_list.into_pointer_value();

        let list_len = self
            .builder
            .build_call(
                self.module
                    .get_function("mux_list_length")
                    .expect("mux_list_length must be declared in runtime"),
                &[raw_list.into()],
                "list_len",
            )
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .expect("mux_list_length should return a basic value")
            .into_int_value();

        let i64_type = self.context.i64_type();
        let zero = i64_type.const_int(0, false);
        let one = i64_type.const_int(1, false);

        match method_name {
            "filter" => {
                if args.len() != 1 {
                    return Err("filter() method takes exactly 1 argument (predicate function)".to_string());
                }

                let predicate_closure = self.generate_expression(&args[0])?;

                let result_list = self.generate_runtime_call(
                    "mux_list_new",
                    &[],
                ).ok_or("mux_list_new should return a value")?;

                let loop_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "filter_loop",
                );
                let body_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "filter_body",
                );
                let continue_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "filter_continue",
                );
                let merge_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "filter_merge",
                );

                let index_alloca = self.builder.build_alloca(i64_type, "filter_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, zero).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(loop_bb);
                let current_index = self.builder.build_load(i64_type, index_alloca, "current_index").map_err(|e| e.to_string())?.into_int_value();
                let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, current_index, list_len, "loop_cond").map_err(|e| e.to_string())?;
                self.builder.build_conditional_branch(cond, body_bb, merge_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_opt = self.builder.build_call(
                    self.module.get_function("mux_list_get").expect("mux_list_get must be declared"),
                    &[list_ptr.into(), current_index.into()],
                    "get_elem",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_list_get should return a value");

                let should_include = self.call_closure_with_args(predicate_closure, &[elem_opt.into()])?;

                let bool_val = if should_include.is_int_value() {
                    should_include.into_int_value()
                } else {
                    self.builder.build_call(
                        self.module.get_function("mux_value_get_bool").expect("mux_value_get_bool must be declared"),
                        &[should_include.into()],
                        "get_bool",
                    ).map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_bool should return a value")
                    .into_int_value()
                };

                let cond_bool = self.builder.build_int_compare(inkwell::IntPredicate::NE, bool_val, self.context.i32_type().const_int(0, false), "bool_cond").map_err(|e| e.to_string())?;
                self.builder.build_conditional_branch(cond_bool, continue_bb, continue_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(continue_bb);
                let bb = self.builder.get_insert_block().ok_or("No insert block")?;
                let then_bb = self.context.insert_basic_block_after(bb, "filter_then");
                let after_bb = self.context.insert_basic_block_after(then_bb, "filter_after");

                self.builder.build_conditional_branch(cond_bool, then_bb, after_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(then_bb);
                let elem_value = self.builder.build_call(
                    self.module.get_function("mux_optional_get_value").expect("mux_optional_get_value must be declared"),
                    &[elem_opt.into()],
                    "get_elem_value",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_optional_get_value should return a value");

                self.builder.build_call(
                    self.module.get_function("mux_list_push_back_value").expect("mux_list_push_back_value must be declared"),
                    &[result_list.into(), elem_value.into()],
                    "push_to_result",
                ).map_err(|e| e.to_string())?;
                self.builder.build_unconditional_branch(after_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(after_bb);
                let next_index = self.builder.build_int_add(current_index, one, "next_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, next_index).map_err(|e| e.to_string())?;
                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_bb);
                Ok(result_list)
            }
            "map" => {
                if args.len() != 1 {
                    return Err("map() method takes exactly 1 argument (transform function)".to_string());
                }

                let transform_closure = self.generate_expression(&args[0])?;

                let result_list = self.generate_runtime_call(
                    "mux_list_new",
                    &[],
                ).ok_or("mux_list_new should return a value")?;

                let loop_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "map_loop",
                );
                let body_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "map_body",
                );
                let continue_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "map_continue",
                );
                let merge_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "map_merge",
                );

                let index_alloca = self.builder.build_alloca(i64_type, "map_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, zero).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(loop_bb);
                let current_index = self.builder.build_load(i64_type, index_alloca, "current_index").map_err(|e| e.to_string())?.into_int_value();
                let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, current_index, list_len, "loop_cond").map_err(|e| e.to_string())?;
                self.builder.build_conditional_branch(cond, body_bb, merge_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_opt = self.builder.build_call(
                    self.module.get_function("mux_list_get").expect("mux_list_get must be declared"),
                    &[list_ptr.into(), current_index.into()],
                    "get_elem",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_list_get should return a value");

                let elem_value = self.builder.build_call(
                    self.module.get_function("mux_optional_get_value").expect("mux_optional_get_value must be declared"),
                    &[elem_opt.into()],
                    "get_elem_value",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_optional_get_value should return a value");

                let transformed = self.call_closure_with_args(transform_closure, &[elem_value.into()])?;

                self.builder.build_call(
                    self.module.get_function("mux_list_push_back_value").expect("mux_list_push_back_value must be declared"),
                    &[result_list.into(), self.box_value(transformed).into()],
                    "push_to_result",
                ).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(continue_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(continue_bb);
                let next_index = self.builder.build_int_add(current_index, one, "next_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, next_index).map_err(|e| e.to_string())?;
                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_bb);
                Ok(result_list)
            }
            "reduce" => {
                if args.len() != 2 {
                    return Err("reduce() method takes exactly 2 arguments (initial value and reducer function)".to_string());
                }

                let initial_value = self.generate_expression(&args[0])?;
                let reducer_closure = self.generate_expression(&args[1])?;

                let accumulator_alloca = self.builder.build_alloca(self.box_value(initial_value).get_type(), "reduce_accumulator").map_err(|e| e.to_string())?;
                self.builder.build_store(accumulator_alloca, self.box_value(initial_value)).map_err(|e| e.to_string())?;

                let loop_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "reduce_loop",
                );
                let body_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "reduce_body",
                );
                let continue_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "reduce_continue",
                );
                let merge_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "reduce_merge",
                );

                let index_alloca = self.builder.build_alloca(i64_type, "reduce_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, zero).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(loop_bb);
                let current_index = self.builder.build_load(i64_type, index_alloca, "current_index").map_err(|e| e.to_string())?.into_int_value();
                let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, current_index, list_len, "loop_cond").map_err(|e| e.to_string())?;
                self.builder.build_conditional_branch(cond, body_bb, merge_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let current_acc = self.builder.build_load(accumulator_alloca.get_type(), accumulator_alloca, "current_acc").map_err(|e| e.to_string())?;
                let elem_opt = self.builder.build_call(
                    self.module.get_function("mux_list_get").expect("mux_list_get must be declared"),
                    &[list_ptr.into(), current_index.into()],
                    "get_elem",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_list_get should return a value");

                let elem_value = self.builder.build_call(
                    self.module.get_function("mux_optional_get_value").expect("mux_optional_get_value must be declared"),
                    &[elem_opt.into()],
                    "get_elem_value",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_optional_get_value should return a value");

                let new_acc = self.call_closure_with_args(reducer_closure, &[current_acc.into(), elem_value.into()])?;
                self.builder.build_store(accumulator_alloca, self.box_value(new_acc)).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(continue_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(continue_bb);
                let next_index = self.builder.build_int_add(current_index, one, "next_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, next_index).map_err(|e| e.to_string())?;
                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_bb);
                let final_acc = self.builder.build_load(accumulator_alloca.get_type(), accumulator_alloca, "final_acc").map_err(|e| e.to_string())?;
                Ok(final_acc)
            }
            "find" => {
                if args.len() != 1 {
                    return Err("find() method takes exactly 1 argument (predicate function)".to_string());
                }

                let predicate_closure = self.generate_expression(&args[0])?;

                let loop_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "find_loop",
                );
                let body_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "find_body",
                );
                let continue_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "find_continue",
                );
                let found_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "find_found",
                );
                let merge_bb = self.context.append_basic_block(
                    self.current_function.unwrap(),
                    "find_merge",
                );

                let index_alloca = self.builder.build_alloca(i64_type, "find_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, zero).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(loop_bb);
                let current_index = self.builder.build_load(i64_type, index_alloca, "current_index").map_err(|e| e.to_string())?.into_int_value();
                let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, current_index, list_len, "loop_cond").map_err(|e| e.to_string())?;
                self.builder.build_conditional_branch(cond, body_bb, merge_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_opt = self.builder.build_call(
                    self.module.get_function("mux_list_get").expect("mux_list_get must be declared"),
                    &[list_ptr.into(), current_index.into()],
                    "get_elem",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_list_get should return a value");

                let matches = self.call_closure_with_args(predicate_closure, &[elem_opt.into()])?;

                let bool_val = if matches.is_int_value() {
                    matches.into_int_value()
                } else {
                    self.builder.build_call(
                        self.module.get_function("mux_value_get_bool").expect("mux_value_get_bool must be declared"),
                        &[matches.into()],
                        "get_bool",
                    ).map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_bool should return a value")
                    .into_int_value()
                };

                let cond_bool = self.builder.build_int_compare(inkwell::IntPredicate::NE, bool_val, self.context.i32_type().const_int(0, false), "bool_cond").map_err(|e| e.to_string())?;
                self.builder.build_conditional_branch(cond_bool, found_bb, continue_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(found_bb);
                let found_elem = self.builder.build_call(
                    self.module.get_function("mux_optional_get_value").expect("mux_optional_get_value must be declared"),
                    &[elem_opt.into()],
                    "found_elem",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_optional_get_value should return a value");

                let optional_some = self.builder.build_call(
                    self.module.get_function("mux_optional_some_value").expect("mux_optional_some_value must be declared"),
                    &[found_elem.into()],
                    "optional_some",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_optional_some_value should return a value");

                let result_boxed = self.builder.build_call(
                    self.module.get_function("mux_optional_into_value").expect("mux_optional_into_value must be declared"),
                    &[optional_some.into()],
                    "result_boxed",
                ).map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .expect("mux_optional_into_value should return a value");

                let result_alloca = self.builder.build_alloca(result_boxed.get_type(), "find_result").map_err(|e| e.to_string())?;
                self.builder.build_store(result_alloca, result_boxed).map_err(|e| e.to_string())?;

                self.builder.build_unconditional_branch(merge_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(continue_bb);
                let next_index = self.builder.build_int_add(current_index, one, "next_index").map_err(|e| e.to_string())?;
                self.builder.build_store(index_alloca, next_index).map_err(|e| e.to_string())?;
                self.builder.build_unconditional_branch(loop_bb).map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_bb);
                let first_bb = merge_bb.get_first_basic_block().ok_or("No first basic block")?;
                if first_bb == found_bb {
                    let result_phi = self.builder.build_phi(result_boxed.get_type(), "find_result_phi").map_err(|e| e.to_string())?;
                    let optional_none = self.builder.build_call(
                        self.module.get_function("mux_optional_none").expect("mux_optional_none must be declared"),
                        &[],
                        "optional_none",
                    ).map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_none should return a value");

                    let none_boxed = self.builder.build_call(
                        self.module.get_function("mux_optional_into_value").expect("mux_optional_into_value must be declared"),
                        &[optional_none.into()],
                        "none_boxed",
                    ).map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a value");

                    result_phi.add_incoming(&[
                        (&result_boxed, found_bb),
                        (&none_boxed, loop_bb),
                    ]);
                    Ok(result_phi.as_basic_value())
                } else {
                    let optional_none = self.builder.build_call(
                        self.module.get_function("mux_optional_none").expect("mux_optional_none must be declared"),
                        &[],
                        "optional_none",
                    ).map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_none should return a value");

                    let none_boxed = self.builder.build_call(
                        self.module.get_function("mux_optional_into_value").expect("mux_optional_into_value must be declared"),
                        &[optional_none.into()],
                        "none_boxed",
                    ).map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a value");

                    Ok(none_boxed)
                }
            }
            _ => Err(format!("Higher-order method {} not implemented for lists", method_name)),
        }
    }

    fn call_closure_with_args(
        &mut self,
        closure_ptr: BasicValueEnum<'a>,
        args: &[BasicValueEnum<'a>],
    ) -> Result<BasicValueEnum<'a>, String> {
        let closure_struct_type = self
            .context
            .struct_type(&[self.context.i64_type().into(), self.context.i64_type().into()], false);

        let fn_ptr_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_ptr.into_pointer_value(), 0, "fn_ptr_field")
            .map_err(|e| e.to_string())?;
        let fn_ptr = self.builder.build_load(self.context.i64_type(), fn_ptr_field, "fn_ptr").map_err(|e| e.to_string())?;

        let captures_ptr_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_ptr.into_pointer_value(), 1, "captures_field")
            .map_err(|e| e.to_string())?;
        let captures_ptr = self.builder.build_load(self.context.i64_type(), captures_ptr_field, "captures_ptr").map_err(|e| e.to_string())?;

        let fn_type = self.context.i64_type().fn_type(
            &std::iter::once(self.context.i64_type().into())
                .chain(args.iter().map(|_| self.context.i64_type().into()))
                .collect::<Vec<_>>(),
            false,
        );

        let fn_ptr_cast = self.builder.build_int_to_ptr(fn_ptr.into_int_value(), fn_type.ptr_type(inkwell::AddressSpace::default()), "fn_ptr_cast").map_err(|e| e.to_string())?;

        let mut call_args = vec![captures_ptr.into()];
        call_args.extend(args.iter().map(|a| {
            if a.is_pointer_value() {
                (*a).into()
            } else {
                self.box_value(*a).into()
            }
        }));

        let call = self.builder.build_call(fn_ptr_cast, &call_args, "closure_call").map_err(|e| e.to_string())?;

        Ok(call.try_as_basic_value().left().ok_or("Closure call should return a value")?)
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
                        &[call
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_to_string should return a basic value")
                            .into()],
                        "new_str",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call2
                    .try_as_basic_value()
                    .left()
                    .expect("mux_new_string_from_cstr should return a basic value"))
            }
            "put" => {
                if args.len() != 2 {
                    return Err("put() method takes exactly 2 arguments".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let value_val = self.generate_expression(&args[1])?;
                // Use mux_map_put_value which modifies the boxed Value directly
                let boxed_key = self.box_value(key_val);
                let boxed_value = self.box_value(value_val);
                self.builder
                    .build_call(
                        self.module
                            .get_function("mux_map_put_value")
                            .expect("mux_map_put_value must be declared in runtime"),
                        &[obj_value.into(), boxed_key.into(), boxed_value.into()],
                        "map_put_value",
                    )
                    .map_err(|e| e.to_string())?;
                // put() returns nothing (void), return a dummy value
                Ok(self.context.i64_type().const_int(0, false).into())
            }
            "get" => {
                if args.len() != 1 {
                    return Err("get() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let map_get_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_get")
                            .expect("mux_map_get must be declared in runtime"),
                        &[extract_map.into(), key_val.into()],
                        "map_get",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_get should return a basic value");
                let optional_value = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_into_value")
                            .expect("mux_optional_into_value must be declared in runtime"),
                        &[map_get_result.into()],
                        "optional_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a basic value");
                Ok(optional_value)
            }
            "get_keys" => {
                if !args.is_empty() {
                    return Err("get_keys() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_keys")
                            .expect("mux_map_keys must be declared in runtime"),
                        &[extract_map.into()],
                        "map_keys",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_keys should return a basic value"))
            }
            "get_values" => {
                if !args.is_empty() {
                    return Err("get_values() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_values")
                            .expect("mux_map_values must be declared in runtime"),
                        &[extract_map.into()],
                        "map_values",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_values should return a basic value"))
            }
            "get_pairs" => {
                if !args.is_empty() {
                    return Err("get_pairs() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_pairs")
                            .expect("mux_map_pairs must be declared in runtime"),
                        &[extract_map.into()],
                        "map_pairs",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_pairs should return a basic value"))
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_contains")
                            .expect("mux_map_contains must be declared in runtime"),
                        &[extract_map.into(), key_val.into()],
                        "map_contains",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_contains should return a basic value"))
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_size")
                            .expect("mux_map_size must be declared in runtime"),
                        &[extract_map.into()],
                        "map_size",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_size should return a basic value"))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_is_empty")
                            .expect("mux_map_is_empty must be declared in runtime"),
                        &[extract_map.into()],
                        "map_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_is_empty should return a basic value"))
            }
            "remove" => {
                if args.len() != 1 {
                    return Err("remove() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let key_ptr = self.box_value(key_val);

                let optional_ptr = self
                    .generate_runtime_call(
                        "mux_map_remove_value",
                        &[obj_value.into(), key_ptr.into()],
                    )
                    .ok_or("mux_map_remove_value should return a value")?;

                let optional_value = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_into_value")
                            .expect("mux_optional_into_value must be declared in runtime"),
                        &[optional_ptr.into()],
                        "optional_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a basic value");
                Ok(optional_value)
            }
            "keys" => {
                if !args.is_empty() {
                    return Err("keys() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_keys")
                            .expect("mux_map_keys must be declared in runtime"),
                        &[extract_map.into()],
                        "map_keys",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_keys should return a basic value"))
            }
            "values" => {
                if !args.is_empty() {
                    return Err("values() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_values")
                            .expect("mux_map_values must be declared in runtime"),
                        &[extract_map.into()],
                        "map_values",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_values should return a basic value"))
            }
            "contains_key" => {
                if args.len() != 1 {
                    return Err("contains_key() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_contains")
                            .expect("mux_map_contains must be declared in runtime"),
                        &[extract_map.into(), key_val.into()],
                        "map_contains_key",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_contains should return a basic value"))
            }
            "clear" => {
                if !args.is_empty() {
                    return Err("clear() method takes no arguments".to_string());
                }
                let extract_map = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_map")
                            .expect("mux_value_get_map must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_map",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_map should return a basic value");
                self.builder
                    .build_call(
                        self.module
                            .get_function("mux_map_clear")
                            .expect("mux_map_clear must be declared in runtime"),
                        &[extract_map.into()],
                        "map_clear",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(self.context.i32_type().const_int(0, false).into())
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
                        &[call
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_to_string should return a basic value")
                            .into()],
                        "new_str",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call2
                    .try_as_basic_value()
                    .left()
                    .expect("mux_new_string_from_cstr should return a basic value"))
            }
            "add" => {
                if args.len() != 1 {
                    return Err("add() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);

                // Use mux_set_add_value which modifies the boxed Value directly
                self.generate_runtime_call(
                    "mux_set_add_value",
                    &[obj_value.into(), elem_ptr.into()],
                );
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "remove" => {
                if args.len() != 1 {
                    return Err("remove() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);

                // Use mux_set_remove_value which modifies the boxed Value directly
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_remove_value")
                            .expect("mux_set_remove_value must be declared in runtime"),
                        &[obj_value.into(), elem_ptr.into()],
                        "set_remove_value",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_remove_value should return a basic value"))
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                let extract_set = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_set",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_contains")
                            .expect("mux_set_contains must be declared in runtime"),
                        &[extract_set.into(), elem_ptr.into()],
                        "set_contains",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_contains should return a basic value"))
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let extract_set = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_set",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_size")
                            .expect("mux_set_size must be declared in runtime"),
                        &[extract_set.into()],
                        "set_size",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_size should return a basic value"))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let extract_set = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_set",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_is_empty")
                            .expect("mux_set_is_empty must be declared in runtime"),
                        &[extract_set.into()],
                        "set_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_is_empty should return a basic value"))
            }
            "union" => {
                if args.len() != 1 {
                    return Err("union() method takes exactly 1 argument".to_string());
                }
                let other_set = self.generate_expression(&args[0])?;
                let extract_self = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_self",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let extract_other = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[other_set.into()],
                        "extract_other",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let result_set = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_union")
                            .expect("mux_set_union must be declared in runtime"),
                        &[extract_self.into(), extract_other.into()],
                        "set_union",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_union should return a basic value");
                let boxed_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_value")
                            .expect("mux_set_value must be declared in runtime"),
                        &[result_set.into()],
                        "boxed_set",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_value should return a basic value");
                Ok(boxed_result)
            }
            "intersection" => {
                if args.len() != 1 {
                    return Err("intersection() method takes exactly 1 argument".to_string());
                }
                let other_set = self.generate_expression(&args[0])?;
                let extract_self = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_self",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let extract_other = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[other_set.into()],
                        "extract_other",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let result_set = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_intersection")
                            .expect("mux_set_intersection must be declared in runtime"),
                        &[extract_self.into(), extract_other.into()],
                        "set_intersection",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_intersection should return a basic value");
                let boxed_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_value")
                            .expect("mux_set_value must be declared in runtime"),
                        &[result_set.into()],
                        "boxed_set",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_value should return a basic value");
                Ok(boxed_result)
            }
            "difference" => {
                if args.len() != 1 {
                    return Err("difference() method takes exactly 1 argument".to_string());
                }
                let other_set = self.generate_expression(&args[0])?;
                let extract_self = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_self",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let extract_other = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_set")
                            .expect("mux_value_get_set must be declared in runtime"),
                        &[other_set.into()],
                        "extract_other",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_set should return a basic value");
                let result_set = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_difference")
                            .expect("mux_set_difference must be declared in runtime"),
                        &[extract_self.into(), extract_other.into()],
                        "set_difference",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_difference should return a basic value");
                let boxed_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_value")
                            .expect("mux_set_value must be declared in runtime"),
                        &[result_set.into()],
                        "boxed_set",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_value should return a basic value");
                Ok(boxed_result)
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
                        &[call
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_to_string should return a basic value")
                            .into()],
                        "new_str",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call2
                    .try_as_basic_value()
                    .left()
                    .expect("mux_new_string_from_cstr should return a basic value"))
            }
            "is_some" => {
                if !args.is_empty() {
                    return Err("is_some() method takes no arguments".to_string());
                }
                let extract_optional = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_optional")
                            .expect("mux_value_get_optional must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_optional",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_optional should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_is_some")
                            .expect("mux_optional_is_some must be declared in runtime"),
                        &[extract_optional.into()],
                        "is_some",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_is_some should return a basic value"))
            }
            "is_none" => {
                if !args.is_empty() {
                    return Err("is_none() method takes no arguments".to_string());
                }
                let extract_optional = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_get_optional")
                            .expect("mux_value_get_optional must be declared in runtime"),
                        &[obj_value.into()],
                        "extract_optional",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_get_optional should return a basic value");
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_is_none")
                            .expect("mux_optional_is_none must be declared in runtime"),
                        &[extract_optional.into()],
                        "is_none",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_is_none should return a basic value"))
            }
            _ => Err(format!(
                "Method {} not implemented for Optionals",
                method_name
            )),
        }
    }

    fn generate_tuple_method_call(
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
                let tuple_ptr = if obj_value.is_pointer_value() {
                    let get_tuple_fn = self
                        .module
                        .get_function("mux_value_get_tuple")
                        .ok_or("mux_value_get_tuple not found")?;
                    self.builder
                        .build_call(get_tuple_fn, &[obj_value.into()], "get_tuple")
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .ok_or("mux_value_get_tuple should return a value")?
                        .into_pointer_value()
                } else {
                    return Err("Tuple method receiver must be a pointer value".to_string());
                };
                let func = self
                    .module
                    .get_function("mux_tuple_to_string")
                    .ok_or("mux_tuple_to_string not found")?;
                let call = self
                    .builder
                    .build_call(func, &[tuple_ptr.into()], "tuple_to_str")
                    .map_err(|e| e.to_string())?;
                let func_new = self
                    .module
                    .get_function("mux_new_string_from_cstr")
                    .ok_or("mux_new_string_from_cstr not found")?;
                let call2 = self
                    .builder
                    .build_call(
                        func_new,
                        &[call
                            .try_as_basic_value()
                            .left()
                            .expect("mux_tuple_to_string should return a basic value")
                            .into()],
                        "new_str",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call2
                    .try_as_basic_value()
                    .left()
                    .expect("mux_new_string_from_cstr should return a basic value"))
            }
            _ => Err(format!("Method {} not implemented for tuples", method_name)),
        }
    }
}
