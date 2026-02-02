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
                            &[call
                                .try_as_basic_value()
                                .left()
                                .expect("mux_float_to_string should return a basic value")
                                .into()],
                            "new_str",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(call2
                        .try_as_basic_value()
                        .left()
                        .expect("mux_new_string_from_cstr should return a basic value"))
                }
                "to_float" => {
                    // Convert raw i64 to f64 using LLVM sitofp instruction
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
                            &[call
                                .try_as_basic_value()
                                .left()
                                .expect("mux_float_to_string should return a basic value")
                                .into()],
                            "new_str",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(call2
                        .try_as_basic_value()
                        .left()
                        .expect("mux_new_string_from_cstr should return a basic value"))
                }
                "to_int" => {
                    // float.to_int() - direct LLVM conversion using fptosi
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
                            &[call
                                .try_as_basic_value()
                                .left()
                                .expect("mux_bool_to_string should return a basic value")
                                .into()],
                            "new_str",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(call2
                        .try_as_basic_value()
                        .left()
                        .expect("mux_new_string_from_cstr should return a basic value"))
                }
                "to_int" => {
                    // bool.to_int() - direct LLVM conversion
                    // obj_value is i32 (0 or 1), extend to i64
                    let bool_i32 = obj_value.into_int_value();
                    let int_val = self
                        .builder
                        .build_int_z_extend(bool_i32, self.context.i64_type(), "bool_to_int")
                        .map_err(|e| e.to_string())?;
                    Ok(int_val.into())
                }
                "to_float" => {
                    // bool.to_float() - direct LLVM conversion
                    // obj_value is i32 (0 or 1), convert to f64
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
                "to_string" => {
                    // char.to_string() - call mux_char_to_string
                    let func = self
                        .module
                        .get_function("mux_char_to_string")
                        .ok_or("mux_char_to_string not found")?;
                    let call = self
                        .builder
                        .build_call(func, &[obj_value.into()], "char_to_str")
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
                                .expect("mux_char_to_string should return a basic value")
                                .into()],
                            "new_str",
                        )
                        .map_err(|e| e.to_string())?;
                    Ok(call2
                        .try_as_basic_value()
                        .left()
                        .expect("mux_new_string_from_cstr should return a basic value"))
                }
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
                            .get_function("mux_value_from_optional")
                            .expect("mux_value_from_optional must be declared in runtime"),
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
                    .expect("mux_value_from_optional should return a basic value"))
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
                            .expect("mux_list_pop_front_value must be declared in runtime"),
                        &[obj_value.into()],
                        "list_pop_front",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_list_pop_front_value should return a basic value"))
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
            "length" => {
                if !args.is_empty() {
                    return Err("length() method takes no arguments".to_string());
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
                        "list_length",
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
                            .get_function("mux_map_put")
                            .expect("mux_map_put must be declared in runtime"),
                        &[extract_map.into(), key_val.into(), value_val.into()],
                        "map_put",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_put should return a basic value"))
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
                            .get_function("mux_value_from_optional")
                            .expect("mux_value_from_optional must be declared in runtime"),
                        &[map_get_result.into()],
                        "optional_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_from_optional should return a basic value");
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
                let map_remove_result = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_map_remove")
                            .expect("mux_map_remove must be declared in runtime"),
                        &[extract_map.into(), key_val.into()],
                        "map_remove",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_map_remove should return a basic value");
                let optional_value = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_value_from_optional")
                            .expect("mux_value_from_optional must be declared in runtime"),
                        &[map_remove_result.into()],
                        "optional_value",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_value_from_optional should return a basic value");
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
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_add")
                            .expect("mux_set_add must be declared in runtime"),
                        &[obj_value.into(), elem_val.into()],
                        "set_add",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_add should return a basic value"))
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_contains")
                            .expect("mux_set_contains must be declared in runtime"),
                        &[obj_value.into(), elem_val.into()],
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
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_size")
                            .expect("mux_set_size must be declared in runtime"),
                        &[obj_value.into()],
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
                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_set_is_empty")
                            .expect("mux_set_is_empty must be declared in runtime"),
                        &[obj_value.into()],
                        "set_is_empty",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_set_is_empty should return a basic value"))
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
            _ => Err(format!(
                "Method {} not implemented for Optionals",
                method_name
            )),
        }
    }
}
