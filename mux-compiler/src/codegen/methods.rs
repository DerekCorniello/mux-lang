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
    fn call_cstr_to_mux_string(
        &self,
        cstr_ptr: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let new_string = self
            .module
            .get_function("mux_new_string_from_cstr")
            .ok_or("mux_new_string_from_cstr not found")?;
        let call = self
            .builder
            .build_call(new_string, &[cstr_ptr.into()], "new_string")
            .map_err(|e| e.to_string())?;
        Ok(call
            .try_as_basic_value()
            .left()
            .expect("mux_new_string_from_cstr should return a basic value"))
    }

    fn call_runtime_function(
        &self,
        func_name: &str,
        args: &[BasicValueEnum<'a>],
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function(func_name)
            .ok_or(format!("Function '{}' not found", func_name))?;
        let call = self
            .builder
            .build_call(
                func,
                &args.iter().map(|v| (*v).into()).collect::<Vec<_>>(),
                "call",
            )
            .map_err(|e| e.to_string())?;
        call.try_as_basic_value()
            .left()
            .ok_or_else(|| format!("{} should return a basic value", func_name))
    }

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
        self.call_cstr_to_mux_string(cstr)
    }

    fn call_runtime_to_string_from_call(
        &self,
        call: inkwell::values::CallSiteValue<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let cstr = call
            .try_as_basic_value()
            .left()
            .ok_or_else(|| "Function should return a basic value".to_string())?;
        self.call_cstr_to_mux_string(cstr)
    }

    fn generate_to_string_call(
        &self,
        obj_value: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function("mux_value_to_string")
            .ok_or("mux_value_to_string not found")?;
        let call = self
            .builder
            .build_call(func, &[obj_value.into()], "val_to_str")
            .map_err(|e| e.to_string())?;
        let cstr = call
            .try_as_basic_value()
            .left()
            .expect("mux_value_to_string should return a basic value");
        self.call_cstr_to_mux_string(cstr)
    }

    fn extract_raw_pointer(
        &self,
        obj_value: BasicValueEnum<'a>,
        getter_func: &str,
        extract_name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        let raw = self
            .builder
            .build_call(
                self.module
                    .get_function(getter_func)
                    .ok_or(format!("{} not found", getter_func))?,
                &[obj_value.into()],
                extract_name,
            )
            .map_err(|e| e.to_string())?;
        raw.try_as_basic_value()
            .left()
            .ok_or_else(|| format!("{} should return a basic value", getter_func))
    }

    fn call_string_conversion_func(
        &self,
        obj_value: BasicValueEnum<'a>,
        conversion_func: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
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
        let func = self
            .module
            .get_function(conversion_func)
            .ok_or(format!("{} not found", conversion_func))?;
        let call = self
            .builder
            .build_call(func, &[cstr.into()], "str_conv")
            .map_err(|e| e.to_string())?;
        Ok(call
            .try_as_basic_value()
            .left()
            .unwrap_or_else(|| panic!("{} should return a basic value", conversion_func)))
    }

    fn call_unary_predicate(
        &self,
        obj_value: BasicValueEnum<'a>,
        func_name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function(func_name)
            .ok_or(format!("{} not found", func_name))?;
        let call = self
            .builder
            .build_call(func, &[obj_value.into()], "predicate_call")
            .map_err(|e| e.to_string())?;
        Ok(call
            .try_as_basic_value()
            .left()
            .unwrap_or_else(|| panic!("{} should return a basic value", func_name)))
    }

    pub(super) fn build_net_call(
        &mut self,
        func_name: &str,
        args: &[BasicValueEnum<'a>],
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function(func_name)
            .ok_or(format!("{} not found", func_name))?;
        let metadata_args = args.iter().map(|v| (*v).into()).collect::<Vec<_>>();
        let call = self
            .builder
            .build_call(
                func,
                &metadata_args,
                &format!("{}_call", func_name.replace('.', "_")),
            )
            .map_err(|e| e.to_string())?;
        if let Some(value) = call.try_as_basic_value().left() {
            Ok(value)
        } else {
            Ok(self.context.i32_type().const_int(0, false).into())
        }
    }

    pub(super) fn bool_to_i32(
        &mut self,
        value: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let int_value = if value.is_int_value() {
            value.into_int_value()
        } else if value.is_pointer_value() {
            self.extract_raw_pointer(value, "mux_value_get_bool", "extract_bool")?
                .into_int_value()
        } else {
            return Err("Expected boolean argument".to_string());
        };
        let extended = self
            .builder
            .build_int_z_extend(int_value, self.context.i32_type(), "bool_to_i32")
            .map_err(|e| e.to_string())?;
        Ok(extended.into())
    }

    pub(super) fn try_generate_net_static_method_call(
        &mut self,
        class_name: &str,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match (class_name, method_name) {
            ("TcpStream", "connect") => {
                if args.len() != 1 {
                    return Err("TcpStream.connect takes exactly 1 argument".to_string());
                }
                let addr = self.generate_expression(&args[0])?;
                let call = self.build_net_call("mux_net_tcp_connect", &[addr])?;
                Ok(Some(call))
            }
            ("UdpSocket", "bind") => {
                if args.len() != 1 {
                    return Err("UdpSocket.bind takes exactly 1 argument".to_string());
                }
                let addr = self.generate_expression(&args[0])?;
                let call = self.build_net_call("mux_net_udp_bind", &[addr])?;
                Ok(Some(call))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn try_generate_net_instance_method_call(
        &mut self,
        obj_value: BasicValueEnum<'a>,
        obj_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let type_name = if let Type::Named(name, _) = obj_type {
            name
        } else {
            return Ok(None);
        };

        match type_name.as_str() {
            "TcpStream" => match method_name {
                "read" => {
                    if args.len() != 1 {
                        return Err("TcpStream.read takes exactly 1 argument".to_string());
                    }
                    let size = self.generate_expression(&args[0])?;
                    let call = self.build_net_call("mux_net_tcp_read", &[obj_value.into(), size])?;
                    Ok(Some(call))
                }
                "write" => {
                    if args.len() != 1 {
                        return Err("TcpStream.write takes exactly 1 argument".to_string());
                    }
                    let data = self.generate_expression(&args[0])?;
                    let call =
                        self.build_net_call("mux_net_tcp_write", &[obj_value.into(), data])?;
                    Ok(Some(call))
                }
                "close" => {
                    if !args.is_empty() {
                        return Err("TcpStream.close takes no arguments".to_string());
                    }
                    let call = self.build_net_call("mux_net_tcp_close", &[obj_value.into()])?;
                    Ok(Some(call))
                }
                "set_nonblocking" => {
                    if args.len() != 1 {
                        return Err("TcpStream.set_nonblocking takes 1 argument".to_string());
                    }
                    let bool_val = self.generate_expression(&args[0])?;
                    let converted = self.bool_to_i32(bool_val)?;
                    let call = self.build_net_call("mux_net_tcp_set_nonblocking", &[obj_value.into(), converted])?;
                    Ok(Some(call))
                }
                "peer_addr" => {
                    if !args.is_empty() {
                        return Err("TcpStream.peer_addr takes no arguments".to_string());
                    }
                    let call = self.build_net_call("mux_net_tcp_peer_addr", &[obj_value.into()])?;
                    Ok(Some(call))
                }
                "local_addr" => {
                    if !args.is_empty() {
                        return Err("TcpStream.local_addr takes no arguments".to_string());
                    }
                    let call = self.build_net_call("mux_net_tcp_local_addr", &[obj_value.into()])?;
                    Ok(Some(call))
                }
                _ => Ok(None),
            },
            "UdpSocket" => match method_name {
                "send_to" => {
                    if args.len() != 2 {
                        return Err("UdpSocket.send_to takes 2 arguments".to_string());
                    }
                    let data = self.generate_expression(&args[0])?;
                    let addr = self.generate_expression(&args[1])?;
                    let call = self.build_net_call(
                        "mux_net_udp_send_to",
                        &[obj_value.into(), data, addr],
                    )?;
                    Ok(Some(call))
                }
                "recv_from" => {
                    if args.len() != 1 {
                        return Err("UdpSocket.recv_from takes 1 argument".to_string());
                    }
                    let size = self.generate_expression(&args[0])?;
                    let call =
                        self.build_net_call("mux_net_udp_recv_from", &[obj_value.into(), size])?;
                    Ok(Some(call))
                }
                "close" => {
                    if !args.is_empty() {
                        return Err("UdpSocket.close takes no arguments".to_string());
                    }
                    let call = self.build_net_call("mux_net_udp_close", &[obj_value.into()])?;
                    Ok(Some(call))
                }
                "set_nonblocking" => {
                    if args.len() != 1 {
                        return Err("UdpSocket.set_nonblocking takes 1 argument".to_string());
                    }
                    let bool_val = self.generate_expression(&args[0])?;
                    let converted = self.bool_to_i32(bool_val)?;
                    let call = self.build_net_call(
                        "mux_net_udp_set_nonblocking",
                        &[obj_value.into(), converted],
                    )?;
                    Ok(Some(call))
                }
                _ => Ok(None),
            },
            _ => Ok(None),
        }
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

        if let Some(call) = self.try_generate_net_instance_method_call(
            obj_value,
            &resolved_obj_type,
            method_name,
            args,
        )? {
            return Ok(call);
        }

        match &resolved_obj_type {
            Type::Primitive(prim) => {
                self.generate_primitive_method_call(obj_value, prim, method_name)
            }
            Type::List(_) => self.generate_list_method_call(obj_value, method_name, args),
            Type::Map(_, _) => self.generate_map_method_call(obj_value, method_name, args),
            Type::Set(_) => self.generate_set_method_call(obj_value, method_name, args),
            Type::Tuple(_, _) => self.generate_tuple_method_call(obj_value, method_name, args),
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
            Type::Result(_, _) => self.generate_result_method_call(obj_value, method_name, args),
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
                "message" => Ok(obj_value),
                "to_int" => self.call_string_conversion_func(obj_value, "mux_string_to_int"),
                "to_float" => self.call_string_conversion_func(obj_value, "mux_string_to_float"),
                "length" => self.call_runtime_function("mux_string_length", &[obj_value]),
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
                "to_int" => self.call_runtime_function("mux_char_to_int", &[obj_value]),
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

                let raw_list =
                    self.extract_raw_pointer(obj_value, "mux_value_get_list", "extract_list")?;

                let call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_list_get")
                            .expect("mux_list_get must be declared in runtime"),
                        &[raw_list.into(), index_val.into()],
                        "list_get",
                    )
                    .map_err(|e| e.to_string())?;
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
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "pop_back" => {
                if !args.is_empty() {
                    return Err("pop_back() method takes no arguments".to_string());
                }
                self.call_runtime_function("mux_list_pop_back_value", &[obj_value])
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
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "pop" => {
                if !args.is_empty() {
                    return Err("pop() method takes no arguments".to_string());
                }
                self.call_runtime_function("mux_list_pop_value", &[obj_value])
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let raw_list =
                    self.extract_raw_pointer(obj_value, "mux_value_get_list", "extract_list")?;
                self.call_runtime_function("mux_list_is_empty", &[raw_list])
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let raw_list =
                    self.extract_raw_pointer(obj_value, "mux_value_get_list", "extract_list")?;
                self.call_runtime_function("mux_list_length", &[raw_list])
            }
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                let raw_list =
                    self.extract_raw_pointer(obj_value, "mux_value_get_list", "extract_list")?;
                let cstr = self.call_runtime_function("mux_list_to_string", &[raw_list])?;
                self.call_cstr_to_mux_string(cstr)
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
                self.generate_to_string_call(obj_value)
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
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
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
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
                self.call_runtime_function("mux_map_keys", &[extract_map])
            }
            "get_values" => {
                if !args.is_empty() {
                    return Err("get_values() method takes no arguments".to_string());
                }
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
                self.call_runtime_function("mux_map_values", &[extract_map])
            }
            "get_pairs" => {
                if !args.is_empty() {
                    return Err("get_pairs() method takes no arguments".to_string());
                }
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
                self.call_runtime_function("mux_map_pairs", &[extract_map])
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let key_val = self.generate_expression(&args[0])?;
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
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
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
                self.call_runtime_function("mux_map_size", &[extract_map])
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let extract_map =
                    self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?;
                self.call_runtime_function("mux_map_is_empty", &[extract_map])
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
                self.generate_to_string_call(obj_value)
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
                self.call_runtime_function("mux_set_remove_value", &[obj_value, elem_ptr.into()])
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                let extract_set =
                    self.extract_raw_pointer(obj_value, "mux_value_get_set", "extract_set")?;
                self.call_runtime_function("mux_set_contains", &[extract_set, elem_ptr.into()])
            }
            "size" => {
                if !args.is_empty() {
                    return Err("size() method takes no arguments".to_string());
                }
                let extract_set =
                    self.extract_raw_pointer(obj_value, "mux_value_get_set", "extract_set")?;
                self.call_runtime_function("mux_set_size", &[extract_set])
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty() method takes no arguments".to_string());
                }
                let extract_set =
                    self.extract_raw_pointer(obj_value, "mux_value_get_set", "extract_set")?;
                self.call_runtime_function("mux_set_is_empty", &[extract_set])
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
                self.generate_to_string_call(obj_value)
            }
            "is_some" => {
                if !args.is_empty() {
                    return Err("is_some() method takes no arguments".to_string());
                }
                self.call_unary_predicate(obj_value, "mux_optional_is_some")
            }
            "is_none" => {
                if !args.is_empty() {
                    return Err("is_none() method takes no arguments".to_string());
                }
                self.call_unary_predicate(obj_value, "mux_optional_is_none")
            }
            _ => Err(format!(
                "Method {} not implemented for Optionals",
                method_name
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
            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string() method takes no arguments".to_string());
                }
                self.generate_to_string_call(obj_value)
            }
            "is_ok" => {
                if !args.is_empty() {
                    return Err("is_ok() method takes no arguments".to_string());
                }
                self.call_unary_predicate(obj_value, "mux_result_is_ok")
            }
            "is_err" => {
                if !args.is_empty() {
                    return Err("is_err() method takes no arguments".to_string());
                }
                self.call_unary_predicate(obj_value, "mux_result_is_err")
            }
            _ => Err(format!(
                "Method {} not implemented for Results",
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
                    self.extract_raw_pointer(obj_value, "mux_value_get_tuple", "get_tuple")?
                        .into_pointer_value()
                } else {
                    return Err("Tuple method receiver must be a pointer value".to_string());
                };
                let cstr =
                    self.call_runtime_function("mux_tuple_to_string", &[tuple_ptr.into()])?;
                self.call_cstr_to_mux_string(cstr)
            }
            _ => Err(format!("Method {} not implemented for tuples", method_name)),
        }
    }
}
