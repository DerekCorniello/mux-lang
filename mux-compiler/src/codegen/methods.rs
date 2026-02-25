//! Method call generation for the code generator.
//!
//! This module handles:
//! - Instance method calls on user-defined classes
//! - Primitive type method calls (int, float, str, bool, char)
//! - Collection method calls (list, map, set)
//! - Optional method calls

use inkwell::AddressSpace;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

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

    fn unbox_value_for_type(
        &mut self,
        value_ptr: BasicValueEnum<'a>,
        expected_type: &Type,
    ) -> Result<BasicValueEnum<'a>, String> {
        match expected_type {
            Type::Primitive(PrimitiveType::Int) => Ok(self.get_raw_int_value(value_ptr)?.into()),
            Type::Primitive(PrimitiveType::Float) => {
                Ok(self.get_raw_float_value(value_ptr)?.into())
            }
            Type::Primitive(PrimitiveType::Bool) => Ok(self.get_raw_bool_value(value_ptr)?.into()),
            Type::Primitive(PrimitiveType::Char) => Ok(self.get_raw_int_value(value_ptr)?.into()),
            _ => Ok(value_ptr),
        }
    }

    fn call_function_value(
        &mut self,
        func_value: BasicValueEnum<'a>,
        func_type: &Type,
        args: &[BasicValueEnum<'a>],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let (params, returns) = match func_type {
            Type::Function {
                params, returns, ..
            } => (params, returns),
            _ => return Err("Expected function type for callable value".to_string()),
        };

        let closure_ptr = func_value.into_pointer_value();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let closure_struct_type = self
            .context
            .struct_type(&[ptr_type.into(), ptr_type.into()], false);

        let fn_ptr_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_ptr, 0, "fn_ptr_field")
            .map_err(|e| e.to_string())?;
        let func_ptr = self
            .builder
            .build_load(ptr_type, fn_ptr_field, "fn_ptr")
            .map_err(|e| e.to_string())?
            .into_pointer_value();

        let captures_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_ptr, 1, "captures_field")
            .map_err(|e| e.to_string())?;
        let captures_ptr = self
            .builder
            .build_load(ptr_type, captures_field, "captures_ptr")
            .map_err(|e| e.to_string())?
            .into_pointer_value();

        let is_null = self
            .builder
            .build_is_null(captures_ptr, "captures_is_null")
            .map_err(|e| e.to_string())?;

        let mut param_types_without: Vec<BasicMetadataTypeEnum> = Vec::new();
        for param in params {
            let type_node = self.type_to_type_node(param);
            param_types_without.push(self.llvm_type_from_mux_type(&type_node)?.into());
        }

        let mut param_types_with: Vec<BasicMetadataTypeEnum> = vec![ptr_type.into()];
        param_types_with.extend(param_types_without.clone());

        let fn_type_with = if matches!(returns.as_ref(), Type::Void) {
            self.context.void_type().fn_type(&param_types_with, false)
        } else {
            let return_type_node = self.type_to_type_node(returns);
            let return_type = self.llvm_type_from_mux_type(&return_type_node)?;
            return_type.fn_type(&param_types_with, false)
        };

        let fn_type_without = if matches!(returns.as_ref(), Type::Void) {
            self.context
                .void_type()
                .fn_type(&param_types_without, false)
        } else {
            let return_type_node = self.type_to_type_node(returns);
            let return_type = self.llvm_type_from_mux_type(&return_type_node)?;
            return_type.fn_type(&param_types_without, false)
        };

        let user_args: Vec<BasicMetadataValueEnum> = args.iter().map(|v| (*v).into()).collect();
        let current_fn = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_parent())
            .ok_or("No current function")?;
        let with_captures_bb = self.context.append_basic_block(current_fn, "with_captures");
        let without_captures_bb = self
            .context
            .append_basic_block(current_fn, "without_captures");
        let merge_bb = self.context.append_basic_block(current_fn, "call_merge");

        self.builder
            .build_conditional_branch(is_null, without_captures_bb, with_captures_bb)
            .map_err(|e| e.to_string())?;

        self.builder.position_at_end(with_captures_bb);
        let mut args_with_captures: Vec<BasicMetadataValueEnum> = vec![captures_ptr.into()];
        args_with_captures.extend(user_args.clone());
        let call_with = self
            .builder
            .build_indirect_call(
                fn_type_with,
                func_ptr,
                &args_with_captures,
                "call_with_captures",
            )
            .map_err(|e| e.to_string())?;
        let result_with = call_with.try_as_basic_value().left();
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let with_end_bb = self
            .builder
            .get_insert_block()
            .ok_or("Missing insertion block after capture call")?;

        self.builder.position_at_end(without_captures_bb);
        let call_without = self
            .builder
            .build_indirect_call(
                fn_type_without,
                func_ptr,
                &user_args,
                "call_without_captures",
            )
            .map_err(|e| e.to_string())?;
        let result_without = call_without.try_as_basic_value().left();
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let without_end_bb = self
            .builder
            .get_insert_block()
            .ok_or("Missing insertion block after non-capture call")?;

        self.builder.position_at_end(merge_bb);
        match (result_with, result_without) {
            (Some(r1), Some(r2)) => {
                let phi = self
                    .builder
                    .build_phi(r1.get_type(), "call_result")
                    .map_err(|e| e.to_string())?;
                phi.add_incoming(&[(&r1, with_end_bb), (&r2, without_end_bb)]);
                Ok(Some(phi.as_basic_value()))
            }
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

        match &resolved_obj_type {
            Type::Primitive(prim) => {
                self.generate_primitive_method_call(obj_value, prim, method_name)
            }
            Type::List(_) => {
                self.generate_list_method_call(obj_value, &resolved_obj_type, method_name, args)
            }
            Type::Map(_, _) => {
                self.generate_map_method_call(obj_value, &resolved_obj_type, method_name, args)
            }
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
        obj_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let elem_type = match obj_type {
            Type::List(inner) => Some(inner.as_ref().clone()),
            _ => None,
        };
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
            "sort" => {
                if !args.is_empty() {
                    return Err("sort() method takes no arguments".to_string());
                }
                self.generate_runtime_call("mux_list_sort_value", &[obj_value.into()]);
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "reverse" => {
                if !args.is_empty() {
                    return Err("reverse() method takes no arguments".to_string());
                }
                self.generate_runtime_call("mux_list_reverse_value", &[obj_value.into()]);
                Ok(self.context.i32_type().const_int(0, false).into())
            }
            "contains" => {
                if args.len() != 1 {
                    return Err("contains() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                let raw_list =
                    self.extract_raw_pointer(obj_value, "mux_value_get_list", "extract_list")?;
                self.call_runtime_function("mux_list_contains", &[raw_list, elem_ptr.into()])
            }
            "index_of" => {
                if args.len() != 1 {
                    return Err("index_of() method takes exactly 1 argument".to_string());
                }
                let elem_val = self.generate_expression(&args[0])?;
                let elem_ptr = self.box_value(elem_val);
                let raw_list =
                    self.extract_raw_pointer(obj_value, "mux_value_get_list", "extract_list")?;
                let call =
                    self.call_runtime_function("mux_list_index_of", &[raw_list, elem_ptr.into()])?;
                let boxed_call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_into_value")
                            .expect("mux_optional_into_value must be declared in runtime"),
                        &[call.into()],
                        "optional_as_value",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(boxed_call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a basic value"))
            }
            "filter" => {
                if args.len() != 1 {
                    return Err("filter() method takes exactly 1 argument".to_string());
                }
                let list_elem_type = elem_type
                    .clone()
                    .ok_or("list element type missing for filter")?;
                let predicate_val = self.generate_expression(&args[0])?;
                let predicate_type = self
                    .analyzer
                    .get_expression_type(&args[0])
                    .map_err(|e| e.message)?;

                let result_list =
                    self.create_empty_collection_value("mux_new_list", "mux_list_value");
                let len = self
                    .call_runtime_function("mux_value_list_length", &[obj_value])?
                    .into_int_value();
                let i64_type = self.context.i64_type();
                let idx_alloca = self
                    .builder
                    .build_alloca(i64_type, "filter_idx")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, i64_type.const_zero())
                    .map_err(|e| e.to_string())?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or("No current function")?;
                let header_bb = self
                    .context
                    .append_basic_block(current_fn, "list_filter_header");
                let body_bb = self
                    .context
                    .append_basic_block(current_fn, "list_filter_body");
                let add_bb = self
                    .context
                    .append_basic_block(current_fn, "list_filter_add");
                let skip_bb = self
                    .context
                    .append_basic_block(current_fn, "list_filter_skip");
                let exit_bb = self
                    .context
                    .append_basic_block(current_fn, "list_filter_exit");
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(header_bb);
                let idx = self
                    .builder
                    .build_load(i64_type, idx_alloca, "filter_idx_val")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, idx, len, "filter_cond")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_conditional_branch(cond, body_bb, exit_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_ptr = self
                    .call_runtime_function("mux_value_list_get_value", &[obj_value, idx.into()])?;
                let typed_elem = self.unbox_value_for_type(elem_ptr, &list_elem_type)?;
                let predicate_result = self
                    .call_function_value(predicate_val, &predicate_type, &[typed_elem])?
                    .ok_or("filter predicate should return a value")?;
                let pred_bool = self.get_raw_bool_value(predicate_result)?;
                self.builder
                    .build_conditional_branch(pred_bool, add_bb, skip_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(add_bb);
                self.generate_runtime_call(
                    "mux_list_push_back_value",
                    &[result_list.into(), elem_ptr.into()],
                );
                self.builder
                    .build_unconditional_branch(skip_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(skip_bb);
                let next = self
                    .builder
                    .build_int_add(idx, i64_type.const_int(1, false), "filter_next")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, next)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(exit_bb);
                Ok(result_list)
            }
            "map" => {
                if args.len() != 1 {
                    return Err("map() method takes exactly 1 argument".to_string());
                }
                let list_elem_type = elem_type
                    .clone()
                    .ok_or("list element type missing for map")?;
                let transform_val = self.generate_expression(&args[0])?;
                let transform_type = self
                    .analyzer
                    .get_expression_type(&args[0])
                    .map_err(|e| e.message)?;

                let result_list =
                    self.create_empty_collection_value("mux_new_list", "mux_list_value");
                let len = self
                    .call_runtime_function("mux_value_list_length", &[obj_value])?
                    .into_int_value();
                let i64_type = self.context.i64_type();
                let idx_alloca = self
                    .builder
                    .build_alloca(i64_type, "map_idx")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, i64_type.const_zero())
                    .map_err(|e| e.to_string())?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or("No current function")?;
                let header_bb = self
                    .context
                    .append_basic_block(current_fn, "list_map_header");
                let body_bb = self.context.append_basic_block(current_fn, "list_map_body");
                let exit_bb = self.context.append_basic_block(current_fn, "list_map_exit");
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(header_bb);
                let idx = self
                    .builder
                    .build_load(i64_type, idx_alloca, "map_idx_val")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, idx, len, "map_cond")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_conditional_branch(cond, body_bb, exit_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_ptr = self
                    .call_runtime_function("mux_value_list_get_value", &[obj_value, idx.into()])?;
                let typed_elem = self.unbox_value_for_type(elem_ptr, &list_elem_type)?;
                let mapped = self
                    .call_function_value(transform_val, &transform_type, &[typed_elem])?
                    .ok_or("map transform should return a value")?;
                let mapped_ptr = self.box_value(mapped);
                self.generate_runtime_call(
                    "mux_list_push_back_value",
                    &[result_list.into(), mapped_ptr.into()],
                );

                let next = self
                    .builder
                    .build_int_add(idx, i64_type.const_int(1, false), "map_next")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, next)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(exit_bb);
                Ok(result_list)
            }
            "reduce" => {
                if args.len() != 2 {
                    return Err("reduce() method takes exactly 2 arguments".to_string());
                }
                let list_elem_type = elem_type
                    .clone()
                    .ok_or("list element type missing for reduce")?;
                let init_val = self.generate_expression(&args[0])?;
                let op_val = self.generate_expression(&args[1])?;
                let op_type = self
                    .analyzer
                    .get_expression_type(&args[1])
                    .map_err(|e| e.message)?;

                let len = self
                    .call_runtime_function("mux_value_list_length", &[obj_value])?
                    .into_int_value();
                let i64_type = self.context.i64_type();
                let idx_alloca = self
                    .builder
                    .build_alloca(i64_type, "reduce_idx")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, i64_type.const_zero())
                    .map_err(|e| e.to_string())?;

                let acc_alloca = self
                    .builder
                    .build_alloca(init_val.get_type(), "reduce_acc")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(acc_alloca, init_val)
                    .map_err(|e| e.to_string())?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or("No current function")?;
                let header_bb = self
                    .context
                    .append_basic_block(current_fn, "list_reduce_header");
                let body_bb = self
                    .context
                    .append_basic_block(current_fn, "list_reduce_body");
                let exit_bb = self
                    .context
                    .append_basic_block(current_fn, "list_reduce_exit");
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(header_bb);
                let idx = self
                    .builder
                    .build_load(i64_type, idx_alloca, "reduce_idx_val")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, idx, len, "reduce_cond")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_conditional_branch(cond, body_bb, exit_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_ptr = self
                    .call_runtime_function("mux_value_list_get_value", &[obj_value, idx.into()])?;
                let typed_elem = self.unbox_value_for_type(elem_ptr, &list_elem_type)?;
                let acc_cur = self
                    .builder
                    .build_load(init_val.get_type(), acc_alloca, "reduce_acc_cur")
                    .map_err(|e| e.to_string())?;
                let next_acc = self
                    .call_function_value(op_val, &op_type, &[acc_cur, typed_elem])?
                    .ok_or("reduce operation should return a value")?;
                self.builder
                    .build_store(acc_alloca, next_acc)
                    .map_err(|e| e.to_string())?;

                let next = self
                    .builder
                    .build_int_add(idx, i64_type.const_int(1, false), "reduce_next")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, next)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(exit_bb);
                self.builder
                    .build_load(init_val.get_type(), acc_alloca, "reduce_result")
                    .map_err(|e| e.to_string())
            }
            "find" => {
                if args.len() != 1 {
                    return Err("find() method takes exactly 1 argument".to_string());
                }
                let list_elem_type = elem_type
                    .clone()
                    .ok_or("list element type missing for find")?;
                let predicate_val = self.generate_expression(&args[0])?;
                let predicate_type = self
                    .analyzer
                    .get_expression_type(&args[0])
                    .map_err(|e| e.message)?;

                let len = self
                    .call_runtime_function("mux_value_list_length", &[obj_value])?
                    .into_int_value();
                let i64_type = self.context.i64_type();
                let idx_alloca = self
                    .builder
                    .build_alloca(i64_type, "find_idx")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, i64_type.const_zero())
                    .map_err(|e| e.to_string())?;
                let result_ptr_alloca = self
                    .builder
                    .build_alloca(self.context.ptr_type(AddressSpace::default()), "find_ptr")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(
                        result_ptr_alloca,
                        self.context.ptr_type(AddressSpace::default()).const_null(),
                    )
                    .map_err(|e| e.to_string())?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or("No current function")?;
                let header_bb = self
                    .context
                    .append_basic_block(current_fn, "list_find_header");
                let body_bb = self
                    .context
                    .append_basic_block(current_fn, "list_find_body");
                let found_bb = self
                    .context
                    .append_basic_block(current_fn, "list_find_found");
                let next_bb = self
                    .context
                    .append_basic_block(current_fn, "list_find_next");
                let exit_bb = self
                    .context
                    .append_basic_block(current_fn, "list_find_exit");
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(header_bb);
                let idx = self
                    .builder
                    .build_load(i64_type, idx_alloca, "find_idx_val")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, idx, len, "find_cond")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_conditional_branch(cond, body_bb, exit_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let elem_ptr = self
                    .call_runtime_function("mux_value_list_get_value", &[obj_value, idx.into()])?;
                let typed_elem = self.unbox_value_for_type(elem_ptr, &list_elem_type)?;
                let predicate_result = self
                    .call_function_value(predicate_val, &predicate_type, &[typed_elem])?
                    .ok_or("find predicate should return a value")?;
                let pred_bool = self.get_raw_bool_value(predicate_result)?;
                self.builder
                    .build_conditional_branch(pred_bool, found_bb, next_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(found_bb);
                self.builder
                    .build_store(result_ptr_alloca, elem_ptr.into_pointer_value())
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(exit_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(next_bb);
                let next = self
                    .builder
                    .build_int_add(idx, i64_type.const_int(1, false), "find_next")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, next)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(exit_bb);
                let result_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        result_ptr_alloca,
                        "find_result_ptr",
                    )
                    .map_err(|e| e.to_string())?;
                let opt_ptr =
                    self.call_runtime_function("mux_optional_some_value", &[result_ptr])?;
                let boxed_call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_into_value")
                            .expect("mux_optional_into_value must be declared in runtime"),
                        &[opt_ptr.into()],
                        "optional_as_value",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(boxed_call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_into_value should return a basic value"))
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
        obj_type: &Type,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let (map_key_type, map_value_type) = match obj_type {
            Type::Map(k, v) => (Some(k.as_ref().clone()), Some(v.as_ref().clone())),
            _ => (None, None),
        };
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
            "filter" => {
                if args.len() != 1 {
                    return Err("filter() method takes exactly 1 argument".to_string());
                }
                let key_type = map_key_type
                    .clone()
                    .ok_or("map key type missing for filter")?;
                let value_type = map_value_type
                    .clone()
                    .ok_or("map value type missing for filter")?;
                let predicate_val = self.generate_expression(&args[0])?;
                let predicate_type = self
                    .analyzer
                    .get_expression_type(&args[0])
                    .map_err(|e| e.message)?;

                let result_map = self.create_empty_collection_value("mux_new_map", "mux_map_value");
                let pairs = self.call_runtime_function(
                    "mux_map_pairs",
                    &[self.extract_raw_pointer(obj_value, "mux_value_get_map", "extract_map")?],
                )?;
                let len = self
                    .call_runtime_function("mux_value_list_length", &[pairs])?
                    .into_int_value();
                let i64_type = self.context.i64_type();
                let idx_alloca = self
                    .builder
                    .build_alloca(i64_type, "map_filter_idx")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, i64_type.const_zero())
                    .map_err(|e| e.to_string())?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or("No current function")?;
                let header_bb = self
                    .context
                    .append_basic_block(current_fn, "map_filter_header");
                let body_bb = self
                    .context
                    .append_basic_block(current_fn, "map_filter_body");
                let add_bb = self
                    .context
                    .append_basic_block(current_fn, "map_filter_add");
                let skip_bb = self
                    .context
                    .append_basic_block(current_fn, "map_filter_skip");
                let exit_bb = self
                    .context
                    .append_basic_block(current_fn, "map_filter_exit");
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(header_bb);
                let idx = self
                    .builder
                    .build_load(i64_type, idx_alloca, "map_filter_idx_val")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, idx, len, "map_filter_cond")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_conditional_branch(cond, body_bb, exit_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                let pair_val =
                    self.call_runtime_function("mux_value_list_get_value", &[pairs, idx.into()])?;
                let pair_ptr = self
                    .call_runtime_function("mux_value_get_tuple", &[pair_val])?
                    .into_pointer_value();
                let key_val = self.call_runtime_function("mux_tuple_left", &[pair_ptr.into()])?;
                let val_val = self.call_runtime_function("mux_tuple_right", &[pair_ptr.into()])?;
                let typed_key = self.unbox_value_for_type(key_val, &key_type)?;
                let typed_val = self.unbox_value_for_type(val_val, &value_type)?;
                let predicate_result = self
                    .call_function_value(predicate_val, &predicate_type, &[typed_key, typed_val])?
                    .ok_or("map.filter predicate should return a value")?;
                let pred_bool = self.get_raw_bool_value(predicate_result)?;
                self.builder
                    .build_conditional_branch(pred_bool, add_bb, skip_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(add_bb);
                self.generate_runtime_call(
                    "mux_map_put_value",
                    &[result_map.into(), key_val.into(), val_val.into()],
                );
                self.builder
                    .build_unconditional_branch(skip_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(skip_bb);
                let next = self
                    .builder
                    .build_int_add(idx, i64_type.const_int(1, false), "map_filter_next")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(idx_alloca, next)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(header_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(exit_bb);
                Ok(result_map)
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
                let optional_ptr = self
                    .call_runtime_function("mux_set_remove_value", &[obj_value, elem_ptr.into()])?;
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
