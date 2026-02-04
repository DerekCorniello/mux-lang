//! Constructor generation for classes and enums.
//!
//! This module handles generating constructors and related initialization code.

use super::CodeGenerator;
use crate::ast::{EnumVariant, ExpressionNode, Field, PrimitiveType, TypeKind};
use crate::semantics::{GenericContext, MethodSig, Type};
use inkwell::AddressSpace;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;

impl<'a> CodeGenerator<'a> {
    pub(super) fn generate_enum_constructors(
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
                let arg = function
                    .get_nth_param(i as u32)
                    .expect("function parameter should exist at expected index");
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

    pub(super) fn generate_class_constructors(
        &mut self,
        name: &str,
        fields: &[Field],
        interfaces: &HashMap<String, HashMap<String, MethodSig>>,
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
            .expect("type_id call should return a basic value")
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
            .expect("alloc_object call should return a pointer value")
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
            .expect("mux_get_object_ptr should return a basic value")
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
            let field_index = self
                .field_map
                .get(name)
                .expect("class should be in field_map after type generation")
                .get(&field.name)
                .expect("field should exist in class after semantic analysis");
            let field_ptr = self
                .builder
                .build_struct_gep(
                    class_type_clone,
                    struct_ptr_typed,
                    *field_index as u32,
                    &field.name,
                )
                .map_err(|e| e.to_string())?;

            // Check if field has a default value
            let default_value: BasicValueEnum = if let Some(default_expr) = &field.default_value {
                // Generate code for the literal default value
                let literal_val = self.generate_expression(default_expr)?;

                // Box if primitive
                if matches!(field.type_.kind, TypeKind::Primitive(_)) {
                    self.box_value(literal_val).into()
                } else {
                    literal_val
                }
            } else {
                // No default value - use zero/null initialization
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
                }
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
                .ok_or_else(|| format!("Field map not found for class {}", name))?
                .get(&vtable_field_name)
                .ok_or_else(|| {
                    format!(
                        "Vtable field {} not found in class {}",
                        vtable_field_name, name
                    )
                })?;
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

    pub(super) fn initialize_field_by_type(
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
                    .expect("mux_new_list should return a basic value");
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
                    .expect("mux_list_value should return a basic value");
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
                    .expect("mux_new_map should return a basic value");
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
                    .expect("mux_map_value should return a basic value");
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
                    .expect("mux_new_set should return a basic value");
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
                    .expect("mux_set_value should return a basic value");
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

    pub(super) fn generate_constructor_call_with_types(
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
    pub(super) fn sanitize_type_name(&self, type_: &Type) -> String {
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

    pub(super) fn create_specialized_method_name(
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

    pub(super) fn generate_constructor_call(
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
            .expect("mux_type_register should return a basic value")
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
            .expect("mux_alloc_object should return a basic value")
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
            .expect("mux_get_object_ptr should return a basic value")
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

    pub(super) fn generate_method_call_on_self(
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

        Ok(call
            .try_as_basic_value()
            .left()
            .expect("method call should return a basic value"))
    }
}
