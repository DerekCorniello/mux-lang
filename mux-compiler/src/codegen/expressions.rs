//! Expression generation for the code generator.
//!
//! This module handles:
//! - All expression types (literals, identifiers, binary ops, function calls, etc.)
//! - If expressions with phi nodes
//! - Lambda expressions with capture handling
//! - List/map/set literals
//! - Field access and method calls
//! - Unary operators (ref, deref, incr, decr, not, neg)
//! - Index access
//! - Match expressions

use inkwell::AddressSpace;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};

use crate::ast::{
    BinaryOp, ExpressionKind, ExpressionNode, FunctionNode, LiteralNode, Param, PrimitiveType,
    StatementNode, TypeKind, TypeNode, UnaryOp,
};
use crate::semantics::{GenericContext, SymbolKind, Type};

use super::CodeGenerator;

impl<'a> CodeGenerator<'a> {
    fn generate_if_expression(
        &mut self,
        cond: &ExpressionNode,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let cond_val = self.generate_expression(cond)?;

        // get current function
        let current_bb = self
            .builder
            .get_insert_block()
            .expect("builder should have an insert block during if generation");
        let function = current_bb
            .get_parent()
            .expect("basic block should have a parent function");

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
        let then_bb_end = self
            .builder
            .get_insert_block()
            .expect("builder should have insert block after then block");

        // else block
        self.builder.position_at_end(else_bb);
        let else_val = self.generate_expression(else_expr)?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        let else_bb_end = self
            .builder
            .get_insert_block()
            .expect("builder should have insert block after else block");

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
        expr: &ExpressionNode,
        params: &[Param],
        return_type: &TypeNode,
        body: &[StatementNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        // save current insert block
        let old_bb = self.builder.get_insert_block();

        // generate unique function name
        let func_name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;

        // Look up captured variables for this lambda
        let captures = self
            .analyzer
            .lambda_captures
            .get(&expr.span)
            .cloned()
            .unwrap_or_default();
        let has_captures = !captures.is_empty();

        // set current function name for proper scoping
        let old_function_name = self.current_function_name.take();
        self.current_function_name = Some(func_name.clone());

        // Build parameter types - if we have captures, add capture struct pointer as first param
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();

        if has_captures {
            // First parameter is pointer to capture struct
            param_types.push(ptr_type.into());
        }

        // Add user-visible parameters
        for param in params {
            let param_type = self.llvm_type_from_mux_type(&param.type_)?;
            param_types.push(param_type.into());
        }

        // Use explicit return type from lambda declaration
        let resolved_return = self
            .analyzer
            .resolve_type(return_type)
            .map_err(|e| e.to_string())?;
        let return_type_opt: Option<BasicTypeEnum<'a>> = if matches!(resolved_return, Type::Void) {
            None
        } else {
            Some(self.llvm_type_from_mux_type(return_type)?)
        };

        // set current function return type for proper return handling
        let old_return_type = self.current_function_return_type.take();
        self.current_function_return_type = Some(resolved_return.clone());

        let fn_type = if let Some(rt) = return_type_opt {
            rt.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        // create the function
        let function = self.module.add_function(&func_name, fn_type, None);

        // set parameter names
        let param_offset = if has_captures { 1 } else { 0 };
        if has_captures {
            function
                .get_nth_param(0)
                .expect("captures parameter should exist")
                .set_name("captures");
        }
        for (i, param) in params.iter().enumerate() {
            let arg = function
                .get_nth_param((i + param_offset) as u32)
                .expect("function parameter should exist at expected index");
            arg.set_name(&param.name);
        }

        // create entry block and set up parameters
        let entry_bb = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_bb);

        // save current variables and create new scope
        let old_variables = self.variables.clone();
        self.variables.clear();

        // Save the parent's RC scope stack and start fresh for this lambda.
        // Lambdas are separate functions and should not clean up the enclosing
        // function's variables when they return.
        let saved_rc_scope_stack = std::mem::take(&mut self.rc_scope_stack);
        self.push_rc_scope();

        // If we have captures, extract them from the capture struct
        // The capture struct layout is: [ptr0, ptr1, ptr2, ...] where each ptr points to the captured variable's storage
        if has_captures {
            let captures_ptr = function
                .get_nth_param(0)
                .expect("captures parameter should exist")
                .into_pointer_value();

            // Create a struct type for the captures (array of pointers)
            let capture_struct_type = self
                .context
                .struct_type(&vec![ptr_type.into(); captures.len()], false);

            for (i, (name, var_type)) in captures.iter().enumerate() {
                // Get pointer to the i-th field in capture struct
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        capture_struct_type,
                        captures_ptr,
                        i as u32,
                        &format!("cap_{}_ptr", name),
                    )
                    .map_err(|e| e.to_string())?;

                // Load the pointer to the captured variable's storage
                let var_ptr = self
                    .builder
                    .build_load(ptr_type, field_ptr, &format!("cap_{}", name))
                    .map_err(|e| e.to_string())?
                    .into_pointer_value();

                // Store in variables map - the var_ptr points to the original variable's alloca
                self.variables.insert(
                    name.clone(),
                    (
                        var_ptr,
                        BasicTypeEnum::PointerType(ptr_type),
                        var_type.clone(),
                    ),
                );
            }
        }

        // set up user parameter variables
        for (i, param) in params.iter().enumerate() {
            let arg = function
                .get_nth_param((i + param_offset) as u32)
                .expect("function parameter should exist at expected index");
            let boxed = self.box_value(arg);
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
                (
                    alloca,
                    BasicTypeEnum::PointerType(ptr_type),
                    resolved_type.clone(),
                ),
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
                    // Generate RC cleanup before void return
                    self.generate_all_scopes_cleanup()?;
                    self.builder.build_return(None).map_err(|e| e.to_string())?;
                }
            }
        }

        // Pop the lambda's RC scope and restore the parent's stack
        self.rc_scope_stack.pop();
        self.rc_scope_stack = saved_rc_scope_stack;

        // restore variables - but keep a copy for looking up captured vars
        self.variables = old_variables;

        // restore return type
        self.current_function_return_type = old_return_type;

        // restore function name
        self.current_function_name = old_function_name;

        // restore builder to previous block
        if let Some(bb) = old_bb {
            self.builder.position_at_end(bb);
        }

        // If we have captures, create a closure object; otherwise return raw function pointer
        if has_captures {
            // Allocate capture struct and populate it with heap-allocated storage for captured variables
            // This ensures captured values survive after the creating function returns
            let capture_struct_type = self
                .context
                .struct_type(&vec![ptr_type.into(); captures.len()], false);

            // Calculate size and allocate the capture struct
            let struct_size = capture_struct_type
                .size_of()
                .ok_or("Failed to get capture struct size")?;
            let malloc_fn = self
                .module
                .get_function("malloc")
                .ok_or("malloc not found")?;
            let capture_mem = self
                .builder
                .build_call(malloc_fn, &[struct_size.into()], "capture_alloc")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("malloc didn't return a value")?
                .into_pointer_value();

            // For each captured variable, allocate heap storage, copy the value, and store the heap pointer
            // Also update self.variables so that outer scope mutations affect the heap location
            for (i, (name, var_type)) in captures.iter().enumerate() {
                // Get the pointer to the captured variable from outer scope
                let (var_ptr, llvm_type, _) = self
                    .variables
                    .get(name)
                    .or_else(|| self.global_variables.get(name))
                    .ok_or_else(|| format!("Captured variable '{}' not found", name))?
                    .clone();

                // Allocate heap storage for this captured value (one pointer-sized slot)
                let ptr_size = self
                    .context
                    .ptr_type(AddressSpace::default())
                    .size_of()
                    .into();
                let heap_storage = self
                    .builder
                    .build_call(malloc_fn, &[ptr_size], &format!("cap_{}_heap", name))
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("malloc didn't return a value")?
                    .into_pointer_value();

                // Load the current value from the original variable and store it in heap storage
                let current_value = self
                    .builder
                    .build_load(ptr_type, var_ptr, &format!("cap_{}_val", name))
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(heap_storage, current_value)
                    .map_err(|e| e.to_string())?;

                // Update self.variables to point to the heap storage
                // This way, mutations in the outer scope will affect the heap-allocated value
                self.variables
                    .insert(name.clone(), (heap_storage, llvm_type, var_type.clone()));

                // Get pointer to the i-th field in capture struct
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        capture_struct_type,
                        capture_mem,
                        i as u32,
                        &format!("cap_field_{}", i),
                    )
                    .map_err(|e| e.to_string())?;

                // Store the heap pointer (not the original stack alloca) in the capture struct
                self.builder
                    .build_store(field_ptr, heap_storage)
                    .map_err(|e| e.to_string())?;
            }

            // Create closure struct: { fn_ptr, captures_ptr }
            let closure_struct_type = self
                .context
                .struct_type(&[ptr_type.into(), ptr_type.into()], false);

            // Allocate closure struct
            let closure_size = closure_struct_type
                .size_of()
                .ok_or("Failed to get closure struct size")?;
            let closure_mem = self
                .builder
                .build_call(malloc_fn, &[closure_size.into()], "closure_alloc")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("malloc didn't return a value")?
                .into_pointer_value();

            // Store function pointer
            let fn_ptr_field = self
                .builder
                .build_struct_gep(closure_struct_type, closure_mem, 0, "closure_fn_ptr")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(fn_ptr_field, function.as_global_value().as_pointer_value())
                .map_err(|e| e.to_string())?;

            // Store captures pointer
            let captures_field = self
                .builder
                .build_struct_gep(closure_struct_type, closure_mem, 1, "closure_captures")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(captures_field, capture_mem)
                .map_err(|e| e.to_string())?;

            // Return closure pointer
            Ok(closure_mem.into())
        } else {
            // No captures - still return a closure struct for uniform calling convention
            // Closure struct: { fn_ptr, null }
            let closure_struct_type = self
                .context
                .struct_type(&[ptr_type.into(), ptr_type.into()], false);

            let malloc_fn = self
                .module
                .get_function("malloc")
                .ok_or("malloc not found")?;
            let closure_size = closure_struct_type
                .size_of()
                .ok_or("Failed to get closure struct size")?;
            let closure_mem = self
                .builder
                .build_call(malloc_fn, &[closure_size.into()], "closure_alloc")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("malloc didn't return a value")?
                .into_pointer_value();

            // Store function pointer
            let fn_ptr_field = self
                .builder
                .build_struct_gep(closure_struct_type, closure_mem, 0, "closure_fn_ptr")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(fn_ptr_field, function.as_global_value().as_pointer_value())
                .map_err(|e| e.to_string())?;

            // Store null for captures
            let captures_field = self
                .builder
                .build_struct_gep(closure_struct_type, closure_mem, 1, "closure_captures")
                .map_err(|e| e.to_string())?;
            let null_ptr = ptr_type.const_null();
            self.builder
                .build_store(captures_field, null_ptr)
                .map_err(|e| e.to_string())?;

            Ok(closure_mem.into())
        }
    }

    /// check if a method's parameters or return type reference any of the given type parameters
    pub(super) fn method_uses_type_params(
        method: &FunctionNode,
        type_param_names: &[&str],
    ) -> bool {
        for param in &method.params {
            if Self::type_node_contains_names(&param.type_, type_param_names) {
                return true;
            }
        }
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
    pub(super) fn generate_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => self.generate_literal(lit),
            ExpressionKind::None => {
                let none_call = self
                    .builder
                    .build_call(
                        self.module
                            .get_function("mux_optional_none")
                            .expect("mux_optional_none must be declared in runtime"),
                        &[],
                        "none_literal",
                    )
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_optional_none should return a basic value");
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
                                PrimitiveType::Char => {
                                    // Char is stored as i64
                                    let raw_char = self.get_raw_int_value(ptr_to_boxed.into())?;
                                    Ok(raw_char.into())
                                }
                                PrimitiveType::Void | PrimitiveType::Auto => {
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
                            let class_name = func_name.split('.').next().ok_or_else(|| {
                                format!("Invalid function name format: {}", func_name)
                            })?;
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
                                    // self_ptr is an alloca containing a boxed Value, so load it first
                                    let self_value_ptr = self
                                        .builder
                                        .build_load(
                                            self.context.ptr_type(AddressSpace::default()),
                                            *self_ptr,
                                            "self_value",
                                        )
                                        .map_err(|e| e.to_string())?
                                        .into_pointer_value();

                                    // Call mux_get_object_ptr to unbox the Value and get the actual object data pointer
                                    let get_ptr_func = self
                                        .module
                                        .get_function("mux_get_object_ptr")
                                        .ok_or("mux_get_object_ptr not found")?;
                                    let object_data_ptr_call = self
                                        .builder
                                        .build_call(
                                            get_ptr_func,
                                            &[self_value_ptr.into()],
                                            "get_object_ptr_call",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    let object_data_ptr = object_data_ptr_call
                                        .try_as_basic_value()
                                        .left()
                                        .ok_or("Invalid return from mux_get_object_ptr")?
                                        .into_pointer_value();

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
                            let class_name = func_name
                                .split('.')
                                .next()
                                .expect("function name contains '.' so next() should return Some");
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
                                            .expect(
                                                "mux_get_object_ptr should return a basic value",
                                            )
                                            .into_pointer_value();

                                        // get the struct type and field pointer
                                        let struct_type =
                                            self.type_map.get(class_name).ok_or_else(|| {
                                                format!(
                                                    "Class {} not found in type map",
                                                    class_name
                                                )
                                            })?;
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
                                        let field_types = self
                                            .field_types_map
                                            .get(class_name)
                                            .expect("class should be in field_types_map");
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
            ExpressionKind::Binary {
                left, op, right, ..
            } => {
                if op.is_assignment() {
                    match op {
                        BinaryOp::Assign => {
                            let right_val = self.generate_expression(right)?;
                            if let ExpressionKind::Identifier(name) = &left.kind {
                                // check if this is a field assignment (bare identifier in method)
                                if let Some(ref func_name) = self.current_function_name {
                                    if func_name.contains('.') {
                                        let class_name = func_name.split('.').next()
                                            .expect("function name contains '.' so next() should return Some");
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
                                                    .expect("mux_get_object_ptr should return a basic value")
                                                    .into_pointer_value();

                                                // get the struct type and field pointer
                                                let struct_type = self
                                                    .type_map
                                                    .get(class_name)
                                                    .ok_or_else(|| {
                                                        format!(
                                                            "Class {} not found in type map",
                                                            class_name
                                                        )
                                                    })?;
                                                let field_ptr = self
                                                    .builder
                                                    .build_struct_gep(
                                                        *struct_type,
                                                        data_ptr,
                                                        *field_index as u32,
                                                        &format!("{}_ptr", name),
                                                    )
                                                    .map_err(|e| e.to_string())?;

                                                // Increment RC on the value being stored (field takes ownership)
                                                self.rc_inc_if_pointer(right_val)?;

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
                                op_span: _,
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
                                let mut struct_ptr = if let ExpressionKind::Identifier(obj_name) =
                                    &expr.kind
                                {
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
                                                    self.context.ptr_type(AddressSpace::default()),
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
                                                    .expect("mux_get_object_ptr should return a basic value")
                                                    .into_pointer_value();
                                            data_ptr
                                        } else {
                                            return Err(
                                                "Self not found in field assignment".to_string()
                                            );
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
                                                .expect("mux_get_object_ptr should return a basic value")
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
                                                        // also check if it's a generic field that resolves to a class
                                                        let field_info = self
                                                            .classes
                                                            .get(class_name.as_str())
                                                            .and_then(|fields| {
                                                                fields
                                                                    .iter()
                                                                    .find(|f| f.name == *field)
                                                            });

                                                        let value_to_store = if let Some(field) =
                                                            field_info
                                                        {
                                                            let field_type = &field.type_;
                                                            let is_generic_param =
                                                                field.is_generic_param;

                                                            // For generic parameters, resolve to concrete type
                                                            if is_generic_param {
                                                                if let TypeNode {
                                                                    kind:
                                                                        TypeKind::Named(param_name, _),
                                                                    ..
                                                                } = field_type
                                                                {
                                                                    // Try to resolve the generic parameter
                                                                    if let Some(context) =
                                                                        &self.generic_context
                                                                    {
                                                                        if let Some(concrete_type) =
                                                                            context
                                                                                .type_params
                                                                                .get(param_name)
                                                                        {
                                                                            // Check what the concrete type is
                                                                            match concrete_type {
                                                                                Type::Primitive(
                                                                                    _,
                                                                                ) => {
                                                                                    // Primitives need boxing
                                                                                    self.box_value(
                                                                                        right_val,
                                                                                    )
                                                                                    .into()
                                                                                }
                                                                                Type::Named(
                                                                                    type_name,
                                                                                    _,
                                                                                ) => {
                                                                                    // Check if it's an enum
                                                                                    let is_enum = self.analyzer.symbol_table()
                                                                                        .lookup(type_name)
                                                                                        .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                                                                                        .unwrap_or(false);
                                                                                    if is_enum {
                                                                                        // Enums store directly
                                                                                        right_val
                                                                                    } else {
                                                                                        // Classes are already pointers - don't box
                                                                                        right_val
                                                                                    }
                                                                                }
                                                                                _ => {
                                                                                    // Other types: box
                                                                                    self.box_value(
                                                                                        right_val,
                                                                                    )
                                                                                    .into()
                                                                                }
                                                                            }
                                                                        } else {
                                                                            // Generic param not in context, box it
                                                                            self.box_value(
                                                                                right_val,
                                                                            )
                                                                            .into()
                                                                        }
                                                                    } else {
                                                                        // No generic context, box it
                                                                        self.box_value(right_val)
                                                                            .into()
                                                                    }
                                                                } else {
                                                                    // Non-named generic param, box it
                                                                    self.box_value(right_val).into()
                                                                }
                                                            } else {
                                                                // Not a generic parameter - use existing logic
                                                                if let TypeNode {
                                                                    kind:
                                                                        TypeKind::Named(
                                                                            field_type_name,
                                                                            _,
                                                                        ),
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
                                                                        self.box_value(right_val)
                                                                            .into()
                                                                    }
                                                                } else {
                                                                    // for non-named types, box the value
                                                                    self.box_value(right_val).into()
                                                                }
                                                            }
                                                        } else {
                                                            // fallback: box the value
                                                            self.box_value(right_val).into()
                                                        };

                                                        // Increment RC on the value being stored (field takes ownership)
                                                        self.rc_inc_if_pointer(value_to_store)?;

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
                            } else if let ExpressionKind::ListAccess {
                                expr: target_expr,
                                index,
                            } = &left.kind
                            {
                                // Index assignment: list[index] = value or map[key] = value

                                // Check if target_expr is itself a ListAccess (nested case)
                                // This works for both Lists and Maps: map["key"][0] or list[0]["key"]
                                if let ExpressionKind::ListAccess { .. } = &target_expr.kind {
                                    // Nested access: could be 2+ levels deep
                                    // Flatten the entire chain to get base expression and all indices
                                    let (base_expr, all_indices) =
                                        self.collect_list_access_chain(left);

                                    if all_indices.len() <= 1 {
                                        // This shouldn't happen, but handle gracefully
                                        return Err(
                                            "Unexpected single-level nesting in nested path"
                                                .to_string(),
                                        );
                                    }

                                    let right_val = self.generate_expression(right)?;
                                    let boxed_value = self.box_value(right_val);

                                    // Generate the nested assignment with ALL indices
                                    // This now handles both Lists and Maps
                                    self.generate_nested_collection_assignment(
                                        base_expr,
                                        &all_indices,
                                        boxed_value.into(),
                                    )?;

                                    Ok(right_val)
                                } else {
                                    // Simple (non-nested) assignment
                                    let target_type = self
                                        .analyzer
                                        .get_expression_type(target_expr)
                                        .map_err(|e| e.message)?;

                                    match target_type {
                                        crate::semantics::Type::List(_) => {
                                            // Simple list index assignment: list[index] = value
                                            // Use mux_list_set_value which modifies the boxed Value directly
                                            let target_val =
                                                self.generate_expression(target_expr)?;
                                            let index_val = self.generate_expression(index)?;
                                            let right_val = self.generate_expression(right)?;

                                            // Box the value for storage
                                            let boxed_value = self.box_value(right_val);

                                            // Call mux_list_set_value to set the element (handles wraparound and extension)
                                            self.builder
                                                .build_call(
                                                    self.module
                                                        .get_function("mux_list_set_value")
                                                        .expect("mux_list_set_value must be declared in runtime"),
                                                    &[target_val.into(), index_val.into(), boxed_value.into()],
                                                    "list_set_value",
                                                )
                                                .map_err(|e| e.to_string())?;

                                            Ok(right_val)
                                        }
                                        crate::semantics::Type::Map(_, _) => {
                                            // Map key assignment: map[key] = value
                                            // Use mux_map_put_value which modifies the boxed Value directly
                                            let target_val =
                                                self.generate_expression(target_expr)?;
                                            let key_val = self.generate_expression(index)?;
                                            let right_val = self.generate_expression(right)?;

                                            // Box the key and value for storage
                                            let boxed_key = self.box_value(key_val);
                                            let boxed_value = self.box_value(right_val);

                                            // Call mux_map_put_value to insert/update the entry
                                            self.builder
                                            .build_call(
                                                self.module
                                                    .get_function("mux_map_put_value")
                                                    .expect(
                                                    "mux_map_put_value must be declared in runtime",
                                                ),
                                                &[
                                                    target_val.into(),
                                                    boxed_key.into(),
                                                    boxed_value.into(),
                                                ],
                                                "map_put_value",
                                            )
                                            .map_err(|e| e.to_string())?;

                                            Ok(right_val)
                                        }
                                        _ => Err(format!(
                                            "Cannot assign to index on non-list/map type: {:?}",
                                            target_type
                                        )),
                                    }
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
                                op_span: _,
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
                                op_span: _,
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
                                let class_name = current_function.split('.').next().expect(
                                    "function name contains '.' so next() should return Some",
                                );
                                if let Some(class_fields) = self.classes.get(class_name) {
                                    if class_fields.iter().any(|f| f.name == *field_name) {
                                        // get the field type before borrowing self mutably
                                        let field_type = class_fields
                                            .iter()
                                            .find(|f| f.name == *field_name)
                                            .expect("field should exist in class after semantic analysis")
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
                                // not a variable, check if it's a class, enum, or module import
                                if let Some(symbol) = self.analyzer.symbol_table().lookup(name) {
                                    if symbol.kind == crate::semantics::SymbolKind::Import {
                                        // Handle module.function calls (e.g., logger.write_log(...))
                                        // Look up the function symbol to get its mangled LLVM name
                                        let function_symbol = self
                                            .analyzer
                                            .imported_symbols()
                                            .get(name)
                                            .and_then(|module_syms| module_syms.get(field));

                                        let llvm_function_name =
                                            if let Some(func_sym) = function_symbol {
                                                func_sym
                                                    .llvm_name
                                                    .clone()
                                                    .unwrap_or_else(|| field.to_string())
                                            } else {
                                                field.to_string()
                                            };

                                        if let Some(func) =
                                            self.module.get_function(&llvm_function_name)
                                        {
                                            let mut call_args = vec![];
                                            for arg in args {
                                                call_args
                                                    .push(self.generate_expression(arg)?.into());
                                            }
                                            let call = self
                                                .builder
                                                .build_call(
                                                    func,
                                                    &call_args,
                                                    &format!("{}_call", field),
                                                )
                                                .map_err(|e| e.to_string())?;
                                            return match call.try_as_basic_value().left() {
                                                Some(val) => Ok(val),
                                                None => Ok(self
                                                    .context
                                                    .i32_type()
                                                    .const_int(0, false)
                                                    .into()),
                                            };
                                        } else {
                                            return Err(format!(
                                                "Function {} not found in module {}",
                                                field, name
                                            ));
                                        }
                                    } else if symbol.kind == crate::semantics::SymbolKind::Class {
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
                                                        .expect(
                                                            "static method should be in module",
                                                        ),
                                                    &call_args,
                                                    &format!("{}.{}_call", name, field),
                                                )
                                                .map_err(|e| e.to_string())?;
                                            return Ok(call.try_as_basic_value().left().expect(
                                                "static method call should return a basic value",
                                            ));
                                        } else {
                                            return Err(format!(
                                                "Method {} not found on class {}",
                                                field, name
                                            ));
                                        }
                                    } else if symbol.kind == crate::semantics::SymbolKind::Enum {
                                        // handle enum constructor calls like Shape.Circ
                                        let constructor_name = format!("{}!{}", name, field);
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
                                            return Ok(call.try_as_basic_value().left().expect(
                                                "constructor call should return a basic value",
                                            ));
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

                                        return Ok(call.try_as_basic_value().left().expect(
                                            "generic method call should return a basic value",
                                        ));
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
                        "read_line" => {
                            if !args.is_empty() {
                                return Err("read_line takes 0 arguments".to_string());
                            }
                            let func_read_line = self
                                .module
                                .get_function("mux_read_line")
                                .ok_or("mux_read_line not found")?;
                            let call = self
                                .builder
                                .build_call(func_read_line, &[], "read_line_call")
                                .map_err(|e| e.to_string())?;
                            let cstr_ptr = call
                                .try_as_basic_value()
                                .left()
                                .ok_or("mux_read_line returned no value")?
                                .into_pointer_value();
                            self.box_string_value(cstr_ptr)
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
                                let result_ptr = call
                                    .try_as_basic_value()
                                    .left()
                                    .expect("result constructor should return a basic value");
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
                            let result_ptr = call
                                .try_as_basic_value()
                                .left()
                                .expect("result constructor should return a basic value");
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
                            let result_ptr = call
                                .try_as_basic_value()
                                .left()
                                .expect("optional constructor should return a basic value");
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
                            let result_ptr = call
                                .try_as_basic_value()
                                .left()
                                .expect("optional constructor should return a basic value");
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
                                    // Load closure pointer (points to {fn_ptr, captures_ptr} struct)
                                    let closure_ptr = self
                                        .builder
                                        .build_load(
                                            self.context.ptr_type(AddressSpace::default()),
                                            *ptr,
                                            name,
                                        )
                                        .map_err(|e| e.to_string())?
                                        .into_pointer_value();

                                    // Define closure struct type
                                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                                    let closure_struct_type = self
                                        .context
                                        .struct_type(&[ptr_type.into(), ptr_type.into()], false);

                                    // Extract function pointer from closure
                                    let fn_ptr_field = self
                                        .builder
                                        .build_struct_gep(
                                            closure_struct_type,
                                            closure_ptr,
                                            0,
                                            "fn_ptr_field",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    let func_ptr = self
                                        .builder
                                        .build_load(ptr_type, fn_ptr_field, "fn_ptr")
                                        .map_err(|e| e.to_string())?
                                        .into_pointer_value();

                                    // Extract captures pointer from closure
                                    let captures_field = self
                                        .builder
                                        .build_struct_gep(
                                            closure_struct_type,
                                            closure_ptr,
                                            1,
                                            "captures_field",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    let captures_ptr = self
                                        .builder
                                        .build_load(ptr_type, captures_field, "captures_ptr")
                                        .map_err(|e| e.to_string())?
                                        .into_pointer_value();

                                    // Check if captures_ptr is null
                                    let is_null = self
                                        .builder
                                        .build_is_null(captures_ptr, "captures_is_null")
                                        .map_err(|e| e.to_string())?;

                                    // Generate user arguments
                                    let mut user_args: Vec<BasicMetadataValueEnum> = vec![];
                                    for arg in args {
                                        user_args.push(self.generate_expression(arg)?.into());
                                    }

                                    // Get function type from resolved type
                                    let (fn_type_with_captures, fn_type_without_captures) =
                                        if let Type::Function {
                                            params, returns, ..
                                        } = var_type_clone
                                        {
                                            // Parameter types for user params
                                            let mut param_types_without: Vec<
                                                BasicMetadataTypeEnum,
                                            > = Vec::new();
                                            for param in &params {
                                                let type_node = self.type_to_type_node(param);
                                                param_types_without.push(
                                                    self.llvm_type_from_mux_type(&type_node)?
                                                        .into(),
                                                );
                                            }

                                            // Parameter types with captures as first param
                                            let mut param_types_with: Vec<BasicMetadataTypeEnum> =
                                                vec![ptr_type.into()];
                                            param_types_with.extend(param_types_without.clone());

                                            let fn_type_with = if matches!(*returns, Type::Void) {
                                                self.context
                                                    .void_type()
                                                    .fn_type(&param_types_with, false)
                                            } else {
                                                let return_type_node =
                                                    self.type_to_type_node(&returns);
                                                let return_type = self
                                                    .llvm_type_from_mux_type(&return_type_node)?;
                                                return_type.fn_type(&param_types_with, false)
                                            };

                                            let fn_type_without = if matches!(*returns, Type::Void)
                                            {
                                                self.context
                                                    .void_type()
                                                    .fn_type(&param_types_without, false)
                                            } else {
                                                let return_type_node =
                                                    self.type_to_type_node(&returns);
                                                let return_type = self
                                                    .llvm_type_from_mux_type(&return_type_node)?;
                                                return_type.fn_type(&param_types_without, false)
                                            };

                                            (fn_type_with, fn_type_without)
                                        } else {
                                            return Err("Expected function type".to_string());
                                        };

                                    // Branch based on whether we have captures
                                    let current_fn = self
                                        .builder
                                        .get_insert_block()
                                        .and_then(|b| b.get_parent())
                                        .ok_or("No current function")?;
                                    let with_captures_bb = self
                                        .context
                                        .append_basic_block(current_fn, "with_captures");
                                    let without_captures_bb = self
                                        .context
                                        .append_basic_block(current_fn, "without_captures");
                                    let merge_bb =
                                        self.context.append_basic_block(current_fn, "call_merge");

                                    self.builder
                                        .build_conditional_branch(
                                            is_null,
                                            without_captures_bb,
                                            with_captures_bb,
                                        )
                                        .map_err(|e| e.to_string())?;

                                    // With captures: call with captures_ptr as first arg
                                    self.builder.position_at_end(with_captures_bb);
                                    let mut args_with_captures: Vec<BasicMetadataValueEnum> =
                                        vec![captures_ptr.into()];
                                    args_with_captures.extend(user_args.clone());
                                    let call_with = self
                                        .builder
                                        .build_indirect_call(
                                            fn_type_with_captures,
                                            func_ptr,
                                            &args_with_captures,
                                            "call_with_captures",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    let result_with = call_with.try_as_basic_value().left();
                                    self.builder
                                        .build_unconditional_branch(merge_bb)
                                        .map_err(|e| e.to_string())?;
                                    let with_captures_end_bb =
                                        self.builder.get_insert_block().expect("Builder should have an insertion block after positioning and building branch");

                                    // Without captures: call directly
                                    self.builder.position_at_end(without_captures_bb);
                                    let call_without = self
                                        .builder
                                        .build_indirect_call(
                                            fn_type_without_captures,
                                            func_ptr,
                                            &user_args,
                                            "call_without_captures",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    let result_without = call_without.try_as_basic_value().left();
                                    self.builder
                                        .build_unconditional_branch(merge_bb)
                                        .map_err(|e| e.to_string())?;
                                    let without_captures_end_bb =
                                        self.builder.get_insert_block().expect("Builder should have an insertion block after positioning and building branch");

                                    // Merge block with phi
                                    self.builder.position_at_end(merge_bb);
                                    return match (result_with, result_without) {
                                        (Some(r1), Some(r2)) => {
                                            let phi = self
                                                .builder
                                                .build_phi(r1.get_type(), "call_result")
                                                .map_err(|e| e.to_string())?;
                                            phi.add_incoming(&[
                                                (&r1, with_captures_end_bb),
                                                (&r2, without_captures_end_bb),
                                            ]);
                                            Ok(phi.as_basic_value())
                                        }
                                        _ => {
                                            // Void return
                                            Ok(self.context.i32_type().const_int(0, false).into())
                                        }
                                    };
                                } else {
                                    // not a function pointer, try global function lookup
                                    if let Some(func) = self.module.get_function(name) {
                                        // print some info about the found function
                                        let mut call_args = vec![];

                                        // Generate provided arguments
                                        for arg in args {
                                            call_args.push(self.generate_expression(arg)?.into());
                                        }

                                        // Handle default parameters
                                        // Clone default expressions first to avoid borrow issues
                                        let default_exprs: Vec<Option<ExpressionNode>> = self
                                            .function_nodes
                                            .get(name)
                                            .map(|func_node| {
                                                let total_params = func_node.params.len();
                                                let provided_args = args.len();
                                                if provided_args < total_params {
                                                    func_node.params[provided_args..total_params]
                                                        .iter()
                                                        .map(|p| p.default_value.clone())
                                                        .collect()
                                                } else {
                                                    Vec::new()
                                                }
                                            })
                                            .unwrap_or_default();

                                        for default_expr_opt in default_exprs {
                                            if let Some(default_expr) = default_expr_opt {
                                                let default_val =
                                                    self.generate_expression(&default_expr)?;
                                                call_args.push(default_val.into());
                                            } else {
                                                return Err(format!(
                                                    "Missing argument for parameter in function '{}'",
                                                    name
                                                ));
                                            }
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
                                // First check if this is a nested function call
                                let mut lookup_name = name.to_string();

                                // Check for nested function: try parent_name!function_name mangling
                                // For deeply nested functions, try all parent levels
                                if let Some(parent_fn) = &self.current_function_name {
                                    // First try direct parent mangling
                                    let mangled_name = format!("{}!{}", parent_fn, name);
                                    if self.module.get_function(&mangled_name).is_some() {
                                        lookup_name = mangled_name;
                                    } else {
                                        // If not found, try stripping nested layers
                                        // e.g., outer!compute -> outer
                                        let parts: Vec<&str> = parent_fn.split('!').collect();
                                        if parts.len() > 1 {
                                            // Try each prefix level (from shortest to longest)
                                            for i in 1..parts.len() {
                                                let prefix = parts[0..i].join("!");
                                                let mangled_name = format!("{}!{}", prefix, name);
                                                if self.module.get_function(&mangled_name).is_some()
                                                {
                                                    lookup_name = mangled_name;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }

                                // If not a nested function, check for imported function with LLVM name
                                if lookup_name == *name {
                                    if let Some(symbol) = self.analyzer.symbol_table().lookup(name)
                                    {
                                        lookup_name = symbol
                                            .llvm_name
                                            .as_ref()
                                            .unwrap_or(&name.to_string())
                                            .clone();
                                    }
                                }

                                if let Some(func) = self.module.get_function(&lookup_name) {
                                    let mut call_args = vec![];

                                    // Generate provided arguments
                                    for arg in args {
                                        call_args.push(self.generate_expression(arg)?.into());
                                    }

                                    // Handle default parameters - look up FunctionNode to get param info
                                    // Use original name for lookup (not mangled lookup_name)
                                    // Clone default expressions first to avoid borrow issues
                                    let default_exprs: Vec<Option<ExpressionNode>> = self
                                        .function_nodes
                                        .get(name)
                                        .map(|func_node| {
                                            let total_params = func_node.params.len();
                                            let provided_args = args.len();
                                            if provided_args < total_params {
                                                func_node.params[provided_args..total_params]
                                                    .iter()
                                                    .map(|p| p.default_value.clone())
                                                    .collect()
                                            } else {
                                                Vec::new()
                                            }
                                        })
                                        .unwrap_or_default();

                                    for default_expr_opt in default_exprs {
                                        if let Some(default_expr) = default_expr_opt {
                                            let default_val =
                                                self.generate_expression(&default_expr)?;
                                            call_args.push(default_val.into());
                                        } else {
                                            return Err(format!(
                                                "Missing argument for parameter in function '{}'",
                                                name
                                            ));
                                        }
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
                                    Err(format!(
                                        "Undefined function: {} (looked for LLVM name: {})",
                                        name, lookup_name
                                    ))
                                }
                            }
                        }
                    }
                } else {
                    // Handle arbitrary function expression (e.g., returned closure: test_func()())
                    // Generate the function expression to get a closure pointer
                    let closure_ptr = self.generate_expression(func)?.into_pointer_value();

                    // Get the function type from semantic analysis
                    let func_type = self
                        .analyzer
                        .get_expression_type(func)
                        .map_err(|e| e.to_string())?;

                    if let Type::Function {
                        params, returns, ..
                    } = func_type
                    {
                        // Use the closure calling mechanism (similar to function pointer variable calls)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());

                        // Define closure struct type: { fn_ptr, captures_ptr }
                        let closure_struct_type = self
                            .context
                            .struct_type(&[ptr_type.into(), ptr_type.into()], false);

                        // Extract function pointer from closure
                        let fn_ptr_field = self
                            .builder
                            .build_struct_gep(closure_struct_type, closure_ptr, 0, "fn_ptr_field")
                            .map_err(|e| e.to_string())?;
                        let func_ptr = self
                            .builder
                            .build_load(ptr_type, fn_ptr_field, "fn_ptr")
                            .map_err(|e| e.to_string())?
                            .into_pointer_value();

                        // Extract captures pointer from closure
                        let captures_field = self
                            .builder
                            .build_struct_gep(closure_struct_type, closure_ptr, 1, "captures_field")
                            .map_err(|e| e.to_string())?;
                        let captures_ptr = self
                            .builder
                            .build_load(ptr_type, captures_field, "captures_ptr")
                            .map_err(|e| e.to_string())?
                            .into_pointer_value();

                        // Check if captures_ptr is null
                        let is_null = self
                            .builder
                            .build_is_null(captures_ptr, "captures_is_null")
                            .map_err(|e| e.to_string())?;

                        // Generate user arguments
                        let mut user_args: Vec<BasicMetadataValueEnum> = vec![];
                        for arg in args {
                            user_args.push(self.generate_expression(arg)?.into());
                        }

                        // Build function types
                        let mut param_types_without: Vec<BasicMetadataTypeEnum> = Vec::new();
                        for param in &params {
                            let type_node = self.type_to_type_node(param);
                            param_types_without
                                .push(self.llvm_type_from_mux_type(&type_node)?.into());
                        }

                        let mut param_types_with: Vec<BasicMetadataTypeEnum> =
                            vec![ptr_type.into()];
                        param_types_with.extend(param_types_without.clone());

                        let fn_type_with = if matches!(*returns, Type::Void) {
                            self.context.void_type().fn_type(&param_types_with, false)
                        } else {
                            let return_type_node = self.type_to_type_node(&returns);
                            let return_type = self.llvm_type_from_mux_type(&return_type_node)?;
                            return_type.fn_type(&param_types_with, false)
                        };

                        let fn_type_without = if matches!(*returns, Type::Void) {
                            self.context
                                .void_type()
                                .fn_type(&param_types_without, false)
                        } else {
                            let return_type_node = self.type_to_type_node(&returns);
                            let return_type = self.llvm_type_from_mux_type(&return_type_node)?;
                            return_type.fn_type(&param_types_without, false)
                        };

                        // Branch based on whether we have captures
                        let current_fn = self
                            .builder
                            .get_insert_block()
                            .and_then(|b| b.get_parent())
                            .ok_or("No current function")?;
                        let with_captures_bb =
                            self.context.append_basic_block(current_fn, "with_captures");
                        let without_captures_bb = self
                            .context
                            .append_basic_block(current_fn, "without_captures");
                        let merge_bb = self.context.append_basic_block(current_fn, "call_merge");

                        self.builder
                            .build_conditional_branch(
                                is_null,
                                without_captures_bb,
                                with_captures_bb,
                            )
                            .map_err(|e| e.to_string())?;

                        // With captures: call with captures_ptr as first arg
                        self.builder.position_at_end(with_captures_bb);
                        let mut args_with_captures: Vec<BasicMetadataValueEnum> =
                            vec![captures_ptr.into()];
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
                        let with_captures_end_bb = self.builder.get_insert_block().expect("Builder should have an insertion block after positioning and building branch");

                        // Without captures: call directly
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
                        let without_captures_end_bb = self.builder.get_insert_block().expect("Builder should have an insertion block after positioning and building branch");

                        // Merge block with phi
                        self.builder.position_at_end(merge_bb);
                        match (result_with, result_without) {
                            (Some(r1), Some(r2)) => {
                                let phi = self
                                    .builder
                                    .build_phi(r1.get_type(), "call_result")
                                    .map_err(|e| e.to_string())?;
                                phi.add_incoming(&[
                                    (&r1, with_captures_end_bb),
                                    (&r2, without_captures_end_bb),
                                ]);
                                Ok(phi.as_basic_value())
                            }
                            _ => {
                                // Void return
                                Ok(self.context.i32_type().const_int(0, false).into())
                            }
                        }
                    } else {
                        Err(format!("Cannot call non-function type: {:?}", func_type))
                    }
                }
            }
            ExpressionKind::ListAccess {
                expr: target_expr,
                index,
            } => {
                let target_val = self.generate_expression(target_expr)?;
                let index_val = self.generate_expression(index)?;

                // Get the target type to determine if it's a list or map
                let target_type = self
                    .analyzer
                    .get_expression_type(target_expr)
                    .map_err(|e| e.message)?;

                match target_type {
                    crate::semantics::Type::List(_) => {
                        // List access: use mux_list_get_value
                        // extract raw List pointer from Value
                        let raw_list = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function("mux_value_get_list")
                                    .expect("mux_value_get_list must be declared in runtime"),
                                &[target_val.into()],
                                "extract_list",
                            )
                            .map_err(|e| e.to_string())?;

                        let raw_list_ptr = raw_list
                            .try_as_basic_value()
                            .left()
                            .expect("mux_value_get_list should return a basic value")
                            .into_pointer_value();

                        // Normalize the index to handle negative wraparound
                        let normalized_index =
                            self.normalize_list_index(index_val, raw_list_ptr)?;

                        // call mux_list_get_value (returns direct value or null)
                        let raw_result = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function("mux_list_get_value")
                                    .expect("mux_list_get_value must be declared in runtime"),
                                &[raw_list_ptr.into(), normalized_index.into()],
                                "list_raw",
                            )
                            .map_err(|e| e.to_string())?;

                        let result_ptr = raw_result
                            .try_as_basic_value()
                            .left()
                            .expect("mux_list_get_value should return a basic value")
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
                            .expect("Builder should have an insertion block")
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
                            .expect("mux_new_string_from_cstr should always return a value");
                        self.generate_runtime_call("mux_print", &[error_str.into()]);
                        self.generate_runtime_call(
                            "exit",
                            &[self.context.i32_type().const_int(1, false).into()],
                        );
                        self.builder
                            .build_unreachable()
                            .map_err(|e| e.to_string())?;

                        // continue block: extract the value based on its actual type
                        self.builder.position_at_end(continue_bb);

                        // Get the element type from the semantic analyzer
                        let element_type = self
                            .analyzer
                            .get_expression_type(expr)
                            .map_err(|e| e.message)?;

                        // Use extract_value_from_ptr to properly extract based on type
                        let (extracted_val, _) =
                            self.extract_value_from_ptr(result_ptr, &element_type, "list_element")?;
                        Ok(extracted_val)
                    }
                    crate::semantics::Type::Map(_, _) => {
                        // Map access: use mux_map_get (returns Optional)
                        // extract raw Map pointer from Value
                        let raw_map = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function("mux_value_get_map")
                                    .expect("mux_value_get_map must be declared in runtime"),
                                &[target_val.into()],
                                "extract_map",
                            )
                            .map_err(|e| e.to_string())?;

                        // Box the index value for map lookup
                        let boxed_index = self.box_value(index_val);

                        // call mux_map_get (returns Optional)
                        let raw_result = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function("mux_map_get")
                                    .expect("mux_map_get must be declared in runtime"),
                                &[
                                    raw_map
                                        .try_as_basic_value()
                                        .left()
                                        .expect("mux_value_get_map should return a basic value")
                                        .into(),
                                    boxed_index.into(),
                                ],
                                "map_get_result",
                            )
                            .map_err(|e| e.to_string())?;

                        let optional_ptr = raw_result
                            .try_as_basic_value()
                            .left()
                            .expect("mux_map_get should return a basic value")
                            .into_pointer_value();

                        // Check if Optional has a value using mux_optional_is_some
                        let is_some = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function("mux_optional_is_some")
                                    .expect("mux_optional_is_some must be declared in runtime"),
                                &[optional_ptr.into()],
                                "map_has_key",
                            )
                            .map_err(|e| e.to_string())?;

                        let is_some_val = is_some
                            .try_as_basic_value()
                            .left()
                            .expect("mux_optional_is_some should return a basic value")
                            .into_int_value();

                        // get current function for basic blocks
                        let current_function = self
                            .builder
                            .get_insert_block()
                            .expect("Builder should have an insertion block")
                            .get_parent()
                            .ok_or("No current function")?;

                        // create error block and continue block
                        let error_bb = self
                            .context
                            .append_basic_block(current_function, "map_key_error");
                        let continue_bb = self
                            .context
                            .append_basic_block(current_function, "map_key_continue");

                        self.builder
                            .build_conditional_branch(is_some_val, continue_bb, error_bb)
                            .map_err(|e| e.to_string())?;

                        // error block: print error with key and exit
                        self.builder.position_at_end(error_bb);
                        let error_msg = self
                            .builder
                            .build_global_string_ptr("Key not found in map", "map_error_msg")
                            .map_err(|e| e.to_string())?;
                        let error_str = self
                            .generate_runtime_call(
                                "mux_new_string_from_cstr",
                                &[error_msg.as_pointer_value().into()],
                            )
                            .expect("mux_new_string_from_cstr should always return a value");
                        self.generate_runtime_call("mux_print", &[error_str.into()]);
                        self.generate_runtime_call(
                            "exit",
                            &[self.context.i32_type().const_int(1, false).into()],
                        );
                        self.builder
                            .build_unreachable()
                            .map_err(|e| e.to_string())?;

                        // continue block: extract the value from the Optional
                        self.builder.position_at_end(continue_bb);

                        // Get the value from Optional using mux_optional_get_value
                        let value_result = self
                            .builder
                            .build_call(
                                self.module
                                    .get_function("mux_optional_get_value")
                                    .expect("mux_optional_get_value must be declared in runtime"),
                                &[optional_ptr.into()],
                                "map_value",
                            )
                            .map_err(|e| e.to_string())?;

                        let value_ptr = value_result
                            .try_as_basic_value()
                            .left()
                            .expect("mux_optional_get_value should return a basic value")
                            .into_pointer_value();

                        // Get the value type from the semantic analyzer
                        let value_type = self
                            .analyzer
                            .get_expression_type(expr)
                            .map_err(|e| e.message)?;

                        // Use extract_value_from_ptr to properly extract based on type
                        let (extracted_val, _) =
                            self.extract_value_from_ptr(value_ptr, &value_type, "map_element")?;
                        Ok(extracted_val)
                    }
                    _ => Err(format!(
                        "ListAccess target must be a list or map, found {:?}",
                        target_type
                    )),
                }
            }
            ExpressionKind::ListLiteral(elements) => {
                let list_ptr = self
                    .generate_runtime_call("mux_new_list", &[])
                    .expect("mux_new_list should always return a value")
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
                    .expect("mux_list_value should always return a value");
                Ok(list_value)
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                let map_ptr = self
                    .generate_runtime_call("mux_new_map", &[])
                    .expect("mux_new_map should always return a value")
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
                    .expect("mux_map_value should always return a value");
                Ok(map_value)
            }
            ExpressionKind::SetLiteral(elements) => {
                let set_ptr = self
                    .generate_runtime_call("mux_new_set", &[])
                    .expect("mux_new_set should always return a value")
                    .into_pointer_value();
                for element in elements {
                    let elem_val = self.generate_expression(element)?;
                    let elem_ptr = self.box_value(elem_val);
                    self.generate_runtime_call("mux_set_add", &[set_ptr.into(), elem_ptr.into()]);
                }
                let set_value = self
                    .generate_runtime_call("mux_set_value", &[set_ptr.into()])
                    .expect("mux_set_value should always return a value");
                Ok(set_value)
            }
            ExpressionKind::TupleLiteral(elements) => {
                assert_eq!(elements.len(), 2, "Tuple must have exactly 2 elements");
                let left_val = self.generate_expression(&elements[0])?;
                let right_val = self.generate_expression(&elements[1])?;
                let left_ptr = self.box_value(left_val);
                let right_ptr = self.box_value(right_val);
                let tuple_value = self
                    .generate_runtime_call("mux_new_tuple", &[left_ptr.into(), right_ptr.into()])
                    .expect("mux_new_tuple should always return a value");
                let wrapped_value = self
                    .generate_runtime_call("mux_tuple_value", &[tuple_value.into()])
                    .expect("mux_tuple_value should always return a value");
                Ok(wrapped_value)
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => Ok(self.generate_if_expression(cond, then_expr, else_expr)?),
            ExpressionKind::Lambda {
                params,
                return_type,
                body,
            } => Ok(self.generate_lambda_expression(expr, params, return_type, body)?),
            ExpressionKind::FieldAccess { expr, field } => {
                // Check if this is a tuple type - handle .left and .right specially
                let expr_type = self
                    .analyzer
                    .get_expression_type(expr)
                    .map_err(|e| e.to_string())?;
                if let Type::Tuple(_, _) = expr_type {
                    let tuple_value = self.generate_expression(expr)?;
                    let tuple_value_ptr = tuple_value.into_pointer_value();

                    // Extract the actual Tuple pointer from the Value
                    let get_tuple_fn = self
                        .module
                        .get_function("mux_value_get_tuple")
                        .ok_or("mux_value_get_tuple not found")?;
                    let tuple_ptr = self
                        .builder
                        .build_call(get_tuple_fn, &[tuple_value_ptr.into()], "get_tuple")
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .ok_or("mux_value_get_tuple should return a value")?
                        .into_pointer_value();

                    let field_index = match field.as_str() {
                        "left" => 0,
                        "right" => 1,
                        _ => return Err(format!("Unknown field '{}' for tuple type", field)),
                    };

                    let get_field_func = self
                        .module
                        .get_function(if field_index == 0 {
                            "mux_tuple_left"
                        } else {
                            "mux_tuple_right"
                        })
                        .ok_or(if field_index == 0 {
                            "mux_tuple_left not found"
                        } else {
                            "mux_tuple_right not found"
                        })?;

                    let field_value = self
                        .builder
                        .build_call(get_field_func, &[tuple_ptr.into()], field)
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .ok_or("mux_tuple_left/right should return a value")?;

                    let field_type = match expr_type {
                        Type::Tuple(left_type, right_type) => {
                            if field_index == 0 {
                                *left_type
                            } else {
                                *right_type
                            }
                        }
                        _ => return Err("Expected tuple type for field access".to_string()),
                    };

                    let field_value = match field_type {
                        Type::Primitive(PrimitiveType::Int) => {
                            let raw_int = self.get_raw_int_value(field_value)?;
                            raw_int.into()
                        }
                        Type::Primitive(PrimitiveType::Float) => {
                            let raw_float = self.get_raw_float_value(field_value)?;
                            raw_float.into()
                        }
                        Type::Primitive(PrimitiveType::Bool) => {
                            let raw_bool = self.get_raw_bool_value(field_value)?;
                            raw_bool.into()
                        }
                        Type::Primitive(PrimitiveType::Char) => {
                            let raw_char = self.get_raw_int_value(field_value)?;
                            raw_char.into()
                        }
                        Type::Primitive(PrimitiveType::Str) => field_value,
                        Type::Primitive(PrimitiveType::Void)
                        | Type::Primitive(PrimitiveType::Auto) => {
                            return Err(format!("Unsupported tuple field type {:?}", field_type));
                        }
                        _ => field_value,
                    };

                    return Ok(field_value);
                }

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
                                .expect("mux_get_object_ptr should return a basic value")
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
                                .expect("mux_get_object_ptr should return a basic value")
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
                                            // For generic parameters, resolve to concrete type first
                                            let is_generic_param = field_def.is_generic_param;
                                            if is_generic_param {
                                                if let TypeNode {
                                                    kind: TypeKind::Named(param_name, _),
                                                    ..
                                                } = &field_def.type_
                                                {
                                                    // Try to resolve the generic parameter
                                                    if let Some(context) = &self.generic_context {
                                                        if let Some(concrete_type) =
                                                            context.type_params.get(param_name)
                                                        {
                                                            // Match on the CONCRETE type to decide unboxing
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
                                                                Type::Named(_type_name, _) => {
                                                                    // For both enums and classes, return pointer directly (no unboxing)
                                                                    return Ok(loaded);
                                                                }
                                                                _ => {
                                                                    // Other types: return loaded pointer as-is
                                                                    return Ok(loaded);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                                // If we can't resolve, fall through to regular logic
                                            }
                                            // handle unboxing for primitive fields (non-generic case)
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
                                        &[call
                                            .try_as_basic_value()
                                            .left()
                                            .expect(
                                                "mux_value_to_string should return a basic value",
                                            )
                                            .into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().expect(
                                    "mux_new_string_from_cstr should return a basic value",
                                ));
                            }
                            Type::Primitive(PrimitiveType::Int) if field == "to_float" => {
                                // Get the boxed int value
                                let ptr = self.generate_expression(expr)?;
                                // Extract the raw i64 value
                                let raw_int = self.get_raw_int_value(ptr)?;
                                // Convert to f64 using sitofp instruction
                                let float_val = self
                                    .builder
                                    .build_signed_int_to_float(
                                        raw_int,
                                        self.context.f64_type(),
                                        "int_to_float",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(float_val.into());
                            }
                            Type::Primitive(PrimitiveType::Int) if field == "to_int" => {
                                // Identity function - just return the boxed int value
                                let int_val = self.generate_expression(expr)?;
                                return Ok(int_val);
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
                                        &[call
                                            .try_as_basic_value()
                                            .left()
                                            .expect(
                                                "mux_float_to_string should return a basic value",
                                            )
                                            .into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().expect(
                                    "mux_new_string_from_cstr should return a basic value",
                                ));
                            }
                            Type::Primitive(PrimitiveType::Float) if field == "to_int" => {
                                // Get the boxed float value
                                let ptr = self.generate_expression(expr)?;
                                // Extract the raw f64 value
                                let raw_float = self.get_raw_float_value(ptr)?;
                                // Convert to i64 using fptosi instruction (truncates)
                                let int_val = self
                                    .builder
                                    .build_float_to_signed_int(
                                        raw_float,
                                        self.context.i64_type(),
                                        "float_to_int",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(int_val.into());
                            }
                            Type::Primitive(PrimitiveType::Float) if field == "to_float" => {
                                // Identity function - just return the boxed float value
                                let float_val = self.generate_expression(expr)?;
                                return Ok(float_val);
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
                                        &[call
                                            .try_as_basic_value()
                                            .left()
                                            .expect("mux_int_to_string should return a basic value")
                                            .into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().expect(
                                    "mux_new_string_from_cstr should return a basic value",
                                ));
                            }
                            Type::Primitive(PrimitiveType::Bool) if field == "to_int" => {
                                // Get the boxed bool value
                                let ptr = self.generate_expression(expr)?;
                                // Extract the raw i32 value
                                let raw_bool = self.get_raw_bool_value(ptr)?;
                                // Extend i32 to i64: true -> 1, false -> 0
                                let int_val = self
                                    .builder
                                    .build_int_z_extend(
                                        raw_bool,
                                        self.context.i64_type(),
                                        "bool_to_int",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(int_val.into());
                            }
                            Type::Primitive(PrimitiveType::Bool) if field == "to_float" => {
                                // Get the boxed bool value
                                let ptr = self.generate_expression(expr)?;
                                // Extract the raw i32 value
                                let raw_bool = self.get_raw_bool_value(ptr)?;
                                // Convert i32 to f64: true -> 1.0, false -> 0.0
                                let float_val = self
                                    .builder
                                    .build_unsigned_int_to_float(
                                        raw_bool,
                                        self.context.f64_type(),
                                        "bool_to_float",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(float_val.into());
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
                                        &[call
                                            .try_as_basic_value()
                                            .left()
                                            .expect(
                                                "mux_value_to_string should return a basic value",
                                            )
                                            .into()],
                                        "new_str",
                                    )
                                    .map_err(|e| e.to_string())?;
                                return Ok(call2.try_as_basic_value().left().expect(
                                    "mux_new_string_from_cstr should return a basic value",
                                ));
                            }
                            Type::Primitive(PrimitiveType::Str) if field == "to_int" => {
                                // Get the string value (it's a *mut Value)
                                let ptr = self.generate_expression(expr)?;
                                // Call mux_value_to_string to get *const c_char
                                let func_to_cstr = self
                                    .module
                                    .get_function("mux_value_to_string")
                                    .ok_or("mux_value_to_string not found")?;
                                let cstr = self
                                    .builder
                                    .build_call(func_to_cstr, &[ptr.into()], "str_to_cstr")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_value_to_string should return a basic value");
                                // Call mux_string_to_int which returns *mut MuxResult
                                let func = self
                                    .module
                                    .get_function("mux_string_to_int")
                                    .ok_or("mux_string_to_int not found")?;
                                let result_ptr = self
                                    .builder
                                    .build_call(func, &[cstr.into()], "str_to_int")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_string_to_int should return a basic value");
                                return Ok(result_ptr);
                            }
                            Type::Primitive(PrimitiveType::Str) if field == "to_float" => {
                                // Get the string value (it's a *mut Value)
                                let ptr = self.generate_expression(expr)?;
                                // Call mux_value_to_string to get *const c_char
                                let func_to_cstr = self
                                    .module
                                    .get_function("mux_value_to_string")
                                    .ok_or("mux_value_to_string not found")?;
                                let cstr = self
                                    .builder
                                    .build_call(func_to_cstr, &[ptr.into()], "str_to_cstr")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_value_to_string should return a basic value");
                                // Call mux_string_to_float which returns *mut MuxResult
                                let func = self
                                    .module
                                    .get_function("mux_string_to_float")
                                    .ok_or("mux_string_to_float not found")?;
                                let result_ptr = self
                                    .builder
                                    .build_call(func, &[cstr.into()], "str_to_float")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_string_to_float should return a basic value");
                                return Ok(result_ptr);
                            }
                            Type::Primitive(PrimitiveType::Char) if field == "to_int" => {
                                // Get the char value (it's an i64)
                                let char_val = self.generate_expression(expr)?;
                                // Call mux_char_to_int which returns *mut MuxResult
                                let func = self
                                    .module
                                    .get_function("mux_char_to_int")
                                    .ok_or("mux_char_to_int not found")?;
                                let result_ptr = self
                                    .builder
                                    .build_call(func, &[char_val.into()], "char_to_int")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_char_to_int should return a basic value");
                                return Ok(result_ptr);
                            }
                            Type::Primitive(PrimitiveType::Char) if field == "to_string" => {
                                // Get the char value (it's an i64)
                                let char_val = self.generate_expression(expr)?;
                                // Call mux_char_to_string which returns *const c_char
                                let func = self
                                    .module
                                    .get_function("mux_char_to_string")
                                    .ok_or("mux_char_to_string not found")?;
                                let cstr = self
                                    .builder
                                    .build_call(func, &[char_val.into()], "str_to_cstr")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_value_to_string should return a basic value");
                                // Convert to *mut Value (string)
                                let func_new = self
                                    .module
                                    .get_function("mux_new_string_from_cstr")
                                    .ok_or("mux_new_string_from_cstr not found")?;
                                let result = self
                                    .builder
                                    .build_call(func_new, &[cstr.into()], "new_str")
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_new_string_from_cstr should return a basic value");
                                return Ok(result);
                            }
                            _ => {}
                        }
                    }
                }
                Err(format!(
                    "Field access not supported for expression type {:?}",
                    expr.kind
                ))
            }
            ExpressionKind::Unary {
                op,
                op_span: _,
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
                                    .expect("mux_value_get_int should return a basic value")
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
                                    .expect("mux_value_get_int should return a basic value")
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
    pub(super) fn generate_literal(
        &mut self,
        lit: &LiteralNode,
    ) -> Result<BasicValueEnum<'a>, String> {
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
                    .expect("mux_new_string_from_cstr should always return a value");
                Ok(call)
            }
            LiteralNode::Char(c) => {
                // Chars are stored as i64 for compatibility with int comparisons
                let val = self.context.i64_type().const_int(*c as u64, false);
                Ok(val.into())
            }
        }
    }

    /// Normalizes a list index to handle negative wraparound.
    ///
    /// Given an index value and the list length, returns a normalized index:
    /// - If index >= 0, returns index as-is
    /// - If index < 0, returns length + index (wraparound from end)
    ///
    /// This matches the behavior of mux_list_set_value in the runtime.
    fn normalize_list_index(
        &mut self,
        index_val: BasicValueEnum<'a>,
        list_ptr: PointerValue<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let index_int = index_val.into_int_value();

        // Get list length
        let list_length = self
            .builder
            .build_call(
                self.module
                    .get_function("mux_list_length")
                    .expect("mux_list_length must be declared in runtime"),
                &[list_ptr.into()],
                "list_len",
            )
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .expect("mux_list_length should return a basic value")
            .into_int_value();

        // Check if index < 0
        let is_negative = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                index_int,
                self.context.i64_type().const_zero(),
                "is_negative",
            )
            .map_err(|e| e.to_string())?;

        // If negative, compute (length + index), otherwise use index as-is
        let wrapped_index = self
            .builder
            .build_int_add(list_length, index_int, "wrapped_index")
            .map_err(|e| e.to_string())?;

        let normalized_index = self
            .builder
            .build_select(is_negative, wrapped_index, index_int, "normalized_index")
            .map_err(|e| e.to_string())?;

        Ok(normalized_index)
    }

    /// Extracts the base expression and all indices from a nested ListAccess chain.
    ///
    /// For example, `matrix[0][1]` returns `(matrix, [0, 1])`
    /// For `hypercube[0][1][2][3]` returns `(hypercube, [0, 1, 2, 3])`
    fn collect_list_access_chain<'b>(
        &self,
        expr: &'b ExpressionNode,
    ) -> (&'b ExpressionNode, Vec<&'b ExpressionNode>) {
        let mut indices = Vec::new();
        let mut current = expr;

        // Walk the chain backwards, collecting indices
        while let ExpressionKind::ListAccess { expr: inner, index } = &current.kind {
            indices.push(index.as_ref());
            current = inner.as_ref();
        }

        // We collected from innermost to outermost, so reverse
        indices.reverse();
        (current, indices)
    }

    /// Helper function to handle nested collection assignments of arbitrary depth.
    ///
    /// This handles assignments for any depth of nesting with Lists and Maps:
    /// - `matrix[0][1] = value` (2 levels, List)
    /// - `cube[0][1][2] = value` (3 levels, List)
    /// - `map_of_lists["key"][0] = value` (2 levels, Map then List)
    /// - `list_of_maps[0]["key"] = value` (2 levels, List then Map)
    ///
    /// Algorithm (recursive get-modify-writeback):
    /// For base[i1][i2]...[iN] = value:
    /// 1. If N == 1: base[i1] = value (base case, direct assignment)
    /// 2. If N > 1:
    ///    a. Get base[i1] → temp (copy)
    ///    b. Recursively: temp[i2]...[iN] = value
    ///    c. Write temp back: base[i1] = temp
    fn generate_nested_collection_assignment(
        &mut self,
        base_expr: &ExpressionNode,
        indices: &[&ExpressionNode],
        value: BasicValueEnum<'a>,
    ) -> Result<(), String> {
        if indices.is_empty() {
            return Err("generate_nested_collection_assignment called with no indices".to_string());
        }

        if indices.len() == 1 {
            // BASE CASE: Simple assignment base[index] = value
            // Determine if base is a List or Map and call appropriate function
            let base_type = self
                .analyzer
                .get_expression_type(base_expr)
                .map_err(|e| e.message)?;

            let base_val = self.generate_expression(base_expr)?;
            let index_val = self.generate_expression(indices[0])?;

            match base_type {
                crate::semantics::Type::List(_) => {
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_list_set_value")
                                .expect("mux_list_set_value must be declared in runtime"),
                            &[base_val.into(), index_val.into(), value.into()],
                            "nested_list_set_direct",
                        )
                        .map_err(|e| e.to_string())?;
                }
                crate::semantics::Type::Map(_, _) => {
                    let boxed_key = self.box_value(index_val);
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_map_put_value")
                                .expect("mux_map_put_value must be declared in runtime"),
                            &[base_val.into(), boxed_key.into(), value.into()],
                            "nested_map_set_direct",
                        )
                        .map_err(|e| e.to_string())?;
                }
                _ => {
                    return Err(format!(
                        "Cannot assign to index on non-list/map type: {:?}",
                        base_type
                    ));
                }
            }

            Ok(())
        } else {
            // RECURSIVE CASE: base[i1][i2]...[iN] = value where N > 1
            // Strategy:
            // 1. Get base[i1] → temp (copy)
            // 2. Recursively handle temp[i2]...[iN] = value
            // 3. Write temp back to base[i1]

            // Determine if base is a List or Map
            let base_type = self
                .analyzer
                .get_expression_type(base_expr)
                .map_err(|e| e.message)?;

            let base_val = self.generate_expression(base_expr)?;
            let first_index_val = self.generate_expression(indices[0])?;

            // Get base[i1] - different logic for List vs Map
            let intermediate_val = match base_type {
                crate::semantics::Type::List(_) => {
                    // Extract raw List from base
                    let raw_base_list = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_value_get_list")
                                .expect("mux_value_get_list must be declared in runtime"),
                            &[base_val.into()],
                            "extract_list",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_get_list should return a basic value")
                        .into_pointer_value();

                    // Normalize the index to handle negative wraparound
                    let normalized_index =
                        self.normalize_list_index(first_index_val, raw_base_list)?;

                    // Get base[i1] (this is a copy)
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_list_get_value")
                                .expect("mux_list_get_value must be declared in runtime"),
                            &[raw_base_list.into(), normalized_index.into()],
                            "get_intermediate",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_list_get_value should return a basic value")
                }
                crate::semantics::Type::Map(_, _) => {
                    // Extract raw Map from base
                    let raw_base_map = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_value_get_map")
                                .expect("mux_value_get_map must be declared in runtime"),
                            &[base_val.into()],
                            "extract_map",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_get_map should return a basic value");

                    // Box the key
                    let boxed_key = self.box_value(first_index_val);

                    // Get map[key] (returns Optional)
                    let optional_ptr = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_map_get")
                                .expect("mux_map_get must be declared in runtime"),
                            &[raw_base_map.into(), boxed_key.into()],
                            "map_get_intermediate",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_map_get should return a basic value")
                        .into_pointer_value();

                    // Check if Optional has a value
                    let is_some = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_optional_is_some")
                                .expect("mux_optional_is_some must be declared in runtime"),
                            &[optional_ptr.into()],
                            "map_has_key",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_optional_is_some should return a basic value")
                        .into_int_value();

                    // Handle None case
                    let current_function = self
                        .builder
                        .get_insert_block()
                        .expect("Builder should have an insertion block")
                        .get_parent()
                        .ok_or("No current function")?;

                    let error_bb = self
                        .context
                        .append_basic_block(current_function, "map_key_error");
                    let continue_bb = self
                        .context
                        .append_basic_block(current_function, "map_key_continue");

                    self.builder
                        .build_conditional_branch(is_some, continue_bb, error_bb)
                        .map_err(|e| e.to_string())?;

                    // Error block
                    self.builder.position_at_end(error_bb);
                    let error_msg = self
                        .builder
                        .build_global_string_ptr(
                            "Key not found in map during nested assignment",
                            "map_key_error_msg",
                        )
                        .map_err(|e| e.to_string())?;
                    let error_str = self
                        .generate_runtime_call(
                            "mux_new_string_from_cstr",
                            &[error_msg.as_pointer_value().into()],
                        )
                        .expect("mux_new_string_from_cstr should always return a value");
                    self.generate_runtime_call("mux_print", &[error_str.into()]);
                    self.generate_runtime_call(
                        "exit",
                        &[self.context.i32_type().const_int(1, false).into()],
                    );
                    self.builder
                        .build_unreachable()
                        .map_err(|e| e.to_string())?;

                    // Continue block - extract value from Optional
                    self.builder.position_at_end(continue_bb);
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_optional_get_value")
                                .expect("mux_optional_get_value must be declared in runtime"),
                            &[optional_ptr.into()],
                            "map_get_value",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_optional_get_value should return a basic value")
                }
                _ => {
                    return Err(format!(
                        "Cannot use nested indexing on non-list/map type: {:?}",
                        base_type
                    ));
                }
            };

            // Check for null (out of bounds) - only for Lists
            // Maps already handled the error case above
            if matches!(base_type, crate::semantics::Type::List(_)) {
                let intermediate_ptr = intermediate_val.into_pointer_value();
                let is_null = self
                    .builder
                    .build_is_null(intermediate_ptr, "is_null")
                    .map_err(|e| e.to_string())?;

                let current_function = self
                    .builder
                    .get_insert_block()
                    .expect("Builder should have an insertion block")
                    .get_parent()
                    .ok_or("No current function")?;

                let error_bb = self
                    .context
                    .append_basic_block(current_function, "nested_index_error");
                let continue_bb = self
                    .context
                    .append_basic_block(current_function, "nested_index_continue");

                self.builder
                    .build_conditional_branch(is_null, error_bb, continue_bb)
                    .map_err(|e| e.to_string())?;

                // Error block
                self.builder.position_at_end(error_bb);
                let error_msg = self
                    .builder
                    .build_global_string_ptr(
                        "Index out of bounds in nested assignment",
                        "nested_error_msg",
                    )
                    .map_err(|e| e.to_string())?;
                let error_str = self
                    .generate_runtime_call(
                        "mux_new_string_from_cstr",
                        &[error_msg.as_pointer_value().into()],
                    )
                    .expect("mux_new_string_from_cstr should always return a value");
                self.generate_runtime_call("mux_print", &[error_str.into()]);
                self.generate_runtime_call(
                    "exit",
                    &[self.context.i32_type().const_int(1, false).into()],
                );
                self.builder
                    .build_unreachable()
                    .map_err(|e| e.to_string())?;

                // Continue block
                self.builder.position_at_end(continue_bb);
            }

            // Now we need to handle: intermediate_val[i2]...[iN] = value
            // But intermediate_val is a *mut Value, not an ExpressionNode
            // We need to handle the remaining indices iteratively

            // Get the element type of the intermediate collection
            let intermediate_type = match &base_type {
                crate::semantics::Type::List(elem_type) => elem_type.as_ref().clone(),
                crate::semantics::Type::Map(_, value_type) => value_type.as_ref().clone(),
                _ => unreachable!(),
            };

            let remaining_indices = &indices[1..];
            self.apply_indices_and_set(
                intermediate_val,
                &intermediate_type,
                remaining_indices,
                value,
            )?;

            // Write the modified intermediate value back to base[i1]
            match base_type {
                crate::semantics::Type::List(_) => {
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_list_set_value")
                                .expect("mux_list_set_value must be declared in runtime"),
                            &[
                                base_val.into(),
                                first_index_val.into(),
                                intermediate_val.into(),
                            ],
                            "list_writeback",
                        )
                        .map_err(|e| e.to_string())?;
                }
                crate::semantics::Type::Map(_, _) => {
                    let boxed_key = self.box_value(first_index_val);
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_map_put_value")
                                .expect("mux_map_put_value must be declared in runtime"),
                            &[base_val.into(), boxed_key.into(), intermediate_val.into()],
                            "map_writeback",
                        )
                        .map_err(|e| e.to_string())?;
                }
                _ => unreachable!(),
            }

            Ok(())
        }
    }

    /// Helper to apply a chain of indices to a value and set the final element.
    /// This handles: value[i1][i2]...[iN] = new_value
    /// where `value` is a BasicValueEnum (*mut Value), not an ExpressionNode.
    ///
    /// Requires the type of `current_val` to determine if it's a List or Map.
    fn apply_indices_and_set(
        &mut self,
        current_val: BasicValueEnum<'a>,
        current_type: &crate::semantics::Type,
        indices: &[&ExpressionNode],
        final_value: BasicValueEnum<'a>,
    ) -> Result<(), String> {
        if indices.is_empty() {
            return Err("apply_indices_and_set called with no indices".to_string());
        }

        if indices.len() == 1 {
            // Base case: current_val[index] = final_value
            let index_val = self.generate_expression(indices[0])?;

            match current_type {
                crate::semantics::Type::List(_) => {
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_list_set_value")
                                .expect("mux_list_set_value must be declared in runtime"),
                            &[current_val.into(), index_val.into(), final_value.into()],
                            "apply_list_set",
                        )
                        .map_err(|e| e.to_string())?;
                }
                crate::semantics::Type::Map(_, _) => {
                    let boxed_key = self.box_value(index_val);
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_map_put_value")
                                .expect("mux_map_put_value must be declared in runtime"),
                            &[current_val.into(), boxed_key.into(), final_value.into()],
                            "apply_map_set",
                        )
                        .map_err(|e| e.to_string())?;
                }
                _ => {
                    return Err(format!(
                        "Cannot apply index to non-list/map type: {:?}",
                        current_type
                    ));
                }
            }
            Ok(())
        } else {
            // Recursive case: get current_val[i1], recurse on remaining, write back
            let first_index_val = self.generate_expression(indices[0])?;

            // Get current_val[i1] - different logic for List vs Map
            let next_val = match current_type {
                crate::semantics::Type::List(_) => {
                    // Extract raw List
                    let raw_list = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_value_get_list")
                                .expect("mux_value_get_list must be declared in runtime"),
                            &[current_val.into()],
                            "extract_for_apply",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_get_list should return a basic value")
                        .into_pointer_value();

                    // Normalize the index to handle negative wraparound
                    let normalized_index = self.normalize_list_index(first_index_val, raw_list)?;

                    // Get the next level
                    let next = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_list_get_value")
                                .expect("mux_list_get_value must be declared in runtime"),
                            &[raw_list.into(), normalized_index.into()],
                            "get_next_for_apply",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_list_get_value should return a basic value");

                    // Check for null
                    let next_ptr = next.into_pointer_value();
                    let is_null = self
                        .builder
                        .build_is_null(next_ptr, "is_null_apply")
                        .map_err(|e| e.to_string())?;

                    let current_function = self
                        .builder
                        .get_insert_block()
                        .expect("Builder should have an insertion block")
                        .get_parent()
                        .ok_or("No current function")?;

                    let error_bb = self
                        .context
                        .append_basic_block(current_function, "apply_index_error");
                    let continue_bb = self
                        .context
                        .append_basic_block(current_function, "apply_index_continue");

                    self.builder
                        .build_conditional_branch(is_null, error_bb, continue_bb)
                        .map_err(|e| e.to_string())?;

                    // Error block
                    self.builder.position_at_end(error_bb);
                    let error_msg = self
                        .builder
                        .build_global_string_ptr(
                            "Index out of bounds in nested assignment",
                            "apply_error_msg",
                        )
                        .map_err(|e| e.to_string())?;
                    let error_str = self
                        .generate_runtime_call(
                            "mux_new_string_from_cstr",
                            &[error_msg.as_pointer_value().into()],
                        )
                        .expect("mux_new_string_from_cstr should always return a value");
                    self.generate_runtime_call("mux_print", &[error_str.into()]);
                    self.generate_runtime_call(
                        "exit",
                        &[self.context.i32_type().const_int(1, false).into()],
                    );
                    self.builder
                        .build_unreachable()
                        .map_err(|e| e.to_string())?;

                    // Continue block
                    self.builder.position_at_end(continue_bb);

                    next
                }
                crate::semantics::Type::Map(_, _) => {
                    // Extract raw Map
                    let raw_map = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_value_get_map")
                                .expect("mux_value_get_map must be declared in runtime"),
                            &[current_val.into()],
                            "extract_map_for_apply",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_get_map should return a basic value");

                    // Box the key
                    let boxed_key = self.box_value(first_index_val);

                    // Get map[key] (returns Optional)
                    let optional_ptr = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_map_get")
                                .expect("mux_map_get must be declared in runtime"),
                            &[raw_map.into(), boxed_key.into()],
                            "apply_map_get",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_map_get should return a basic value")
                        .into_pointer_value();

                    // Check if Optional has a value
                    let is_some = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("mux_optional_is_some")
                                .expect("mux_optional_is_some must be declared in runtime"),
                            &[optional_ptr.into()],
                            "apply_map_has_key",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_optional_is_some should return a basic value")
                        .into_int_value();

                    let current_function = self
                        .builder
                        .get_insert_block()
                        .expect("Builder should have an insertion block")
                        .get_parent()
                        .ok_or("No current function")?;

                    let error_bb = self
                        .context
                        .append_basic_block(current_function, "apply_map_key_error");
                    let continue_bb = self
                        .context
                        .append_basic_block(current_function, "apply_map_key_continue");

                    self.builder
                        .build_conditional_branch(is_some, continue_bb, error_bb)
                        .map_err(|e| e.to_string())?;

                    // Error block
                    self.builder.position_at_end(error_bb);
                    let error_msg = self
                        .builder
                        .build_global_string_ptr(
                            "Key not found in map during nested assignment",
                            "apply_map_key_error_msg",
                        )
                        .map_err(|e| e.to_string())?;
                    let error_str = self
                        .generate_runtime_call(
                            "mux_new_string_from_cstr",
                            &[error_msg.as_pointer_value().into()],
                        )
                        .expect("mux_new_string_from_cstr should always return a value");
                    self.generate_runtime_call("mux_print", &[error_str.into()]);
                    self.generate_runtime_call(
                        "exit",
                        &[self.context.i32_type().const_int(1, false).into()],
                    );
                    self.builder
                        .build_unreachable()
                        .map_err(|e| e.to_string())?;

                    // Continue block - extract value from Optional
                    self.builder.position_at_end(continue_bb);
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_optional_get_value")
                                .expect("mux_optional_get_value must be declared in runtime"),
                            &[optional_ptr.into()],
                            "apply_map_get_value",
                        )
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .expect("mux_optional_get_value should return a basic value")
                }
                _ => {
                    return Err(format!(
                        "Cannot apply nested index to non-list/map type: {:?}",
                        current_type
                    ));
                }
            };

            // Get the element type for recursion
            let next_type = match current_type {
                crate::semantics::Type::List(elem_type) => elem_type.as_ref().clone(),
                crate::semantics::Type::Map(_, value_type) => value_type.as_ref().clone(),
                _ => unreachable!(),
            };

            // Recurse on the remaining indices
            self.apply_indices_and_set(next_val, &next_type, &indices[1..], final_value)?;

            // Write back
            match current_type {
                crate::semantics::Type::List(_) => {
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_list_set_value")
                                .expect("mux_list_set_value must be declared in runtime"),
                            &[current_val.into(), first_index_val.into(), next_val.into()],
                            "apply_list_writeback",
                        )
                        .map_err(|e| e.to_string())?;
                }
                crate::semantics::Type::Map(_, _) => {
                    let boxed_key = self.box_value(first_index_val);
                    self.builder
                        .build_call(
                            self.module
                                .get_function("mux_map_put_value")
                                .expect("mux_map_put_value must be declared in runtime"),
                            &[current_val.into(), boxed_key.into(), next_val.into()],
                            "apply_map_writeback",
                        )
                        .map_err(|e| e.to_string())?;
                }
                _ => unreachable!(),
            }

            Ok(())
        }
    }
}
