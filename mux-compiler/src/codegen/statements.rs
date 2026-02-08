//! Statement generation for the code generator.
//!
//! This module handles:
//! - Variable declarations (auto and typed)
//! - Return statements
//! - If statements with proper block handling
//! - While and for loops
//! - Expression statements
//! - Match statements for pattern matching
//! - RC scope management for statements

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

use crate::ast::{
    ExpressionKind, ExpressionNode, LiteralNode, PatternNode, PrimitiveType, StatementKind,
    StatementNode, TypeKind,
};
use crate::semantics::{Type, Type as ResolvedType};

use super::CodeGenerator;

impl<'a> CodeGenerator<'a> {
    pub(super) fn generate_statement(
        &mut self,
        stmt: &StatementNode,
        function: Option<&FunctionValue<'a>>,
    ) -> Result<(), String> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, expr) => {
                // Check if variable already exists BEFORE generating expression
                // (generate_expression may modify self.variables)
                let existing_var = self.variables.get(name).cloned();
                let value = self.generate_expression(expr)?;
                // Get the type from the expression directly using semantic analyzer
                // (not from symbol table, since local variables are not stored there to avoid collisions)
                let resolved_type = self
                    .analyzer
                    .get_expression_type(expr)
                    .map_err(|e| format!("Failed to get type for {}: {}", name, e.message))?;
                // Resolve type variables using the current generic context
                // For example, in a specialized method Chain$string.box_value, when we see
                // auto b = Box<T>.new(), the type from expression is Box<Variable("T")>
                // but we need to resolve it to Box<Primitive(Str)] using generic_context
                let concrete_type = self
                    .resolve_type(&resolved_type)
                    .unwrap_or_else(|_| resolved_type.clone());
                // Use the existing variable if we found one before expression generation
                if let Some((existing_ptr, _, _)) = existing_var {
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
                            .insert(name.clone(), (alloca, var_type, concrete_type.clone()));
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
                                concrete_type.clone(),
                            ),
                        );
                        // Track for RC cleanup
                        if function.is_some() && self.type_needs_rc_tracking(&concrete_type) {
                            self.track_rc_variable(name, alloca);
                        }
                    }
                }
            }
            StatementKind::TypedDecl(name, type_node, expr) => {
                let var_type = self.llvm_type_from_mux_type(type_node)?;
                let value = self.generate_expression(expr)?;
                // Get the declared type from the type_node directly using semantic analyzer
                // (not from symbol table, since local variables are not stored there to avoid collisions)
                let resolved_type = self
                    .analyzer
                    .resolve_type(type_node)
                    .map_err(|e| format!("Failed to resolve type for {}: {}", name, e.message))?;
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
                        // Track for RC cleanup
                        if function.is_some() && self.type_needs_rc_tracking(&resolved_type) {
                            self.track_rc_variable(name, alloca);
                        }
                    }
                }
            }
            StatementKind::ConstDecl(name, _, expr) => {
                let value = self.generate_expression(expr)?;
                let boxed = self.box_value(value);
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                // Get the type from the expression directly using semantic analyzer
                // (not from symbol table, since local variables are not stored there to avoid collisions)
                let resolved_type = self
                    .analyzer
                    .get_expression_type(expr)
                    .map_err(|e| format!("Failed to get type for {}: {}", name, e.message))?;
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
                    // Track for RC cleanup
                    if function.is_some() && self.type_needs_rc_tracking(&resolved_type) {
                        self.track_rc_variable(name, alloca);
                    }
                }
            }
            StatementKind::Return(Some(expr)) => {
                // special handling for boolean literals in boolean functions
                if let ExpressionKind::Literal(LiteralNode::Boolean(b)) = &expr.kind {
                    if let Some(ResolvedType::Primitive(PrimitiveType::Bool)) =
                        &self.current_function_return_type
                    {
                        // Generate cleanup before returning bool literal
                        self.generate_all_scopes_cleanup()?;
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
                            // Cleanup before return (int is unboxed, cleanup frees the box)
                            self.generate_all_scopes_cleanup()?;
                            self.builder
                                .build_return(Some(&raw_int))
                                .map_err(|e| e.to_string())?;
                        }
                        ResolvedType::Primitive(PrimitiveType::Float) => {
                            // for float, unbox if necessary
                            let raw_float = self.get_raw_float_value(value)?;
                            // Cleanup before return (float is unboxed, cleanup frees the box)
                            self.generate_all_scopes_cleanup()?;
                            self.builder
                                .build_return(Some(&raw_float))
                                .map_err(|e| e.to_string())?;
                        }
                        ResolvedType::Primitive(PrimitiveType::Bool) => {
                            // for bool, unbox if necessary and return i1
                            if value.is_int_value() {
                                let int_val = value.into_int_value();
                                // Cleanup before return
                                self.generate_all_scopes_cleanup()?;
                                // check if we need to truncate i32/i64 to i1
                                if int_val.get_type().get_bit_width() == 1 {
                                    // already i1, return directly
                                    self.builder
                                        .build_return(Some(&int_val))
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
                                // Cleanup before return (bool is unboxed)
                                self.generate_all_scopes_cleanup()?;
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
                                // Increment RC before cleanup since we're returning a pointer
                                self.rc_inc_if_pointer(value)?;
                                self.generate_all_scopes_cleanup()?;
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
                            // Increment RC before cleanup since we're returning a pointer
                            self.rc_inc_if_pointer(boxed)?;
                            self.generate_all_scopes_cleanup()?;
                            self.builder
                                .build_return(Some(&boxed))
                                .map_err(|e| e.to_string())?;
                        }
                    }
                } else {
                    // fallback: assume boxed
                    let boxed = self.box_value(value);
                    // Increment RC before cleanup since we're returning a pointer
                    self.rc_inc_if_pointer(boxed.into())?;
                    self.generate_all_scopes_cleanup()?;
                    self.builder
                        .build_return(Some(&boxed))
                        .map_err(|e| e.to_string())?;
                }
            }
            StatementKind::Return(None) => {
                // Cleanup before void return
                self.generate_all_scopes_cleanup()?;
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
                            self.module
                                .get_function("mux_value_list_length")
                                .expect("mux_value_list_length must be declared in runtime"),
                            &[list_val.into()],
                            "list_len",
                        )
                        .map_err(|e| e.to_string())?;
                    let len_val = len_call
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_list_length should return a basic value")
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
                                .expect("mux_value_list_get_value must be declared in runtime"),
                            &[list_val.into(), index_load2.into()],
                            "list_get_value",
                        )
                        .map_err(|e| e.to_string())?;
                    let value_ptr = get_call
                        .try_as_basic_value()
                        .left()
                        .expect("mux_value_list_get_value should return a basic value")
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
                        } else if let Some((_, _, var_type)) = self.variables.get(name) {
                            // Check local variables first (they're not in all_symbols anymore)
                            match var_type {
                                Type::Named(n, _) => n.clone(),
                                Type::Optional(_) => "Optional".to_string(),
                                _ => {
                                    return Err("Match expression must be an enum type".to_string());
                                }
                            }
                        } else if let Some(symbol) = self.analyzer.symbol_table().lookup(name) {
                            // Fall back to symbol table for globals (enums, classes, etc.)
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
                                            if let TypeKind::Named(n, _) = &f.type_.kind {
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
                                                if let TypeKind::Named(n, _) = &f.type_.kind {
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

                let mut current_bb = self
                    .builder
                    .get_insert_block()
                    .expect("Builder should have an insertion block");
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
                                        .expect("data function should return a basic value")
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
                            let struct_type = self
                                .type_map
                                .get(&enum_name)
                                .ok_or_else(|| format!("Enum {} not found in type map", enum_name))?
                                .into_struct_type();
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
                                            temp_ptr_opt.ok_or_else(|| {
                                                "Temp pointer should be Some".to_string()
                                            })?,
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
                                            i,
                                            enum_name,
                                            name,
                                            field_types_clone.len()
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
                                            i,
                                            enum_name,
                                            name,
                                            field_types_clone.len()
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
            StatementKind::Function(func) => {
                // Generate nested function with mangled name
                let parent_name = self
                    .current_function_name
                    .as_ref()
                    .ok_or("Nested function outside of parent function")?;
                let mangled_name = format!("{}!{}", parent_name, func.name);

                // Store the original function node with its mangled name
                self.function_nodes
                    .insert(mangled_name.clone(), func.clone());

                // Declare the function if not already declared
                if !self.functions.contains_key(&mangled_name) {
                    // Save current builder position
                    let saved_insert_block = self.builder.get_insert_block();

                    // Declare the function
                    let return_type = self
                        .analyzer
                        .resolve_type(&func.return_type)
                        .map_err(|e| e.to_string())?;
                    let llvm_return_type = if matches!(return_type, Type::Void) {
                        None
                    } else {
                        Some(self.llvm_type_from_mux_type(&func.return_type)?)
                    };

                    let mut param_types = vec![];
                    for param in &func.params {
                        param_types.push(self.llvm_type_from_mux_type(&param.type_)?.into());
                    }

                    let fn_type = if let Some(ret_type) = llvm_return_type {
                        ret_type.fn_type(&param_types, false)
                    } else {
                        self.context.void_type().fn_type(&param_types, false)
                    };

                    let function = self.module.add_function(&mangled_name, fn_type, None);
                    self.functions.insert(mangled_name.clone(), function);

                    // Restore builder position
                    if let Some(block) = saved_insert_block {
                        self.builder.position_at_end(block);
                    }
                }

                // Generate the function body
                // Save current context
                let saved_function_name = self.current_function_name.clone();
                let saved_function_return_type = self.current_function_return_type.clone();
                let saved_variables = self.variables.clone();

                // Create a modified function node with the mangled name for generation
                let mut mangled_func = func.clone();
                mangled_func.name = mangled_name.clone();

                // Generate the function
                self.generate_function(&mangled_func)?;

                // Restore context
                self.current_function_name = saved_function_name;
                self.current_function_return_type = saved_function_return_type;
                self.variables = saved_variables;

                // Re-position builder to the correct block after generating nested function
                if let Some(current_fn) = function {
                    if let Some(block) = current_fn.get_last_basic_block() {
                        self.builder.position_at_end(block);
                    }
                }
            }
            _ => {} // skip other statement types for now
        }
        Ok(())
    }
}
