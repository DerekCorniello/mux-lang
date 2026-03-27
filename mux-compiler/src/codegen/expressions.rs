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

#![allow(clippy::collapsible_if, clippy::let_and_return)]

use inkwell::AddressSpace;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};

use crate::ast::{
    BinaryOp, ExpressionKind, ExpressionNode, FunctionNode, LiteralNode, Param, PrimitiveType,
    StatementNode, TypeKind, TypeNode, UnaryOp,
};
use crate::semantics::{GenericContext, SymbolKind, Type};

use super::CodeGenerator;

/// Stdlib module prefixes that map to runtime functions via the "mux_" prefix.
/// Adding a new stdlib module only requires adding its prefix here and
/// declaring the corresponding LLVM functions in codegen/mod.rs.
impl<'a> CodeGenerator<'a> {
    fn build_import_call_args(
        &mut self,
        args: &[ExpressionNode],
        func_type: Option<&Type>,
        llvm_func: Option<inkwell::values::FunctionValue<'a>>,
    ) -> Result<Vec<BasicMetadataValueEnum<'a>>, String> {
        let param_types = match func_type {
            Some(Type::Function { params, .. }) => Some(params.as_slice()),
            _ => None,
        };
        let llvm_param_types = llvm_func.map(|f| f.get_type().get_param_types());

        let mut call_args = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let arg_val = self.generate_expression(arg)?;
            let mut coerced = arg_val;
            if let Some(params) = param_types
                && let Some(param_type) = params.get(idx)
            {
                coerced = self.coerce_import_arg(coerced, param_type)?;
            }

            if let Some(ref param_types) = llvm_param_types
                && let Some(expected) = param_types.get(idx)
            {
                coerced = self.coerce_to_llvm_param_type(coerced, *expected)?;
            }
            call_args.push(coerced.into());
        }
        Ok(call_args)
    }

    fn coerce_to_llvm_param_type(
        &self,
        value: BasicValueEnum<'a>,
        expected: BasicMetadataTypeEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        if value.is_int_value()
            && let BasicMetadataTypeEnum::IntType(expected_int) = expected
        {
            let actual_int = value.into_int_value();
            let actual_width = actual_int.get_type().get_bit_width();
            let expected_width = expected_int.get_bit_width();

            if actual_width < expected_width {
                let widened = self
                    .builder
                    .build_int_z_extend(actual_int, expected_int, "ffi_int_widen")
                    .map_err(|e| e.to_string())?;
                return Ok(widened.into());
            }
            if actual_width > expected_width {
                let narrowed = self
                    .builder
                    .build_int_truncate(actual_int, expected_int, "ffi_int_narrow")
                    .map_err(|e| e.to_string())?;
                return Ok(narrowed.into());
            }
        }
        Ok(value)
    }

    /// Coerce arguments for FFI/calling convention compatibility.
    /// This is NOT type coercion (types are already verified by semantic analysis).
    /// Instead, this converts from Mux representation to C/FFI representation:
    /// - Strings: extract raw *mut c_char from boxed Mux string
    /// - Complex types (Variable, Optional, Named): box the value
    ///
    /// This is necessary infrastructure for calling external/runtime functions.
    fn coerce_import_arg(
        &mut self,
        arg_val: BasicValueEnum<'a>,
        param_type: &Type,
    ) -> Result<BasicValueEnum<'a>, String> {
        match param_type {
            Type::Primitive(PrimitiveType::Str) => {
                if !arg_val.is_pointer_value() {
                    return Err("String arguments must be boxed values".to_string());
                }
                let cstr = self.extract_c_string_from_value(arg_val.into_pointer_value())?;
                Ok(cstr.into())
            }
            Type::Variable(_) | Type::Optional(_) | Type::Named(..) => {
                let boxed = self.box_value(arg_val);
                Ok(boxed.into())
            }
            _ => Ok(arg_val),
        }
    }

    /// Helper function to resolve the class/struct name from any expression
    /// Uses the semantic analyzer to get the expression type
    fn resolve_expression_class_name(&mut self, expr: &ExpressionNode) -> Option<String> {
        if let ExpressionKind::Identifier(name) = &expr.kind
            && let Some((_, _, var_type)) = self
                .variables
                .get(name)
                .or_else(|| self.global_variables.get(name))
        {
            return match var_type {
                Type::Named(type_name, _) => Some(type_name.clone()),
                Type::Reference(inner) => {
                    if let Type::Named(type_name, _) = inner.as_ref() {
                        Some(type_name.clone())
                    } else {
                        None
                    }
                }
                _ => None,
            };
        }

        let expr_type = self.analyzer.get_expression_type(expr).ok()?;
        match expr_type {
            Type::Named(name, _) => Some(name.clone()),
            Type::Reference(inner) => {
                if let Type::Named(name, _) = inner.as_ref() {
                    Some(name.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn generate_csv_field_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        if field != "headers" && field != "rows" {
            return Err(format!("Csv has no field '{}'", field));
        }
        let csv_value = self.generate_expression(expr)?;
        let raw_map = self
            .generate_runtime_call("mux_value_get_map", &[csv_value.into()])
            .ok_or_else(|| "mux_value_get_map should return a value".to_string())?;
        let key_value = self.csv_field_key_value(field)?;
        let map_get = self
            .generate_runtime_call("mux_map_get", &[raw_map.into(), key_value.into()])
            .ok_or_else(|| "mux_map_get should return a value".to_string())?
            .into_pointer_value();
        let value = self
            .generate_runtime_call("mux_optional_get_value", &[map_get.into()])
            .ok_or_else(|| "mux_optional_get_value should return a value".to_string())?;
        let free_opt = self
            .module
            .get_function("mux_free_optional")
            .ok_or("mux_free_optional not found")?;
        let _ = self
            .builder
            .build_call(free_opt, &[map_get.into()], "free_csv_optional");
        Ok(value)
    }

    fn csv_field_key_value(&mut self, field: &str) -> Result<BasicValueEnum<'a>, String> {
        let key_global = self
            .builder
            .build_global_string_ptr(field, &format!("csv_field_key_{}", field))
            .map_err(|e| e.to_string())?;
        let key_ptr = key_global.as_pointer_value();
        self.generate_runtime_call("mux_new_string_from_cstr", &[key_ptr.into()])
            .ok_or_else(|| "mux_new_string_from_cstr should return a value".to_string())
    }

    /// Helper to generate a call to the mux_print runtime function.
    /// Used for both direct print() calls and std.print() static method calls.
    pub(super) fn generate_print_call(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
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
        // Return void, but since BasicValueEnum is required, return a dummy
        Ok(self.context.i32_type().const_int(0, false).into())
    }

    /// Helper to generate a call to the mux_read_line runtime function.
    /// Used for both direct read_line() calls and std.read_line() static method calls.
    pub(super) fn generate_read_line_call(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
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

    fn try_resolve_module_class_static_call(
        &mut self,
        module_name: &str,
        class_name: &str,
        method_name: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let symbol = match self.analyzer.symbol_table().lookup(module_name) {
            Some(s) => s,
            None => return Ok(None),
        };

        if symbol.kind != crate::semantics::SymbolKind::Import {
            return Ok(None);
        }

        let module_syms = match self.analyzer.imported_symbols().get(module_name) {
            Some(syms) => syms,
            None => return Ok(None),
        };

        let class_symbol = match module_syms.get(class_name) {
            Some(sym) => sym,
            None => return Ok(None),
        };

        if class_symbol.kind != crate::semantics::SymbolKind::Class {
            return Ok(None);
        }

        if !class_symbol.methods.contains_key(method_name) {
            return Ok(None);
        }

        if let Some(call) =
            self.try_generate_net_static_method_call(class_name, method_name, args)?
        {
            return Ok(Some(call));
        }

        let mut call_args = vec![];
        for arg in args {
            call_args.push(self.generate_expression(arg)?.into());
        }

        let func_name = format!("{}.{}", class_name, method_name);
        match self.module.get_function(&func_name) {
            Some(func) => {
                let call = self
                    .builder
                    .build_call(func, &call_args, &format!("{}_call", func_name))
                    .map_err(|e| e.to_string())?;
                Ok(Some(
                    call.try_as_basic_value()
                        .left()
                        .ok_or("static method call should return a basic value")?,
                ))
            }
            None => Err(format!(
                "Static method {} not found for class {}",
                method_name, class_name
            )),
        }
    }

    fn call_import_or_runtime_function(
        &mut self,
        llvm_function_name: &str,
        display_name: &str,
        args: &[ExpressionNode],
        func_type: Option<&Type>,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match llvm_function_name {
            "mux_print" => return self.generate_print_call(args).map(Some),
            "mux_read_line" => return self.generate_read_line_call(args).map(Some),
            _ => {}
        }

        let Some(func) = self.module.get_function(llvm_function_name) else {
            return Ok(None);
        };

        let call_args = self.build_import_call_args(args, func_type, Some(func))?;
        let call = self
            .builder
            .build_call(func, &call_args, &format!("{}_call", display_name))
            .map_err(|e| e.to_string())?;
        Ok(Some(self.call_result_or_default_i32(call)))
    }

    fn call_imported_symbol_function(
        &mut self,
        function_symbol: Option<crate::semantics::Symbol>,
        fallback_name: &str,
        display_name: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let func_type = function_symbol.as_ref().and_then(|s| s.type_.clone());
        let llvm_function_name = function_symbol
            .as_ref()
            .and_then(|s| s.llvm_name.clone())
            .unwrap_or_else(|| fallback_name.to_string());
        self.call_import_or_runtime_function(
            &llvm_function_name,
            display_name,
            args,
            func_type.as_ref(),
        )
    }

    fn build_call_args_from_expressions(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<Vec<BasicMetadataValueEnum<'a>>, String> {
        let mut call_args = vec![];
        for arg in args {
            call_args.push(self.generate_expression(arg)?.into());
        }
        Ok(call_args)
    }

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
        let old_bb = self.builder.get_insert_block();

        let func_name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;

        let captures = self
            .analyzer
            .lambda_captures
            .get(&expr.span)
            .cloned()
            .unwrap_or_default();
        let has_captures = !captures.is_empty();

        let old_function_name = self.current_function_name.take();
        self.current_function_name = Some(func_name.clone());

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let param_types = self.build_lambda_param_types(params, has_captures, ptr_type)?;

        let resolved_return = self
            .analyzer
            .resolve_type(return_type)
            .map_err(|e| e.to_string())?;
        let return_type_opt: Option<BasicTypeEnum<'a>> = if matches!(resolved_return, Type::Void) {
            None
        } else {
            Some(self.llvm_type_from_mux_type(return_type)?)
        };

        let old_return_type = self.current_function_return_type.take();
        self.current_function_return_type = Some(resolved_return.clone());

        let fn_type = if let Some(rt) = return_type_opt {
            rt.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        let function = self.module.add_function(&func_name, fn_type, None);

        let param_offset = self.setup_lambda_param_names(function, params, has_captures);

        let entry_bb = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_bb);

        let old_variables = self.variables.clone();
        self.variables.clear();

        let saved_rc_scope_stack = std::mem::take(&mut self.rc_scope_stack);
        self.push_rc_scope();

        if has_captures {
            self.extract_captures_from_struct(function, &captures, ptr_type)?;
        }

        self.setup_lambda_user_params(function, params, param_offset, ptr_type)?;

        for stmt in body {
            self.generate_statement(stmt, Some(&function))?;
        }

        self.handle_lambda_void_return(return_type_opt)?;

        self.rc_scope_stack.pop();
        self.rc_scope_stack = saved_rc_scope_stack;

        self.variables = old_variables;
        self.current_function_return_type = old_return_type;
        self.current_function_name = old_function_name;

        if let Some(bb) = old_bb {
            self.builder.position_at_end(bb);
        }

        if has_captures {
            self.create_closure_with_captures(function, &captures, ptr_type)
        } else {
            self.create_closure_without_captures(function, ptr_type)
        }
    }

    fn build_lambda_param_types(
        &self,
        params: &[Param],
        has_captures: bool,
        ptr_type: inkwell::types::PointerType<'a>,
    ) -> Result<Vec<BasicMetadataTypeEnum<'a>>, String> {
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();

        if has_captures {
            param_types.push(ptr_type.into());
        }

        for param in params {
            let param_type = self.llvm_type_from_mux_type(&param.type_)?;
            param_types.push(param_type.into());
        }

        Ok(param_types)
    }

    fn setup_lambda_param_names(
        &self,
        function: inkwell::values::FunctionValue<'a>,
        params: &[Param],
        has_captures: bool,
    ) -> u32 {
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
        param_offset as u32
    }

    fn extract_captures_from_struct(
        &mut self,
        function: inkwell::values::FunctionValue<'a>,
        captures: &[(String, Type)],
        ptr_type: inkwell::types::PointerType<'a>,
    ) -> Result<(), String> {
        let captures_ptr = function
            .get_nth_param(0)
            .expect("captures parameter should exist")
            .into_pointer_value();

        let capture_struct_type = self
            .context
            .struct_type(&vec![ptr_type.into(); captures.len()], false);

        for (i, (name, var_type)) in captures.iter().enumerate() {
            let field_ptr = self
                .builder
                .build_struct_gep(
                    capture_struct_type,
                    captures_ptr,
                    i as u32,
                    &format!("cap_{}_ptr", name),
                )
                .map_err(|e| e.to_string())?;

            let var_ptr = self
                .builder
                .build_load(ptr_type, field_ptr, &format!("cap_{}", name))
                .map_err(|e| e.to_string())?
                .into_pointer_value();

            self.variables.insert(
                name.clone(),
                (
                    var_ptr,
                    BasicTypeEnum::PointerType(ptr_type),
                    var_type.clone(),
                ),
            );
        }
        Ok(())
    }

    fn setup_lambda_user_params(
        &mut self,
        function: inkwell::values::FunctionValue<'a>,
        params: &[Param],
        param_offset: u32,
        ptr_type: inkwell::types::PointerType<'a>,
    ) -> Result<(), String> {
        for (i, param) in params.iter().enumerate() {
            let arg = function
                .get_nth_param(i as u32 + param_offset)
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
        Ok(())
    }

    fn handle_lambda_void_return(
        &mut self,
        return_type_opt: Option<BasicTypeEnum<'a>>,
    ) -> Result<(), String> {
        if return_type_opt.is_none()
            && let Some(block) = self.builder.get_insert_block()
            && block.get_terminator().is_none()
        {
            self.generate_all_scopes_cleanup()?;
            self.builder.build_return(None).map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    fn create_closure_with_captures(
        &mut self,
        function: inkwell::values::FunctionValue<'a>,
        captures: &[(String, Type)],
        ptr_type: inkwell::types::PointerType<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let capture_struct_type = self
            .context
            .struct_type(&vec![ptr_type.into(); captures.len()], false);

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

        for (i, (name, var_type)) in captures.iter().enumerate() {
            let (var_ptr, llvm_type, _) = self
                .variables
                .get(name)
                .or_else(|| self.global_variables.get(name))
                .ok_or_else(|| format!("Captured variable '{}' not found", name))?
                .clone();

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

            let current_value = self
                .builder
                .build_load(ptr_type, var_ptr, &format!("cap_{}_val", name))
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(heap_storage, current_value)
                .map_err(|e| e.to_string())?;

            self.variables
                .insert(name.clone(), (heap_storage, llvm_type, var_type.clone()));

            let field_ptr = self
                .builder
                .build_struct_gep(
                    capture_struct_type,
                    capture_mem,
                    i as u32,
                    &format!("cap_field_{}", i),
                )
                .map_err(|e| e.to_string())?;

            self.builder
                .build_store(field_ptr, heap_storage)
                .map_err(|e| e.to_string())?;
        }

        let closure_struct_type = self
            .context
            .struct_type(&[ptr_type.into(), ptr_type.into()], false);

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

        let fn_ptr_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_mem, 0, "closure_fn_ptr")
            .map_err(|e| e.to_string())?;
        self.builder
            .build_store(fn_ptr_field, function.as_global_value().as_pointer_value())
            .map_err(|e| e.to_string())?;

        let captures_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_mem, 1, "closure_captures")
            .map_err(|e| e.to_string())?;
        self.builder
            .build_store(captures_field, capture_mem)
            .map_err(|e| e.to_string())?;

        Ok(closure_mem.into())
    }

    fn create_closure_without_captures(
        &self,
        function: inkwell::values::FunctionValue<'a>,
        ptr_type: inkwell::types::PointerType<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
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

        let fn_ptr_field = self
            .builder
            .build_struct_gep(closure_struct_type, closure_mem, 0, "closure_fn_ptr")
            .map_err(|e| e.to_string())?;
        self.builder
            .build_store(fn_ptr_field, function.as_global_value().as_pointer_value())
            .map_err(|e| e.to_string())?;

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

    fn generate_none_expression(&mut self) -> Result<BasicValueEnum<'a>, String> {
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
        Ok(none_call)
    }

    fn generate_non_assignment_binary_expression(
        &mut self,
        left: &ExpressionNode,
        op: &BinaryOp,
        right: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
            return self.generate_short_circuit_logical_op(left, op, right);
        }

        let left_val = self.generate_expression(left)?;
        let right_val = self.generate_expression(right)?;
        self.generate_binary_op(left, left_val, op, right, right_val)
    }

    fn generate_list_literal_expression(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let list_ptr = self
            .generate_runtime_call("mux_new_list", &[])
            .expect("mux_new_list should always return a value")
            .into_pointer_value();
        for element in elements {
            let elem_val = self.generate_expression(element)?;
            let elem_ptr = self.box_value(elem_val);
            self.generate_runtime_call("mux_list_push_back", &[list_ptr.into(), elem_ptr.into()]);
        }
        let list_value = self
            .generate_runtime_call("mux_list_value", &[list_ptr.into()])
            .expect("mux_list_value should always return a value");
        Ok(list_value)
    }

    fn generate_map_literal_expression(
        &mut self,
        entries: &[(ExpressionNode, ExpressionNode)],
    ) -> Result<BasicValueEnum<'a>, String> {
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

    fn generate_set_literal_expression(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
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

    fn generate_tuple_literal_expression(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if elements.len() != 2 {
            return Err(format!(
                "Tuple must have exactly 2 elements, got {}",
                elements.len()
            ));
        }
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

    fn generate_unary_expression(
        &mut self,
        op: &UnaryOp,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        match op {
            UnaryOp::Ref => self.generate_ref_unary_expression(expr),
            UnaryOp::Deref => self.generate_deref_unary_expression(expr),
            UnaryOp::Incr => self.generate_update_unary_expression(expr, true, true),
            UnaryOp::Decr => self.generate_update_unary_expression(expr, false, true),
            UnaryOp::Not => self.generate_not_unary_expression(expr),
            UnaryOp::Neg => self.generate_neg_unary_expression(expr),
        }
    }

    fn generate_ref_unary_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let ExpressionKind::Identifier(name) = &expr.kind {
            if let Some((ptr, _, _)) = self
                .variables
                .get(name)
                .or_else(|| self.global_variables.get(name))
            {
                return Ok((*ptr).into());
            }
            return Err(format!("Undefined variable {}", name));
        }

        let expr_val = self.generate_expression(expr)?;
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

    fn generate_deref_unary_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let ref_val = self.generate_expression(expr)?;
        let boxed_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                ref_val.into_pointer_value(),
                "boxed_ptr",
            )
            .map_err(|e| e.to_string())?;

        if let ExpressionKind::Identifier(name) = &expr.kind
            && let Some((_, _, Type::Reference(inner_type))) = self
                .variables
                .get(name)
                .or_else(|| self.global_variables.get(name))
        {
            return match inner_type.as_ref() {
                Type::Primitive(PrimitiveType::Int) => {
                    self.get_raw_int_value(boxed_ptr).map(Into::into)
                }
                Type::Primitive(PrimitiveType::Float) => {
                    self.get_raw_float_value(boxed_ptr).map(Into::into)
                }
                Type::Primitive(PrimitiveType::Bool) => {
                    self.get_raw_bool_value(boxed_ptr).map(Into::into)
                }
                _ => Ok(boxed_ptr),
            };
        }

        Ok(boxed_ptr)
    }

    fn generate_update_unary_expression(
        &mut self,
        expr: &ExpressionNode,
        increment: bool,
        allow_global: bool,
    ) -> Result<BasicValueEnum<'a>, String> {
        let ExpressionKind::Identifier(name) = &expr.kind else {
            return Err(format!(
                "Cannot {} on non-identifier",
                if increment { "increment" } else { "decrement" }
            ));
        };

        let ptr = if allow_global {
            self.variables
                .get(name)
                .or_else(|| self.global_variables.get(name))
                .map(|(p, _, _)| *p)
        } else {
            self.variables.get(name).map(|(p, _, _)| *p)
        }
        .ok_or_else(|| format!("Undefined variable {}", name))?;

        let value_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                ptr,
                &format!("{}_load", name),
            )
            .map_err(|e| e.to_string())?
            .into_pointer_value();
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

        let one = self.context.i64_type().const_int(1, false);
        let new_val = if increment {
            self.builder
                .build_int_add(current_val, one, "incr_result")
                .map_err(|e| e.to_string())?
        } else {
            self.builder
                .build_int_sub(current_val, one, "decr_result")
                .map_err(|e| e.to_string())?
        };
        let boxed_val = self.box_value(new_val.into());
        self.builder
            .build_store(ptr, boxed_val)
            .map_err(|e| e.to_string())?;
        Ok(new_val.into())
    }

    fn generate_not_unary_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let expr_val = self.generate_expression(expr)?;
        let bool_val = self.get_raw_bool_value(expr_val)?;
        let not_result = self
            .builder
            .build_not(bool_val, "not")
            .map_err(|e| e.to_string())?;
        Ok(not_result.into())
    }

    fn generate_neg_unary_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let expr_val = self.generate_expression(expr)?;
        if expr_val.is_int_value() {
            let int_val = expr_val.into_int_value();
            let neg = self
                .builder
                .build_int_neg(int_val, "neg")
                .map_err(|e| e.to_string())?;
            return Ok(neg.into());
        }
        if expr_val.is_float_value() {
            let float_val = expr_val.into_float_value();
            let neg = self
                .builder
                .build_float_neg(float_val, "neg")
                .map_err(|e| e.to_string())?;
            return Ok(neg.into());
        }
        if let Ok(int_val) = self.get_raw_int_value(expr_val) {
            let neg = self
                .builder
                .build_int_neg(int_val, "neg")
                .map_err(|e| e.to_string())?;
            return Ok(neg.into());
        }
        if let Ok(float_val) = self.get_raw_float_value(expr_val) {
            let neg = self
                .builder
                .build_float_neg(float_val, "neg")
                .map_err(|e| e.to_string())?;
            return Ok(neg.into());
        }
        Err("Negation only works on int or float values".to_string())
    }

    fn ok_builtin_constructor_name(arg_type: &Type) -> Option<&'static str> {
        match arg_type {
            Type::Primitive(PrimitiveType::Int) => Some("mux_result_ok_int"),
            Type::Primitive(PrimitiveType::Float) => Some("mux_result_ok_float"),
            Type::Primitive(PrimitiveType::Bool) => Some("mux_result_ok_bool"),
            Type::Primitive(PrimitiveType::Char) => Some("mux_result_ok_char"),
            Type::Primitive(PrimitiveType::Str)
            | Type::List(_)
            | Type::Map(_, _)
            | Type::Set(_)
            | Type::Named(_, _)
            | Type::Instantiated(_, _) => Some("mux_result_ok_value"),
            _ => None,
        }
    }

    fn some_builtin_constructor_name(arg_type: &Type) -> Option<&'static str> {
        match arg_type {
            Type::Primitive(PrimitiveType::Int) => Some("mux_optional_some_int"),
            Type::Primitive(PrimitiveType::Float) => Some("mux_optional_some_float"),
            Type::Primitive(PrimitiveType::Bool) => Some("mux_optional_some_bool"),
            Type::Primitive(PrimitiveType::Char) => Some("mux_optional_some_char"),
            Type::Primitive(PrimitiveType::Str)
            | Type::List(_)
            | Type::Map(_, _)
            | Type::Set(_)
            | Type::Named(_, _)
            | Type::Variable(_)
            | Type::Generic(_)
            | Type::Instantiated(_, _) => Some("mux_optional_some_value"),
            _ => None,
        }
    }

    fn call_single_arg_builtin(
        &mut self,
        func_name: &str,
        arg_val: BasicValueEnum<'a>,
        call_name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function(func_name)
            .ok_or(format!("{} not found", func_name))?;
        let call = self
            .builder
            .build_call(func, &[arg_val.into()], call_name)
            .map_err(|e| e.to_string())?;
        call.try_as_basic_value()
            .left()
            .ok_or_else(|| format!("{} should return a basic value", func_name))
    }

    fn generate_ok_builtin_call(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if args.len() != 1 {
            return Err("Ok takes 1 argument".to_string());
        }
        let arg_expr = &args[0];
        let arg_type = self
            .analyzer
            .get_expression_type(arg_expr)
            .map_err(|e| format!("Type inference failed: {}", e))?;
        let arg_val = self.generate_expression(arg_expr)?;
        let func_name = Self::ok_builtin_constructor_name(&arg_type)
            .ok_or_else(|| format!("Ok() not supported for type {:?}", arg_type))?;
        self.call_single_arg_builtin(func_name, arg_val, "ok_call")
    }

    fn generate_some_builtin_call(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if args.len() != 1 {
            return Err("Some takes 1 argument".to_string());
        }
        let arg_expr = &args[0];
        let arg_type = self
            .analyzer
            .get_expression_type(arg_expr)
            .or_else(|_| {
                if let ExpressionKind::Identifier(name) = &arg_expr.kind {
                    if let Some((_, _, ty)) = self
                        .variables
                        .get(name)
                        .or_else(|| self.global_variables.get(name))
                    {
                        return Ok(ty.clone());
                    }
                }
                Err(crate::semantics::SemanticError::new(
                    "Type inference failed for Some() argument",
                    arg_expr.span,
                ))
            })
            .map_err(|e| format!("Type inference failed: {}", e))?;
        let arg_val = self.generate_expression(arg_expr)?;
        let func_name = Self::some_builtin_constructor_name(&arg_type)
            .ok_or_else(|| format!("Some() not supported for type {:?}", arg_type))?;
        self.call_single_arg_builtin(func_name, arg_val, "some_call")
    }

    fn generate_none_builtin_call(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if !args.is_empty() {
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
        call.try_as_basic_value()
            .left()
            .ok_or("optional constructor should return a basic value".to_string())
    }

    fn generate_err_builtin_call(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if args.len() != 1 {
            return Err("Err takes 1 argument".to_string());
        }
        let arg_val = self.generate_expression(&args[0])?;
        let boxed_arg = self.box_value(arg_val);
        self.call_single_arg_builtin("mux_result_err_value", boxed_arg.into(), "err_call")
    }

    fn call_result_or_default_i32(
        &self,
        call: inkwell::values::CallSiteValue<'a>,
    ) -> BasicValueEnum<'a> {
        match call.try_as_basic_value().left() {
            Some(val) => val,
            None => self.context.i32_type().const_int(0, false).into(),
        }
    }

    fn build_args_with_class_copy(
        &mut self,
        args: &[ExpressionNode],
    ) -> Result<Vec<BasicMetadataValueEnum<'a>>, String> {
        let mut call_args = vec![];
        for arg in args {
            let arg_type = self
                .analyzer
                .get_expression_type(arg)
                .map_err(|e| e.message)?;

            let needs_copy = if let Type::Named(class_name, _) = &arg_type {
                if let Some(symbol) = self.analyzer.symbol_table().lookup(class_name) {
                    symbol.kind == crate::semantics::SymbolKind::Class
                } else {
                    false
                }
            } else {
                false
            };

            if needs_copy {
                let arg_value = self.generate_expression(arg)?;
                let ptr = arg_value.into_pointer_value();
                let copy_func = self
                    .module
                    .get_function("mux_copy_object")
                    .ok_or("mux_copy_object not found")?;
                let copied_ptr = self
                    .builder
                    .build_call(copy_func, &[ptr.into()], "copy_arg")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .expect("mux_copy_object should return a value")
                    .into_pointer_value();
                call_args.push(copied_ptr.into());
            } else {
                call_args.push(self.generate_expression(arg)?.into());
            }
        }
        Ok(call_args)
    }

    fn append_default_call_args(
        &mut self,
        function_name: &str,
        provided_args_len: usize,
        call_args: &mut Vec<BasicMetadataValueEnum<'a>>,
    ) -> Result<(), String> {
        let default_exprs: Vec<Option<ExpressionNode>> = self
            .function_nodes
            .get(function_name)
            .map(|func_node| {
                let total_params = func_node.params.len();
                if provided_args_len < total_params {
                    func_node.params[provided_args_len..total_params]
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
                let default_val = self.generate_expression(&default_expr)?;
                call_args.push(default_val.into());
            } else {
                return Err(format!(
                    "Missing argument for parameter in function '{}'",
                    function_name
                ));
            }
        }
        Ok(())
    }

    fn call_closure_with_optional_captures(
        &mut self,
        closure_ptr: PointerValue<'a>,
        params: &[Type],
        returns: &Type,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
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

        let mut user_args: Vec<BasicMetadataValueEnum> = vec![];
        for arg in args {
            user_args.push(self.generate_expression(arg)?.into());
        }

        let mut param_types_without: Vec<BasicMetadataTypeEnum> = Vec::new();
        for param in params {
            let type_node = self.type_to_type_node(param);
            param_types_without.push(self.llvm_type_from_mux_type(&type_node)?.into());
        }

        let mut param_types_with: Vec<BasicMetadataTypeEnum> = vec![ptr_type.into()];
        param_types_with.extend(param_types_without.clone());

        let fn_type_with = if matches!(returns, Type::Void) {
            self.context.void_type().fn_type(&param_types_with, false)
        } else {
            let return_type_node = self.type_to_type_node(returns);
            let return_type = self.llvm_type_from_mux_type(&return_type_node)?;
            return_type.fn_type(&param_types_with, false)
        };

        let fn_type_without = if matches!(returns, Type::Void) {
            self.context
                .void_type()
                .fn_type(&param_types_without, false)
        } else {
            let return_type_node = self.type_to_type_node(returns);
            let return_type = self.llvm_type_from_mux_type(&return_type_node)?;
            return_type.fn_type(&param_types_without, false)
        };

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
        let with_captures_end_bb = self
            .builder
            .get_insert_block()
            .expect("Builder should have an insertion block after positioning and building branch");

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
        let without_captures_end_bb = self
            .builder
            .get_insert_block()
            .expect("Builder should have an insertion block after positioning and building branch");

        self.builder.position_at_end(merge_bb);
        match (result_with, result_without) {
            (Some(r1), Some(r2)) => {
                let phi = self
                    .builder
                    .build_phi(r1.get_type(), "call_result")
                    .map_err(|e| e.to_string())?;
                phi.add_incoming(&[(&r1, with_captures_end_bb), (&r2, without_captures_end_bb)]);
                Ok(phi.as_basic_value())
            }
            _ => Ok(self.context.i32_type().const_int(0, false).into()),
        }
    }
    pub(super) fn generate_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        self.generate_expression_impl(expr)
    }

    fn generate_identifier_expression(&mut self, name: &str) -> Result<BasicValueEnum<'a>, String> {
        if let Some((ptr, var_type, type_node)) = self
            .variables
            .get(name)
            .or_else(|| self.global_variables.get(name))
        {
            let ptr_copy = *ptr;
            let var_type_copy = *var_type;
            let type_node_copy = type_node.clone();
            return self.generate_identifier_from_binding(
                name,
                ptr_copy,
                var_type_copy,
                &type_node_copy,
            );
        }

        if self
            .analyzer
            .symbol_table()
            .lookup(name)
            .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
            .unwrap_or(false)
        {
            return Err(format!("Enums cannot be used as values: {}", name));
        }

        if let Some(value) = self.try_generate_method_enum_field_identifier(name)? {
            return Ok(value);
        }

        if let Some(value) = self.try_generate_method_field_identifier(name)? {
            return Ok(value);
        }

        self.generate_identifier_function_reference(name)
    }

    fn load_boxed_ptr_from_alloca(
        &self,
        ptr: PointerValue<'a>,
        name: &str,
    ) -> Result<PointerValue<'a>, String> {
        self.builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                ptr,
                &format!("load_{}", name),
            )
            .map_err(|e| e.to_string())
            .map(|v| v.into_pointer_value())
    }

    fn generate_identifier_from_binding(
        &mut self,
        name: &str,
        ptr: PointerValue<'a>,
        var_type: BasicTypeEnum<'a>,
        type_node: &Type,
    ) -> Result<BasicValueEnum<'a>, String> {
        match type_node {
            Type::Optional(_) | Type::Result(_, _) => {
                self.load_boxed_ptr_from_alloca(ptr, name).map(|v| v.into())
            }
            Type::Named(type_name, _) => {
                self.generate_named_identifier_from_binding(name, ptr, var_type, type_name)
            }
            Type::Primitive(prim) => {
                self.generate_primitive_identifier_from_binding(name, ptr, prim)
            }
            Type::Function { .. } => self.load_boxed_ptr_from_alloca(ptr, name).map(|v| v.into()),
            _ => self.load_boxed_ptr_from_alloca(ptr, name).map(|v| v.into()),
        }
    }

    fn generate_named_identifier_from_binding(
        &mut self,
        name: &str,
        ptr: PointerValue<'a>,
        var_type: BasicTypeEnum<'a>,
        type_name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        if type_name == "optional" || type_name == "result" {
            return self.load_boxed_ptr_from_alloca(ptr, name).map(|v| v.into());
        }

        let is_enum = self
            .analyzer
            .symbol_table()
            .lookup(type_name)
            .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
            .unwrap_or(false);
        if is_enum {
            if let BasicTypeEnum::StructType(st) = var_type {
                return self
                    .builder
                    .build_load(st, ptr, &format!("load_{}", name))
                    .map_err(|e| e.to_string());
            }
            return Err(format!("Expected struct type for enum variable {}", name));
        }

        self.load_boxed_ptr_from_alloca(ptr, name).map(|v| v.into())
    }

    fn generate_primitive_identifier_from_binding(
        &mut self,
        name: &str,
        ptr: PointerValue<'a>,
        prim: &PrimitiveType,
    ) -> Result<BasicValueEnum<'a>, String> {
        let boxed_ptr = self.load_boxed_ptr_from_alloca(ptr, name)?;
        match prim {
            PrimitiveType::Int => self.get_raw_int_value(boxed_ptr.into()).map(|v| v.into()),
            PrimitiveType::Float => self.get_raw_float_value(boxed_ptr.into()).map(|v| v.into()),
            PrimitiveType::Bool => self.get_raw_bool_value(boxed_ptr.into()).map(|v| v.into()),
            PrimitiveType::Str => Ok(boxed_ptr.into()),
            PrimitiveType::Char => self.get_raw_int_value(boxed_ptr.into()).map(|v| v.into()),
            PrimitiveType::Void | PrimitiveType::Auto => {
                Err(format!("Unsupported primitive type {:?}", prim))
            }
        }
    }

    fn try_generate_method_enum_field_identifier(
        &mut self,
        name: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let Some(func_name) = self.current_function_name.as_ref() else {
            return Ok(None);
        };
        if !func_name.contains('.') {
            return Ok(None);
        }

        let class_name = func_name
            .split('.')
            .next()
            .ok_or_else(|| format!("Invalid function name format: {}", func_name))?;
        let has_field = self
            .field_map
            .get(class_name)
            .and_then(|fields| fields.get(name))
            .is_some();
        if !has_field {
            return Ok(None);
        }

        let Some((self_ptr, _, _)) = self
            .variables
            .get("self")
            .or_else(|| self.global_variables.get("self"))
        else {
            return Ok(None);
        };

        let self_value_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                *self_ptr,
                "self_value",
            )
            .map_err(|e| e.to_string())?
            .into_pointer_value();
        let get_ptr_func = self
            .module
            .get_function("mux_get_object_ptr")
            .ok_or("mux_get_object_ptr not found")?;
        let object_data_ptr = self
            .builder
            .build_call(
                get_ptr_func,
                &[self_value_ptr.into()],
                "get_object_ptr_call",
            )
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Invalid return from mux_get_object_ptr")?
            .into_pointer_value();
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
        let field_indices = self
            .field_map
            .get(class_name)
            .ok_or_else(|| format!("Field map not found for class {}", class_name))?;
        let field_index = field_indices
            .get(name)
            .ok_or_else(|| format!("Field {} not found in class {}", name, class_name))?;
        let field_ptr = self
            .builder
            .build_struct_gep(
                *class_type,
                struct_ptr_typed,
                *field_index as u32,
                "field_ptr",
            )
            .map_err(|e| e.to_string())?;
        let class_fields = self.classes.get(class_name).ok_or("Class not found")?;
        let field = class_fields
            .iter()
            .find(|f| f.name == name)
            .ok_or("Field not found")?;
        let field_type = self.llvm_type_from_mux_type(&field.type_)?;
        let enum_val = self
            .builder
            .build_load(field_type, field_ptr, "field_enum")
            .map_err(|e| e.to_string())?;
        Ok(Some(enum_val))
    }

    fn try_generate_method_field_identifier(
        &mut self,
        name: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let Some(func_name) = self.current_function_name.as_ref() else {
            return Ok(None);
        };
        if !func_name.contains('.') {
            return Ok(None);
        }

        let class_name = func_name
            .split('.')
            .next()
            .expect("function name contains '.' so next() should return Some");
        if !self.classes.contains_key(class_name) {
            return Ok(None);
        }

        let Some(field_index) = self
            .field_map
            .get(class_name)
            .and_then(|fields| fields.get(name))
        else {
            return Ok(None);
        };
        let field_index = *field_index;
        let Some((self_ptr, _, _)) = self
            .variables
            .get("self")
            .or_else(|| self.global_variables.get("self"))
        else {
            return Ok(None);
        };

        let self_value_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                *self_ptr,
                "load_self_for_field_access",
            )
            .map_err(|e| e.to_string())?
            .into_pointer_value();
        let get_ptr_func = self
            .module
            .get_function("mux_get_object_ptr")
            .ok_or("mux_get_object_ptr not found")?;
        let data_ptr = self
            .builder
            .build_call(get_ptr_func, &[self_value_ptr.into()], "get_data_ptr")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .expect("mux_get_object_ptr should return a basic value")
            .into_pointer_value();
        let struct_type = self
            .type_map
            .get(class_name)
            .ok_or_else(|| format!("Class {} not found in type map", class_name))?;
        let field_ptr = self
            .builder
            .build_struct_gep(
                *struct_type,
                data_ptr,
                field_index as u32,
                &format!("{}_ptr", name),
            )
            .map_err(|e| e.to_string())?;
        let field_types = self
            .field_types_map
            .get(class_name)
            .expect("class should be in field_types_map");
        let field_type = field_types[field_index];
        let loaded = self
            .builder
            .build_load(field_type, field_ptr, name)
            .map_err(|e| e.to_string())?;
        Ok(Some(loaded))
    }

    fn generate_identifier_function_reference(
        &self,
        name: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let Some(func) = self.module.get_function(name) {
            return Ok(func.as_global_value().as_pointer_value().into());
        }
        Err(format!("Undefined variable: {}", name))
    }

    fn generate_binary_expression(
        &mut self,
        left: &ExpressionNode,
        op: &BinaryOp,
        right: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        if !op.is_assignment() {
            return self.generate_non_assignment_binary_expression(left, op, right);
        }

        match op {
            BinaryOp::Assign => self.generate_simple_assignment_expression(left, right),
            BinaryOp::AddAssign => self.generate_compound_assignment_expression(left, right, true),
            BinaryOp::SubtractAssign => {
                self.generate_compound_assignment_expression(left, right, false)
            }
            _ => Err("Assignment op not implemented".to_string()),
        }
    }

    fn generate_simple_assignment_expression(
        &mut self,
        left: &ExpressionNode,
        right: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let right_val = self.generate_expression(right)?;
        match &left.kind {
            ExpressionKind::Identifier(name) => self.assign_to_identifier(name, right_val),
            ExpressionKind::Unary {
                op: UnaryOp::Deref,
                expr: deref_expr,
                ..
            } => self.assign_to_deref(deref_expr, right_val),
            ExpressionKind::FieldAccess { expr, field } => {
                self.assign_to_field_access(expr, field, right_val)
            }
            ExpressionKind::ListAccess {
                expr: target_expr,
                index,
            } => self.assign_to_list_access(target_expr, index, left, right),
            _ => Err("Assignment to non-identifier/deref/field not implemented".to_string()),
        }
    }

    fn assign_to_identifier(
        &mut self,
        name: &str,
        right_val: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let Some(result) = self.try_assign_identifier_to_method_field(name, right_val)? {
            return Ok(result);
        }

        let Some((ptr, _, type_node)) = self
            .variables
            .get(name)
            .or_else(|| self.global_variables.get(name))
        else {
            return Err(format!("Undefined variable {}", name));
        };
        let ptr_copy = *ptr;
        let type_node_copy = type_node.clone();

        let value_to_store = if let Type::Named(type_name, _) = &type_node_copy {
            let is_enum = self
                .analyzer
                .symbol_table()
                .lookup(type_name)
                .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                .unwrap_or(false);
            if is_enum {
                right_val
            } else {
                self.box_value(right_val).into()
            }
        } else {
            self.box_value(right_val).into()
        };

        self.builder
            .build_store(ptr_copy, value_to_store)
            .map_err(|e| e.to_string())?;
        Ok(right_val)
    }

    fn try_assign_identifier_to_method_field(
        &mut self,
        name: &str,
        right_val: BasicValueEnum<'a>,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let Some(func_name) = self.current_function_name.as_ref() else {
            return Ok(None);
        };
        let func_name = func_name.clone();
        if !func_name.contains('.') {
            return Ok(None);
        }

        let class_name = func_name
            .split('.')
            .next()
            .expect("function name contains '.' so next() should return Some")
            .to_string();
        let Some(field_index) = self
            .field_map
            .get(class_name.as_str())
            .and_then(|fields| fields.get(name))
        else {
            return Ok(None);
        };
        let field_index = *field_index;
        let Some((self_ptr, _, _)) = self
            .variables
            .get("self")
            .or_else(|| self.global_variables.get("self"))
        else {
            return Ok(None);
        };

        let data_ptr = self.load_object_data_ptr_from_self_alloca(*self_ptr, "get_data_ptr")?;
        let struct_type = self
            .type_map
            .get(class_name.as_str())
            .ok_or_else(|| format!("Class {} not found in type map", class_name))?;
        let field_ptr = self
            .builder
            .build_struct_gep(
                *struct_type,
                data_ptr,
                field_index as u32,
                &format!("{}_ptr", name),
            )
            .map_err(|e| e.to_string())?;
        self.rc_inc_if_pointer(right_val)?;
        self.builder
            .build_store(field_ptr, right_val)
            .map_err(|e| e.to_string())?;
        Ok(Some(right_val))
    }

    fn assign_to_deref(
        &mut self,
        deref_expr: &ExpressionNode,
        right_val: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let ref_val = self.generate_expression(deref_expr)?;
        let ptr = ref_val.into_pointer_value();
        let boxed = self.box_value(right_val);
        self.builder
            .build_store(ptr, boxed)
            .map_err(|e| e.to_string())?;
        Ok(right_val)
    }

    fn assign_to_field_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
        right_val: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let struct_ptr = self.resolve_struct_pointer_for_field_access(expr, "data_ptr_assign")?;
        let class_name = self
            .resolve_expression_class_name(expr)
            .ok_or("Named type expected")?;
        let field_ptr = self.resolve_struct_field_pointer(&class_name, field, struct_ptr)?;
        let value_to_store = self.compute_field_store_value(&class_name, field, right_val)?;

        self.rc_inc_if_pointer(value_to_store)?;
        self.builder
            .build_store(field_ptr, value_to_store)
            .map_err(|e| e.to_string())?;
        Ok(right_val)
    }

    fn resolve_struct_pointer_for_field_access(
        &mut self,
        expr: &ExpressionNode,
        call_name: &str,
    ) -> Result<PointerValue<'a>, String> {
        let mut struct_ptr = if let ExpressionKind::Identifier(obj_name) = &expr.kind {
            if obj_name == "self" {
                let Some((self_ptr, _, _)) = self
                    .variables
                    .get("self")
                    .or_else(|| self.global_variables.get("self"))
                else {
                    return Err("Self not found in field assignment".to_string());
                };
                self.load_object_data_ptr_from_self_alloca(*self_ptr, "self_data_ptr_assign")?
            } else {
                self.generate_expression(expr)?.into_pointer_value()
            }
        } else {
            self.generate_expression(expr)?.into_pointer_value()
        };

        let is_self = matches!(expr.kind, ExpressionKind::Identifier(ref name) if name == "self");
        if is_self || self.resolve_expression_class_name(expr).is_none() {
            return Ok(struct_ptr);
        }

        let get_ptr_func = self
            .module
            .get_function("mux_get_object_ptr")
            .ok_or("mux_get_object_ptr not found")?;
        let is_ref = self.is_reference_expression(expr);
        let ptr_to_use = if is_ref {
            self.builder
                .build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    struct_ptr,
                    "load_ref_ptr_assign",
                )
                .map_err(|e| e.to_string())?
                .into_pointer_value()
        } else {
            struct_ptr
        };
        struct_ptr = self
            .builder
            .build_call(get_ptr_func, &[ptr_to_use.into()], call_name)
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .expect("mux_get_object_ptr should return a basic value")
            .into_pointer_value();
        Ok(struct_ptr)
    }

    fn is_reference_expression(&mut self, expr: &ExpressionNode) -> bool {
        if let ExpressionKind::Identifier(obj_name) = &expr.kind {
            return self
                .variables
                .get(obj_name)
                .or_else(|| self.global_variables.get(obj_name))
                .map(|(_, _, t)| matches!(t, Type::Reference(_)))
                .unwrap_or(false);
        }
        self.analyzer
            .get_expression_type(expr)
            .map(|t| matches!(t, Type::Reference(_)))
            .unwrap_or(false)
    }

    fn load_object_data_ptr_from_self_alloca(
        &mut self,
        self_ptr: PointerValue<'a>,
        call_name: &str,
    ) -> Result<PointerValue<'a>, String> {
        let self_value_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                self_ptr,
                "load_self_for_field_assign",
            )
            .map_err(|e| e.to_string())?
            .into_pointer_value();
        let get_ptr_func = self
            .module
            .get_function("mux_get_object_ptr")
            .ok_or("mux_get_object_ptr not found")?;
        self.builder
            .build_call(get_ptr_func, &[self_value_ptr.into()], call_name)
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| "mux_get_object_ptr should return a basic value".to_string())
            .map(|v| v.into_pointer_value())
    }

    fn resolve_struct_field_pointer(
        &mut self,
        class_name: &str,
        field: &str,
        struct_ptr: PointerValue<'a>,
    ) -> Result<PointerValue<'a>, String> {
        let field_indices = self
            .field_map
            .get(class_name)
            .ok_or("Field map not found")?;
        let index = field_indices
            .get(field)
            .copied()
            .ok_or_else(|| format!("Field {} not found", field))?;
        let struct_type = self
            .type_map
            .get(class_name)
            .ok_or("Class type not found")?;
        if let BasicTypeEnum::StructType(st) = *struct_type {
            return self
                .builder
                .build_struct_gep(st, struct_ptr, index as u32, field)
                .map_err(|e| e.to_string());
        }
        Err("Struct type expected".to_string())
    }

    fn compute_field_store_value(
        &mut self,
        class_name: &str,
        field_name: &str,
        right_val: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let field_info = self
            .classes
            .get(class_name)
            .and_then(|fields| fields.iter().find(|f| f.name == field_name));
        let Some(field) = field_info else {
            return Ok(self.box_value(right_val).into());
        };
        let field_type = field.type_.clone();
        let is_generic_param = field.is_generic_param;

        if is_generic_param {
            return Ok(self.compute_generic_field_store_value(&field_type, right_val));
        }
        if let TypeNode {
            kind: TypeKind::Named(field_type_name, _),
            ..
        } = &field_type
        {
            let is_enum = self
                .analyzer
                .symbol_table()
                .lookup(field_type_name)
                .map(|s| s.kind == crate::semantics::SymbolKind::Enum)
                .unwrap_or(false);
            if is_enum {
                return Ok(right_val);
            }
        }
        Ok(self.box_value(right_val).into())
    }

    fn compute_generic_field_store_value(
        &mut self,
        field_type: &TypeNode,
        right_val: BasicValueEnum<'a>,
    ) -> BasicValueEnum<'a> {
        let TypeNode {
            kind: TypeKind::Named(param_name, _),
            ..
        } = field_type
        else {
            return self.box_value(right_val).into();
        };

        let Some(context) = &self.generic_context else {
            return self.box_value(right_val).into();
        };
        let Some(concrete_type) = context.type_params.get(param_name) else {
            return self.box_value(right_val).into();
        };

        match concrete_type {
            Type::Primitive(_) => self.box_value(right_val).into(),
            Type::Named(_, _) => right_val,
            _ => self.box_value(right_val).into(),
        }
    }

    fn assign_to_list_access(
        &mut self,
        target_expr: &ExpressionNode,
        index: &ExpressionNode,
        left: &ExpressionNode,
        right: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let ExpressionKind::ListAccess { .. } = &target_expr.kind {
            let (base_expr, all_indices) = self.collect_list_access_chain(left);
            if all_indices.len() <= 1 {
                return Err("Unexpected single-level nesting in nested path".to_string());
            }
            let right_val = self.generate_expression(right)?;
            let boxed_value = self.box_value(right_val);
            self.generate_nested_collection_assignment(
                base_expr,
                &all_indices,
                boxed_value.into(),
            )?;
            return Ok(right_val);
        }

        let target_type = self.resolve_expression_type_with_fallback(target_expr)?;
        match target_type {
            crate::semantics::Type::List(_) => {
                let target_val = self.generate_expression(target_expr)?;
                let index_val = self.generate_expression(index)?;
                let right_val = self.generate_expression(right)?;
                let boxed_value = self.box_value(right_val);
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
                let target_val = self.generate_expression(target_expr)?;
                let key_val = self.generate_expression(index)?;
                let right_val = self.generate_expression(right)?;
                let boxed_key = self.box_value(key_val);
                let boxed_value = self.box_value(right_val);
                self.builder
                    .build_call(
                        self.module
                            .get_function("mux_map_put_value")
                            .expect("mux_map_put_value must be declared in runtime"),
                        &[target_val.into(), boxed_key.into(), boxed_value.into()],
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

    fn generate_compound_assignment_expression(
        &mut self,
        left: &ExpressionNode,
        right: &ExpressionNode,
        is_add: bool,
    ) -> Result<BasicValueEnum<'a>, String> {
        let left_val = self.generate_expression(left)?;
        let right_val = self.generate_expression(right)?;
        let result = if left_val.is_int_value() {
            if is_add {
                self.builder
                    .build_int_add(
                        left_val.into_int_value(),
                        right_val.into_int_value(),
                        "add_assign",
                    )
                    .map_err(|e| e.to_string())?
                    .into()
            } else {
                self.builder
                    .build_int_sub(
                        left_val.into_int_value(),
                        right_val.into_int_value(),
                        "sub_assign",
                    )
                    .map_err(|e| e.to_string())?
                    .into()
            }
        } else if left_val.is_float_value() {
            if is_add {
                self.builder
                    .build_float_add(
                        left_val.into_float_value(),
                        right_val.into_float_value(),
                        "fadd_assign",
                    )
                    .map_err(|e| e.to_string())?
                    .into()
            } else {
                self.builder
                    .build_float_sub(
                        left_val.into_float_value(),
                        right_val.into_float_value(),
                        "fsub_assign",
                    )
                    .map_err(|e| e.to_string())?
                    .into()
            }
        } else if is_add {
            return Err("Unsupported add assign operands".to_string());
        } else {
            return Err("Unsupported sub assign operands".to_string());
        };

        match &left.kind {
            ExpressionKind::Identifier(name) => {
                let Some((ptr, _, _)) = self
                    .variables
                    .get(name)
                    .or_else(|| self.global_variables.get(name))
                else {
                    return Err(format!("Undefined variable {}", name));
                };
                let ptr_copy = *ptr;
                let boxed = self.box_value(result);
                self.builder
                    .build_store(ptr_copy, boxed)
                    .map_err(|e| e.to_string())?;
                Ok(result)
            }
            ExpressionKind::Unary {
                op: UnaryOp::Deref,
                expr: deref_expr,
                ..
            } => {
                let ref_val = self.generate_expression(deref_expr)?;
                let ptr = ref_val.into_pointer_value();
                let boxed = self.box_value(result);
                self.builder
                    .build_store(ptr, boxed)
                    .map_err(|e| e.to_string())?;
                Ok(result)
            }
            _ => Err("Assignment to non-identifier/deref not implemented".to_string()),
        }
    }

    fn generate_call_expression(
        &mut self,
        call_expr: &ExpressionNode,
        func: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        match &func.kind {
            ExpressionKind::FieldAccess { expr, field } => {
                self.generate_method_style_call(call_expr, expr, field, args)
            }
            ExpressionKind::Identifier(name) => self.generate_identifier_function_call(name, args),
            _ => self.generate_callable_expression_call(func, args),
        }
    }

    fn generate_method_style_call(
        &mut self,
        call_expr: &ExpressionNode,
        expr: &ExpressionNode,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if matches!(&expr.kind, ExpressionKind::Identifier(obj_name) if obj_name == "self") {
            return self.generate_method_call_on_self(field, args);
        }
        if let Some(result) =
            self.try_generate_implicit_self_field_method_call(call_expr, expr, field, args)?
        {
            return Ok(result);
        }
        if let Some(result) = self.try_generate_nested_module_method_call(expr, field, args)? {
            return Ok(result);
        }
        if let Some(result) = self.try_generate_identifier_method_target_call(expr, field, args)? {
            return Ok(result);
        }
        if let Some(result) = self.try_generate_generic_type_method_call(expr, field, args)? {
            return Ok(result);
        }
        self.generate_general_method_call(expr, field, args)
    }

    fn try_generate_implicit_self_field_method_call(
        &mut self,
        call_expr: &ExpressionNode,
        expr: &ExpressionNode,
        method: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let ExpressionKind::Identifier(field_name) = &expr.kind else {
            return Ok(None);
        };
        let Some(current_function) = &self.current_function_name else {
            return Ok(None);
        };
        if !current_function.contains('.') {
            return Ok(None);
        }
        let class_name = current_function
            .split('.')
            .next()
            .expect("function name contains '.' so next() should return Some");
        let Some(class_fields) = self.classes.get(class_name) else {
            return Ok(None);
        };
        if !class_fields.iter().any(|f| f.name == *field_name) {
            return Ok(None);
        }

        let field_type = class_fields
            .iter()
            .find(|f| f.name == *field_name)
            .expect("field should exist in class after semantic analysis")
            .type_
            .clone();
        let resolved_field_type = self
            .analyzer
            .resolve_type(&field_type)
            .map_err(|e| format!("Type resolution failed: {}", e))?;
        let self_field_expr = ExpressionNode {
            kind: ExpressionKind::FieldAccess {
                expr: Box::new(ExpressionNode {
                    kind: ExpressionKind::Identifier("self".to_string()),
                    span: expr.span,
                }),
                field: field_name.clone(),
            },
            span: call_expr.span,
        };
        let obj_value = self.generate_expression(&self_field_expr)?;
        self.generate_method_call(obj_value, &resolved_field_type, method, args)
            .map(Some)
    }

    fn try_generate_nested_module_method_call(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let ExpressionKind::FieldAccess {
            expr: inner_expr,
            field: submodule_name,
        } = &expr.kind
        else {
            return Ok(None);
        };
        if let ExpressionKind::Identifier(module_name) = &inner_expr.kind
            && let Some(symbol) = self.analyzer.symbol_table().lookup(module_name)
            && symbol.kind == crate::semantics::SymbolKind::Import
            && self
                .analyzer
                .imported_symbols()
                .get(module_name)
                .and_then(|module_syms| module_syms.get(submodule_name))
                .is_some()
        {
            let qualified_submodule_name = format!("{}.{}", module_name, submodule_name);
            let function_symbol = self
                .analyzer
                .imported_symbols()
                .get(&qualified_submodule_name)
                .and_then(|submodule_syms| submodule_syms.get(field))
                .or_else(|| {
                    self.analyzer
                        .imported_symbols()
                        .get(submodule_name)
                        .and_then(|submodule_syms| submodule_syms.get(field))
                });
            if let Some(function_symbol) = function_symbol
                && let Some(call_result) = self.call_imported_symbol_function(
                    Some(function_symbol.clone()),
                    field,
                    field,
                    args,
                )?
            {
                return Ok(Some(call_result));
            }
        }

        if let ExpressionKind::Identifier(module_name) = &inner_expr.kind
            && let Some(result) =
                self.try_resolve_module_class_static_call(module_name, submodule_name, field, args)?
        {
            return Ok(Some(result));
        }
        Ok(None)
    }

    fn try_generate_identifier_method_target_call(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let ExpressionKind::Identifier(name) = &expr.kind else {
            return Ok(None);
        };
        if let Some((_, _, var_type)) = self
            .variables
            .get(name)
            .or_else(|| self.global_variables.get(name))
        {
            let var_type_clone = var_type.clone();
            let obj_value = self.generate_expression(expr)?;
            return self
                .generate_method_call(obj_value, &var_type_clone, field, args)
                .map(Some);
        }
        let Some(symbol) = self.analyzer.symbol_table().lookup(name) else {
            return Ok(None);
        };
        match symbol.kind {
            crate::semantics::SymbolKind::Import => {
                self.generate_import_symbol_method_call(name, field, args)
            }
            crate::semantics::SymbolKind::Class => {
                self.generate_class_symbol_method_call(name, field, args)
            }
            crate::semantics::SymbolKind::Enum => {
                self.generate_enum_symbol_method_call(name, field, args)
            }
            _ => Ok(None),
        }
    }

    fn generate_import_symbol_method_call(
        &mut self,
        module_name: &str,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let function_symbol = self
            .analyzer
            .imported_symbols()
            .get(module_name)
            .and_then(|module_syms| module_syms.get(field));
        if let Some(call_result) =
            self.call_imported_symbol_function(function_symbol.cloned(), field, field, args)?
        {
            return Ok(Some(call_result));
        }
        if let Some(generic_func) = self.function_nodes.get(field)
            && !generic_func.type_params.is_empty()
        {
            return self.generate_generic_function_call(field, args).map(Some);
        }
        Err(format!(
            "Function {} not found in module {}",
            field, module_name
        ))
    }

    fn generate_class_symbol_method_call(
        &mut self,
        class_name: &str,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        if let Some(created) = self.try_generate_lock_new_call(class_name, field, args)? {
            return Ok(Some(created));
        }
        let class_symbol = self
            .analyzer
            .symbol_table()
            .lookup(class_name)
            .ok_or_else(|| format!("Class {} not found", class_name))?;
        let Some(method) = class_symbol.methods.get(field) else {
            return Err(format!(
                "Method {} not found on class {}",
                field, class_name
            ));
        };
        if let Some(call) = self.try_generate_net_static_method_call(class_name, field, args)? {
            return Ok(Some(call));
        }
        if !method.is_static {
            return Err(format!(
                "Method {} on class {} is not static",
                field, class_name
            ));
        }
        let call_args = self.build_call_args_from_expressions(args)?;
        let call = self
            .builder
            .build_call(
                self.module
                    .get_function(&format!("{}.{}", class_name, field))
                    .expect("static method should be in module"),
                &call_args,
                &format!("{}.{}_call", class_name, field),
            )
            .map_err(|e| e.to_string())?;
        Ok(Some(
            call.try_as_basic_value()
                .left()
                .expect("static method call should return a basic value"),
        ))
    }

    fn try_generate_lock_new_call(
        &mut self,
        class_name: &str,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        if !matches!(class_name, "Mutex" | "RwLock" | "CondVar") || field != "new" {
            return Ok(None);
        }
        if !args.is_empty() {
            return Err(format!("{}.new() takes no arguments", class_name));
        }
        let runtime_fn = match class_name {
            "Mutex" => "mux_mutex_new",
            "RwLock" => "mux_rwlock_new",
            "CondVar" => "mux_condvar_new",
            _ => unreachable!(),
        };
        let created = self
            .builder
            .build_call(
                self.module
                    .get_function(runtime_fn)
                    .ok_or(format!("{} not found", runtime_fn))?,
                &[],
                &format!("{}_new_call", class_name),
            )
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| format!("{} should return a value", runtime_fn))?;
        Ok(Some(created))
    }

    fn generate_enum_symbol_method_call(
        &mut self,
        enum_name: &str,
        variant: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let constructor_name = format!("{}!{}", enum_name, variant);
        let Some(constructor_func) = self.module.get_function(&constructor_name) else {
            return Err(format!(
                "Enum variant {} not found in enum {}",
                variant, enum_name
            ));
        };
        let call_args = self.build_call_args_from_expressions(args)?;
        let call = self
            .builder
            .build_call(
                constructor_func,
                &call_args,
                &format!("{}_call", constructor_name),
            )
            .map_err(|e| e.to_string())?;
        Ok(Some(
            call.try_as_basic_value()
                .left()
                .expect("constructor call should return a basic value"),
        ))
    }

    fn try_generate_generic_type_method_call(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let ExpressionKind::GenericType(class_name, type_args) = &expr.kind else {
            return Ok(None);
        };
        let resolved_class_name = class_name
            .split('.')
            .next_back()
            .unwrap_or(class_name.as_str())
            .to_string();
        let resolved_type_args = type_args
            .iter()
            .map(|arg| self.type_node_to_type(arg))
            .collect::<Vec<_>>();
        if field == "new" {
            let concrete_type_args = resolved_type_args
                .iter()
                .map(|arg| self.resolve_type(arg))
                .collect::<Result<Vec<_>, _>>()?;
            return self
                .generate_constructor_call_with_types(
                    &resolved_class_name,
                    &concrete_type_args,
                    args,
                )
                .map(Some);
        }
        let Some(class_symbol) = self.analyzer.symbol_table().lookup(&resolved_class_name) else {
            return Err(format!(
                "Method {} not found on class {}",
                field, class_name
            ));
        };
        let Some(method) = class_symbol.methods.get(field) else {
            return Err(format!(
                "Method {} not found on class {}",
                field, class_name
            ));
        };
        if !method.is_static {
            return Err(format!(
                "Method {} on class {} is not static",
                field, class_name
            ));
        }
        self.generate_generic_static_method_call(
            class_name,
            &resolved_class_name,
            &resolved_type_args,
            field,
            args,
        )
        .map(Some)
    }

    fn generate_generic_static_method_call(
        &mut self,
        class_name: &str,
        resolved_class_name: &str,
        resolved_type_args: &[Type],
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let context = GenericContext {
            type_params: self.build_type_param_map(resolved_class_name, resolved_type_args)?,
        };
        let old_context = self.generic_context.take();
        self.generic_context = Some(context);
        let call_result = (|| {
            let saved_variables = self.variables.clone();
            let saved_insert_block = self.builder.get_insert_block();
            if !resolved_type_args.is_empty() {
                self.generate_specialized_methods(resolved_class_name, resolved_type_args)?;
            }
            self.variables = saved_variables;
            if let Some(block) = saved_insert_block {
                self.builder.position_at_end(block);
            }
            let call_args = self.build_call_args_from_expressions(args)?;
            let specialized_method_name =
                self.create_specialized_method_name(class_name, resolved_type_args, field);
            let function_name = if self.module.get_function(&specialized_method_name).is_some() {
                specialized_method_name
            } else {
                format!("{}.{}", class_name, field)
            };
            let call = self
                .builder
                .build_call(
                    self.module
                        .get_function(&function_name)
                        .ok_or(format!("Method '{}' not found", function_name))?,
                    &call_args,
                    &format!("{}_call", function_name.replace('.', "_")),
                )
                .map_err(|e| e.to_string())?;
            Ok(call
                .try_as_basic_value()
                .left()
                .expect("generic method call should return a basic value"))
        })();
        self.generic_context = old_context;
        call_result
    }

    fn generate_general_method_call(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let obj_value = self.generate_expression(expr)?;
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
    }

    fn generate_identifier_function_call(
        &mut self,
        name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        if let Some(result) = self.try_generate_builtin_call(name, args)? {
            return Ok(result);
        }
        if let Some(result) = self.try_generate_variable_or_direct_function_call(name, args)? {
            return Ok(result);
        }
        if self.should_generate_generic_function_call(name) {
            return self.generate_generic_function_call(name, args);
        }
        let lookup_name = self.resolve_function_lookup_name(name);
        self.generate_named_function_call(name, &lookup_name, args)
    }

    fn try_generate_builtin_call(
        &mut self,
        name: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let value = match name {
            "print" => self.generate_print_call(args)?,
            "read_line" => self.generate_read_line_call(args)?,
            "err" => self.generate_err_builtin_call(args)?,
            "ok" => self.generate_ok_builtin_call(args)?,
            "some" => self.generate_some_builtin_call(args)?,
            "none" => self.generate_none_builtin_call(args)?,
            _ => return Ok(None),
        };
        Ok(Some(value))
    }

    fn try_generate_variable_or_direct_function_call(
        &mut self,
        name: &str,
        args: &[ExpressionNode],
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let Some((ptr, _, var_type)) = self
            .variables
            .get(name)
            .or_else(|| self.global_variables.get(name))
        else {
            return Ok(None);
        };
        let ptr_copy = *ptr;
        let var_type_copy = var_type.clone();
        if let Type::Function {
            params, returns, ..
        } = var_type_copy
        {
            let closure_ptr = self
                .builder
                .build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    ptr_copy,
                    name,
                )
                .map_err(|e| e.to_string())?
                .into_pointer_value();
            return self
                .call_closure_with_optional_captures(closure_ptr, &params, &returns, args)
                .map(Some);
        }
        if let Some(func) = self.module.get_function(name) {
            let mut call_args = self.build_args_with_class_copy(args)?;
            self.append_default_call_args(name, args.len(), &mut call_args)?;
            let call = self
                .builder
                .build_call(func, &call_args, "user_func_call")
                .map_err(|e| e.to_string())?;
            return Ok(Some(self.call_result_or_default_i32(call)));
        }
        Err(format!("Undefined function: {}", name))
    }

    fn should_generate_generic_function_call(&mut self, name: &str) -> bool {
        if let Some(func_node) = self.function_nodes.get(name)
            && !func_node.type_params.is_empty()
        {
            return true;
        }
        if let Some(func_symbol) = self.analyzer.symbol_table().lookup(name)
            && let SymbolKind::Function = func_symbol.kind
            && let Some(func_node) = self.function_nodes.get(name)
        {
            return !func_node.type_params.is_empty()
                && func_node.type_params.iter().any(|(param_name, _)| {
                    param_name.chars().next().unwrap_or(' ').is_uppercase() || param_name.len() > 3
                });
        }
        false
    }

    fn resolve_function_lookup_name(&mut self, name: &str) -> String {
        let mut lookup_name = name.to_string();
        if let Some(nested_name) = self.find_nested_mangled_function_name(name) {
            lookup_name = nested_name;
        } else if let Some(import_llvm_name) = self.resolve_imported_llvm_name(name) {
            lookup_name = import_llvm_name;
        }
        lookup_name
    }

    fn find_nested_mangled_function_name(&self, name: &str) -> Option<String> {
        let parent_fn = self.current_function_name.as_ref()?;
        let direct = format!("{}!{}", parent_fn, name);
        if self.module.get_function(&direct).is_some() {
            return Some(direct);
        }
        let parts: Vec<&str> = parent_fn.split('!').collect();
        if parts.len() <= 1 {
            return None;
        }
        for i in 1..parts.len() {
            let prefix = parts[0..i].join("!");
            let nested = format!("{}!{}", prefix, name);
            if self.module.get_function(&nested).is_some() {
                return Some(nested);
            }
        }
        None
    }

    fn resolve_imported_llvm_name(&mut self, name: &str) -> Option<String> {
        let symbol = self.analyzer.symbol_table().lookup(name)?;
        symbol
            .llvm_name
            .as_ref()
            .cloned()
            .or_else(|| Some(name.to_string()))
    }

    fn generate_named_function_call(
        &mut self,
        display_name: &str,
        lookup_name: &str,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let Some(func) = self.module.get_function(lookup_name) else {
            return Err(format!(
                "Undefined function: {} (looked for LLVM name: {})",
                display_name, lookup_name
            ));
        };
        let mut call_args = self.build_args_with_class_copy(args)?;
        self.append_default_call_args(display_name, args.len(), &mut call_args)?;
        let call = self
            .builder
            .build_call(func, &call_args, "user_func_call")
            .map_err(|e| e.to_string())?;
        Ok(self.call_result_or_default_i32(call))
    }

    fn generate_callable_expression_call(
        &mut self,
        func: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<BasicValueEnum<'a>, String> {
        let closure_ptr = self.generate_expression(func)?.into_pointer_value();
        let func_type = self
            .analyzer
            .get_expression_type(func)
            .map_err(|e| e.to_string())?;
        if let Type::Function {
            params, returns, ..
        } = func_type
        {
            return self.call_closure_with_optional_captures(closure_ptr, &params, &returns, args);
        }
        Err(format!("Cannot call non-function type: {:?}", func_type))
    }

    fn generate_list_access_expression(
        &mut self,
        target_expr: &ExpressionNode,
        index: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        let target_val = self.generate_expression(target_expr)?;
        let index_val = self.generate_expression(index)?;

        // Get the target type to determine if it's a list or map
        let target_type = self.resolve_expression_type_with_fallback(target_expr)?;

        match &target_type {
            crate::semantics::Type::List(element_type) => {
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
                let normalized_index = self.normalize_list_index(index_val, raw_list_ptr)?;

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

                // Use extract_value_from_ptr to properly extract based on type
                let (extracted_val, _) =
                    self.extract_value_from_ptr(result_ptr, element_type, "list_element")?;
                Ok(extracted_val)
            }
            crate::semantics::Type::Map(_, value_type) => {
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

                // Use extract_value_from_ptr to properly extract based on type
                let (extracted_val, _) =
                    self.extract_value_from_ptr(value_ptr, value_type, "map_element")?;
                Ok(extracted_val)
            }
            _ => Err(format!(
                "ListAccess target must be a list or map, found {:?}",
                target_type
            )),
        }
    }

    fn generate_field_access_expression(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let Some(value) = self.try_generate_stdlib_constant_field_access(expr, field)? {
            return Ok(value);
        }
        if let Some(value) = self.try_generate_tuple_field_access(expr, field)? {
            return Ok(value);
        }
        if self.is_csv_expression(expr)? {
            return self.generate_csv_field_access(expr, field);
        }
        if let Some(value) = self.try_generate_class_field_access(expr, field)? {
            return Ok(value);
        }
        if let Some(value) = self.try_generate_primitive_field_method_access(expr, field)? {
            return Ok(value);
        }
        Err(format!(
            "Field access not supported for expression type {:?}",
            expr.kind
        ))
    }

    fn try_generate_stdlib_constant_field_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let ExpressionKind::Identifier(module_name) = &expr.kind else {
            return Ok(None);
        };
        let Some(symbol) = self.analyzer.symbol_table().lookup(module_name) else {
            return Ok(None);
        };
        if symbol.kind != crate::semantics::SymbolKind::Import {
            return Ok(None);
        }
        let Some(module_syms) = self.analyzer.imported_symbols().get(module_name) else {
            return Ok(None);
        };
        let Some(field_sym) = module_syms.get(field) else {
            return Ok(None);
        };
        if field_sym.kind != crate::semantics::SymbolKind::Constant {
            return Ok(None);
        }

        use crate::semantics::stdlib::{ConstantValue, lookup_stdlib_item};
        let full_name = format!("{}.{}", module_name, field);
        let Some(crate::semantics::stdlib::StdlibItem::Constant { value, .. }) =
            lookup_stdlib_item(&full_name)
        else {
            return Ok(None);
        };
        let generated = match value {
            ConstantValue::Float(f) => self.context.f64_type().const_float(f).into(),
            ConstantValue::Int(i) => self.context.i64_type().const_int(i as u64, false).into(),
            ConstantValue::Bool(b) => self.context.bool_type().const_int(b as u64, false).into(),
        };
        Ok(Some(generated))
    }

    fn try_generate_tuple_field_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let expr_type = self
            .analyzer
            .get_expression_type(expr)
            .map_err(|e| e.to_string())?;
        let Type::Tuple(left_type, right_type) = expr_type else {
            return Ok(None);
        };

        let tuple_value_ptr = self.generate_expression(expr)?.into_pointer_value();
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
        let (field_index, field_type) = match field {
            "left" => (0, *left_type),
            "right" => (1, *right_type),
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
        let unboxed = self.unbox_value_for_type(field_value, &field_type)?;
        Ok(Some(unboxed))
    }

    fn is_csv_expression(&mut self, expr: &ExpressionNode) -> Result<bool, String> {
        let expr_type = self
            .analyzer
            .get_expression_type(expr)
            .map_err(|e| e.to_string())?;
        Ok(matches!(expr_type, Type::Named(name, _) if name == "Csv"))
    }

    fn try_generate_class_field_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let Some(class_name) = self.resolve_expression_class_name(expr) else {
            return Ok(None);
        };
        let struct_ptr = self.resolve_struct_pointer_for_field_access(expr, "data_ptr")?;
        let field_indices = self
            .field_map
            .get(class_name.as_str())
            .ok_or("Field map not found")?;
        let index = field_indices
            .get(field)
            .copied()
            .ok_or_else(|| format!("Field {} not found", field))?;
        let field_ptr = self.resolve_struct_field_pointer(&class_name, field, struct_ptr)?;
        self.load_class_field_value(expr, &class_name, field, index, field_ptr)
            .map(Some)
    }

    fn load_class_field_value(
        &mut self,
        expr: &ExpressionNode,
        class_name: &str,
        field: &str,
        index: usize,
        field_ptr: PointerValue<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let field_types = self
            .field_types_map
            .get(class_name)
            .ok_or("Field types not found for class")?;
        let field_type = *field_types
            .get(index)
            .ok_or("Field type index out of bounds")?;
        if let BasicTypeEnum::StructType(struct_type) = field_type {
            return self
                .builder
                .build_load(struct_type, field_ptr, field)
                .map_err(|e| e.to_string());
        }
        if field_type == self.context.i64_type().into() {
            return self
                .builder
                .build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    field_ptr,
                    field,
                )
                .map_err(|e| e.to_string());
        }

        let field_def = self
            .classes
            .get(class_name)
            .and_then(|fields| fields.iter().find(|f| f.name == field))
            .cloned()
            .ok_or("Field not found")?;
        let resolved_field_type = self
            .analyzer
            .resolve_type(&field_def.type_)
            .map_err(|e| e.to_string())?;
        let loaded = self
            .builder
            .build_load(field_type, field_ptr, field)
            .map_err(|e| e.to_string())?;
        if let Some(unboxed) =
            self.try_unbox_generic_field_value(expr, class_name, &field_def, loaded)?
        {
            return Ok(unboxed);
        }
        if let Some(unboxed) =
            self.try_unbox_named_substituted_field_value(loaded, &resolved_field_type)?
        {
            return Ok(unboxed);
        }
        self.unbox_value_for_type_or_identity(loaded, &resolved_field_type)
    }

    fn try_unbox_generic_field_value(
        &mut self,
        expr: &ExpressionNode,
        class_name: &str,
        field_def: &crate::ast::Field,
        loaded: BasicValueEnum<'a>,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        if !field_def.is_generic_param {
            return Ok(None);
        }
        let TypeNode {
            kind: TypeKind::Named(param_name, _),
            ..
        } = &field_def.type_
        else {
            return Ok(None);
        };
        let Some(concrete_type) =
            self.resolve_generic_field_concrete_type(expr, class_name, param_name.as_str())
        else {
            return Ok(None);
        };
        self.unbox_value_for_type_or_identity(loaded, &concrete_type)
            .map(Some)
    }

    fn resolve_generic_field_concrete_type(
        &self,
        expr: &ExpressionNode,
        class_name: &str,
        param_name: &str,
    ) -> Option<Type> {
        if let Some(context) = &self.generic_context
            && let Some(concrete) = context.type_params.get(param_name)
        {
            return Some(concrete.clone());
        }
        let ExpressionKind::Identifier(obj_name) = &expr.kind else {
            return None;
        };
        let (_, _, obj_type) = self
            .variables
            .get(obj_name)
            .or_else(|| self.global_variables.get(obj_name))?;
        let type_args = match obj_type {
            Type::Named(_, type_args) | Type::Instantiated(_, type_args) => type_args,
            _ => return None,
        };
        if type_args.is_empty() {
            return None;
        }
        let class_symbol = self.analyzer.all_symbols().get(class_name)?;
        let param_index = class_symbol
            .type_params
            .iter()
            .position(|(p, _)| p == param_name)?;
        type_args.get(param_index).cloned()
    }

    fn try_unbox_named_substituted_field_value(
        &mut self,
        loaded: BasicValueEnum<'a>,
        resolved_field_type: &Type,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let Type::Named(name, _) = resolved_field_type else {
            return Ok(None);
        };
        let Some(context) = &self.generic_context else {
            return Ok(None);
        };
        let Some(concrete_type) = context.type_params.get(name).cloned() else {
            return Ok(None);
        };
        self.unbox_value_for_type_or_identity(loaded, &concrete_type)
            .map(Some)
    }

    fn unbox_value_for_type_or_identity(
        &mut self,
        loaded: BasicValueEnum<'a>,
        ty: &Type,
    ) -> Result<BasicValueEnum<'a>, String> {
        match ty {
            Type::Primitive(PrimitiveType::Int)
            | Type::Primitive(PrimitiveType::Float)
            | Type::Primitive(PrimitiveType::Bool)
            | Type::Primitive(PrimitiveType::Char)
            | Type::Primitive(PrimitiveType::Str) => self.unbox_value_for_type(loaded, ty),
            _ => Ok(loaded),
        }
    }

    fn unbox_value_for_type(
        &mut self,
        value: BasicValueEnum<'a>,
        ty: &Type,
    ) -> Result<BasicValueEnum<'a>, String> {
        match ty {
            Type::Primitive(PrimitiveType::Int) => self.get_raw_int_value(value).map(|v| v.into()),
            Type::Primitive(PrimitiveType::Float) => {
                self.get_raw_float_value(value).map(|v| v.into())
            }
            Type::Primitive(PrimitiveType::Bool) => {
                self.get_raw_bool_value(value).map(|v| v.into())
            }
            Type::Primitive(PrimitiveType::Char) => self.get_raw_int_value(value).map(|v| v.into()),
            Type::Primitive(PrimitiveType::Str) => Ok(value),
            Type::Primitive(PrimitiveType::Void) | Type::Primitive(PrimitiveType::Auto) => {
                Err(format!("Unsupported tuple field type {:?}", ty))
            }
            _ => Ok(value),
        }
    }

    fn try_generate_primitive_field_method_access(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        let ExpressionKind::Identifier(obj_name) = &expr.kind else {
            return Ok(None);
        };
        let Some((_, _, type_node)) = self
            .variables
            .get(obj_name)
            .or_else(|| self.global_variables.get(obj_name))
        else {
            return Ok(None);
        };
        let type_node = type_node.clone();
        match type_node {
            Type::Primitive(PrimitiveType::Int) => self.generate_int_field_method(expr, field),
            Type::Primitive(PrimitiveType::Float) => self.generate_float_field_method(expr, field),
            Type::Primitive(PrimitiveType::Bool) => self.generate_bool_field_method(expr, field),
            Type::Primitive(PrimitiveType::Str) => self.generate_string_field_method(expr, field),
            Type::Primitive(PrimitiveType::Char) => self.generate_char_field_method(expr, field),
            _ => Ok(None),
        }
    }

    fn generate_int_field_method(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match field {
            "to_string" => {
                let value = self.generate_expression(expr)?;
                self.value_to_runtime_string(value).map(Some)
            }
            "to_float" => {
                let value = self.generate_expression(expr)?;
                let raw_int = self.get_raw_int_value(value)?;
                let float_val = self
                    .builder
                    .build_signed_int_to_float(raw_int, self.context.f64_type(), "int_to_float")
                    .map_err(|e| e.to_string())?;
                Ok(Some(float_val.into()))
            }
            "to_int" => self.generate_expression(expr).map(Some),
            _ => Ok(None),
        }
    }

    fn generate_float_field_method(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match field {
            "to_string" => {
                let float_val = self.generate_expression(expr)?;
                let func = self
                    .module
                    .get_function("mux_float_to_string")
                    .ok_or("mux_float_to_string not found")?;
                let cstr = self
                    .builder
                    .build_call(func, &[float_val.into()], "float_to_str")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_float_to_string should return a basic value")?;
                self.cstr_to_mux_string(cstr).map(Some)
            }
            "to_int" => {
                let value = self.generate_expression(expr)?;
                let raw_float = self.get_raw_float_value(value)?;
                let int_val = self
                    .builder
                    .build_float_to_signed_int(raw_float, self.context.i64_type(), "float_to_int")
                    .map_err(|e| e.to_string())?;
                Ok(Some(int_val.into()))
            }
            "to_float" => self.generate_expression(expr).map(Some),
            _ => Ok(None),
        }
    }

    fn generate_bool_field_method(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match field {
            "to_string" => {
                let value = self.generate_expression(expr)?;
                self.value_to_runtime_string(value).map(Some)
            }
            "to_int" => {
                let value = self.generate_expression(expr)?;
                let raw_bool = self.get_raw_bool_value(value)?;
                let int_val = self
                    .builder
                    .build_int_z_extend(raw_bool, self.context.i64_type(), "bool_to_int")
                    .map_err(|e| e.to_string())?;
                Ok(Some(int_val.into()))
            }
            "to_float" => {
                let value = self.generate_expression(expr)?;
                let raw_bool = self.get_raw_bool_value(value)?;
                let float_val = self
                    .builder
                    .build_unsigned_int_to_float(raw_bool, self.context.f64_type(), "bool_to_float")
                    .map_err(|e| e.to_string())?;
                Ok(Some(float_val.into()))
            }
            _ => Ok(None),
        }
    }

    fn generate_string_field_method(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match field {
            "to_string" => {
                let value = self.generate_expression(expr)?;
                self.value_to_runtime_string(value).map(Some)
            }
            "to_int" => {
                let value = self.generate_expression(expr)?;
                let cstr = self.value_to_cstr(value)?;
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
                    .ok_or("mux_string_to_int should return a basic value")?;
                Ok(Some(result_ptr))
            }
            "to_float" => {
                let value = self.generate_expression(expr)?;
                let cstr = self.value_to_cstr(value)?;
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
                    .ok_or("mux_string_to_float should return a basic value")?;
                Ok(Some(result_ptr))
            }
            _ => Ok(None),
        }
    }

    fn generate_char_field_method(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match field {
            "to_int" => {
                let char_val = self.generate_expression(expr)?;
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
                    .ok_or("mux_char_to_int should return a basic value")?;
                Ok(Some(result_ptr))
            }
            "to_string" => {
                let char_val = self.generate_expression(expr)?;
                let func = self
                    .module
                    .get_function("mux_char_to_string")
                    .ok_or("mux_char_to_string not found")?;
                let cstr = self
                    .builder
                    .build_call(func, &[char_val.into()], "char_to_cstr")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .ok_or("mux_char_to_string should return a basic value")?;
                self.cstr_to_mux_string(cstr).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn value_to_cstr(&mut self, value: BasicValueEnum<'a>) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function("mux_value_to_string")
            .ok_or("mux_value_to_string not found")?;
        self.builder
            .build_call(func, &[value.into()], "val_to_cstr")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("mux_value_to_string should return a basic value".to_string())
    }

    fn cstr_to_mux_string(
        &mut self,
        cstr: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let func_new = self
            .module
            .get_function("mux_new_string_from_cstr")
            .ok_or("mux_new_string_from_cstr not found")?;
        self.builder
            .build_call(func_new, &[cstr.into()], "new_str")
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("mux_new_string_from_cstr should return a basic value".to_string())
    }

    fn value_to_runtime_string(
        &mut self,
        value: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        let cstr = self.value_to_cstr(value)?;
        self.cstr_to_mux_string(cstr)
    }

    fn generate_expression_impl(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'a>, String> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => self.generate_literal(lit),
            ExpressionKind::None => self.generate_none_expression(),
            ExpressionKind::Identifier(name) => self.generate_identifier_expression(name),
            ExpressionKind::Binary {
                left, op, right, ..
            } => self.generate_binary_expression(left, op, right),
            ExpressionKind::Call { func, args } => self.generate_call_expression(expr, func, args),
            ExpressionKind::ListAccess {
                expr: target_expr,
                index,
            } => self.generate_list_access_expression(target_expr, index),
            ExpressionKind::ListLiteral(elements) => {
                self.generate_list_literal_expression(elements)
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                self.generate_map_literal_expression(entries)
            }
            ExpressionKind::SetLiteral(elements) => self.generate_set_literal_expression(elements),
            ExpressionKind::TupleLiteral(elements) => {
                self.generate_tuple_literal_expression(elements)
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
                self.generate_field_access_expression(expr, field)
            }
            ExpressionKind::Unary { op, expr, .. } => self.generate_unary_expression(op, expr),
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
    ///    a. Get base[i1] -> temp (copy)
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
            // 1. Get base[i1] -> temp (copy)
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
                self.generate_runtime_call("mux_flush_stdout", &[]);
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
