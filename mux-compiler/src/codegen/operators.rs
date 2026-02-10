//! Binary and logical operator generation for the code generator.
//!
//! This module handles:
//! - Short-circuit logical operators (&&, ||)
//! - Binary arithmetic operators (+, -, *, /, %, **)
//! - Comparison operators (==, !=, <, >, <=, >=)
//! - The 'in' operator for containment checks

use inkwell::values::{BasicValueEnum, PointerValue};

use crate::ast::{BinaryOp, ExpressionNode, PrimitiveType};
use crate::semantics::Type;

use super::CodeGenerator;

impl<'a> CodeGenerator<'a> {
    /// Ensure a value is a pointer, boxing it if necessary.
    fn ensure_pointer(&mut self, val: BasicValueEnum<'a>) -> PointerValue<'a> {
        if val.is_pointer_value() {
            val.into_pointer_value()
        } else {
            self.box_value(val)
        }
    }

    /// Generate a numeric comparison (int or float) with the given predicates.
    fn generate_numeric_compare(
        &mut self,
        left: BasicValueEnum<'a>,
        right: BasicValueEnum<'a>,
        int_pred: inkwell::IntPredicate,
        float_pred: inkwell::FloatPredicate,
        label: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let (Ok(left_int), Ok(right_int)) =
            (self.get_raw_int_value(left), self.get_raw_int_value(right))
        {
            self.builder
                .build_int_compare(int_pred, left_int, right_int, label)
                .map_err(|e| e.to_string())
                .map(|v| v.into())
        } else if let (Ok(left_float), Ok(right_float)) = (
            self.get_raw_float_value(left),
            self.get_raw_float_value(right),
        ) {
            let flabel = format!("f{}", label);
            self.builder
                .build_float_compare(float_pred, left_float, right_float, &flabel)
                .map_err(|e| e.to_string())
                .map(|v| v.into())
        } else {
            Err(format!("Unsupported {} operands", label))
        }
    }

    /// Call a runtime comparison function on two pointer values and convert
    /// the i32 result to an i1 bool.
    fn call_comparison_runtime(
        &mut self,
        left: PointerValue<'a>,
        right: PointerValue<'a>,
        func_name: &str,
        label: &str,
    ) -> Result<BasicValueEnum<'a>, String> {
        let func = self
            .module
            .get_function(func_name)
            .ok_or_else(|| format!("{} not found", func_name))?;
        let result = self
            .builder
            .build_call(func, &[left.into(), right.into()], label)
            .map_err(|e| e.to_string())?
            .try_as_basic_value()
            .left()
            .ok_or("Call returned no value")?;

        let result_i32 = result.into_int_value();
        let zero = self.context.i32_type().const_zero();
        self.builder
            .build_int_compare(inkwell::IntPredicate::NE, result_i32, zero, "to_bool")
            .map_err(|e| e.to_string())
            .map(|v| v.into())
    }

    pub(super) fn generate_short_circuit_logical_op(
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

    pub(super) fn generate_binary_op(
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
                        let left_ptr = self.ensure_pointer(left);
                        let right_ptr = self.ensure_pointer(right);
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
            BinaryOp::Exponent => {
                // try to get raw int values first
                if let (Ok(left_int), Ok(right_int)) =
                    (self.get_raw_int_value(left), self.get_raw_int_value(right))
                {
                    let pow_fn = self
                        .module
                        .get_function("mux_int_pow")
                        .ok_or("mux_int_pow not found")?;
                    let result = self
                        .builder
                        .build_call(pow_fn, &[left_int.into(), right_int.into()], "pow")
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .ok_or("Call returned no value")?;
                    Ok(result)
                } else if let (Ok(left_float), Ok(right_float)) = (
                    self.get_raw_float_value(left),
                    self.get_raw_float_value(right),
                ) {
                    let pow_fn = self
                        .module
                        .get_function("mux_math_pow")
                        .ok_or("mux_math_pow not found")?;
                    let result = self
                        .builder
                        .build_call(pow_fn, &[left_float.into(), right_float.into()], "pow")
                        .map_err(|e| e.to_string())?
                        .try_as_basic_value()
                        .left()
                        .ok_or("Call returned no value")?;
                    Ok(result)
                } else {
                    Err("Unsupported pow operands".to_string())
                }
            }
            BinaryOp::Equal => {
                // Get the semantic type to determine what kind of comparison to perform
                let left_type = self
                    .analyzer
                    .get_expression_type(left_expr)
                    .map_err(|e| format!("Failed to get left operand type: {}", e))?;

                match &left_type {
                    Type::Primitive(PrimitiveType::Str) => {
                        let left_ptr = self.ensure_pointer(left);
                        let right_ptr = self.ensure_pointer(right);
                        let left_cstr = self.extract_c_string_from_value(left_ptr)?;
                        let right_cstr = self.extract_c_string_from_value(right_ptr)?;
                        self.call_comparison_runtime(
                            left_cstr,
                            right_cstr,
                            "mux_string_equal",
                            "string_equal",
                        )
                    }
                    Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Char) => {
                        let left_int = self.get_raw_int_value(left)?;
                        let right_int = self.get_raw_int_value(right)?;
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::EQ, left_int, right_int, "eq")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
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
                    Type::List(_)
                    | Type::Map(_, _)
                    | Type::Set(_)
                    | Type::Tuple(_, _)
                    | Type::EmptyList
                    | Type::EmptyMap
                    | Type::EmptySet => {
                        let left_ptr = self.ensure_pointer(left);
                        let right_ptr = self.ensure_pointer(right);
                        self.call_comparison_runtime(
                            left_ptr,
                            right_ptr,
                            "mux_value_equal",
                            "value_equal",
                        )
                    }
                    _ => Err(format!(
                        "Equality comparison not supported for type: {:?}",
                        left_type
                    )),
                }
            }
            BinaryOp::Less => self.generate_numeric_compare(
                left,
                right,
                inkwell::IntPredicate::SLT,
                inkwell::FloatPredicate::OLT,
                "lt",
            ),
            BinaryOp::Greater => self.generate_numeric_compare(
                left,
                right,
                inkwell::IntPredicate::SGT,
                inkwell::FloatPredicate::OGT,
                "gt",
            ),
            BinaryOp::LessEqual => self.generate_numeric_compare(
                left,
                right,
                inkwell::IntPredicate::SLE,
                inkwell::FloatPredicate::OLE,
                "le",
            ),
            BinaryOp::GreaterEqual => self.generate_numeric_compare(
                left,
                right,
                inkwell::IntPredicate::SGE,
                inkwell::FloatPredicate::OGE,
                "ge",
            ),
            BinaryOp::NotEqual => {
                // Get the semantic type to determine what kind of comparison to perform
                let left_type = self
                    .analyzer
                    .get_expression_type(left_expr)
                    .map_err(|e| format!("Failed to get left operand type: {}", e))?;

                match &left_type {
                    Type::Primitive(PrimitiveType::Str) => {
                        let left_ptr = self.ensure_pointer(left);
                        let right_ptr = self.ensure_pointer(right);
                        let left_cstr = self.extract_c_string_from_value(left_ptr)?;
                        let right_cstr = self.extract_c_string_from_value(right_ptr)?;
                        self.call_comparison_runtime(
                            left_cstr,
                            right_cstr,
                            "mux_string_not_equal",
                            "string_not_equal",
                        )
                    }
                    Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Char) => {
                        let left_int = self.get_raw_int_value(left)?;
                        let right_int = self.get_raw_int_value(right)?;
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::NE, left_int, right_int, "ne")
                            .map_err(|e| e.to_string())
                            .map(|v| v.into())
                    }
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
                    Type::List(_)
                    | Type::Map(_, _)
                    | Type::Set(_)
                    | Type::EmptyList
                    | Type::EmptyMap
                    | Type::EmptySet => {
                        let left_ptr = self.ensure_pointer(left);
                        let right_ptr = self.ensure_pointer(right);
                        self.call_comparison_runtime(
                            left_ptr,
                            right_ptr,
                            "mux_value_not_equal",
                            "value_not_equal",
                        )
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
                let right_type = self
                    .analyzer
                    .get_expression_type(_right_expr)
                    .map_err(|e| format!("Failed to get right operand type: {}", e))?;

                match right_type {
                    Type::List(_) | Type::EmptyList => {
                        let raw_list = self.extract_list_from_value(right.into_pointer_value())?;
                        let item_ptr = self.ensure_pointer(left);
                        let result = self
                            .generate_runtime_call(
                                "mux_list_contains",
                                &[raw_list.into(), item_ptr.into()],
                            )
                            .ok_or("mux_list_contains returned no value")?;
                        Ok(result)
                    }

                    Type::Set(_) | Type::EmptySet => {
                        let raw_set = self.extract_set_from_value(right.into_pointer_value())?;
                        let item_ptr = self.ensure_pointer(left);
                        let result = self
                            .generate_runtime_call(
                                "mux_set_contains",
                                &[raw_set.into(), item_ptr.into()],
                            )
                            .ok_or("mux_set_contains returned no value")?;
                        Ok(result)
                    }

                    Type::Primitive(PrimitiveType::Str) => {
                        // String/char substring search
                        let left_type = self
                            .analyzer
                            .get_expression_type(left_expr)
                            .map_err(|e| format!("Failed to get left operand type: {}", e))?;

                        // String is *const c_char
                        let string_ptr = right.into_pointer_value();

                        match left_type {
                            Type::Primitive(PrimitiveType::Char) => {
                                // Char in string: mux_string_contains_char(string, char_as_i64)
                                let char_i64 = left.into_int_value();

                                let contains_fn = self
                                    .module
                                    .get_function("mux_string_contains_char")
                                    .ok_or("mux_string_contains_char not found")?;
                                let result = self
                                    .builder
                                    .build_call(
                                        contains_fn,
                                        &[string_ptr.into(), char_i64.into()],
                                        "string_contains_char",
                                    )
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_string_contains_char should return a basic value");

                                Ok(result)
                            }
                            Type::Primitive(PrimitiveType::Str) => {
                                // Substring search: mux_string_contains(haystack, needle)
                                let substring_ptr = left.into_pointer_value();

                                let contains_fn = self
                                    .module
                                    .get_function("mux_string_contains")
                                    .ok_or("mux_string_contains not found")?;
                                let result = self
                                    .builder
                                    .build_call(
                                        contains_fn,
                                        &[string_ptr.into(), substring_ptr.into()],
                                        "string_contains",
                                    )
                                    .map_err(|e| e.to_string())?
                                    .try_as_basic_value()
                                    .left()
                                    .expect("mux_string_contains should return a basic value");

                                Ok(result)
                            }
                            _ => Err(format!(
                                "Invalid left operand type for 'in' operator with string: {:?}",
                                left_type
                            )),
                        }
                    }

                    _ => Err(format!(
                        "'in' operator not supported for type: {:?}",
                        right_type
                    )),
                }
            }
            _ => Err("Binary op not implemented".to_string()),
        }
    }
}
