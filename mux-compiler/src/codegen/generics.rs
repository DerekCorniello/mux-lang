//! Generic type instantiation and specialization for the code generator.
//!
//! This module handles:
//! - Specialized method generation for generic classes
//! - Generic function instantiation and calls
//! - Type substitution in AST nodes

use std::collections::HashMap;

use crate::ast::{
    ExpressionKind, ExpressionNode, Param, PrimitiveType, StatementKind, StatementNode, TypeKind,
    TypeNode,
};
use crate::lexer::Span;
use crate::semantics::Type;

use super::CodeGenerator;
use super::GenericContext;

impl<'a> CodeGenerator<'a> {
    pub(super) fn generate_specialized_methods(
        &mut self,
        class_name: &str,
        type_args: &[Type],
    ) -> Result<(), String> {
        // save the current builder position so we can restore it after generating specialized methods
        let saved_insert_block = self.builder.get_insert_block();

        // check if we need to generate specialized methods for this variant
        let variant_suffix = type_args
            .iter()
            .map(|t| self.sanitize_type_name(t))
            .collect::<Vec<_>>()
            .join("$");
        let variant_key = format!("{}${}", class_name, variant_suffix);

        // skip if we've already generated methods for this variant
        if self.generated_methods.contains_key(&variant_key) {
            return Ok(());
        }

        // mark this variant as being processed to prevent infinite recursion
        self.generated_methods.insert(variant_key.clone(), true);

        // get the class symbol to access methods
        let class_symbol = self
            .analyzer
            .symbol_table()
            .lookup(class_name)
            .ok_or(format!("Class {} not found", class_name))?;

        // Set class-level type parameter bounds for specialized method generation
        if !class_symbol.type_params.is_empty() {
            self.analyzer
                .set_class_type_params(class_symbol.type_params.clone());
        }

        // generate specialized methods (including static methods)
        for (method_name, method_sig) in &class_symbol.methods {
            let specialized_method_name =
                self.create_specialized_method_name(class_name, type_args, method_name);

            // skip if this specific method was already generated
            if self
                .generated_methods
                .contains_key(&specialized_method_name)
            {
                continue;
            }

            // get the original method AST node
            let original_method_name = if method_sig.is_static {
                // static methods are stored as "ClassName.method_name"
                format!("{}.{}", class_name, method_name)
            } else {
                // instance methods are stored as "ClassName.method_name" too
                format!("{}.{}", class_name, method_name)
            };

            if let Some(method_node) = self.function_nodes.get(&original_method_name) {
                // clone the method and specialize it
                let mut specialized_method = method_node.clone();
                specialized_method.name = specialized_method_name.clone();

                // create type parameter substitution map
                let type_param_map = class_symbol
                    .type_params
                    .iter()
                    .enumerate()
                    .map(|(_i, param)| (param.0.clone(), type_args[_i].clone()))
                    .collect::<HashMap<_, _>>();

                // substitute types in parameters and return type
                for param in &mut specialized_method.params {
                    param.type_ = self.substitute_types_in_type_node(&param.type_, &type_param_map);
                }
                specialized_method.return_type = self.substitute_types_in_type_node(
                    &specialized_method.return_type,
                    &type_param_map,
                );

                // substitute types in the method body
                let mut substituted_body = Vec::new();
                for stmt in &specialized_method.body {
                    substituted_body
                        .push(self.substitute_types_in_statement(stmt, &type_param_map));
                }
                specialized_method.body = substituted_body;

                // set up generic context for specialized method generation
                let specialized_context = GenericContext {
                    type_params: type_param_map,
                };
                let old_context = self.generic_context.take();
                self.generic_context = Some(specialized_context);

                // IMPORTANT: Save the current variables table before generating the specialized method
                // because generate_function clears self.variables, which would lose the current function's
                // parameters if we're generating this specialized method from within another function
                let saved_variables = self.variables.clone();

                // declare the specialized method first
                self.declare_function(&specialized_method)?;

                // generate the specialized method
                // Note: generate_function now handles its own RC scope stack isolation internally,
                // so we don't need to save/restore it here
                self.generate_function(&specialized_method)?;

                // mark as generated
                self.generated_methods.insert(specialized_method_name, true);

                // restore original context and variables
                self.generic_context = old_context;
                self.variables = saved_variables;
            }
        }

        // restore the builder position to where we were before generating specialized methods
        if let Some(block) = saved_insert_block {
            self.builder.position_at_end(block);
        }

        // Clear class-level type params after generating specialized methods
        self.analyzer.clear_class_type_params();

        Ok(())
    }

    pub(super) fn build_type_param_map(
        &self,
        class_name: &str,
        type_args: &[Type],
    ) -> Result<HashMap<String, Type>, String> {
        let mut type_params = HashMap::new();

        // get the class symbol to find generic parameter names
        if let Some(class_symbol) = self.analyzer.symbol_table().lookup(class_name) {
            if class_symbol.type_params.len() == type_args.len() {
                for (i, param) in class_symbol.type_params.iter().enumerate() {
                    // param is (String, Vec<String>) - first element is the parameter name
                    type_params.insert(param.0.clone(), type_args[i].clone());
                }
            } else {
                return Err(format!(
                    "Type argument count mismatch for class {}",
                    class_name
                ));
            }
        } else {
            return Err(format!("Class {} not found", class_name));
        }

        Ok(type_params)
    }

    pub(super) fn generate_generic_function_call(
        &mut self,
        func_name: &str,
        args: &[ExpressionNode],
    ) -> Result<inkwell::values::BasicValueEnum<'a>, String> {
        // get the generic function AST node
        let func_node = self
            .function_nodes
            .get(func_name)
            .ok_or(format!("Generic function {} not found", func_name))?;

        // infer concrete types by matching function signature against arguments
        let mut type_map = std::collections::HashMap::new();

        // for each parameter in the function signature, match against the corresponding argument
        for (param_idx, param) in func_node.params.iter().enumerate() {
            if param_idx >= args.len() {
                break;
            }

            let arg_type = self
                .analyzer
                .get_expression_type(&args[param_idx])
                .map_err(|e| format!("Failed to get argument type: {}", e))?;

            // recursively match the parameter type against the argument type to infer generic parameters
            self.infer_types_from_signature(&param.type_, &arg_type, &mut type_map)?;
        }

        // convert to concrete types list in the order of type parameters
        let mut concrete_types = Vec::new();
        for (type_param_name, _) in &func_node.type_params {
            if let Some(concrete_type) = type_map.get(type_param_name) {
                concrete_types.push(concrete_type.clone());
            } else {
                return Err(format!(
                    "Could not infer concrete type for generic parameter {}",
                    type_param_name
                ));
            }
        }

        // create instantiation key
        let type_names: Vec<String> = concrete_types
            .iter()
            .map(|t| self.type_to_string(t))
            .collect();
        let instance_name = format!("{}_{}", func_name, type_names.join("_"));

        // check if already instantiated
        if self.module.get_function(&instance_name).is_none() {
            // instantiate the generic function
            self.instantiate_generic_function(func_name, &concrete_types, &instance_name)?;
        }

        // call the instantiated function
        let func = self
            .module
            .get_function(&instance_name)
            .ok_or(format!("Instantiated function {} not found", instance_name))?;

        let mut call_args = vec![];
        for arg in args {
            call_args.push(self.generate_expression(arg)?.into());
        }

        let call = self
            .builder
            .build_call(func, &call_args, "generic_func_call")
            .map_err(|e| e.to_string())?;

        match call.try_as_basic_value().left() {
            Some(val) => Ok(val),
            None => Ok(self.context.i32_type().const_int(0, false).into()),
        }
    }

    fn instantiate_generic_function(
        &mut self,
        func_name: &str,
        concrete_types: &[Type],
        instance_name: &str,
    ) -> Result<(), String> {
        let func_node = self
            .function_nodes
            .get(func_name)
            .ok_or(format!("Generic function {} not found", func_name))?;

        // create type substitution map
        let mut type_map = std::collections::HashMap::new();
        for (i, type_param) in func_node.type_params.iter().enumerate() {
            if i < concrete_types.len() {
                type_map.insert(type_param.0.clone(), concrete_types[i].clone());
            }
        }

        // clone and substitute the function
        let mut substituted_func = func_node.clone();
        substituted_func.name = instance_name.to_string();
        substituted_func.type_params.clear(); // no longer generic

        // substitute types in parameters and return type
        for param in &mut substituted_func.params {
            param.type_ = self.substitute_types_in_type_node(&param.type_, &type_map);
        }
        substituted_func.return_type =
            self.substitute_types_in_type_node(&substituted_func.return_type, &type_map);

        // substitute types in the function body
        let mut substituted_body = Vec::new();
        for stmt in &substituted_func.body {
            substituted_body.push(self.substitute_types_in_statement(stmt, &type_map));
        }
        substituted_func.body = substituted_body;

        // save current context (from calling context)
        let saved_variables = self.variables.clone();
        let saved_current_function_name = self.current_function_name.take();
        let saved_current_function_return_type = self.current_function_return_type.take();
        let saved_builder_position = self.builder.get_insert_block();

        // set up generic context for the instantiated function
        let context = GenericContext {
            type_params: type_map.clone(),
        };
        self.generic_context = Some(context);

        // declare the instantiated function
        self.declare_function(&substituted_func)?;

        // generate the instantiated function
        self.generate_function(&substituted_func)?;

        // clear generic context
        self.generic_context = None;

        // restore context (back to calling context)
        self.variables = saved_variables;
        self.current_function_name = saved_current_function_name;
        self.current_function_return_type = saved_current_function_return_type;
        if let Some(block) = saved_builder_position {
            self.builder.position_at_end(block);
        }

        Ok(())
    }

    pub(super) fn substitute_types_in_type_node(
        &self,
        type_node: &TypeNode,
        type_map: &std::collections::HashMap<String, Type>,
    ) -> TypeNode {
        match &type_node.kind {
            TypeKind::Named(name, args) => {
                if type_map.contains_key(name) {
                    // this is a generic type parameter - substitute it
                    let concrete_type = &type_map[name];
                    self.type_to_type_node(concrete_type)
                } else {
                    // not a generic parameter, substitute arguments recursively
                    let substituted_args = args
                        .iter()
                        .map(|arg| self.substitute_types_in_type_node(arg, type_map))
                        .collect();
                    TypeNode {
                        kind: TypeKind::Named(name.clone(), substituted_args),
                        span: type_node.span,
                    }
                }
            }
            TypeKind::List(inner) => TypeNode {
                kind: TypeKind::List(Box::new(
                    self.substitute_types_in_type_node(inner, type_map),
                )),
                span: Span::new(0, 0),
            },
            TypeKind::Function { params, returns } => {
                let substituted_params = params
                    .iter()
                    .map(|p| self.substitute_types_in_type_node(p, type_map))
                    .collect();
                let substituted_returns = self.substitute_types_in_type_node(returns, type_map);
                TypeNode {
                    kind: TypeKind::Function {
                        params: substituted_params,
                        returns: Box::new(substituted_returns),
                    },
                    span: Span::new(0, 0),
                }
            }
            // for other types, return as-is (they don't contain generic parameters)
            _ => type_node.clone(),
        }
    }

    pub(super) fn substitute_types_in_statement(
        &self,
        stmt: &StatementNode,
        type_map: &std::collections::HashMap<String, Type>,
    ) -> StatementNode {
        match &stmt.kind {
            StatementKind::TypedDecl(name, type_node, expr) => {
                let substituted_type = self.substitute_types_in_type_node(type_node, type_map);
                let substituted_expr = self.substitute_types_in_expression(expr, type_map);
                StatementNode {
                    kind: StatementKind::TypedDecl(
                        name.clone(),
                        substituted_type,
                        substituted_expr,
                    ),
                    span: stmt.span,
                }
            }
            StatementKind::AutoDecl(name, type_node, expr) => {
                let substituted_type = self.substitute_types_in_type_node(type_node, type_map);
                let substituted_expr = self.substitute_types_in_expression(expr, type_map);
                StatementNode {
                    kind: StatementKind::AutoDecl(name.clone(), substituted_type, substituted_expr),
                    span: stmt.span,
                }
            }
            StatementKind::For {
                var,
                var_type,
                iter,
                body,
            } => {
                let substituted_var_type = self.substitute_types_in_type_node(var_type, type_map);
                let substituted_iter = self.substitute_types_in_expression(iter, type_map);
                let substituted_body = body
                    .iter()
                    .map(|s| self.substitute_types_in_statement(s, type_map))
                    .collect();
                StatementNode {
                    kind: StatementKind::For {
                        var: var.clone(),
                        var_type: substituted_var_type,
                        iter: substituted_iter,
                        body: substituted_body,
                    },
                    span: stmt.span,
                }
            }
            StatementKind::Return(expr) => {
                let substituted_expr = expr
                    .as_ref()
                    .map(|e| self.substitute_types_in_expression(e, type_map));
                StatementNode {
                    kind: StatementKind::Return(substituted_expr),
                    span: stmt.span,
                }
            }
            StatementKind::Expression(expr) => {
                let substituted_expr = self.substitute_types_in_expression(expr, type_map);
                StatementNode {
                    kind: StatementKind::Expression(substituted_expr),
                    span: stmt.span,
                }
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let substituted_cond = self.substitute_types_in_expression(cond, type_map);
                let substituted_then = then_block
                    .iter()
                    .map(|s| self.substitute_types_in_statement(s, type_map))
                    .collect();
                let substituted_else = else_block.as_ref().map(|b| {
                    b.iter()
                        .map(|s| self.substitute_types_in_statement(s, type_map))
                        .collect()
                });
                StatementNode {
                    kind: StatementKind::If {
                        cond: substituted_cond,
                        then_block: substituted_then,
                        else_block: substituted_else,
                    },
                    span: stmt.span,
                }
            }
            // for other statement types, return unchanged for now
            _ => stmt.clone(),
        }
    }

    pub(super) fn substitute_types_in_expression(
        &self,
        expr: &ExpressionNode,
        type_map: &std::collections::HashMap<String, Type>,
    ) -> ExpressionNode {
        match &expr.kind {
            ExpressionKind::Call { func, args } => {
                let substituted_func = self.substitute_types_in_expression(func, type_map);
                let substituted_args = args
                    .iter()
                    .map(|a| self.substitute_types_in_expression(a, type_map))
                    .collect();
                ExpressionNode {
                    kind: ExpressionKind::Call {
                        func: Box::new(substituted_func),
                        args: substituted_args,
                    },
                    span: expr.span,
                }
            }
            ExpressionKind::FieldAccess {
                expr: inner_expr,
                field,
            } => {
                let substituted_expr = self.substitute_types_in_expression(inner_expr, type_map);
                ExpressionNode {
                    kind: ExpressionKind::FieldAccess {
                        expr: Box::new(substituted_expr),
                        field: field.clone(),
                    },
                    span: expr.span,
                }
            }
            ExpressionKind::Lambda {
                params,
                return_type,
                body,
            } => {
                // for lambda parameters, substitute their types
                let substituted_params = params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        type_: self.substitute_types_in_type_node(&p.type_, type_map),
                        default_value: p
                            .default_value
                            .as_ref()
                            .map(|dv| self.substitute_types_in_expression(dv, type_map)),
                    })
                    .collect();
                // substitute return type
                let substituted_return_type =
                    self.substitute_types_in_type_node(return_type, type_map);
                // for lambda body, substitute statements
                let substituted_body = body
                    .iter()
                    .map(|s| self.substitute_types_in_statement(s, type_map))
                    .collect();
                ExpressionNode {
                    kind: ExpressionKind::Lambda {
                        params: substituted_params,
                        return_type: substituted_return_type,
                        body: substituted_body,
                    },
                    span: expr.span,
                }
            }
            // for other expression types, return unchanged for now
            _ => expr.clone(),
        }
    }

    /// recursively match a parameter type pattern against a concrete argument type to infer generic parameters
    pub(super) fn infer_types_from_signature(
        &self,
        param_type: &TypeNode,
        arg_type: &Type,
        type_map: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), String> {
        match &param_type.kind {
            TypeKind::Named(name, type_args) => {
                if type_args.is_empty() {
                    // this is a potential generic parameter or concrete type
                    if name.chars().next().unwrap_or(' ').is_uppercase() || name.len() <= 3 {
                        // likely a generic parameter - infer it from the argument type
                        if let Some(existing) = type_map.get(name) {
                            if existing != arg_type {
                                return Err(format!(
                                    "Type mismatch for generic parameter {}: expected {:?}, got {:?}",
                                    name, existing, arg_type
                                ));
                            }
                        } else {
                            type_map.insert(name.clone(), arg_type.clone());
                        }
                    } else {
                        // concrete type - should match exactly
                        let expected_concrete = self.type_node_to_type(param_type);
                        if expected_concrete != *arg_type {
                            return Err(format!(
                                "Type mismatch: expected {:?}, got {:?}",
                                expected_concrete, arg_type
                            ));
                        }
                    }
                } else {
                    // generic type with arguments (like list<T>)
                    match arg_type {
                        Type::Named(arg_name, arg_type_args) => {
                            if name != arg_name {
                                return Err(format!(
                                    "Type name mismatch: expected {}, got {}",
                                    name, arg_name
                                ));
                            }
                            if type_args.len() != arg_type_args.len() {
                                return Err(format!(
                                    "Type argument count mismatch for {}: expected {}, got {}",
                                    name,
                                    type_args.len(),
                                    arg_type_args.len()
                                ));
                            }
                            // recursively match type arguments
                            for (param_arg, arg_arg) in type_args.iter().zip(arg_type_args.iter()) {
                                self.infer_types_from_signature(param_arg, arg_arg, type_map)?;
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Expected named type with args, got {:?}",
                                arg_type
                            ));
                        }
                    }
                }
            }
            TypeKind::List(inner_param_type) => match arg_type {
                Type::List(inner_arg_type) => {
                    self.infer_types_from_signature(inner_param_type, inner_arg_type, type_map)?;
                }
                _ => return Err(format!("Expected list type, got {:?}", arg_type)),
            },
            TypeKind::Function {
                params: param_params,
                returns: param_returns,
            } => {
                match arg_type {
                    Type::Function {
                        params: arg_params,
                        returns: arg_returns,
                        ..
                    } => {
                        if param_params.len() != arg_params.len() {
                            return Err(format!(
                                "Function parameter count mismatch: expected {}, got {}",
                                param_params.len(),
                                arg_params.len()
                            ));
                        }
                        // match parameter types
                        for (param_param, arg_param) in param_params.iter().zip(arg_params.iter()) {
                            self.infer_types_from_signature(param_param, arg_param, type_map)?;
                        }
                        // match return type
                        self.infer_types_from_signature(param_returns, arg_returns, type_map)?;
                    }
                    _ => return Err(format!("Expected function type, got {:?}", arg_type)),
                }
            }
            TypeKind::Primitive(primitive) => {
                let expected = match primitive {
                    PrimitiveType::Int => Type::Primitive(PrimitiveType::Int),
                    PrimitiveType::Float => Type::Primitive(PrimitiveType::Float),
                    PrimitiveType::Bool => Type::Primitive(PrimitiveType::Bool),
                    PrimitiveType::Str => Type::Primitive(PrimitiveType::Str),
                    PrimitiveType::Char => Type::Primitive(PrimitiveType::Char),
                    _ => return Err(format!("Unsupported primitive type {:?}", primitive)),
                };
                if expected != *arg_type {
                    return Err(format!(
                        "Primitive type mismatch: expected {:?}, got {:?}",
                        expected, arg_type
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Unsupported type kind in signature matching: {:?}",
                    param_type.kind
                ));
            }
        }
        Ok(())
    }
}
