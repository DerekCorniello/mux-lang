// Module declarations
pub mod error;
pub mod format;
pub mod symbol_table;
pub mod types;
pub mod unifier;

// Re-exports for public API
pub use error::SemanticError;
pub use format::{format_binary_op, format_type};
#[allow(unused_imports)]
pub use format::{format_span_location, format_unary_op};
pub use symbol_table::{SymbolTable, BUILT_IN_FUNCTIONS};
pub use types::{BuiltInSig, GenericContext, MethodSig, Symbol, SymbolKind, Type};
pub use unifier::Unifier;

// Internal imports
use crate::ast::*;
use crate::diagnostic::Files;
use crate::lexer::Span;
use std::cell::RefCell;
use std::rc::Rc;

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    current_bounds: std::collections::HashMap<String, Vec<String>>,
    errors: Vec<SemanticError>,
    is_in_static_method: bool,
    pub current_self_type: Option<Type>,
    pub module_resolver: Option<Rc<RefCell<crate::module_resolver::ModuleResolver>>>,
    pub imported_symbols:
        std::collections::HashMap<String, std::collections::HashMap<String, Symbol>>,
    pub all_module_asts: std::collections::HashMap<String, Vec<AstNode>>,
    pub module_dependencies: Vec<String>,
    current_file: Option<std::path::PathBuf>, // Track current file for relative imports
    pub lambda_captures: std::collections::HashMap<Span, Vec<(String, Type)>>, // Track captured variables for each lambda
    pub current_return_type: Option<Type>, // Track current function/lambda return type
    pub current_class_type_params: Option<Vec<(String, Vec<String>)>>, // Track class-level type params with bounds for method analysis
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticAnalyzer {
    // Helper function to sanitize module paths for use in identifiers
    fn sanitize_module_path(module_path: &str) -> String {
        module_path.replace(['.', '/'], "_")
    }

    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();
        Self {
            symbol_table,
            current_bounds: std::collections::HashMap::new(),
            errors: Vec::new(),
            is_in_static_method: false,
            current_self_type: None,
            module_resolver: None,
            imported_symbols: std::collections::HashMap::new(),
            all_module_asts: std::collections::HashMap::new(),
            module_dependencies: Vec::new(),
            current_file: None,
            lambda_captures: std::collections::HashMap::new(),
            current_return_type: None,
            current_class_type_params: None,
        }
    }

    pub fn new_with_resolver(
        resolver: Rc<RefCell<crate::module_resolver::ModuleResolver>>,
    ) -> Self {
        let symbol_table = SymbolTable::new();
        Self {
            symbol_table,
            current_bounds: std::collections::HashMap::new(),
            errors: Vec::new(),
            is_in_static_method: false,
            current_self_type: None,
            module_resolver: Some(resolver),
            imported_symbols: std::collections::HashMap::new(),
            all_module_asts: std::collections::HashMap::new(),
            module_dependencies: Vec::new(),
            current_file: None,
            lambda_captures: std::collections::HashMap::new(),
            current_return_type: None,
            current_class_type_params: None,
        }
    }

    pub fn set_current_file(&mut self, file: std::path::PathBuf) {
        self.current_file = Some(file);
    }

    fn new_for_module(resolver: Rc<RefCell<crate::module_resolver::ModuleResolver>>) -> Self {
        let symbol_table = SymbolTable::new();
        Self {
            symbol_table,
            current_bounds: std::collections::HashMap::new(),
            errors: Vec::new(),
            is_in_static_method: false,
            current_self_type: None,
            module_resolver: Some(resolver),
            imported_symbols: std::collections::HashMap::new(),
            all_module_asts: std::collections::HashMap::new(),
            module_dependencies: Vec::new(),
            current_file: None,
            lambda_captures: std::collections::HashMap::new(),
            current_return_type: None,
            current_class_type_params: None,
        }
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn all_symbols(&self) -> &std::collections::HashMap<String, Symbol> {
        &self.symbol_table.all_symbols
    }

    pub fn imported_symbols(
        &self,
    ) -> &std::collections::HashMap<String, std::collections::HashMap<String, Symbol>> {
        &self.imported_symbols
    }

    pub fn all_module_asts(&self) -> &std::collections::HashMap<String, Vec<AstNode>> {
        &self.all_module_asts
    }

    /// Set class-level type parameters and their bounds for method analysis.
    /// This should be called before analyzing/generating methods of a generic class.
    pub fn set_class_type_params(&mut self, params: Vec<(String, Vec<String>)>) {
        self.current_class_type_params = Some(params.clone());
        // Also add to current_bounds for immediate use in type checking
        for (param, bounds) in params {
            self.current_bounds.insert(param, bounds);
        }
    }

    /// Clear class-level type parameters after finishing with a class.
    pub fn clear_class_type_params(&mut self) {
        if let Some(params) = &self.current_class_type_params {
            for (param, _) in params {
                self.current_bounds.remove(param);
            }
        }
        self.current_class_type_params = None;
    }

    fn get_builtin_sig(&self, name: &str) -> Option<&BuiltInSig> {
        BUILT_IN_FUNCTIONS.get(name)
    }

    pub fn analyze(&mut self, ast: &[AstNode], files: Option<&mut Files>) -> Vec<SemanticError> {
        self.add_builtin_functions();
        if let Err(e) = self.collect_hoistable_declarations(ast) {
            self.errors.push(e);
        }
        self.analyze_nodes(ast, files);
        std::mem::take(&mut self.errors)
    }

    fn add_builtin_functions(&mut self) {
        let builtins = vec![
            (
                "print",
                vec![Type::Primitive(PrimitiveType::Str)],
                Type::Void,
            ),
            ("read_line", vec![], Type::Primitive(PrimitiveType::Str)),
            (
                "range",
                vec![
                    Type::Primitive(PrimitiveType::Int),
                    Type::Primitive(PrimitiveType::Int),
                ],
                Type::List(Box::new(Type::Primitive(PrimitiveType::Int))),
            ),
            (
                "Some",
                vec![Type::Variable("T".to_string())],
                Type::Optional(Box::new(Type::Variable("T".to_string()))),
            ),
            ("None", vec![], Type::Optional(Box::new(Type::Void))),
            (
                "Ok",
                vec![Type::Variable("T".to_string())],
                Type::Named(
                    "Result".to_string(),
                    vec![
                        Type::Variable("T".to_string()),
                        Type::Variable("E".to_string()),
                    ],
                ),
            ),
            (
                "Err",
                vec![Type::Variable("E".to_string())],
                Type::Named(
                    "Result".to_string(),
                    vec![
                        Type::Variable("T".to_string()),
                        Type::Variable("E".to_string()),
                    ],
                ),
            ),
        ];
        for (name, params, ret) in builtins {
            let func_type = Type::Function {
                params,
                returns: Box::new(ret),
                default_count: 0,
            };
            self.symbol_table
                .add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Function,
                        span: Span::new(0, 0), // built-in function, no source span
                        type_: Some(func_type),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: 0,
                        variants: None,
                    },
                )
                .expect("builtin function registration should not fail");
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn resolve_type(&self, type_node: &TypeNode) -> Result<Type, SemanticError> {
        match &type_node.kind {
            TypeKind::Primitive(prim) => match prim {
                crate::ast::PrimitiveType::Int => {
                    Ok(Type::Primitive(crate::ast::PrimitiveType::Int))
                }
                crate::ast::PrimitiveType::Float => {
                    Ok(Type::Primitive(crate::ast::PrimitiveType::Float))
                }
                crate::ast::PrimitiveType::Bool => {
                    Ok(Type::Primitive(crate::ast::PrimitiveType::Bool))
                }
                crate::ast::PrimitiveType::Char => {
                    Ok(Type::Primitive(crate::ast::PrimitiveType::Char))
                }
                crate::ast::PrimitiveType::Str => {
                    Ok(Type::Primitive(crate::ast::PrimitiveType::Str))
                }
                crate::ast::PrimitiveType::Void => Ok(Type::Void),
                crate::ast::PrimitiveType::Auto => Err(SemanticError {
                    message: "The 'auto' type is not allowed in this context".into(),
                    span: type_node.span,
                }),
            },
            TypeKind::Named(name, type_args) => {
                // Handle type parameters (generic type variables)
                if type_args.is_empty() {
                    if let Some(symbol) = self.symbol_table.lookup(name) {
                        if matches!(symbol.kind, SymbolKind::Type) {
                            return Ok(Type::Variable(name.clone()));
                        }
                    }
                }

                // Handle built-in generic types
                if name == "Optional" && type_args.len() == 1 {
                    let resolved_arg = self.resolve_type(&type_args[0])?;
                    return Ok(Type::Optional(Box::new(resolved_arg)));
                } else if name == "Result" && type_args.len() == 2 {
                    let resolved_ok = self.resolve_type(&type_args[0])?;
                    let resolved_err = self.resolve_type(&type_args[1])?;
                    return Ok(Type::Named(
                        "Result".to_string(),
                        vec![resolved_ok, resolved_err],
                    ));
                }

                // named types are assumed to be classes, enums, or interfaces
                let resolved_args = type_args
                    .iter()
                    .map(|arg| self.resolve_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Named(name.clone(), resolved_args))
            }
            TypeKind::Function { params, returns } => {
                let resolved_params = params
                    .iter()
                    .map(|p| self.resolve_type(p))
                    .collect::<Result<Vec<_>, _>>()?;
                let resolved_return = self.resolve_type(returns)?;
                Ok(Type::Function {
                    params: resolved_params,
                    returns: Box::new(resolved_return),
                    default_count: 0,
                })
            }
            TypeKind::Reference(inner) => {
                let resolved_inner = self.resolve_type(inner)?;
                Ok(Type::Reference(Box::new(resolved_inner)))
            }
            TypeKind::List(inner) => {
                let resolved_inner = self.resolve_type(inner)?;
                Ok(Type::List(Box::new(resolved_inner)))
            }
            TypeKind::Map(key, value) => {
                let resolved_key = self.resolve_type(key)?;
                let resolved_value = self.resolve_type(value)?;
                Ok(Type::Map(Box::new(resolved_key), Box::new(resolved_value)))
            }
            TypeKind::Set(inner) => {
                let resolved_inner = self.resolve_type(inner)?;
                Ok(Type::Set(Box::new(resolved_inner)))
            }

            TypeKind::TraitObject(_) => Err(SemanticError {
                message: "Trait objects not yet supported".into(),
                span: type_node.span,
            }),
            TypeKind::Auto => Err(SemanticError {
                message: "The 'auto' type is not allowed in this context".into(),
                span: type_node.span,
            }),
        }
    }

    pub fn get_expression_type(&mut self, expr: &ExpressionNode) -> Result<Type, SemanticError> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => match lit {
                LiteralNode::Integer(_) => Ok(Type::Primitive(crate::ast::PrimitiveType::Int)),
                LiteralNode::Float(_) => Ok(Type::Primitive(crate::ast::PrimitiveType::Float)),
                LiteralNode::String(_) => Ok(Type::Primitive(crate::ast::PrimitiveType::Str)),
                LiteralNode::Boolean(_) => Ok(Type::Primitive(crate::ast::PrimitiveType::Bool)),
                LiteralNode::Char(_) => Ok(Type::Primitive(crate::ast::PrimitiveType::Char)),
            },
            ExpressionKind::None => Ok(Type::Optional(Box::new(Type::Never))),
            ExpressionKind::Identifier(name) => {
                if name == "self" {
                    // For 'self', use the current self type if available
                    if let Some(self_type) = &self.current_self_type {
                        Ok(self_type.clone())
                    } else {
                        // placeholder type to allow analysis to continue when self type is unavailable
                        Ok(Type::Named("Unknown".to_string(), vec![]))
                    }
                } else {
                    // For other identifiers, check current scope first, then global symbols
                    let symbol = self
                        .symbol_table
                        .get_cloned(name)
                        .or_else(|| self.symbol_table.lookup(name));

                    if let Some(symbol) = symbol {
                        let type_ = symbol.type_.clone().ok_or_else(|| SemanticError {
                            message: format!("Symbol '{}' has no type information", name),
                            span: expr.span,
                        })?;
                        // For type parameters, return Type::Variable instead of Type::Generic
                        let type_ = match &type_ {
                            Type::Generic(n) if n == name => Type::Variable(name.clone()),
                            _ => type_,
                        };
                        Ok(type_)
                    } else if let Some(sig) = self.get_builtin_sig(name) {
                        Ok(Type::Function {
                            params: sig.params.clone(),
                            returns: Box::new(sig.return_type.clone()),
                            default_count: 0,
                        })
                    } else {
                        Err(SemanticError {
                            message: format!("Undefined variable '{}'", name),
                            span: expr.span,
                        })
                    }
                }
            }
            ExpressionKind::Binary {
                left,
                right,
                op,
                op_span,
                ..
            } => {
                let left_type = self.get_expression_type(left)?;
                let right_type = self.get_expression_type(right)?;

                if *op == crate::ast::BinaryOp::Assign {
                    if let crate::ast::ExpressionKind::Identifier(name) = &left.kind {
                        let symbol =
                            self.symbol_table
                                .lookup(name)
                                .ok_or_else(|| SemanticError {
                                    message: format!("Undefined variable '{}'", name),
                                    span: left.span,
                                })?;

                        // Check if trying to assign to a constant
                        if symbol.kind == SymbolKind::Constant {
                            return Err(SemanticError::with_help(
                                format!("Cannot assign to constant '{}'", name),
                                expr.span,
                                "Constants cannot be modified after initialization",
                            ));
                        }

                        let var_type = symbol.type_.as_ref().ok_or_else(|| SemanticError {
                            message: format!("Variable '{}' has no type information", name),
                            span: left.span,
                        })?;
                        if let Type::Reference(inner) = var_type {
                            self.check_type_compatibility(inner, &right_type, expr.span)?;
                        } else {
                            self.check_type_compatibility(var_type, &right_type, expr.span)?;
                        }
                    } else if let crate::ast::ExpressionKind::FieldAccess {
                        expr: obj_expr,
                        field,
                    } = &left.kind
                    {
                        // Check if trying to assign to a const field
                        let obj_type = self.get_expression_type(obj_expr)?;

                        if let Type::Named(class_name, _) = &obj_type {
                            if let Some(symbol) = self.symbol_table.lookup(class_name) {
                                if let Some((field_type, is_const)) = symbol.fields.get(field) {
                                    // Check if trying to assign to a const field
                                    if *is_const {
                                        return Err(SemanticError {
                                            message: format!(
                                                "Cannot assign to const field '{}'",
                                                field
                                            ),
                                            span: expr.span,
                                        });
                                    }

                                    self.check_type_compatibility(
                                        field_type,
                                        &right_type,
                                        expr.span,
                                    )?;
                                } else {
                                    return Err(SemanticError {
                                        message: format!(
                                            "Field '{}' not found on type '{}'",
                                            field, class_name
                                        ),
                                        span: left.span,
                                    });
                                }
                            }
                        }
                    } else if let crate::ast::ExpressionKind::Unary {
                        op: crate::ast::UnaryOp::Deref,
                        op_span: _,
                        expr: _,
                        postfix: _,
                    } = &left.kind
                    {
                    } else if let crate::ast::ExpressionKind::ListAccess {
                        expr: target_expr,
                        index: _,
                    } = &left.kind
                    {
                        // Index assignment: list[index] = value or map[key] = value
                        let target_type = self.get_expression_type(target_expr)?;
                        match target_type {
                            Type::List(ref elem_type) => {
                                self.check_type_compatibility(elem_type, &right_type, expr.span)?;
                            }
                            Type::Map(_, ref value_type) => {
                                self.check_type_compatibility(value_type, &right_type, expr.span)?;
                            }
                            _ => {
                                return Err(SemanticError {
                                    message: "Cannot assign to index on non-list/map type".into(),
                                    span: expr.span,
                                });
                            }
                        }
                    } else {
                        return Err(SemanticError {
                            message: "Assignment to non-identifier is not supported".into(),
                            span: expr.span,
                        });
                    }
                    Ok(right_type) // assignment returns the assigned value
                } else if matches!(
                    op,
                    crate::ast::BinaryOp::AddAssign
                        | crate::ast::BinaryOp::SubtractAssign
                        | crate::ast::BinaryOp::MultiplyAssign
                        | crate::ast::BinaryOp::DivideAssign
                        | crate::ast::BinaryOp::ModuloAssign
                ) {
                    if let crate::ast::ExpressionKind::Identifier(name) = &left.kind {
                        let symbol =
                            self.symbol_table
                                .lookup(name)
                                .ok_or_else(|| SemanticError {
                                    message: format!("Undefined variable '{}'", name),
                                    span: left.span,
                                })?;

                        // Check if trying to modify a constant
                        if symbol.kind == SymbolKind::Constant {
                            return Err(SemanticError::with_help(
                                format!("Cannot modify constant '{}'", name),
                                expr.span,
                                "Constants cannot be modified after initialization",
                            ));
                        }
                    } else if let crate::ast::ExpressionKind::FieldAccess {
                        expr: obj_expr,
                        field,
                    } = &left.kind
                    {
                        let obj_type = self.get_expression_type(obj_expr)?;

                        if let Type::Named(class_name, _) = &obj_type {
                            if let Some(symbol) = self.symbol_table.lookup(class_name) {
                                if let Some((_field_type, is_const)) = symbol.fields.get(field) {
                                    // Check if trying to modify a const field
                                    if *is_const {
                                        return Err(SemanticError {
                                            message: format!(
                                                "Cannot modify const field '{}'",
                                                field
                                            ),
                                            span: expr.span,
                                        });
                                    }

                                    let base_op = match op {
                                        crate::ast::BinaryOp::AddAssign => {
                                            crate::ast::BinaryOp::Add
                                        }
                                        crate::ast::BinaryOp::SubtractAssign => {
                                            crate::ast::BinaryOp::Subtract
                                        }
                                        crate::ast::BinaryOp::MultiplyAssign => {
                                            crate::ast::BinaryOp::Multiply
                                        }
                                        crate::ast::BinaryOp::DivideAssign => {
                                            crate::ast::BinaryOp::Divide
                                        }
                                        crate::ast::BinaryOp::ModuloAssign => {
                                            crate::ast::BinaryOp::Modulo
                                        }
                                        _ => unreachable!(),
                                    };

                                    self.resolve_binary_operator(&left_type, &right_type, &base_op)
                                        .ok_or_else(|| SemanticError {
                                            message: format!(
                                                "Binary operator '{}' is not supported for types {} and {}",
                                                format_binary_op(&base_op),
                                                format_type(&left_type),
                                                format_type(&right_type)
                                            ),
                                            span: *op_span,
                                        })?;
                                } else {
                                    return Err(SemanticError {
                                        message: format!(
                                            "Field '{}' not found on type '{}'",
                                            field, class_name
                                        ),
                                        span: left.span,
                                    });
                                }
                            }
                        }
                    } else if let crate::ast::ExpressionKind::Unary {
                        op: crate::ast::UnaryOp::Deref,
                        op_span: _,
                        expr: _,
                        postfix: _,
                    } = &left.kind
                    {
                    }

                    let base_op = match op {
                        crate::ast::BinaryOp::AddAssign => crate::ast::BinaryOp::Add,
                        crate::ast::BinaryOp::SubtractAssign => crate::ast::BinaryOp::Subtract,
                        crate::ast::BinaryOp::MultiplyAssign => crate::ast::BinaryOp::Multiply,
                        crate::ast::BinaryOp::DivideAssign => crate::ast::BinaryOp::Divide,
                        crate::ast::BinaryOp::ModuloAssign => crate::ast::BinaryOp::Modulo,
                        _ => unreachable!(),
                    };

                    if let Some(result_type) =
                        self.resolve_binary_operator(&left_type, &right_type, &base_op)
                    {
                        Ok(result_type)
                    } else {
                        Err(SemanticError {
                            message: format!(
                                "Binary operator '{}' is not supported for types {} and {}",
                                format_binary_op(&base_op),
                                format_type(&left_type),
                                format_type(&right_type)
                            ),
                            span: *op_span,
                        })
                    }
                } else if let Some(result_type) =
                    self.resolve_binary_operator(&left_type, &right_type, op)
                {
                    Ok(result_type)
                } else {
                    Err(SemanticError {
                        message: format!(
                            "Binary operator '{}' is not supported for types {} and {}",
                            format_binary_op(op),
                            format_type(&left_type),
                            format_type(&right_type)
                        ),
                        span: *op_span,
                    })
                }
            }
            ExpressionKind::Unary {
                expr, op, op_span, ..
            } => match op {
                UnaryOp::Not => Ok(Type::Primitive(crate::ast::PrimitiveType::Bool)),
                UnaryOp::Neg => {
                    let operand_type = self.get_expression_type(expr)?;
                    match operand_type {
                        Type::Primitive(crate::ast::PrimitiveType::Int)
                        | Type::Primitive(crate::ast::PrimitiveType::Float) => Ok(operand_type),
                        _ => Err(SemanticError {
                            message: "Negation operator requires a numeric operand".into(),
                            span: *op_span,
                        }),
                    }
                }
                UnaryOp::Ref => {
                    let operand_type = self.get_expression_type(expr)?;
                    Ok(Type::Reference(Box::new(operand_type)))
                }
                UnaryOp::Deref => {
                    let operand_type = self.get_expression_type(expr)?;
                    if let Type::Reference(inner) = operand_type {
                        Ok(*inner)
                    } else {
                        Err(SemanticError {
                            message: "Cannot dereference a non-reference type".into(),
                            span: *op_span,
                        })
                    }
                }
                UnaryOp::Incr | UnaryOp::Decr => {
                    // Check if trying to modify a constant
                    if let crate::ast::ExpressionKind::Identifier(name) = &expr.kind {
                        if let Some(symbol) = self.symbol_table.lookup(name) {
                            if symbol.kind == SymbolKind::Constant {
                                return Err(SemanticError::with_help(
                                    format!("Cannot modify constant '{}'", name),
                                    *op_span,
                                    "Constants cannot be modified after initialization",
                                ));
                            }
                        }
                    } else if let crate::ast::ExpressionKind::FieldAccess {
                        expr: obj_expr,
                        field,
                    } = &expr.kind
                    {
                        // Check if field is const
                        let obj_type = self.get_expression_type(obj_expr)?;
                        if let Type::Named(class_name, _) = &obj_type {
                            if let Some(symbol) = self.symbol_table.lookup(class_name) {
                                if let Some((_field_type, is_const)) = symbol.fields.get(field) {
                                    if *is_const {
                                        return Err(SemanticError {
                                            message: format!(
                                                "Cannot modify const field '{}'",
                                                field
                                            ),
                                            span: *op_span,
                                        });
                                    }
                                }
                            }
                        }
                    } else if let crate::ast::ExpressionKind::Unary {
                        op: crate::ast::UnaryOp::Deref,
                        op_span: _,
                        expr: _,
                        postfix: _,
                    } = &expr.kind
                    {
                        // Dereference increment/decrement is allowed (e.g., (*p)++)
                        // No const check needed
                    }

                    let operand_type = self.get_expression_type(expr)?;
                    match operand_type {
                        Type::Primitive(crate::ast::PrimitiveType::Int) => Ok(operand_type),
                        _ => Err(SemanticError {
                            message: "Increment/decrement operators require an int operand".into(),
                            span: *op_span,
                        }),
                    }
                }
            },
            ExpressionKind::Call { func, args } => {
                // get function type
                let func_type = match &func.kind {
                    ExpressionKind::GenericType(name, type_args) => {
                        self.get_instantiated_constructor_type(name, type_args, func.span)?
                    }
                    ExpressionKind::Identifier(name) => match self.get_expression_type(func) {
                        Ok(t) => t,
                        Err(e) if e.message.contains("Undefined variable") => {
                            return Err(SemanticError {
                                message: format!("Undefined function '{}'", name),
                                span: func.span,
                            });
                        }
                        Err(e) => return Err(e),
                    },
                    _ => self.get_expression_type(func)?,
                };
                match func_type {
                    Type::Function {
                        params,
                        returns,
                        default_count,
                        ..
                    } => {
                        // For named functions, verify the symbol's default_count matches
                        let actual_default_count = match &func.kind {
                            ExpressionKind::Identifier(name) => {
                                // Use the greater of type default_count and symbol default_count
                                // (they should be the same for functions, but type carries lambda defaults)
                                let symbol_default = self
                                    .symbol_table
                                    .lookup(name)
                                    .map(|s| s.default_param_count)
                                    .unwrap_or(0);
                                std::cmp::max(default_count, symbol_default)
                            }
                            _ => default_count, // For lambdas and other expressions, use type's default_count
                        };

                        let min_args = params.len() - actual_default_count;
                        let max_args = params.len();

                        if args.len() < min_args || args.len() > max_args {
                            if actual_default_count > 0 {
                                return Err(SemanticError {
                                    message: format!(
                                        "Function expects {} to {} arguments, got {}",
                                        min_args,
                                        max_args,
                                        args.len()
                                    ),
                                    span: expr.span,
                                });
                            } else {
                                return Err(SemanticError {
                                    message: format!(
                                        "Function expects {} arguments, got {}",
                                        params.len(),
                                        args.len()
                                    ),
                                    span: expr.span,
                                });
                            }
                        }

                        let mut unifier = Unifier::new();
                        // Only validate provided arguments, missing ones will use defaults
                        for (param, arg) in params.iter().zip(args.iter()) {
                            let arg_type = self.get_expression_type(arg)?;
                            unifier.unify(param, &arg_type, expr.span)?;
                        }
                        let unified_returns = unifier.apply(&returns);
                        Ok(unified_returns)
                    }
                    _ => Err(SemanticError::with_help(
                        "Cannot call non-function type",
                        expr.span,
                        "Only functions can be called with '()'. Ensure the expression before '()' is a function.",
                    )),
                }
            }
            ExpressionKind::FieldAccess { expr, field } => {
                let expr_type = self.get_expression_type(expr)?;

                // Handle module.symbol access (e.g., shapes.create_shape)
                if let Type::Module(module_name) = &expr_type {
                    // Look up the imported symbols for this module
                    if let Some(module_symbols) = self.imported_symbols.get(module_name) {
                        if let Some(symbol) = module_symbols.get(field) {
                            // Return the type of the symbol from the imported module
                            symbol.type_.clone().ok_or_else(|| SemanticError {
                                message: format!(
                                    "Symbol '{}' in module '{}' has no type information",
                                    field, module_name
                                ),
                                span: expr.span,
                            })
                        } else {
                            Err(SemanticError {
                                message: format!(
                                    "Module '{}' has no exported symbol '{}'",
                                    module_name, field
                                ),
                                span: expr.span,
                            })
                        }
                    } else {
                        Err(SemanticError {
                            message: format!("Module '{}' not found in imports", module_name),
                            span: expr.span,
                        })
                    }
                } else if let Some(method_sig) = self.get_method_sig(&expr_type, field) {
                    Ok(Type::Function {
                        params: method_sig.params,
                        returns: Box::new(method_sig.return_type),
                        default_count: 0,
                    })
                } else if let Type::Named(name, args) = &expr_type {
                    if let Some(symbol) = self.symbol_table.lookup(name) {
                        if let Some((field_type, _is_const)) = symbol.fields.get(field) {
                            let substituted =
                                self.substitute_type_params(field_type, &symbol.type_params, args);
                            Ok(substituted)
                        } else {
                            Err(SemanticError {
                                message: format!(
                                    "Unknown field '{}' on type {}",
                                    field,
                                    format_type(&expr_type)
                                ),
                                span: expr.span,
                            })
                        }
                    } else {
                        Err(SemanticError {
                            message: format!(
                                "Undefined method '{}' on type {}",
                                field,
                                format_type(&expr_type)
                            ),
                            span: expr.span,
                        })
                    }
                } else {
                    Err(SemanticError {
                        message: format!(
                            "Undefined method '{}' on type {}",
                            field,
                            format_type(&expr_type)
                        ),
                        span: expr.span,
                    })
                }
            }
            ExpressionKind::ListAccess { expr, index: _ } => {
                // list/map access returns direct element_type (runtime error if out of bounds/key not found)
                // Use .get() method for safe Optional access
                let target_type = self.get_expression_type(expr)?;
                match target_type {
                    Type::List(elem_type) => Ok(*elem_type),
                    Type::Map(_, value_type) => Ok(*value_type),
                    Type::EmptyMap => Err(SemanticError::with_help(
                        "Cannot index empty map",
                        expr.span,
                        "The map type is unknown. Provide type annotations or add entries to the map literal.",
                    )),
                    _ => Err(SemanticError::with_help(
                        "Cannot index non-list type",
                        expr.span,
                        "Only lists and maps can be indexed with '[]'. Examples: my_list[0], my_map['key']",
                    )),
                }
            }
            ExpressionKind::ListLiteral(elements) => {
                if elements.is_empty() {
                    Ok(Type::EmptyList)
                } else {
                    // Check that all elements have the same type
                    let first_type = self.get_expression_type(&elements[0])?;

                    // Validate ALL elements match the first element's type
                    for (index, element) in elements.iter().enumerate() {
                        let element_type = self.get_expression_type(element)?;
                        if self
                            .check_type_compatibility(&first_type, &element_type, element.span)
                            .is_err()
                        {
                            return Err(SemanticError {
                                message: format!(
                                    "Heterogeneous list: expected all elements to be of type {}, but element at index {} has type {}",
                                    format_type(&first_type),
                                    index,
                                    format_type(&element_type)
                                ),
                                span: element.span,
                            });
                        }
                    }

                    Ok(Type::List(Box::new(first_type)))
                }
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                if entries.is_empty() {
                    Ok(Type::EmptyMap)
                } else {
                    let (key, value) = &entries[0];
                    let key_type = self.get_expression_type(key)?;
                    let value_type = self.get_expression_type(value)?;
                    Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
                }
            }
            ExpressionKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                let then_type = self.get_expression_type(then_expr)?;
                let else_type = self.get_expression_type(else_expr)?;
                if then_type == else_type {
                    Ok(then_type)
                } else {
                    Err(SemanticError::with_help(
                        "If expression branches must have the same type",
                        expr.span,
                        format!(
                            "Then branch has type {}, else branch has type {}",
                            format_type(&then_type),
                            format_type(&else_type)
                        ),
                    ))
                }
            }
            ExpressionKind::Lambda {
                params,
                return_type,
                body,
            } => {
                // Check if this lambda has already been analyzed (captures detected)
                // If so, just return its type without re-analyzing to avoid scope issues
                if self.lambda_captures.contains_key(&expr.span) {
                    let param_types = params
                        .iter()
                        .map(|p| self.resolve_type(&p.type_))
                        .collect::<Result<Vec<_>, _>>()?;
                    let resolved_return_type = self.resolve_type(return_type)?;
                    let default_count = params.iter().filter(|p| p.default_value.is_some()).count();
                    return Ok(Type::Function {
                        params: param_types,
                        returns: Box::new(resolved_return_type),
                        default_count,
                    });
                }

                // Collect parameter names to identify what's local vs captured
                let mut local_vars = std::collections::HashSet::new();
                for param in params {
                    local_vars.insert(param.name.clone());
                }

                self.symbol_table.push_scope()?;
                for param in params {
                    let param_type = self.resolve_type(&param.type_)?;
                    self.symbol_table.add_symbol(
                        &param.name,
                        Symbol {
                            kind: SymbolKind::Variable,
                            span: param.type_.span,
                            type_: Some(param_type),
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    )?;
                }
                self.analyze_block(body, None)?;

                // Detect free variables (captured variables)
                let captures = self.find_free_variables_in_block(body, &local_vars)?;

                // Store captures for this lambda using its span as key
                self.lambda_captures.insert(expr.span, captures);

                let param_types = params
                    .iter()
                    .map(|p| self.resolve_type(&p.type_))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_type_resolved = if body.is_empty() {
                    Type::Void
                } else {
                    match &body
                        .last()
                        .expect("body is not empty (checked above), so last() should return Some")
                        .kind
                    {
                        StatementKind::Expression(expr) => self.get_expression_type(expr)?,
                        StatementKind::Return(Some(expr)) => self.get_expression_type(expr)?,
                        _ => Type::Void,
                    }
                };
                self.symbol_table.pop_scope()?;
                let default_count = params.iter().filter(|p| p.default_value.is_some()).count();
                Ok(Type::Function {
                    params: param_types,
                    returns: Box::new(return_type_resolved),
                    default_count,
                })
            }
            ExpressionKind::SetLiteral(elements) => {
                if elements.is_empty() {
                    Ok(Type::EmptySet)
                } else {
                    let elem_type = self.get_expression_type(&elements[0])?;
                    Ok(Type::Set(Box::new(elem_type)))
                }
            }
            ExpressionKind::GenericType(name, type_args) => {
                if !self.symbol_table.exists(name) {
                    return Err(SemanticError {
                        message: format!("Undefined type '{}'", name),
                        span: expr.span,
                    });
                }

                // Check type argument count
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    let expected_count = symbol.type_params.len();
                    let actual_count = type_args.len();
                    if expected_count != actual_count {
                        return Err(SemanticError {
                            message: format!(
                                "Generic type '{}' requires {} type argument(s), got {}",
                                name, expected_count, actual_count
                            ),
                            span: expr.span,
                        });
                    }
                }

                let resolved_args = type_args
                    .iter()
                    .map(|arg| self.resolve_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Named(name.clone(), resolved_args))
            }
        }
    }

    fn check_type_compatibility(
        &self,
        expected: &Type,
        actual: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        let mut temp_unifier = Unifier::new();
        temp_unifier.unify(expected, actual, span).map_err(|_| {
            SemanticError::new(
                format!(
                    "Type mismatch: expected {}, got {}",
                    format_type(expected),
                    format_type(actual)
                ),
                span,
            )
        })
    }

    #[allow(clippy::only_used_in_recursion)]
    fn substitute_type_param(&self, type_: &Type, param: &str, replacement: &Type) -> Type {
        match type_ {
            Type::Variable(var) if var == param => replacement.clone(),
            Type::Generic(var) if var == param => replacement.clone(),
            Type::Named(name, args) if name == param && args.is_empty() => replacement.clone(),
            Type::Named(name, args) => Type::Named(
                name.clone(),
                args.iter()
                    .map(|a| self.substitute_type_param(a, param, replacement))
                    .collect(),
            ),
            Type::List(inner) => Type::List(Box::new(self.substitute_type_param(
                inner,
                param,
                replacement,
            ))),
            Type::Set(inner) => Type::Set(Box::new(self.substitute_type_param(
                inner,
                param,
                replacement,
            ))),
            Type::Map(key, value) => Type::Map(
                Box::new(self.substitute_type_param(key, param, replacement)),
                Box::new(self.substitute_type_param(value, param, replacement)),
            ),
            Type::Optional(inner) => Type::Optional(Box::new(self.substitute_type_param(
                inner,
                param,
                replacement,
            ))),
            Type::Function {
                params,
                returns,
                default_count,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|p| self.substitute_type_param(p, param, replacement))
                    .collect(),
                returns: Box::new(self.substitute_type_param(returns, param, replacement)),
                default_count: *default_count,
            },
            Type::Reference(inner) => Type::Reference(Box::new(self.substitute_type_param(
                inner,
                param,
                replacement,
            ))),
            _ => type_.clone(),
        }
    }

    fn substitute_type_params(
        &self,
        type_: &Type,
        params: &[(String, Vec<String>)],
        replacements: &[Type],
    ) -> Type {
        let mut result = type_.clone();
        for ((param_name, _), replacement) in params.iter().zip(replacements) {
            result = self.substitute_type_param(&result, param_name, replacement);
        }
        result
    }

    fn check_method_compatibility(
        &self,
        interface_sig: &MethodSig,
        class_sig: &MethodSig,
        span: Span,
    ) -> Result<(), SemanticError> {
        let mut unifier = Unifier::new();
        // Unify return types
        unifier
            .unify(&interface_sig.return_type, &class_sig.return_type, span)
            .map_err(|e| SemanticError {
                message: format!(
                    "Return type mismatch in interface implementation: {}",
                    e.message
                ),
                span,
            })?;
        // Unify params
        if interface_sig.params.len() != class_sig.params.len() {
            return Err(SemanticError {
                message: format!(
                    "Parameter count mismatch: expected {}, got {}",
                    interface_sig.params.len(),
                    class_sig.params.len()
                ),
                span,
            });
        }
        for (i, (int_param, class_param)) in interface_sig
            .params
            .iter()
            .zip(&class_sig.params)
            .enumerate()
        {
            unifier
                .unify(int_param, class_param, span)
                .map_err(|e| SemanticError {
                    message: format!(
                        "Parameter {} type mismatch in interface implementation: {}",
                        i, e.message
                    ),
                    span,
                })?;
        }
        Ok(())
    }

    fn get_instantiated_constructor_type(
        &mut self,
        name: &str,
        type_args: &[TypeNode],
        span: Span,
    ) -> Result<Type, SemanticError> {
        let symbol = self
            .symbol_table
            .get_cloned(name)
            .ok_or_else(|| SemanticError {
                message: format!("Undefined type '{}'", name),
                span,
            })?;
        if symbol.kind != SymbolKind::Class {
            return Err(SemanticError {
                message: format!("'{}' is not a class", name),
                span,
            });
        }
        let resolved_args = type_args
            .iter()
            .map(|arg| self.resolve_type(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if resolved_args.len() != symbol.type_params.len() {
            return Err(SemanticError {
                message: format!(
                    "Expected {} type arguments for '{}', got {}",
                    symbol.type_params.len(),
                    name,
                    resolved_args.len()
                ),
                span,
            });
        }
        let new_sig = symbol.methods.get("new").ok_or_else(|| SemanticError {
            message: format!("Class '{}' has no constructor", name),
            span,
        })?;
        let substituted_params = new_sig
            .params
            .iter()
            .map(|p| self.substitute_type_params(p, &symbol.type_params, &resolved_args))
            .collect();
        let substituted_returns =
            self.substitute_type_params(&new_sig.return_type, &symbol.type_params, &resolved_args);
        Ok(Type::Function {
            params: substituted_params,
            returns: Box::new(substituted_returns),
            default_count: 0,
        })
    }

    fn type_implements_interface(&self, type_: &Type, interface_name: &str) -> bool {
        match type_ {
            Type::Named(name, _) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    symbol.interfaces.contains_key(interface_name)
                } else {
                    false
                }
            }
            Type::Primitive(prim) => match prim {
                PrimitiveType::Str => {
                    interface_name == "Add"
                        || interface_name == "Stringable"
                        || interface_name == "Equatable"
                        || interface_name == "Comparable"
                }
                PrimitiveType::Int => {
                    matches!(
                        interface_name,
                        "Add"
                            | "Sub"
                            | "Mul"
                            | "Div"
                            | "Arithmetic"
                            | "Stringable"
                            | "Equatable"
                            | "Comparable"
                    )
                }
                PrimitiveType::Float => {
                    matches!(
                        interface_name,
                        "Add"
                            | "Sub"
                            | "Mul"
                            | "Div"
                            | "Arithmetic"
                            | "Stringable"
                            | "Equatable"
                            | "Comparable"
                    )
                }
                PrimitiveType::Bool => {
                    matches!(interface_name, "Stringable" | "Equatable")
                }
                _ => false,
            },
            Type::Variable(var) | Type::Generic(var) => {
                if let Some(bounds) = self.current_bounds.get(var) {
                    bounds.contains(&interface_name.to_string())
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn get_builtin_interface_method(
        &self,
        interface_name: &str,
        method_name: &str,
    ) -> Option<MethodSig> {
        match interface_name {
            "Stringable" => match method_name {
                "to_string" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Str),
                    is_static: false,
                }),
                _ => None,
            },
            "Add" => match method_name {
                "add" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Generic("Self".to_string()),
                    is_static: false,
                }),
                _ => None,
            },
            "Sub" => match method_name {
                "sub" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Generic("Self".to_string()),
                    is_static: false,
                }),
                _ => None,
            },
            "Mul" => match method_name {
                "mul" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Generic("Self".to_string()),
                    is_static: false,
                }),
                _ => None,
            },
            "Div" => match method_name {
                "div" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Generic("Self".to_string()),
                    is_static: false,
                }),
                _ => None,
            },
            "Arithmetic" => match method_name {
                "add" | "sub" | "mul" | "div" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Generic("Self".to_string()),
                    is_static: false,
                }),
                _ => None,
            },
            "Equatable" => match method_name {
                "eq" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                _ => None,
            },
            "Comparable" => match method_name {
                "cmp" => Some(MethodSig {
                    params: vec![Type::Generic("Self".to_string())],
                    return_type: Type::Primitive(PrimitiveType::Int),
                    is_static: false,
                }),
                _ => None,
            },
            _ => None,
        }
    }

    fn get_method_sig(&self, type_: &Type, method_name: &str) -> Option<MethodSig> {
        match type_ {
            Type::Named(name, args) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    if let Some(sig) = symbol.methods.get(method_name) {
                        if args.is_empty() {
                            return Some(sig.clone());
                        } else {
                            // Substitute type parameters for instantiated generics
                            let substituted_params = sig
                                .params
                                .iter()
                                .map(|p| self.substitute_type_params(p, &symbol.type_params, args))
                                .collect();
                            let substituted_return = self.substitute_type_params(
                                &sig.return_type,
                                &symbol.type_params,
                                args,
                            );
                            return Some(MethodSig {
                                params: substituted_params,
                                return_type: substituted_return,
                                is_static: false,
                            });
                        }
                    }
                    for interface_methods in symbol.interfaces.values() {
                        if let Some(sig) = interface_methods.get(method_name) {
                            return Some(sig.clone());
                        }
                    }
                }
                None
            }
            Type::Variable(var) | Type::Generic(var) => {
                if let Some(bounds) = self.current_bounds.get(var) {
                    for bound in bounds {
                        // First check if it's a built-in interface
                        if let Some(sig) = self.get_builtin_interface_method(bound, method_name) {
                            return Some(sig);
                        }
                        // Then check the symbol table
                        if let Some(interface_symbol) = self.symbol_table.lookup(bound) {
                            if let Some(interface_methods) = interface_symbol.interfaces.get(bound)
                            {
                                if let Some(sig) = interface_methods.get(method_name) {
                                    return Some(sig.clone());
                                }
                            }
                        }
                    }
                }
                None
            }
            Type::Primitive(prim) => {
                match prim {
                    PrimitiveType::Int => match method_name {
                        "to_string" | "to_float" | "to_int" => Some(MethodSig {
                            params: vec![],
                            return_type: if method_name == "to_string" {
                                Type::Primitive(PrimitiveType::Str)
                            } else if method_name == "to_float" {
                                Type::Primitive(PrimitiveType::Float)
                            } else {
                                Type::Primitive(PrimitiveType::Int)
                            },
                            is_static: false,
                        }),
                        // Add interface
                        "add" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Int)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        // Sub interface
                        "sub" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Int)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        // Mul interface
                        "mul" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Int)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        // Div interface
                        "div" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Int)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        // Equatable interface
                        "eq" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Int)],
                            return_type: Type::Primitive(PrimitiveType::Bool),
                            is_static: false,
                        }),
                        // Comparable interface
                        "cmp" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Int)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        _ => None,
                    },
                    PrimitiveType::Float => match method_name {
                        "to_string" | "to_int" | "to_float" => Some(MethodSig {
                            params: vec![],
                            return_type: if method_name == "to_string" {
                                Type::Primitive(PrimitiveType::Str)
                            } else if method_name == "to_int" {
                                Type::Primitive(PrimitiveType::Int)
                            } else {
                                Type::Primitive(PrimitiveType::Float)
                            },
                            is_static: false,
                        }),
                        // Add interface
                        "add" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Float)],
                            return_type: Type::Primitive(PrimitiveType::Float),
                            is_static: false,
                        }),
                        // Sub interface
                        "sub" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Float)],
                            return_type: Type::Primitive(PrimitiveType::Float),
                            is_static: false,
                        }),
                        // Mul interface
                        "mul" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Float)],
                            return_type: Type::Primitive(PrimitiveType::Float),
                            is_static: false,
                        }),
                        // Div interface
                        "div" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Float)],
                            return_type: Type::Primitive(PrimitiveType::Float),
                            is_static: false,
                        }),
                        // Equatable interface
                        "eq" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Float)],
                            return_type: Type::Primitive(PrimitiveType::Bool),
                            is_static: false,
                        }),
                        // Comparable interface
                        "cmp" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Float)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        _ => None,
                    },
                    PrimitiveType::Str => match method_name {
                        "to_string" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Primitive(PrimitiveType::Str),
                            is_static: false,
                        }),
                        "length" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        "to_int" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Named(
                                "Result".to_string(),
                                vec![
                                    Type::Primitive(PrimitiveType::Int),
                                    Type::Primitive(PrimitiveType::Str),
                                ],
                            ),
                            is_static: false,
                        }),
                        "to_float" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Named(
                                "Result".to_string(),
                                vec![
                                    Type::Primitive(PrimitiveType::Float),
                                    Type::Primitive(PrimitiveType::Str),
                                ],
                            ),
                            is_static: false,
                        }),
                        // Add interface (string concatenation)
                        "add" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Str)],
                            return_type: Type::Primitive(PrimitiveType::Str),
                            is_static: false,
                        }),
                        // Equatable interface
                        "eq" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Str)],
                            return_type: Type::Primitive(PrimitiveType::Bool),
                            is_static: false,
                        }),
                        // Comparable interface (lexicographic comparison)
                        "cmp" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Str)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        _ => None,
                    },
                    PrimitiveType::Bool => match method_name {
                        "to_string" | "to_int" | "to_float" => Some(MethodSig {
                            params: vec![],
                            return_type: if method_name == "to_string" {
                                Type::Primitive(PrimitiveType::Str)
                            } else if method_name == "to_int" {
                                Type::Primitive(PrimitiveType::Int)
                            } else {
                                Type::Primitive(PrimitiveType::Float)
                            },
                            is_static: false,
                        }),
                        // Equatable interface
                        "eq" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Bool)],
                            return_type: Type::Primitive(PrimitiveType::Bool),
                            is_static: false,
                        }),
                        _ => None,
                    },
                    PrimitiveType::Char => match method_name {
                        "to_string" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Primitive(PrimitiveType::Str),
                            is_static: false,
                        }),
                        "to_int" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Named(
                                "Result".to_string(),
                                vec![
                                    Type::Primitive(PrimitiveType::Int),
                                    Type::Primitive(PrimitiveType::Str),
                                ],
                            ),
                            is_static: false,
                        }),
                        // Equatable interface
                        "eq" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Char)],
                            return_type: Type::Primitive(PrimitiveType::Bool),
                            is_static: false,
                        }),
                        // Comparable interface
                        "cmp" => Some(MethodSig {
                            params: vec![Type::Primitive(PrimitiveType::Char)],
                            return_type: Type::Primitive(PrimitiveType::Int),
                            is_static: false,
                        }),
                        _ => None,
                    },
                    PrimitiveType::Void => None,
                    PrimitiveType::Auto => None,
                }
            }
            Type::List(elem_type) => match method_name {
                "push_back" => Some(MethodSig {
                    params: vec![*elem_type.clone()],
                    return_type: Type::Void,
                    is_static: false,
                }),
                "pop_back" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Optional(elem_type.clone()),
                    is_static: false,
                }),
                "push" => Some(MethodSig {
                    params: vec![*elem_type.clone()],
                    return_type: Type::Void,
                    is_static: false,
                }),
                "pop" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Optional(elem_type.clone()),
                    is_static: false,
                }),
                "get" => Some(MethodSig {
                    params: vec![Type::Primitive(PrimitiveType::Int)],
                    return_type: Type::Optional(elem_type.clone()),
                    is_static: false,
                }),
                "is_empty" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                "size" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Int),
                    is_static: false,
                }),
                "to_string" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Str),
                    is_static: false,
                }),
                _ => None,
            },
            Type::Map(key_type, value_type) => match method_name {
                "put" => Some(MethodSig {
                    params: vec![*key_type.clone(), *value_type.clone()],
                    return_type: Type::Void,
                    is_static: false,
                }),
                "get" => Some(MethodSig {
                    params: vec![*key_type.clone()],
                    return_type: Type::Optional(value_type.clone()),
                    is_static: false,
                }),
                "contains" => Some(MethodSig {
                    params: vec![*key_type.clone()],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                "remove" => Some(MethodSig {
                    params: vec![*key_type.clone()],
                    return_type: Type::Optional(value_type.clone()),
                    is_static: false,
                }),
                "size" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Int),
                    is_static: false,
                }),
                "is_empty" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                "to_string" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Str),
                    is_static: false,
                }),
                _ => None,
            },
            Type::Set(elem_type) => match method_name {
                "add" => Some(MethodSig {
                    params: vec![*elem_type.clone()],
                    return_type: Type::Void,
                    is_static: false,
                }),
                "remove" => Some(MethodSig {
                    params: vec![*elem_type.clone()],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                "contains" => Some(MethodSig {
                    params: vec![*elem_type.clone()],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                "size" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Int),
                    is_static: false,
                }),
                "is_empty" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_static: false,
                }),
                "to_string" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Str),
                    is_static: false,
                }),
                _ => None,
            },
            Type::Optional(_) => match method_name {
                "to_string" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Str),
                    is_static: false,
                }),
                _ => None,
            },

            _ => None,
        }
    }

    fn resolve_binary_operator(
        &self,
        left_type: &Type,
        right_type: &Type,
        op: &BinaryOp,
    ) -> Option<Type> {
        // Handle 'in' operator separately since it doesn't require same types
        if let BinaryOp::In = op {
            // 'in' operator checks if left operand is contained in right operand
            // Right operand should be a collection type (list, set, or map)
            match right_type {
                Type::List(_) | Type::Set(_) => {
                    // For list and set, left operand should be compatible with element type
                    Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
                }
                Type::Map(key_type, _) => {
                    // For map, left operand should be compatible with key type
                    if left_type == key_type.as_ref() {
                        Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
                    } else {
                        None
                    }
                }
                Type::Primitive(PrimitiveType::Str) => {
                    // For string, left operand should be a character or string
                    if matches!(
                        left_type,
                        Type::Primitive(PrimitiveType::Char | PrimitiveType::Str)
                    ) {
                        Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } else {
            // No implicit type promotion: operands must have the same type (or be supported by
            // explicit interfaces/overloads elsewhere in the type system).
            if left_type != right_type {
                return None;
            }

            match op {
                BinaryOp::Add => {
                    // built-in support for primitives and collections, or interface support for custom types
                    if matches!(
                        left_type,
                        Type::Primitive(
                            PrimitiveType::Str | PrimitiveType::Int | PrimitiveType::Float
                        ) | Type::List(_)
                            | Type::Map(_, _)
                            | Type::Set(_)
                    ) || self.type_implements_interface(left_type, "Add")
                    {
                        Some(left_type.clone())
                    } else {
                        None
                    }
                }
                BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo
                | BinaryOp::Exponent => {
                    // built-in support for numeric primitives, or interface support for custom types
                    if matches!(
                        left_type,
                        Type::Primitive(PrimitiveType::Int | PrimitiveType::Float)
                    ) || self.type_implements_interface(left_type, "Arithmetic")
                    {
                        Some(left_type.clone())
                    } else {
                        None
                    }
                }
                BinaryOp::Equal | BinaryOp::NotEqual => {
                    // equality is supported for all types
                    Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
                }
                BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual => {
                    // comparison operators for ordered types
                    if matches!(
                        left_type,
                        Type::Primitive(
                            PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Str
                        )
                    ) || self.type_implements_interface(left_type, "Comparable")
                    {
                        Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
                    } else {
                        None
                    }
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    if matches!(left_type, Type::Primitive(PrimitiveType::Bool))
                        && matches!(right_type, Type::Primitive(PrimitiveType::Bool))
                    {
                        Some(Type::Primitive(PrimitiveType::Bool))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
    }

    // first pass, collect hoistable declarations like functions and classes.
    fn collect_hoistable_declarations(&mut self, ast: &[AstNode]) -> Result<(), SemanticError> {
        for node in ast {
            match node {
                AstNode::Function(func) => {
                    if func.is_common {
                        return Err(SemanticError {
                            message: "Common methods are only allowed in classes".to_string(),
                            span: func.span,
                        });
                    }
                    // resolve function type
                    let param_types = func
                        .params
                        .iter()
                        .map(|p| self.resolve_type(&p.type_))
                        .collect::<Result<Vec<_>, _>>()?;
                    let return_type = self.resolve_type(&func.return_type)?;
                    let mut func_type = Type::Function {
                        params: param_types,
                        returns: Box::new(return_type),
                        default_count: 0,
                    };

                    // substitute type params with variables
                    for (type_param_name, _) in &func.type_params {
                        let var = Type::Variable(type_param_name.clone());
                        func_type = self.substitute_type_param(&func_type, type_param_name, &var);
                    }

                    // Count parameters with default values
                    let default_count = func
                        .params
                        .iter()
                        .filter(|p| p.default_value.is_some())
                        .count();

                    if let Err(e) = self.symbol_table.add_symbol(
                        &func.name,
                        Symbol {
                            kind: SymbolKind::Function,
                            span: func.span,
                            type_: Some(func_type),
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: func
                                .type_params
                                .iter()
                                .map(|(p, b)| {
                                    (p.clone(), b.iter().map(|tb| tb.name.clone()).collect())
                                })
                                .collect::<Vec<(String, Vec<String>)>>(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: default_count,
                            variants: None,
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Class {
                    name,
                    traits,
                    fields,
                    methods,
                    type_params,
                    ..
                } => {
                    let mut implemented_interfaces = std::collections::HashMap::new();
                    for trait_ref in traits {
                        // lookup the interface and get its methods
                        if let Some(interface_symbol) = self.symbol_table.lookup(&trait_ref.name) {
                            if let Some(interface_methods) =
                                interface_symbol.interfaces.get(&trait_ref.name)
                            {
                                // Substitute type_args into interface methods
                                let resolved_args = trait_ref
                                    .type_args
                                    .iter()
                                    .map(|arg| self.resolve_type(arg))
                                    .collect::<Result<Vec<_>, _>>()?;
                                let interface_type_params = &interface_symbol.type_params;
                                let mut substituted_methods = std::collections::HashMap::new();
                                for (method_name, method_sig) in interface_methods {
                                    let sub_params = method_sig
                                        .params
                                        .iter()
                                        .map(|p| {
                                            self.substitute_type_params(
                                                p,
                                                interface_type_params,
                                                &resolved_args,
                                            )
                                        })
                                        .collect();
                                    let sub_return = self.substitute_type_params(
                                        &method_sig.return_type,
                                        interface_type_params,
                                        &resolved_args,
                                    );
                                    substituted_methods.insert(
                                        method_name.clone(),
                                        MethodSig {
                                            params: sub_params,
                                            return_type: sub_return,
                                            is_static: method_sig.is_static,
                                        },
                                    );
                                }
                                implemented_interfaces
                                    .insert(trait_ref.name.clone(), substituted_methods);
                            }
                        }
                    }
                    let type_param_bounds: Vec<(String, Vec<String>)> = type_params
                        .iter()
                        .map(|(p, b)| (p.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
                        .collect();

                    // Add class type parameters to symbol table so they can be resolved in field types
                    for (param_name, _) in type_params {
                        let _ = self.symbol_table.add_symbol(
                            param_name,
                            Symbol {
                                kind: SymbolKind::Type,
                                span: *node.span(),
                                type_: Some(Type::Generic(param_name.clone())),
                                interfaces: std::collections::HashMap::new(),
                                methods: std::collections::HashMap::new(),
                                fields: std::collections::HashMap::new(),
                                type_params: Vec::new(),
                                original_name: None,
                                llvm_name: None,
                                default_param_count: 0,
                                variants: None,
                            },
                        );
                    }

                    // collect field types for constructor and fields map
                    let mut field_types = Vec::new();
                    let mut fields_map = std::collections::HashMap::new();
                    for field in fields {
                        // Check for duplicate field
                        if fields_map.contains_key(&field.name) {
                            return Err(SemanticError {
                                message: format!(
                                    "Duplicate field '{}' in class '{}'",
                                    field.name, name
                                ),
                                span: field.type_.span,
                            });
                        }
                        match self.resolve_type(&field.type_) {
                            Ok(t) => {
                                field_types.push(t.clone());
                                fields_map.insert(field.name.clone(), (t, field.is_const));
                            }
                            Err(e) => self.errors.push(e),
                        }
                    }
                    if field_types.is_empty() {
                        None
                    } else {
                        Some(Type::Function {
                            params: field_types.clone(),
                            returns: Box::new(Type::Named(name.clone(), vec![])),
                            default_count: 0,
                        })
                    };

                    // collect methods
                    let mut methods_map = std::collections::HashMap::new();
                    for method in methods {
                        let mut param_types = Vec::new();
                        for p in &method.params {
                            match self.resolve_type(&p.type_) {
                                Ok(t) => param_types.push(t),
                                Err(e) => self.errors.push(e),
                            }
                        }
                        let ret = match self.resolve_type(&method.return_type) {
                            Ok(t) => t,
                            Err(e) => {
                                self.errors.push(e);
                                continue;
                            }
                        };
                        let method_sig = MethodSig {
                            params: param_types,
                            return_type: ret,
                            is_static: method.is_common,
                        };
                        methods_map.insert(method.name.clone(), method_sig);
                    }
                    // add "new" as static method for constructor
                    let new_sig = MethodSig {
                        params: vec![], // Default constructor takes no args
                        return_type: Type::Named(
                            name.clone(),
                            type_param_bounds
                                .iter()
                                .map(|(p, _)| Type::Variable(p.clone()))
                                .collect::<Vec<_>>(),
                        ),
                        is_static: true,
                    };
                    methods_map.insert("new".to_string(), new_sig);

                    // Validate interface implementations
                    for (interface_name, interface_methods) in &implemented_interfaces {
                        for (method_name, interface_sig) in interface_methods {
                            if let Some(class_sig) = methods_map.get(method_name) {
                                if let Err(e) = self.check_method_compatibility(
                                    interface_sig,
                                    class_sig,
                                    *node.span(),
                                ) {
                                    self.errors.push(e);
                                }
                            } else {
                                self.errors.push(SemanticError {
                                    message: format!(
                                        "Class '{}' does not implement method '{}' required by interface '{}'",
                                        name, method_name, interface_name
                                    ),
                                    span: *node.span(),
                                });
                            }
                        }

                        // Validate interface field requirements
                        if let Some(interface_symbol) = self.symbol_table.lookup(interface_name) {
                            for (field_name, (interface_field_type, interface_is_const)) in
                                &interface_symbol.fields
                            {
                                if let Some((class_field_type, class_is_const)) =
                                    fields_map.get(field_name)
                                {
                                    // Check type compatibility
                                    if !self
                                        .types_compatible(class_field_type, interface_field_type)
                                    {
                                        self.errors.push(SemanticError {
                                            message: format!(
                                                "Field '{}' type mismatch in class '{}': class has {}, interface '{}' requires {}",
                                                field_name,
                                                name,
                                                format_type(class_field_type),
                                                interface_name,
                                                format_type(interface_field_type)
                                            ),
                                            span: *node.span(),
                                        });
                                    }

                                    // Check const compatibility
                                    if *interface_is_const && !*class_is_const {
                                        self.errors.push(SemanticError {
                                            message: format!(
                                                "Field '{}' must be const in class '{}' to implement interface '{}'",
                                                field_name, name, interface_name
                                            ),
                                            span: *node.span(),
                                        });
                                    }
                                } else {
                                    // Field missing in class
                                    self.errors.push(SemanticError {
                                        message: format!(
                                            "Class '{}' missing required field '{}' from interface '{}'",
                                            name, field_name, interface_name
                                        ),
                                        span: *node.span(),
                                    });
                                }
                            }
                        }
                    }

                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Class,
                            span: *node.span(),
                            type_: Some(Type::Named(name.clone(), vec![])),
                            interfaces: implemented_interfaces,
                            methods: methods_map,
                            fields: fields_map,
                            type_params: type_param_bounds,
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Enum { name, variants, .. } => {
                    let mut methods = std::collections::HashMap::new();
                    let mut variant_names = Vec::new();
                    for variant in variants {
                        variant_names.push(variant.name.clone());
                        let params = variant
                            .data
                            .clone()
                            .unwrap_or_default()
                            .into_iter()
                            .map(|t| self.resolve_type(&t).unwrap_or(Type::Void))
                            .collect();
                        let return_type = Type::Named(name.clone(), vec![]);
                        methods.insert(
                            variant.name.clone(),
                            MethodSig {
                                params,
                                return_type,
                                is_static: true,
                            },
                        );
                    }
                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Enum,
                            span: *node.span(),
                            type_: Some(Type::Named(name.clone(), vec![])),
                            interfaces: std::collections::HashMap::new(),
                            methods,
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: Some(variant_names),
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Interface {
                    name,
                    type_params,
                    fields,
                    methods,
                    ..
                } => {
                    let mut interface_methods = std::collections::HashMap::new();
                    for method in methods {
                        let param_types = method
                            .params
                            .iter()
                            .map(|p| self.resolve_type(&p.type_))
                            .collect::<Result<Vec<_>, _>>()?;
                        let return_type = self.resolve_type(&method.return_type)?;
                        let method_sig = MethodSig {
                            params: param_types,
                            return_type,
                            is_static: false,
                        };
                        interface_methods.insert(method.name.clone(), method_sig);
                    }

                    // Process interface fields
                    let mut interface_fields = std::collections::HashMap::new();
                    for field in fields {
                        let field_type = self.resolve_type(&field.type_)?;

                        // Validate default value type if present
                        if let Some(default_expr) = &field.default_value {
                            let default_type = self.infer_literal_type(default_expr)?;
                            if !self.types_compatible(&default_type, &field_type) {
                                self.errors.push(SemanticError {
                                    message: format!(
                                        "Default value type mismatch for field '{}': expected {}, got {}",
                                        field.name,
                                        format_type(&field_type),
                                        format_type(&default_type)
                                    ),
                                    span: default_expr.span,
                                });
                            }
                        }

                        interface_fields.insert(field.name.clone(), (field_type, field.is_const));
                    }

                    let mut interfaces_map = std::collections::HashMap::new();
                    interfaces_map.insert(name.clone(), interface_methods);
                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Interface,
                            span: *node.span(),
                            type_: None,
                            interfaces: interfaces_map,
                            methods: std::collections::HashMap::new(),
                            fields: interface_fields,
                            type_params: type_params
                                .iter()
                                .map(|(p, b)| {
                                    (p.clone(), b.iter().map(|tb| tb.name.clone()).collect())
                                })
                                .collect::<Vec<(String, Vec<String>)>>(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    // second pass, analyze all nodes with full symbol information.
    fn analyze_nodes(&mut self, nodes: &[AstNode], mut files: Option<&mut Files>) {
        for node in nodes {
            if let Err(e) = self.analyze_node(node, files.as_deref_mut()) {
                self.errors.push(e);
            }
        }
    }

    fn analyze_node(
        &mut self,
        node: &AstNode,
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        match node {
            AstNode::Function(func) => {
                if func.is_common {
                    return Err(SemanticError {
                        message: "Common methods are only allowed in classes".to_string(),
                        span: func.span,
                    });
                }

                // Check that main() returns void
                if func.name == "main" {
                    let return_type = self.resolve_type(&func.return_type)?;
                    if !matches!(return_type, Type::Void) {
                        return Err(SemanticError {
                            message: format!(
                                "Function 'main' must return void, not '{}'",
                                format_type(&return_type)
                            ),
                            span: func.return_type.span,
                        });
                    }
                }

                self.analyze_function(func, None)
            }
            AstNode::Class {
                name,
                fields,
                methods,
                type_params,
                ..
            } => {
                let type_param_bounds: Vec<(String, Vec<String>)> = type_params
                    .iter()
                    .map(|(p, b)| (p.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
                    .collect();
                self.analyze_class(name, fields, methods, &type_param_bounds)
            }
            AstNode::Enum { .. } => Ok(()), // enums don't need further analysis.
            AstNode::Interface { .. } => Ok(()), // interfaces don't need further analysis.
            AstNode::Statement(stmt) => self.analyze_statement(stmt, files),
        }
    }

    fn analyze_function(
        &mut self,
        func: &FunctionNode,
        self_type: Option<Type>,
    ) -> Result<(), SemanticError> {
        let was_static = self.is_in_static_method;
        let old_self_type = self.current_self_type.clone();
        let old_return_type = self.current_return_type.clone();
        self.is_in_static_method = func.is_common;
        self.current_self_type = self_type.clone();

        // Set return type context
        let return_type = self.resolve_type(&func.return_type)?;
        self.current_return_type = Some(return_type.clone());

        // set current bounds
        self.current_bounds.clear();
        for (param, bounds) in &func.type_params {
            let bound_names = bounds.iter().map(|b| b.name.clone()).collect();
            self.current_bounds.insert(param.clone(), bound_names);
        }

        // Also add class-level type params (for methods in generic classes)
        if let Some(class_type_params) = &self.current_class_type_params {
            for (param, bounds) in class_type_params {
                // Don't override function-level type params
                if !self.current_bounds.contains_key(param) {
                    self.current_bounds.insert(param.clone(), bounds.clone());
                }
            }
        }

        // create new scope for function parameters and body.
        self.symbol_table.push_scope()?;

        // add generic type parameters to symbol table
        for (param_name, _) in &func.type_params {
            self.symbol_table.add_symbol(
                param_name,
                Symbol {
                    kind: SymbolKind::Type,
                    span: func.span, // Use function span since we don't have param spans
                    type_: Some(Type::Generic(param_name.clone())),
                    interfaces: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    fields: std::collections::HashMap::new(),
                    type_params: Vec::new(),
                    original_name: None,
                    llvm_name: None,
                    default_param_count: 0,
                    variants: None,
                },
            )?;
        }

        // add self if provided
        if let Some(self_type) = self_type {
            self.symbol_table.add_symbol(
                "self",
                Symbol {
                    kind: SymbolKind::Variable,
                    span: func.span,
                    type_: Some(self_type),
                    interfaces: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    fields: std::collections::HashMap::new(),
                    type_params: Vec::new(),
                    original_name: None,
                    llvm_name: None,
                    default_param_count: 0,
                    variants: None,
                },
            )?;
        }

        for param in &func.params {
            let param_type = self.resolve_type(&param.type_)?;
            self.symbol_table.add_symbol(
                &param.name,
                Symbol {
                    kind: SymbolKind::Variable,
                    span: param.type_.span,
                    type_: Some(param_type),
                    interfaces: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    fields: std::collections::HashMap::new(),
                    type_params: Vec::new(),
                    original_name: None,
                    llvm_name: None,
                    default_param_count: 0,
                    variants: None,
                },
            )?;
        }

        // analyze function body with new scope.
        self.analyze_block(&func.body, None)?;

        // clean up function scope.
        self.symbol_table.pop_scope()?;
        self.current_bounds.clear();
        self.is_in_static_method = was_static;
        self.current_self_type = old_self_type;
        self.current_return_type = old_return_type;
        Ok(())
    }

    fn analyze_class(
        &mut self,
        name: &str,
        _fields: &[Field],
        methods: &[FunctionNode],
        type_params: &[(String, Vec<String>)],
    ) -> Result<(), SemanticError> {
        // Methods were already added to the class symbol during first pass (collect_hoistable_declarations)
        // Here we just need to analyze method bodies with proper self type

        // Create self type for this class
        let self_type = if type_params.is_empty() {
            Type::Named(name.to_string(), vec![])
        } else {
            // For generic classes, use type variables for self
            Type::Named(
                name.to_string(),
                type_params
                    .iter()
                    .map(|(param_name, _)| Type::Variable(param_name.clone()))
                    .collect(),
            )
        };

        // Set current class type params for method analysis
        self.set_class_type_params(type_params.to_vec());

        // Analyze each method body with proper self type
        for method in methods {
            // Static methods (common) don't have self
            let method_self_type = if method.is_common {
                None
            } else {
                Some(self_type.clone())
            };

            self.analyze_function(method, method_self_type)?;
        }

        // Clear class type params after analyzing class methods
        self.clear_class_type_params();

        Ok(())
    }

    fn analyze_block(
        &mut self,
        stmts: &[StatementNode],
        mut files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        // first collect function declarations in this block.
        for stmt in stmts {
            if let StatementKind::Function(func) = &stmt.kind {
                // resolve function type
                let param_types = func
                    .params
                    .iter()
                    .map(|p| self.resolve_type(&p.type_))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_type = self.resolve_type(&func.return_type)?;
                let mut func_type = Type::Function {
                    params: param_types,
                    returns: Box::new(return_type),
                    default_count: 0,
                };

                // substitute type params with variables
                for (type_param_name, _) in &func.type_params {
                    let var = Type::Variable(type_param_name.clone());
                    func_type = self.substitute_type_param(&func_type, type_param_name, &var);
                }

                // Count parameters with default values
                let default_count = func
                    .params
                    .iter()
                    .filter(|p| p.default_value.is_some())
                    .count();

                self.symbol_table.add_symbol(
                    &func.name,
                    Symbol {
                        kind: SymbolKind::Function,
                        span: stmt.span,
                        type_: Some(func_type),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: func
                            .type_params
                            .iter()
                            .map(|(n, b)| (n.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
                            .collect(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: default_count,
                        variants: None,
                    },
                )?;
            }
        }

        // then analyze all statements in order.
        for stmt in stmts {
            self.analyze_statement(stmt, files.as_deref_mut())?;
        }

        Ok(())
    }

    fn analyze_statement(
        &mut self,
        stmt: &StatementNode,
        mut files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, expr) => {
                self.analyze_expression(expr)?;
                let expr_type = self.get_expression_type(expr)?;
                if matches!(expr_type, Type::EmptyList | Type::EmptyMap | Type::EmptySet) {
                    let collection_type = match expr_type {
                        Type::EmptyList => "list",
                        Type::EmptyMap => "map",
                        Type::EmptySet => "set",
                        _ => unreachable!(),
                    };
                    return Err(SemanticError {
                        message: format!(
                            "Cannot infer type for empty {} literal; use explicit type annotation (e.g., `{}<int> myvar = {{}}`).",
                            collection_type, collection_type
                        ),
                        span: expr.span,
                    });
                }
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                        type_: Some(expr_type),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: 0,
                        variants: None,
                    },
                )?;
            }
            StatementKind::TypedDecl(name, type_node, expr) => {
                let declared_type = self.resolve_type(type_node)?;
                self.analyze_expression(expr)?;
                let expr_type = self.get_expression_type(expr)?;
                self.check_type_compatibility(&declared_type, &expr_type, expr.span)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                        type_: Some(declared_type),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: 0,
                        variants: None,
                    },
                )?;
            }
            StatementKind::Expression(expr) => {
                self.analyze_expression(expr)?;
            }
            StatementKind::Block(stmts) => {
                self.symbol_table.push_scope()?;
                self.analyze_block(stmts, files.as_deref_mut())?;
                self.symbol_table.pop_scope()?;
            }
            StatementKind::ConstDecl(name, type_node, expr) => {
                let declared_type = self.resolve_type(type_node)?;
                self.analyze_expression(expr)?;
                let expr_type = self.get_expression_type(expr)?;
                self.check_type_compatibility(&declared_type, &expr_type, expr.span)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Constant,
                        span: stmt.span,
                        type_: Some(declared_type),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: 0,
                        variants: None,
                    },
                )?;
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.analyze_expression(cond)?;

                self.symbol_table.push_scope()?;
                self.analyze_block(then_block, files.as_deref_mut())?;
                self.symbol_table.pop_scope()?;

                if let Some(else_block) = else_block {
                    self.symbol_table.push_scope()?;
                    self.analyze_block(else_block, files.as_deref_mut())?;
                    self.symbol_table.pop_scope()?;
                }
            }
            StatementKind::For {
                var, iter, body, ..
            } => {
                self.symbol_table.push_scope()?;
                self.analyze_expression(iter)?;
                let iter_type = self.get_expression_type(iter)?;
                // for now, assume iterator yields the element type directly
                // Note: Iteration handled via to_list() for sets/maps; no Iterator interface needed.
                let var_type = match iter_type {
                    Type::List(elem_type) => *elem_type,
                    _ => {
                        return Err(SemanticError {
                            message: "Cannot iterate over non-list type".into(),
                            span: iter.span,
                        });
                    }
                };
                self.symbol_table.add_symbol(
                    var,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                        type_: Some(var_type),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: 0,
                        variants: None,
                    },
                )?;
                self.analyze_block(body, files.as_deref_mut())?;
                self.symbol_table.pop_scope()?;
            }
            StatementKind::While { cond, body } => {
                self.analyze_expression(cond)?;

                self.symbol_table.push_scope()?;
                self.analyze_block(body, files.as_deref_mut())?;
                self.symbol_table.pop_scope()?;
            }
            StatementKind::Match { expr, arms } => {
                self.analyze_expression(expr)?;
                let expr_type = self.get_expression_type(expr)?;

                // Track return types from each arm if we're in a non-void function context
                let expecting_return = self.current_return_type.is_some()
                    && !matches!(self.current_return_type, Some(Type::Void));
                let mut arm_return_types = Vec::new();

                for arm in arms {
                    self.symbol_table.push_scope()?;
                    // set types for pattern variables based on expr_type
                    self.set_pattern_types(&arm.pattern, &expr_type, expr.span)?;
                    self.analyze_pattern(&arm.pattern)?;
                    if let Some(guard) = &arm.guard {
                        self.analyze_expression(guard)?;
                    }
                    self.analyze_block(&arm.body, files.as_deref_mut())?;

                    // Check for return in this arm
                    if expecting_return {
                        let mut has_return = false;
                        for stmt in &arm.body {
                            if let StatementKind::Return(Some(ret_expr)) = &stmt.kind {
                                let ret_type = self.get_expression_type(ret_expr)?;
                                arm_return_types.push((ret_type, stmt.span));
                                has_return = true;
                                break;
                            } else if let StatementKind::Return(None) = &stmt.kind {
                                has_return = true;
                                // Void return in non-void context - will be caught by Return validation
                            }
                        }
                        if !has_return {
                            return Err(SemanticError {
                                message: "Match arm must return a value in a function with non-void return type".into(),
                                span: arm.body.first().map(|s| s.span).unwrap_or(expr.span),
                            });
                        }
                    }

                    self.symbol_table.pop_scope()?;
                }

                // Validate all arms return compatible types
                if !arm_return_types.is_empty() {
                    let (first_type, _first_span) = &arm_return_types[0];
                    for (i, (arm_type, arm_span)) in arm_return_types.iter().enumerate().skip(1) {
                        if self
                            .check_type_compatibility(first_type, arm_type, *arm_span)
                            .is_err()
                        {
                            return Err(SemanticError {
                                message: format!(
                                    "Match arms have incompatible return types: first arm returns '{}', but arm {} returns '{}'",
                                    format_type(first_type),
                                    i + 1,
                                    format_type(arm_type)
                                ),
                                span: *arm_span,
                            });
                        }
                    }
                }

                // Check exhaustiveness
                self.check_match_exhaustiveness(&expr_type, arms, expr.span)?;

                return Ok(());
            }
            StatementKind::Return(Some(expr)) => {
                // Check if we're in a function at all
                if self.current_return_type.is_none() {
                    return Err(SemanticError {
                        message: "Cannot use 'return' outside of a function".into(),
                        span: expr.span,
                    });
                }

                self.analyze_expression(expr)?;

                // Check if we're in a void function
                if matches!(self.current_return_type, Some(Type::Void)) {
                    return Err(SemanticError {
                        message: "Cannot return a value from a void function".into(),
                        span: expr.span,
                    });
                }

                // Validate return type matches function return type
                if let Some(expected_type) = self.current_return_type.clone() {
                    let actual_type = self.get_expression_type(expr)?;
                    self.check_type_compatibility(&expected_type, &actual_type, expr.span)?;
                }
                return Ok(());
            }
            StatementKind::Return(None) => {
                // Check if we're in a function at all
                if self.current_return_type.is_none() {
                    return Err(SemanticError {
                        message: "Cannot use 'return' outside of a function".into(),
                        span: stmt.span,
                    });
                }

                // Check if we're NOT in a void function (missing return value)
                if !matches!(self.current_return_type, Some(Type::Void)) {
                    if let Some(expected_type) = &self.current_return_type {
                        return Err(SemanticError {
                            message: format!(
                                "Missing return value; function expects '{}', but return has no value",
                                format_type(expected_type)
                            ),
                            span: stmt.span,
                        });
                    }
                }
                return Ok(());
            }
            StatementKind::Import { module_path, spec } => {
                use crate::ast::ImportSpec;

                // Handle std library specially
                if module_path.starts_with("std.") {
                    self.handle_std_import(module_path, spec, stmt.span)?;
                    return Ok(());
                }

                // Clone the resolver early to avoid borrow issues
                let resolver = self.module_resolver.clone().ok_or_else(|| SemanticError {
                    message: "Module resolver not available".to_string(),
                    span: stmt.span,
                })?;

                // Files must be available for import processing
                let files = files.expect("Files registry must be available for import processing");

                let module_nodes = resolver
                    .borrow_mut()
                    .resolve_import_path(module_path, self.current_file.as_deref(), files)
                    .map_err(|e| SemanticError {
                        message: format!("Import error: {}", e),
                        span: stmt.span,
                    })?;

                // Analyze the imported module
                let mut module_analyzer = SemanticAnalyzer::new_for_module(resolver.clone());
                module_analyzer.set_current_file(std::path::PathBuf::from(
                    module_path.replace('.', "/") + ".mux",
                ));
                let errors = module_analyzer.analyze(&module_nodes, Some(files));
                if !errors.is_empty() {
                    let error_messages: Vec<String> =
                        errors.iter().map(|e| e.message.clone()).collect();
                    return Err(SemanticError {
                        message: format!(
                            "Errors in imported module {}:\n  {}",
                            module_path,
                            error_messages.join("\n  ")
                        ),
                        span: stmt.span,
                    });
                }

                // Get all symbols from the module
                let module_symbols = module_analyzer.symbol_table.all_symbols.clone();

                // Compute mangled LLVM names for functions in this module
                let module_name_for_mangling = Self::sanitize_module_path(module_path);

                // Merge ALL symbols from the imported module into the main symbol table
                // This is needed for codegen to work properly
                for (name, symbol) in &module_symbols {
                    // Skip built-in functions to avoid conflicts
                    if !matches!(symbol.kind, SymbolKind::Function)
                        || !name.starts_with("print")
                            && !name.starts_with("read_line")
                            && !name.starts_with("range")
                            && !name.starts_with("Some")
                            && !name.starts_with("None")
                            && !name.starts_with("Ok")
                            && !name.starts_with("Err")
                    {
                        // Add symbol to main symbol table if it doesn't already exist
                        if !self.symbol_table.all_symbols.contains_key(name) {
                            let mut mangled_symbol = symbol.clone();
                            // For functions, set the mangled LLVM name
                            if matches!(symbol.kind, SymbolKind::Function) {
                                mangled_symbol.llvm_name =
                                    Some(format!("{}!{}", module_name_for_mangling, name));
                            }
                            self.symbol_table
                                .all_symbols
                                .insert(name.clone(), mangled_symbol);
                        }
                    }
                }

                // Process import specification
                match spec {
                    ImportSpec::Module { alias } => {
                        if let Some(namespace) = alias {
                            // import logger (as ns) - add as namespaced module
                            self.add_module_namespace(
                                namespace,
                                module_symbols,
                                module_path,
                                stmt.span,
                            )?;
                        }
                        // if alias is None, it's a side-effect import (as _) - don't add symbols
                    }

                    ImportSpec::Item { item, alias } => {
                        // import logger.log (as lg)
                        let symbol_name = alias.as_ref().unwrap_or(item);
                        self.import_single_symbol(
                            &module_symbols,
                            item,
                            symbol_name,
                            module_path,
                            stmt.span,
                        )?;
                    }

                    ImportSpec::Items { items } => {
                        // import logger.(log, error as err)
                        for (item, alias) in items {
                            let symbol_name = alias.as_ref().unwrap_or(item);
                            self.import_single_symbol(
                                &module_symbols,
                                item,
                                symbol_name,
                                module_path,
                                stmt.span,
                            )?;
                        }
                    }

                    ImportSpec::Wildcard => {
                        // import logger.* - import all symbols without namespace
                        self.import_all_symbols(&module_symbols, module_path, stmt.span)?;
                    }
                }

                // Cache module and track dependencies
                resolver
                    .borrow_mut()
                    .cache_module(module_path, module_nodes.clone());
                resolver.borrow_mut().finish_import(module_path);

                self.all_module_asts
                    .insert(module_path.to_string(), module_nodes);

                if !self.module_dependencies.contains(&module_path.to_string()) {
                    self.module_dependencies.push(module_path.to_string());
                }
            }
            StatementKind::Function(func) => {
                // Nested function - analyze its body
                self.analyze_function(func, None)?;
            }
            _ => {} // handle other statement types
        }
        Ok(())
    }

    // Check if a match expression covers all variants of an enum
    fn check_match_exhaustiveness(
        &self,
        expr_type: &Type,
        arms: &[crate::ast::MatchArm],
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        // Only check for Named types (user-defined enums)
        if let Type::Named(type_name, _) = expr_type {
            // Look up the enum symbol
            if let Some(symbol) = self.symbol_table.lookup(type_name) {
                if let Some(variant_names) = &symbol.variants {
                    let mut covered: std::collections::HashSet<String> =
                        std::collections::HashSet::new();
                    let mut has_wildcard = false;

                    for arm in arms {
                        match &arm.pattern {
                            PatternNode::Wildcard => {
                                has_wildcard = true;
                                break;
                            }
                            PatternNode::EnumVariant { name, .. } => {
                                covered.insert(name.clone());
                            }
                            PatternNode::Identifier(name) => {
                                // Check if this identifier is actually a variant name
                                if variant_names.contains(name) {
                                    covered.insert(name.clone());
                                }
                                // Otherwise it's a variable binding, doesn't cover a specific variant
                            }
                            // Literals don't cover specific variants
                            _ => {}
                        }
                    }

                    // If there's a wildcard, pattern is exhaustive by design
                    if has_wildcard {
                        return Ok(());
                    }

                    // Check if all variants are covered
                    let uncovered: Vec<&String> = variant_names
                        .iter()
                        .filter(|v| !covered.contains(*v))
                        .collect();

                    if !uncovered.is_empty() {
                        let uncovered_list = uncovered
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        return Err(SemanticError {
                            message: format!(
                                "Non-exhaustive match: patterns not covering all variants of '{}'. Missing: {}",
                                type_name, uncovered_list
                            ),
                            span: expr_span,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    // Set types for pattern variables based on expected type
    // Uses expr.span for pattern errors to point to the match expression
    fn set_pattern_types(
        &mut self,
        pattern: &PatternNode,
        expected_type: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        match pattern {
            PatternNode::Identifier(name) => {
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span,
                        type_: Some(expected_type.clone()),
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                        original_name: None,
                        llvm_name: None,
                        default_param_count: 0,
                        variants: None,
                    },
                )?;
            }
            PatternNode::EnumVariant { name, args } => {
                match expected_type {
                    Type::Optional(inner) => {
                        if name == "Some" && args.len() == 1 {
                            self.set_pattern_types(&args[0], inner, span)?;
                        } else if name == "None" && args.is_empty() {
                            // no vars
                        } else {
                            return Err(SemanticError {
                                message: format!(
                                    "Pattern {} does not match type {}",
                                    name,
                                    format_type(expected_type)
                                ),
                                span,
                            });
                        }
                    }
                    Type::Named(type_name, type_args)
                        if type_name == "Result" && type_args.len() == 2 =>
                    {
                        let ok_type = &type_args[0];
                        let err_type = &type_args[1];
                        if name == "Ok" && args.len() == 1 {
                            self.set_pattern_types(&args[0], ok_type, span)?;
                        } else if name == "Err" && args.len() == 1 {
                            self.set_pattern_types(&args[0], err_type, span)?;
                        } else {
                            return Err(SemanticError {
                                message: format!(
                                    "Pattern {} does not match type {}",
                                    name,
                                    format_type(expected_type)
                                ),
                                span,
                            });
                        }
                    }
                    Type::Named(enum_name, _) => {
                        // For user enums, check if the variant exists and set arg types
                        if let Some(symbol) = self.symbol_table.lookup(enum_name) {
                            if let Some(sig) = symbol.methods.get(name) {
                                if args.len() != sig.params.len() {
                                    return Err(SemanticError {
                                        message: format!(
                                            "Pattern {} has {} args, expected {}",
                                            name,
                                            args.len(),
                                            sig.params.len()
                                        ),
                                        span,
                                    });
                                }
                                for (arg, param_type) in args.iter().zip(&sig.params) {
                                    self.set_pattern_types(arg, param_type, span)?;
                                }
                            } else {
                                return Err(SemanticError {
                                    message: format!(
                                        "Unknown variant {} for enum {}",
                                        name, enum_name
                                    ),
                                    span,
                                });
                            }
                        } else {
                            return Err(SemanticError {
                                message: format!("Unknown enum {}", enum_name),
                                span,
                            });
                        }
                    }
                    _ => {
                        return Err(SemanticError {
                            message: format!(
                                "Enum variant patterns are not supported for type {}",
                                format_type(expected_type)
                            ),
                            span,
                        });
                    }
                }
            }
            PatternNode::Literal(_) => {} // literals don't bind variables
            PatternNode::Wildcard => {}   // no binding
        }
        Ok(())
    }

    // analyze a pattern (for match arms).
    #[allow(clippy::only_used_in_recursion)]
    fn analyze_pattern(&mut self, pattern: &PatternNode) -> Result<(), SemanticError> {
        match pattern {
            PatternNode::Identifier(_) => {} // already added in set_pattern_types
            PatternNode::EnumVariant { args, .. } => {
                for arg in args {
                    self.analyze_pattern(arg)?;
                }
            }
            PatternNode::Literal(_) => {} // literals don't bind variables
            PatternNode::Wildcard => {}   // no binding
        }
        Ok(())
    }

    // analyze an expression.
    fn analyze_expression(&mut self, expr: &ExpressionNode) -> Result<(), SemanticError> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                if name == "self" {
                    if self.is_in_static_method {
                        return Err(SemanticError {
                            message: "Cannot use 'self' in a common method".to_string(),
                            span: expr.span,
                        });
                    }
                    // For 'self', check if we have a current self type
                    if self.current_self_type.is_none() {
                        return Err(SemanticError {
                            message: "Cannot use 'self' outside of a method".to_string(),
                            span: expr.span,
                        });
                    }
                    return Ok(());
                }
                if !self.symbol_table.exists(name) && self.get_builtin_sig(name).is_none() {
                    return Err(SemanticError {
                        message: format!("Undefined variable '{}'", name),
                        span: expr.span,
                    });
                }
                Ok(())
            }
            ExpressionKind::Literal(_) => Ok(()), // literals are fine
            ExpressionKind::None => Ok(()),       // None is fine
            ExpressionKind::Binary { left, right, .. } => {
                self.analyze_expression(left)?;
                self.analyze_expression(right)?;
                // Trigger comprehensive type checking (const, binary operators, etc.)
                let _ = self.get_expression_type(expr)?;
                Ok(())
            }
            ExpressionKind::Unary {
                expr,
                op,
                op_span,
                postfix: _,
            } => {
                self.analyze_expression(expr)?;
                let operand_type = self.get_expression_type(expr)?;
                match op {
                    UnaryOp::Not => {
                        if !matches!(
                            operand_type,
                            Type::Primitive(crate::ast::PrimitiveType::Bool)
                        ) {
                            return Err(SemanticError {
                                message: "Logical 'not' operator requires a boolean operand".into(),
                                span: *op_span,
                            });
                        }
                    }
                    UnaryOp::Neg => {
                        if !matches!(
                            operand_type,
                            Type::Primitive(crate::ast::PrimitiveType::Int)
                                | Type::Primitive(crate::ast::PrimitiveType::Float)
                        ) {
                            return Err(SemanticError {
                                message: "Negation operator requires a numeric operand".into(),
                                span: *op_span,
                            });
                        }
                    }
                    UnaryOp::Ref => {
                        // reference operator, operand can be any type
                    }
                    UnaryOp::Incr | UnaryOp::Decr => {
                        if !matches!(
                            operand_type,
                            Type::Primitive(crate::ast::PrimitiveType::Int)
                        ) {
                            return Err(SemanticError {
                                message: "Increment/decrement operators require an int operand"
                                    .into(),
                                span: *op_span,
                            });
                        }

                        // Check if trying to modify a constant
                        if let crate::ast::ExpressionKind::Identifier(name) = &expr.kind {
                            if let Some(symbol) = self.symbol_table.lookup(name) {
                                if symbol.kind == SymbolKind::Constant {
                                    return Err(SemanticError::with_help(
                                        format!("Cannot modify constant '{}'", name),
                                        *op_span,
                                        "Constants cannot be modified after initialization",
                                    ));
                                }
                            }
                        }

                        if let crate::ast::ExpressionKind::FieldAccess {
                            expr: obj_expr,
                            field,
                        } = &expr.kind
                        {
                            // Check if field is const
                            let obj_type = self.get_expression_type(obj_expr)?;
                            if let Type::Named(class_name, _) = &obj_type {
                                if let Some(symbol) = self.symbol_table.lookup(class_name) {
                                    if let Some((_field_type, is_const)) = symbol.fields.get(field)
                                    {
                                        if *is_const {
                                            return Err(SemanticError {
                                                message: format!(
                                                    "Cannot modify const field '{}'",
                                                    field
                                                ),
                                                span: *op_span,
                                            });
                                        }
                                    }
                                }
                            }
                        }

                        // Dereference increment/decrement is allowed (no const check needed)
                    }
                    _ => {} // other unary ops not fully implemented yet
                }
                Ok(())
            }
            ExpressionKind::Call { func, args } => {
                // Check for undefined function before analyzing
                if let ExpressionKind::Identifier(name) = &func.kind {
                    if !self.symbol_table.exists(name) && self.get_builtin_sig(name).is_none() {
                        return Err(SemanticError {
                            message: format!("Undefined function '{}'", name),
                            span: func.span,
                        });
                    }
                }

                self.analyze_expression(func)?;
                for arg in args {
                    self.analyze_expression(arg)?;
                }
                // Special check for Some
                if let ExpressionKind::Identifier(name) = &func.kind {
                    if name == "Some" {
                        if args.len() != 1 {
                            return Err(SemanticError {
                                message: "Some() takes exactly 1 argument".to_string(),
                                span: expr.span,
                            });
                        }
                        let arg_type = self.get_expression_type(&args[0])?;
                        if let Type::Optional(_) = arg_type {
                            return Err(SemanticError {
                                message: "Some() cannot take an optional value".to_string(),
                                span: expr.span,
                            });
                        }
                    }
                }
                // Trigger comprehensive type checking (callable, method existence, args)
                let _ = self.get_expression_type(expr)?;
                Ok(())
            }
            ExpressionKind::FieldAccess { expr, .. } => {
                self.analyze_expression(expr)?;
                // Trigger comprehensive type checking (field/method existence)
                let _ = self.get_expression_type(expr)?;
                Ok(())
            }
            ExpressionKind::ListAccess { expr, index } => {
                self.analyze_expression(expr)?;
                self.analyze_expression(index)?;
                // type check list/map access
                let target_type = self.get_expression_type(expr)?;
                let index_type = self.get_expression_type(index)?;
                match &target_type {
                    Type::List(_) => {
                        // List requires integer index
                        if !matches!(index_type, Type::Primitive(crate::ast::PrimitiveType::Int)) {
                            return Err(SemanticError {
                                message: "List index must be an integer".into(),
                                span: index.span,
                            });
                        }
                    }
                    Type::Map(expected_key_type, _) => {
                        // Map requires matching key type
                        if index_type != **expected_key_type {
                            return Err(SemanticError {
                                message: format!(
                                    "Map key type mismatch: expected {}, found {}",
                                    format_type(expected_key_type),
                                    format_type(&index_type)
                                ),
                                span: index.span,
                            });
                        }
                    }
                    Type::EmptyMap => {
                        return Err(SemanticError::with_help(
                            "Cannot index empty map",
                            expr.span,
                            "The map type is unknown. Provide type annotations or add entries to the map literal.",
                        ));
                    }
                    _ => {
                        return Err(SemanticError::with_help(
                            "Cannot index non-list type",
                            expr.span,
                            "Only lists and maps can be indexed with '[]'. Examples: my_list[0], my_map['key']",
                        ));
                    }
                }
                Ok(())
            }
            ExpressionKind::ListLiteral(elements) => {
                for elem in elements {
                    self.analyze_expression(elem)?;
                }
                // type check list literal, ensure all elements have the same type
                if !elements.is_empty() {
                    let first_type = self.get_expression_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let elem_type = self.get_expression_type(elem)?;
                        if elem_type != first_type {
                            return Err(SemanticError {
                                message: "All elements in list literal must have the same type"
                                    .into(),
                                span: elem.span,
                            });
                        }
                    }
                }
                Ok(())
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                for (key, value) in entries {
                    self.analyze_expression(key)?;
                    self.analyze_expression(value)?;
                }
                // type check map literal, ensure all keys and values have consistent types
                // also validate that keys are hashable (primitive types only for now)
                if !entries.is_empty() {
                    let (first_key, first_value) = &entries[0];
                    let key_type = self.get_expression_type(first_key)?;

                    // Check if key type is hashable (primitives only for now)
                    let is_hashable = matches!(key_type, Type::Primitive(_));
                    if !is_hashable {
                        return Err(SemanticError {
                            message: format!(
                                "Map keys must be hashable (primitive types only). Found '{}'",
                                format_type(&key_type)
                            ),
                            span: first_key.span,
                        });
                    }

                    let value_type = self.get_expression_type(first_value)?;
                    for (key, value) in &entries[1..] {
                        let k_type = self.get_expression_type(key)?;

                        // Check key type is hashable
                        let is_hashable = matches!(k_type, Type::Primitive(_));
                        if !is_hashable {
                            return Err(SemanticError {
                                message: format!(
                                    "Map keys must be hashable (primitive types only). Found '{}'",
                                    format_type(&k_type)
                                ),
                                span: key.span,
                            });
                        }

                        let v_type = self.get_expression_type(value)?;
                        if k_type != key_type {
                            return Err(SemanticError {
                                message: "All keys in map literal must have the same type".into(),
                                span: key.span,
                            });
                        }
                        if v_type != value_type {
                            return Err(SemanticError {
                                message: "All values in map literal must have the same type".into(),
                                span: value.span,
                            });
                        }
                    }
                }
                Ok(())
            }
            ExpressionKind::SetLiteral(elements) => {
                for elem in elements {
                    self.analyze_expression(elem)?;
                }
                // type check set literal, ensure all elements have the same type
                if !elements.is_empty() {
                    let first_type = self.get_expression_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let elem_type = self.get_expression_type(elem)?;
                        if elem_type != first_type {
                            return Err(SemanticError {
                                message: "All elements in set literal must have the same type"
                                    .into(),
                                span: elem.span,
                            });
                        }
                    }
                }
                Ok(())
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                self.analyze_expression(cond)?;
                self.analyze_expression(then_expr)?;
                self.analyze_expression(else_expr)?;
                // type check if expression
                let cond_type = self.get_expression_type(cond)?;
                if !matches!(cond_type, Type::Primitive(crate::ast::PrimitiveType::Bool)) {
                    return Err(SemanticError {
                        message: "If condition must be boolean".into(),
                        span: cond.span,
                    });
                }
                Ok(())
            }
            ExpressionKind::Lambda {
                params,
                return_type,
                body,
            } => {
                // Collect parameter names to identify what's local vs captured
                let mut local_vars = std::collections::HashSet::new();
                for param in params {
                    local_vars.insert(param.name.clone());
                }

                self.symbol_table.push_scope()?;

                // Set up return type context for lambda
                let lambda_return_type = self.resolve_type(return_type)?;
                let prev_return_type = self.current_return_type.clone();

                self.current_return_type = Some(lambda_return_type.clone());

                for param in params {
                    let param_type = self.resolve_type(&param.type_)?;
                    self.symbol_table.add_symbol(
                        &param.name,
                        Symbol {
                            kind: SymbolKind::Variable,
                            span: param.type_.span,
                            type_: Some(param_type),
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    )?;
                }

                self.analyze_block(body, None)?;

                // Validate that the body returns the correct type
                if !matches!(self.current_return_type, Some(Type::Void)) {
                    // Find the last statement to check if it's a return
                    let mut found_return = false;
                    if let Some(last_stmt) = body.last() {
                        if let StatementKind::Return(Some(ret_expr)) = &last_stmt.kind {
                            let actual_type = self.get_expression_type(ret_expr)?;
                            self.check_type_compatibility(
                                &lambda_return_type,
                                &actual_type,
                                ret_expr.span,
                            )?;
                            found_return = true;
                        } else if let StatementKind::Return(None) = &last_stmt.kind {
                            found_return = true;
                        }
                    }

                    if !found_return && !body.is_empty() {
                        return Err(SemanticError {
                            message: format!(
                                "Lambda must return a value of type '{}'",
                                format_type(&lambda_return_type)
                            ),
                            span: expr.span,
                        });
                    }
                }

                // Restore previous return type context
                self.current_return_type = prev_return_type;

                // Detect free variables (captured variables)
                let captures = self.find_free_variables_in_block(body, &local_vars)?;
                // Store captures for this lambda using its span as key
                self.lambda_captures.insert(expr.span, captures);

                self.symbol_table.pop_scope()?;
                Ok(())
            }
            // Instantiate generic types (e.g., Stack<int>)
            ExpressionKind::GenericType(name, _) => {
                if !self.symbol_table.exists(name) {
                    return Err(SemanticError {
                        message: format!("Undefined type '{}'", name),
                        span: expr.span,
                    });
                }
                Ok(())
            }
        }
    }

    fn infer_literal_type(&self, expr: &ExpressionNode) -> Result<Type, SemanticError> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => match lit {
                LiteralNode::Integer(_) => Ok(Type::Primitive(PrimitiveType::Int)),
                LiteralNode::Float(_) => Ok(Type::Primitive(PrimitiveType::Float)),
                LiteralNode::String(_) => Ok(Type::Primitive(PrimitiveType::Str)),
                LiteralNode::Boolean(_) => Ok(Type::Primitive(PrimitiveType::Bool)),
                LiteralNode::Char(_) => Ok(Type::Primitive(PrimitiveType::Char)),
            },
            _ => Err(SemanticError {
                message: "Expected literal expression".to_string(),
                span: expr.span,
            }),
        }
    }

    fn types_compatible(&self, type1: &Type, type2: &Type) -> bool {
        // Handle type variables - same name means compatible
        match (type1, type2) {
            (Type::Variable(v1), Type::Variable(v2)) => v1 == v2,
            (Type::Generic(g1), Type::Generic(g2)) => g1 == g2,
            _ => type1 == type2,
        }
    }

    // Add module as namespace (import logger as log)
    fn add_module_namespace(
        &mut self,
        namespace: &str,
        symbols: std::collections::HashMap<String, Symbol>,
        module_path: &str,
        span: Span,
    ) -> Result<(), SemanticError> {
        // Mangle function names in the symbols before storing
        let module_name_for_mangling = module_path.replace(['.', '/'], "_");
        let mut mangled_symbols = std::collections::HashMap::new();

        for (name, symbol) in symbols {
            let mut mangled_symbol = symbol.clone();
            // Set llvm_name for functions
            if matches!(symbol.kind, SymbolKind::Function) {
                mangled_symbol.llvm_name = Some(format!("{}!{}", module_name_for_mangling, name));
            }
            mangled_symbols.insert(name, mangled_symbol);
        }

        // Store module symbols for namespaced access
        self.imported_symbols
            .insert(namespace.to_string(), mangled_symbols);

        // Add module symbol to symbol table
        self.symbol_table.add_symbol(
            namespace,
            Symbol {
                kind: SymbolKind::Import,
                span,
                type_: Some(Type::Module(namespace.to_string())),
                interfaces: std::collections::HashMap::new(),
                methods: std::collections::HashMap::new(),
                fields: std::collections::HashMap::new(),
                type_params: Vec::new(),
                original_name: None,
                llvm_name: None,
                default_param_count: 0,
                variants: None,
            },
        )?;

        Ok(())
    }

    // Import single symbol (import logger.log)
    fn import_single_symbol(
        &mut self,
        module_symbols: &std::collections::HashMap<String, Symbol>,
        item_name: &str,
        local_name: &str,
        module_path: &str,
        span: Span,
    ) -> Result<(), SemanticError> {
        let symbol = module_symbols.get(item_name).ok_or_else(|| SemanticError {
            message: format!("Symbol '{}' not found in module", item_name),
            span,
        })?;

        // Clone the symbol and set original_name/llvm_name if needed
        let mut imported_symbol = symbol.clone();

        // Set original_name if it's an alias
        if item_name != local_name {
            imported_symbol.original_name = Some(item_name.to_string());
        }

        // Set llvm_name for functions (mangled with module path)
        if matches!(symbol.kind, SymbolKind::Function) {
            let module_name_for_mangling = module_path.replace(['.', '/'], "_");
            imported_symbol.llvm_name = Some(format!("{}!{}", module_name_for_mangling, item_name));
        }

        self.symbol_table.add_symbol(local_name, imported_symbol)?;
        Ok(())
    }

    // Import all symbols (import logger.*)
    fn import_all_symbols(
        &mut self,
        module_symbols: &std::collections::HashMap<String, Symbol>,
        module_path: &str,
        _span: Span,
    ) -> Result<(), SemanticError> {
        let module_name_for_mangling = module_path.replace(['.', '/'], "_");

        for (name, symbol) in module_symbols {
            // Import all symbols directly into current namespace
            // Skip if already exists in current scope to avoid conflicts
            if self.symbol_table.get_cloned(name).is_none() {
                let mut imported_symbol = symbol.clone();

                // Set llvm_name for functions (mangled with module path)
                if matches!(symbol.kind, SymbolKind::Function) {
                    imported_symbol.llvm_name =
                        Some(format!("{}!{}", module_name_for_mangling, name));
                }

                // Try to add, but ignore duplicate errors since we already checked
                let _ = self.symbol_table.add_symbol(name, imported_symbol);
            }
        }
        Ok(())
    }

    // Handle std library imports
    fn handle_std_import(
        &mut self,
        module_path: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;

        match spec {
            ImportSpec::Module { alias } => {
                let symbol_name = alias.as_ref().map(|s| s.as_str()).unwrap_or_else(|| {
                    module_path
                        .split('.')
                        .last()
                        .expect("module path should have at least one component")
                });

                if let Some(sig) = self.get_builtin_sig(symbol_name) {
                    self.symbol_table.add_symbol(
                        symbol_name,
                        Symbol {
                            kind: SymbolKind::Function,
                            span,
                            type_: Some(Type::Function {
                                params: sig.params.clone(),
                                returns: Box::new(sig.return_type.clone()),
                                default_count: 0,
                            }),
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    )?;
                } else if symbol_name == "None" {
                    self.symbol_table.add_symbol(
                        symbol_name,
                        Symbol {
                            kind: SymbolKind::Constant,
                            span,
                            type_: Some(Type::Optional(Box::new(Type::Void))),
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    )?;
                }
            }
            ImportSpec::Item { item, alias } => {
                // Support selective std imports: import std.io.print
                let symbol_name = alias.as_ref().unwrap_or(item);
                if let Some(sig) = self.get_builtin_sig(item) {
                    self.symbol_table.add_symbol(
                        symbol_name,
                        Symbol {
                            kind: SymbolKind::Function,
                            span,
                            type_: Some(Type::Function {
                                params: sig.params.clone(),
                                returns: Box::new(sig.return_type.clone()),
                                default_count: 0,
                            }),
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                            original_name: None,
                            llvm_name: None,
                            default_param_count: 0,
                            variants: None,
                        },
                    )?;
                }
            }
            _ => {
                // Items and Wildcard can be supported similarly if needed
            }
        }
        Ok(())
    }

    // Helper to find free variables in a block of statements
    // Returns variables that are used but not declared in the local scope
    fn find_free_variables_in_block(
        &self,
        body: &[StatementNode],
        local_vars: &std::collections::HashSet<String>,
    ) -> Result<Vec<(String, Type)>, SemanticError> {
        let mut free_vars = std::collections::HashMap::new();
        let mut local_vars = local_vars.clone();

        for stmt in body {
            self.find_free_variables_in_statement(stmt, &mut local_vars, &mut free_vars)?;
        }

        Ok(free_vars.into_iter().collect())
    }

    fn find_free_variables_in_statement(
        &self,
        stmt: &StatementNode,
        local_vars: &mut std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        match &stmt.kind {
            StatementKind::Expression(expr) | StatementKind::Return(Some(expr)) => {
                self.find_free_variables_in_expression(expr, local_vars, free_vars)?;
            }
            StatementKind::AutoDecl(name, _, expr) => {
                // First analyze the expression (uses happen before the decl is in scope)
                self.find_free_variables_in_expression(expr, local_vars, free_vars)?;
                // Then add the variable to local scope
                local_vars.insert(name.clone());
            }
            StatementKind::TypedDecl(name, _, expr) => {
                self.find_free_variables_in_expression(expr, local_vars, free_vars)?;
                local_vars.insert(name.clone());
            }
            StatementKind::ConstDecl(name, _, expr) => {
                self.find_free_variables_in_expression(expr, local_vars, free_vars)?;
                local_vars.insert(name.clone());
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.find_free_variables_in_expression(cond, local_vars, free_vars)?;
                for s in then_block {
                    self.find_free_variables_in_statement(s, local_vars, free_vars)?;
                }
                if let Some(else_stmts) = else_block {
                    for s in else_stmts {
                        self.find_free_variables_in_statement(s, local_vars, free_vars)?;
                    }
                }
            }
            StatementKind::While { cond, body } => {
                self.find_free_variables_in_expression(cond, local_vars, free_vars)?;
                for s in body {
                    self.find_free_variables_in_statement(s, local_vars, free_vars)?;
                }
            }
            StatementKind::For {
                var, iter, body, ..
            } => {
                self.find_free_variables_in_expression(iter, local_vars, free_vars)?;
                // Iterator variable is local to the for loop
                local_vars.insert(var.clone());
                for s in body {
                    self.find_free_variables_in_statement(s, local_vars, free_vars)?;
                }
            }
            StatementKind::Block(stmts) => {
                for s in stmts {
                    self.find_free_variables_in_statement(s, local_vars, free_vars)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn find_free_variables_in_expression(
        &self,
        expr: &ExpressionNode,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                // Check if it's a local variable (parameter or declared in lambda body)
                if !local_vars.contains(name) {
                    // Check if it exists in outer scopes
                    if let Some(symbol) = self.symbol_table.lookup(name) {
                        if matches!(symbol.kind, SymbolKind::Variable) {
                            if let Some(var_type) = &symbol.type_ {
                                free_vars.insert(name.clone(), var_type.clone());
                            }
                        }
                    }
                }
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.find_free_variables_in_expression(left, local_vars, free_vars)?;
                self.find_free_variables_in_expression(right, local_vars, free_vars)?;
            }
            ExpressionKind::Unary {
                expr: inner,
                op_span: _,
                ..
            } => {
                self.find_free_variables_in_expression(inner, local_vars, free_vars)?;
            }
            ExpressionKind::Call { func, args } => {
                self.find_free_variables_in_expression(func, local_vars, free_vars)?;
                for arg in args {
                    self.find_free_variables_in_expression(arg, local_vars, free_vars)?;
                }
            }
            ExpressionKind::FieldAccess { expr: inner, .. } => {
                self.find_free_variables_in_expression(inner, local_vars, free_vars)?;
            }
            ExpressionKind::ListAccess { expr: inner, index } => {
                self.find_free_variables_in_expression(inner, local_vars, free_vars)?;
                self.find_free_variables_in_expression(index, local_vars, free_vars)?;
            }
            ExpressionKind::ListLiteral(elements) => {
                for elem in elements {
                    self.find_free_variables_in_expression(elem, local_vars, free_vars)?;
                }
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                for (key, val) in entries {
                    self.find_free_variables_in_expression(key, local_vars, free_vars)?;
                    self.find_free_variables_in_expression(val, local_vars, free_vars)?;
                }
            }
            ExpressionKind::SetLiteral(elements) => {
                for elem in elements {
                    self.find_free_variables_in_expression(elem, local_vars, free_vars)?;
                }
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                self.find_free_variables_in_expression(cond, local_vars, free_vars)?;
                self.find_free_variables_in_expression(then_expr, local_vars, free_vars)?;
                self.find_free_variables_in_expression(else_expr, local_vars, free_vars)?;
            }
            ExpressionKind::Lambda { params, body, .. } => {
                // For nested lambdas, we need to find free variables that escape to the outer scope
                // The lambda's own parameters are local to it, so create a new local_vars set
                let mut inner_local_vars = local_vars.clone();
                for param in params {
                    inner_local_vars.insert(param.name.clone());
                }
                // Recursively find free variables in the nested lambda's body
                // Any variables found that aren't in our local_vars will be captured by us too
                for stmt in body {
                    self.find_free_variables_in_statement(stmt, &mut inner_local_vars, free_vars)?;
                }
            }
            // Literals and other expressions don't have free variables
            _ => {}
        }
        Ok(())
    }
}
