use crate::lexer::Span;
use crate::parser::*;
use crate::parser::PrimitiveType;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub span: Span,
    pub type_: Option<Type>,
    pub interfaces: Vec<String>, // Interfaces implemented by this symbol (for classes)
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Function,
    Variable,
    Class,
    Interface,
    Enum,
    Constant,
    Import,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(crate::parser::PrimitiveType),
    Named(String, Vec<Type>),
    Function {
        params: Vec<Type>,
        returns: Box<Type>,
    },
    Reference(Box<Type>),
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Tuple(Vec<Type>),
    Void,
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Rc<RefCell<Scope>>>,
}

#[derive(Debug, Default)]
struct Scope {
    symbols: HashMap<String, Symbol>,
    children: Vec<Rc<RefCell<Scope>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let root = Rc::new(RefCell::new(Scope::default()));
        SymbolTable {
            scopes: vec![root],
        }
    }

    pub fn print(&self) {
        println!("Symbol Table:");
        self.print_scope(&self.scopes[0], 0);
    }

    fn print_scope(&self, scope: &Rc<RefCell<Scope>>, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{}Scope {}:", indent, depth);

        let scope_borrow = scope.borrow();
        for (name, symbol) in &scope_borrow.symbols {
            let type_str = match &symbol.type_ {
                Some(t) => format!("{:?}", t),
                None => "unresolved".to_string(),
            };
            println!("{}  {}: {} ({:?})", indent, name, type_str, symbol.kind);
        }

        for child in &scope_borrow.children {
            self.print_scope(child, depth + 1);
        }
    }

    pub fn push_scope(&mut self) -> Result<(), SemanticError> {
        let new_scope = Rc::new(RefCell::new(Scope::default()));
        self.scopes
            .last()
            .unwrap()
            .borrow_mut()
            .children
            .push(Rc::clone(&new_scope));
        self.scopes.push(new_scope);
        Ok(())
    }

    pub fn pop_scope(&mut self) -> Result<(), SemanticError> {
        if self.scopes.len() <= 1 {
            return Err(SemanticError {
                message: "cannot pop the global scope".into(),
                span: Span::new(0, 0),
            });
        }
        self.scopes.pop();
        Ok(())
    }

    pub fn exists(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }



    pub fn add_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        if self.scopes.is_empty() {
            return Err(SemanticError {
                message: "no active scope".into(),
                span: Span::new(0, 0),
            });
        }

        let current = self.scopes.last().unwrap();
        let mut current_borrow = current.borrow_mut();

        if current_borrow.symbols.contains_key(name) {
            return Err(SemanticError {
                message: format!("duplicate declaration of '{}'", name),
                span: symbol.span,
            });
        }

        current_borrow.symbols.insert(name.to_string(), symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            let scope_borrow = scope.borrow();
            if let Some(symbol) = scope_borrow.symbols.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }
}

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    // get reference to the symbol table for debugging.
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn analyze(&mut self, ast: &[AstNode]) -> Vec<SemanticError> {
        if let Err(e) = self.collect_hoistable_declarations(ast) {
            self.errors.push(e);
        }
        self.analyze_nodes(ast);
        std::mem::take(&mut self.errors)
    }

    // Resolve a parsed TypeNode to a resolved Type
    fn resolve_type(&self, type_node: &TypeNode) -> Result<Type, SemanticError> {
        match &type_node.kind {
            TypeKind::Primitive(prim) => match prim {
                crate::parser::PrimitiveType::Int => Ok(Type::Primitive(crate::parser::PrimitiveType::Int)),
                crate::parser::PrimitiveType::Float => Ok(Type::Primitive(crate::parser::PrimitiveType::Float)),
                crate::parser::PrimitiveType::Bool => Ok(Type::Primitive(crate::parser::PrimitiveType::Bool)),
                crate::parser::PrimitiveType::Char => Ok(Type::Primitive(crate::parser::PrimitiveType::Char)),
                crate::parser::PrimitiveType::Str => Ok(Type::Primitive(crate::parser::PrimitiveType::Str)),
                crate::parser::PrimitiveType::Void => Ok(Type::Void),
                crate::parser::PrimitiveType::Auto => Err(SemanticError {
                    message: "'auto' type not allowed in this context".into(),
                    span: type_node.span,
                }),
            },
            TypeKind::Named(name, type_args) => {
                // For now, assume named types are classes/enums/interfaces
                // TODO: Resolve actual type definitions
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
            TypeKind::Tuple(elements) => {
                let resolved_elements = elements
                    .iter()
                    .map(|e| self.resolve_type(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Tuple(resolved_elements))
            }
            TypeKind::TraitObject(_) => Err(SemanticError {
                message: "Trait objects not yet supported".into(),
                span: type_node.span,
            }),
            TypeKind::Auto => Err(SemanticError {
                message: "'auto' type not allowed in this context".into(),
                span: type_node.span,
            }),
        }
    }

    // Get the type of an expression
    fn get_expression_type(&self, expr: &ExpressionNode) -> Result<Type, SemanticError> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => match lit {
                LiteralNode::Integer(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Int)),
                LiteralNode::Float(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Float)),
                LiteralNode::String(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Str)),
                LiteralNode::Boolean(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Bool)),
                LiteralNode::Char(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Char)),
            },
            ExpressionKind::Identifier(name) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    symbol.type_.clone().ok_or_else(|| SemanticError {
                        message: format!("Symbol '{}' has no type information", name),
                        span: expr.span,
                    })
                } else {
                    Err(SemanticError {
                        message: format!("Undefined variable '{}'", name),
                        span: expr.span,
                    })
                }
            }
            ExpressionKind::Binary { left, right, op } => {
                let left_type = self.get_expression_type(left)?;
                let right_type = self.get_expression_type(right)?;

                if let Some(result_type) = self.resolve_binary_operator(&left_type, &right_type, op) {
                    Ok(result_type)
                } else {
                    Err(SemanticError {
                        message: format!("Binary operator {:?} not supported for types {:?} and {:?}", op, left_type, right_type),
                        span: expr.span,
                    })
                }
            }
            ExpressionKind::Unary { expr, op, .. } => {
                match op {
                    UnaryOp::Not => Ok(Type::Primitive(crate::parser::PrimitiveType::Bool)),
                    UnaryOp::Neg => {
                        let operand_type = self.get_expression_type(expr)?;
                        match operand_type {
                            Type::Primitive(crate::parser::PrimitiveType::Int) | Type::Primitive(crate::parser::PrimitiveType::Float) => Ok(operand_type),
                            _ => Err(SemanticError {
                                message: "Negation operator requires numeric operand".into(),
                                span: expr.span,
                            }),
                        }
                    }
                    _ => Err(SemanticError {
                        message: format!("Unsupported unary operator {:?}", op),
                        span: expr.span,
                    }),
                }
            }
            ExpressionKind::Call { func, args: _ } => {
                // Get function type and return its return type
                let func_type = self.get_expression_type(func)?;
                match func_type {
                    Type::Function { returns, .. } => Ok(*returns),
                    _ => Err(SemanticError {
                        message: "Cannot call non-function type".into(),
                        span: func.span,
                    }),
                }
            }
            ExpressionKind::FieldAccess { expr, field: _ } => {
                // TODO: Resolve field type from struct/class type
                let _ = self.get_expression_type(expr)?;
                Err(SemanticError {
                    message: "Field access type checking not yet implemented".into(),
                    span: expr.span,
                })
            }
            ExpressionKind::ListAccess { expr, index: _ } => {
                // Assume list access returns element type
                let list_type = self.get_expression_type(expr)?;
                match list_type {
                    Type::List(elem_type) => Ok(*elem_type),
                    _ => Err(SemanticError {
                        message: "Cannot index non-list type".into(),
                        span: expr.span,
                    }),
                }
            }
            ExpressionKind::ListLiteral(elements) => {
                if elements.is_empty() {
                    // Empty list - could be any type, but for now require explicit typing
                    Err(SemanticError {
                        message: "Empty list literals need explicit type annotation".into(),
                        span: expr.span,
                    })
                } else {
                    // Assume all elements have the same type
                    let elem_type = self.get_expression_type(&elements[0])?;
                    Ok(Type::List(Box::new(elem_type)))
                }
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                if entries.is_empty() {
                    Err(SemanticError {
                        message: "Empty map literals need explicit type annotation".into(),
                        span: expr.span,
                    })
                } else {
                    let (key, value) = &entries[0];
                    let key_type = self.get_expression_type(key)?;
                    let value_type = self.get_expression_type(value)?;
                    Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
                }
            }
             ExpressionKind::If { then_expr, else_expr, .. } => {
                 let then_type = self.get_expression_type(then_expr)?;
                 let else_type = self.get_expression_type(else_expr)?;
                 if then_type == else_type {
                     Ok(then_type)
                 } else {
                     Err(SemanticError {
                         message: "If expression branches must have the same type".into(),
                         span: expr.span,
                     })
                 }
             }
             ExpressionKind::Lambda { params: _, body } => {
                // Lambda return type is the type of the last expression in body
                // For now, assume void if body is empty
                if body.is_empty() {
                    Ok(Type::Void)
                } else {
                    // Get type of last statement (assuming it's an expression statement)
                    match &body.last().unwrap().kind {
                        StatementKind::Expression(expr) => self.get_expression_type(expr),
                        _ => Ok(Type::Void), // Non-expression statements don't return values
                    }
                }
            }
            ExpressionKind::SetLiteral(elements) => {
                if elements.is_empty() {
                    Err(SemanticError {
                        message: "Empty set literals need explicit type annotation".into(),
                        span: expr.span,
                    })
                } else {
                    let elem_type = self.get_expression_type(&elements[0])?;
                    Ok(Type::Set(Box::new(elem_type)))
                }
            }
            ExpressionKind::GenericType(_, _) => {
                Err(SemanticError {
                    message: "Generic type expressions not yet supported".into(),
                    span: expr.span,
                })
            }
        }
    }

    // Check if two types are compatible for assignment
    fn check_type_compatibility(&self, expected: &Type, actual: &Type, span: Span) -> Result<(), SemanticError> {
        if expected == actual {
            Ok(())
        } else {
            Err(SemanticError {
                message: format!("Type mismatch: expected {:?}, got {:?}", expected, actual),
                span,
            })
        }
    }

    // Check if a type implements a specific operator interface
    fn type_implements_interface(&self, type_: &Type, interface_name: &str) -> bool {
        match type_ {
            Type::Named(name, _) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    symbol.interfaces.contains(&interface_name.to_string())
                } else {
                    false
                }
            }
            Type::Primitive(prim) => {
                // Built-in types implement certain interfaces
                match prim {
                    PrimitiveType::Str => interface_name == "Add",
                    PrimitiveType::Int => matches!(interface_name, "Add" | "Sub" | "Mul" | "Div" | "Arithmetic"),
                    PrimitiveType::Float => matches!(interface_name, "Add" | "Sub" | "Mul" | "Div" | "Arithmetic"),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    // Resolve binary operator for given operand types
    fn resolve_binary_operator(&self, left_type: &Type, right_type: &Type, op: &BinaryOp) -> Option<Type> {
        // Types must be compatible
        if left_type != right_type {
            return None;
        }

        match op {
            BinaryOp::Add => {
                // Built-in support for primitives, or interface support for custom types
                if matches!(left_type, Type::Primitive(PrimitiveType::Str | PrimitiveType::Int | PrimitiveType::Float)) ||
                   self.type_implements_interface(left_type, "Add") {
                    Some(left_type.clone())
                } else {
                    None
                }
            }
            BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
                // Built-in support for numeric primitives, or interface support for custom types
                if matches!(left_type, Type::Primitive(PrimitiveType::Int | PrimitiveType::Float)) ||
                   self.type_implements_interface(left_type, "Arithmetic") {
                    Some(left_type.clone())
                } else {
                    None
                }
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                // Equality is supported for all types
                Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
            }
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                // Comparison operators for ordered types
                if matches!(left_type, Type::Primitive(PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Str)) {
                    Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
                } else {
                    None
                }
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                if matches!(left_type, Type::Primitive(PrimitiveType::Bool)) {
                    Some(Type::Primitive(PrimitiveType::Bool))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // first pass, collect hoistable declarations like functions and classes.
    fn collect_hoistable_declarations(&mut self, ast: &[AstNode]) -> Result<(), SemanticError> {
        for node in ast {
            match node {
                AstNode::Function(func) => {
                    // Resolve function type
                    let param_types = func.params.iter()
                        .map(|p| self.resolve_type(&p.type_))
                        .collect::<Result<Vec<_>, _>>()?;
                    let return_type = self.resolve_type(&func.return_type)?;
                    let func_type = Type::Function {
                        params: param_types,
                        returns: Box::new(return_type),
                    };

                    if let Err(e) = self.symbol_table.add_symbol(
                        &func.name,
                        Symbol {
                            kind: SymbolKind::Function,
                            span: func.span,
                            type_: Some(func_type),
                            interfaces: Vec::new(),
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Class { name, traits, .. } => {
                    let mut interfaces = Vec::new();
                    for trait_ref in traits {
                        interfaces.push(trait_ref.name.clone());
                    }
                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Class,
                            span: *node.span(),
                            type_: None,
                            interfaces,
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Enum { name, .. } => {
                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Enum,
                            span: *node.span(),
                            type_: None,
                            interfaces: Vec::new(),
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Interface { name, .. } => {
                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Interface,
                            span: *node.span(),
                            type_: None,
                            interfaces: Vec::new(),
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
    fn analyze_nodes(&mut self, nodes: &[AstNode]) {
        for node in nodes {
            if let Err(e) = self.analyze_node(node) {
                self.errors.push(e);
            }
        }
    }

    fn analyze_node(&mut self, node: &AstNode) -> Result<(), SemanticError> {
        match node {
            AstNode::Function(func) => self.analyze_function(func),
            AstNode::Class { name, traits, fields, methods, .. } => self.analyze_class(name, traits, fields, methods),
            AstNode::Enum { .. } => Ok(()), // enums don't need further analysis.
            AstNode::Interface { .. } => Ok(()), // interfaces don't need further analysis.
            AstNode::Statement(stmt) => self.analyze_statement(stmt),
        }
    }

    fn analyze_function(&mut self, func: &FunctionNode) -> Result<(), SemanticError> {
        // create new scope for function parameters and body.
        self.symbol_table.push_scope()?;

        // add parameters to function scope.
        for param in &func.params {
            let param_type = self.resolve_type(&param.type_)?;
            self.symbol_table.add_symbol(
                &param.name,
                Symbol {
                    kind: SymbolKind::Variable,
                    span: param.type_.span,
                    type_: Some(param_type),
                    interfaces: Vec::new(),
                },
            )?;
        }

        // analyze function body with new scope.
        let result = self.analyze_block(&func.body);

        // clean up function scope.
        self.symbol_table.pop_scope()?;
        result
    }

    fn analyze_class(&mut self, _name: &str, _traits: &[crate::parser::TraitRef], fields: &[Field], methods: &[FunctionNode]) -> Result<(), SemanticError> {
        // create new scope for class members.
        self.symbol_table.push_scope()?;

        // add fields to class scope.
        for field in fields {
            let field_type = self.resolve_type(&field.type_)?;
            self.symbol_table.add_symbol(
                &field.name,
                Symbol {
                    kind: SymbolKind::Variable,
                    span: field.type_.span,
                    type_: Some(field_type),
                    interfaces: Vec::new(),
                },
            )?;
        }

        // add methods to class scope and analyze their bodies.
        for method in methods {
            // Resolve method type
            let param_types = method.params.iter()
                .map(|p| self.resolve_type(&p.type_))
                .collect::<Result<Vec<_>, _>>()?;
            let return_type = self.resolve_type(&method.return_type)?;
            let method_type = Type::Function {
                params: param_types,
                returns: Box::new(return_type),
            };

            self.symbol_table.add_symbol(
                &method.name,
                Symbol {
                    kind: SymbolKind::Function,
                    span: method.span,
                    type_: Some(method_type),
                    interfaces: Vec::new(),
                },
            )?;
            self.analyze_function(method)?;
        }

        // clean up class scope.
        self.symbol_table.pop_scope()?;
        Ok(())
    }

    fn analyze_block(&mut self, stmts: &[StatementNode]) -> Result<(), SemanticError> {
        // first collect function declarations in this block.
        for stmt in stmts {
            if let StatementKind::Function(func) = &stmt.kind {
                self.symbol_table.add_symbol(
                    &func.name,
                    Symbol {
                        kind: SymbolKind::Function,
                        span: stmt.span,
                        type_: None, // Will be resolved later
                        interfaces: Vec::new(),
                    },
                )?;
            }
        }

        // then analyze all statements in order.
        for stmt in stmts {
            self.analyze_statement(stmt)?;
        }

        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &StatementNode) -> Result<(), SemanticError> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, expr) => {
                let expr_type = self.get_expression_type(expr)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                        type_: Some(expr_type),
                        interfaces: Vec::new(),
                    },
                )?;
            }
            StatementKind::TypedDecl(name, type_node, expr) => {
                let declared_type = self.resolve_type(type_node)?;
                let expr_type = self.get_expression_type(expr)?;
                self.check_type_compatibility(&declared_type, &expr_type, expr.span)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                        type_: Some(declared_type),
                        interfaces: Vec::new(),
                    },
                )?;
            }
            StatementKind::Expression(expr) => {
                self.analyze_expression(expr)?;
            }
            StatementKind::Block(stmts) => self.analyze_block(stmts)?,
            StatementKind::ConstDecl(name, type_node, expr) => {
                let declared_type = self.resolve_type(type_node)?;
                let expr_type = self.get_expression_type(expr)?;
                self.check_type_compatibility(&declared_type, &expr_type, expr.span)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Constant,
                        span: stmt.span,
                        type_: Some(declared_type),
                        interfaces: Vec::new(),
                    },
                )?;
            }
            StatementKind::If { cond, then_block, else_block } => {
                self.analyze_expression(cond)?;
                self.analyze_block(then_block)?;
                if let Some(else_block) = else_block {
                    self.analyze_block(else_block)?;
                }
            }
            StatementKind::For { var, iter, body, .. } => {
                self.symbol_table.push_scope()?;
                self.analyze_expression(iter)?;
                let iter_type = self.get_expression_type(iter)?;
                // For now, assume iterator yields the element type directly
                // TODO: Handle proper iterator types
                let var_type = match iter_type {
                    Type::List(elem_type) => *elem_type,
                    _ => return Err(SemanticError {
                        message: "Cannot iterate over non-list type".into(),
                        span: iter.span,
                    }),
                };
                self.symbol_table.add_symbol(
                    var,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                        type_: Some(var_type),
                        interfaces: Vec::new(),
                    },
                )?;
                self.analyze_block(body)?;
                self.symbol_table.pop_scope()?;
            }
            StatementKind::While { cond, body } => {
                self.analyze_expression(cond)?;
                self.analyze_block(body)?;
            }
            StatementKind::Match { expr, arms } => {
                self.analyze_expression(expr)?;
                for arm in arms {
                    self.symbol_table.push_scope()?;
                    self.analyze_pattern(&arm.pattern)?;
                    if let Some(guard) = &arm.guard {
                        self.analyze_expression(guard)?;
                    }
                    self.analyze_block(&arm.body)?;
                    self.symbol_table.pop_scope()?;
                }
            }
            StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.analyze_expression(expr)?;
                }
            }
            StatementKind::Import { module_path, alias } => {
                // in a real implementation, we would resolve the module path, analyze the imported module, and add its symbols to the current scope.
                // for now, we'll just track the import for later resolution.

                let symbol_name = if let Some(alias) = alias {
                    alias
                } else {
                    // if no alias, use the last component of the module path as the symbol name.
                    module_path.split('.').last().unwrap_or(module_path)
                };

                // TODO: implement proper stdlib resolution instead of faking imports.
                // fake stdlib: if importing from std, treat as predefined symbol.
                let kind = if module_path.starts_with("std.") {
                    SymbolKind::Variable // treat as variables/constants for simplicity.
                } else {
                    SymbolKind::Import
                };

                self.symbol_table.add_symbol(
                    symbol_name,
                    Symbol {
                        kind,
                        span: stmt.span,
                        type_: None, // Imports don't have resolved types yet
                        interfaces: Vec::new(),
                    },
                )?;
            }
            _ => {} // handle other statement types
        }
        Ok(())
    }
    // analyze a pattern (for match arms).
    fn analyze_pattern(&mut self, pattern: &PatternNode) -> Result<(), SemanticError> {
        match pattern {
            PatternNode::Identifier(name) => {
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: Span::new(0, 0), // dummy span
                        type_: None, // Pattern variables get types from context
                        interfaces: Vec::new(),
                    },
                )?;
            }
            PatternNode::EnumVariant { args, .. } => {
                for arg in args {
                    self.analyze_pattern(arg)?;
                }
            }
            PatternNode::Literal(_) => {} // literals don't bind variables
            PatternNode::Wildcard => {} // no binding
            PatternNode::Tuple(elements) => {
                for elem in elements {
                    self.analyze_pattern(elem)?;
                }
            }
        }
        Ok(())
    }

    // analyze an expression.
    fn analyze_expression(&mut self, expr: &ExpressionNode) -> Result<(), SemanticError> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                if !self.symbol_table.exists(name) {
                    return Err(SemanticError {
                        message: format!("undefined variable '{}'", name),
                        span: expr.span,
                    });
                }
                // Type checking for identifier is handled in get_expression_type
            }
            ExpressionKind::Literal(_) => {} // literals are fine
             ExpressionKind::Binary { left, right, op: _ } => {
                 self.analyze_expression(left)?;
                 self.analyze_expression(right)?;
                 // Type checking is now handled in get_expression_type via resolve_binary_operator
             }
            ExpressionKind::Unary { expr, op, postfix: _ } => {
                let operand_type = self.get_expression_type(expr)?;
                match op {
                    UnaryOp::Not => {
                        if !matches!(operand_type, Type::Primitive(crate::parser::PrimitiveType::Bool)) {
                            return Err(SemanticError {
                                message: "Logical not operator requires boolean operand".into(),
                                span: expr.span,
                            });
                        }
                    }
                    UnaryOp::Neg => {
                        if !matches!(operand_type, Type::Primitive(crate::parser::PrimitiveType::Int) | Type::Primitive(crate::parser::PrimitiveType::Float)) {
                            return Err(SemanticError {
                                message: "Negation operator requires numeric operand".into(),
                                span: expr.span,
                            });
                        }
                    }
                    _ => {} // Other unary ops not fully implemented yet
                }
            }
            ExpressionKind::Call { func, args } => {
                self.analyze_expression(func)?;
                for arg in args {
                    self.analyze_expression(arg)?;
                }
                // Type check function call
                let func_type = self.get_expression_type(func)?;
                match func_type {
                    Type::Function { params, .. } => {
                        if params.len() != args.len() {
                            return Err(SemanticError {
                                message: format!("Function expects {} arguments, got {}", params.len(), args.len()),
                                span: expr.span,
                            });
                        }
                        for (i, (param_type, arg)) in params.iter().zip(args).enumerate() {
                            let arg_type = self.get_expression_type(arg)?;
                            if param_type != &arg_type {
                                return Err(SemanticError {
                                    message: format!("Argument {} type mismatch: expected {:?}, got {:?}", i + 1, param_type, arg_type),
                                    span: arg.span,
                                });
                            }
                        }
                    }
                    _ => return Err(SemanticError {
                        message: "Cannot call non-function".into(),
                        span: func.span,
                    }),
                }
            }
            ExpressionKind::FieldAccess { expr, .. } => {
                self.analyze_expression(expr)?;
            }
            ExpressionKind::ListAccess { expr, index } => {
                self.analyze_expression(expr)?;
                self.analyze_expression(index)?;
                // Type check list access
                let list_type = self.get_expression_type(expr)?;
                match list_type {
                    Type::List(_) => {}
                    _ => return Err(SemanticError {
                        message: "Cannot index non-list type".into(),
                        span: expr.span,
                    }),
                }
                let index_type = self.get_expression_type(index)?;
                if !matches!(index_type, Type::Primitive(crate::parser::PrimitiveType::Int)) {
                    return Err(SemanticError {
                        message: "List index must be an integer".into(),
                        span: index.span,
                    });
                }
            }
            ExpressionKind::ListLiteral(elements) => {
                for elem in elements {
                    self.analyze_expression(elem)?;
                }
                // Type check list literal - ensure all elements have the same type
                if !elements.is_empty() {
                    let first_type = self.get_expression_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let elem_type = self.get_expression_type(elem)?;
                        if elem_type != first_type {
                            return Err(SemanticError {
                                message: "All elements in list literal must have the same type".into(),
                                span: elem.span,
                            });
                        }
                    }
                }
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                for (key, value) in entries {
                    self.analyze_expression(key)?;
                    self.analyze_expression(value)?;
                }
                // Type check map literal - ensure all keys and values have consistent types
                if !entries.is_empty() {
                    let (first_key, first_value) = &entries[0];
                    let key_type = self.get_expression_type(first_key)?;
                    let value_type = self.get_expression_type(first_value)?;
                    for (key, value) in &entries[1..] {
                        let k_type = self.get_expression_type(key)?;
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
            }
            ExpressionKind::SetLiteral(elements) => {
                for elem in elements {
                    self.analyze_expression(elem)?;
                }
                // Type check set literal - ensure all elements have the same type
                if !elements.is_empty() {
                    let first_type = self.get_expression_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let elem_type = self.get_expression_type(elem)?;
                        if elem_type != first_type {
                            return Err(SemanticError {
                                message: "All elements in set literal must have the same type".into(),
                                span: elem.span,
                            });
                        }
                    }
                }
            }
            ExpressionKind::If { cond, then_expr, else_expr } => {
                self.analyze_expression(cond)?;
                self.analyze_expression(then_expr)?;
                self.analyze_expression(else_expr)?;
                // Type check if expression
                let cond_type = self.get_expression_type(cond)?;
                if !matches!(cond_type, Type::Primitive(crate::parser::PrimitiveType::Bool)) {
                    return Err(SemanticError {
                        message: "If condition must be boolean".into(),
                        span: cond.span,
                    });
                }
                // then_expr and else_expr types are checked in get_expression_type
            }
            ExpressionKind::Lambda { params, body } => {
                self.symbol_table.push_scope()?;
                for param in params {
                    let param_type = self.resolve_type(&param.type_)?;
                    self.symbol_table.add_symbol(
                        &param.name,
                        Symbol {
                            kind: SymbolKind::Variable,
                            span: param.type_.span,
                            type_: Some(param_type),
                            interfaces: Vec::new(),
                        },
                    )?;
                }
                self.analyze_block(body)?;
                self.symbol_table.pop_scope()?;
            }
            ExpressionKind::GenericType(name, _type_args) => {
                // check if the generic type name exists.
                if !self.symbol_table.exists(&name) {
                    return Err(SemanticError {
                        message: format!("undefined type '{}'", name),
                        span: expr.span,
                    });
                }
                // TODO: Handle generic type instantiation
            }
        }
        Ok(())
    }
}

// represents a semantic error with location information
#[derive(Debug)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "semantic error: {} at {:?}", self.message, self.span)
    }
}

impl std::error::Error for SemanticError {}
