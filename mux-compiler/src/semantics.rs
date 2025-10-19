use crate::lexer::Span;
use crate::parser::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

// represents a symbol in the symbol table
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub span: Span,
}

// different kinds of symbols we track
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

// manages nested scopes and symbol lookups
#[derive(Debug)]
pub struct SymbolTable {
    root: Rc<RefCell<Scope>>,
    scopes: Vec<Rc<RefCell<Scope>>>,
}

// represents a single scope level
#[derive(Debug, Default)]
struct Scope {
    symbols: HashMap<String, Symbol>,
    parent: Option<Weak<RefCell<Scope>>>,
    children: Vec<Rc<RefCell<Scope>>>,
}

impl SymbolTable {
    // create a new symbol table with global scope
    pub fn new() -> Self {
        let root = Rc::new(RefCell::new(Scope::default()));
        SymbolTable {
            root: Rc::clone(&root),
            scopes: vec![root],
        }
    }

    // add a new nested scope
    pub fn push_scope(&mut self) -> Result<(), SemanticError> {
        let new_scope = Rc::new(RefCell::new(Scope {
            parent: Some(Rc::downgrade(self.scopes.last().unwrap())),
            ..Default::default()
        }));
        self.scopes
            .last()
            .unwrap()
            .borrow_mut()
            .children
            .push(Rc::clone(&new_scope));
        self.scopes.push(new_scope);
        Ok(())
    }

    // exit current scope
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

    // check if symbol exists in any scope
    pub fn exists(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    // check if symbol exists in current scope only
    pub fn exists_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.borrow().symbols.contains_key(name))
    }

    // add a symbol to the current scope
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

    // find a symbol in current or parent scopes
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

// main semantic analyzer
pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    // create a new semantic analyzer
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    // Get reference to the symbol table for debugging
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    // entry point for semantic analysis
    pub fn analyze(&mut self, ast: &[AstNode]) -> Vec<SemanticError> {
        self.collect_hoistable_declarations(ast);
        self.analyze_nodes(ast);
        std::mem::take(&mut self.errors)
    }

    // first pass: collect hoistable declarations (functions, classes, etc)
    fn collect_hoistable_declarations(&mut self, ast: &[AstNode]) {
        for node in ast {
            match node {
                AstNode::Function(func) => {
                    if let Err(e) = self.symbol_table.add_symbol(
                        &func.name,
                        Symbol {
                            kind: SymbolKind::Function,
                            span: func.span,
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Class { name, .. } => {
                    if let Err(e) = self.symbol_table.add_symbol(
                        name,
                        Symbol {
                            kind: SymbolKind::Class,
                            span: *node.span(),
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
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                _ => {}
            }
        }
    }

    // second pass: analyze all nodes with full symbol information
    fn analyze_nodes(&mut self, nodes: &[AstNode]) {
        for node in nodes {
            if let Err(e) = self.analyze_node(node) {
                self.errors.push(e);
            }
        }
    }

    // analyze a single node
    fn analyze_node(&mut self, node: &AstNode) -> Result<(), SemanticError> {
        match node {
            AstNode::Function(func) => self.analyze_function(func),
            AstNode::Class { name, fields, methods, .. } => self.analyze_class(name, fields, methods),
            AstNode::Enum { .. } => Ok(()), // enums don't need further analysis
            AstNode::Interface { .. } => Ok(()), // interfaces don't need further analysis
            AstNode::Statement(stmt) => self.analyze_statement(stmt),
        }
    }

    // analyze a function definition
    fn analyze_function(&mut self, func: &FunctionNode) -> Result<(), SemanticError> {
        // create new scope for function parameters and body
        self.symbol_table.push_scope()?;

        // add parameters to function scope
        for param in &func.params {
            self.symbol_table.add_symbol(
                &param.name,
                Symbol {
                    kind: SymbolKind::Variable,
                    span: param.type_.span,
                },
            )?;
        }

        // analyze function body with new scope
        let result = self.analyze_block(&func.body);

        // clean up function scope
        self.symbol_table.pop_scope()?;
        result
    }

    // analyze a class definition
    fn analyze_class(&mut self, _name: &str, fields: &[Field], methods: &[FunctionNode]) -> Result<(), SemanticError> {
        // create new scope for class members
        self.symbol_table.push_scope()?;

        // add fields to class scope
        for field in fields {
            self.symbol_table.add_symbol(
                &field.name,
                Symbol {
                    kind: SymbolKind::Variable,
                    span: field.type_.span,
                },
            )?;
        }

        // add methods to class scope and analyze their bodies
        for method in methods {
            self.symbol_table.add_symbol(
                &method.name,
                Symbol {
                    kind: SymbolKind::Function,
                    span: method.span,
                },
            )?;
            self.analyze_function(method)?;
        }

        // clean up class scope
        self.symbol_table.pop_scope()?;
        Ok(())
    }

    // analyze a block of statements
    fn analyze_block(&mut self, stmts: &[StatementNode]) -> Result<(), SemanticError> {
        // first collect function declarations in this block
        for stmt in stmts {
            if let StatementKind::Function(func) = &stmt.kind {
                self.symbol_table.add_symbol(
                    &func.name,
                    Symbol {
                        kind: SymbolKind::Function,
                        span: stmt.span,
                    },
                )?;
            }
        }

        // then analyze all statements in order
        for stmt in stmts {
            self.analyze_statement(stmt)?;
        }

        Ok(())
    }

    // analyze a single statement
    fn analyze_statement(&mut self, stmt: &StatementNode) -> Result<(), SemanticError> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, expr) => {
                self.analyze_expression(expr)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                    },
                )?;
            }
            StatementKind::TypedDecl(name, _, expr) => {
                self.analyze_expression(expr)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
                    },
                )?;
            }
            StatementKind::Expression(expr) => {
                self.analyze_expression(expr)?;
            }
            StatementKind::Block(stmts) => self.analyze_block(stmts)?,
            StatementKind::ConstDecl(name, _, expr) => {
                self.analyze_expression(expr)?;
                self.symbol_table.add_symbol(
                    name,
                    Symbol {
                        kind: SymbolKind::Constant,
                        span: stmt.span,
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
                self.symbol_table.add_symbol(
                    var,
                    Symbol {
                        kind: SymbolKind::Variable,
                        span: stmt.span,
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
                    self.analyze_block(&arm.body)?;
                }
            }
            StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.analyze_expression(expr)?;
                }
            }
            StatementKind::Import { module_path, alias } => {
                // In a real implementation, we would resolve the module path,
                // analyze the imported module, and add its symbols to the current scope
                // For now, we'll just track the import for later resolution

                let symbol_name = if let Some(alias) = alias {
                    alias
                } else {
                    // If no alias, use the last component of the module path as the symbol name
                    module_path.split('.').last().unwrap_or(module_path)
                };

                self.symbol_table.add_symbol(
                    symbol_name,
                    Symbol {
                        kind: SymbolKind::Import,
                        span: stmt.span,
                    },
                )?;
            }
            _ => {} // handle other statement types
        }
        Ok(())
    }
    // analyze an expression
    fn analyze_expression(&self, expr: &ExpressionNode) -> Result<(), SemanticError> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                if !self.symbol_table.exists(name) {
                    return Err(SemanticError {
                        message: format!("undefined variable '{}'", name),
                        span: expr.span,
                    });
                }
            }
            ExpressionKind::Literal(_) => {} // literals are fine
            ExpressionKind::Binary { left, right, .. } => {
                self.analyze_expression(left)?;
                self.analyze_expression(right)?;
            }
            ExpressionKind::Unary { expr, .. } => {
                self.analyze_expression(expr)?;
            }
            ExpressionKind::Call { func, args } => {
                self.analyze_expression(func)?;
                for arg in args {
                    self.analyze_expression(arg)?;
                }
            }
            ExpressionKind::FieldAccess { expr, .. } => {
                self.analyze_expression(expr)?;
            }
            ExpressionKind::ListAccess { expr, index } => {
                self.analyze_expression(expr)?;
                self.analyze_expression(index)?;
            }
            ExpressionKind::ListLiteral(elements) => {
                for elem in elements {
                    self.analyze_expression(elem)?;
                }
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                for (key, value) in entries {
                    self.analyze_expression(key)?;
                    self.analyze_expression(value)?;
                }
            }
            ExpressionKind::If { cond, then_expr, else_expr } => {
                self.analyze_expression(cond)?;
                self.analyze_expression(then_expr)?;
                self.analyze_expression(else_expr)?;
            }
            ExpressionKind::Lambda { params, body } => {
                // Note: lambdas are not fully implemented in semantic analysis yet
                // Would need to push scope, add params, analyze body
                // For now, just analyze the body as a block
                let mut temp_analyzer = SemanticAnalyzer::new();
                temp_analyzer.analyze_block(body)?;
            }
            _ => {} // handle other expression types like GenericType
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
