use crate::ast::PrimitiveType;
use crate::lexer::Span;
use crate::semantics::error::SemanticError;
use crate::semantics::types::{BuiltInSig, Symbol, Type};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

lazy_static! {
    pub static ref BUILT_IN_FUNCTIONS: HashMap<&'static str, BuiltInSig> = {
        let mut m = HashMap::new();
        // int functions
        m.insert("int_to_string", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Str),
        });
        m.insert("int_to_float", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        m.insert("int_add", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("int_sub", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("int_mul", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("int_div", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("int_rem", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("int_eq", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Bool),
        });
        m.insert("int_lt", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::Primitive(PrimitiveType::Bool),
        });
        // float functions
        m.insert("float_to_string", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Str),
        });
        m.insert("float_to_int", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("float_add", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float), Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        // string functions
        m.insert("string_to_int", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Str)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        m.insert("string_to_float", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Str)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        m.insert("string_concat", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Str), Type::Primitive(PrimitiveType::Str)],
            return_type: Type::Primitive(PrimitiveType::Str),
        });
        m.insert("string_length", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Str)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        // bool functions
        m.insert("bool_to_string", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Bool)],
            return_type: Type::Primitive(PrimitiveType::Str),
        });
        m.insert("bool_to_int", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Bool)],
            return_type: Type::Primitive(PrimitiveType::Int),
        });
        // math functions
        m.insert("math_pow", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float), Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        m.insert("math_sqrt", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        m.insert("math_sin", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        m.insert("math_cos", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Float)],
            return_type: Type::Primitive(PrimitiveType::Float),
        });
        // io functions
        m.insert("print", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Str)],
            return_type: Type::Void,
        });
        m.insert("read_line", BuiltInSig {
            params: vec![],
            return_type: Type::Primitive(PrimitiveType::Str),
        });
        // std functions
        m.insert("range", BuiltInSig {
            params: vec![Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)],
            return_type: Type::List(Box::new(Type::Primitive(PrimitiveType::Int))),
        });
        m.insert("Some", BuiltInSig {
            params: vec![Type::Variable("T".to_string())],
            return_type: Type::Optional(Box::new(Type::Variable("T".to_string()))),
        });

        m.insert("None", BuiltInSig {
            params: vec![],
            return_type: Type::Optional(Box::new(Type::Void)),
        });
        m
    };
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Rc<RefCell<Scope>>>,
    pub all_symbols: HashMap<String, Symbol>,
}

#[derive(Debug, Default)]
struct Scope {
    symbols: HashMap<String, Symbol>,
    children: Vec<Rc<RefCell<Scope>>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        let root = Rc::new(RefCell::new(Scope::default()));
        SymbolTable {
            scopes: vec![root],
            all_symbols: HashMap::new(),
        }
    }

    pub fn push_scope(&mut self) -> Result<(), SemanticError> {
        let new_scope = Rc::new(RefCell::new(Scope::default()));
        self.scopes
            .last()
            .expect("at least global scope should exist")
            .borrow_mut()
            .children
            .push(Rc::clone(&new_scope));
        self.scopes.push(new_scope);
        Ok(())
    }

    pub fn pop_scope(&mut self) -> Result<(), SemanticError> {
        if self.scopes.len() <= 1 {
            return Err(SemanticError {
                message: "Cannot pop the global scope".into(),
                span: Span::new(0, 0), // Internal error, no user span available
            });
        }
        self.scopes.pop();
        Ok(())
    }

    pub fn exists(&self, name: &str) -> bool {
        // For variable/constant lookups, only check the scope stack (not global)
        // For other symbol kinds (functions, classes, etc.), check global too
        if self.get_cloned(name).is_some() {
            return true;
        }

        // Only consider symbols that are currently in scope.
        // Global `all_symbols` is intentionally ignored here to prevent
        // out-of-scope symbols from being treated as visible.
        self.get_cloned(name).is_some()
    }

    pub fn add_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        if self.scopes.is_empty() {
            return Err(SemanticError {
                message: "No active scope".into(),
                span: Span::new(0, 0), // Internal error, no user span available
            });
        }

        let current = self
            .scopes
            .last()
            .expect("at least global scope should exist");
        let mut current_borrow = current.borrow_mut();

        if current_borrow.symbols.contains_key(name) {
            return Err(SemanticError {
                message: format!("Duplicate declaration of '{}'", name),
                span: symbol.span,
            });
        }

        current_borrow
            .symbols
            .insert(name.to_string(), symbol.clone());
        // Add all symbols to all_symbols for codegen lookups
        // Note: This can cause name collisions for local variables across different functions,
        // but it's needed because codegen calls get_expression_type after scopes are popped.
        // For the collision case in generic methods, codegen should use expression types directly.
        if name != "self" {
            self.all_symbols.insert(name.to_string(), symbol);
        }
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.all_symbols.get(name) {
            return Some(symbol.clone());
        }
        None
    }

    pub fn get_cloned(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            let scope_borrow = scope.borrow();
            if let Some(symbol) = scope_borrow.symbols.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }
}
