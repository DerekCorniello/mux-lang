use crate::ast::PrimitiveType;
use crate::lexer::Span;
use crate::semantics::error::SemanticError;
use crate::semantics::types::{BuiltInSig, Symbol, Type};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn float() -> Type {
    Type::Primitive(PrimitiveType::Float)
}
fn int() -> Type {
    Type::Primitive(PrimitiveType::Int)
}
fn str_() -> Type {
    Type::Primitive(PrimitiveType::Str)
}
fn bool_() -> Type {
    Type::Primitive(PrimitiveType::Bool)
}

fn sig(params: Vec<Type>, return_type: Type) -> BuiltInSig {
    BuiltInSig {
        params,
        return_type,
    }
}

fn register_batch(
    m: &mut HashMap<&'static str, BuiltInSig>,
    names: &[&'static str],
    sig: BuiltInSig,
) {
    for name in names {
        m.insert(name, sig.clone());
    }
}

lazy_static! {
    pub static ref BUILT_IN_FUNCTIONS: HashMap<&'static str, BuiltInSig> = {
        let mut m = HashMap::new();

        // int functions
        m.insert("int_to_string", sig(vec![int()], str_()));
        m.insert("int_to_float", sig(vec![int()], float()));
        register_batch(&mut m, &["int_add", "int_sub", "int_mul", "int_div", "int_rem"], sig(vec![int(), int()], int()));
        register_batch(&mut m, &["int_eq", "int_lt"], sig(vec![int(), int()], bool_()));

        // float functions
        m.insert("float_to_string", sig(vec![float()], str_()));
        m.insert("float_to_int", sig(vec![float()], int()));
        m.insert("float_add", sig(vec![float(), float()], float()));

        // string functions
        m.insert("string_to_int", sig(vec![str_()], int()));
        m.insert("string_to_float", sig(vec![str_()], float()));
        m.insert("string_concat", sig(vec![str_(), str_()], str_()));
        m.insert("string_length", sig(vec![str_()], int()));

        // bool functions
        m.insert("bool_to_string", sig(vec![bool_()], str_()));
        m.insert("bool_to_int", sig(vec![bool_()], int()));

        // math: single-arg float -> float
        register_batch(&mut m, &[
            "math_sqrt", "math_sin", "math_cos", "math_tan",
            "math_asin", "math_acos", "math_atan",
            "math_ln", "math_log2", "math_log10", "math_exp",
            "math_abs", "math_floor", "math_ceil", "math_round",
        ], sig(vec![float()], float()));

        // math: two-arg (float, float) -> float
        register_batch(&mut m, &[
            "math_pow", "math_atan2", "math_log",
            "math_min", "math_max", "math_hypot",
        ], sig(vec![float(), float()], float()));

        // math: zero-arg constants
        register_batch(&mut m, &["math_pi", "math_e"], sig(vec![], float()));

        // io functions
        m.insert("print", sig(vec![str_()], Type::Void));
        m.insert("read_line", sig(vec![], str_()));

        // std functions
        m.insert("range", sig(
            vec![int(), int()],
            Type::List(Box::new(int())),
        ));
        m.insert("Some", sig(
            vec![Type::Variable("T".to_string())],
            Type::Optional(Box::new(Type::Variable("T".to_string()))),
        ));
        m.insert("None", sig(
            vec![],
            Type::Optional(Box::new(Type::Void)),
        ));
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
