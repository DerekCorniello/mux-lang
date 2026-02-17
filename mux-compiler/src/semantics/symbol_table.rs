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

/// Value representation for compile-time constants
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum ConstantValue {
    Float(f64),
    Int(i64),
    Bool(bool),
}

/// Types of items in stdlib modules
/// Uses &'static [Type] instead of Vec<Type> for const compatibility in PHF
#[derive(Debug, Clone, PartialEq)]
pub enum StdlibItem {
    Function {
        params: &'static [Type],
        ret: Type,
        llvm_name: &'static str,
    },
    Constant {
        ty: Type,
        value: ConstantValue,
    },
}

// Static arrays for function parameters (required for PHF const compatibility)
static FLOAT_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Float)];
static FLOAT_FLOAT_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Float),
    Type::Primitive(PrimitiveType::Float),
];
static INT_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Int)];
static INT_INT_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Int),
    Type::Primitive(PrimitiveType::Int),
];
static STR_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Str)];
static STR_STR_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Str),
    Type::Primitive(PrimitiveType::Str),
];
static EMPTY_PARAMS: &[Type] = &[];

fn io_fn(name: &'static str, params: &'static [Type], ret: Type) -> StdlibItem {
    StdlibItem::Function {
        params,
        ret,
        llvm_name: name,
    }
}

fn io_str_fn(name: &'static str, ret: Type) -> StdlibItem {
    io_fn(name, STR_PARAM, ret)
}

fn io_str_str_fn(name: &'static str, ret: Type) -> StdlibItem {
    io_fn(name, STR_STR_PARAMS, ret)
}

fn io_result(ok: Type) -> Type {
    Type::Named("Result".to_string(), vec![ok, str_()])
}

/// All stdlib items organized by module.function or module.constant
/// Using PHF for O(1) compile-time perfect hashing - ideal for large stdlibs
pub static STDLIB_ITEMS: phf::Map<&'static str, StdlibItem> = phf::phf_map! {
    // Math module - single argument float -> float functions
    "math.sqrt" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_sqrt",
    },
    "math.sin" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_sin",
    },
    "math.cos" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_cos",
    },
    "math.tan" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_tan",
    },
    "math.asin" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_asin",
    },
    "math.acos" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_acos",
    },
    "math.atan" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_atan",
    },
    "math.ln" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_ln",
    },
    "math.log2" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_log2",
    },
    "math.log10" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_log10",
    },
    "math.exp" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_exp",
    },
    "math.abs" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_abs",
    },
    "math.floor" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_floor",
    },
    "math.ceil" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_ceil",
    },
    "math.round" => StdlibItem::Function {
        params: FLOAT_PARAM,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_round",
    },

    // Math module - two argument float -> float functions
    "math.atan2" => StdlibItem::Function {
        params: FLOAT_FLOAT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_atan2",
    },
    "math.log" => StdlibItem::Function {
        params: FLOAT_FLOAT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_log",
    },
    "math.min" => StdlibItem::Function {
        params: FLOAT_FLOAT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_min",
    },
    "math.max" => StdlibItem::Function {
        params: FLOAT_FLOAT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_max",
    },
    "math.hypot" => StdlibItem::Function {
        params: FLOAT_FLOAT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_hypot",
    },
    "math.pow" => StdlibItem::Function {
        params: FLOAT_FLOAT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_math_pow",
    },

    // Math module - constants (NOT functions!)
    "math.pi" => StdlibItem::Constant {
        ty: Type::Primitive(PrimitiveType::Float),
        value: ConstantValue::Float(std::f64::consts::PI),
    },
    "math.e" => StdlibItem::Constant {
        ty: Type::Primitive(PrimitiveType::Float),
        value: ConstantValue::Float(std::f64::consts::E),
    },

    // Random module functions
    "random.seed" => StdlibItem::Function {
        params: INT_PARAM,
        ret: Type::Void,
        llvm_name: "mux_rand_init",
    },
    "random.next_int" => StdlibItem::Function {
        params: EMPTY_PARAMS,
        ret: Type::Primitive(PrimitiveType::Int),
        llvm_name: "mux_rand_int",
    },
    "random.next_range" => StdlibItem::Function {
        params: INT_INT_PARAMS,
        ret: Type::Primitive(PrimitiveType::Int),
        llvm_name: "mux_rand_range",
    },
    "random.next_float" => StdlibItem::Function {
        params: EMPTY_PARAMS,
        ret: Type::Primitive(PrimitiveType::Float),
        llvm_name: "mux_rand_float",
    },
    "random.next_bool" => StdlibItem::Function {
        params: EMPTY_PARAMS,
        ret: Type::Primitive(PrimitiveType::Bool),
        llvm_name: "mux_rand_bool",
    },
};

/// List of all available stdlib modules for wildcard imports
pub const STDLIB_MODULES: &[&str] = &["io", "math", "random"];

lazy_static! {
    // io uses lazy_static rather than PHF because signatures include Type::Named(Result<...>),
    // and Type currently stores owned Strings, which are not const-constructible for PHF values.
    pub static ref IO_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        let mut m = HashMap::new();
        m.insert("io.read_file", io_str_fn("mux_io_read_file", io_result(str_())));
        m.insert(
            "io.write_file",
            io_str_str_fn("mux_io_write_file", io_result(Type::Void)),
        );
        m.insert("io.exists", io_str_fn("mux_io_exists", io_result(bool_())));
        m.insert("io.remove", io_str_fn("mux_io_remove", io_result(Type::Void)));
        m.insert("io.is_file", io_str_fn("mux_io_is_file", io_result(bool_())));
        m.insert("io.is_dir", io_str_fn("mux_io_is_dir", io_result(bool_())));
        m.insert("io.mkdir", io_str_fn("mux_io_mkdir", io_result(Type::Void)));
        m.insert(
            "io.listdir",
            io_str_fn(
                "mux_io_listdir",
                io_result(Type::List(Box::new(Type::Primitive(PrimitiveType::Str)))),
            ),
        );
        m.insert("io.join", io_str_str_fn("mux_io_join", io_result(str_())));
        m.insert("io.basename", io_str_fn("mux_io_basename", io_result(str_())));
        m.insert("io.dirname", io_str_fn("mux_io_dirname", io_result(str_())));
        m
    };

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

pub fn all_stdlib_items() -> impl Iterator<Item = (&'static str, &'static StdlibItem)> {
    STDLIB_ITEMS
        .entries()
        .map(|(key, item)| (*key, item))
        .chain(IO_STDLIB_ITEMS.iter().map(|(key, item)| (*key, item)))
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

    /// Find symbols with names similar to the given name (for "did you mean?" suggestions).
    /// Uses a simple edit distance check to find candidates within a threshold.
    pub fn find_similar(&self, name: &str) -> Option<String> {
        let threshold = calculate_similarity_threshold(name);

        let mut best: Option<(String, usize)> = None;

        // Check all scopes
        for scope in self.scopes.iter().rev() {
            let scope_borrow = scope.borrow();
            best = Self::find_best_match(name, threshold, scope_borrow.symbols.keys(), best);
        }

        // Also check all_symbols (hoisted functions, classes, etc.)
        best = Self::find_best_match(name, threshold, self.all_symbols.keys(), best);

        // Check built-in functions
        best = Self::find_best_match(name, threshold, BUILT_IN_FUNCTIONS.keys().copied(), best);

        best.map(|(name, _)| name)
    }

    fn find_best_match<S: AsRef<str>>(
        name: &str,
        threshold: usize,
        candidates: impl Iterator<Item = S>,
        best: Option<(String, usize)>,
    ) -> Option<(String, usize)> {
        let mut current_best = best;
        for candidate in candidates {
            let s = candidate.as_ref();
            let dist = edit_distance(name, s);
            if dist <= threshold && current_best.as_ref().is_none_or(|(_, d)| dist < *d) {
                current_best = Some((s.to_string(), dist));
            }
        }
        current_best
    }
}

/// Calculate the maximum allowed edit distance for suggesting similar names.
/// Uses an adaptive threshold based on name length:
/// - 1-2 chars: threshold of 1 (strict for short names)
/// - 3-5 chars: threshold of 2 (moderate for medium names)
/// - 6+ chars: threshold of 3 (permissive for long names)
pub fn calculate_similarity_threshold(name: &str) -> usize {
    match name.len() {
        0..=2 => 1,
        3..=5 => 2,
        _ => 3,
    }
}

/// Compute the Levenshtein edit distance between two strings.
pub fn edit_distance(a: &str, b: &str) -> usize {
    let a_len = a.chars().count();
    let b_len = b.chars().count();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0; b_len + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1).min(curr[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[b_len]
}
