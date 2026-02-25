use crate::ast::PrimitiveType;
use crate::lexer::Span;
use crate::semantics::error::SemanticError;
use crate::semantics::types::{BuiltInSig, Symbol, Type};
use lazy_static::lazy_static;
use phf::phf_map;
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
/// Owns its data to avoid Box::leak
#[derive(Debug, Clone, PartialEq)]
pub enum StdlibItem {
    Function {
        params: Vec<Type>,
        ret: Type,
        llvm_name: String,
    },
    Constant {
        ty: Type,
        value: ConstantValue,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeDesc {
    Float,
    Int,
    Bool,
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstDesc {
    Pi,
    E,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StdlibItemDesc {
    Function {
        params: &'static [TypeDesc],
        ret: TypeDesc,
        llvm_name: &'static str,
    },
    Constant {
        ty: TypeDesc,
        value: ConstDesc,
    },
}

const EMPTY_PARAM_DESC: &[TypeDesc] = &[];
const INT_PARAM_DESC: &[TypeDesc] = &[TypeDesc::Int];
const INT_INT_PARAM_DESC: &[TypeDesc] = &[TypeDesc::Int, TypeDesc::Int];

fn materialize_type(desc: TypeDesc) -> Type {
    match desc {
        TypeDesc::Float => Type::Primitive(PrimitiveType::Float),
        TypeDesc::Int => Type::Primitive(PrimitiveType::Int),
        TypeDesc::Bool => Type::Primitive(PrimitiveType::Bool),
        TypeDesc::Void => Type::Void,
    }
}

fn materialize_const(desc: ConstDesc) -> ConstantValue {
    match desc {
        ConstDesc::Pi => ConstantValue::Float(std::f64::consts::PI),
        ConstDesc::E => ConstantValue::Float(std::f64::consts::E),
    }
}

fn materialize_stdlib_item(desc: &StdlibItemDesc) -> StdlibItem {
    match desc {
        StdlibItemDesc::Function {
            params,
            ret,
            llvm_name,
        } => StdlibItem::Function {
            params: params.iter().copied().map(materialize_type).collect(),
            ret: materialize_type(*ret),
            llvm_name: (*llvm_name).to_string(),
        },
        StdlibItemDesc::Constant { ty, value } => StdlibItem::Constant {
            ty: materialize_type(*ty),
            value: materialize_const(*value),
        },
    }
}

// Static arrays for function parameters (required for PHF const compatibility)
static FLOAT_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Float)];
static FLOAT_FLOAT_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Float),
    Type::Primitive(PrimitiveType::Float),
];
static INT_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Int)];
static INT_STR_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Int),
    Type::Primitive(PrimitiveType::Str),
];
static BOOL_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Bool)];
static BOOL_STR_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Bool),
    Type::Primitive(PrimitiveType::Str),
];
static STR_PARAM: &[Type] = &[Type::Primitive(PrimitiveType::Str)];
static STR_STR_PARAMS: &[Type] = &[
    Type::Primitive(PrimitiveType::Str),
    Type::Primitive(PrimitiveType::Str),
];
static EMPTY_PARAMS: &[Type] = &[];

// Lazy static arrays for ASSERT functions (to avoid Box::leak)
lazy_static::lazy_static! {
    static ref T_PARAM: Vec<Type> = vec![Type::Variable("T".to_string())];
    static ref T_T_PARAMS: Vec<Type> = vec![
        Type::Variable("T".to_string()),
        Type::Variable("T".to_string()),
    ];
    static ref OPTIONAL_T_PARAM: Vec<Type> =
        vec![Type::Optional(Box::new(Type::Variable("T".to_string())))];
    static ref RESULT_T_E_PARAMS: Vec<Type> = vec![
        Type::Result(
            Box::new(Type::Variable("T".to_string())),
            Box::new(Type::Variable("E".to_string())),
        ),
    ];
}

fn io_fn(name: &'static str, params: &'static [Type], ret: Type) -> StdlibItem {
    StdlibItem::Function {
        params: params.to_vec(),
        ret,
        llvm_name: name.to_string(),
    }
}

fn io_str_fn(name: &'static str, ret: Type) -> StdlibItem {
    io_fn(name, STR_PARAM, ret)
}

fn io_str_str_fn(name: &'static str, ret: Type) -> StdlibItem {
    io_fn(name, STR_STR_PARAMS, ret)
}

fn io_result(ok: Type) -> Type {
    Type::Result(Box::new(ok), Box::new(str_()))
}

fn datetime_fn(name: &'static str, params: &'static [Type], ret: Type) -> StdlibItem {
    StdlibItem::Function {
        params: params.to_vec(),
        ret,
        llvm_name: name.to_string(),
    }
}

static STDLIB_ITEMS: phf::Map<&'static str, StdlibItemDesc> = phf_map! {
    "math.pi" => StdlibItemDesc::Constant { ty: TypeDesc::Float, value: ConstDesc::Pi },
    "math.e" => StdlibItemDesc::Constant { ty: TypeDesc::Float, value: ConstDesc::E },
    "random.seed" => StdlibItemDesc::Function {
        params: INT_PARAM_DESC,
        ret: TypeDesc::Void,
        llvm_name: "mux_rand_init",
    },
    "random.next_int" => StdlibItemDesc::Function {
        params: EMPTY_PARAM_DESC,
        ret: TypeDesc::Int,
        llvm_name: "mux_rand_int",
    },
    "random.next_range" => StdlibItemDesc::Function {
        params: INT_INT_PARAM_DESC,
        ret: TypeDesc::Int,
        llvm_name: "mux_rand_range",
    },
    "random.next_float" => StdlibItemDesc::Function {
        params: EMPTY_PARAM_DESC,
        ret: TypeDesc::Float,
        llvm_name: "mux_rand_float",
    },
    "random.next_bool" => StdlibItemDesc::Function {
        params: EMPTY_PARAM_DESC,
        ret: TypeDesc::Bool,
        llvm_name: "mux_rand_bool",
    },
};

/// List of all available stdlib modules for wildcard imports
pub const STDLIB_MODULES: &[&str] = &["assert", "datetime", "io", "math", "random"];

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

    // Math module - single argument float -> float functions
    // Math module - two argument float -> float functions
    pub static ref MATH_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        fn make_math_fn(llvm_name: &'static str, params: &'static [Type]) -> StdlibItem {
            StdlibItem::Function {
                params: params.to_vec(),
                ret: Type::Primitive(PrimitiveType::Float),
                llvm_name: llvm_name.to_string(),
            }
        }

        let mut m = HashMap::new();

        // Single-arg math functions
        m.insert("math.sqrt", make_math_fn("mux_math_sqrt", FLOAT_PARAM));
        m.insert("math.sin", make_math_fn("mux_math_sin", FLOAT_PARAM));
        m.insert("math.cos", make_math_fn("mux_math_cos", FLOAT_PARAM));
        m.insert("math.tan", make_math_fn("mux_math_tan", FLOAT_PARAM));
        m.insert("math.asin", make_math_fn("mux_math_asin", FLOAT_PARAM));
        m.insert("math.acos", make_math_fn("mux_math_acos", FLOAT_PARAM));
        m.insert("math.atan", make_math_fn("mux_math_atan", FLOAT_PARAM));
        m.insert("math.ln", make_math_fn("mux_math_ln", FLOAT_PARAM));
        m.insert("math.log2", make_math_fn("mux_math_log2", FLOAT_PARAM));
        m.insert("math.log10", make_math_fn("mux_math_log10", FLOAT_PARAM));
        m.insert("math.exp", make_math_fn("mux_math_exp", FLOAT_PARAM));
        m.insert("math.abs", make_math_fn("mux_math_abs", FLOAT_PARAM));
        m.insert("math.floor", make_math_fn("mux_math_floor", FLOAT_PARAM));
        m.insert("math.ceil", make_math_fn("mux_math_ceil", FLOAT_PARAM));
        m.insert("math.round", make_math_fn("mux_math_round", FLOAT_PARAM));

        // Two-arg math functions
        m.insert("math.atan2", make_math_fn("mux_math_atan2", FLOAT_FLOAT_PARAMS));
        m.insert("math.log", make_math_fn("mux_math_log", FLOAT_FLOAT_PARAMS));
        m.insert("math.min", make_math_fn("mux_math_min", FLOAT_FLOAT_PARAMS));
        m.insert("math.max", make_math_fn("mux_math_max", FLOAT_FLOAT_PARAMS));
        m.insert("math.hypot", make_math_fn("mux_math_hypot", FLOAT_FLOAT_PARAMS));
        m.insert("math.pow", make_math_fn("mux_math_pow", FLOAT_FLOAT_PARAMS));

        m
    };

    pub static ref DATETIME_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        let mut m = HashMap::new();
        m.insert("datetime.now", datetime_fn("mux_datetime_now", EMPTY_PARAMS, io_result(int())));
        m.insert("datetime.now_millis", datetime_fn("mux_datetime_now_millis", EMPTY_PARAMS, io_result(int())));
        m.insert("datetime.year", datetime_fn("mux_datetime_year", INT_PARAM, io_result(int())));
        m.insert("datetime.month", datetime_fn("mux_datetime_month", INT_PARAM, io_result(int())));
        m.insert("datetime.day", datetime_fn("mux_datetime_day", INT_PARAM, io_result(int())));
        m.insert("datetime.hour", datetime_fn("mux_datetime_hour", INT_PARAM, io_result(int())));
        m.insert("datetime.minute", datetime_fn("mux_datetime_minute", INT_PARAM, io_result(int())));
        m.insert("datetime.second", datetime_fn("mux_datetime_second", INT_PARAM, io_result(int())));
        m.insert("datetime.weekday", datetime_fn("mux_datetime_weekday", INT_PARAM, io_result(int())));
        m.insert("datetime.format", datetime_fn("mux_datetime_format", INT_STR_PARAMS, io_result(str_())));
        m.insert("datetime.format_local", datetime_fn("mux_datetime_format_local", INT_STR_PARAMS, io_result(str_())));
        m.insert("datetime.sleep", datetime_fn("mux_datetime_sleep", INT_PARAM, io_result(Type::Void)));
        m.insert("datetime.sleep_millis", datetime_fn("mux_datetime_sleep_millis", INT_PARAM, io_result(Type::Void)));
        m
    };

    pub static ref ASSERT_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        fn make_item(llvm_name: &'static str, params: &[Type]) -> StdlibItem {
            StdlibItem::Function {
                params: params.to_vec(),
                ret: Type::Void,
                llvm_name: llvm_name.to_string(),
            }
        }
        let mut m = HashMap::new();
        m.insert("assert.assert", make_item("mux_assert_assert", BOOL_STR_PARAMS));
        m.insert("assert.assert_eq", make_item("mux_assert_eq", &T_T_PARAMS));
        m.insert("assert.assert_ne", make_item("mux_assert_ne", &T_T_PARAMS));
        m.insert("assert.assert_true", make_item("mux_assert_true", BOOL_PARAM));
        m.insert("assert.assert_false", make_item("mux_assert_false", BOOL_PARAM));
        m.insert("assert.assert_some", make_item("mux_assert_some", &OPTIONAL_T_PARAM));
        m.insert("assert.assert_none", make_item("mux_assert_none", &OPTIONAL_T_PARAM));
        m.insert("assert.assert_ok", make_item("mux_assert_ok", &RESULT_T_E_PARAMS));
        m.insert("assert.assert_err", make_item("mux_assert_err", &RESULT_T_E_PARAMS));
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
        m.insert("some", sig(
            vec![Type::Variable("T".to_string())],
            Type::Optional(Box::new(Type::Variable("T".to_string()))),
        ));
        m.insert("none", sig(
            vec![],
            Type::Optional(Box::new(Type::Void)),
        ));
        m
    };
}

pub fn all_stdlib_items() -> impl Iterator<Item = (String, StdlibItem)> {
    STDLIB_ITEMS
        .entries()
        .map(|(key, item)| (key.to_string(), materialize_stdlib_item(item)))
        .chain(
            IO_STDLIB_ITEMS
                .iter()
                .map(|(key, item)| (key.to_string(), item.clone())),
        )
        .chain(
            MATH_STDLIB_ITEMS
                .iter()
                .map(|(key, item)| (key.to_string(), item.clone())),
        )
        .chain(
            DATETIME_STDLIB_ITEMS
                .iter()
                .map(|(key, item)| (key.to_string(), item.clone())),
        )
        .chain(
            ASSERT_STDLIB_ITEMS
                .iter()
                .map(|(key, item)| (key.to_string(), item.clone())),
        )
}

pub fn lookup_stdlib_item(name: &str) -> Option<StdlibItem> {
    if let Some(item) = STDLIB_ITEMS.get(name) {
        return Some(materialize_stdlib_item(item));
    }
    IO_STDLIB_ITEMS
        .get(name)
        .cloned()
        .or_else(|| MATH_STDLIB_ITEMS.get(name).cloned())
        .or_else(|| DATETIME_STDLIB_ITEMS.get(name).cloned())
        .or_else(|| ASSERT_STDLIB_ITEMS.get(name).cloned())
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
