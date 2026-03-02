//! Canonical stdlib registry
//!
//! This module centralizes the standard library (stdlib) description used by the
//! semantic analyzer and codegen. It provides:
//! - `StdlibItem` — canonical representation for stdlib functions and constants
//! - `lookup_stdlib_item` / `all_stdlib_items` — accessors for items
//! - `BUILT_IN_FUNCTIONS` — built-in function signatures used for name resolution
//! - `*_STDLIB_ITEMS` tables for per-module items (IO, MATH, DATETIME, ASSERT, SYNC)
//! - `net_module_class_symbols` / `sync_module_class_symbols` — class symbol builders
//! - `stdlib_item_to_symbol` / `register_stdlib_item_into` — helpers to convert and
//!   register stdlib items into the compiler's `SymbolTable`.
//!
//! Contributing
//! - Add new stdlib items by updating the appropriate table (`MATH_STDLIB_ITEMS`,
//!   `IO_STDLIB_ITEMS`, or `STDLIB_ITEMS` for PHF-backed descriptors).
//! - Prefer adding entries to the module-specific HashMaps rather than spreading
//!   duplicates across the codebase — these are the single source of truth.
//! - For class types (e.g., `net` / `sync`), update the corresponding `*_methods`
//!   helper and the `*_module_class_symbols` function so the analyzer can import
//!   class symbols.
//!
//! Rationale
//! - The previous code duplicated stdlib declarations in multiple places (symbol
//!   table, semantic analyzer). Consolidating them here prevents drift and keeps
//!   a clear mapping between stdlib names and their runtime/LLVM counterparts.

use crate::ast::PrimitiveType;
use crate::lexer::Span;
use crate::semantics::types::{BuiltInSig, MethodSig, Symbol, SymbolKind, Type};
use lazy_static::lazy_static;
use phf::phf_map;
use std::collections::HashMap;

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
lazy_static! {
    static ref T_PARAM: Vec<Type> = vec![Type::Variable("T".to_string())];
    static ref T_T_PARAMS: Vec<Type> = vec![
        Type::Variable("T".to_string()),
        Type::Variable("T".to_string())
    ];
    static ref OPTIONAL_T_PARAM: Vec<Type> =
        vec![Type::Optional(Box::new(Type::Variable("T".to_string())))];
    static ref RESULT_T_E_PARAMS: Vec<Type> = vec![Type::Result(
        Box::new(Type::Variable("T".to_string())),
        Box::new(Type::Variable("E".to_string()))
    )];
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

fn net_bytes_type() -> Type {
    Type::List(Box::new(Type::Primitive(PrimitiveType::Int)))
}
fn net_tuple_bytes_addr_type() -> Type {
    Type::Tuple(Box::new(net_bytes_type()), Box::new(str_()))
}
fn tcp_stream_type() -> Type {
    Type::Named("TcpStream".to_string(), Vec::new())
}
fn udp_socket_type() -> Type {
    Type::Named("UdpSocket".to_string(), Vec::new())
}

fn make_class_symbol(name: &str, methods: HashMap<String, MethodSig>, span: Span) -> Symbol {
    Symbol {
        kind: SymbolKind::Class,
        span,
        type_: Some(Type::Named(name.to_string(), Vec::new())),
        interfaces: HashMap::new(),
        methods,
        fields: HashMap::new(),
        type_params: Vec::new(),
        original_name: None,
        llvm_name: None,
        default_param_count: 0,
        variants: None,
    }
}

fn tcp_stream_methods() -> HashMap<String, MethodSig> {
    let mut methods = HashMap::new();
    methods.insert(
        "connect".to_string(),
        MethodSig {
            params: vec![str_()],
            return_type: io_result(tcp_stream_type()),
            is_static: true,
        },
    );
    methods.insert(
        "read".to_string(),
        MethodSig {
            params: vec![int()],
            return_type: io_result(net_bytes_type()),
            is_static: false,
        },
    );
    methods.insert(
        "write".to_string(),
        MethodSig {
            params: vec![net_bytes_type()],
            return_type: io_result(int()),
            is_static: false,
        },
    );
    methods.insert(
        "close".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Void,
            is_static: false,
        },
    );
    methods.insert(
        "set_nonblocking".to_string(),
        MethodSig {
            params: vec![bool_()],
            return_type: io_result(Type::Void),
            is_static: false,
        },
    );
    methods.insert(
        "peer_addr".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: io_result(str_()),
            is_static: false,
        },
    );
    methods.insert(
        "local_addr".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: io_result(str_()),
            is_static: false,
        },
    );
    methods
}

fn udp_socket_methods() -> HashMap<String, MethodSig> {
    let mut methods = HashMap::new();
    methods.insert(
        "bind".to_string(),
        MethodSig {
            params: vec![str_()],
            return_type: io_result(udp_socket_type()),
            is_static: true,
        },
    );
    methods.insert(
        "send_to".to_string(),
        MethodSig {
            params: vec![net_bytes_type(), str_()],
            return_type: io_result(int()),
            is_static: false,
        },
    );
    methods.insert(
        "recv_from".to_string(),
        MethodSig {
            params: vec![int()],
            return_type: io_result(net_tuple_bytes_addr_type()),
            is_static: false,
        },
    );
    methods.insert(
        "close".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Void,
            is_static: false,
        },
    );
    methods.insert(
        "set_nonblocking".to_string(),
        MethodSig {
            params: vec![bool_()],
            return_type: io_result(Type::Void),
            is_static: false,
        },
    );
    methods.insert(
        "peer_addr".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: io_result(str_()),
            is_static: false,
        },
    );
    methods.insert(
        "local_addr".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: io_result(str_()),
            is_static: false,
        },
    );
    methods
}

pub fn net_module_class_symbols(span: Span) -> HashMap<String, Symbol> {
    let mut classes = HashMap::new();
    classes.insert(
        "TcpStream".to_string(),
        make_class_symbol("TcpStream", tcp_stream_methods(), span),
    );
    classes.insert(
        "UdpSocket".to_string(),
        make_class_symbol("UdpSocket", udp_socket_methods(), span),
    );
    classes
}

fn thread_methods() -> HashMap<String, MethodSig> {
    let mut methods = HashMap::new();
    methods.insert(
        "join".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods
}

fn condvar_methods() -> HashMap<String, MethodSig> {
    let mut methods = HashMap::new();
    methods.insert(
        "new".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Named("CondVar".to_string(), Vec::new()),
            is_static: true,
        },
    );
    methods.insert(
        "wait".to_string(),
        MethodSig {
            params: vec![Type::Named("Mutex".to_string(), Vec::new())],
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods.insert(
        "signal".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods
}

fn rwlock_methods() -> HashMap<String, MethodSig> {
    let mut methods = HashMap::new();
    methods.insert(
        "new".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Named("RwLock".to_string(), Vec::new()),
            is_static: true,
        },
    );
    methods.insert(
        "read_lock".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods.insert(
        "write_lock".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods.insert(
        "unlock".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods
}

fn mutex_methods() -> HashMap<String, MethodSig> {
    let mut methods = HashMap::new();
    methods.insert(
        "new".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Named("Mutex".to_string(), Vec::new()),
            is_static: true,
        },
    );
    methods.insert(
        "lock".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods.insert(
        "unlock".to_string(),
        MethodSig {
            params: Vec::new(),
            return_type: Type::Result(Box::new(Type::Void), Box::new(str_())),
            is_static: false,
        },
    );
    methods
}

pub fn sync_module_class_symbols(span: Span) -> HashMap<String, Symbol> {
    let mut classes = HashMap::new();
    classes.insert(
        "Thread".to_string(),
        make_class_symbol("Thread", thread_methods(), span),
    );
    classes.insert(
        "CondVar".to_string(),
        make_class_symbol("CondVar", condvar_methods(), span),
    );
    classes.insert(
        "RwLock".to_string(),
        make_class_symbol("RwLock", rwlock_methods(), span),
    );
    classes.insert(
        "Mutex".to_string(),
        make_class_symbol("Mutex", mutex_methods(), span),
    );
    classes
}

fn datetime_fn(name: &'static str, params: &'static [Type], ret: Type) -> StdlibItem {
    io_fn(name, params, ret)
}

static STDLIB_ITEMS: phf::Map<&'static str, StdlibItemDesc> = phf_map! {
    "math.pi" => StdlibItemDesc::Constant { ty: TypeDesc::Float, value: ConstDesc::Pi },
    "math.e" => StdlibItemDesc::Constant { ty: TypeDesc::Float, value: ConstDesc::E },
    "random.seed" => StdlibItemDesc::Function { params: INT_PARAM_DESC, ret: TypeDesc::Void, llvm_name: "mux_rand_init" },
    "random.next_int" => StdlibItemDesc::Function { params: EMPTY_PARAM_DESC, ret: TypeDesc::Int, llvm_name: "mux_rand_int" },
    "random.next_range" => StdlibItemDesc::Function { params: INT_INT_PARAM_DESC, ret: TypeDesc::Int, llvm_name: "mux_rand_range" },
    "random.next_float" => StdlibItemDesc::Function { params: EMPTY_PARAM_DESC, ret: TypeDesc::Float, llvm_name: "mux_rand_float" },
    "random.next_bool" => StdlibItemDesc::Function { params: EMPTY_PARAM_DESC, ret: TypeDesc::Bool, llvm_name: "mux_rand_bool" },
};

/// List of all available stdlib modules for wildcard imports
pub const STDLIB_MODULES: &[&str] = &["assert", "datetime", "io", "math", "random", "sync", "net"];

lazy_static! {
    pub static ref IO_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        let mut m = HashMap::new();
        m.insert(
            "io.read_file",
            io_str_fn("mux_io_read_file", io_result(str_())),
        );
        m.insert(
            "io.write_file",
            io_str_str_fn("mux_io_write_file", io_result(Type::Void)),
        );
        m.insert("io.exists", io_str_fn("mux_io_exists", io_result(bool_())));
        m.insert(
            "io.remove",
            io_str_fn("mux_io_remove", io_result(Type::Void)),
        );
        m.insert(
            "io.is_file",
            io_str_fn("mux_io_is_file", io_result(bool_())),
        );
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
        m.insert(
            "io.basename",
            io_str_fn("mux_io_basename", io_result(str_())),
        );
        m.insert("io.dirname", io_str_fn("mux_io_dirname", io_result(str_())));
        m
    };
    pub static ref MATH_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        fn make_math_fn(llvm_name: &'static str, params: &'static [Type]) -> StdlibItem {
            StdlibItem::Function {
                params: params.to_vec(),
                ret: Type::Primitive(PrimitiveType::Float),
                llvm_name: llvm_name.to_string(),
            }
        }
        let mut m = HashMap::new();
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
        m.insert(
            "math.atan2",
            make_math_fn("mux_math_atan2", FLOAT_FLOAT_PARAMS),
        );
        m.insert("math.log", make_math_fn("mux_math_log", FLOAT_FLOAT_PARAMS));
        m.insert("math.min", make_math_fn("mux_math_min", FLOAT_FLOAT_PARAMS));
        m.insert("math.max", make_math_fn("mux_math_max", FLOAT_FLOAT_PARAMS));
        m.insert(
            "math.hypot",
            make_math_fn("mux_math_hypot", FLOAT_FLOAT_PARAMS),
        );
        m.insert("math.pow", make_math_fn("mux_math_pow", FLOAT_FLOAT_PARAMS));
        m
    };
    pub static ref DATETIME_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        let mut m = HashMap::new();
        m.insert(
            "datetime.now",
            datetime_fn("mux_datetime_now", EMPTY_PARAMS, io_result(int())),
        );
        m.insert(
            "datetime.now_millis",
            datetime_fn("mux_datetime_now_millis", EMPTY_PARAMS, io_result(int())),
        );
        m.insert(
            "datetime.year",
            datetime_fn("mux_datetime_year", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.month",
            datetime_fn("mux_datetime_month", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.day",
            datetime_fn("mux_datetime_day", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.hour",
            datetime_fn("mux_datetime_hour", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.minute",
            datetime_fn("mux_datetime_minute", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.second",
            datetime_fn("mux_datetime_second", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.weekday",
            datetime_fn("mux_datetime_weekday", INT_PARAM, io_result(int())),
        );
        m.insert(
            "datetime.format",
            datetime_fn("mux_datetime_format", INT_STR_PARAMS, io_result(str_())),
        );
        m.insert(
            "datetime.format_local",
            datetime_fn(
                "mux_datetime_format_local",
                INT_STR_PARAMS,
                io_result(str_()),
            ),
        );
        m.insert(
            "datetime.sleep",
            datetime_fn("mux_datetime_sleep", INT_PARAM, io_result(Type::Void)),
        );
        m.insert(
            "datetime.sleep_millis",
            datetime_fn(
                "mux_datetime_sleep_millis",
                INT_PARAM,
                io_result(Type::Void),
            ),
        );
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
        m.insert(
            "assert.assert",
            make_item("mux_assert_assert", BOOL_STR_PARAMS),
        );
        m.insert("assert.assert_eq", make_item("mux_assert_eq", &T_T_PARAMS));
        m.insert("assert.assert_ne", make_item("mux_assert_ne", &T_T_PARAMS));
        m.insert(
            "assert.assert_true",
            make_item("mux_assert_true", BOOL_PARAM),
        );
        m.insert(
            "assert.assert_false",
            make_item("mux_assert_false", BOOL_PARAM),
        );
        m.insert(
            "assert.assert_some",
            make_item("mux_assert_some", &OPTIONAL_T_PARAM),
        );
        m.insert(
            "assert.assert_none",
            make_item("mux_assert_none", &OPTIONAL_T_PARAM),
        );
        m.insert(
            "assert.assert_ok",
            make_item("mux_assert_ok", &RESULT_T_E_PARAMS),
        );
        m.insert(
            "assert.assert_err",
            make_item("mux_assert_err", &RESULT_T_E_PARAMS),
        );
        m
    };
    pub static ref SYNC_STDLIB_ITEMS: HashMap<&'static str, StdlibItem> = {
        let mut m = HashMap::new();
        m.insert(
            "sync.spawn",
            StdlibItem::Function {
                params: vec![Type::Function {
                    params: Vec::new(),
                    returns: Box::new(Type::Void),
                    default_count: 0,
                }],
                ret: Type::Result(
                    Box::new(Type::Named("Thread".to_string(), Vec::new())),
                    Box::new(str_()),
                ),
                llvm_name: "mux_sync_spawn".to_string(),
            },
        );
        m.insert(
            "sync.sleep",
            StdlibItem::Function {
                params: INT_PARAM.to_vec(),
                ret: Type::Void,
                llvm_name: "mux_sync_sleep".to_string(),
            },
        );
        m
    };
    pub static ref BUILT_IN_FUNCTIONS: HashMap<&'static str, BuiltInSig> = {
        let mut m = HashMap::new();
        m.insert("int_to_string", sig(vec![int()], str_()));
        m.insert("int_to_float", sig(vec![int()], float()));
        register_batch(
            &mut m,
            &["int_add", "int_sub", "int_mul", "int_div", "int_rem"],
            sig(vec![int(), int()], int()),
        );
        register_batch(
            &mut m,
            &["int_eq", "int_lt"],
            sig(vec![int(), int()], bool_()),
        );
        m.insert("float_to_string", sig(vec![float()], str_()));
        m.insert("float_to_int", sig(vec![float()], int()));
        m.insert("float_add", sig(vec![float(), float()], float()));
        m.insert("string_to_int", sig(vec![str_()], int()));
        m.insert("string_to_float", sig(vec![str_()], float()));
        m.insert("string_concat", sig(vec![str_(), str_()], str_()));
        m.insert("string_length", sig(vec![str_()], int()));
        m.insert("bool_to_string", sig(vec![bool_()], str_()));
        m.insert("bool_to_int", sig(vec![bool_()], int()));
        m.insert("print", sig(vec![str_()], Type::Void));
        m.insert("read_line", sig(vec![], str_()));
        m.insert(
            "range",
            sig(vec![int(), int()], Type::List(Box::new(int()))),
        );
        m.insert(
            "some",
            sig(
                vec![Type::Variable("T".to_string())],
                Type::Optional(Box::new(Type::Variable("T".to_string()))),
            ),
        );
        m.insert("none", sig(vec![], Type::Optional(Box::new(Type::Void))));
        m.insert(
            "ok",
            sig(
                vec![Type::Variable("T".to_string())],
                Type::Result(
                    Box::new(Type::Variable("T".to_string())),
                    Box::new(Type::Variable("E".to_string())),
                ),
            ),
        );
        m.insert(
            "err",
            sig(
                vec![Type::Variable("E".to_string())],
                Type::Result(
                    Box::new(Type::Variable("T".to_string())),
                    Box::new(Type::Variable("E".to_string())),
                ),
            ),
        );
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
                .map(|(k, v)| (k.to_string(), v.clone())),
        )
        .chain(
            MATH_STDLIB_ITEMS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone())),
        )
        .chain(
            DATETIME_STDLIB_ITEMS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone())),
        )
        .chain(
            ASSERT_STDLIB_ITEMS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone())),
        )
        .chain(
            SYNC_STDLIB_ITEMS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone())),
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
        .or_else(|| SYNC_STDLIB_ITEMS.get(name).cloned())
}

/// Convert a canonical `StdlibItem` into a `Symbol` suitable for registration in a SymbolTable.
pub fn stdlib_item_to_symbol(item: &StdlibItem, span: Span) -> Symbol {
    match item {
        StdlibItem::Function {
            params,
            ret,
            llvm_name,
        } => Symbol {
            kind: SymbolKind::Function,
            span,
            type_: Some(Type::Function {
                params: params.to_vec(),
                returns: Box::new(ret.clone()),
                default_count: 0,
            }),
            interfaces: std::collections::HashMap::new(),
            methods: std::collections::HashMap::new(),
            fields: std::collections::HashMap::new(),
            type_params: Vec::new(),
            original_name: None,
            llvm_name: Some(llvm_name.to_string()),
            default_param_count: 0,
            variants: None,
        },
        StdlibItem::Constant { ty, .. } => Symbol {
            kind: SymbolKind::Constant,
            span,
            type_: Some(ty.clone()),
            interfaces: std::collections::HashMap::new(),
            methods: std::collections::HashMap::new(),
            fields: std::collections::HashMap::new(),
            type_params: Vec::new(),
            original_name: None,
            llvm_name: None,
            default_param_count: 0,
            variants: None,
        },
    }
}

/// Register a single stdlib item into the provided symbol table (used for flat imports).
pub fn register_stdlib_item_into(
    table: &mut crate::semantics::symbol_table::SymbolTable,
    name: &str,
    item: &StdlibItem,
    span: Span,
) -> Result<(), crate::semantics::error::SemanticError> {
    let symbol = stdlib_item_to_symbol(item, span);
    table.add_symbol(name, symbol)?;
    Ok(())
}
