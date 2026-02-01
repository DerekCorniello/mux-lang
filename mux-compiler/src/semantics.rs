use crate::lexer::Span;
use crate::parser::PrimitiveType;
use crate::parser::*;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn format_type(t: &Type) -> String {
    match t {
        Type::Primitive(p) => format_primitive_type(p),
        Type::List(inner) => format!("[{}]", format_type(inner)),
        Type::Map(k, v) => format!("{{{}: {}}}", format_type(k), format_type(v)),
        Type::Set(inner) => format!("{{{}}}", format_type(inner)),
        Type::Optional(inner) => format!("Optional<{}>", format_type(inner)),
        Type::Reference(inner) => format!("&{}", format_type(inner)),
        Type::Void => "void".to_string(),
        Type::Never => "never".to_string(),
        Type::EmptyList => "[?]".to_string(),
        Type::EmptyMap => "{:}".to_string(),
        Type::EmptySet => "{?}".to_string(),
        Type::Function {
            params,
            returns,
            default_count: _,
        } => {
            let params_str = params
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({params_str}) -> {}", format_type(returns))
        }
        Type::Named(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let args_str = args.iter().map(format_type).collect::<Vec<_>>().join(", ");
                format!("{name}<{args_str}>")
            }
        }
        Type::Variable(name) | Type::Generic(name) => name.clone(),
        Type::Instantiated(name, args) => {
            let args_str = args.iter().map(format_type).collect::<Vec<_>>().join(", ");
            format!("{name}<{args_str}>")
        }
        Type::Module(name) => format!("module:{name}"),
    }
}

fn format_primitive_type(p: &PrimitiveType) -> String {
    match p {
        PrimitiveType::Int => "int".to_string(),
        PrimitiveType::Float => "float".to_string(),
        PrimitiveType::Bool => "bool".to_string(),
        PrimitiveType::Char => "char".to_string(),
        PrimitiveType::Str => "string".to_string(),
        PrimitiveType::Void => "void".to_string(),
        PrimitiveType::Auto => "auto".to_string(),
    }
}

pub fn format_binary_op(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "+".to_string(),
        BinaryOp::Subtract => "-".to_string(),
        BinaryOp::Multiply => "*".to_string(),
        BinaryOp::Divide => "/".to_string(),
        BinaryOp::Modulo => "%".to_string(),
        BinaryOp::Exponent => "**".to_string(),
        BinaryOp::Equal => "==".to_string(),
        BinaryOp::NotEqual => "!=".to_string(),
        BinaryOp::Less => "<".to_string(),
        BinaryOp::LessEqual => "<=".to_string(),
        BinaryOp::Greater => ">".to_string(),
        BinaryOp::GreaterEqual => ">=".to_string(),
        BinaryOp::LogicalAnd => "&&".to_string(),
        BinaryOp::LogicalOr => "||".to_string(),
        BinaryOp::In => "in".to_string(),
        BinaryOp::Assign => "=".to_string(),
        BinaryOp::AddAssign => "+=".to_string(),
        BinaryOp::SubtractAssign => "-=".to_string(),
        BinaryOp::MultiplyAssign => "*=".to_string(),
        BinaryOp::DivideAssign => "/=".to_string(),
        BinaryOp::ModuloAssign => "%=".to_string(),
    }
}

#[allow(unused)]
pub fn format_unary_op(op: &UnaryOp) -> String {
    match op {
        UnaryOp::Not => "!".to_string(),
        UnaryOp::Neg => "-".to_string(),
        UnaryOp::Ref => "&".to_string(),
        UnaryOp::Deref => "*".to_string(),
        UnaryOp::Incr => "++".to_string(),
        UnaryOp::Decr => "--".to_string(),
    }
}

pub fn format_span_location(span: &Span) -> String {
    let row = span.row_start;
    let col = span.col_start;
    format!("{row}:{col}")
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
    Type, // For generic type parameters
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub span: Span,
    pub type_: Option<Type>,
    pub interfaces: std::collections::HashMap<String, std::collections::HashMap<String, MethodSig>>,
    pub methods: std::collections::HashMap<String, MethodSig>,
    pub fields: std::collections::HashMap<String, (Type, bool)>, // (Type, is_const)
    pub type_params: Vec<(String, Vec<String>)>,
    pub original_name: Option<String>, // For imported symbols with aliases, stores the original source name
    pub llvm_name: Option<String>,     // For imported symbols, stores the mangled LLVM name
    pub default_param_count: usize, // Number of parameters with default values (must be at the end)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),

    Optional(Box<Type>),
    Reference(Box<Type>),
    Void,
    Never,
    EmptyList,
    EmptyMap,
    EmptySet,
    Function {
        params: Vec<Type>,
        returns: Box<Type>,
        default_count: usize, // Number of parameters with default values (must be at the end)
    },
    Named(String, Vec<Type>),
    Variable(String),
    Generic(String), // Generic parameter like "T", "U"
    // not sure why this is needed to be allowed,
    // i am using it in codegen
    #[allow(dead_code)]
    Instantiated(String, Vec<Type>), // Concrete instantiation like "Pair<string, bool>"
    Module(String), // Module namespace (e.g., "shapes" from "import shapes")
}

#[derive(Debug, Clone)]
pub struct GenericContext {
    pub type_params: HashMap<String, Type>, // T -> string, U -> bool
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodSig {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltInSig {
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, Default)]
pub struct Unifier {
    pub substitutions: std::collections::HashMap<String, Type>,
}

impl Unifier {
    pub fn new() -> Self {
        Self::default()
    }

    // unifies two types, returning an error with the provided span on mismatch.
    pub fn unify(&mut self, a: &Type, b: &Type, span: Span) -> Result<(), SemanticError> {
        match (a, b) {
            (Type::Variable(var), t) | (t, Type::Variable(var)) => {
                // If t is the same type variable, they're compatible
                if let Type::Variable(tvar) = t {
                    if tvar == var {
                        return Ok(());
                    }
                }
                if let Some(existing) = self.substitutions.get(var).cloned() {
                    self.unify(&existing, t, span)?;
                } else {
                    // occurs check, ensure var not in t (but not if t is the same variable)
                    if self.occurs(var, t) {
                        return Err(SemanticError {
                            message: format!(
                                "Recursive type: {} occurs in {}",
                                var,
                                format_type(t)
                            ),
                            span,
                        });
                    }
                    self.substitutions.insert(var.clone(), t.clone());
                }
            }
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => {}
            (Type::Named(n1, args1), Type::Named(n2, args2))
                if n1 == n2 && args1.len() == args2.len() =>
            {
                for (a1, a2) in args1.iter().zip(args2) {
                    self.unify(a1, a2, span)?;
                }
            }
            (
                Type::Function {
                    params: p1,
                    returns: r1,
                    ..
                },
                Type::Function {
                    params: p2,
                    returns: r2,
                    ..
                },
            ) if p1.len() == p2.len() => {
                for (a1, a2) in p1.iter().zip(p2) {
                    self.unify(a1, a2, span)?;
                }
                self.unify(r1, r2, span)?;
            }
            (Type::Reference(t1), Type::Reference(t2)) => self.unify(t1, t2, span)?,
            (Type::List(t1), Type::List(t2)) => self.unify(t1, t2, span)?,
            (Type::Map(k1, v1), Type::Map(k2, v2)) => {
                self.unify(k1, k2, span)?;
                self.unify(v1, v2, span)?;
            }
            (Type::Set(t1), Type::Set(t2)) => self.unify(t1, t2, span)?,

            (Type::Optional(t1), Type::Optional(t2)) => self.unify(t1, t2, span)?,
            (Type::Void, Type::Void) => {}
            (Type::EmptyList, Type::EmptyList) => {}
            (Type::EmptyMap, Type::EmptyMap) => {}
            (Type::EmptySet, Type::EmptySet) => {}
            (Type::List(_), Type::EmptyList) | (Type::EmptyList, Type::List(_)) => {}
            (Type::Map(_, _), Type::EmptyMap) | (Type::EmptyMap, Type::Map(_, _)) => {}
            (Type::Set(_), Type::EmptySet) | (Type::EmptySet, Type::Set(_)) => {}
            (Type::Map(_, _), Type::EmptySet) | (Type::EmptySet, Type::Map(_, _)) => {}
            (Type::Never, _) => {}
            (_, Type::Never) => {}
            _ => {
                return Err(SemanticError::new(
                    format!(
                        "Type mismatch: expected {}, got {}",
                        format_type(a),
                        format_type(b)
                    ),
                    span,
                ));
            }
        }
        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn occurs(&self, var: &str, t: &Type) -> bool {
        match t {
            Type::Variable(v) if v == var => true,
            Type::Named(_, args) => args.iter().any(|arg| self.occurs(var, arg)),
            Type::Function {
                params, returns, ..
            } => params.iter().any(|p| self.occurs(var, p)) || self.occurs(var, returns),
            Type::Reference(inner)
            | Type::List(inner)
            | Type::Set(inner)
            | Type::Optional(inner) => self.occurs(var, inner),
            Type::Map(k, v) => self.occurs(var, k) || self.occurs(var, v),

            _ => false,
        }
    }

    pub fn apply(&self, t: &Type) -> Type {
        match t {
            Type::Variable(var) => self
                .substitutions
                .get(var)
                .cloned()
                .unwrap_or_else(|| t.clone()),
            Type::Named(name, args) => {
                let applied_args: Vec<Type> = args.iter().map(|arg| self.apply(arg)).collect();
                Type::Named(name.clone(), applied_args)
            }
            Type::Function {
                params,
                returns,
                default_count,
            } => Type::Function {
                params: params.iter().map(|p| self.apply(p)).collect(),
                returns: Box::new(self.apply(returns)),
                default_count: *default_count,
            },
            Type::Reference(inner) => Type::Reference(Box::new(self.apply(inner))),
            Type::List(inner) => Type::List(Box::new(self.apply(inner))),
            Type::Map(k, v) => Type::Map(Box::new(self.apply(k)), Box::new(self.apply(v))),
            Type::Set(inner) => Type::Set(Box::new(self.apply(inner))),
            Type::Optional(inner) => Type::Optional(Box::new(self.apply(inner))),
            _ => t.clone(),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Rc<RefCell<Scope>>>,
    all_symbols: std::collections::HashMap<String, Symbol>,
}

#[derive(Debug, Default)]
struct Scope {
    symbols: HashMap<String, Symbol>,
    children: Vec<Rc<RefCell<Scope>>>,
}

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
            all_symbols: std::collections::HashMap::new(),
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

        let current = self.scopes.last().unwrap();
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

    // get reference to the symbol table for debugging.
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
        // Remove class type params from current_bounds
        if let Some(params) = &self.current_class_type_params {
            for (param, _) in params {
                self.current_bounds.remove(param);
            }
        }
        self.current_class_type_params = None;
    }

    // check if a name is a built-in function and return its signature
    fn get_builtin_sig(&self, name: &str) -> Option<&BuiltInSig> {
        BUILT_IN_FUNCTIONS.get(name)
    }

    pub fn analyze(&mut self, ast: &[AstNode]) -> Vec<SemanticError> {
        self.add_builtin_functions();
        if let Err(e) = self.collect_hoistable_declarations(ast) {
            self.errors.push(e);
        }
        self.analyze_nodes(ast);
        std::mem::take(&mut self.errors)
    }

    fn add_builtin_functions(&mut self) {
        // add built-in functions
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
                    },
                )
                .unwrap();
        }
    }

    // resolve a parsed typenode to a resolved type
    #[allow(clippy::only_used_in_recursion)]
    pub fn resolve_type(&self, type_node: &TypeNode) -> Result<Type, SemanticError> {
        match &type_node.kind {
            TypeKind::Primitive(prim) => match prim {
                crate::parser::PrimitiveType::Int => {
                    Ok(Type::Primitive(crate::parser::PrimitiveType::Int))
                }
                crate::parser::PrimitiveType::Float => {
                    Ok(Type::Primitive(crate::parser::PrimitiveType::Float))
                }
                crate::parser::PrimitiveType::Bool => {
                    Ok(Type::Primitive(crate::parser::PrimitiveType::Bool))
                }
                crate::parser::PrimitiveType::Char => {
                    Ok(Type::Primitive(crate::parser::PrimitiveType::Char))
                }
                crate::parser::PrimitiveType::Str => {
                    Ok(Type::Primitive(crate::parser::PrimitiveType::Str))
                }
                crate::parser::PrimitiveType::Void => Ok(Type::Void),
                crate::parser::PrimitiveType::Auto => Err(SemanticError {
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

                // for now, assume named types are classes/enums/interfaces
                // todo, implement full type definition resolution for enums/interfaces (currently only handles generics).
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

    // get the type of an expression
    pub fn get_expression_type(&mut self, expr: &ExpressionNode) -> Result<Type, SemanticError> {
        match &expr.kind {
            ExpressionKind::Literal(lit) => match lit {
                LiteralNode::Integer(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Int)),
                LiteralNode::Float(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Float)),
                LiteralNode::String(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Str)),
                LiteralNode::Boolean(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Bool)),
                LiteralNode::Char(_) => Ok(Type::Primitive(crate::parser::PrimitiveType::Char)),
            },
            ExpressionKind::None => Ok(Type::Optional(Box::new(Type::Never))),
            ExpressionKind::Identifier(name) => {
                if name == "self" {
                    // For 'self', use the current self type if available
                    if let Some(self_type) = &self.current_self_type {
                        Ok(self_type.clone())
                    } else {
                        // For now, return a placeholder type to allow analysis to continue
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
                        // Return the actual type of the variable (including Reference types)
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

                if *op == crate::parser::BinaryOp::Assign {
                    // special handling for assignment
                    if let crate::parser::ExpressionKind::Identifier(name) = &left.kind {
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
                    } else if let crate::parser::ExpressionKind::FieldAccess {
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

                                    // Type check
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
                    } else if let crate::parser::ExpressionKind::Unary {
                        op: crate::parser::UnaryOp::Deref,
                        op_span: _,
                        expr: _,
                        postfix: _,
                    } = &left.kind
                    {
                        // Dereference assignment is allowed (e.g., *p = 20)
                        // Type compatibility already checked via left_type and right_type
                        // No const checking needed - you can't have a const pointer target
                    } else {
                        return Err(SemanticError {
                            message: "Assignment to non-identifier is not supported".into(),
                            span: expr.span,
                        });
                    }
                    Ok(right_type) // assignment returns the assigned value
                } else if matches!(
                    op,
                    crate::parser::BinaryOp::AddAssign
                        | crate::parser::BinaryOp::SubtractAssign
                        | crate::parser::BinaryOp::MultiplyAssign
                        | crate::parser::BinaryOp::DivideAssign
                        | crate::parser::BinaryOp::ModuloAssign
                ) {
                    // Compound assignment operators - check for const
                    if let crate::parser::ExpressionKind::Identifier(name) = &left.kind {
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
                    } else if let crate::parser::ExpressionKind::FieldAccess {
                        expr: obj_expr,
                        field,
                    } = &left.kind
                    {
                        // Check if field is const
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

                                    // Type check
                                    let base_op = match op {
                                        crate::parser::BinaryOp::AddAssign => {
                                            crate::parser::BinaryOp::Add
                                        }
                                        crate::parser::BinaryOp::SubtractAssign => {
                                            crate::parser::BinaryOp::Subtract
                                        }
                                        crate::parser::BinaryOp::MultiplyAssign => {
                                            crate::parser::BinaryOp::Multiply
                                        }
                                        crate::parser::BinaryOp::DivideAssign => {
                                            crate::parser::BinaryOp::Divide
                                        }
                                        crate::parser::BinaryOp::ModuloAssign => {
                                            crate::parser::BinaryOp::Modulo
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
                    } else if let crate::parser::ExpressionKind::Unary {
                        op: crate::parser::UnaryOp::Deref,
                        op_span: _,
                        expr: _,
                        postfix: _,
                    } = &left.kind
                    {
                        // Dereference compound assignment is allowed (e.g., *p += 5)
                        // Type checking happens below via resolve_binary_operator
                    }

                    // Type check compound assignment
                    let base_op = match op {
                        crate::parser::BinaryOp::AddAssign => crate::parser::BinaryOp::Add,
                        crate::parser::BinaryOp::SubtractAssign => {
                            crate::parser::BinaryOp::Subtract
                        }
                        crate::parser::BinaryOp::MultiplyAssign => {
                            crate::parser::BinaryOp::Multiply
                        }
                        crate::parser::BinaryOp::DivideAssign => crate::parser::BinaryOp::Divide,
                        crate::parser::BinaryOp::ModuloAssign => crate::parser::BinaryOp::Modulo,
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
                UnaryOp::Not => Ok(Type::Primitive(crate::parser::PrimitiveType::Bool)),
                UnaryOp::Neg => {
                    let operand_type = self.get_expression_type(expr)?;
                    match operand_type {
                        Type::Primitive(crate::parser::PrimitiveType::Int)
                        | Type::Primitive(crate::parser::PrimitiveType::Float) => Ok(operand_type),
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
                    if let crate::parser::ExpressionKind::Identifier(name) = &expr.kind {
                        if let Some(symbol) = self.symbol_table.lookup(name) {
                            if symbol.kind == SymbolKind::Constant {
                                return Err(SemanticError::with_help(
                                    format!("Cannot modify constant '{}'", name),
                                    *op_span,
                                    "Constants cannot be modified after initialization",
                                ));
                            }
                        }
                    } else if let crate::parser::ExpressionKind::FieldAccess {
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
                    } else if let crate::parser::ExpressionKind::Unary {
                        op: crate::parser::UnaryOp::Deref,
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
                        Type::Primitive(crate::parser::PrimitiveType::Int) => Ok(operand_type),
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
                                "Unknown method '{}' on type {}",
                                field,
                                format_type(&expr_type)
                            ),
                            span: expr.span,
                        })
                    }
                } else {
                    Err(SemanticError {
                        message: format!(
                            "Unknown method '{}' on type {}",
                            field,
                            format_type(&expr_type)
                        ),
                        span: expr.span,
                    })
                }
            }
            ExpressionKind::ListAccess { expr, index: _ } => {
                // list access returns direct element_type (runtime error if out of bounds)
                // Use .get() method for safe Optional access
                let list_type = self.get_expression_type(expr)?;
                match list_type {
                    Type::List(elem_type) => Ok(*elem_type),
                    _ => Err(SemanticError::with_help(
                        "Cannot index non-list type",
                        expr.span,
                        "Only lists can be indexed with '[]'. Example: my_list[0]",
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
                                    format_type(&first_type), index, format_type(&element_type)
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

                // First time analyzing this lambda - do full analysis
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
                        },
                    )?;
                }
                self.analyze_block(body)?;

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
                    match &body.last().unwrap().kind {
                        StatementKind::Expression(expr) => self.get_expression_type(expr)?,
                        StatementKind::Return(Some(expr)) => self.get_expression_type(expr)?,
                        _ => Type::Void,
                    }
                };
                self.symbol_table.pop_scope()?;
                // Count default parameters in lambda
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
                // check if the generic type name exists.
                if !self.symbol_table.exists(name) {
                    return Err(SemanticError {
                        message: format!("Undefined type '{}'", name),
                        span: expr.span,
                    });
                }
                let resolved_args = type_args
                    .iter()
                    .map(|arg| self.resolve_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Named(name.clone(), resolved_args))
            }
        }
    }

    // check if two types are compatible for assignment
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

    // check if a type implements a specific operator interface
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

    // get method signature for built-in interfaces (used by generic type parameters)
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

    // get method signature for a type and method name
    fn get_method_sig(&self, type_: &Type, method_name: &str) -> Option<MethodSig> {
        match type_ {
            Type::Named(name, args) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    // check direct methods
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
                    // check implemented interfaces
                    for interface_methods in symbol.interfaces.values() {
                        if let Some(sig) = interface_methods.get(method_name) {
                            return Some(sig.clone());
                        }
                    }
                }
                None
            }
            Type::Variable(var) | Type::Generic(var) => {
                // for generics, check bounds
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
                // built-in methods for primitives
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
                "push_front" => Some(MethodSig {
                    params: vec![*elem_type.clone()],
                    return_type: Type::Void,
                    is_static: false,
                }),
                "pop" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Optional(elem_type.clone()),
                    is_static: false,
                }),
                "pop_front" => Some(MethodSig {
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
                "length" => Some(MethodSig {
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

    // resolve binary operator for given operand types
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
                    Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
                }
                Type::Map(key_type, _) => {
                    // For map, left operand should be compatible with key type
                    if left_type == key_type.as_ref() {
                        Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
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
                        Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
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
                    Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
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
                        Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
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
                            },
                        );
                    }

                    // collect field types for constructor and fields map
                    let mut field_types = Vec::new();
                    let mut fields_map = std::collections::HashMap::new();
                    for field in fields {
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
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Enum { name, variants, .. } => {
                    let mut methods = std::collections::HashMap::new();
                    for variant in variants {
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
            AstNode::Function(func) => {
                if func.is_common {
                    return Err(SemanticError {
                        message: "Common methods are only allowed in classes".to_string(),
                        span: func.span,
                    });
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
            AstNode::Statement(stmt) => self.analyze_statement(stmt),
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
                },
            )?;
        }

        // add parameters to function scope.
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
                },
            )?;
        }

        // analyze function body with new scope.
        self.analyze_block(&func.body)?;

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

    fn analyze_block(&mut self, stmts: &[StatementNode]) -> Result<(), SemanticError> {
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
                    },
                )?;
            }
            StatementKind::Expression(expr) => {
                self.analyze_expression(expr)?;
            }
            StatementKind::Block(stmts) => {
                self.symbol_table.push_scope()?;
                self.analyze_block(stmts)?;
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
                self.analyze_block(then_block)?;
                self.symbol_table.pop_scope()?;

                if let Some(else_block) = else_block {
                    self.symbol_table.push_scope()?;
                    self.analyze_block(else_block)?;
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
                    },
                )?;
                self.analyze_block(body)?;
                self.symbol_table.pop_scope()?;
            }
            StatementKind::While { cond, body } => {
                self.analyze_expression(cond)?;

                self.symbol_table.push_scope()?;
                self.analyze_block(body)?;
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
                    self.analyze_block(&arm.body)?;

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
                return Ok(());
            }
            StatementKind::Return(Some(expr)) => {
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
                use crate::parser::ImportSpec;

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

                let module_nodes = resolver
                    .borrow_mut()
                    .resolve_import_path(module_path, self.current_file.as_deref())
                    .map_err(|e| SemanticError {
                        message: format!("Import error: {}", e),
                        span: stmt.span,
                    })?;

                // Analyze the imported module
                let mut module_analyzer = SemanticAnalyzer::new_for_module(resolver.clone());
                module_analyzer.set_current_file(std::path::PathBuf::from(
                    module_path.replace('.', "/") + ".mux",
                ));
                let errors = module_analyzer.analyze(&module_nodes);
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
                                    Some(format!("{}_{}", module_name_for_mangling, name));
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
                            Type::Primitive(crate::parser::PrimitiveType::Bool)
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
                            Type::Primitive(crate::parser::PrimitiveType::Int)
                                | Type::Primitive(crate::parser::PrimitiveType::Float)
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
                            Type::Primitive(crate::parser::PrimitiveType::Int)
                        ) {
                            return Err(SemanticError {
                                message: "Increment/decrement operators require an int operand"
                                    .into(),
                                span: *op_span,
                            });
                        }

                        // Check if trying to modify a constant
                        if let crate::parser::ExpressionKind::Identifier(name) = &expr.kind {
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

                        if let crate::parser::ExpressionKind::FieldAccess {
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
                // type check list access
                let list_type = self.get_expression_type(expr)?;
                match list_type {
                    Type::List(_) => {}
                    _ => {
                        return Err(SemanticError::with_help(
                            "Cannot index non-list type",
                            expr.span,
                            "Only lists can be indexed with '[]'. Example: my_list[0]",
                        ));
                    }
                }
                let index_type = self.get_expression_type(index)?;
                if !matches!(
                    index_type,
                    Type::Primitive(crate::parser::PrimitiveType::Int)
                ) {
                    return Err(SemanticError {
                        message: "List index must be an integer".into(),
                        span: index.span,
                    });
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
                if !matches!(
                    cond_type,
                    Type::Primitive(crate::parser::PrimitiveType::Bool)
                ) {
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
                        },
                    )?;
                }

                self.analyze_block(body)?;

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
                // check if the generic type name exists.
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
                mangled_symbol.llvm_name = Some(format!("{}_{}", module_name_for_mangling, name));
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
            imported_symbol.llvm_name = Some(format!("{}_{}", module_name_for_mangling, item_name));
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
                        Some(format!("{}_{}", module_name_for_mangling, name));
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
        spec: &crate::parser::ImportSpec,
        span: Span,
    ) -> Result<(), SemanticError> {
        use crate::parser::ImportSpec;

        match spec {
            ImportSpec::Module { alias } => {
                let symbol_name = alias
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or_else(|| module_path.split('.').last().unwrap());

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

// represents a semantic error with location information
#[derive(Debug, Clone, PartialEq)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

impl SemanticError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn with_help(message: impl Into<String>, span: Span, help: impl Into<String>) -> Self {
        Self {
            message: format!("{}\n  = help: {}", message.into(), help.into()),
            span,
        }
    }

    #[allow(unused)]
    pub fn with_suggestion(message: impl Into<String>, span: Span, suggestion: &str) -> Self {
        Self {
            message: format!("{}\n  = help: {}", message.into(), suggestion),
            span,
        }
    }
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Semantic error at {}: {}",
            format_span_location(&self.span),
            self.message
        )
    }
}

impl std::error::Error for SemanticError {}
