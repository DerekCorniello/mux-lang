use crate::lexer::Span;
use crate::parser::PrimitiveType;
use crate::parser::*;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Function,
    Variable,
    Class,
    Interface,
    Enum,
    Constant,
    Import,
    Type,  // For generic type parameters
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub span: Span,
    pub type_: Option<Type>,
    pub interfaces: std::collections::HashMap<String, std::collections::HashMap<String, MethodSig>>,
    pub methods: std::collections::HashMap<String, MethodSig>,
    pub fields: std::collections::HashMap<String, Type>,
    pub type_params: Vec<(String, Vec<String>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Tuple(Vec<Type>),
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
    },
    Named(String, Vec<Type>),
    Variable(String),
    Generic(String),  // Generic parameter like "T", "U"
    // not sure why this is needed to be allowed,
    // i am using it in codegen
    #[allow(dead_code)]
    Instantiated(String, Vec<Type>),  // Concrete instantiation like "Pair<string, bool>"
}

#[derive(Debug, Clone)]
pub struct GenericContext {
    pub type_params: HashMap<String, Type>,  // T -> string, U -> bool
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
                if let Some(existing) = self.substitutions.get(var).cloned() {
                    self.unify(&existing, t, span)?;
                } else {
                    // occurs check, ensure var not in t
                    if self.occurs(var, t) {
                        return Err(SemanticError {
                            message: format!("Recursive type: {} occurs in {:?}", var, t),
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
                },
                Type::Function {
                    params: p2,
                    returns: r2,
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
            (Type::Tuple(ts1), Type::Tuple(ts2)) if ts1.len() == ts2.len() => {
                for (t1, t2) in ts1.iter().zip(ts2) {
                    self.unify(t1, t2, span)?;
                }
            }
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
                return Err(SemanticError {
                    message: format!("Type mismatch: {:?} vs {:?}", a, b),
                    span,
                });
            }
        }
        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn occurs(&self, var: &str, t: &Type) -> bool {
        match t {
            Type::Variable(v) if v == var => true,
            Type::Named(_, args) => args.iter().any(|arg| self.occurs(var, arg)),
            Type::Function { params, returns } => {
                params.iter().any(|p| self.occurs(var, p)) || self.occurs(var, returns)
            }
            Type::Reference(inner)
            | Type::List(inner)
            | Type::Set(inner)
            | Type::Optional(inner) => self.occurs(var, inner),
            Type::Map(k, v) => self.occurs(var, k) || self.occurs(var, v),
            Type::Tuple(ts) => ts.iter().any(|t| self.occurs(var, t)),
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
            Type::Named(name, args) => Type::Named(
                name.clone(),
                args.iter().map(|arg| self.apply(arg)).collect(),
            ),
            Type::Function { params, returns } => Type::Function {
                params: params.iter().map(|p| self.apply(p)).collect(),
                returns: Box::new(self.apply(returns)),
            },
            Type::Reference(inner) => Type::Reference(Box::new(self.apply(inner))),
            Type::List(inner) => Type::List(Box::new(self.apply(inner))),
            Type::Map(k, v) => Type::Map(Box::new(self.apply(k)), Box::new(self.apply(v))),
            Type::Set(inner) => Type::Set(Box::new(self.apply(inner))),
            Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| self.apply(t)).collect()),
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
        m.insert("println", BuiltInSig {
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
        SymbolTable { scopes: vec![root], all_symbols: std::collections::HashMap::new() }
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
                span: Span::new(0, 0), // Internal error, no user span available
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
                span: Span::new(0, 0), // Internal error, no user span available
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

        current_borrow.symbols.insert(name.to_string(), symbol.clone());
        // Don't add 'self' to global symbol table - it should only exist in local scope
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
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();
        Self {
            symbol_table,
            current_bounds: std::collections::HashMap::new(),
            errors: Vec::new(),
            is_in_static_method: false,
            current_self_type: None,
        }
    }

    // get reference to the symbol table for debugging.
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn all_symbols(&self) -> &std::collections::HashMap<String, Symbol> {
        &self.symbol_table.all_symbols
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
            (
                "println",
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
                    message: "'auto' type not allowed in this context".into(),
                    span: type_node.span,
                }),
            },
            TypeKind::Named(name, type_args) => {
                // Handle built-in generic types
                if name == "Optional" && type_args.len() == 1 {
                    let resolved_arg = self.resolve_type(&type_args[0])?;
                    return Ok(Type::Optional(Box::new(resolved_arg)));
                } else if name == "Result" && type_args.len() == 2 {
                    let resolved_ok = self.resolve_type(&type_args[0])?;
                    let resolved_err = self.resolve_type(&type_args[1])?;
                    return Ok(Type::Named("Result".to_string(), vec![resolved_ok, resolved_err]));
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
                        println!("DEBUG: get_expression_type: Found self type: {:?} at {:?}", self_type, expr.span);
                        Ok(self_type.clone())
                    } else {
                        println!("DEBUG: get_expression_type: No current self type available at {:?}, is_in_static_method: {}", expr.span, self.is_in_static_method);
                        // For now, return a placeholder type to allow analysis to continue
                        Ok(Type::Named("Unknown".to_string(), vec![]))
                    }
                } else {
                    // For other identifiers, check current scope first, then global symbols
                    let symbol = self.symbol_table.get_cloned(name)
                        .or_else(|| self.symbol_table.lookup(name));

                    if let Some(symbol) = symbol {
                        let type_ = symbol.type_.clone().ok_or_else(|| SemanticError {
                            message: format!("Symbol '{}' has no type information", name),
                            span: expr.span,
                        })?;
                        // Return the actual type of the variable (including Reference types)
                        Ok(type_)
                    } else if let Some(sig) = self.get_builtin_sig(name) {
                        Ok(Type::Function {
                            params: sig.params.clone(),
                            returns: Box::new(sig.return_type.clone()),
                        })
                    } else {
                        Err(SemanticError {
                            message: format!("undefined variable '{}'", name),
                            span: expr.span,
                        })
                    }
                }
            }
            ExpressionKind::Binary { left, right, op } => {
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
                        let var_type = symbol.type_.as_ref().ok_or_else(|| SemanticError {
                            message: format!("Variable '{}' has no type information", name),
                            span: left.span,
                        })?;
                        if let Type::Reference(inner) = var_type {
                            self.check_type_compatibility(inner, &right_type, expr.span)?;
                        } else {
                            self.check_type_compatibility(var_type, &right_type, expr.span)?;
                        }
                    } else {
                        return Err(SemanticError {
                            message: "Assignment to non-identifier not supported".into(),
                            span: expr.span,
                        });
                    }
                    Ok(right_type) // assignment returns the assigned value
                } else if let Some(result_type) =
                    self.resolve_binary_operator(&left_type, &right_type, op)
                {
                    Ok(result_type)
                } else {
                    Err(SemanticError {
                        message: format!(
                            "Binary operator {:?} not supported for types {:?} and {:?}",
                            op, left_type, right_type
                        ),
                        span: expr.span,
                    })
                }
            }
            ExpressionKind::Unary { expr, op, .. } => match op {
                UnaryOp::Not => Ok(Type::Primitive(crate::parser::PrimitiveType::Bool)),
                UnaryOp::Neg => {
                    let operand_type = self.get_expression_type(expr)?;
                    match operand_type {
                        Type::Primitive(crate::parser::PrimitiveType::Int)
                        | Type::Primitive(crate::parser::PrimitiveType::Float) => Ok(operand_type),
                        _ => Err(SemanticError {
                            message: "Negation operator requires numeric operand".into(),
                            span: expr.span,
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
                            message: "Cannot dereference non-reference type".into(),
                            span: expr.span,
                        })
                    }
                }
                _ => Err(SemanticError {
                    message: format!("Unsupported unary operator {:?}", op),
                    span: expr.span,
                }),
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
                    Type::Function { params, returns } => {
                        if args.len() != params.len() {
                            return Err(SemanticError {
                                message: format!(
                                    "Function expects {} arguments, got {}",
                                    params.len(),
                                    args.len()
                                ),
                                span: expr.span,
                            });
                        }
                        let mut unifier = Unifier::new();
                        for (param, arg) in params.iter().zip(args) {
                            let arg_type = self.get_expression_type(arg)?;
                            unifier.unify(param, &arg_type, expr.span)?;
                        }
                        let unified_returns = unifier.apply(&returns);
                        Ok(unified_returns)
                    }
                    _ => Err(SemanticError {
                        message: "Cannot call non-function type".into(),
                        span: expr.span,
                    }),
                }
            }
            ExpressionKind::FieldAccess { expr, field } => {
                let expr_type = self.get_expression_type(expr)?;
                if let Some(method_sig) = self.get_method_sig(&expr_type, field) {
                    Ok(Type::Function {
                        params: method_sig.params,
                        returns: Box::new(method_sig.return_type),
                    })
                } else if let Type::Named(name, args) = &expr_type {
                    if let Some(symbol) = self.symbol_table.lookup(name) {
                        if let Some(field_type) = symbol.fields.get(field) {
                            let substituted = self.substitute_type_params(field_type, &symbol.type_params, args);
                            Ok(substituted)
                        } else {
                            Err(SemanticError {
                                message: format!("Unknown field '{}' on type {:?}", field, expr_type),
                                span: expr.span,
                            })
                        }
                    } else {
                        Err(SemanticError {
                            message: format!("Unknown method '{}' on type {:?}", field, expr_type),
                            span: expr.span,
                        })
                    }
                } else {
                    Err(SemanticError {
                        message: format!("Unknown method '{}' on type {:?}", field, expr_type),
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
                    _ => Err(SemanticError {
                        message: "Cannot index non-list type".into(),
                        span: expr.span,
                    }),
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
                        if self.check_type_compatibility(&first_type, &element_type, element.span).is_err() {
                            return Err(SemanticError {
                                message: format!(
                                    "Heterogeneous list: expected all elements to be of type {:?}, but element at index {} has type {:?}",
                                    first_type, index, element_type
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
                    Err(SemanticError {
                        message: "If expression branches must have the same type".into(),
                        span: expr.span,
                    })
                }
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
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                        },
                    )?;
                }
                self.analyze_block(body)?;
                self.symbol_table.pop_scope()?;
                
                let param_types = params
                    .iter()
                    .map(|p| self.resolve_type(&p.type_))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_type = if body.is_empty() {
                    Type::Void
                } else {
                    match &body.last().unwrap().kind {
                        StatementKind::Expression(expr) => self.get_expression_type(expr)?,
                        StatementKind::Return(Some(expr)) => self.get_expression_type(expr)?,
                        _ => Type::Void,
                    }
                };
                Ok(Type::Function {
                    params: param_types,
                    returns: Box::new(return_type),
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
                        message: format!("undefined type '{}'", name),
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
        temp_unifier
            .unify(expected, actual, span)
            .map_err(|_| SemanticError {
                message: format!("Type mismatch: expected {:?}, got {:?}", expected, actual),
                span,
            })
    }

    #[allow(clippy::only_used_in_recursion)]
    fn substitute_type_param(&self, type_: &Type, param: &str, replacement: &Type) -> Type {
        match type_ {
            Type::Variable(var) if var == param => replacement.clone(),
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
            Type::Function { params, returns } => Type::Function {
                params: params
                    .iter()
                    .map(|p| self.substitute_type_param(p, param, replacement))
                    .collect(),
                returns: Box::new(self.substitute_type_param(returns, param, replacement)),
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
                message: format!("Return type mismatch in interface implementation: {}", e.message),
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
            unifier.unify(int_param, class_param, span).map_err(|e| SemanticError {
                message: format!("Parameter {} type mismatch in interface implementation: {}", i, e.message),
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
            Type::Primitive(prim) => {
                // built-in types implement certain interfaces
                match prim {
                    PrimitiveType::Str => interface_name == "Add",
                    PrimitiveType::Int => {
                        matches!(interface_name, "Add" | "Sub" | "Mul" | "Div" | "Arithmetic")
                    }
                    PrimitiveType::Float => {
                        matches!(interface_name, "Add" | "Sub" | "Mul" | "Div" | "Arithmetic")
                    }
                    _ => false,
                }
            }
            _ => false,
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
            Type::Variable(var) => {
                // for generics, check bounds
                if let Some(bounds) = self.current_bounds.get(var) {
                    for bound in bounds {
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
                        _ => None,
                    },
                    PrimitiveType::Str => match method_name {
                        "to_string" | "length" => Some(MethodSig {
                            params: vec![],
                            return_type: if method_name == "to_string" {
                                Type::Primitive(PrimitiveType::Str)
                            } else {
                                Type::Primitive(PrimitiveType::Int)
                            },
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
                        _ => None,
                    },
                    PrimitiveType::Char => match method_name {
                        "to_string" => Some(MethodSig {
                            params: vec![],
                            return_type: Type::Primitive(PrimitiveType::Str),
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
                    if matches!(left_type, Type::Primitive(PrimitiveType::Char | PrimitiveType::Str)) {
                        Some(Type::Primitive(crate::parser::PrimitiveType::Bool))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } else {
            // For all other operators, types must be compatible
            if left_type != right_type {
                return None;
            }

            match op {
            BinaryOp::Add => {
                // built-in support for primitives, or interface support for custom types
                if matches!(
                    left_type,
                    Type::Primitive(PrimitiveType::Str | PrimitiveType::Int | PrimitiveType::Float)
                ) || self.type_implements_interface(left_type, "Add")
                {
                    Some(left_type.clone())
                } else {
                    None
                }
            }
            BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
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
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                // comparison operators for ordered types
                if matches!(
                    left_type,
                    Type::Primitive(PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Str)
                ) {
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
    }

    // first pass, collect hoistable declarations like functions and classes.
    fn collect_hoistable_declarations(&mut self, ast: &[AstNode]) -> Result<(), SemanticError> {
        for node in ast {
            match node {
                AstNode::Function(func) => {
                    if func.is_common {
                        return Err(SemanticError {
                            message: "common methods are only allowed in classes".to_string(),
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
                    };

                    // substitute type params with variables
                    for (type_param_name, _) in &func.type_params {
                        let var = Type::Variable(type_param_name.clone());
                        func_type = self.substitute_type_param(&func_type, type_param_name, &var);
                    }

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
                                        .map(|p| self.substitute_type_params(p, interface_type_params, &resolved_args))
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
                                implemented_interfaces.insert(trait_ref.name.clone(), substituted_methods);
                            }
                        }
                    }
                    let type_param_bounds: Vec<(String, Vec<String>)> = type_params
                        .iter()
                        .map(|(p, b)| (p.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
                        .collect();

                    // collect field types for constructor and fields map
                    let mut field_types = Vec::new();
                    let mut fields_map = std::collections::HashMap::new();
                    for field in fields {
                        match self.resolve_type(&field.type_) {
                            Ok(t) => {
                                field_types.push(t.clone());
                                fields_map.insert(field.name.clone(), t);
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
                        params: vec![],  // Default constructor takes no args
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
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Enum { name, variants, .. } => {
                    let mut methods = std::collections::HashMap::new();
                    for variant in variants {
                        let params = variant.data.clone().unwrap_or_default().into_iter().map(|t| self.resolve_type(&t).unwrap_or(Type::Void)).collect();
                        let return_type = Type::Named(name.clone(), vec![]);
                        methods.insert(variant.name.clone(), MethodSig { params, return_type, is_static: true });
                        println!("Added method {} to {}", variant.name, name);
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
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
                AstNode::Interface {
                    name,
                    type_params,
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
                            fields: std::collections::HashMap::new(),
                            type_params: type_params
                                .iter()
                                .map(|(p, b)| {
                                    (p.clone(), b.iter().map(|tb| tb.name.clone()).collect())
                                })
                                .collect::<Vec<(String, Vec<String>)>>(),
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
                        message: "common methods are only allowed in classes".to_string(),
                        span: func.span,
                    });
                }
                self.analyze_function(func, None)
            },
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
            },
            AstNode::Enum { .. } => Ok(()), // enums don't need further analysis.
            AstNode::Interface { .. } => Ok(()), // interfaces don't need further analysis.
            AstNode::Statement(stmt) => self.analyze_statement(stmt),
        }
    }

    fn analyze_function(&mut self, func: &FunctionNode, self_type: Option<Type>) -> Result<(), SemanticError> {
        let was_static = self.is_in_static_method;
        let old_self_type = self.current_self_type.clone();
        self.is_in_static_method = func.is_common;
        self.current_self_type = self_type.clone();
        println!("DEBUG: analyze_function {} with self_type: {:?}, current_self_type set to: {:?}", func.name, self_type, self.current_self_type);

        // set current bounds
        self.current_bounds.clear();
        for (param, bounds) in &func.type_params {
            let bound_names = bounds.iter().map(|b| b.name.clone()).collect();
            self.current_bounds.insert(param.clone(), bound_names);
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
                },
            )?;
        }

        // analyze function body with new scope.
        let result = self.analyze_block(&func.body);

        // clean up function scope.
        self.symbol_table.pop_scope()?;
        self.current_bounds.clear();
        self.is_in_static_method = was_static;
        self.current_self_type = old_self_type;
        result
    }

    fn analyze_class(
        &mut self,
        name_: &str,
        fields: &[Field],
        methods: &[FunctionNode],
        type_params: &[(String, Vec<String>)],
    ) -> Result<(), SemanticError> {
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
                    interfaces: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    fields: std::collections::HashMap::new(),
                    type_params: Vec::new(),
                },
            )?;
        }

        // add methods to class scope and analyze their bodies.
        for method in methods {
            // resolve method type
            let param_types = method
                .params
                .iter()
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
                    interfaces: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    fields: std::collections::HashMap::new(),
                    type_params: Vec::new(),
                },
            )?;
            let self_type = if method.is_common {
                None
            } else {
                Some(Type::Named(name_.to_string(), type_params.iter().map(|(p, _)| Type::Variable(p.clone())).collect()))
            };
            // Set current_self_type for method body analysis
            let old_self_type = self.current_self_type.clone();
            self.current_self_type = self_type.clone();
            self.analyze_function(method, self_type)?;
            // Restore previous self type
            self.current_self_type = old_self_type;
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
                        type_: None, // will be resolved later
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
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
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
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
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                    },
                )?;
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.analyze_expression(cond)?;
                self.analyze_block(then_block)?;
                if let Some(else_block) = else_block {
                    self.analyze_block(else_block)?;
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
                let expr_type = self.get_expression_type(expr)?;
                for arm in arms {
                    self.symbol_table.push_scope()?;
                    // set types for pattern variables based on expr_type
                    self.set_pattern_types(&arm.pattern, &expr_type, expr.span)?;
                    self.analyze_pattern(&arm.pattern)?;
                    if let Some(guard) = &arm.guard {
                        self.analyze_expression(guard)?;
                    }
                    self.analyze_block(&arm.body)?;
                    self.symbol_table.pop_scope()?;
                }
            }
            StatementKind::Return(Some(expr)) => {
                self.analyze_expression(expr)?;
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
                let (kind, type_) = if module_path.starts_with("std.") {
                    if let Some(sig) = self.get_builtin_sig(symbol_name) {
                        (
                            SymbolKind::Function,
                            Some(Type::Function {
                                params: sig.params.clone(),
                                returns: Box::new(sig.return_type.clone()),
                            }),
                        )
                    } else if symbol_name == "None" {
                        (
                            SymbolKind::Constant,
                            Some(Type::Optional(Box::new(Type::Void))),
                        )
                    } else {
                        (SymbolKind::Variable, None) // treat as variables/constants for simplicity.
                    }
                } else {
                    (SymbolKind::Import, None)
                };

                self.symbol_table.add_symbol(
                    symbol_name,
                    Symbol {
                        kind,
                        span: stmt.span,
                        type_,
                        interfaces: std::collections::HashMap::new(),
                        methods: std::collections::HashMap::new(),
                        fields: std::collections::HashMap::new(),
                        type_params: Vec::new(),
                    },
                )?;
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
                                    "Pattern {} does not match type {:?}",
                                    name, expected_type
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
                                    "Pattern {} does not match type {:?}",
                                    name, expected_type
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
                                            name, args.len(), sig.params.len()
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
                                "Enum variant patterns are not supported for type {:?}",
                                expected_type
                            ),
                            span,
                        });
                    }
                }
            }
            PatternNode::Literal(_) => {} // literals don't bind variables
            PatternNode::Wildcard => {}   // no binding
            PatternNode::Tuple(elements) => {
                if let Type::Tuple(expected_elements) = expected_type {
                    if elements.len() != expected_elements.len() {
                        return Err(SemanticError {
                            message: "Tuple pattern length mismatch".to_string(),
                            span,
                        });
                    }
                    for (elem, expected) in elements.iter().zip(expected_elements) {
                        self.set_pattern_types(elem, expected, span)?;
                    }
                } else {
                    return Err(SemanticError {
                        message: format!("Tuple pattern on non-tuple type {:?}", expected_type),
                        span,
                    });
                }
            }
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
                if name == "self" {
                    if self.is_in_static_method {
                        return Err(SemanticError {
                            message: "cannot use 'self' in common method".to_string(),
                            span: expr.span,
                        });
                    }
                    // For 'self', check if we have a current self type
                    if self.current_self_type.is_none() {
                        return Err(SemanticError {
                            message: "cannot use 'self' outside of method".to_string(),
                            span: expr.span,
                        });
                    }
                    return Ok(());
                }
                if !self.symbol_table.exists(name) && self.get_builtin_sig(name).is_none() {
                    return Err(SemanticError {
                        message: format!("undefined variable '{}'", name),
                        span: expr.span,
                    });
                }
                Ok(())
            }
            ExpressionKind::Literal(_) => Ok(()), // literals are fine
            ExpressionKind::None => Ok(()), // None is fine
            ExpressionKind::Binary { left, right, op: _ } => {
                self.analyze_expression(left)?;
                self.analyze_expression(right)?;
                // type checking is now handled in get_expression_type via resolve_binary_operator
                Ok(())
            }
            ExpressionKind::Unary {
                expr,
                op,
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
                                message: "Logical not operator requires boolean operand".into(),
                                span: expr.span,
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
                                message: "Negation operator requires numeric operand".into(),
                                span: expr.span,
                            });
                        }
                    }
                    UnaryOp::Ref => {
                        // reference operator, operand can be any type
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
                // type check is done in get_expression_type when called
                Ok(())
            }
            ExpressionKind::FieldAccess { expr, .. } => {
                self.analyze_expression(expr)?;
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
                        return Err(SemanticError {
                            message: "Cannot index non-list type".into(),
                            span: expr.span,
                        });
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
                            interfaces: std::collections::HashMap::new(),
                            methods: std::collections::HashMap::new(),
                            fields: std::collections::HashMap::new(),
                            type_params: Vec::new(),
                        },
                    )?;
                }
                self.analyze_block(body)?;
                self.symbol_table.pop_scope()?;
                Ok(())
            }
            // Instantiate generic types (e.g., Stack<int>)
            ExpressionKind::GenericType(name, _) => {
                // check if the generic type name exists.
                if !self.symbol_table.exists(name) {
                    return Err(SemanticError {
                        message: format!("undefined type '{}'", name),
                        span: expr.span,
                    });
                }
                Ok(())
            }
        }
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
