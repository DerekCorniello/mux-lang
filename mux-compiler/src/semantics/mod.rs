// Module declarations

pub mod error;
pub mod format;
pub mod stdlib;
pub mod symbol_table;
pub mod types;
pub mod unifier;

// Re-exports for public API
pub use error::SemanticError;
pub use format::{format_binary_op, format_type};
pub use symbol_table::SymbolTable;
pub use types::{BuiltInSig, GenericContext, MethodSig, Symbol, SymbolKind, Type};
pub use unifier::Unifier;

// Internal imports
use crate::ast::{
    AstNode, BinaryOp, EnumVariant, ExpressionKind, ExpressionNode, Field, FunctionNode,
    ImportSpec, LiteralNode, Param, PatternNode, PrimitiveType, Spanned, StatementKind,
    StatementNode, TraitBound, TraitRef, TypeKind, TypeNode, UnaryOp,
};
use crate::diagnostic::Files;
use crate::lexer::Span;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type GenericBound = (String, Vec<Type>);
type GenericBounds = Vec<GenericBound>;
type ResolvedInterface = (Vec<Type>, HashMap<String, MethodSig>);
type ClassFieldInfo = (Type, bool);

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    current_bounds: std::collections::HashMap<String, GenericBounds>,
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
    pub current_class_type_params: Option<Vec<(String, GenericBounds)>>, // Track class-level type params with bounds for method analysis
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
        Self {
            module_resolver: Some(resolver),
            ..Self::new()
        }
    }

    fn make_symbol(kind: SymbolKind, span: Span, type_: Option<Type>) -> Symbol {
        Symbol {
            kind,
            span,
            type_,
            interfaces: std::collections::HashMap::new(),
            methods: std::collections::HashMap::new(),
            fields: std::collections::HashMap::new(),
            type_params: Vec::new(),
            original_name: None,
            llvm_name: None,
            default_param_count: 0,
            variants: None,
        }
    }

    fn make_module_symbol(&self, module_name: &str, span: Span) -> Symbol {
        Self::make_symbol(
            SymbolKind::Import,
            span,
            Some(Type::Module(module_name.to_string())),
        )
    }

    fn make_function_symbol(
        &self,
        span: Span,
        function_type: Type,
        type_params: &[(String, Vec<crate::ast::TraitBound>)],
        default_param_count: usize,
    ) -> Symbol {
        Symbol {
            kind: SymbolKind::Function,
            span,
            type_: Some(function_type),
            interfaces: std::collections::HashMap::new(),
            methods: std::collections::HashMap::new(),
            fields: std::collections::HashMap::new(),
            type_params: type_params
                .iter()
                .map(|(name, bounds)| {
                    (
                        name.clone(),
                        bounds.iter().map(|bound| bound.name.clone()).collect(),
                    )
                })
                .collect(),
            original_name: None,
            llvm_name: None,
            default_param_count,
            variants: None,
        }
    }

    fn stdlib_modules() -> Vec<(&'static str, &'static str)> {
        vec![
            ("std.assert", "assert"),
            ("std.datetime", "datetime"),
            ("std.io", "io"),
            ("std.math", "math"),
            ("std.data", "data"),
            ("std.dsa", "dsa"),
            ("std.random", "random"),
            ("std.net", "net"),
            ("std.sync", "sync"),
            ("std.env", "env"),
            ("std.sql", "sql"),
        ]
    }

    /// Map of stdlib parent module -> declared nested child modules.
    /// Keys are the short parent name (e.g. "net", "data"). Values are full
    /// child module paths (e.g. "net.http", "data.json"). This is used to
    /// eagerly inject nested stdlib modules when a parent stdlib module is
    /// imported (for example importing `std.net` also makes `net.http` usable).
    fn stdlib_nested_modules_map() -> std::collections::HashMap<&'static str, Vec<&'static str>> {
        let mut m = std::collections::HashMap::new();
        m.insert("net", vec!["net.http"]);
        m.insert("data", vec!["data.json", "data.csv"]);
        m.insert(
            "dsa",
            vec![
                "dsa.collection",
                "dsa.stack",
                "dsa.queue",
                "dsa.heap",
                "dsa.bintree",
                "dsa.graph",
                "dsa.algorithm",
            ],
        );
        m
    }

    /// Inject nested stdlib children for a given parent stdlib module into
    /// `self.imported_symbols`. `parent_module` is the short name (e.g. "net").
    fn inject_nested_stdlib_children(&mut self, parent_module: &str, span: Span) {
        let map = Self::stdlib_nested_modules_map();
        if let Some(children) = map.get(parent_module) {
            for child in children {
                // collect symbols for the child module (child is a full path like "net.http" or "data.json")
                let child_symbols = self.collect_stdlib_module_symbols(child, span);
                // store under the full child module path so module imports and
                // module-qualified accesses (Module("net.http")) resolve correctly
                self.imported_symbols
                    .insert(child.to_string(), child_symbols.clone());

                // Also expose the short child name (e.g. "json") for backward
                // compatibility so code referencing `json.parse` works after
                // importing the parent (e.g. `import std.data`). Don't overwrite
                // an existing user-provided namespace.
                if let Some(short_name) = child.split('.').next_back()
                    && !self.imported_symbols.contains_key(short_name)
                {
                    self.imported_symbols
                        .insert(short_name.to_string(), child_symbols);
                    // Register module symbol in symbol table so unqualified
                    // module references resolve (e.g., json.parse)
                    let _ = self
                        .symbol_table
                        .add_symbol(short_name, self.make_module_symbol(short_name, span));
                }
            }
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

    /// Generate helpful context for binary operator type mismatches.
    fn binary_op_help(left: &Type, right: &Type, op: &crate::ast::BinaryOp) -> String {
        match (left, right) {
            (Type::Primitive(crate::ast::PrimitiveType::Str), Type::Primitive(crate::ast::PrimitiveType::Int))
            | (Type::Primitive(crate::ast::PrimitiveType::Int), Type::Primitive(crate::ast::PrimitiveType::Str)) => {
                "Strings and integers cannot be combined directly. Use int_to_string() to convert the integer first, then use '+' for concatenation.".to_string()
            }
            (Type::Primitive(crate::ast::PrimitiveType::Str), Type::Primitive(crate::ast::PrimitiveType::Float))
            | (Type::Primitive(crate::ast::PrimitiveType::Float), Type::Primitive(crate::ast::PrimitiveType::Str)) => {
                "Strings and floats cannot be combined directly. Use float_to_string() to convert the float first, then use '+' for concatenation.".to_string()
            }
            (Type::Primitive(crate::ast::PrimitiveType::Int), Type::Primitive(crate::ast::PrimitiveType::Float))
            | (Type::Primitive(crate::ast::PrimitiveType::Float), Type::Primitive(crate::ast::PrimitiveType::Int)) => {
                "Cannot mix int and float in arithmetic. Use int_to_float() or float_to_int() to convert one operand.".to_string()
            }
            (Type::Primitive(crate::ast::PrimitiveType::Str), Type::Primitive(crate::ast::PrimitiveType::Str)) => {
                format!("The '{}' operator is not supported between two strings.", format_binary_op(op))
            }
            _ => {
                format!(
                    "Ensure both operands have compatible types. Left is {}, right is {}.",
                    format_type(left),
                    format_type(right)
                )
            }
        }
    }

    /// Build an "undefined symbol" error with a "did you mean?" suggestion if a similar
    /// symbol exists in the current scope.
    fn undefined_symbol_error(&self, kind: &str, name: &str, span: Span) -> SemanticError {
        if let Some(suggestion) = self.symbol_table.find_similar(name) {
            SemanticError::with_help(
                format!("Undefined {} '{}'", kind, name),
                span,
                format!("Did you mean '{}'?", suggestion),
            )
        } else {
            SemanticError::new(format!("Undefined {} '{}'", kind, name), span)
        }
    }

    /// Generic helper for item-not-found errors, suggesting similar names if available.
    fn item_not_found_error<F, M>(
        &self,
        item_type: &str,
        item: &str,
        type_name: &str,
        span: Span,
        get_available: F,
        message_format: M,
    ) -> SemanticError
    where
        F: Fn(&str) -> Vec<String>,
        M: Fn(&str, &str, &str) -> String,
    {
        let available_items = get_available(type_name);
        if available_items.is_empty() {
            SemanticError::new(message_format(item_type, item, type_name), span)
        } else {
            let threshold = calculate_similarity_threshold(item);
            let suggestion = available_items
                .iter()
                .map(|f| (f, levenshtein_distance(item, f)))
                .filter(|(_, dist)| *dist <= threshold)
                .min_by_key(|(_, dist)| *dist)
                .map(|(f, _)| f);
            let available = available_items.join(", ");
            if let Some(similar) = suggestion {
                SemanticError::with_help(
                    message_format(item_type, item, type_name),
                    span,
                    format!(
                        "Did you mean '{}'? Available {}s: {}",
                        similar,
                        item_type.to_lowercase(),
                        available
                    ),
                )
            } else {
                SemanticError::with_help(
                    message_format(item_type, item, type_name),
                    span,
                    format!("Available {}s: {}", item_type.to_lowercase(), available),
                )
            }
        }
    }

    /// Build a field-not-found error, suggesting similar field names if available.
    fn field_not_found_error(&self, field: &str, type_name: &str, span: Span) -> SemanticError {
        self.item_not_found_error(
            "Field",
            field,
            type_name,
            span,
            |t| self.get_available_fields(t),
            |_item_type, item, type_name| {
                format!("Field '{}' not found on type '{}'", item, type_name)
            },
        )
    }

    /// Get a list of field names for a given type.
    fn get_available_fields(&self, type_name: &str) -> Vec<String> {
        if let Some(symbol) = self.symbol_table.lookup(type_name) {
            symbol.fields.keys().cloned().collect()
        } else {
            Vec::new()
        }
    }

    /// Build a method-not-found error, suggesting similar method names if available.
    fn method_not_found_error(&self, method: &str, type_name: &str, span: Span) -> SemanticError {
        self.item_not_found_error(
            "Method",
            method,
            type_name,
            span,
            |t| self.get_available_methods(t),
            |_item_type, item, type_name| {
                format!("Undefined method '{}' on type '{}'", item, type_name)
            },
        )
    }

    /// Get a list of method names for a given type.
    fn get_available_methods(&self, type_name: &str) -> Vec<String> {
        if let Some(symbol) = self.symbol_table.lookup(type_name) {
            symbol.methods.keys().cloned().collect()
        } else {
            Vec::new()
        }
    }

    /// Set class-level type parameters and their bounds for method analysis.
    /// This should be called before analyzing/generating methods of a generic class.
    pub fn set_class_type_params(&mut self, params: Vec<(String, GenericBounds)>) {
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

    fn normalize_type_for_bound(&self, type_: &Type, known_type_params: &[String]) -> Type {
        match type_ {
            Type::Named(name, args)
                if args.is_empty() && known_type_params.iter().any(|p| p == name) =>
            {
                Type::Variable(name.clone())
            }
            Type::Named(name, args) => Type::Named(
                name.clone(),
                args.iter()
                    .map(|arg| self.normalize_type_for_bound(arg, known_type_params))
                    .collect(),
            ),
            Type::List(inner) => Type::List(Box::new(
                self.normalize_type_for_bound(inner, known_type_params),
            )),
            Type::Set(inner) => Type::Set(Box::new(
                self.normalize_type_for_bound(inner, known_type_params),
            )),
            Type::Map(key, value) => Type::Map(
                Box::new(self.normalize_type_for_bound(key, known_type_params)),
                Box::new(self.normalize_type_for_bound(value, known_type_params)),
            ),
            Type::Optional(inner) => Type::Optional(Box::new(
                self.normalize_type_for_bound(inner, known_type_params),
            )),
            Type::Reference(inner) => Type::Reference(Box::new(
                self.normalize_type_for_bound(inner, known_type_params),
            )),
            Type::Function {
                params,
                returns,
                default_count,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|param| self.normalize_type_for_bound(param, known_type_params))
                    .collect(),
                returns: Box::new(self.normalize_type_for_bound(returns, known_type_params)),
                default_count: *default_count,
            },
            _ => type_.clone(),
        }
    }

    fn resolve_type_param_bounds(
        &self,
        type_params: &[(String, Vec<TraitBound>)],
    ) -> Result<Vec<(String, GenericBounds)>, SemanticError> {
        let mut resolved = Vec::new();
        let mut known_type_params = Vec::new();

        for (param, bounds) in type_params {
            let mut resolved_bounds = Vec::new();
            for bound in bounds {
                let resolved_type_args = bound
                    .type_params
                    .iter()
                    .map(|type_param| self.resolve_type(type_param))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|ty| self.normalize_type_for_bound(&ty, &known_type_params))
                    .collect();
                resolved_bounds.push((bound.name.clone(), resolved_type_args));
            }
            resolved.push((param.clone(), resolved_bounds));
            known_type_params.push(param.clone());
        }

        Ok(resolved)
    }

    fn infer_missing_type_params_from_function_bounds(
        &self,
        func_name: &str,
        substitutions: &mut std::collections::HashMap<String, Type>,
    ) {
        let mut func_node_opt = None;
        for module_nodes in self.all_module_asts.values() {
            for node in module_nodes {
                if let AstNode::Function(func) = node
                    && func.name == func_name
                {
                    func_node_opt = Some(func);
                    break;
                }
            }
            if func_node_opt.is_some() {
                break;
            }
        }

        let Some(func_node) = func_node_opt else {
            return;
        };

        infer_missing_type_params_from_bounds(&func_node.type_params, substitutions);
    }

    fn get_builtin_sig(&self, name: &str) -> Option<&BuiltInSig> {
        // Use the canonical BUILT_IN_FUNCTIONS from the stdlib module
        crate::semantics::stdlib::BUILT_IN_FUNCTIONS.get(name)
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
        // Register built-in functions from the canonical stdlib table.
        let span = Span::new(0, 0);
        for (name, sig) in crate::semantics::stdlib::BUILT_IN_FUNCTIONS.iter() {
            self.register_builtin_function(name, sig, span);
        }

        // Register builtin classes
        self.add_sync_builtin_types();
        self.add_csv_builtin_types();
    }

    fn add_sync_builtin_types(&mut self) {
        let span = Span::new(0, 0);
        // Use canonical class symbols from the stdlib module and register them.
        let classes = crate::semantics::stdlib::sync_module_class_symbols(span);
        for (name, sym) in classes {
            let _ = self.symbol_table.add_symbol(&name, sym);
        }
    }

    fn add_csv_builtin_types(&mut self) {
        let span = Span::new(0, 0);
        let symbol = Self::make_csv_symbol(span);
        let _ = self.symbol_table.add_symbol("Csv", symbol);
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
                crate::ast::PrimitiveType::Auto => Err(SemanticError::with_help(
                    "The 'auto' type is not allowed in this context",
                    type_node.span,
                    "Use an explicit type annotation instead of 'auto'",
                )),
            },
            TypeKind::Named(name, type_args) => {
                // Handle type parameters (generic type variables)
                if type_args.is_empty()
                    && let Some(symbol) = self.symbol_table.lookup(name)
                    && matches!(symbol.kind, SymbolKind::Type)
                {
                    return Ok(Type::Variable(name.clone()));
                }

                // Handle built-in generic types
                if name == "optional" && type_args.len() == 1 {
                    let resolved_arg = self.resolve_type(&type_args[0])?;
                    return Ok(Type::Optional(Box::new(resolved_arg)));
                } else if name == "result" && type_args.len() == 2 {
                    let resolved_ok = self.resolve_type(&type_args[0])?;
                    let resolved_err = self.resolve_type(&type_args[1])?;
                    if !self.type_implements_interface(&resolved_err, "Error") {
                        return Err(SemanticError::with_help(
                            format!(
                                "Result error type must implement Error, but found {}",
                                format_type(&resolved_err)
                            ),
                            type_node.span,
                            "Use an error type that implements Error (requires message() -> string).",
                        ));
                    }
                    return Ok(Type::Result(Box::new(resolved_ok), Box::new(resolved_err)));
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
            TypeKind::Tuple(left, right) => {
                let resolved_left = self.resolve_type(left)?;
                let resolved_right = self.resolve_type(right)?;
                Ok(Type::Tuple(
                    Box::new(resolved_left),
                    Box::new(resolved_right),
                ))
            }

            TypeKind::TraitObject(_) => Err(SemanticError::new(
                "Trait objects are not yet supported",
                type_node.span,
            )),
            TypeKind::Auto => Err(SemanticError::with_help(
                "The 'auto' type is not allowed in this context",
                type_node.span,
                "Use an explicit type annotation instead of 'auto'",
            )),
        }
    }

    pub fn get_expression_type(&mut self, expr: &ExpressionNode) -> Result<Type, SemanticError> {
        match &expr.kind {
            ExpressionKind::Literal(_) => self.infer_literal_type(expr),
            ExpressionKind::None => Ok(Type::Optional(Box::new(Type::Never))),
            ExpressionKind::Identifier(name) => {
                self.resolve_identifier_expression_type(name, expr.span)
            }
            ExpressionKind::Binary {
                left,
                right,
                op,
                op_span,
                ..
            } => self.resolve_binary_expression_type(left, right, op, op_span, expr.span),
            ExpressionKind::Unary {
                expr, op, op_span, ..
            } => self.resolve_unary_expression_type(expr, op, op_span),
            ExpressionKind::Call { func, args } => {
                self.resolve_call_expression_type(func, args, expr.span)
            }
            ExpressionKind::FieldAccess { expr, field } => {
                self.resolve_field_access_type(expr, field, expr.span)
            }
            ExpressionKind::ListAccess { expr, index: _ } => {
                self.resolve_list_access_type(expr, expr.span)
            }
            ExpressionKind::ListLiteral(elements) => self.resolve_list_literal_type(elements),
            ExpressionKind::MapLiteral { entries, .. } => self.resolve_map_literal_type(entries),
            ExpressionKind::If {
                then_expr,
                else_expr,
                ..
            } => self.resolve_if_expression_type(then_expr, else_expr, expr.span),
            ExpressionKind::Lambda {
                params,
                return_type,
                body,
            } => self.resolve_lambda_type(params, return_type, body, expr.span),
            ExpressionKind::SetLiteral(elements) => self.resolve_set_literal_type(elements),
            ExpressionKind::TupleLiteral(elements) => {
                self.resolve_tuple_literal_type(elements, expr.span)
            }
            ExpressionKind::GenericType(name, type_args) => {
                self.resolve_generic_type(name, type_args, expr.span)
            }
        }
    }

    fn resolve_field_access_type(
        &mut self,
        expr: &ExpressionNode,
        field: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        let expr_type_res = self.get_expression_type(expr);
        let expr_type = match expr_type_res {
            Ok(t) => t,
            Err(e) => {
                if let crate::ast::ExpressionKind::Identifier(name) = &expr.kind
                    && let Some(func_type) = self.try_stdlib_method_lookup(name, field)
                {
                    return Ok(func_type);
                }
                return Err(e);
            }
        };

        self.resolve_field_access_by_type(&expr_type, field, span)
    }

    fn try_stdlib_method_lookup(&self, name: &str, field: &str) -> Option<Type> {
        let stdlib_names: std::collections::HashSet<String> = Self::stdlib_modules()
            .iter()
            .map(|(_, n)| n.to_string())
            .collect();
        for (ns, module_symbols) in &self.imported_symbols {
            if !stdlib_names.contains(ns) {
                continue;
            }
            if let Some(class_sym) = module_symbols.get(name)
                && matches!(class_sym.kind, SymbolKind::Class)
                && let Some(method_sig) = class_sym.methods.get(field)
            {
                return Some(Type::Function {
                    params: method_sig.params.clone(),
                    returns: Box::new(method_sig.return_type.clone()),
                    default_count: 0,
                });
            }
        }
        None
    }

    fn resolve_field_access_by_type(
        &mut self,
        expr_type: &Type,
        field: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        if let Type::Module(module_name) = expr_type {
            return self.resolve_module_field(module_name, field, span);
        }
        if let Type::Reference(inner) = expr_type {
            let inner_type = (*inner).clone();
            return self.resolve_reference_field(&inner_type, field, span);
        }
        if let Some(method_sig) = self.get_method_sig(expr_type, field) {
            return Ok(Type::Function {
                params: method_sig.params,
                returns: Box::new(method_sig.return_type),
                default_count: 0,
            });
        }
        if let Type::Named(name, args) = expr_type {
            return self.resolve_named_field(expr_type, name, args, field, span);
        }
        if let Type::Tuple(left_type, right_type) = expr_type {
            return match field {
                "left" => Ok(*left_type.clone()),
                "right" => Ok(*right_type.clone()),
                _ => Err(SemanticError::with_help(
                    format!("Unknown field '{}' on tuple type", field),
                    span,
                    "Tuples only have two fields: 'left' and 'right'. Example: auto pair = (1, 2); print(int_to_string(pair.left))",
                )),
            };
        }
        let type_name = format_type(expr_type);
        Err(self.method_not_found_error(field, &type_name, span))
    }

    fn resolve_list_access_type(
        &mut self,
        expr: &ExpressionNode,
        span: Span,
    ) -> Result<Type, SemanticError> {
        let target_type = self.get_expression_type(expr)?;
        match target_type {
            Type::List(elem_type) => Ok(*elem_type),
            Type::Map(_, value_type) => Ok(*value_type),
            Type::EmptyMap => Err(SemanticError::with_help(
                "Cannot index empty map",
                span,
                "The map type is unknown. Provide type annotations or add entries to the map literal.",
            )),
            _ => Err(SemanticError::with_help(
                "Cannot index non-list type",
                span,
                "Only lists and maps can be indexed with '[]'. Examples: my_list[0], my_map['key']",
            )),
        }
    }

    fn resolve_list_literal_type(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<Type, SemanticError> {
        if elements.is_empty() {
            return Ok(Type::EmptyList);
        }
        let first_type = self.get_expression_type(&elements[0])?;
        for (index, element) in elements.iter().enumerate() {
            let element_type = self.get_expression_type(element)?;
            if self
                .check_type_compatibility(&first_type, &element_type, element.span)
                .is_err()
            {
                return Err(SemanticError::with_help(
                    format!(
                        "List element type mismatch: expected {}, but element at index {} has type {}",
                        format_type(&first_type),
                        index,
                        format_type(&element_type)
                    ),
                    element.span,
                    "All elements in a list must have the same type. The list type is inferred from the first element.",
                ));
            }
        }
        Ok(Type::List(Box::new(first_type)))
    }

    fn resolve_map_literal_type(
        &mut self,
        entries: &[(ExpressionNode, ExpressionNode)],
    ) -> Result<Type, SemanticError> {
        if entries.is_empty() {
            return Ok(Type::EmptyMap);
        }
        let (key, value) = &entries[0];
        let key_type = self.get_expression_type(key)?;
        let value_type = self.get_expression_type(value)?;
        Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
    }

    fn resolve_if_expression_type(
        &mut self,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
        span: Span,
    ) -> Result<Type, SemanticError> {
        let then_type = self.get_expression_type(then_expr)?;
        let else_type = self.get_expression_type(else_expr)?;
        if then_type == else_type {
            Ok(then_type)
        } else {
            Err(SemanticError::with_help(
                "If expression branches must have the same type",
                span,
                format!(
                    "Then branch has type {}, else branch has type {}",
                    format_type(&then_type),
                    format_type(&else_type)
                ),
            ))
        }
    }

    fn resolve_lambda_type(
        &mut self,
        params: &[Param],
        return_type: &TypeNode,
        body: &[StatementNode],
        span: Span,
    ) -> Result<Type, SemanticError> {
        if self.lambda_captures.contains_key(&span) {
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

        let mut local_vars = std::collections::HashSet::new();
        for param in params {
            local_vars.insert(param.name.clone());
        }

        self.symbol_table.push_scope()?;
        for param in params {
            let param_type = self.resolve_type(&param.type_)?;
            self.symbol_table.add_symbol(
                &param.name,
                Self::make_symbol(SymbolKind::Variable, param.type_.span, Some(param_type)),
            )?;
        }
        self.analyze_block(body, None)?;

        let captures = self.find_free_variables_in_block(body, &local_vars)?;
        self.lambda_captures.insert(span, captures);

        let param_types = params
            .iter()
            .map(|p| self.resolve_type(&p.type_))
            .collect::<Result<Vec<_>, _>>()?;
        let return_type_resolved = if body.is_empty() {
            Type::Void
        } else {
            match &body.last().expect("body is not empty").kind {
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

    fn resolve_set_literal_type(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<Type, SemanticError> {
        if elements.is_empty() {
            return Ok(Type::EmptySet);
        }
        let elem_type = self.get_expression_type(&elements[0])?;
        Ok(Type::Set(Box::new(elem_type)))
    }

    fn resolve_tuple_literal_type(
        &mut self,
        elements: &[ExpressionNode],
        span: Span,
    ) -> Result<Type, SemanticError> {
        if elements.len() != 2 {
            return Err(SemanticError::with_help(
                format!(
                    "Tuple must have exactly 2 elements, found {}",
                    elements.len()
                ),
                span,
                "Tuples in Mux always contain exactly 2 elements: (left, right). Example: auto pair = (1, \"hello\")",
            ));
        }
        let left_type = self.get_expression_type(&elements[0])?;
        let right_type = self.get_expression_type(&elements[1])?;
        Ok(Type::Tuple(Box::new(left_type), Box::new(right_type)))
    }

    fn resolve_generic_type(
        &mut self,
        name: &str,
        type_args: &[TypeNode],
        span: Span,
    ) -> Result<Type, SemanticError> {
        if name == "tuple" {
            return self.resolve_tuple_type_annotation(type_args, span);
        }
        let (lookup_name, symbol) = self.resolve_generic_type_symbol(name, span)?;
        self.validate_type_argument_count(&lookup_name, &symbol, type_args, span)?;
        let resolved_args = type_args
            .iter()
            .map(|arg| self.resolve_type(arg))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Type::Named(lookup_name, resolved_args))
    }

    fn resolve_tuple_type_annotation(
        &self,
        type_args: &[TypeNode],
        span: Span,
    ) -> Result<Type, SemanticError> {
        if type_args.len() != 2 {
            return Err(SemanticError::with_help(
                format!(
                    "Tuple type requires exactly 2 type arguments, got {}",
                    type_args.len()
                ),
                span,
                "Tuple types always have exactly 2 type parameters. Example: tuple<int, string>",
            ));
        }
        let left_type = Box::new(self.resolve_type(&type_args[0])?);
        let right_type = Box::new(self.resolve_type(&type_args[1])?);
        Ok(Type::Tuple(left_type, right_type))
    }

    fn resolve_generic_type_symbol(
        &mut self,
        name: &str,
        span: Span,
    ) -> Result<(String, Symbol), SemanticError> {
        if let Some((module_name, type_name)) = name.split_once('.') {
            let module_symbols = self
                .imported_symbols
                .get(module_name)
                .ok_or_else(|| self.undefined_symbol_error("module", module_name, span))?;
            let symbol = module_symbols
                .get(type_name)
                .ok_or_else(|| self.undefined_symbol_error("type", type_name, span))?;
            if self.symbol_table.lookup(type_name).is_none() {
                let _ = self.symbol_table.add_symbol(type_name, symbol.clone());
            }
            Ok((type_name.to_string(), symbol.clone()))
        } else if let Some(symbol) = self.symbol_table.lookup(name) {
            Ok((name.to_string(), symbol))
        } else {
            Err(self.undefined_symbol_error("type", name, span))
        }
    }

    fn validate_type_argument_count(
        &self,
        lookup_name: &str,
        symbol: &Symbol,
        type_args: &[TypeNode],
        span: Span,
    ) -> Result<(), SemanticError> {
        let expected_count = symbol.type_params.len();
        let actual_count = type_args.len();
        if expected_count != actual_count {
            return Err(SemanticError::with_help(
                format!(
                    "Generic type '{}' requires {} type argument(s), got {}",
                    lookup_name, expected_count, actual_count
                ),
                span,
                format!(
                    "Provide exactly {} type argument{} in angle brackets, e.g. {}<{}>",
                    expected_count,
                    if expected_count == 1 { "" } else { "s" },
                    lookup_name,
                    symbol
                        .type_params
                        .iter()
                        .map(|(p, _)| p.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            ));
        }
        Ok(())
    }

    fn resolve_unary_expression_type(
        &mut self,
        expr: &ExpressionNode,
        op: &UnaryOp,
        op_span: &Span,
    ) -> Result<Type, SemanticError> {
        match op {
            UnaryOp::Not => Ok(Type::Primitive(crate::ast::PrimitiveType::Bool)),
            UnaryOp::Neg => {
                let operand_type = self.get_expression_type(expr)?;
                match operand_type {
                    Type::Primitive(crate::ast::PrimitiveType::Int)
                    | Type::Primitive(crate::ast::PrimitiveType::Float) => Ok(operand_type),
                    _ => Err(SemanticError::with_help(
                        format!(
                            "Negation operator '-' requires a numeric operand, found {}",
                            format_type(&operand_type)
                        ),
                        *op_span,
                        "The unary '-' operator can only be applied to int or float values",
                    )),
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
                    Err(SemanticError::with_help(
                        format!(
                            "Cannot dereference type {}, which is not a reference",
                            format_type(&operand_type)
                        ),
                        *op_span,
                        "The dereference operator '*' can only be applied to reference types (e.g., ref int)",
                    ))
                }
            }
            UnaryOp::Incr | UnaryOp::Decr => {
                self.check_not_modifying_constant(expr, op_span)?;
                let operand_type = self.get_expression_type(expr)?;
                match operand_type {
                    Type::Primitive(crate::ast::PrimitiveType::Int) => Ok(operand_type),
                    _ => Err(SemanticError::with_help(
                        format!(
                            "Increment/decrement operators require an int operand, found {}",
                            format_type(&operand_type)
                        ),
                        *op_span,
                        "The '++' and '--' operators can only be applied to int variables",
                    )),
                }
            }
        }
    }

    fn resolve_call_expression_type(
        &mut self,
        func: &ExpressionNode,
        args: &[ExpressionNode],
        expr_span: Span,
    ) -> Result<Type, SemanticError> {
        let func_type = self.resolve_called_function_type(func)?;

        match func_type {
            Type::Function {
                params,
                returns,
                default_count,
                ..
            } => self.resolve_function_call_type(
                func,
                args,
                params,
                returns,
                default_count,
                expr_span,
            ),
            _ => Err(SemanticError::with_help(
                "Cannot call non-function type",
                expr_span,
                "Only functions can be called with '()'. Ensure the expression before '()' is a function.",
            )),
        }
    }

    fn resolve_called_function_type(
        &mut self,
        func: &ExpressionNode,
    ) -> Result<Type, SemanticError> {
        match &func.kind {
            ExpressionKind::GenericType(name, type_args) => {
                self.get_instantiated_constructor_type(name, type_args, func.span)
            }
            ExpressionKind::Identifier(name) => match self.get_expression_type(func) {
                Ok(t) => Ok(t),
                Err(e) if e.message.contains("Undefined variable") => {
                    Err(self.undefined_symbol_error("function", name, func.span))
                }
                Err(e) => Err(e),
            },
            _ => self.get_expression_type(func),
        }
    }

    fn resolve_function_call_type(
        &mut self,
        func: &ExpressionNode,
        args: &[ExpressionNode],
        params: Vec<Type>,
        returns: Box<Type>,
        default_count: usize,
        expr_span: Span,
    ) -> Result<Type, SemanticError> {
        let actual_default_count = self.call_default_param_count(func, default_count);
        let min_args = params.len() - actual_default_count;
        let max_args = params.len();

        if args.len() < min_args || args.len() > max_args {
            self.report_call_arity_error(
                func,
                args.len(),
                min_args,
                max_args,
                actual_default_count,
                expr_span,
            )?;
        }

        let mut unifier = Unifier::new();
        for (param, arg) in params.iter().zip(args.iter()) {
            let arg_type = self.get_expression_type(arg)?;
            unifier.unify(param, &arg_type, expr_span)?;
        }

        if let Some(func_name) = self.call_function_name(func) {
            self.infer_missing_type_params_from_function_bounds(
                func_name,
                &mut unifier.substitutions,
            );
        }

        Ok(unifier.apply(&returns))
    }

    fn call_default_param_count(&self, func: &ExpressionNode, default_count: usize) -> usize {
        match &func.kind {
            ExpressionKind::Identifier(name) => {
                let symbol_default = self
                    .symbol_table
                    .lookup(name)
                    .map(|s| s.default_param_count)
                    .unwrap_or(0);
                std::cmp::max(default_count, symbol_default)
            }
            _ => default_count,
        }
    }

    fn call_function_name<'a>(&self, func: &'a ExpressionNode) -> Option<&'a str> {
        match &func.kind {
            ExpressionKind::Identifier(name) => Some(name.as_str()),
            ExpressionKind::FieldAccess { field, .. } => Some(field.as_str()),
            _ => None,
        }
    }

    fn report_call_arity_error(
        &self,
        func: &ExpressionNode,
        arg_count: usize,
        min_args: usize,
        max_args: usize,
        actual_default_count: usize,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let func_name = match &func.kind {
            ExpressionKind::Identifier(name) => format!("'{}'", name),
            ExpressionKind::FieldAccess { field, .. } => format!("'{}'", field),
            _ => "this function".to_string(),
        };

        if actual_default_count > 0 {
            Err(SemanticError::with_help(
                format!(
                    "{} expects {} to {} arguments, but {} {} provided",
                    func_name,
                    min_args,
                    max_args,
                    arg_count,
                    if arg_count == 1 { "was" } else { "were" }
                ),
                expr_span,
                format!(
                    "{} has {} required parameter(s) and {} optional parameter(s) with defaults",
                    func_name, min_args, actual_default_count
                ),
            ))
        } else {
            Err(SemanticError::with_help(
                format!(
                    "{} expects {} argument(s), but {} {} provided",
                    func_name,
                    max_args,
                    arg_count,
                    if arg_count == 1 { "was" } else { "were" }
                ),
                expr_span,
                if arg_count > max_args {
                    "Too many arguments. Remove the extra argument(s).".to_string()
                } else {
                    format!(
                        "Not enough arguments. {} requires {} argument(s).",
                        func_name, max_args
                    )
                },
            ))
        }
    }

    fn resolve_identifier_expression_type(
        &self,
        name: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        if name == "self" {
            if let Some(self_type) = &self.current_self_type {
                return Ok(self_type.clone());
            }
            return Ok(Type::Named("Unknown".to_string(), vec![]));
        }

        let symbol = self
            .symbol_table
            .get_cloned(name)
            .or_else(|| self.symbol_table.lookup(name));

        if let Some(symbol) = symbol {
            let type_ = symbol.type_.clone().ok_or_else(|| {
                SemanticError::new(format!("Symbol '{}' has no type information", name), span)
            })?;
            let type_ = match &type_ {
                Type::Generic(n) if n == name => Type::Variable(name.to_string()),
                _ => type_,
            };
            return Ok(type_);
        }

        if let Some(sig) = self.get_builtin_sig(name) {
            return Ok(Type::Function {
                params: sig.params.clone(),
                returns: Box::new(sig.return_type.clone()),
                default_count: 0,
            });
        }

        let stdlib_names: std::collections::HashSet<String> = Self::stdlib_modules()
            .iter()
            .map(|(_, short_name)| short_name.to_string())
            .collect();

        for (module_ns, module_symbols) in &self.imported_symbols {
            if !stdlib_names.contains(module_ns) {
                continue;
            }
            if let Some(sym) = module_symbols.get(name)
                && matches!(sym.kind, SymbolKind::Class)
            {
                return Ok(Type::Named(name.to_string(), Vec::new()));
            }
        }

        Err(self.undefined_symbol_error("variable", name, span))
    }

    fn resolve_binary_expression_type(
        &mut self,
        left: &ExpressionNode,
        right: &ExpressionNode,
        op: &crate::ast::BinaryOp,
        op_span: &Span,
        expr_span: Span,
    ) -> Result<Type, SemanticError> {
        let left_type = self.get_expression_type(left)?;
        let right_type = self.get_expression_type(right)?;

        if *op == crate::ast::BinaryOp::Assign {
            self.validate_assignment_target(left, &right_type, expr_span)?;
            return Ok(right_type);
        }

        if matches!(
            op,
            crate::ast::BinaryOp::AddAssign
                | crate::ast::BinaryOp::SubtractAssign
                | crate::ast::BinaryOp::MultiplyAssign
                | crate::ast::BinaryOp::DivideAssign
                | crate::ast::BinaryOp::ModuloAssign
        ) {
            self.validate_compound_assignment_target(
                left,
                &left_type,
                &right_type,
                op,
                expr_span,
                op_span,
            )?;
            let base_op = Self::compound_base_op(op);
            if let Some(result_type) =
                self.resolve_binary_operator(&left_type, &right_type, &base_op)
            {
                return Ok(result_type);
            }
            return Err(SemanticError::with_help(
                format!(
                    "Operator '{}' is not supported between types {} and {}",
                    format_binary_op(&base_op),
                    format_type(&left_type),
                    format_type(&right_type)
                ),
                *op_span,
                Self::binary_op_help(&left_type, &right_type, &base_op),
            ));
        }

        if let Some(result_type) = self.resolve_binary_operator(&left_type, &right_type, op) {
            return Ok(result_type);
        }

        Err(SemanticError::with_help(
            format!(
                "Operator '{}' is not supported between types {} and {}",
                format_binary_op(op),
                format_type(&left_type),
                format_type(&right_type)
            ),
            *op_span,
            Self::binary_op_help(&left_type, &right_type, op),
        ))
    }

    fn validate_assignment_target(
        &mut self,
        left: &ExpressionNode,
        right_type: &Type,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        match &left.kind {
            crate::ast::ExpressionKind::Identifier(name) => {
                self.validate_identifier_assignment_target(name, left.span, right_type, expr_span)
            }
            crate::ast::ExpressionKind::FieldAccess {
                expr: obj_expr,
                field,
            } => self.validate_field_assignment_target(
                obj_expr, field, left.span, right_type, expr_span,
            ),
            crate::ast::ExpressionKind::Unary {
                op: crate::ast::UnaryOp::Deref,
                ..
            } => Ok(()),
            crate::ast::ExpressionKind::ListAccess {
                expr: target_expr, ..
            } => self.validate_index_assignment_target(target_expr, right_type, expr_span),
            _ => Err(SemanticError::with_help(
                "Cannot assign to this expression",
                expr_span,
                "Only variables, fields, dereferences, and indexed expressions can be assigned to",
            )),
        }
    }

    fn resolve_lvalue_class_symbol(
        &mut self,
        obj_expr: &ExpressionNode,
    ) -> Result<Option<(String, crate::semantics::Symbol)>, SemanticError> {
        let obj_type = self.get_expression_type(obj_expr)?;
        if let Type::Named(class_name, _) = &obj_type
            && let Some(symbol) = self.symbol_table.lookup(class_name)
        {
            return Ok(Some((class_name.clone(), symbol)));
        }
        Ok(None)
    }

    fn check_const_field_assignment(
        &self,
        field: &str,
        fields: &std::collections::HashMap<String, (Type, bool)>,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        if let Some((_field_type, is_const)) = fields.get(field)
            && *is_const
        {
            return Err(SemanticError::with_help(
                format!("Cannot assign to const field '{}'", field),
                expr_span,
                "Const fields cannot be modified after initialization. Remove the 'const' modifier from the field declaration if mutation is needed.",
            ));
        }
        Ok(())
    }

    fn validate_identifier_assignment_target(
        &mut self,
        name: &str,
        left_span: Span,
        right_type: &Type,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let symbol = self
            .symbol_table
            .lookup(name)
            .ok_or_else(|| self.undefined_symbol_error("variable", name, left_span))?;

        if symbol.kind == SymbolKind::Constant {
            return Err(SemanticError::with_help(
                format!("Cannot assign to constant '{}'", name),
                expr_span,
                "Constants cannot be modified after initialization",
            ));
        }

        let var_type = symbol.type_.as_ref().ok_or_else(|| {
            SemanticError::new(
                format!("Variable '{}' has no type information", name),
                left_span,
            )
        })?;
        if let Type::Reference(inner) = var_type {
            self.check_type_compatibility(inner, right_type, expr_span)
        } else {
            self.check_type_compatibility(var_type, right_type, expr_span)
        }
    }

    fn validate_field_assignment_target(
        &mut self,
        obj_expr: &ExpressionNode,
        field: &str,
        left_span: Span,
        right_type: &Type,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let Some((class_name, symbol)) = self.resolve_lvalue_class_symbol(obj_expr)? else {
            return Ok(());
        };

        self.check_const_field_assignment(field, &symbol.fields, expr_span)?;

        if let Some((field_type, _)) = symbol.fields.get(field) {
            self.check_type_compatibility(field_type, right_type, expr_span)?;
            return Ok(());
        }
        Err(self.field_not_found_error(field, &class_name, left_span))
    }

    fn validate_index_assignment_target(
        &mut self,
        target_expr: &ExpressionNode,
        right_type: &Type,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let target_type = self.get_expression_type(target_expr)?;
        match target_type {
            Type::List(ref elem_type) => {
                self.check_type_compatibility(elem_type, right_type, expr_span)
            }
            Type::Map(_, ref value_type) => {
                self.check_type_compatibility(value_type, right_type, expr_span)
            }
            _ => Err(SemanticError::with_help(
                format!(
                    "Cannot assign to index on type {}",
                    format_type(&target_type)
                ),
                expr_span,
                "Only lists and maps support index assignment. Example: my_list[0] = value, my_map[\"key\"] = value",
            )),
        }
    }

    fn validate_compound_assignment_target(
        &mut self,
        left: &ExpressionNode,
        left_type: &Type,
        right_type: &Type,
        op: &crate::ast::BinaryOp,
        expr_span: Span,
        op_span: &Span,
    ) -> Result<(), SemanticError> {
        match &left.kind {
            crate::ast::ExpressionKind::Identifier(name) => {
                self.validate_identifier_compound_target(name, left.span, expr_span)?;
            }
            crate::ast::ExpressionKind::FieldAccess { .. } => {
                self.validate_field_compound_target(
                    left, left_type, right_type, op, expr_span, op_span,
                )?;
            }
            crate::ast::ExpressionKind::Unary {
                op: crate::ast::UnaryOp::Deref,
                ..
            } => {}
            _ => {}
        }

        Ok(())
    }

    fn validate_identifier_compound_target(
        &self,
        name: &str,
        left_span: Span,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let symbol = self
            .symbol_table
            .lookup(name)
            .ok_or_else(|| self.undefined_symbol_error("variable", name, left_span))?;

        if symbol.kind == SymbolKind::Constant {
            return Err(SemanticError::with_help(
                format!("Cannot modify constant '{}'", name),
                expr_span,
                "Constants cannot be modified after initialization. Declare the variable with 'auto' instead of 'const' if you need to change its value.",
            ));
        }

        Ok(())
    }

    fn validate_field_compound_target(
        &mut self,
        left: &ExpressionNode,
        left_type: &Type,
        right_type: &Type,
        op: &crate::ast::BinaryOp,
        expr_span: Span,
        op_span: &Span,
    ) -> Result<(), SemanticError> {
        let crate::ast::ExpressionKind::FieldAccess {
            expr: obj_expr,
            field,
        } = &left.kind
        else {
            return Ok(());
        };

        let Some((class_name, symbol)) = self.resolve_lvalue_class_symbol(obj_expr)? else {
            return Ok(());
        };

        if let Some((_field_type, is_const)) = symbol.fields.get(field) {
            if *is_const {
                return Err(SemanticError::with_help(
                    format!("Cannot modify const field '{}'", field),
                    expr_span,
                    "Const fields cannot be modified after initialization. Remove the 'const' modifier from the field declaration if mutation is needed.",
                ));
            }

            let base_op = Self::compound_base_op(op);
            self.resolve_binary_operator(left_type, right_type, &base_op)
                .ok_or_else(|| {
                    SemanticError::with_help(
                        format!(
                            "Operator '{}' is not supported between types {} and {}",
                            format_binary_op(&base_op),
                            format_type(left_type),
                            format_type(right_type)
                        ),
                        *op_span,
                        format!(
                            "The '{}' operator cannot be applied to {} and {}. Ensure both operands have compatible types.",
                            format_binary_op(&base_op),
                            format_type(left_type),
                            format_type(right_type)
                        ),
                    )
                })?;
        } else {
            return Err(self.field_not_found_error(field, &class_name, left.span));
        }

        Ok(())
    }

    fn compound_base_op(op: &crate::ast::BinaryOp) -> crate::ast::BinaryOp {
        match op {
            crate::ast::BinaryOp::AddAssign => crate::ast::BinaryOp::Add,
            crate::ast::BinaryOp::SubtractAssign => crate::ast::BinaryOp::Subtract,
            crate::ast::BinaryOp::MultiplyAssign => crate::ast::BinaryOp::Multiply,
            crate::ast::BinaryOp::DivideAssign => crate::ast::BinaryOp::Divide,
            crate::ast::BinaryOp::ModuloAssign => crate::ast::BinaryOp::Modulo,
            _ => unreachable!(),
        }
    }

    fn resolve_module_field(
        &self,
        module_name: &str,
        field: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        let module_symbols = self.imported_symbols.get(module_name).ok_or_else(|| {
            SemanticError::with_help(
                format!("Module '{}' not found in imports", module_name),
                span,
                format!(
                    "Make sure you have imported '{}' at the top of your file, e.g. import {}",
                    module_name, module_name
                ),
            )
        })?;
        let symbol = module_symbols.get(field).ok_or_else(|| {
            let available: Vec<&String> = module_symbols.keys().collect();
            if available.is_empty() {
                SemanticError::new(
                    format!(
                        "Module '{}' has no exported symbol '{}'",
                        module_name, field
                    ),
                    span,
                )
            } else {
                SemanticError::with_help(
                    format!(
                        "Module '{}' has no exported symbol '{}'",
                        module_name, field
                    ),
                    span,
                    format!(
                        "Available exports: {}",
                        available
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )
            }
        })?;
        symbol.type_.clone().ok_or_else(|| {
            SemanticError::new(
                format!(
                    "Symbol '{}' in module '{}' has no type information",
                    field, module_name
                ),
                span,
            )
        })
    }

    fn resolve_reference_field(
        &mut self,
        inner_type: &Type,
        field: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        if let Type::Named(name, args) = inner_type {
            if let Some(symbol) = self.symbol_table.lookup(name) {
                if let Some((field_type, _)) = symbol.fields.get(field) {
                    return Ok(self.substitute_type_params(field_type, &symbol.type_params, args));
                }
                if let Some(method_sig) = self.get_method_sig(inner_type, field) {
                    return Ok(Type::Function {
                        params: method_sig.params,
                        returns: Box::new(method_sig.return_type),
                        default_count: 0,
                    });
                }
                return Err(self.field_not_found_error(field, name, span));
            }
            return Err(self.undefined_symbol_error("type", name, span));
        }
        if let Some(method_sig) = self.get_method_sig(inner_type, field) {
            return Ok(Type::Function {
                params: method_sig.params,
                returns: Box::new(method_sig.return_type),
                default_count: 0,
            });
        }
        Err(SemanticError::with_help(
            format!(
                "Cannot access field '{}' on type {}",
                field,
                format_type(inner_type)
            ),
            span,
            format!(
                "The type {} does not have a field or method named '{}'",
                format_type(inner_type),
                field
            ),
        ))
    }

    fn resolve_named_field(
        &mut self,
        expr_type: &Type,
        name: &str,
        args: &[Type],
        field: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        if let Some(symbol) = self.symbol_table.lookup(name) {
            return self
                .resolve_field_from_local_symbol(&symbol, args, expr_type, field, name, span);
        }

        self.resolve_field_from_imported_module(name, field, expr_type, args, span)
    }

    fn resolve_field_from_local_symbol(
        &mut self,
        symbol: &Symbol,
        args: &[Type],
        expr_type: &Type,
        field: &str,
        name: &str,
        span: Span,
    ) -> Result<Type, SemanticError> {
        if let Some((field_type, _)) = symbol.fields.get(field) {
            return Ok(self.substitute_type_params(field_type, &symbol.type_params, args));
        }

        if let Some(method_sig) = self.get_method_sig(expr_type, field) {
            return Ok(self.wrap_method_signature(&method_sig));
        }

        Err(self.field_not_found_error(field, name, span))
    }

    fn resolve_field_from_imported_module(
        &mut self,
        name: &str,
        field: &str,
        expr_type: &Type,
        args: &[Type],
        span: Span,
    ) -> Result<Type, SemanticError> {
        for module_symbols in self.imported_symbols.values() {
            if let Some(class_symbol) = module_symbols.get(name)
                && let Some(method_sig) = class_symbol.methods.get(field)
            {
                let resolved_sig =
                    self.resolve_method_sig_for_field(method_sig, &class_symbol.type_params, args);
                return Ok(self.wrap_method_signature(&resolved_sig));
            }
        }

        Err(self.method_not_found_error(field, &format_type(expr_type), span))
    }

    fn resolve_method_sig_for_field(
        &self,
        method_sig: &MethodSig,
        type_params: &[(String, Vec<String>)],
        args: &[Type],
    ) -> MethodSig {
        if args.is_empty() {
            method_sig.clone()
        } else {
            self.substitute_method_sig(method_sig, type_params, args)
        }
    }

    fn wrap_method_signature(&self, method_sig: &MethodSig) -> Type {
        Type::Function {
            params: method_sig.params.clone(),
            returns: Box::new(method_sig.return_type.clone()),
            default_count: 0,
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
            .map_err(|e| {
                SemanticError::with_help(
                    format!(
                        "Return type mismatch in interface implementation: {}",
                        e.message
                    ),
                    span,
                    "The class method's return type must match the interface method's return type",
                )
            })?;
        // Unify params
        if interface_sig.params.len() != class_sig.params.len() {
            return Err(SemanticError::with_help(
                format!(
                    "Parameter count mismatch: interface expects {} parameter{}, class provides {}",
                    interface_sig.params.len(),
                    if interface_sig.params.len() == 1 {
                        ""
                    } else {
                        "s"
                    },
                    class_sig.params.len()
                ),
                span,
                "The class method must have the same number of parameters as the interface method",
            ));
        }
        for (i, (int_param, class_param)) in interface_sig
            .params
            .iter()
            .zip(&class_sig.params)
            .enumerate()
        {
            unifier.unify(int_param, class_param, span).map_err(|e| {
                SemanticError::with_help(
                    format!(
                        "Parameter {} type mismatch in interface implementation: {}",
                        i, e.message
                    ),
                    span,
                    format!(
                        "Parameter {} must have type {} to match the interface",
                        i,
                        format_type(int_param)
                    ),
                )
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
            .ok_or_else(|| self.undefined_symbol_error("type", name, span))?;
        if symbol.kind != SymbolKind::Class {
            return Err(SemanticError::with_help(
                format!("'{}' is not a class", name),
                span,
                format!(
                    "'{}' is a {}not a class. Only classes can be instantiated with .new()",
                    name,
                    match symbol.kind {
                        SymbolKind::Function => "a function, ",
                        SymbolKind::Variable => "a variable, ",
                        SymbolKind::Interface => "an interface, ",
                        SymbolKind::Enum => "an enum, ",
                        SymbolKind::Constant => "a constant, ",
                        SymbolKind::Import => "an import, ",
                        SymbolKind::Type => "a type parameter, ",
                        _ => "",
                    }
                ),
            ));
        }
        let resolved_args = type_args
            .iter()
            .map(|arg| self.resolve_type(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if resolved_args.len() != symbol.type_params.len() {
            return Err(SemanticError::with_help(
                format!(
                    "Expected {} type argument(s) for '{}', got {}",
                    symbol.type_params.len(),
                    name,
                    resolved_args.len()
                ),
                span,
                format!(
                    "Class '{}' requires {} type parameter(s). Example: {}<{}>",
                    name,
                    symbol.type_params.len(),
                    name,
                    symbol
                        .type_params
                        .iter()
                        .map(|(p, _)| p.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            ));
        }
        let new_sig = symbol.methods.get("new").ok_or_else(|| SemanticError::with_help(
            format!("Class '{}' has no constructor", name),
            span,
            format!(
                "Class '{}' does not have a .new() method. Ensure the class has fields or a constructor defined.",
                name
            ),
        ))?;
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
        self.type_implements_interface_with_args(type_, interface_name, &[])
    }

    fn type_implements_interface_with_args(
        &self,
        type_: &Type,
        interface_name: &str,
        interface_args: &[Type],
    ) -> bool {
        match type_ {
            Type::Named(name, _) => {
                self.type_implements_interface_with_named(name, interface_name, interface_args)
            }
            Type::Primitive(prim) => {
                self.type_implements_interface_with_primitive(prim, interface_name)
            }
            Type::Variable(var) | Type::Generic(var) => {
                self.type_implements_interface_with_variable(var, interface_name, interface_args)
            }
            _ => false,
        }
    }

    fn type_implements_interface_with_named(
        &self,
        name: &str,
        interface_name: &str,
        interface_args: &[Type],
    ) -> bool {
        if let Some(symbol) = self.symbol_table.lookup(name) {
            if let Some((stored_args, _)) = symbol.interfaces.get(interface_name) {
                if interface_args.is_empty() {
                    return true;
                }
                // Verify the interface exists and arity matches
                if let Some(interface_symbol) = self.symbol_table.lookup(interface_name)
                    && interface_symbol.type_params.len() != interface_args.len()
                {
                    return false;
                }
                // Compare stored concrete type arguments with provided interface_args
                stored_args == interface_args
            } else {
                false
            }
        } else {
            false
        }
    }

    fn type_implements_interface_with_primitive(
        &self,
        prim: &PrimitiveType,
        interface_name: &str,
    ) -> bool {
        match prim {
            PrimitiveType::Str => {
                interface_name == "Stringable"
                    || interface_name == "Equatable"
                    || interface_name == "Comparable"
                    || interface_name == "Hashable"
                    || interface_name == "Error"
            }
            PrimitiveType::Int => {
                matches!(
                    interface_name,
                    "Stringable" | "Equatable" | "Comparable" | "Hashable"
                )
            }
            PrimitiveType::Float => {
                matches!(
                    interface_name,
                    "Stringable" | "Equatable" | "Comparable" | "Hashable"
                )
            }
            PrimitiveType::Bool => {
                matches!(interface_name, "Stringable" | "Equatable" | "Hashable")
            }
            PrimitiveType::Char => {
                matches!(
                    interface_name,
                    "Stringable" | "Equatable" | "Comparable" | "Hashable"
                )
            }
            _ => false,
        }
    }

    fn type_implements_interface_with_variable(
        &self,
        var: &str,
        interface_name: &str,
        interface_args: &[Type],
    ) -> bool {
        if let Some(bounds) = self.current_bounds.get(var) {
            bounds.iter().any(|(bound_name, bound_args)| {
                bound_name == interface_name
                    && (interface_args.is_empty() || bound_args == interface_args)
            })
        } else {
            false
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
            "Hashable" => match method_name {
                "hash" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Int),
                    is_static: false,
                }),
                _ => None,
            },
            "Error" => match method_name {
                "message" => Some(MethodSig {
                    params: vec![],
                    return_type: Type::Primitive(PrimitiveType::Str),
                    is_static: false,
                }),
                _ => None,
            },
            _ => None,
        }
    }

    fn get_named_method_sig(
        &self,
        name: &str,
        args: &[Type],
        method_name: &str,
    ) -> Option<MethodSig> {
        let symbol = self.symbol_table.lookup(name)?;
        if let Some(sig) = symbol.methods.get(method_name) {
            return Some(if args.is_empty() {
                sig.clone()
            } else {
                self.substitute_method_sig(sig, &symbol.type_params, args)
            });
        }
        self.find_interface_method(&symbol, method_name)
    }

    fn get_variable_generic_method_sig(&self, var: &str, method_name: &str) -> Option<MethodSig> {
        let bounds = self.current_bounds.get(var)?;
        for (bound_name, bound_args) in bounds {
            if let Some(sig) = self.get_builtin_interface_method(bound_name, method_name) {
                return Some(sig);
            }
            if let Some(interface_symbol) = self.symbol_table.lookup(bound_name)
                && let Some(sig) = self.find_interface_method(&interface_symbol, method_name)
            {
                if bound_args.is_empty() {
                    return Some(sig);
                }

                return Some(self.substitute_method_sig(
                    &sig,
                    &interface_symbol.type_params,
                    bound_args,
                ));
            }
        }
        None
    }

    fn get_primitive_method_sig(
        &self,
        prim: &PrimitiveType,
        method_name: &str,
    ) -> Option<MethodSig> {
        use PrimitiveType::{Bool, Char, Float, Int, Str};
        let resolver = match prim {
            Int => Some(Self::get_int_method_sig as fn(&Self, &str) -> Option<MethodSig>),
            Float => Some(Self::get_float_method_sig as fn(&Self, &str) -> Option<MethodSig>),
            Str => Some(Self::get_string_method_sig as fn(&Self, &str) -> Option<MethodSig>),
            Bool => Some(Self::get_bool_method_sig as fn(&Self, &str) -> Option<MethodSig>),
            Char => Some(Self::get_char_method_sig as fn(&Self, &str) -> Option<MethodSig>),
            PrimitiveType::Void | PrimitiveType::Auto => None,
        };
        resolver.and_then(|resolve| resolve(self, method_name))
    }

    fn make_instance_method_sig(params: Vec<Type>, return_type: Type) -> MethodSig {
        MethodSig {
            params,
            return_type,
            is_static: false,
        }
    }

    fn make_eq_method_sig(param_type: PrimitiveType) -> MethodSig {
        Self::make_instance_method_sig(
            vec![Type::Primitive(param_type)],
            Type::Primitive(PrimitiveType::Bool),
        )
    }

    fn make_cmp_method_sig(param_type: PrimitiveType) -> MethodSig {
        Self::make_instance_method_sig(
            vec![Type::Primitive(param_type)],
            Type::Primitive(PrimitiveType::Int),
        )
    }

    fn make_hash_method_sig() -> MethodSig {
        Self::make_instance_method_sig(vec![], Type::Primitive(PrimitiveType::Int))
    }

    fn make_to_string_method_sig() -> MethodSig {
        Self::make_instance_method_sig(vec![], Type::Primitive(PrimitiveType::Str))
    }

    fn make_str_parse_result_method_sig(value_type: PrimitiveType) -> MethodSig {
        Self::make_instance_method_sig(
            vec![],
            Type::Named(
                "result".to_string(),
                vec![
                    Type::Primitive(value_type),
                    Type::Primitive(PrimitiveType::Str),
                ],
            ),
        )
    }

    fn get_int_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "to_string" => Some(Self::make_to_string_method_sig()),
            "to_float" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Float),
            )),
            "to_int" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Int),
            )),
            "eq" => Some(Self::make_eq_method_sig(PrimitiveType::Int)),
            "cmp" => Some(Self::make_cmp_method_sig(PrimitiveType::Int)),
            "hash" => Some(Self::make_hash_method_sig()),
            _ => None,
        }
    }

    fn get_float_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "to_string" => Some(Self::make_to_string_method_sig()),
            "to_int" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Int),
            )),
            "to_float" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Float),
            )),
            "eq" => Some(Self::make_eq_method_sig(PrimitiveType::Float)),
            "cmp" => Some(Self::make_cmp_method_sig(PrimitiveType::Float)),
            "hash" => Some(Self::make_hash_method_sig()),
            _ => None,
        }
    }

    fn get_string_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "to_string" | "message" => Some(Self::make_to_string_method_sig()),
            "length" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Int),
            )),
            "to_int" => Some(Self::make_str_parse_result_method_sig(PrimitiveType::Int)),
            "to_float" => Some(Self::make_str_parse_result_method_sig(PrimitiveType::Float)),
            "eq" => Some(Self::make_eq_method_sig(PrimitiveType::Str)),
            "cmp" => Some(Self::make_cmp_method_sig(PrimitiveType::Str)),
            "hash" => Some(Self::make_hash_method_sig()),
            _ => None,
        }
    }

    fn get_bool_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "to_string" => Some(Self::make_to_string_method_sig()),
            "to_int" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Int),
            )),
            "to_float" => Some(Self::make_instance_method_sig(
                vec![],
                Type::Primitive(PrimitiveType::Float),
            )),
            "eq" => Some(Self::make_eq_method_sig(PrimitiveType::Bool)),
            "hash" => Some(Self::make_hash_method_sig()),
            _ => None,
        }
    }

    fn get_char_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "to_string" => Some(Self::make_to_string_method_sig()),
            "to_int" => Some(Self::make_str_parse_result_method_sig(PrimitiveType::Int)),
            "eq" => Some(Self::make_eq_method_sig(PrimitiveType::Char)),
            "cmp" => Some(Self::make_cmp_method_sig(PrimitiveType::Char)),
            "hash" => Some(Self::make_hash_method_sig()),
            _ => None,
        }
    }

    fn get_list_method_sig(&self, elem_type: &Type, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "push_back" => Some(MethodSig {
                params: vec![elem_type.clone()],
                return_type: Type::Void,
                is_static: false,
            }),
            "pop_back" => Some(MethodSig {
                params: vec![],
                return_type: Type::Optional(Box::new(elem_type.clone())),
                is_static: false,
            }),
            "push" => Some(MethodSig {
                params: vec![elem_type.clone()],
                return_type: Type::Void,
                is_static: false,
            }),
            "pop" => Some(MethodSig {
                params: vec![],
                return_type: Type::Optional(Box::new(elem_type.clone())),
                is_static: false,
            }),
            "get" => Some(MethodSig {
                params: vec![Type::Primitive(PrimitiveType::Int)],
                return_type: Type::Optional(Box::new(elem_type.clone())),
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
        }
    }

    fn get_map_method_sig(
        &self,
        key_type: &Type,
        value_type: &Type,
        method_name: &str,
    ) -> Option<MethodSig> {
        match method_name {
            "put" => Some(MethodSig {
                params: vec![key_type.clone(), value_type.clone()],
                return_type: Type::Void,
                is_static: false,
            }),
            "get" => Some(MethodSig {
                params: vec![key_type.clone()],
                return_type: Type::Optional(Box::new(value_type.clone())),
                is_static: false,
            }),
            "get_keys" => Some(MethodSig {
                params: vec![],
                return_type: Type::List(Box::new(key_type.clone())),
                is_static: false,
            }),
            "get_values" => Some(MethodSig {
                params: vec![],
                return_type: Type::List(Box::new(value_type.clone())),
                is_static: false,
            }),
            "get_pairs" => Some(MethodSig {
                params: vec![],
                return_type: Type::List(Box::new(Type::Tuple(
                    Box::new(key_type.clone()),
                    Box::new(value_type.clone()),
                ))),
                is_static: false,
            }),
            "contains" => Some(MethodSig {
                params: vec![key_type.clone()],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_static: false,
            }),
            "remove" => Some(MethodSig {
                params: vec![key_type.clone()],
                return_type: Type::Optional(Box::new(value_type.clone())),
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
        }
    }

    fn get_set_method_sig(&self, elem_type: &Type, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "add" => Some(MethodSig {
                params: vec![elem_type.clone()],
                return_type: Type::Void,
                is_static: false,
            }),
            "remove" => Some(MethodSig {
                params: vec![elem_type.clone()],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_static: false,
            }),
            "contains" => Some(MethodSig {
                params: vec![elem_type.clone()],
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
        }
    }

    fn get_optional_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        fn bool_method_sig() -> MethodSig {
            MethodSig {
                params: vec![],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_static: false,
            }
        }

        match method_name {
            "to_string" => Some(MethodSig {
                params: vec![],
                return_type: Type::Primitive(PrimitiveType::Str),
                is_static: false,
            }),
            "is_some" | "is_none" => Some(bool_method_sig()),
            _ => None,
        }
    }

    fn get_result_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        fn bool_method_sig() -> MethodSig {
            MethodSig {
                params: vec![],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_static: false,
            }
        }

        match method_name {
            "to_string" => Some(MethodSig {
                params: vec![],
                return_type: Type::Primitive(PrimitiveType::Str),
                is_static: false,
            }),
            "is_ok" | "is_err" => Some(bool_method_sig()),
            _ => None,
        }
    }

    fn get_tuple_method_sig(&self, method_name: &str) -> Option<MethodSig> {
        match method_name {
            "to_string" => Some(MethodSig {
                params: vec![],
                return_type: Type::Primitive(PrimitiveType::Str),
                is_static: false,
            }),
            "new" => Some(MethodSig {
                params: vec![],
                return_type: Type::Tuple(
                    Box::new(Type::Primitive(PrimitiveType::Int)),
                    Box::new(Type::Primitive(PrimitiveType::Str)),
                ),
                is_static: true,
            }),
            _ => None,
        }
    }

    pub(crate) fn get_method_sig(&self, type_: &Type, method_name: &str) -> Option<MethodSig> {
        match type_ {
            Type::Named(name, args) => self.get_named_method_sig(name, args, method_name),
            Type::Variable(var) | Type::Generic(var) => {
                self.get_variable_generic_method_sig(var, method_name)
            }
            Type::Primitive(prim) => self.get_primitive_method_sig(prim, method_name),
            Type::List(elem_type) => self.get_list_method_sig(elem_type, method_name),
            Type::Map(key_type, value_type) => {
                self.get_map_method_sig(key_type, value_type, method_name)
            }
            Type::Set(elem_type) => self.get_set_method_sig(elem_type, method_name),
            Type::Optional(_) => self.get_optional_method_sig(method_name),
            Type::Result(_, _) => self.get_result_method_sig(method_name),
            Type::Tuple(_, _) => self.get_tuple_method_sig(method_name),
            Type::Reference(inner) => self.get_method_sig(inner, method_name),
            _ => None,
        }
    }

    fn find_interface_method(
        &self,
        symbol: &crate::semantics::Symbol,
        method_name: &str,
    ) -> Option<MethodSig> {
        for (_, interface_methods) in symbol.interfaces.values() {
            if let Some(sig) = interface_methods.get(method_name) {
                return Some(sig.clone());
            }
        }
        None
    }

    fn substitute_method_sig(
        &self,
        sig: &MethodSig,
        type_params: &[(String, Vec<String>)],
        args: &[Type],
    ) -> MethodSig {
        let substituted_params = sig
            .params
            .iter()
            .map(|p| self.substitute_type_params(p, type_params, args))
            .collect();
        let substituted_return = self.substitute_type_params(&sig.return_type, type_params, args);
        MethodSig {
            params: substituted_params,
            return_type: substituted_return,
            is_static: false,
        }
    }

    fn resolve_binary_operator(
        &self,
        left_type: &Type,
        right_type: &Type,
        op: &BinaryOp,
    ) -> Option<Type> {
        if matches!(op, BinaryOp::In) {
            return self.resolve_in_binary_operator(left_type, right_type);
        }

        if left_type != right_type {
            return None;
        }

        self.resolve_equal_type_binary_operator(left_type, right_type, op)
    }

    fn resolve_in_binary_operator(&self, left_type: &Type, right_type: &Type) -> Option<Type> {
        match right_type {
            Type::List(_) | Type::Set(_) => Some(Type::Primitive(crate::ast::PrimitiveType::Bool)),
            Type::Map(key_type, _) => {
                if left_type == key_type.as_ref() {
                    Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
                } else {
                    None
                }
            }
            Type::Primitive(PrimitiveType::Str) => {
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
    }

    fn resolve_equal_type_binary_operator(
        &self,
        left_type: &Type,
        right_type: &Type,
        op: &BinaryOp,
    ) -> Option<Type> {
        match op {
            BinaryOp::Add => self.resolve_add_binary_operator(left_type),
            BinaryOp::Subtract
            | BinaryOp::Multiply
            | BinaryOp::Divide
            | BinaryOp::Modulo
            | BinaryOp::Exponent => self.resolve_numeric_binary_operator(left_type),
            BinaryOp::Equal | BinaryOp::NotEqual => {
                Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
            }
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                self.resolve_comparison_binary_operator(left_type)
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                self.resolve_logical_binary_operator(left_type, right_type)
            }
            _ => None,
        }
    }

    fn resolve_add_binary_operator(&self, left_type: &Type) -> Option<Type> {
        if matches!(
            left_type,
            Type::Primitive(PrimitiveType::Str | PrimitiveType::Int | PrimitiveType::Float)
                | Type::List(_)
                | Type::Map(_, _)
                | Type::Set(_)
        ) {
            Some(left_type.clone())
        } else {
            None
        }
    }

    fn resolve_numeric_binary_operator(&self, left_type: &Type) -> Option<Type> {
        if matches!(
            left_type,
            Type::Primitive(PrimitiveType::Int | PrimitiveType::Float)
        ) {
            Some(left_type.clone())
        } else {
            None
        }
    }

    fn resolve_comparison_binary_operator(&self, left_type: &Type) -> Option<Type> {
        if matches!(
            left_type,
            Type::Primitive(PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Str)
        ) || self.type_implements_interface(left_type, "Comparable")
        {
            Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
        } else {
            None
        }
    }

    fn resolve_logical_binary_operator(&self, left_type: &Type, right_type: &Type) -> Option<Type> {
        if matches!(left_type, Type::Primitive(PrimitiveType::Bool))
            && matches!(right_type, Type::Primitive(PrimitiveType::Bool))
        {
            Some(Type::Primitive(crate::ast::PrimitiveType::Bool))
        } else {
            None
        }
    }

    // first pass, collect hoistable declarations like functions and classes.
    fn collect_hoistable_declarations(&mut self, ast: &[AstNode]) -> Result<(), SemanticError> {
        for node in ast {
            match node {
                AstNode::Function(func) => {
                    self.collect_function_symbol(func)?;
                }
                AstNode::Class {
                    name,
                    traits,
                    fields,
                    methods,
                    type_params,
                    ..
                } => {
                    self.collect_class_symbol(
                        name,
                        traits,
                        fields,
                        methods,
                        type_params,
                        node.span(),
                    )?;
                }
                AstNode::Enum { name, variants, .. } => {
                    self.collect_enum_symbol(name, variants, node.span());
                }
                AstNode::Interface {
                    name,
                    type_params,
                    fields,
                    methods,
                    ..
                } => {
                    self.collect_interface_symbol(name, type_params, fields, methods, node.span());
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn collect_function_symbol(&mut self, func: &FunctionNode) -> Result<(), SemanticError> {
        if func.is_common {
            return Err(SemanticError::with_help(
                "Common methods are only allowed in classes",
                func.span,
                "The 'common' modifier creates a static method. Move this function inside a class definition, or remove the 'common' keyword.",
            ));
        }
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

        for (type_param_name, _) in &func.type_params {
            let var = Type::Variable(type_param_name.clone());
            func_type = self.substitute_type_param(&func_type, type_param_name, &var);
        }

        let default_count = func
            .params
            .iter()
            .filter(|p| p.default_value.is_some())
            .count();

        if let Err(e) = self.symbol_table.add_symbol(
            &func.name,
            self.make_function_symbol(func.span, func_type, &func.type_params, default_count),
        ) {
            self.errors.push(e);
        }
        Ok(())
    }

    fn collect_class_symbol(
        &mut self,
        name: &str,
        traits: &[TraitRef],
        fields: &[Field],
        methods: &[FunctionNode],
        type_params: &[(String, Vec<TraitBound>)],
        span: &Span,
    ) -> Result<(), SemanticError> {
        let implemented_interfaces = self.resolve_implemented_interfaces(traits, span)?;
        let type_param_bounds: Vec<(String, Vec<String>)> = type_params
            .iter()
            .map(|(p, b)| (p.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
            .collect();

        for (param_name, _) in type_params {
            let _ = self.symbol_table.add_symbol(
                param_name,
                Self::make_symbol(
                    SymbolKind::Type,
                    *span,
                    Some(Type::Generic(param_name.clone())),
                ),
            );
        }

        let (fields_map, _) = self.collect_class_fields(name, fields, span)?;
        let methods_map = self.collect_class_methods(methods, name, type_params)?;

        self.validate_interface_implementations(
            name,
            &implemented_interfaces,
            &fields_map,
            &methods_map,
            span,
        );

        if let Err(e) = self.symbol_table.add_symbol(
            name,
            Symbol {
                kind: SymbolKind::Class,
                span: *span,
                type_: Some(Type::Named(name.to_string(), vec![])),
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
        Ok(())
    }

    fn resolve_implemented_interfaces(
        &self,
        traits: &[TraitRef],
        _span: &Span,
    ) -> Result<HashMap<String, ResolvedInterface>, SemanticError> {
        let mut implemented_interfaces = std::collections::HashMap::new();
        for trait_ref in traits {
            if let Some(interface_symbol) = self.symbol_table.lookup(&trait_ref.name)
                && let Some((_, interface_methods)) =
                    interface_symbol.interfaces.get(&trait_ref.name)
            {
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
                            self.substitute_type_params(p, interface_type_params, &resolved_args)
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
                    .insert(trait_ref.name.clone(), (resolved_args, substituted_methods));
            }
        }
        Ok(implemented_interfaces)
    }

    fn collect_class_fields(
        &mut self,
        name: &str,
        fields: &[Field],
        _span: &Span,
    ) -> Result<(HashMap<String, ClassFieldInfo>, Vec<Type>), SemanticError> {
        let mut field_types = Vec::new();
        let mut fields_map = std::collections::HashMap::new();
        for field in fields {
            if fields_map.contains_key(&field.name) {
                return Err(SemanticError::with_help(
                    format!("Duplicate field '{}' in class '{}'", field.name, name),
                    field.type_.span,
                    "Each field name must be unique within a class. Rename or remove the duplicate field.",
                ));
            }
            match self.resolve_type(&field.type_) {
                Ok(t) => {
                    field_types.push(t.clone());
                    fields_map.insert(field.name.clone(), (t, field.is_const));
                }
                Err(e) => self.errors.push(e),
            }
        }
        Ok((fields_map, field_types))
    }

    fn collect_class_methods(
        &mut self,
        methods: &[FunctionNode],
        class_name: &str,
        type_params: &[(String, Vec<TraitBound>)],
    ) -> Result<HashMap<String, MethodSig>, SemanticError> {
        let mut methods_map = HashMap::new();
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

        let type_args: Vec<Type> = type_params
            .iter()
            .map(|(p, _)| Type::Variable(p.clone()))
            .collect();
        let new_sig = MethodSig {
            params: vec![],
            return_type: Type::Named(class_name.to_string(), type_args),
            is_static: true,
        };
        methods_map.insert("new".to_string(), new_sig);
        Ok(methods_map)
    }

    fn validate_interface_implementations(
        &mut self,
        class_name: &str,
        implemented_interfaces: &HashMap<String, ResolvedInterface>,
        fields_map: &HashMap<String, ClassFieldInfo>,
        methods_map: &HashMap<String, MethodSig>,
        span: &Span,
    ) {
        for (interface_name, (_, interface_methods)) in implemented_interfaces {
            self.validate_interface_methods(
                class_name,
                interface_name,
                interface_methods,
                methods_map,
                *span,
            );
            self.validate_interface_fields(class_name, interface_name, fields_map, *span);
        }
    }

    fn validate_interface_methods(
        &mut self,
        class_name: &str,
        interface_name: &str,
        interface_methods: &HashMap<String, MethodSig>,
        methods_map: &HashMap<String, MethodSig>,
        span: Span,
    ) {
        for (method_name, interface_sig) in interface_methods {
            if let Some(class_sig) = methods_map.get(method_name) {
                if let Err(e) = self.check_method_compatibility(interface_sig, class_sig, span) {
                    self.errors.push(e);
                }
            } else {
                self.errors.push(SemanticError::with_help(
                    format!(
                        "Class '{}' does not implement method '{}' required by interface '{}'",
                        class_name, method_name, interface_name
                    ),
                    span,
                    format!("Add a method '{}' to class '{}' with the signature required by interface '{}'", method_name, class_name, interface_name),
                ));
            }
        }
    }

    fn validate_interface_fields(
        &mut self,
        class_name: &str,
        interface_name: &str,
        fields_map: &HashMap<String, ClassFieldInfo>,
        span: Span,
    ) {
        if let Some(interface_symbol) = self.symbol_table.lookup(interface_name) {
            for (field_name, (interface_field_type, interface_is_const)) in &interface_symbol.fields
            {
                if let Some((class_field_type, class_is_const)) = fields_map.get(field_name) {
                    self.validate_field_type(
                        class_name,
                        interface_name,
                        field_name,
                        interface_field_type,
                        class_field_type,
                        span,
                    );
                    self.validate_field_const(
                        class_name,
                        interface_name,
                        field_name,
                        *interface_is_const,
                        *class_is_const,
                        span,
                    );
                } else {
                    self.errors.push(SemanticError::with_help(
                        format!(
                            "Class '{}' is missing required field '{}' from interface '{}'",
                            class_name, field_name, interface_name
                        ),
                        span,
                        format!(
                            "Add field '{}: {}' to class '{}'",
                            field_name,
                            format_type(interface_field_type),
                            class_name
                        ),
                    ));
                }
            }
        }
    }

    fn validate_field_type(
        &mut self,
        class_name: &str,
        interface_name: &str,
        field_name: &str,
        interface_field_type: &Type,
        class_field_type: &Type,
        span: Span,
    ) {
        if !self.types_compatible(class_field_type, interface_field_type) {
            self.errors.push(SemanticError::with_help(
                format!(
                    "Field '{}' type mismatch in class '{}': class has {}, interface '{}' requires {}",
                    field_name,
                    class_name,
                    format_type(class_field_type),
                    interface_name,
                    format_type(interface_field_type)
                ),
                span,
                format!("Change the type of field '{}' to {} to match interface '{}'", field_name, format_type(interface_field_type), interface_name),
            ));
        }
    }

    fn validate_field_const(
        &mut self,
        class_name: &str,
        interface_name: &str,
        field_name: &str,
        interface_is_const: bool,
        class_is_const: bool,
        span: Span,
    ) {
        if interface_is_const && !class_is_const {
            self.errors.push(SemanticError::with_help(
                format!(
                    "Field '{}' must be const in class '{}' to implement interface '{}'",
                    field_name, class_name, interface_name
                ),
                span,
                format!(
                    "Add the 'const' modifier to field '{}' in class '{}'",
                    field_name, class_name
                ),
            ));
        }
    }

    fn collect_enum_symbol(&mut self, name: &str, variants: &[EnumVariant], span: &Span) {
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
            let return_type = Type::Named(name.to_string(), vec![]);
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
                span: *span,
                type_: Some(Type::Named(name.to_string(), vec![])),
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

    fn collect_interface_symbol(
        &mut self,
        name: &str,
        type_params: &[(String, Vec<TraitBound>)],
        fields: &[Field],
        methods: &[FunctionNode],
        span: &Span,
    ) {
        let mut interface_methods = std::collections::HashMap::new();
        for method in methods {
            let param_types = method
                .params
                .iter()
                .map(|p| self.resolve_type(&p.type_))
                .collect::<Result<Vec<_>, _>>();
            if let Ok(param_types) = param_types {
                let return_type = self.resolve_type(&method.return_type);
                if let Ok(return_type) = return_type {
                    let method_sig = MethodSig {
                        params: param_types,
                        return_type,
                        is_static: false,
                    };
                    interface_methods.insert(method.name.clone(), method_sig);
                }
            }
        }

        let mut interface_fields = std::collections::HashMap::new();
        for field in fields {
            if let Ok(field_type) = self.resolve_type(&field.type_) {
                if let Some(default_expr) = &field.default_value
                    && let Ok(default_type) = self.infer_literal_type(default_expr)
                    && !self.types_compatible(&default_type, &field_type)
                {
                    self.errors.push(SemanticError::with_help(
                        format!(
                            "Default value type mismatch for field '{}': expected {}, got {}",
                            field.name,
                            format_type(&field_type),
                            format_type(&default_type)
                        ),
                        default_expr.span,
                        format!(
                            "The default value must match the field's declared type of {}",
                            format_type(&field_type)
                        ),
                    ));
                }
                interface_fields.insert(field.name.clone(), (field_type, field.is_const));
            }
        }

        let mut interfaces_map = std::collections::HashMap::new();
        interfaces_map.insert(name.to_string(), (vec![], interface_methods));
        if let Err(e) = self.symbol_table.add_symbol(
            name,
            Symbol {
                kind: SymbolKind::Interface,
                span: *span,
                type_: None,
                interfaces: interfaces_map,
                methods: std::collections::HashMap::new(),
                fields: interface_fields,
                type_params: type_params
                    .iter()
                    .map(|(p, b)| (p.clone(), b.iter().map(|tb| tb.name.clone()).collect()))
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
                    return Err(SemanticError::with_help(
                        "Common methods are only allowed inside class definitions",
                        func.span,
                        "The 'common' modifier creates static methods on a class. Remove 'common' for standalone functions.",
                    ));
                }

                // Check that main() returns void
                if func.name == "main" {
                    let return_type = self.resolve_type(&func.return_type)?;
                    if !matches!(return_type, Type::Void) {
                        return Err(SemanticError::with_help(
                            format!(
                                "Function 'main' must return void, not '{}'",
                                format_type(&return_type)
                            ),
                            func.return_type.span,
                            "The entry point 'main' function must be declared as 'returns void'. Change the return type to void.",
                        ));
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
                let type_param_bounds = self.resolve_type_param_bounds(type_params)?;
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
        for (param, bounds) in self.resolve_type_param_bounds(&func.type_params)? {
            self.current_bounds.insert(param, bounds);
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
                Self::make_symbol(SymbolKind::Variable, func.span, Some(self_type)),
            )?;
        }

        for param in &func.params {
            let param_type = self.resolve_type(&param.type_)?;
            self.symbol_table.add_symbol(
                &param.name,
                Self::make_symbol(SymbolKind::Variable, param.type_.span, Some(param_type)),
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

        if !matches!(return_type, Type::Void)
            && (func.body.is_empty() || !self.all_paths_return(&func.body))
        {
            return Err(SemanticError::with_help(
                format!(
                    "Function must return a value of type '{}' on all code paths",
                    format_type(&return_type)
                ),
                func.span,
                "Add a return statement at the end of every branch (if/else, match, etc.)",
            ));
        }

        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn all_paths_return(&self, stmts: &[StatementNode]) -> bool {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::Return(_) => return true,
                StatementKind::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    let else_returns = else_block
                        .as_ref()
                        .is_some_and(|b| self.all_paths_return(b));
                    if self.all_paths_return(then_block) && else_returns {
                        return true;
                    }
                }
                StatementKind::Block(block_stmts) => {
                    if self.all_paths_return(block_stmts) {
                        return true;
                    }
                }
                StatementKind::While { .. }
                | StatementKind::For { .. }
                | StatementKind::Break
                | StatementKind::Continue => {}
                StatementKind::Match { arms, .. } => {
                    if arms.iter().all(|arm| self.all_paths_return(&arm.body)) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn analyze_class(
        &mut self,
        name: &str,
        _fields: &[Field],
        methods: &[FunctionNode],
        type_params: &[(String, GenericBounds)],
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
                    self.make_function_symbol(
                        stmt.span,
                        func_type,
                        &func.type_params,
                        default_count,
                    ),
                )?;
            }
        }

        // then analyze all statements in order.
        for stmt in stmts {
            self.analyze_statement(stmt, files.as_deref_mut())?;
        }

        Ok(())
    }

    fn analyze_auto_decl_statement(
        &mut self,
        name: &str,
        expr: &ExpressionNode,
        span: Span,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(expr)?;
        let expr_type = self.get_expression_type(expr)?;
        if matches!(expr_type, Type::EmptyList | Type::EmptyMap | Type::EmptySet) {
            let collection_type = match expr_type {
                Type::EmptyList => "list",
                Type::EmptyMap => "map",
                Type::EmptySet => "set",
                _ => unreachable!(),
            };
            return Err(SemanticError::with_help(
                format!("Cannot infer type for empty {} literal", collection_type),
                expr.span,
                format!(
                    "Use an explicit type annotation, e.g. {}<int> myVar = {{}}",
                    collection_type
                ),
            ));
        }
        self.symbol_table.add_symbol(
            name,
            Self::make_symbol(SymbolKind::Variable, span, Some(expr_type)),
        )?;
        Ok(())
    }

    fn analyze_typed_decl_statement(
        &mut self,
        name: &str,
        type_node: &TypeNode,
        expr: &ExpressionNode,
        span: Span,
    ) -> Result<(), SemanticError> {
        let declared_type = self.resolve_type(type_node)?;
        self.analyze_expression(expr)?;
        let expr_type = self.get_expression_type(expr)?;
        self.check_type_compatibility(&declared_type, &expr_type, expr.span)?;
        self.symbol_table.add_symbol(
            name,
            Self::make_symbol(SymbolKind::Variable, span, Some(declared_type)),
        )?;
        Ok(())
    }

    fn analyze_const_decl_statement(
        &mut self,
        name: &str,
        type_node: &TypeNode,
        expr: &ExpressionNode,
        span: Span,
    ) -> Result<(), SemanticError> {
        let declared_type = self.resolve_type(type_node)?;
        self.analyze_expression(expr)?;
        let expr_type = self.get_expression_type(expr)?;
        self.check_type_compatibility(&declared_type, &expr_type, expr.span)?;
        self.symbol_table.add_symbol(
            name,
            Self::make_symbol(SymbolKind::Constant, span, Some(declared_type)),
        )?;
        Ok(())
    }

    fn analyze_if_statement(
        &mut self,
        cond: &ExpressionNode,
        then_block: &[StatementNode],
        else_block: &Option<Vec<StatementNode>>,
        mut files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(cond)?;

        self.symbol_table.push_scope()?;
        self.analyze_block(then_block, files.as_deref_mut())?;
        self.symbol_table.pop_scope()?;

        if let Some(else_block) = else_block {
            self.symbol_table.push_scope()?;
            self.analyze_block(else_block, files)?;
            self.symbol_table.pop_scope()?;
        }

        Ok(())
    }

    fn analyze_for_statement(
        &mut self,
        var: &str,
        iter: &ExpressionNode,
        body: &[StatementNode],
        span: Span,
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        self.symbol_table.push_scope()?;
        self.analyze_expression(iter)?;
        let iter_type = self.get_expression_type(iter)?;
        let var_type = match iter_type {
            Type::List(elem_type) => *elem_type,
            _ => {
                return Err(SemanticError::with_help(
                    format!("Cannot iterate over type {}", format_type(&iter_type)),
                    iter.span,
                    "The 'for' loop can only iterate over list types. Use .to_list() for sets/maps, or range(start, end) for numeric ranges.",
                ));
            }
        };
        self.symbol_table.add_symbol(
            var,
            Self::make_symbol(SymbolKind::Variable, span, Some(var_type)),
        )?;
        self.analyze_block(body, files)?;
        self.symbol_table.pop_scope()?;
        Ok(())
    }

    fn analyze_while_statement(
        &mut self,
        cond: &ExpressionNode,
        body: &[StatementNode],
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(cond)?;
        self.symbol_table.push_scope()?;
        self.analyze_block(body, files)?;
        self.symbol_table.pop_scope()?;
        Ok(())
    }

    fn collect_match_arm_returns(
        &mut self,
        arm: &crate::ast::MatchArm,
        expr_span: Span,
        expecting_return: bool,
        arm_return_types: &mut Vec<(Type, Span)>,
    ) -> Result<(), SemanticError> {
        if !expecting_return {
            return Ok(());
        }

        let mut has_return = false;
        for stmt in &arm.body {
            if let StatementKind::Return(Some(ret_expr)) = &stmt.kind {
                let ret_type = self.get_expression_type(ret_expr)?;
                arm_return_types.push((ret_type, stmt.span));
                has_return = true;
                break;
            }
            if let StatementKind::Return(None) = &stmt.kind {
                has_return = true;
            }
        }

        if has_return {
            return Ok(());
        }

        Err(SemanticError::with_help(
            "Match arm must return a value in a function with non-void return type",
            arm.body.first().map(|s| s.span).unwrap_or(expr_span),
            "Add a return statement to this match arm",
        ))
    }

    fn validate_match_arm_return_types(
        &self,
        arm_return_types: &[(Type, Span)],
    ) -> Result<(), SemanticError> {
        if arm_return_types.is_empty() {
            return Ok(());
        }

        let (first_type, _) = &arm_return_types[0];
        for (i, (arm_type, arm_span)) in arm_return_types.iter().enumerate().skip(1) {
            if self
                .check_type_compatibility(first_type, arm_type, *arm_span)
                .is_err()
            {
                return Err(SemanticError::with_help(
                    format!(
                        "Match arms have incompatible return types: first arm returns '{}', but arm {} returns '{}'",
                        format_type(first_type),
                        i + 1,
                        format_type(arm_type)
                    ),
                    *arm_span,
                    "All match arms must return the same type when used as an expression or in a returning context",
                ));
            }
        }

        Ok(())
    }

    fn analyze_match_statement(
        &mut self,
        expr: &ExpressionNode,
        arms: &[crate::ast::MatchArm],
        mut files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(expr)?;
        let expr_type = self.get_expression_type(expr)?;

        let expecting_return = self.current_return_type.is_some()
            && !matches!(self.current_return_type, Some(Type::Void));
        let mut arm_return_types = Vec::new();

        for arm in arms {
            self.symbol_table.push_scope()?;
            self.set_pattern_types(&arm.pattern, &expr_type, expr.span)?;
            self.analyze_pattern(&arm.pattern)?;
            if let Some(guard) = &arm.guard {
                self.analyze_expression(guard)?;
            }
            self.analyze_block(&arm.body, files.as_deref_mut())?;
            self.collect_match_arm_returns(
                arm,
                expr.span,
                expecting_return,
                &mut arm_return_types,
            )?;
            self.symbol_table.pop_scope()?;
        }

        self.validate_match_arm_return_types(&arm_return_types)?;
        self.check_match_exhaustiveness(&expr_type, arms, expr.span)
    }

    fn analyze_return_with_value(&mut self, expr: &ExpressionNode) -> Result<(), SemanticError> {
        if self.current_return_type.is_none() {
            return Err(SemanticError::with_help(
                "Cannot use 'return' outside of a function",
                expr.span,
                "'return' can only be used inside a function body",
            ));
        }

        self.analyze_expression(expr)?;

        if matches!(self.current_return_type, Some(Type::Void)) {
            return Err(SemanticError::with_help(
                "Cannot return a value from a void function",
                expr.span,
                "This function is declared as 'returns void'. Either remove the return value or change the function's return type.",
            ));
        }

        if let Some(expected_type) = self.current_return_type.clone() {
            let actual_type = self.get_expression_type(expr)?;
            self.check_type_compatibility(&expected_type, &actual_type, expr.span)?;
        }
        Ok(())
    }

    fn analyze_return_without_value(&mut self, span: Span) -> Result<(), SemanticError> {
        if self.current_return_type.is_none() {
            return Err(SemanticError::with_help(
                "Cannot use 'return' outside of a function",
                span,
                "'return' can only be used inside a function body",
            ));
        }

        if !matches!(self.current_return_type, Some(Type::Void))
            && let Some(expected_type) = &self.current_return_type
        {
            return Err(SemanticError::with_help(
                format!(
                    "Missing return value; function expects '{}', but return has no value",
                    format_type(expected_type)
                ),
                span,
                format!(
                    "Add a value after 'return' that matches the function's return type '{}'. Example: return some_value",
                    format_type(expected_type)
                ),
            ));
        }
        Ok(())
    }

    fn analyze_import_statement(
        &mut self,
        module_path: &str,
        spec: &ImportSpec,
        span: Span,
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        if module_path == "std" || module_path.starts_with("std.") {
            self.handle_std_import(module_path, spec, span, files)?;
            return Ok(());
        }

        let resolver = self
            .module_resolver
            .clone()
            .ok_or_else(|| SemanticError::new("Module resolver not available", span))?;

        let (has_file, has_directory) = resolver.borrow().check_module_path(module_path);

        if has_file && has_directory {
            return Err(SemanticError::with_help(
                format!("Ambiguous import: '{}'", module_path),
                span,
                format!(
                    "Both {}.mux and {}/ directory exist. Please remove one.",
                    module_path.replace('.', "/"),
                    module_path.replace('.', "/")
                ),
            ));
        }

        let files = files.expect("Files registry must be available for import processing");

        if has_directory {
            self.handle_directory_import(module_path, spec, span, resolver, files)?;
            return Ok(());
        }

        let module_nodes = resolver
            .borrow_mut()
            .resolve_import_path(module_path, self.current_file.as_deref(), files)
            .map_err(|e| {
                SemanticError::with_help(
                    format!("Failed to import module '{}'", module_path),
                    span,
                    e.to_string(),
                )
            })?;

        let mut module_analyzer = SemanticAnalyzer::new_for_module(resolver.clone());
        module_analyzer.set_current_file(std::path::PathBuf::from(
            module_path.replace('.', "/") + ".mux",
        ));
        let errors = module_analyzer.analyze(&module_nodes, Some(files));
        if !errors.is_empty() {
            let error_messages: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
            return Err(SemanticError::with_help(
                format!("Errors in imported module '{}'", module_path),
                span,
                format!(
                    "Fix the following errors in '{}':\n  {}",
                    module_path,
                    error_messages.join("\n  ")
                ),
            ));
        }

        let module_symbols =
            self.filter_module_export_symbols(&module_analyzer.symbol_table.all_symbols);

        match spec {
            ImportSpec::Module { alias } => {
                if let Some(namespace) = alias {
                    self.add_module_namespace(namespace, module_symbols, module_path, span)?;
                }
            }
            ImportSpec::Item { item, alias } => {
                let symbol_name = alias.as_ref().unwrap_or(item);
                self.import_single_symbol(&module_symbols, item, symbol_name, module_path, span)?;
            }
            ImportSpec::Items { items } => {
                for (item, alias) in items {
                    let symbol_name = alias.as_ref().unwrap_or(item);
                    self.import_single_symbol(
                        &module_symbols,
                        item,
                        symbol_name,
                        module_path,
                        span,
                    )?;
                }
            }
            ImportSpec::Wildcard => {
                self.import_all_symbols(&module_symbols, module_path, span)?;
            }
        }

        resolver
            .borrow_mut()
            .cache_module(module_path, module_nodes.clone());
        resolver.borrow_mut().finish_import(module_path);

        self.all_module_asts
            .insert(module_path.to_string(), module_nodes);

        if !self.module_dependencies.contains(&module_path.to_string()) {
            self.module_dependencies.push(module_path.to_string());
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
                self.analyze_auto_decl_statement(name, expr, stmt.span)?;
            }
            StatementKind::TypedDecl(name, type_node, expr) => {
                self.analyze_typed_decl_statement(name, type_node, expr, stmt.span)?;
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
                self.analyze_const_decl_statement(name, type_node, expr, stmt.span)?;
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.analyze_if_statement(cond, then_block, else_block, files.as_deref_mut())?;
            }
            StatementKind::For {
                var, iter, body, ..
            } => {
                self.analyze_for_statement(var, iter, body, stmt.span, files.as_deref_mut())?;
            }
            StatementKind::While { cond, body } => {
                self.analyze_while_statement(cond, body, files.as_deref_mut())?;
            }
            StatementKind::Match { expr, arms } => {
                self.analyze_match_statement(expr, arms, files.as_deref_mut())?;
            }
            StatementKind::Return(Some(expr)) => {
                self.analyze_return_with_value(expr)?;
            }
            StatementKind::Return(None) => {
                self.analyze_return_without_value(stmt.span)?;
            }
            StatementKind::Import { module_path, spec } => {
                self.analyze_import_statement(module_path, spec, stmt.span, files)?;
            }
            StatementKind::Function(func) => {
                self.analyze_function(func, None)?;
            }
            _ => {}
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
        match expr_type {
            Type::Result(_, _) => self.check_result_exhaustiveness(arms, expr_span),
            Type::Named(type_name, _) => {
                self.check_named_enum_exhaustiveness(type_name, arms, expr_type, expr_span)
            }
            Type::Optional(_) => self.check_optional_exhaustiveness(arms, expr_span),
            _ => self.require_wildcard_pattern(arms, expr_type, expr_span),
        }
    }

    fn check_result_exhaustiveness(
        &self,
        arms: &[crate::ast::MatchArm],
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let has_ok = arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(&arm.pattern, PatternNode::EnumVariant { name, .. } if name == "ok")
        });
        let has_err = arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(&arm.pattern, PatternNode::EnumVariant { name, .. } if name == "err")
        });
        let has_wildcard = arms
            .iter()
            .any(|arm| matches!(&arm.pattern, PatternNode::Wildcard));

        if has_wildcard || (has_ok && has_err) {
            return Ok(());
        }

        let mut missing = Vec::new();
        if !has_ok {
            missing.push("ok");
        }
        if !has_err {
            missing.push("err");
        }
        Err(SemanticError::with_help(
            format!(
                "Non-exhaustive match: missing pattern{} for Result: {}",
                if missing.len() > 1 { "s" } else { "" },
                missing.join(", ")
            ),
            expr_span,
            format!(
                "Add match arm{} for: {}, or add a wildcard '_' pattern to cover all remaining cases",
                if missing.len() > 1 { "s" } else { "" },
                missing.join(", ")
            ),
        ))
    }

    fn check_optional_exhaustiveness(
        &self,
        arms: &[crate::ast::MatchArm],
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let has_some = arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(&arm.pattern, PatternNode::EnumVariant { name, .. } if name == "some")
        });
        let has_none = arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(&arm.pattern, PatternNode::EnumVariant { name, .. } if name == "none")
        });
        let has_wildcard = arms
            .iter()
            .any(|arm| matches!(&arm.pattern, PatternNode::Wildcard));

        if has_wildcard || (has_some && has_none) {
            return Ok(());
        }

        let mut missing = Vec::new();
        if !has_some {
            missing.push("some");
        }
        if !has_none {
            missing.push("none");
        }
        Err(SemanticError::with_help(
            format!(
                "Non-exhaustive match: missing pattern{} for Optional: {}",
                if missing.len() > 1 { "s" } else { "" },
                missing.join(", ")
            ),
            expr_span,
            format!(
                "Add match arm{} for: {}, or add a wildcard '_' pattern",
                if missing.len() > 1 { "s" } else { "" },
                missing.join(", ")
            ),
        ))
    }

    fn check_named_enum_exhaustiveness(
        &self,
        type_name: &str,
        arms: &[crate::ast::MatchArm],
        expr_type: &Type,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let Some(symbol) = self.symbol_table.lookup(type_name) else {
            return self.require_wildcard_pattern(arms, expr_type, expr_span);
        };
        let Some(variant_names) = &symbol.variants else {
            return self.require_wildcard_pattern(arms, expr_type, expr_span);
        };

        let (covered, has_wildcard) = self.collect_enum_covered_variants(arms, variant_names);

        if has_wildcard {
            return Ok(());
        }

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
            return Err(SemanticError::with_help(
                format!(
                    "Non-exhaustive match: missing variant{} of '{}': {}",
                    if uncovered.len() > 1 { "s" } else { "" },
                    type_name,
                    uncovered_list
                ),
                expr_span,
                format!(
                    "Add match arm{} for: {}, or add a wildcard '_' pattern",
                    if uncovered.len() > 1 { "s" } else { "" },
                    uncovered_list
                ),
            ));
        }
        Ok(())
    }

    fn collect_enum_covered_variants(
        &self,
        arms: &[crate::ast::MatchArm],
        variant_names: &[String],
    ) -> (std::collections::HashSet<String>, bool) {
        let mut covered: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut has_wildcard = false;

        for arm in arms {
            match &arm.pattern {
                PatternNode::Wildcard => {
                    has_wildcard = true;
                    break;
                }
                PatternNode::EnumVariant { name, .. } => {
                    if arm.guard.is_none() {
                        covered.insert(name.clone());
                    }
                }
                PatternNode::Identifier(name) => {
                    if arm.guard.is_none() && variant_names.contains(name) {
                        covered.insert(name.clone());
                    }
                }
                _ => {}
            }
        }

        (covered, has_wildcard)
    }

    fn require_wildcard_pattern(
        &self,
        arms: &[crate::ast::MatchArm],
        expr_type: &Type,
        expr_span: Span,
    ) -> Result<(), SemanticError> {
        let has_wildcard = arms
            .iter()
            .any(|arm| matches!(arm.pattern, PatternNode::Wildcard));
        if !has_wildcard {
            return Err(SemanticError::with_help(
                format!("Non-exhaustive match on type '{}'", format_type(expr_type)),
                expr_span,
                "Add a wildcard '_' pattern as the last match arm to handle all remaining cases",
            ));
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
                self.handle_identifier_pattern(name, expected_type, span)?;
            }
            PatternNode::EnumVariant { name, args } => {
                self.handle_enum_variant_pattern(name, args, expected_type, span)?;
            }
            PatternNode::Literal(lit) => {
                self.handle_literal_pattern(lit, expected_type, span)?;
            }
            PatternNode::List { elements, rest } => {
                self.handle_list_pattern(elements, rest, expected_type, span)?;
            }
            PatternNode::Wildcard => {} // no binding
        }
        Ok(())
    }

    fn handle_identifier_pattern(
        &mut self,
        name: &str,
        expected_type: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        let is_constant = self
            .symbol_table
            .lookup(name)
            .map(|s| s.kind == SymbolKind::Constant)
            .unwrap_or(false);

        if is_constant {
            let const_type = self
                .symbol_table
                .lookup(name)
                .and_then(|s| s.type_.clone())
                .ok_or_else(|| {
                    SemanticError::new(format!("Constant '{}' has no type information", name), span)
                })?;
            self.check_type_compatibility(&const_type, expected_type, span)?;
        } else {
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
        Ok(())
    }

    fn handle_enum_variant_pattern(
        &mut self,
        name: &str,
        args: &[PatternNode],
        expected_type: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        match expected_type {
            Type::Optional(inner) => self.match_optional_variant(name, args, inner, span),
            Type::Result(ok_type, err_type) => {
                self.match_result_variant(name, args, ok_type, err_type, span)
            }
            Type::Named(enum_name, _) => {
                let symbol = self
                    .symbol_table
                    .lookup(enum_name)
                    .ok_or_else(|| self.undefined_symbol_error("type", enum_name, span))?;
                self.match_enum_variant(name, args, enum_name, &symbol, span)
            }
            _ => Err(SemanticError::with_help(
                format!(
                    "Enum variant patterns are not supported for type {}",
                    format_type(expected_type)
                ),
                span,
                "Variant patterns can only be used with enum, Optional, or Result types",
            )),
        }
    }

    fn match_optional_variant(
        &mut self,
        name: &str,
        args: &[PatternNode],
        inner: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        if name == "some" && args.len() == 1 {
            self.set_pattern_types(&args[0], inner, span)?;
            Ok(())
        } else if name == "none" && args.is_empty() {
            Ok(())
        } else {
            Err(SemanticError::with_help(
                format!(
                    "Pattern '{}' does not match type {}",
                    name,
                    format_type(&Type::Optional(Box::new(inner.clone())))
                ),
                span,
                "Optional values can only be matched with Some(value) or None",
            ))
        }
    }

    fn match_result_variant(
        &mut self,
        name: &str,
        args: &[PatternNode],
        ok_type: &Type,
        err_type: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        if name == "ok" && args.len() == 1 {
            self.set_pattern_types(&args[0], ok_type, span)?;
            Ok(())
        } else if name == "err" && args.len() == 1 {
            self.set_pattern_types(&args[0], err_type, span)?;
            Ok(())
        } else {
            Err(SemanticError::with_help(
                format!(
                    "Pattern '{}' does not match type {}",
                    name,
                    format_type(&Type::Result(
                        Box::new(ok_type.clone()),
                        Box::new(err_type.clone())
                    ))
                ),
                span,
                "Result values can only be matched with Ok(value) or Err(value)",
            ))
        }
    }

    fn handle_literal_pattern(
        &mut self,
        lit: &crate::ast::LiteralNode,
        expected_type: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        let literal_type = match lit {
            crate::ast::LiteralNode::Integer(_) => Type::Primitive(PrimitiveType::Int),
            crate::ast::LiteralNode::Float(_) => Type::Primitive(PrimitiveType::Float),
            crate::ast::LiteralNode::String(_) => Type::Primitive(PrimitiveType::Str),
            crate::ast::LiteralNode::Boolean(_) => Type::Primitive(PrimitiveType::Bool),
            crate::ast::LiteralNode::Char(_) => Type::Primitive(PrimitiveType::Char),
        };
        self.check_type_compatibility(&literal_type, expected_type, span)
    }

    fn handle_list_pattern(
        &mut self,
        elements: &[PatternNode],
        rest: &Option<Box<PatternNode>>,
        expected_type: &Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        let inner_type = match expected_type {
            Type::List(inner) => (**inner).clone(),
            Type::EmptyList => Type::Void,
            _ => {
                return Err(SemanticError::with_help(
                    format!(
                        "List pattern cannot match type {}",
                        format_type(expected_type)
                    ),
                    span,
                    "List patterns (e.g. [head, ...rest]) can only match list types",
                ));
            }
        };
        for elem in elements {
            self.set_pattern_types(elem, &inner_type, span)?;
        }
        if let Some(rest_pat) = rest {
            let rest_type = Type::List(Box::new(inner_type));
            self.set_pattern_types(rest_pat, &rest_type, span)?;
        }
        Ok(())
    }

    fn match_enum_variant(
        &mut self,
        name: &str,
        args: &[PatternNode],
        enum_name: &str,
        symbol: &crate::semantics::Symbol,
        span: Span,
    ) -> Result<(), SemanticError> {
        let sig = symbol.methods.get(name).ok_or_else(|| {
            let available_variants: Vec<&String> = symbol.methods.keys().collect();
            if available_variants.is_empty() {
                SemanticError::new(
                    format!("Unknown variant '{}' for enum '{}'", name, enum_name),
                    span,
                )
            } else {
                SemanticError::with_help(
                    format!("Unknown variant '{}' for enum '{}'", name, enum_name),
                    span,
                    format!(
                        "Available variants: {}",
                        available_variants
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )
            }
        })?;
        if args.len() != sig.params.len() {
            return Err(SemanticError::with_help(
                format!(
                    "Variant '{}' expects {} argument{}, but pattern provides {}",
                    name,
                    sig.params.len(),
                    if sig.params.len() == 1 { "" } else { "s" },
                    args.len()
                ),
                span,
                format!(
                    "Match the variant with exactly {} argument{}",
                    sig.params.len(),
                    if sig.params.len() == 1 { "" } else { "s" }
                ),
            ));
        }
        for (arg, param_type) in args.iter().zip(&sig.params) {
            self.set_pattern_types(arg, param_type, span)?;
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
            PatternNode::List { elements, rest } => {
                for elem in elements {
                    self.analyze_pattern(elem)?;
                }
                if let Some(rest_pat) = rest {
                    self.analyze_pattern(rest_pat)?;
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
            ExpressionKind::Identifier(name) => self.analyze_identifier_expr(name, expr),
            ExpressionKind::Literal(_) => Ok(()),
            ExpressionKind::None => Ok(()),
            ExpressionKind::Binary { left, right, .. } => {
                self.analyze_expression(left)?;
                self.analyze_expression(right)?;
                let _ = self.get_expression_type(expr)?;
                Ok(())
            }
            ExpressionKind::Unary {
                expr,
                op,
                op_span,
                postfix: _,
            } => self.analyze_unary_expr(expr, op, *op_span),
            ExpressionKind::Call { func, args } => self.analyze_call_expr(expr, func, args),
            ExpressionKind::FieldAccess { expr, .. } => {
                self.analyze_expression(expr)?;
                let _ = self.get_expression_type(expr)?;
                Ok(())
            }
            ExpressionKind::ListAccess { expr, index } => {
                self.analyze_list_access_expr(expr, index)
            }
            ExpressionKind::ListLiteral(elements) => self.analyze_list_literal_expr(elements),
            ExpressionKind::MapLiteral { entries, .. } => {
                self.analyze_map_literal_expr(expr, entries)
            }
            ExpressionKind::SetLiteral(elements) => self.analyze_set_literal_expr(elements),
            ExpressionKind::TupleLiteral(elements) => {
                self.analyze_tuple_literal_expr(expr, elements)
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => self.analyze_if_expr(cond, then_expr, else_expr),
            ExpressionKind::Lambda {
                params,
                return_type,
                body,
            } => self.analyze_lambda_expr(expr, params, return_type, body),
            ExpressionKind::GenericType(name, type_args) => {
                self.analyze_generic_type_expr(expr, name, type_args)
            }
        }
    }

    fn analyze_identifier_expr(
        &mut self,
        name: &str,
        expr: &ExpressionNode,
    ) -> Result<(), SemanticError> {
        if name == "self" {
            return self.analyze_self_identifier(expr);
        }
        let mut exists_like =
            self.symbol_table.exists(name) || self.get_builtin_sig(name).is_some();
        if !exists_like {
            exists_like = self.check_stdlib_imports_for_class(name);
        }
        if !exists_like {
            return Err(self.undefined_symbol_error("variable", name, expr.span));
        }
        Ok(())
    }

    fn analyze_self_identifier(&self, expr: &ExpressionNode) -> Result<(), SemanticError> {
        if self.is_in_static_method {
            return Err(SemanticError::with_help(
                "Cannot use 'self' in a common method",
                expr.span,
                "Common (static) methods do not have access to 'self'. Remove the 'common' modifier or access the class through a parameter instead.",
            ));
        }
        if self.current_self_type.is_none() {
            return Err(SemanticError::with_help(
                "Cannot use 'self' outside of a method",
                expr.span,
                "'self' is only available inside instance methods of a class",
            ));
        }
        Ok(())
    }

    fn check_stdlib_imports_for_class(&self, name: &str) -> bool {
        let stdlib_names: std::collections::HashSet<String> = Self::stdlib_modules()
            .iter()
            .map(|(_, n)| n.to_string())
            .collect();
        for (ns, module_symbols) in &self.imported_symbols {
            if !stdlib_names.contains(ns) {
                continue;
            }
            if let Some(sym) = module_symbols.get(name)
                && matches!(sym.kind, SymbolKind::Class)
            {
                return true;
            }
        }
        false
    }

    fn analyze_unary_expr(
        &mut self,
        expr: &ExpressionNode,
        op: &UnaryOp,
        op_span: Span,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(expr)?;
        let operand_type = self.get_expression_type(expr)?;
        match op {
            UnaryOp::Not => self.check_not_operator_type(&operand_type, op_span),
            UnaryOp::Neg => self.check_neg_operator_type(&operand_type, op_span),
            UnaryOp::Ref => Ok(()),
            UnaryOp::Incr | UnaryOp::Decr => {
                self.check_incr_decr_operator_type(&operand_type, op_span)?;
                self.check_incr_decr_const_modification(expr, op_span)
            }
            _ => Ok(()),
        }
    }

    fn check_not_operator_type(
        &self,
        operand_type: &Type,
        op_span: Span,
    ) -> Result<(), SemanticError> {
        if !matches!(
            operand_type,
            Type::Primitive(crate::ast::PrimitiveType::Bool)
        ) {
            return Err(SemanticError::with_help(
                format!(
                    "Logical 'not' operator requires a boolean operand, found {}",
                    format_type(operand_type)
                ),
                op_span,
                "The '!' operator can only be applied to bool values",
            ));
        }
        Ok(())
    }

    fn check_neg_operator_type(
        &self,
        operand_type: &Type,
        op_span: Span,
    ) -> Result<(), SemanticError> {
        if !matches!(
            operand_type,
            Type::Primitive(crate::ast::PrimitiveType::Int)
                | Type::Primitive(crate::ast::PrimitiveType::Float)
        ) {
            return Err(SemanticError::with_help(
                format!(
                    "Negation operator '-' requires a numeric operand, found {}",
                    format_type(operand_type)
                ),
                op_span,
                "The unary '-' operator can only be applied to int or float values",
            ));
        }
        Ok(())
    }

    fn check_incr_decr_operator_type(
        &self,
        operand_type: &Type,
        op_span: Span,
    ) -> Result<(), SemanticError> {
        if !matches!(
            operand_type,
            Type::Primitive(crate::ast::PrimitiveType::Int)
        ) {
            return Err(SemanticError::with_help(
                format!(
                    "Increment/decrement operators require an int operand, found {}",
                    format_type(operand_type)
                ),
                op_span,
                "The '++' and '--' operators can only be applied to int variables",
            ));
        }
        Ok(())
    }

    fn check_incr_decr_const_modification(
        &mut self,
        expr: &ExpressionNode,
        op_span: Span,
    ) -> Result<(), SemanticError> {
        if let crate::ast::ExpressionKind::Identifier(name) = &expr.kind
            && let Some(symbol) = self.symbol_table.lookup(name)
            && symbol.kind == SymbolKind::Constant
        {
            return Err(SemanticError::with_help(
                format!("Cannot modify constant '{}'", name),
                op_span,
                "Constants cannot be modified after initialization",
            ));
        }

        if let crate::ast::ExpressionKind::FieldAccess {
            expr: obj_expr,
            field,
        } = &expr.kind
        {
            let obj_type = self.get_expression_type(obj_expr)?;
            if let Type::Named(class_name, _) = &obj_type
                && let Some(symbol) = self.symbol_table.lookup(class_name)
                && let Some((_field_type, is_const)) = symbol.fields.get(field)
                && *is_const
            {
                return Err(SemanticError::with_help(
                    format!("Cannot modify const field '{}'", field),
                    op_span,
                    "Const fields cannot be modified after initialization. Remove the 'const' modifier from the field declaration if mutation is needed.",
                ));
            }
        }
        Ok(())
    }

    fn analyze_call_expr(
        &mut self,
        expr: &ExpressionNode,
        func: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<(), SemanticError> {
        if let ExpressionKind::Identifier(name) = &func.kind
            && !self.symbol_table.exists(name)
            && self.get_builtin_sig(name).is_none()
        {
            return Err(self.undefined_symbol_error("function", name, func.span));
        }

        self.analyze_expression(func)?;
        for arg in args {
            self.analyze_expression(arg)?;
        }

        if let ExpressionKind::Identifier(name) = &func.kind
            && name == "some"
        {
            self.check_some_call_args(expr, args)?;
        }

        let _ = self.get_expression_type(expr)?;
        Ok(())
    }

    fn check_some_call_args(
        &mut self,
        expr: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<(), SemanticError> {
        if args.len() != 1 {
            return Err(SemanticError::with_help(
                format!("Some() takes exactly 1 argument, got {}", args.len()),
                expr.span,
                "Wrap a single value in Some(), e.g. Some(42)",
            ));
        }
        let arg_type = self.get_expression_type(&args[0])?;
        if let Type::Optional(_) = arg_type {
            return Err(SemanticError::with_help(
                "Some() cannot wrap an Optional value",
                expr.span,
                "The argument to Some() must not be Optional. Remove the nested Some() or unwrap the inner value first.",
            ));
        }
        Ok(())
    }

    fn analyze_list_access_expr(
        &mut self,
        expr: &ExpressionNode,
        index: &ExpressionNode,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(expr)?;
        self.analyze_expression(index)?;
        let target_type = self.get_expression_type(expr)?;
        let index_type = self.get_expression_type(index)?;
        match &target_type {
            Type::List(_) => {
                if !matches!(index_type, Type::Primitive(crate::ast::PrimitiveType::Int)) {
                    return Err(SemanticError::with_help(
                        format!(
                            "List index must be an integer, found {}",
                            format_type(&index_type)
                        ),
                        index.span,
                        "Lists can only be indexed with integer values, e.g. myList[0]",
                    ));
                }
            }
            Type::Map(expected_key_type, _) => {
                if index_type != **expected_key_type {
                    return Err(SemanticError::with_help(
                        format!(
                            "Map key type mismatch: expected {}, found {}",
                            format_type(expected_key_type),
                            format_type(&index_type)
                        ),
                        index.span,
                        format!(
                            "This map has keys of type {}",
                            format_type(expected_key_type)
                        ),
                    ));
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

    fn analyze_list_literal_expr(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<(), SemanticError> {
        for elem in elements {
            self.analyze_expression(elem)?;
        }
        if !elements.is_empty() {
            let first_type = self.get_expression_type(&elements[0])?;
            for elem in &elements[1..] {
                let elem_type = self.get_expression_type(elem)?;
                if elem_type != first_type {
                    return Err(SemanticError::with_help(
                        format!(
                            "List element type mismatch: expected {}, found {}",
                            format_type(&first_type),
                            format_type(&elem_type)
                        ),
                        elem.span,
                        "All elements in a list literal must have the same type",
                    ));
                }
            }
        }
        Ok(())
    }

    fn analyze_map_literal_expr(
        &mut self,
        _expr: &ExpressionNode,
        entries: &[(ExpressionNode, ExpressionNode)],
    ) -> Result<(), SemanticError> {
        for (key, value) in entries {
            self.analyze_expression(key)?;
            self.analyze_expression(value)?;
        }
        if !entries.is_empty() {
            let (first_key, first_value) = &entries[0];
            let key_type = self.get_expression_type(first_key)?;
            self.check_map_key_hashable(first_key, &key_type)?;

            let value_type = self.get_expression_type(first_value)?;
            for (key, value) in &entries[1..] {
                self.check_map_entry_type_consistency(key, value, &key_type, &value_type)?;
            }
        }
        Ok(())
    }

    fn check_map_key_hashable(
        &self,
        key_expr: &ExpressionNode,
        key_type: &Type,
    ) -> Result<(), SemanticError> {
        let is_hashable = matches!(key_type, Type::Primitive(_));
        if !is_hashable {
            return Err(SemanticError::with_help(
                format!(
                    "Map keys must be a hashable type, found '{}'",
                    format_type(key_type)
                ),
                key_expr.span,
                "Only primitive types (int, float, string, bool, char) can be used as map keys",
            ));
        }
        Ok(())
    }

    fn check_map_entry_type_consistency(
        &mut self,
        key: &ExpressionNode,
        value: &ExpressionNode,
        expected_key: &Type,
        expected_value: &Type,
    ) -> Result<(), SemanticError> {
        let k_type = self.get_expression_type(key)?;
        self.check_map_key_hashable(key, &k_type)?;

        let v_type = self.get_expression_type(value)?;
        if k_type != *expected_key {
            return Err(SemanticError::with_help(
                format!(
                    "Map key type mismatch: expected {}, found {}",
                    format_type(expected_key),
                    format_type(&k_type)
                ),
                key.span,
                "All keys in a map literal must have the same type",
            ));
        }
        if v_type != *expected_value {
            return Err(SemanticError::with_help(
                format!(
                    "Map value type mismatch: expected {}, found {}",
                    format_type(expected_value),
                    format_type(&v_type)
                ),
                value.span,
                "All values in a map literal must have the same type",
            ));
        }
        Ok(())
    }

    fn analyze_set_literal_expr(
        &mut self,
        elements: &[ExpressionNode],
    ) -> Result<(), SemanticError> {
        for elem in elements {
            self.analyze_expression(elem)?;
        }
        if !elements.is_empty() {
            let first_type = self.get_expression_type(&elements[0])?;
            for elem in &elements[1..] {
                let elem_type = self.get_expression_type(elem)?;
                if elem_type != first_type {
                    return Err(SemanticError::with_help(
                        format!(
                            "Set element type mismatch: expected {}, found {}",
                            format_type(&first_type),
                            format_type(&elem_type)
                        ),
                        elem.span,
                        "All elements in a set literal must have the same type",
                    ));
                }
            }
        }
        Ok(())
    }

    fn analyze_tuple_literal_expr(
        &mut self,
        expr: &ExpressionNode,
        elements: &[ExpressionNode],
    ) -> Result<(), SemanticError> {
        if elements.len() != 2 {
            return Err(SemanticError::with_help(
                format!(
                    "Tuple must have exactly 2 elements, found {}",
                    elements.len()
                ),
                expr.span,
                "Tuples in Mux are pairs with exactly two elements, e.g. (1, 2)",
            ));
        }
        for elem in elements {
            self.analyze_expression(elem)?;
        }
        Ok(())
    }

    fn analyze_if_expr(
        &mut self,
        cond: &ExpressionNode,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
    ) -> Result<(), SemanticError> {
        self.analyze_expression(cond)?;
        self.analyze_expression(then_expr)?;
        self.analyze_expression(else_expr)?;
        let cond_type = self.get_expression_type(cond)?;
        if !matches!(cond_type, Type::Primitive(crate::ast::PrimitiveType::Bool)) {
            return Err(SemanticError::with_help(
                format!(
                    "If condition must be boolean, found {}",
                    format_type(&cond_type)
                ),
                cond.span,
                "The condition in an if expression must evaluate to a bool value",
            ));
        }
        Ok(())
    }

    fn analyze_lambda_expr(
        &mut self,
        expr: &ExpressionNode,
        params: &[Param],
        return_type: &TypeNode,
        body: &[StatementNode],
    ) -> Result<(), SemanticError> {
        let mut local_vars = std::collections::HashSet::new();
        for param in params {
            local_vars.insert(param.name.clone());
        }

        self.symbol_table.push_scope()?;

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

        if !matches!(self.current_return_type, Some(Type::Void)) {
            self.check_lambda_return_paths(expr, body, &lambda_return_type)?;
        }

        self.current_return_type = prev_return_type;
        let captures = self.find_free_variables_in_block(body, &local_vars)?;
        self.lambda_captures.insert(expr.span, captures);

        self.symbol_table.pop_scope()?;
        Ok(())
    }

    fn check_lambda_return_paths(
        &mut self,
        expr: &ExpressionNode,
        body: &[StatementNode],
        lambda_return_type: &Type,
    ) -> Result<(), SemanticError> {
        if body.is_empty() || !self.all_paths_return(body) {
            return Err(SemanticError::with_help(
                format!(
                    "Lambda must return a value of type '{}' on all code paths",
                    format_type(lambda_return_type)
                ),
                expr.span,
                "Add a return statement at the end of every branch in the lambda body",
            ));
        }
        if let Some(last_stmt) = body.last()
            && let StatementKind::Return(Some(ret_expr)) = &last_stmt.kind
        {
            let actual_type = self.get_expression_type(ret_expr)?;
            self.check_type_compatibility(lambda_return_type, &actual_type, ret_expr.span)?;
        }
        Ok(())
    }

    fn analyze_generic_type_expr(
        &mut self,
        expr: &ExpressionNode,
        name: &str,
        type_args: &[TypeNode],
    ) -> Result<(), SemanticError> {
        if name == "tuple" {
            return self.check_tuple_type_args(expr, type_args);
        }
        if let Some((module_name, type_name)) = name.split_once('.') {
            let module_symbols = self
                .imported_symbols
                .get(module_name)
                .ok_or_else(|| self.undefined_symbol_error("module", module_name, expr.span))?;
            let _ = module_symbols
                .get(type_name)
                .ok_or_else(|| self.undefined_symbol_error("type", type_name, expr.span))?;
        } else if !self.symbol_table.exists(name) {
            return Err(self.undefined_symbol_error("type", name, expr.span));
        }
        Ok(())
    }

    fn check_tuple_type_args(
        &self,
        expr: &ExpressionNode,
        type_args: &[TypeNode],
    ) -> Result<(), SemanticError> {
        if type_args.len() != 2 {
            return Err(SemanticError::with_help(
                format!(
                    "Tuple type requires exactly 2 type arguments, got {}",
                    type_args.len()
                ),
                expr.span,
                "Tuples in Mux are pairs, e.g. tuple<int, string>",
            ));
        }
        for arg in type_args {
            self.resolve_type(arg)?;
        }
        Ok(())
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
            _ => Err(SemanticError::with_help(
                "Expected a literal expression",
                expr.span,
                "Only literal values (integers, floats, strings, booleans, chars) are allowed here",
            )),
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

    fn filter_module_export_symbols(
        &self,
        symbols: &HashMap<String, Symbol>,
    ) -> HashMap<String, Symbol> {
        symbols
            .iter()
            .filter(|(_, symbol)| {
                matches!(
                    symbol.kind,
                    SymbolKind::Function
                        | SymbolKind::Class
                        | SymbolKind::Interface
                        | SymbolKind::Enum
                        | SymbolKind::Constant
                )
            })
            .map(|(name, symbol)| (name.clone(), symbol.clone()))
            .collect()
    }

    fn collect_declared_module_symbols(
        &self,
        module_nodes: &[AstNode],
        module_analyzer: &SemanticAnalyzer,
    ) -> HashMap<String, Symbol> {
        let global_symbols = module_analyzer.symbol_table.global_scope_symbols();
        let declared: HashMap<String, Symbol> = module_nodes
            .iter()
            .filter_map(Self::declared_symbol_name)
            .filter_map(|symbol_name| {
                global_symbols
                    .get(symbol_name)
                    .cloned()
                    .map(|symbol| (symbol_name.to_string(), symbol))
            })
            .collect();

        self.filter_module_export_symbols(&declared)
    }

    fn declared_symbol_name(node: &AstNode) -> Option<&str> {
        match node {
            AstNode::Function(func) => Some(func.name.as_str()),
            AstNode::Class { name, .. }
            | AstNode::Interface { name, .. }
            | AstNode::Enum { name, .. } => Some(name.as_str()),
            AstNode::Statement(stmt) => Self::declared_statement_symbol_name(stmt),
        }
    }

    fn declared_statement_symbol_name(stmt: &StatementNode) -> Option<&str> {
        match &stmt.kind {
            StatementKind::AutoDecl(name, _, _)
            | StatementKind::TypedDecl(name, _, _)
            | StatementKind::ConstDecl(name, _, _) => Some(name.as_str()),
            StatementKind::Function(func) => Some(func.name.as_str()),
            _ => None,
        }
    }

    fn analyze_imported_module(
        &mut self,
        module_nodes: &[AstNode],
        module_path: &str,
        resolver: &Rc<RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut Files,
        span: Span,
    ) -> Result<HashMap<String, Symbol>, SemanticError> {
        let mut module_analyzer = SemanticAnalyzer::new_for_module(resolver.clone());
        module_analyzer.set_current_file(std::path::PathBuf::from(
            module_path.replace('.', "/") + ".mux",
        ));
        let errors = module_analyzer.analyze(module_nodes, Some(files));
        if !errors.is_empty() {
            resolver.borrow_mut().finish_import(module_path);
            let error_messages: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
            return Err(SemanticError::with_help(
                format!("Errors in imported module '{}'", module_path),
                span,
                format!(
                    "Fix the following errors in '{}':\n  {}",
                    module_path,
                    error_messages.join("\n  ")
                ),
            ));
        }
        Ok(self.collect_declared_module_symbols(module_nodes, &module_analyzer))
    }

    fn mangle_and_import_module_symbols(
        &mut self,
        module_symbols: &HashMap<String, Symbol>,
        module_path: &str,
    ) -> Result<(), SemanticError> {
        let module_name_for_mangling = Self::sanitize_module_path(module_path);
        for (name, symbol) in module_symbols {
            let name_str = name.as_str();
            let is_unmangled_builtin_function = matches!(symbol.kind, SymbolKind::Function)
                && (name_str.starts_with("print")
                    || name_str.starts_with("read_line")
                    || name_str.starts_with("range")
                    || name_str.starts_with("some")
                    || name_str.starts_with("none")
                    || name_str.starts_with("ok")
                    || name_str.starts_with("err"));

            if !is_unmangled_builtin_function && !self.symbol_table.all_symbols.contains_key(name) {
                let mut mangled_symbol = symbol.clone();
                if matches!(symbol.kind, SymbolKind::Function) {
                    mangled_symbol.llvm_name =
                        Some(format!("{}!{}", module_name_for_mangling, name));
                }
                self.symbol_table
                    .all_symbols
                    .insert(name.clone(), mangled_symbol);
            }
        }

        Ok(())
    }

    fn handle_import_spec(
        &mut self,
        spec: &crate::ast::ImportSpec,
        module_symbols: &HashMap<String, Symbol>,
        module_path: &str,
        span: Span,
    ) -> Result<(), SemanticError> {
        match spec {
            ImportSpec::Module { alias } => {
                if let Some(namespace) = alias {
                    self.add_module_namespace(
                        namespace,
                        module_symbols.clone(),
                        module_path,
                        span,
                    )?;
                } else if module_path.contains('.') {
                    let name = module_path
                        .split('.')
                        .next_back()
                        .expect("module path should include at least one segment");
                    self.add_module_namespace(name, module_symbols.clone(), module_path, span)?;
                }
            }
            ImportSpec::Item { item, alias } => {
                let symbol_name = alias.as_ref().unwrap_or(item);
                self.import_single_symbol(module_symbols, item, symbol_name, module_path, span)?;
            }
            ImportSpec::Items { items } => {
                for (item, alias) in items {
                    let symbol_name = alias.as_ref().unwrap_or(item);
                    self.import_single_symbol(
                        module_symbols,
                        item,
                        symbol_name,
                        module_path,
                        span,
                    )?;
                }
            }
            ImportSpec::Wildcard => {
                self.import_all_symbols(module_symbols, module_path, span)?;
            }
        }
        Ok(())
    }

    fn import_module_from_resolver(
        &mut self,
        module_path: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
        files: &mut Files,
    ) -> Result<(), SemanticError> {
        let resolver = self
            .module_resolver
            .clone()
            .ok_or_else(|| SemanticError::new("Module resolver not available", span))?;

        let (has_file, has_directory) = resolver.borrow().check_module_path(module_path);
        if has_file && has_directory {
            return Err(SemanticError::with_help(
                format!("Ambiguous import: '{}'", module_path),
                span,
                format!(
                    "Both {}.mux and {}/ directory exist. Please remove one.",
                    module_path.replace('.', "/"),
                    module_path.replace('.', "/")
                ),
            ));
        }

        if has_directory {
            self.handle_directory_import(module_path, spec, span, resolver, files)?;
            return Ok(());
        }

        let module_nodes = resolver
            .borrow_mut()
            .resolve_import_path(module_path, self.current_file.as_deref(), files)
            .map_err(|e| {
                SemanticError::with_help(
                    format!("Failed to import module '{}'", module_path),
                    span,
                    e.to_string(),
                )
            })?;

        let module_symbols =
            self.analyze_imported_module(&module_nodes, module_path, &resolver, files, span)?;
        self.mangle_and_import_module_symbols(&module_symbols, module_path)?;

        self.handle_import_spec(spec, &module_symbols, module_path, span)?;

        resolver
            .borrow_mut()
            .cache_module(module_path, module_nodes.clone());
        resolver.borrow_mut().finish_import(module_path);
        self.all_module_asts
            .insert(module_path.to_string(), module_nodes);

        if !self.module_dependencies.contains(&module_path.to_string()) {
            self.module_dependencies.push(module_path.to_string());
        }

        Ok(())
    }

    // Handle directory-based module import (e.g., import utils where utils/ is a directory)
    fn resolve_and_register_submodule(
        &mut self,
        submodule_path: &str,
        namespace: &str,
        span: Span,
        resolver: &Rc<RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut Files,
    ) -> Result<Symbol, SemanticError> {
        let submodule_nodes = resolver
            .borrow_mut()
            .resolve_import_path(submodule_path, self.current_file.as_deref(), files)
            .map_err(|e| {
                SemanticError::with_help(
                    format!("Failed to import submodule '{}'", submodule_path),
                    span,
                    e.to_string(),
                )
            })?;

        let mut submodule_analyzer = SemanticAnalyzer::new_for_module(resolver.clone());
        submodule_analyzer.set_current_file(std::path::PathBuf::from(
            submodule_path.replace('.', "/") + ".mux",
        ));
        let errors = submodule_analyzer.analyze(&submodule_nodes, Some(files));
        if !errors.is_empty() {
            resolver.borrow_mut().finish_import(submodule_path);
            let error_messages: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
            return Err(SemanticError::with_help(
                format!("Errors in submodule '{}'", submodule_path),
                span,
                format!(
                    "Fix the following errors in '{}':\n  {}",
                    submodule_path,
                    error_messages.join("\n  ")
                ),
            ));
        }

        let submodule_symbols =
            self.collect_declared_module_symbols(&submodule_nodes, &submodule_analyzer);

        self.mangle_and_import_module_symbols(&submodule_symbols, submodule_path)?;

        let mangled_submodule_symbols =
            self.mangle_module_symbols(&submodule_symbols, submodule_path);
        self.imported_symbols
            .insert(namespace.to_string(), mangled_submodule_symbols);

        let symbol = self.make_module_symbol(namespace, span);

        resolver
            .borrow_mut()
            .cache_module(submodule_path, submodule_nodes.clone());
        resolver.borrow_mut().finish_import(submodule_path);

        self.all_module_asts
            .insert(submodule_path.to_string(), submodule_nodes);

        if !self
            .module_dependencies
            .contains(&submodule_path.to_string())
        {
            self.module_dependencies.push(submodule_path.to_string());
        }

        Ok(symbol)
    }

    fn handle_directory_import(
        &mut self,
        module_path: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
        resolver: std::rc::Rc<std::cell::RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut crate::diagnostic::Files,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;
        let submodules = self.get_directory_submodules(module_path, span, &resolver)?;
        match spec {
            ImportSpec::Module { alias } => self.import_directory_as_module(
                module_path,
                alias.as_deref(),
                &submodules,
                span,
                &resolver,
                files,
            )?,
            ImportSpec::Item { item, alias } => {
                self.ensure_directory_submodule_exists(module_path, item, &submodules, span)?;
                self.import_directory_single_item(
                    module_path,
                    item,
                    alias.as_deref(),
                    span,
                    &resolver,
                    files,
                )?
            }
            ImportSpec::Items { items } => self.import_directory_items(
                module_path,
                items,
                &submodules,
                span,
                &resolver,
                files,
            )?,
            ImportSpec::Wildcard => {
                self.import_directory_wildcard(module_path, &submodules, span, &resolver, files)?
            }
        }
        Ok(())
    }

    fn get_directory_submodules(
        &self,
        module_path: &str,
        span: Span,
        resolver: &std::rc::Rc<std::cell::RefCell<crate::module_resolver::ModuleResolver>>,
    ) -> Result<Vec<String>, SemanticError> {
        resolver
            .borrow()
            .get_submodules(module_path)
            .map_err(|e| SemanticError::new(format!("Failed to get submodules: {}", e), span))
    }

    fn ensure_directory_submodule_exists(
        &self,
        module_path: &str,
        submodule_name: &str,
        submodules: &[String],
        span: Span,
    ) -> Result<(), SemanticError> {
        if submodules.contains(&submodule_name.to_string()) {
            return Ok(());
        }
        Err(SemanticError::with_help(
            format!(
                "Submodule '{}' not found in '{}'",
                submodule_name, module_path
            ),
            span,
            format!("Available submodules: {}", submodules.join(", ")),
        ))
    }

    fn import_directory_as_module(
        &mut self,
        module_path: &str,
        alias: Option<&str>,
        submodules: &[String],
        span: Span,
        resolver: &std::rc::Rc<std::cell::RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut crate::diagnostic::Files,
    ) -> Result<(), SemanticError> {
        let namespace = alias.unwrap_or(module_path);
        let mut module_symbols = std::collections::HashMap::new();
        for submodule_name in submodules {
            let submodule_path = format!("{}.{}", module_path, submodule_name);
            let symbol = self.resolve_and_register_submodule(
                &submodule_path,
                submodule_name,
                span,
                resolver,
                files,
            )?;
            module_symbols.insert(submodule_name.to_string(), symbol);
        }
        self.imported_symbols
            .insert(namespace.to_string(), module_symbols);
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

    fn import_directory_single_item(
        &mut self,
        module_path: &str,
        item: &str,
        alias: Option<&str>,
        span: Span,
        resolver: &std::rc::Rc<std::cell::RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut crate::diagnostic::Files,
    ) -> Result<(), SemanticError> {
        let submodule_path = format!("{}.{}", module_path, item);
        let namespace = alias.unwrap_or(item);
        let symbol =
            self.resolve_and_register_submodule(&submodule_path, namespace, span, resolver, files)?;
        self.symbol_table.add_symbol(namespace, symbol)?;
        Ok(())
    }

    fn import_directory_items(
        &mut self,
        module_path: &str,
        items: &[(String, Option<String>)],
        submodules: &[String],
        span: Span,
        resolver: &std::rc::Rc<std::cell::RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut crate::diagnostic::Files,
    ) -> Result<(), SemanticError> {
        for (item, alias) in items {
            self.ensure_directory_submodule_exists(module_path, item, submodules, span)?;
            self.import_directory_single_item(
                module_path,
                item,
                alias.as_deref(),
                span,
                resolver,
                files,
            )?;
        }
        Ok(())
    }

    fn import_directory_wildcard(
        &mut self,
        module_path: &str,
        submodules: &[String],
        span: Span,
        resolver: &std::rc::Rc<std::cell::RefCell<crate::module_resolver::ModuleResolver>>,
        files: &mut crate::diagnostic::Files,
    ) -> Result<(), SemanticError> {
        for submodule_name in submodules {
            let submodule_path = format!("{}.{}", module_path, submodule_name);
            let symbol = self.resolve_and_register_submodule(
                &submodule_path,
                submodule_name,
                span,
                resolver,
                files,
            )?;
            self.symbol_table.add_symbol(submodule_name, symbol)?;
        }
        Ok(())
    }

    // Helper method to mangle module symbols
    fn mangle_module_symbols(
        &self,
        symbols: &std::collections::HashMap<String, Symbol>,
        module_path: &str,
    ) -> std::collections::HashMap<String, Symbol> {
        let module_name_for_mangling = Self::sanitize_module_path(module_path);
        let mut mangled_symbols = std::collections::HashMap::new();

        for (name, symbol) in symbols {
            let mut mangled_symbol = symbol.clone();
            if matches!(symbol.kind, SymbolKind::Function) {
                mangled_symbol.llvm_name = Some(format!("{}!{}", module_name_for_mangling, name));
            }
            mangled_symbols.insert(name.clone(), mangled_symbol);
        }

        mangled_symbols
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
        let symbol = module_symbols.get(item_name).ok_or_else(|| {
            let available: Vec<&String> = module_symbols.keys().collect();
            if available.is_empty() {
                SemanticError::new(
                    format!(
                        "Symbol '{}' not found in module '{}'",
                        item_name, module_path
                    ),
                    span,
                )
            } else {
                SemanticError::with_help(
                    format!(
                        "Symbol '{}' not found in module '{}'",
                        item_name, module_path
                    ),
                    span,
                    format!(
                        "Available symbols: {}",
                        available
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )
            }
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

        self.symbol_table
            .add_imported_symbol(local_name, imported_symbol)?;
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
                let _ = self.symbol_table.add_imported_symbol(name, imported_symbol);
            }
        }
        Ok(())
    }

    /// Register a single built-in function into the symbol table from its signature.
    fn register_builtin_function(&mut self, name: &str, sig: &BuiltInSig, span: Span) {
        let _ = self.symbol_table.add_symbol(
            name,
            Self::make_symbol(
                SymbolKind::Function,
                span,
                Some(Type::Function {
                    params: sig.params.clone(),
                    returns: Box::new(sig.return_type.clone()),
                    default_count: 0,
                }),
            ),
        );
    }

    /// Register all built-in functions whose names start with the given prefix.
    fn register_builtin_functions_with_prefix(&mut self, prefix: &str, span: Span) {
        let matching: Vec<_> = crate::semantics::stdlib::BUILT_IN_FUNCTIONS
            .iter()
            .filter(|(k, _)| k.starts_with(prefix))
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        for (func_name, sig) in matching {
            self.register_builtin_function(&func_name, &sig, span);
        }
    }

    fn import_std_item_with_resolver_check(
        &mut self,
        item: &str,
        alias: &Option<String>,
        span: Span,
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;
        let full_module_path = format!("std.{}", item);
        if let Some(resolver) = &self.module_resolver {
            let (has_file, has_directory) = resolver.borrow().check_module_path(&full_module_path);
            if has_file || has_directory {
                let files = files.ok_or_else(|| {
                    SemanticError::new("Files registry must be available for std imports", span)
                })?;
                return self.import_module_from_resolver(
                    &full_module_path,
                    &ImportSpec::Module {
                        alias: alias.clone(),
                    },
                    span,
                    files,
                );
            }
        }
        self.import_stdlib_module(
            item,
            &ImportSpec::Module {
                alias: alias.clone(),
            },
            span,
        )
    }

    fn handle_std_import_with_std_prefix(
        &mut self,
        spec: &crate::ast::ImportSpec,
        span: Span,
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;
        let mut files = files;
        match spec {
            ImportSpec::Item { item, alias } => {
                self.import_std_item_with_resolver_check(item, alias, span, files.as_deref_mut())
            }
            ImportSpec::Items { items } => {
                for (item, alias) in items {
                    self.import_std_item_with_resolver_check(
                        item,
                        alias,
                        span,
                        files.as_deref_mut(),
                    )?;
                }
                Ok(())
            }
            _ => self.handle_std_import_all(spec, span),
        }
    }

    fn handle_std_import(
        &mut self,
        module_path: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
        files: Option<&mut Files>,
    ) -> Result<(), SemanticError> {
        if module_path == "std" {
            return self.handle_std_import_with_std_prefix(spec, span, files);
        }

        if let Some(resolver) = &self.module_resolver {
            let (has_file, has_directory) = resolver.borrow().check_module_path(module_path);
            if has_file || has_directory {
                let files = files.expect("Files registry must be available for std imports");
                return self.import_module_from_resolver(module_path, spec, span, files);
            }
        }

        // First, try exact matches like "std.data" -> module_name "data"
        if let Some(module_name) = Self::stdlib_modules()
            .iter()
            .find(|(path, _)| *path == module_path)
            .map(|(_, name)| name)
        {
            return self.import_stdlib_module(module_name, spec, span);
        }

        // Support nested stdlib import paths like "std.data.json" by mapping
        // them to their parent stdlib entry (e.g. "std.data") and passing the
        // tail ("data.json") to import_stdlib_module which understands
        // nested child names.
        if let Some(tail) = module_path.strip_prefix("std.") {
            // tail is like "data.json" or "net.http" or just "data"
            if let Some(parent) = tail.split('.').next() {
                let parent_path = format!("std.{}", parent);
                if Self::stdlib_modules()
                    .iter()
                    .any(|(path, _)| *path == parent_path)
                {
                    // Pass the full tail (e.g. "data.json") as the module_name so
                    // collect_stdlib_module_symbols can lookup the child symbols.
                    return self.import_stdlib_module(tail, spec, span);
                }
            }
        }

        match module_path {
            "std" | "stdlib" => self.handle_std_import_all(spec, span),
            _ => self.handle_non_stdlib_import(module_path, spec, span),
        }
    }

    fn handle_std_import_all(
        &mut self,
        spec: &crate::ast::ImportSpec,
        span: Span,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;
        use crate::semantics::stdlib::{STDLIB_MODULES, all_stdlib_items};

        match spec {
            ImportSpec::Module { alias } => {
                let namespace = alias.as_deref().unwrap_or("std");
                let namespace_symbols: std::collections::HashMap<String, Symbol> = STDLIB_MODULES
                    .iter()
                    .map(|m| (m.to_string(), self.make_module_symbol(m, span)))
                    .collect();

                for module in STDLIB_MODULES {
                    let module_symbols = self.collect_stdlib_module_symbols(module, span);
                    self.imported_symbols
                        .insert(module.to_string(), module_symbols);
                    // Inject any nested stdlib children declared for this parent module
                    self.inject_nested_stdlib_children(module, span);
                }

                self.imported_symbols
                    .insert(namespace.to_string(), namespace_symbols);
                self.symbol_table
                    .add_symbol(namespace, self.make_module_symbol(namespace, span))?;
            }
            ImportSpec::Wildcard => {
                for (key, item) in all_stdlib_items() {
                    if let Some(item_name) = key.find('.').map(|i| &key[i + 1..]) {
                        crate::semantics::stdlib::register_stdlib_item_into(
                            &mut self.symbol_table,
                            item_name,
                            &item,
                            span,
                        )?;
                    }
                }
            }
            ImportSpec::Items { items } => {
                for (item, alias) in items {
                    self.import_stdlib_module(
                        item,
                        &ImportSpec::Module {
                            alias: alias.clone(),
                        },
                        span,
                    )?;
                }
            }
            ImportSpec::Item { item, alias } => {
                self.import_stdlib_module(
                    item,
                    &ImportSpec::Module {
                        alias: alias.clone(),
                    },
                    span,
                )?;
            }
        }
        Ok(())
    }

    fn handle_non_stdlib_import(
        &mut self,
        module_path: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;

        let module_name = module_path
            .split('.')
            .next_back()
            .expect("module path should have at least one component");

        match spec {
            ImportSpec::Module { alias } => {
                let symbol_name = alias.as_ref().map(|s| s.as_str()).unwrap_or(module_name);
                if let Some(sig) = self.get_builtin_sig(symbol_name).cloned() {
                    self.register_builtin_function(symbol_name, &sig, span);
                } else if symbol_name == "none" {
                    self.symbol_table.add_symbol(
                        symbol_name,
                        Self::make_symbol(
                            SymbolKind::Constant,
                            span,
                            Some(Type::Optional(Box::new(Type::Void))),
                        ),
                    )?;
                } else {
                    self.register_builtin_functions_with_prefix(&format!("{}_", symbol_name), span);
                }
            }
            ImportSpec::Item { item, alias } => {
                let symbol_name = alias.as_ref().unwrap_or(item);
                if let Some(sig) = self.get_builtin_sig(item).cloned() {
                    self.register_builtin_function(symbol_name, &sig, span);
                }
            }
            ImportSpec::Wildcard => {
                self.register_builtin_functions_with_prefix(&format!("{}_", module_name), span);
            }
            ImportSpec::Items { items } => {
                for (item, alias) in items {
                    let qualified_name = format!("{}_{}", module_name, item);
                    let symbol_name = alias.as_ref().unwrap_or(&qualified_name);
                    if let Some(sig) = self.get_builtin_sig(&qualified_name).cloned() {
                        self.register_builtin_function(symbol_name, &sig, span);
                    }
                }
            }
        }
        Ok(())
    }

    /// Import a stdlib module (e.g., "math", "random")
    fn import_stdlib_module(
        &mut self,
        module_name: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
    ) -> Result<(), SemanticError> {
        let module_symbols = self.collect_stdlib_module_symbols(module_name, span);
        // Inject any nested stdlib children declared for this parent
        self.inject_nested_stdlib_children(module_name, span);
        self.apply_stdlib_module_import_spec(module_name, spec, span, module_symbols)
    }

    fn collect_stdlib_module_symbols(
        &self,
        module_name: &str,
        span: Span,
    ) -> std::collections::HashMap<String, Symbol> {
        use crate::semantics::stdlib::all_stdlib_items;

        let mut module_symbols = std::collections::HashMap::new();
        let prefixes = Self::stdlib_module_prefixes(module_name);

        for (key, item) in all_stdlib_items() {
            if let Some(item_name) = Self::stdlib_item_name_for_module(&key, &prefixes) {
                module_symbols.insert(
                    item_name,
                    crate::semantics::stdlib::stdlib_item_to_symbol(&item, span),
                );
            }
        }

        Self::inject_stdlib_module_special_symbols(module_name, span, &mut module_symbols);
        module_symbols
    }

    fn stdlib_module_prefixes(module_name: &str) -> Vec<String> {
        if !module_name.contains('.') {
            return vec![module_name.to_string()];
        }
        let mut prefixes = vec![module_name.to_string()];
        if let Some(last) = module_name.split('.').next_back() {
            prefixes.push(last.to_string());
        }
        prefixes
    }

    fn stdlib_item_name_for_module(key: &str, prefixes: &[String]) -> Option<String> {
        for prefix in prefixes {
            let pattern = format!("{}.", prefix);
            if let Some(rest) = key.strip_prefix(&pattern)
                && !rest.contains('.')
            {
                return Some(rest.to_string());
            }
        }
        None
    }

    fn inject_stdlib_module_special_symbols(
        module_name: &str,
        span: Span,
        module_symbols: &mut std::collections::HashMap<String, Symbol>,
    ) {
        match module_name {
            "net" => {
                module_symbols.extend(crate::semantics::stdlib::net_module_class_symbols(span))
            }
            "sync" => {
                module_symbols.extend(crate::semantics::stdlib::sync_module_class_symbols(span))
            }
            "sql" => {
                module_symbols.extend(crate::semantics::stdlib::sql_module_class_symbols(span))
            }
            _ if module_name.ends_with(".json") => {
                module_symbols.insert("Json".to_string(), Self::make_json_symbol(span));
            }
            _ if module_name.ends_with(".csv") => {
                module_symbols.insert("Csv".to_string(), Self::make_csv_symbol(span));
            }
            _ => {}
        }
    }

    fn csv_headers_type() -> Type {
        Type::List(Box::new(Type::Primitive(PrimitiveType::Str)))
    }

    fn csv_rows_type() -> Type {
        Type::List(Box::new(Self::csv_headers_type()))
    }

    fn csv_fields() -> std::collections::HashMap<String, (Type, bool)> {
        let mut fields = std::collections::HashMap::new();
        fields.insert("headers".to_string(), (Self::csv_headers_type(), true));
        fields.insert("rows".to_string(), (Self::csv_rows_type(), true));
        fields
    }

    fn make_csv_symbol(span: Span) -> Symbol {
        let mut methods = std::collections::HashMap::new();
        methods.insert(
            "stringify".to_string(),
            MethodSig {
                params: vec![],
                return_type: Type::Result(
                    Box::new(Type::Primitive(PrimitiveType::Str)),
                    Box::new(Type::Primitive(PrimitiveType::Str)),
                ),
                is_static: false,
            },
        );
        Symbol {
            kind: SymbolKind::Class,
            span,
            type_: Some(Type::Named("Csv".to_string(), Vec::new())),
            interfaces: std::collections::HashMap::new(),
            methods,
            fields: Self::csv_fields(),
            type_params: Vec::new(),
            original_name: None,
            llvm_name: None,
            default_param_count: 0,
            variants: None,
        }
    }

    fn make_json_symbol(span: Span) -> Symbol {
        let mut methods = std::collections::HashMap::new();
        methods.insert(
            "stringify".to_string(),
            MethodSig {
                params: vec![Type::Optional(Box::new(Type::Primitive(
                    PrimitiveType::Int,
                )))],
                return_type: Type::Result(
                    Box::new(Type::Primitive(PrimitiveType::Str)),
                    Box::new(Type::Primitive(PrimitiveType::Str)),
                ),
                is_static: false,
            },
        );
        Symbol {
            kind: SymbolKind::Class,
            span,
            type_: Some(Type::Named("Json".to_string(), Vec::new())),
            interfaces: std::collections::HashMap::new(),
            methods,
            fields: std::collections::HashMap::new(),
            type_params: Vec::new(),
            original_name: None,
            llvm_name: None,
            default_param_count: 0,
            variants: None,
        }
    }

    fn apply_stdlib_module_import_spec(
        &mut self,
        module_name: &str,
        spec: &crate::ast::ImportSpec,
        span: Span,
        module_symbols: std::collections::HashMap<String, Symbol>,
    ) -> Result<(), SemanticError> {
        use crate::ast::ImportSpec;

        match spec {
            ImportSpec::Module { alias } => {
                let namespace = alias.as_deref().map(|s| s.to_string()).unwrap_or_else(|| {
                    // If importing a nested module like "data.json" prefer the
                    // short child name "json" as the namespace so callers can
                    // reference `json.parse` without an alias.
                    if module_name.contains('.') {
                        module_name.split('.').next_back().unwrap().to_string()
                    } else {
                        module_name.to_string()
                    }
                });

                self.imported_symbols
                    .insert(namespace.to_string(), module_symbols.clone());
                self.symbol_table.add_symbol(
                    &namespace,
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
            }
            ImportSpec::Item { item, alias } => {
                let symbol_name = alias.as_ref().unwrap_or(item);
                if let Some(symbol) = module_symbols.get(item) {
                    self.symbol_table.add_symbol(symbol_name, symbol.clone())?;
                }
            }
            ImportSpec::Wildcard => {
                for (name, symbol) in module_symbols {
                    self.symbol_table.add_symbol(&name, symbol)?;
                }
            }
            ImportSpec::Items { items } => {
                for (item, alias) in items {
                    let symbol_name = alias.as_ref().unwrap_or(item);
                    if let Some(symbol) = module_symbols.get(item) {
                        self.symbol_table.add_symbol(symbol_name, symbol.clone())?;
                    }
                }
            }
        }
        Ok(())
    }

    // stdlib item conversion/registration delegated to `crate::semantics::stdlib`.

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
                self.handle_variable_declaration(name, expr, local_vars, free_vars)?;
            }
            StatementKind::TypedDecl(name, _, expr) => {
                self.handle_variable_declaration(name, expr, local_vars, free_vars)?;
            }
            StatementKind::ConstDecl(name, _, expr) => {
                self.handle_variable_declaration(name, expr, local_vars, free_vars)?;
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.handle_if_statement(cond, then_block, else_block, local_vars, free_vars)?;
            }
            StatementKind::While { cond, body } => {
                self.handle_while_statement(cond, body, local_vars, free_vars)?;
            }
            StatementKind::For {
                var, iter, body, ..
            } => {
                self.handle_for_statement(var, iter, body, local_vars, free_vars)?;
            }
            StatementKind::Block(stmts) => {
                self.handle_block_statement(stmts, local_vars, free_vars)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_variable_declaration(
        &self,
        name: &str,
        expr: &ExpressionNode,
        local_vars: &mut std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        // First analyze the expression (uses happen before the decl is in scope)
        self.find_free_variables_in_expression(expr, local_vars, free_vars)?;
        // Then add the variable to local scope
        local_vars.insert(name.to_string());
        Ok(())
    }

    fn handle_if_statement(
        &self,
        cond: &ExpressionNode,
        then_block: &[StatementNode],
        else_block: &Option<Vec<StatementNode>>,
        local_vars: &mut std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(cond, local_vars, free_vars)?;
        for s in then_block {
            self.find_free_variables_in_statement(s, local_vars, free_vars)?;
        }
        if let Some(else_stmts) = else_block {
            for s in else_stmts {
                self.find_free_variables_in_statement(s, local_vars, free_vars)?;
            }
        }
        Ok(())
    }

    fn handle_while_statement(
        &self,
        cond: &ExpressionNode,
        body: &[StatementNode],
        local_vars: &mut std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(cond, local_vars, free_vars)?;
        for s in body {
            self.find_free_variables_in_statement(s, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_for_statement(
        &self,
        var: &str,
        iter: &ExpressionNode,
        body: &[StatementNode],
        local_vars: &mut std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(iter, local_vars, free_vars)?;
        // Iterator variable is local to the for loop
        local_vars.insert(var.to_string());
        for s in body {
            self.find_free_variables_in_statement(s, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_block_statement(
        &self,
        stmts: &[StatementNode],
        local_vars: &mut std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        for s in stmts {
            self.find_free_variables_in_statement(s, local_vars, free_vars)?;
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
                self.handle_identifier_expression(name, local_vars, free_vars)?;
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.handle_binary_expression(left, right, local_vars, free_vars)?;
            }
            ExpressionKind::Unary {
                expr: inner,
                op_span: _,
                ..
            } => {
                self.handle_unary_expression(inner, local_vars, free_vars)?;
            }
            ExpressionKind::Call { func, args } => {
                self.handle_call_expression(func, args, local_vars, free_vars)?;
            }
            ExpressionKind::FieldAccess { expr: inner, .. } => {
                self.handle_field_access_expression(inner, local_vars, free_vars)?;
            }
            ExpressionKind::ListAccess { expr: inner, index } => {
                self.handle_list_access_expression(inner, index, local_vars, free_vars)?;
            }
            ExpressionKind::ListLiteral(elements) => {
                self.handle_list_literal_expression(elements, local_vars, free_vars)?;
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                self.handle_map_literal_expression(entries, local_vars, free_vars)?;
            }
            ExpressionKind::SetLiteral(elements) => {
                self.handle_set_literal_expression(elements, local_vars, free_vars)?;
            }
            ExpressionKind::TupleLiteral(elements) => {
                self.handle_tuple_literal_expression(elements, local_vars, free_vars)?;
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                self.handle_if_expression(cond, then_expr, else_expr, local_vars, free_vars)?;
            }
            ExpressionKind::Lambda { params, body, .. } => {
                self.handle_lambda_expression(params, body, local_vars, free_vars)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_identifier_expression(
        &self,
        name: &str,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        // Check if it's a local variable (parameter or declared in lambda body)
        if !local_vars.contains(name) {
            // Check if it exists in outer scopes
            if let Some(symbol) = self.symbol_table.lookup(name)
                && matches!(symbol.kind, SymbolKind::Variable)
                && let Some(var_type) = &symbol.type_
            {
                free_vars.insert(name.to_string(), var_type.clone());
            }
        }
        Ok(())
    }

    fn handle_binary_expression(
        &self,
        left: &ExpressionNode,
        right: &ExpressionNode,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(left, local_vars, free_vars)?;
        self.find_free_variables_in_expression(right, local_vars, free_vars)?;
        Ok(())
    }

    fn handle_unary_expression(
        &self,
        inner: &ExpressionNode,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(inner, local_vars, free_vars)?;
        Ok(())
    }

    fn handle_call_expression(
        &self,
        func: &ExpressionNode,
        args: &[ExpressionNode],
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(func, local_vars, free_vars)?;
        for arg in args {
            self.find_free_variables_in_expression(arg, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_field_access_expression(
        &self,
        inner: &ExpressionNode,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(inner, local_vars, free_vars)?;
        Ok(())
    }

    fn handle_list_access_expression(
        &self,
        inner: &ExpressionNode,
        index: &ExpressionNode,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(inner, local_vars, free_vars)?;
        self.find_free_variables_in_expression(index, local_vars, free_vars)?;
        Ok(())
    }

    fn handle_list_literal_expression(
        &self,
        elements: &[ExpressionNode],
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        for elem in elements {
            self.find_free_variables_in_expression(elem, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_map_literal_expression(
        &self,
        entries: &[(ExpressionNode, ExpressionNode)],
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        for (key, val) in entries {
            self.find_free_variables_in_expression(key, local_vars, free_vars)?;
            self.find_free_variables_in_expression(val, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_set_literal_expression(
        &self,
        elements: &[ExpressionNode],
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        for elem in elements {
            self.find_free_variables_in_expression(elem, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_tuple_literal_expression(
        &self,
        elements: &[ExpressionNode],
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        for elem in elements {
            self.find_free_variables_in_expression(elem, local_vars, free_vars)?;
        }
        Ok(())
    }

    fn handle_if_expression(
        &self,
        cond: &ExpressionNode,
        then_expr: &ExpressionNode,
        else_expr: &ExpressionNode,
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        self.find_free_variables_in_expression(cond, local_vars, free_vars)?;
        self.find_free_variables_in_expression(then_expr, local_vars, free_vars)?;
        self.find_free_variables_in_expression(else_expr, local_vars, free_vars)?;
        Ok(())
    }

    fn handle_lambda_expression(
        &self,
        params: &[Param],
        body: &[StatementNode],
        local_vars: &std::collections::HashSet<String>,
        free_vars: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), SemanticError> {
        let mut inner_local_vars = local_vars.clone();
        for param in params {
            inner_local_vars.insert(param.name.clone());
        }
        for stmt in body {
            self.find_free_variables_in_statement(stmt, &mut inner_local_vars, free_vars)?;
        }
        Ok(())
    }

    fn check_not_modifying_constant(
        &mut self,
        expr: &ExpressionNode,
        op_span: &Span,
    ) -> Result<(), SemanticError> {
        if let crate::ast::ExpressionKind::Identifier(name) = &expr.kind {
            if let Some(symbol) = self.symbol_table.lookup(name)
                && symbol.kind == SymbolKind::Constant
            {
                return Err(SemanticError::with_help(
                    format!("Cannot modify constant '{}'", name),
                    *op_span,
                    "Constants cannot be modified after initialization",
                ));
            }
        } else if let crate::ast::ExpressionKind::FieldAccess {
            expr: obj_expr,
            field,
        } = &expr.kind
        {
            let obj_type = self.get_expression_type(obj_expr)?;
            if let Type::Named(class_name, _) = &obj_type
                && let Some(symbol) = self.symbol_table.lookup(class_name)
                && let Some((_, is_const)) = symbol.fields.get(field)
                && *is_const
            {
                return Err(SemanticError::with_help(
                    format!("Cannot modify const field '{}'", field),
                    *op_span,
                    "Const fields cannot be modified after initialization. Remove the 'const' modifier from the field declaration if mutation is needed.",
                ));
            }
        }
        Ok(())
    }
}

/// Infer missing type parameters from already-inferred parameters' trait bounds.
///
/// This implements the three-level nested loop that scans each missing type parameter's
/// bounds to see if one of the already-inferred type parameters' bounds contains a
/// reference to the missing parameter at a specific position, allowing inference of the
/// concrete type argument at that position.
pub(crate) fn infer_missing_type_params_from_bounds(
    type_params: &[(String, Vec<crate::ast::TraitBound>)],
    substitutions: &mut std::collections::HashMap<String, Type>,
) {
    for (missing_param_name, _) in type_params {
        if substitutions.contains_key(missing_param_name) {
            continue;
        }

        if let Some(inferred_type) =
            infer_missing_param_from_bounds(missing_param_name, type_params, substitutions)
        {
            substitutions.insert(missing_param_name.clone(), inferred_type);
        }
    }
}

fn infer_missing_param_from_bounds(
    missing_param_name: &str,
    type_params: &[(String, Vec<crate::ast::TraitBound>)],
    substitutions: &std::collections::HashMap<String, Type>,
) -> Option<Type> {
    type_params
        .iter()
        .find_map(|(owner_param_name, owner_bounds)| {
            substitutions
                .get(owner_param_name)
                .and_then(owner_concrete_type_args)
                .filter(|owner_type_args| !owner_type_args.is_empty())
                .and_then(|owner_type_args| {
                    infer_bound_type_arg(missing_param_name, owner_bounds, owner_type_args)
                })
        })
}

fn owner_concrete_type_args(owner_concrete_type: &Type) -> Option<&[Type]> {
    match owner_concrete_type {
        Type::Named(_, args) => Some(args.as_slice()),
        Type::Reference(inner) => match inner.as_ref() {
            Type::Named(_, args) => Some(args.as_slice()),
            _ => None,
        },
        _ => None,
    }
}

fn infer_bound_type_arg(
    missing_param_name: &str,
    owner_bounds: &[crate::ast::TraitBound],
    owner_type_args: &[Type],
) -> Option<Type> {
    for bound in owner_bounds {
        for (idx, bound_type_arg) in bound.type_params.iter().enumerate() {
            if let TypeKind::Named(bound_name, _) = &bound_type_arg.kind
                && bound_name == missing_param_name
                && let Some(concrete_arg) = owner_type_args.get(idx)
            {
                return Some(concrete_arg.clone());
            }
        }
    }
    None
}

/// Compute the Levenshtein edit distance between two strings.
// Use the existing edit_distance from symbol_table instead of duplicating
use crate::semantics::symbol_table::{
    calculate_similarity_threshold, edit_distance as levenshtein_distance,
};
