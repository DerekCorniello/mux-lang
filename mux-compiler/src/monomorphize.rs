//! Monomorphization pass for generics.
//!
//! This module transforms generic functions and classes into specialized
//! (monomorphic) versions for each concrete type instantiation discovered
//! in the program.
//!
//! # Overview
//!
//! Monomorphization is the process of generating specialized code for generic
//! definitions based on their actual usage. For example:
//!
//! ```text
//! func max<T>(T a, T b) returns T { if a > b { return a } return b }
//! auto x = max(1, 2)      // uses max with T=int
//! auto y = max(1.0, 2.0)  // uses max with T=float
//! ```
//!
//! After monomorphization, we get:
//!
//! ```text
//! func max_int(int a, int b) returns int { if a > b { return a } return b }
//! func max_float(float a, float b) returns float { if a > b { return a } return b }
//! auto x = max_int(1, 2)
//! auto y = max_float(1.0, 2.0)
//! ```

use crate::lexer::Span;
use crate::parser::*;
use crate::semantics::{SemanticAnalyzer, Type};
use std::collections::{HashMap, HashSet};

/// Represents a concrete instantiation of a generic item.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instantiation {
    /// The original generic name (e.g., "Stack", "max")
    pub name: String,
    /// The concrete type arguments (e.g., [Type::Primitive(Int)])
    pub type_args: Vec<Type>,
}

impl Instantiation {
    pub fn new(name: String, type_args: Vec<Type>) -> Self {
        Self { name, type_args }
    }

    /// Generate a mangled name for this instantiation.
    /// e.g., Stack<int> -> "Stack_int", max<float> -> "max_float"
    pub fn mangled_name(&self) -> String {
        if self.type_args.is_empty() {
            self.name.clone()
        } else {
            let type_suffix: Vec<String> = self.type_args.iter().map(mangle_type).collect();
            format!("{}_{}", self.name, type_suffix.join("_"))
        }
    }
}

/// Generate a mangled name component for a type.
fn mangle_type(t: &Type) -> String {
    match t {
        Type::Primitive(p) => match p {
            PrimitiveType::Int => "int".to_string(),
            PrimitiveType::Float => "float".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
            PrimitiveType::Char => "char".to_string(),
            PrimitiveType::Str => "str".to_string(),
            PrimitiveType::Void => "void".to_string(),
            PrimitiveType::Auto => "auto".to_string(),
        },
        Type::Named(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let arg_parts: Vec<String> = args.iter().map(mangle_type).collect();
                format!("{}__{}", name, arg_parts.join("_"))
            }
        }
        Type::List(inner) => format!("list__{}", mangle_type(inner)),
        Type::Map(k, v) => format!("map__{}_{}", mangle_type(k), mangle_type(v)),
        Type::Set(inner) => format!("set__{}", mangle_type(inner)),
        Type::Optional(inner) => format!("opt__{}", mangle_type(inner)),
        Type::Tuple(types) => {
            let parts: Vec<String> = types.iter().map(mangle_type).collect();
            format!("tuple__{}", parts.join("_"))
        }
        Type::Reference(inner) => format!("ref__{}", mangle_type(inner)),
        Type::Function { params, returns } => {
            let param_parts: Vec<String> = params.iter().map(mangle_type).collect();
            format!(
                "fn__{}__{}",
                param_parts.join("_"),
                mangle_type(returns)
            )
        }
        Type::Variable(name) => format!("var__{}", name),
        Type::Void => "void".to_string(),
        Type::EmptyList => "emptylist".to_string(),
        Type::EmptyMap => "emptymap".to_string(),
        Type::EmptySet => "emptyset".to_string(),
    }
}

/// Context for the monomorphization pass.
pub struct Monomorphizer {
    /// Collected function instantiations (generic_name -> set of type args)
    function_instantiations: HashMap<String, HashSet<Vec<Type>>>,
    /// Collected class instantiations
    class_instantiations: HashMap<String, HashSet<Vec<Type>>>,
    /// Original generic function definitions
    generic_functions: HashMap<String, FunctionNode>,
    /// Original generic class definitions (name -> (type_params, fields, methods))
    generic_classes: HashMap<String, GenericClassDef>,
    /// Mapping from generic name + type args to mangled name
    name_mapping: HashMap<Instantiation, String>,
    /// Type parameters currently in scope for substitution
    current_type_params: Vec<(String, Type)>,
}

/// Stored generic class definition for later instantiation.
#[derive(Debug, Clone)]
struct GenericClassDef {
    type_params: Vec<(String, Vec<TraitBound>)>,
    traits: Vec<TraitRef>,
    fields: Vec<Field>,
    methods: Vec<FunctionNode>,
    span: Span,
}

impl Default for Monomorphizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Monomorphizer {
    pub fn new() -> Self {
        Self {
            function_instantiations: HashMap::new(),
            class_instantiations: HashMap::new(),
            generic_functions: HashMap::new(),
            generic_classes: HashMap::new(),
            name_mapping: HashMap::new(),
            current_type_params: Vec::new(),
        }
    }

    /// Main entry point: monomorphize an entire program AST.
    ///
    /// This performs the following steps:
    /// 1. Collect all generic definitions (functions and classes)
    /// 2. Walk the AST to discover all instantiations
    /// 3. Generate specialized versions for each instantiation
    /// 4. Rewrite the AST to use the specialized versions
    pub fn monomorphize(&mut self, ast: Vec<AstNode>) -> Vec<AstNode> {
        // Phase 1: Collect generic definitions
        self.collect_generic_definitions(&ast);

        // Phase 2: Discover all instantiations by walking the AST
        self.discover_instantiations(&ast);

        // Phase 3: Generate mangled names for all instantiations
        self.generate_name_mappings();

        // Phase 4: Generate specialized definitions and rewrite AST
        self.transform_ast(ast)
    }

    /// Phase 1: Collect all generic function and class definitions.
    fn collect_generic_definitions(&mut self, ast: &[AstNode]) {
        for node in ast {
            match node {
                AstNode::Function(func) if !func.type_params.is_empty() => {
                    self.generic_functions
                        .insert(func.name.clone(), func.clone());
                }
                AstNode::Class {
                    name,
                    type_params,
                    traits,
                    fields,
                    methods,
                    span,
                } if !type_params.is_empty() => {
                    self.generic_classes.insert(
                        name.clone(),
                        GenericClassDef {
                            type_params: type_params.clone(),
                            traits: traits.clone(),
                            fields: fields.clone(),
                            methods: methods.clone(),
                            span: *span,
                        },
                    );
                }
                _ => {}
            }
        }
    }

    /// Phase 2: Discover all instantiations in the AST.
    fn discover_instantiations(&mut self, ast: &[AstNode]) {
        for node in ast {
            self.visit_node(node);
        }
    }

    fn visit_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Function(func) => {
                // Visit function body, but skip the generic function itself
                if func.type_params.is_empty() {
                    for stmt in &func.body {
                        self.visit_statement(stmt);
                    }
                }
            }
            AstNode::Class {
                methods,
                type_params,
                ..
            } => {
                // Only visit non-generic class methods
                if type_params.is_empty() {
                    for method in methods {
                        for stmt in &method.body {
                            self.visit_statement(stmt);
                        }
                    }
                }
            }
            AstNode::Statement(stmt) => {
                self.visit_statement(stmt);
            }
            _ => {}
        }
    }

    fn visit_statement(&mut self, stmt: &StatementNode) {
        match &stmt.kind {
            StatementKind::AutoDecl(_, _, expr) => self.visit_expression(expr),
            StatementKind::TypedDecl(_, type_node, expr) => {
                self.visit_type_node(type_node);
                self.visit_expression(expr);
            }
            StatementKind::ConstDecl(_, type_node, expr) => {
                self.visit_type_node(type_node);
                self.visit_expression(expr);
            }
            StatementKind::Expression(expr) => self.visit_expression(expr),
            StatementKind::Block(stmts) => {
                for s in stmts {
                    self.visit_statement(s);
                }
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.visit_expression(cond);
                for s in then_block {
                    self.visit_statement(s);
                }
                if let Some(else_stmts) = else_block {
                    for s in else_stmts {
                        self.visit_statement(s);
                    }
                }
            }
            StatementKind::While { cond, body } => {
                self.visit_expression(cond);
                for s in body {
                    self.visit_statement(s);
                }
            }
            StatementKind::For {
                var_type, iter, body, ..
            } => {
                self.visit_type_node(var_type);
                self.visit_expression(iter);
                for s in body {
                    self.visit_statement(s);
                }
            }
            StatementKind::Match { expr, arms } => {
                self.visit_expression(expr);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                    for s in &arm.body {
                        self.visit_statement(s);
                    }
                }
            }
            StatementKind::Return(Some(expr)) => self.visit_expression(expr),
            StatementKind::Function(func) => {
                if func.type_params.is_empty() {
                    for s in &func.body {
                        self.visit_statement(s);
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_expression(&mut self, expr: &ExpressionNode) {
        match &expr.kind {
            ExpressionKind::GenericType(name, type_args) => {
                // This is a generic instantiation!
                let resolved_types = self.resolve_type_args(type_args);
                self.record_instantiation(name, resolved_types);

                // Also visit the type arguments themselves
                for arg in type_args {
                    self.visit_type_node(arg);
                }
            }
            ExpressionKind::Call { func, args } => {
                self.visit_expression(func);
                for arg in args {
                    self.visit_expression(arg);
                }
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            ExpressionKind::Unary { expr, .. } => {
                self.visit_expression(expr);
            }
            ExpressionKind::FieldAccess { expr, .. } => {
                self.visit_expression(expr);
            }
            ExpressionKind::ListAccess { expr, index } => {
                self.visit_expression(expr);
                self.visit_expression(index);
            }
            ExpressionKind::ListLiteral(elements) => {
                for elem in elements {
                    self.visit_expression(elem);
                }
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                for (k, v) in entries {
                    self.visit_expression(k);
                    self.visit_expression(v);
                }
            }
            ExpressionKind::SetLiteral(elements) => {
                for elem in elements {
                    self.visit_expression(elem);
                }
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                self.visit_expression(cond);
                self.visit_expression(then_expr);
                self.visit_expression(else_expr);
            }
            ExpressionKind::Lambda { body, .. } => {
                for s in body {
                    self.visit_statement(s);
                }
            }
            _ => {}
        }
    }

    fn visit_type_node(&mut self, type_node: &TypeNode) {
        match &type_node.kind {
            TypeKind::Named(name, type_args) if !type_args.is_empty() => {
                // Check if this is a generic class instantiation
                if self.generic_classes.contains_key(name) {
                    let resolved_types = self.resolve_type_args(type_args);
                    self.record_class_instantiation(name, resolved_types);
                }
                // Recursively visit type arguments
                for arg in type_args {
                    self.visit_type_node(arg);
                }
            }
            TypeKind::List(inner) => self.visit_type_node(inner),
            TypeKind::Map(k, v) => {
                self.visit_type_node(k);
                self.visit_type_node(v);
            }
            TypeKind::Set(inner) => self.visit_type_node(inner),
            TypeKind::Reference(inner) => self.visit_type_node(inner),
            TypeKind::Function { params, returns } => {
                for p in params {
                    self.visit_type_node(p);
                }
                self.visit_type_node(returns);
            }
            TypeKind::Tuple(elems) => {
                for e in elems {
                    self.visit_type_node(e);
                }
            }
            _ => {}
        }
    }

    fn resolve_type_args(&self, type_args: &[TypeNode]) -> Vec<Type> {
        type_args.iter().map(|t| self.type_node_to_type(t)).collect()
    }

    fn type_node_to_type(&self, type_node: &TypeNode) -> Type {
        match &type_node.kind {
            TypeKind::Primitive(p) => Type::Primitive(*p),
            TypeKind::Named(name, args) => {
                // Check if this is a type parameter in scope
                for (param_name, replacement) in &self.current_type_params {
                    if param_name == name && args.is_empty() {
                        return replacement.clone();
                    }
                }
                let resolved_args: Vec<Type> = args.iter().map(|a| self.type_node_to_type(a)).collect();
                Type::Named(name.clone(), resolved_args)
            }
            TypeKind::List(inner) => Type::List(Box::new(self.type_node_to_type(inner))),
            TypeKind::Map(k, v) => Type::Map(
                Box::new(self.type_node_to_type(k)),
                Box::new(self.type_node_to_type(v)),
            ),
            TypeKind::Set(inner) => Type::Set(Box::new(self.type_node_to_type(inner))),
            TypeKind::Reference(inner) => Type::Reference(Box::new(self.type_node_to_type(inner))),
            TypeKind::Function { params, returns } => Type::Function {
                params: params.iter().map(|p| self.type_node_to_type(p)).collect(),
                returns: Box::new(self.type_node_to_type(returns)),
            },
            TypeKind::Tuple(elems) => {
                Type::Tuple(elems.iter().map(|e| self.type_node_to_type(e)).collect())
            }
            TypeKind::Auto => Type::Void, // Should not happen in well-formed code
            TypeKind::TraitObject(_) => Type::Void, // Not supported yet
        }
    }

    fn record_instantiation(&mut self, name: &str, type_args: Vec<Type>) {
        if self.generic_functions.contains_key(name) {
            self.function_instantiations
                .entry(name.to_string())
                .or_default()
                .insert(type_args.clone());
        }
        if self.generic_classes.contains_key(name) {
            self.class_instantiations
                .entry(name.to_string())
                .or_default()
                .insert(type_args);
        }
    }

    fn record_class_instantiation(&mut self, name: &str, type_args: Vec<Type>) {
        if self.generic_classes.contains_key(name) {
            self.class_instantiations
                .entry(name.to_string())
                .or_default()
                .insert(type_args);
        }
    }

    /// Phase 3: Generate mangled names for all collected instantiations.
    fn generate_name_mappings(&mut self) {
        // Functions
        for (name, instantiations) in &self.function_instantiations {
            for type_args in instantiations {
                let inst = Instantiation::new(name.clone(), type_args.clone());
                let mangled = inst.mangled_name();
                self.name_mapping.insert(inst, mangled);
            }
        }
        // Classes
        for (name, instantiations) in &self.class_instantiations {
            for type_args in instantiations {
                let inst = Instantiation::new(name.clone(), type_args.clone());
                let mangled = inst.mangled_name();
                self.name_mapping.insert(inst, mangled);
            }
        }
    }

    /// Phase 4: Transform the AST.
    fn transform_ast(&mut self, ast: Vec<AstNode>) -> Vec<AstNode> {
        let mut result = Vec::new();

        // First, generate all specialized function versions
        let func_instantiations = std::mem::take(&mut self.function_instantiations);
        for (name, instantiations) in &func_instantiations {
            if let Some(generic_func) = self.generic_functions.get(name) {
                for type_args in instantiations {
                    let specialized = self.specialize_function(generic_func, type_args);
                    result.push(AstNode::Function(specialized));
                }
            }
        }
        self.function_instantiations = func_instantiations;

        // Generate all specialized class versions
        let class_instantiations = std::mem::take(&mut self.class_instantiations);
        for (name, instantiations) in &class_instantiations {
            if let Some(generic_class) = self.generic_classes.get(name).cloned() {
                for type_args in instantiations {
                    let specialized = self.specialize_class(&name.clone(), &generic_class, type_args);
                    result.push(specialized);
                }
            }
        }
        self.class_instantiations = class_instantiations;

        // Now transform the rest of the AST
        for node in ast {
            match node {
                // Skip generic definitions - they've been specialized
                AstNode::Function(ref func) if !func.type_params.is_empty() => {
                    // Don't include generic functions in output
                    continue;
                }
                AstNode::Class {
                    ref type_params, ..
                } if !type_params.is_empty() => {
                    // Don't include generic classes in output
                    continue;
                }
                // Transform non-generic nodes
                _ => {
                    result.push(self.transform_node(node));
                }
            }
        }

        result
    }

    /// Specialize a generic function for specific type arguments.
    fn specialize_function(&mut self, func: &FunctionNode, type_args: &[Type]) -> FunctionNode {
        // Set up type parameter substitutions
        self.current_type_params.clear();
        for ((param_name, _), type_arg) in func.type_params.iter().zip(type_args) {
            self.current_type_params
                .push((param_name.clone(), type_arg.clone()));
        }

        // Generate mangled name
        let inst = Instantiation::new(func.name.clone(), type_args.to_vec());
        let mangled_name = self.name_mapping.get(&inst).cloned().unwrap_or_else(|| inst.mangled_name());

        // Transform parameters
        let params: Vec<Param> = func
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                type_: self.substitute_type_node(&p.type_),
            })
            .collect();

        // Transform return type
        let return_type = self.substitute_type_node(&func.return_type);

        // Transform body
        let body: Vec<StatementNode> = func
            .body
            .iter()
            .map(|s| self.transform_statement(s))
            .collect();

        self.current_type_params.clear();

        FunctionNode {
            name: mangled_name,
            type_params: Vec::new(), // No longer generic
            params,
            return_type,
            body,
            span: func.span,
        }
    }

    /// Specialize a generic class for specific type arguments.
    fn specialize_class(
        &mut self,
        name: &str,
        class_def: &GenericClassDef,
        type_args: &[Type],
    ) -> AstNode {
        // Set up type parameter substitutions
        self.current_type_params.clear();
        for ((param_name, _), type_arg) in class_def.type_params.iter().zip(type_args) {
            self.current_type_params
                .push((param_name.clone(), type_arg.clone()));
        }

        // Generate mangled name
        let inst = Instantiation::new(name.to_string(), type_args.to_vec());
        let mangled_name = self.name_mapping.get(&inst).cloned().unwrap_or_else(|| inst.mangled_name());

        // Transform fields
        let fields: Vec<Field> = class_def
            .fields
            .iter()
            .map(|f| Field {
                name: f.name.clone(),
                type_: self.substitute_type_node(&f.type_),
            })
            .collect();

        // Transform methods
        let methods: Vec<FunctionNode> = class_def
            .methods
            .iter()
            .map(|m| {
                let params: Vec<Param> = m
                    .params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        type_: self.substitute_type_node(&p.type_),
                    })
                    .collect();

                let return_type = self.substitute_type_node(&m.return_type);

                let body: Vec<StatementNode> = m
                    .body
                    .iter()
                    .map(|s| self.transform_statement(s))
                    .collect();

                FunctionNode {
                    name: m.name.clone(),
                    type_params: Vec::new(),
                    params,
                    return_type,
                    body,
                    span: m.span,
                }
            })
            .collect();

        // Transform trait refs (substitute type args in trait references)
        let traits: Vec<TraitRef> = class_def
            .traits
            .iter()
            .map(|t| TraitRef {
                name: t.name.clone(),
                type_args: t.type_args.iter().map(|a| self.substitute_type_node(a)).collect(),
            })
            .collect();

        self.current_type_params.clear();

        AstNode::Class {
            name: mangled_name,
            type_params: Vec::new(), // No longer generic
            traits,
            fields,
            methods,
            span: class_def.span,
        }
    }

    /// Substitute type parameters in a TypeNode.
    fn substitute_type_node(&self, type_node: &TypeNode) -> TypeNode {
        let new_kind = match &type_node.kind {
            TypeKind::Named(name, args) if args.is_empty() => {
                // Check if this is a type parameter
                for (param_name, replacement) in &self.current_type_params {
                    if param_name == name {
                        return self.type_to_type_node(replacement, type_node.span);
                    }
                }
                TypeKind::Named(name.clone(), Vec::new())
            }
            TypeKind::Named(name, args) => {
                // Check if this is a generic class instantiation that needs renaming
                let substituted_args: Vec<TypeNode> =
                    args.iter().map(|a| self.substitute_type_node(a)).collect();

                // Check if we have a specialized version
                let resolved_types: Vec<Type> = substituted_args
                    .iter()
                    .map(|a| self.type_node_to_type(a))
                    .collect();
                let inst = Instantiation::new(name.clone(), resolved_types);
                if let Some(mangled) = self.name_mapping.get(&inst) {
                    // Use the mangled name with no type args
                    TypeKind::Named(mangled.clone(), Vec::new())
                } else {
                    TypeKind::Named(name.clone(), substituted_args)
                }
            }
            TypeKind::List(inner) => {
                TypeKind::List(Box::new(self.substitute_type_node(inner)))
            }
            TypeKind::Map(k, v) => TypeKind::Map(
                Box::new(self.substitute_type_node(k)),
                Box::new(self.substitute_type_node(v)),
            ),
            TypeKind::Set(inner) => {
                TypeKind::Set(Box::new(self.substitute_type_node(inner)))
            }
            TypeKind::Reference(inner) => {
                TypeKind::Reference(Box::new(self.substitute_type_node(inner)))
            }
            TypeKind::Function { params, returns } => TypeKind::Function {
                params: params.iter().map(|p| self.substitute_type_node(p)).collect(),
                returns: Box::new(self.substitute_type_node(returns)),
            },
            TypeKind::Tuple(elems) => {
                TypeKind::Tuple(elems.iter().map(|e| self.substitute_type_node(e)).collect())
            }
            _ => type_node.kind.clone(),
        };

        TypeNode {
            kind: new_kind,
            span: type_node.span,
        }
    }

    /// Convert a Type back to a TypeNode.
    fn type_to_type_node(&self, t: &Type, span: Span) -> TypeNode {
        let kind = match t {
            Type::Primitive(p) => TypeKind::Primitive(*p),
            Type::Named(name, args) => {
                let type_args: Vec<TypeNode> = args
                    .iter()
                    .map(|a| self.type_to_type_node(a, span))
                    .collect();
                TypeKind::Named(name.clone(), type_args)
            }
            Type::List(inner) => TypeKind::List(Box::new(self.type_to_type_node(inner, span))),
            Type::Map(k, v) => TypeKind::Map(
                Box::new(self.type_to_type_node(k, span)),
                Box::new(self.type_to_type_node(v, span)),
            ),
            Type::Set(inner) => TypeKind::Set(Box::new(self.type_to_type_node(inner, span))),
            Type::Optional(inner) => {
                TypeKind::Named("Optional".to_string(), vec![self.type_to_type_node(inner, span)])
            }
            Type::Reference(inner) => {
                TypeKind::Reference(Box::new(self.type_to_type_node(inner, span)))
            }
            Type::Function { params, returns } => TypeKind::Function {
                params: params.iter().map(|p| self.type_to_type_node(p, span)).collect(),
                returns: Box::new(self.type_to_type_node(returns, span)),
            },
            Type::Tuple(types) => {
                TypeKind::Tuple(types.iter().map(|t| self.type_to_type_node(t, span)).collect())
            }
            Type::Variable(name) => TypeKind::Named(name.clone(), Vec::new()),
            Type::Void => TypeKind::Primitive(PrimitiveType::Void),
            Type::EmptyList => TypeKind::List(Box::new(TypeNode {
                kind: TypeKind::Primitive(PrimitiveType::Void),
                span,
            })),
            Type::EmptyMap => TypeKind::Map(
                Box::new(TypeNode {
                    kind: TypeKind::Primitive(PrimitiveType::Void),
                    span,
                }),
                Box::new(TypeNode {
                    kind: TypeKind::Primitive(PrimitiveType::Void),
                    span,
                }),
            ),
            Type::EmptySet => TypeKind::Set(Box::new(TypeNode {
                kind: TypeKind::Primitive(PrimitiveType::Void),
                span,
            })),
        };

        TypeNode { kind, span }
    }

    /// Transform an AST node, rewriting generic instantiations.
    fn transform_node(&mut self, node: AstNode) -> AstNode {
        match node {
            AstNode::Function(func) => AstNode::Function(FunctionNode {
                name: func.name,
                type_params: func.type_params,
                params: func.params,
                return_type: func.return_type,
                body: func.body.into_iter().map(|s| self.transform_statement(&s)).collect(),
                span: func.span,
            }),
            AstNode::Class {
                name,
                type_params,
                traits,
                fields,
                methods,
                span,
            } => AstNode::Class {
                name,
                type_params,
                traits,
                fields,
                methods: methods
                    .into_iter()
                    .map(|m| FunctionNode {
                        name: m.name,
                        type_params: m.type_params,
                        params: m.params,
                        return_type: m.return_type,
                        body: m.body.into_iter().map(|s| self.transform_statement(&s)).collect(),
                        span: m.span,
                    })
                    .collect(),
                span,
            },
            AstNode::Statement(stmt) => AstNode::Statement(self.transform_statement(&stmt)),
            other => other,
        }
    }

    fn transform_statement(&mut self, stmt: &StatementNode) -> StatementNode {
        let kind = match &stmt.kind {
            StatementKind::AutoDecl(name, type_node, expr) => StatementKind::AutoDecl(
                name.clone(),
                self.substitute_type_node(type_node),
                self.transform_expression(expr),
            ),
            StatementKind::TypedDecl(name, type_node, expr) => StatementKind::TypedDecl(
                name.clone(),
                self.substitute_type_node(type_node),
                self.transform_expression(expr),
            ),
            StatementKind::ConstDecl(name, type_node, expr) => StatementKind::ConstDecl(
                name.clone(),
                self.substitute_type_node(type_node),
                self.transform_expression(expr),
            ),
            StatementKind::Expression(expr) => {
                StatementKind::Expression(self.transform_expression(expr))
            }
            StatementKind::Block(stmts) => {
                StatementKind::Block(stmts.iter().map(|s| self.transform_statement(s)).collect())
            }
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => StatementKind::If {
                cond: self.transform_expression(cond),
                then_block: then_block.iter().map(|s| self.transform_statement(s)).collect(),
                else_block: else_block
                    .as_ref()
                    .map(|b| b.iter().map(|s| self.transform_statement(s)).collect()),
            },
            StatementKind::While { cond, body } => StatementKind::While {
                cond: self.transform_expression(cond),
                body: body.iter().map(|s| self.transform_statement(s)).collect(),
            },
            StatementKind::For {
                var,
                var_type,
                iter,
                body,
            } => StatementKind::For {
                var: var.clone(),
                var_type: self.substitute_type_node(var_type),
                iter: self.transform_expression(iter),
                body: body.iter().map(|s| self.transform_statement(s)).collect(),
            },
            StatementKind::Match { expr, arms } => StatementKind::Match {
                expr: self.transform_expression(expr),
                arms: arms
                    .iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern.clone(),
                        guard: arm.guard.as_ref().map(|g| self.transform_expression(g)),
                        body: arm.body.iter().map(|s| self.transform_statement(s)).collect(),
                    })
                    .collect(),
            },
            StatementKind::Return(Some(expr)) => {
                StatementKind::Return(Some(self.transform_expression(expr)))
            }
            StatementKind::Function(func) => {
                StatementKind::Function(FunctionNode {
                    name: func.name.clone(),
                    type_params: func.type_params.clone(),
                    params: func.params.clone(),
                    return_type: func.return_type.clone(),
                    body: func.body.iter().map(|s| self.transform_statement(s)).collect(),
                    span: func.span,
                })
            }
            other => other.clone(),
        };

        StatementNode {
            kind,
            span: stmt.span,
        }
    }

    fn transform_expression(&mut self, expr: &ExpressionNode) -> ExpressionNode {
        let kind = match &expr.kind {
            ExpressionKind::GenericType(name, type_args) => {
                // Transform generic instantiation to mangled name
                let resolved_args: Vec<Type> = type_args
                    .iter()
                    .map(|a| self.type_node_to_type(&self.substitute_type_node(a)))
                    .collect();
                let inst = Instantiation::new(name.clone(), resolved_args);

                if let Some(mangled) = self.name_mapping.get(&inst) {
                    // Replace with plain identifier
                    ExpressionKind::Identifier(mangled.clone())
                } else {
                    // Keep original (might be a built-in generic like Optional)
                    let transformed_args: Vec<TypeNode> =
                        type_args.iter().map(|a| self.substitute_type_node(a)).collect();
                    ExpressionKind::GenericType(name.clone(), transformed_args)
                }
            }
            ExpressionKind::Call { func, args } => ExpressionKind::Call {
                func: Box::new(self.transform_expression(func)),
                args: args.iter().map(|a| self.transform_expression(a)).collect(),
            },
            ExpressionKind::Binary { left, right, op } => ExpressionKind::Binary {
                left: Box::new(self.transform_expression(left)),
                right: Box::new(self.transform_expression(right)),
                op: *op,
            },
            ExpressionKind::Unary { expr, op, postfix } => ExpressionKind::Unary {
                expr: Box::new(self.transform_expression(expr)),
                op: *op,
                postfix: *postfix,
            },
            ExpressionKind::FieldAccess { expr, field } => ExpressionKind::FieldAccess {
                expr: Box::new(self.transform_expression(expr)),
                field: field.clone(),
            },
            ExpressionKind::ListAccess { expr, index } => ExpressionKind::ListAccess {
                expr: Box::new(self.transform_expression(expr)),
                index: Box::new(self.transform_expression(index)),
            },
            ExpressionKind::ListLiteral(elements) => ExpressionKind::ListLiteral(
                elements.iter().map(|e| self.transform_expression(e)).collect(),
            ),
            ExpressionKind::MapLiteral { entries, ordered } => ExpressionKind::MapLiteral {
                entries: entries
                    .iter()
                    .map(|(k, v)| (self.transform_expression(k), self.transform_expression(v)))
                    .collect(),
                ordered: *ordered,
            },
            ExpressionKind::SetLiteral(elements) => ExpressionKind::SetLiteral(
                elements.iter().map(|e| self.transform_expression(e)).collect(),
            ),
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => ExpressionKind::If {
                cond: Box::new(self.transform_expression(cond)),
                then_expr: Box::new(self.transform_expression(then_expr)),
                else_expr: Box::new(self.transform_expression(else_expr)),
            },
            ExpressionKind::Lambda { params, body } => ExpressionKind::Lambda {
                params: params.clone(),
                body: body.iter().map(|s| self.transform_statement(s)).collect(),
            },
            other => other.clone(),
        };

        ExpressionNode {
            kind,
            span: expr.span,
        }
    }
}

/// Run the monomorphization pass on an AST.
///
/// This is the main entry point for monomorphization.
pub fn monomorphize(ast: Vec<AstNode>) -> Vec<AstNode> {
    let mut mono = Monomorphizer::new();
    mono.monomorphize(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instantiation_mangling() {
        let inst = Instantiation::new(
            "Stack".to_string(),
            vec![Type::Primitive(PrimitiveType::Int)],
        );
        assert_eq!(inst.mangled_name(), "Stack_int");

        let inst2 = Instantiation::new(
            "Map".to_string(),
            vec![
                Type::Primitive(PrimitiveType::Str),
                Type::Primitive(PrimitiveType::Int),
            ],
        );
        assert_eq!(inst2.mangled_name(), "Map_str_int");
    }

    #[test]
    fn test_mangle_type() {
        assert_eq!(mangle_type(&Type::Primitive(PrimitiveType::Int)), "int");
        assert_eq!(
            mangle_type(&Type::List(Box::new(Type::Primitive(PrimitiveType::Str)))),
            "list__str"
        );
        assert_eq!(
            mangle_type(&Type::Named(
                "Optional".to_string(),
                vec![Type::Primitive(PrimitiveType::Int)]
            )),
            "Optional__int"
        );
    }
}
