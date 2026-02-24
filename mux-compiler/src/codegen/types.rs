//! Type conversion between Mux types, LLVM types, and type nodes.
//!
//! This module handles all the various type conversions needed during code generation.

use super::CodeGenerator;
use crate::ast::{PrimitiveType, TypeKind, TypeNode};
use crate::lexer::Span;
use crate::semantics::Type;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;

/// Placeholder span for synthetically-constructed TypeNodes in codegen.
const SYNTHETIC_SPAN: Span = Span {
    row_start: 0,
    col_start: 0,
    row_end: None,
    col_end: None,
};

impl<'a> CodeGenerator<'a> {
    /// Returns a pointer type, used for heap-allocated values.
    fn ptr_type(&self) -> BasicTypeEnum<'a> {
        self.context.ptr_type(AddressSpace::default()).into()
    }

    /// Resolve a generic type parameter name via the current generic context.
    fn resolve_generic_param(&self, name: &str) -> Option<&Type> {
        self.generic_context
            .as_ref()
            .and_then(|ctx| ctx.type_params.get(name))
    }

    /// Create a synthetic TypeNode (no source location) from a TypeKind.
    fn synthetic_type_node(kind: TypeKind) -> TypeNode {
        TypeNode {
            kind,
            span: SYNTHETIC_SPAN,
        }
    }

    pub(super) fn llvm_type_from_resolved_type(
        &self,
        resolved_type: &Type,
    ) -> Result<BasicTypeEnum<'a>, String> {
        match resolved_type {
            Type::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            Type::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            Type::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            Type::Primitive(PrimitiveType::Str) => Ok(self.ptr_type()),
            Type::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),
            Type::Primitive(PrimitiveType::Void) | Type::Void => {
                Err("Void type not allowed here".to_string())
            }
            Type::Primitive(PrimitiveType::Auto) => Err("Auto type should be resolved".to_string()),
            Type::Named(name, args) => {
                if args.is_empty() {
                    if let Some(concrete) = self.resolve_generic_param(name) {
                        return self.llvm_type_from_resolved_type(&concrete.clone());
                    }
                }
                if self.classes.contains_key(name) {
                    Ok(self.ptr_type())
                } else {
                    Err(format!("Unknown type: {}", name))
                }
            }
            Type::Function { .. }
            | Type::List(_)
            | Type::Map(_, _)
            | Type::Set(_)
            | Type::Tuple(_, _)
            | Type::Optional(_)
            | Type::Result(_, _)
            | Type::Reference(_)
            | Type::EmptyList
            | Type::EmptyMap
            | Type::EmptySet => Ok(self.ptr_type()),
            Type::Generic(_) => Err("Generic types should be resolved".to_string()),
            Type::Instantiated(_, _) => Err("Instantiated types should be resolved".to_string()),
            Type::Variable(name) => {
                if let Some(concrete) = self.resolve_generic_param(name) {
                    return self.llvm_type_from_resolved_type(&concrete.clone());
                }
                Err(format!("Variable type '{}' should be resolved", name))
            }
            Type::Never => Err("Never type not allowed here".to_string()),
            Type::Module(_) => {
                panic!("Module types should not appear in codegen - they are compile-time only")
            }
        }
    }

    pub(super) fn llvm_type_from_mux_type(
        &self,
        type_node: &TypeNode,
    ) -> Result<BasicTypeEnum<'a>, String> {
        self.type_kind_to_llvm_type(&type_node.kind)
    }

    pub(super) fn semantic_type_to_llvm(
        &self,
        sem_type: &Type,
    ) -> Result<BasicTypeEnum<'a>, String> {
        match sem_type {
            Type::Primitive(prim) => match prim {
                PrimitiveType::Int => Ok(self.context.i64_type().into()),
                PrimitiveType::Float => Ok(self.context.f64_type().into()),
                PrimitiveType::Bool => Ok(self.context.bool_type().into()),
                PrimitiveType::Str => Ok(self.ptr_type()),
                PrimitiveType::Char => Ok(self.context.i8_type().into()),
                PrimitiveType::Void => Err("Void type not allowed in fields".to_string()),
                PrimitiveType::Auto => Err("Auto type should be resolved".to_string()),
            },
            Type::Named(_, _)
            | Type::List(_)
            | Type::Map(_, _)
            | Type::Set(_)
            | Type::Optional(_)
            | Type::Reference(_)
            | Type::Function { .. } => Ok(self.ptr_type()),
            _ => Err(format!(
                "Unsupported type in interface fields: {:?}",
                sem_type
            )),
        }
    }

    pub(super) fn type_kind_to_llvm_type(
        &self,
        type_kind: &TypeKind,
    ) -> Result<BasicTypeEnum<'a>, String> {
        match type_kind {
            TypeKind::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            TypeKind::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            TypeKind::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            TypeKind::Primitive(PrimitiveType::Str) => Ok(self.ptr_type()),
            TypeKind::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),
            TypeKind::Named(name, _) => {
                if let Some(concrete) = self.resolve_generic_param(name) {
                    return self.llvm_type_from_resolved_type(&concrete.clone());
                }
                if self.enum_variants.contains_key(name) {
                    if name == "Optional" || name == "Result" {
                        Ok(self.ptr_type())
                    } else {
                        let struct_type = self
                            .type_map
                            .get(name)
                            .ok_or_else(|| format!("Enum type {} not found in type map", name))?;
                        Ok(*struct_type)
                    }
                } else {
                    Ok(self.ptr_type())
                }
            }
            TypeKind::List(_)
            | TypeKind::Map(_, _)
            | TypeKind::Set(_)
            | TypeKind::Tuple(_, _)
            | TypeKind::Reference(_)
            | TypeKind::Function { .. }
            | TypeKind::TraitObject(_) => Ok(self.ptr_type()),
            TypeKind::Primitive(PrimitiveType::Void) => {
                Err("Void type cannot be used in enum variant fields".to_string())
            }
            TypeKind::Primitive(PrimitiveType::Auto) | TypeKind::Auto => {
                Err("Auto type should be resolved before codegen".to_string())
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub(super) fn type_to_type_node(&self, type_: &Type) -> TypeNode {
        let auto_node = || Self::synthetic_type_node(TypeKind::Auto);
        let kind = match type_ {
            Type::Primitive(p) => TypeKind::Primitive(p.clone()),
            Type::List(inner) => TypeKind::List(Box::new(self.type_to_type_node(inner))),
            Type::Map(k, v) => TypeKind::Map(
                Box::new(self.type_to_type_node(k)),
                Box::new(self.type_to_type_node(v)),
            ),
            Type::Set(inner) => TypeKind::Set(Box::new(self.type_to_type_node(inner))),
            Type::Tuple(l, r) => TypeKind::Tuple(
                Box::new(self.type_to_type_node(l)),
                Box::new(self.type_to_type_node(r)),
            ),
            Type::Optional(inner) => {
                TypeKind::Named("Optional".to_string(), vec![self.type_to_type_node(inner)])
            }
            Type::Result(ok, err) => TypeKind::Named(
                "Result".to_string(),
                vec![self.type_to_type_node(ok), self.type_to_type_node(err)],
            ),
            Type::Reference(inner) => TypeKind::Reference(Box::new(self.type_to_type_node(inner))),
            Type::Void => TypeKind::Primitive(PrimitiveType::Void),
            Type::EmptyList => TypeKind::List(Box::new(auto_node())),
            Type::EmptyMap => TypeKind::Map(Box::new(auto_node()), Box::new(auto_node())),
            Type::EmptySet => TypeKind::Set(Box::new(auto_node())),
            Type::Function {
                params, returns, ..
            } => TypeKind::Function {
                params: params.iter().map(|p| self.type_to_type_node(p)).collect(),
                returns: Box::new(self.type_to_type_node(returns)),
            },
            Type::Named(name, generics) | Type::Instantiated(name, generics) => TypeKind::Named(
                name.clone(),
                generics.iter().map(|g| self.type_to_type_node(g)).collect(),
            ),
            Type::Generic(name) => TypeKind::Named(name.clone(), vec![]),
            Type::Variable(_) | Type::Never => TypeKind::Auto,
            Type::Module(_) => {
                panic!("Module types should not appear in codegen - they are compile-time only")
            }
        };
        Self::synthetic_type_node(kind)
    }

    pub(super) fn type_node_to_type(&self, type_node: &TypeNode) -> Type {
        match &type_node.kind {
            TypeKind::Primitive(p) => Type::Primitive(p.clone()),
            TypeKind::List(inner) => Type::List(Box::new(self.type_node_to_type(inner))),
            TypeKind::Map(k, v) => Type::Map(
                Box::new(self.type_node_to_type(k)),
                Box::new(self.type_node_to_type(v)),
            ),
            TypeKind::Set(inner) => Type::Set(Box::new(self.type_node_to_type(inner))),
            TypeKind::Tuple(l, r) => Type::Tuple(
                Box::new(self.type_node_to_type(l)),
                Box::new(self.type_node_to_type(r)),
            ),

            TypeKind::TraitObject(_) => Type::Variable("trait_object".to_string()),

            TypeKind::Reference(inner) => Type::Reference(Box::new(self.type_node_to_type(inner))),
            TypeKind::Named(name, generics) => {
                if generics.is_empty() {
                    // special case for Pair.from method
                    if self.current_function_name == Some("Pair.from".to_string()) {
                        if name == "T" {
                            return Type::Named("string".to_string(), vec![]);
                        } else if name == "U" {
                            return Type::Primitive(PrimitiveType::Bool);
                        }
                    }
                    if let Some(concrete_type) = self.resolve_generic_param(name) {
                        concrete_type.clone()
                    } else {
                        Type::Named(name.clone(), vec![])
                    }
                } else {
                    Type::Named(
                        name.clone(),
                        generics.iter().map(|g| self.type_node_to_type(g)).collect(),
                    )
                }
            }
            TypeKind::Function { params, returns } => Type::Function {
                params: params.iter().map(|p| self.type_node_to_type(p)).collect(),
                returns: Box::new(self.type_node_to_type(returns)),
                default_count: 0,
            },
            TypeKind::Auto => Type::Variable("auto".to_string()),
        }
    }

    pub(super) fn resolve_type(&self, type_: &Type) -> Result<Type, String> {
        match type_ {
            Type::Primitive(_)
            | Type::Void
            | Type::Never
            | Type::EmptyList
            | Type::EmptyMap
            | Type::EmptySet => Ok(type_.clone()),
            Type::Generic(name) | Type::Variable(name) => {
                if let Some(concrete) = self.resolve_generic_param(name) {
                    return self.resolve_type(&concrete.clone());
                }
                Err(format!("Unresolved generic: {}", name))
            }
            Type::Named(name, type_args) => {
                if type_args.is_empty() {
                    if let Some(concrete) = self.resolve_generic_param(name) {
                        return self.resolve_type(&concrete.clone());
                    }
                    Ok(Type::Named(name.clone(), vec![]))
                } else {
                    let resolved_args = type_args
                        .iter()
                        .map(|arg| self.resolve_type(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Type::Named(name.clone(), resolved_args))
                }
            }
            Type::Instantiated(name, type_args) => {
                let resolved_args = type_args
                    .iter()
                    .map(|arg| self.resolve_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Instantiated(name.clone(), resolved_args))
            }
            Type::List(inner) => Ok(Type::List(Box::new(self.resolve_type(inner)?))),
            Type::Set(inner) => Ok(Type::Set(Box::new(self.resolve_type(inner)?))),
            Type::Map(k, v) => Ok(Type::Map(
                Box::new(self.resolve_type(k)?),
                Box::new(self.resolve_type(v)?),
            )),
            Type::Tuple(l, r) => Ok(Type::Tuple(
                Box::new(self.resolve_type(l)?),
                Box::new(self.resolve_type(r)?),
            )),

            Type::Optional(inner) => Ok(Type::Optional(Box::new(self.resolve_type(inner)?))),
            Type::Result(ok, err) => Ok(Type::Result(
                Box::new(self.resolve_type(ok)?),
                Box::new(self.resolve_type(err)?),
            )),
            Type::Reference(inner) => Ok(Type::Reference(Box::new(self.resolve_type(inner)?))),
            Type::Function {
                params,
                returns,
                default_count,
                ..
            } => Ok(Type::Function {
                params: params
                    .iter()
                    .map(|p| self.resolve_type(p))
                    .collect::<Result<Vec<_>, _>>()?,
                returns: Box::new(self.resolve_type(returns)?),
                default_count: *default_count,
            }),
            Type::Module(_) => {
                panic!("Module types should not appear in codegen - they are compile-time only")
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub(super) fn type_to_string(&self, type_: &Type) -> String {
        match type_ {
            Type::Primitive(PrimitiveType::Int) => "int".to_string(),
            Type::Primitive(PrimitiveType::Float) => "float".to_string(),
            Type::Primitive(PrimitiveType::Bool) => "bool".to_string(),
            Type::Primitive(PrimitiveType::Str) => "string".to_string(),
            Type::List(inner) => format!("list_{}", self.type_to_string(inner)),
            _ => "unknown".to_string(),
        }
    }
}
