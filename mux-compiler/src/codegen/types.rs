//! Type conversion between Mux types, LLVM types, and type nodes.
//!
//! This module handles all the various type conversions needed during code generation.

use super::CodeGenerator;
use crate::ast::{PrimitiveType, TypeKind, TypeNode};
use crate::lexer::Span;
use crate::semantics::Type;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;

impl<'a> CodeGenerator<'a> {
    pub(super) fn llvm_type_from_resolved_type(
        &self,
        resolved_type: &Type,
    ) -> Result<BasicTypeEnum<'a>, String> {
        match resolved_type {
            Type::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            Type::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            Type::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            Type::Primitive(PrimitiveType::Str) => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),
            Type::Primitive(PrimitiveType::Void) => Err("Void type not allowed here".to_string()),
            Type::Void => Err("Void type not allowed here".to_string()),
            Type::Primitive(PrimitiveType::Auto) => Err("Auto type should be resolved".to_string()),
            Type::Named(name, args) => {
                if args.is_empty() {
                    if let Some(context) = &self.generic_context {
                        if let Some(concrete) = context.type_params.get(name) {
                            return self.llvm_type_from_resolved_type(concrete);
                        }
                    }
                }

                if self.classes.contains_key(name) {
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                } else {
                    Err(format!("Unknown type: {}", name))
                }
            }
            Type::Function {
                params: _,
                returns: _,
                ..
            } => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::List(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Map(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Set(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Optional(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Reference(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),

            Type::EmptyList => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::EmptyMap => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::EmptySet => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Generic(_) => Err("Generic types should be resolved".to_string()),
            Type::Instantiated(_, _) => Err("Instantiated types should be resolved".to_string()),
            Type::Variable(name) => {
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.llvm_type_from_resolved_type(concrete);
                    }
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
                PrimitiveType::Str => Ok(self.context.ptr_type(AddressSpace::default()).into()),
                PrimitiveType::Char => Ok(self.context.i8_type().into()),
                PrimitiveType::Void => Err("Void type not allowed in fields".to_string()),
                PrimitiveType::Auto => Err("Auto type should be resolved".to_string()),
            },
            Type::Named(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::List(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Map(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Set(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Optional(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Reference(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Function { .. } => Ok(self.context.ptr_type(AddressSpace::default()).into()),
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
            // Primitive types
            TypeKind::Primitive(PrimitiveType::Int) => Ok(self.context.i64_type().into()),
            TypeKind::Primitive(PrimitiveType::Float) => Ok(self.context.f64_type().into()),
            TypeKind::Primitive(PrimitiveType::Bool) => Ok(self.context.bool_type().into()),
            TypeKind::Primitive(PrimitiveType::Str) => {
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            TypeKind::Primitive(PrimitiveType::Char) => Ok(self.context.i8_type().into()),

            // Named types (enums, classes)
            TypeKind::Named(name, _) => {
                // Check if it's a generic type parameter
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.llvm_type_from_resolved_type(concrete);
                    }
                }

                // Check if it's an enum
                if self.enum_variants.contains_key(name) {
                    if name == "Optional" || name == "Result" {
                        // Optional/Result are Value* pointers
                        Ok(self.context.ptr_type(AddressSpace::default()).into())
                    } else {
                        // Custom enums are struct values
                        let struct_type = self
                            .type_map
                            .get(name)
                            .ok_or_else(|| format!("Enum type {} not found in type map", name))?;
                        Ok(*struct_type)
                    }
                } else {
                    // Classes are pointers
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                }
            }

            // Collection types (all pointers)
            TypeKind::List(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Map(_, _) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Set(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::Reference(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),

            // Function and trait types (pointers)
            TypeKind::Function { .. } => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            TypeKind::TraitObject(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),

            // Invalid types
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
        match type_ {
            Type::Primitive(p) => TypeNode {
                kind: TypeKind::Primitive(p.clone()),
                span: Span::new(0, 0),
            },
            Type::List(inner) => TypeNode {
                kind: TypeKind::List(Box::new(self.type_to_type_node(inner))),
                span: Span::new(0, 0),
            },
            Type::Map(k, v) => TypeNode {
                kind: TypeKind::Map(
                    Box::new(self.type_to_type_node(k)),
                    Box::new(self.type_to_type_node(v)),
                ),
                span: Span::new(0, 0),
            },
            Type::Set(inner) => TypeNode {
                kind: TypeKind::Set(Box::new(self.type_to_type_node(inner))),
                span: Span::new(0, 0),
            },

            Type::Optional(inner) => TypeNode {
                kind: TypeKind::Named("Optional".to_string(), vec![self.type_to_type_node(inner)]),
                span: Span::new(0, 0),
            },
            Type::Reference(inner) => TypeNode {
                kind: TypeKind::Reference(Box::new(self.type_to_type_node(inner))),
                span: Span::new(0, 0),
            },
            Type::Void => TypeNode {
                kind: TypeKind::Primitive(PrimitiveType::Void),
                span: Span::new(0, 0),
            },
            Type::EmptyList => TypeNode {
                kind: TypeKind::List(Box::new(TypeNode {
                    kind: TypeKind::Auto,
                    span: Span::new(0, 0),
                })),
                span: Span::new(0, 0),
            },
            Type::EmptyMap => TypeNode {
                kind: TypeKind::Map(
                    Box::new(TypeNode {
                        kind: TypeKind::Auto,
                        span: Span::new(0, 0),
                    }),
                    Box::new(TypeNode {
                        kind: TypeKind::Auto,
                        span: Span::new(0, 0),
                    }),
                ),
                span: Span::new(0, 0),
            },
            Type::EmptySet => TypeNode {
                kind: TypeKind::Set(Box::new(TypeNode {
                    kind: TypeKind::Auto,
                    span: Span::new(0, 0),
                })),
                span: Span::new(0, 0),
            },
            Type::Function {
                params, returns, ..
            } => TypeNode {
                kind: TypeKind::Function {
                    params: params.iter().map(|p| self.type_to_type_node(p)).collect(),
                    returns: Box::new(self.type_to_type_node(returns)),
                },
                span: Span::new(0, 0),
            },
            Type::Named(name, generics) => TypeNode {
                kind: TypeKind::Named(
                    name.clone(),
                    generics.iter().map(|g| self.type_to_type_node(g)).collect(),
                ),
                span: Span::new(0, 0),
            },
            Type::Variable(_) => TypeNode {
                kind: TypeKind::Auto,
                span: Span::new(0, 0),
            }, // should not happen
            Type::Never => TypeNode {
                kind: TypeKind::Auto, // use Auto as placeholder
                span: Span::new(0, 0),
            }, // should not happen
            Type::Generic(name) => TypeNode {
                kind: TypeKind::Named(name.clone(), vec![]),
                span: Span::new(0, 0),
            },
            Type::Instantiated(name, generics) => TypeNode {
                kind: TypeKind::Named(
                    name.clone(),
                    generics.iter().map(|g| self.type_to_type_node(g)).collect(),
                ),
                span: Span::new(0, 0),
            },
            Type::Module(_) => {
                panic!("Module types should not appear in codegen - they are compile-time only")
            }
        }
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

                    // check if this is a generic parameter by looking at the current context
                    if let Some(context) = &self.generic_context {
                        if let Some(concrete_type) = context.type_params.get(name) {
                            // return the concrete type directly
                            concrete_type.clone()
                        } else {
                            Type::Named(name.clone(), vec![])
                        }
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
                if let Some(context) = &self.generic_context {
                    if let Some(concrete) = context.type_params.get(name) {
                        return self.resolve_type(concrete);
                    }
                }
                Err(format!("Unresolved generic: {}", name))
            }
            Type::Named(name, type_args) => {
                if type_args.is_empty() {
                    if let Some(context) = &self.generic_context {
                        if let Some(concrete) = context.type_params.get(name) {
                            return self.resolve_type(concrete);
                        }
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

            Type::Optional(inner) => Ok(Type::Optional(Box::new(self.resolve_type(inner)?))),
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

    // recursion is necessary here for proper type resolution
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
