//! User-defined type generation (classes, interfaces, enums).
//!
//! This module handles generating LLVM types for classes, interfaces, and enums.

use super::CodeGenerator;
use crate::ast::{AstNode, EnumVariant, Field, PrimitiveType, TypeKind, TypeNode};
use crate::semantics::MethodSig;
use inkwell::AddressSpace;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, IntValue};
use std::collections::HashMap;

impl<'a> CodeGenerator<'a> {
    pub(super) fn generate_user_defined_types(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        // generate LLVM types for classes, interfaces, enums
        for node in nodes {
            match node {
                AstNode::Class { name, fields, .. } => {
                    let interfaces = self
                        .analyzer
                        .all_symbols()
                        .get(name)
                        .map(|sym| sym.interfaces.clone())
                        .unwrap_or_default();
                    self.classes.insert(name.clone(), fields.clone());
                    self.generate_class_type(name, fields, &interfaces)?;
                }
                AstNode::Interface { name, .. } => {
                    self.generate_interface_type(name)?;
                }
                AstNode::Enum { name, variants, .. } => {
                    self.generate_enum_type(name, variants)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub(super) fn generate_class_type(
        &mut self,
        name: &str,
        fields: &[Field],
        interfaces: &HashMap<String, HashMap<String, MethodSig>>,
    ) -> Result<(), String> {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();

        // Collect type parameter names for this class from the symbol table
        let type_param_names: std::collections::HashSet<String> =
            if let Some(class_symbol) = self.analyzer.all_symbols().get(name) {
                class_symbol
                    .type_params
                    .iter()
                    .map(|(param_name, _)| param_name.clone())
                    .collect()
            } else {
                std::collections::HashSet::new()
            };

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        for interface_name in interfaces.keys() {
            field_types.push(ptr_type.into());
            field_indices.insert(format!("vtable_{}", interface_name), field_types.len() - 1);
        }

        for field in fields {
            let field_type = if let TypeNode {
                kind: TypeKind::Named(field_type_name, _),
                ..
            } = &field.type_
            {
                // Check if this field type is a type parameter of the class
                if type_param_names.contains(field_type_name) {
                    // generic fields should be pointers (boxed values)
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    // for primitive fields, use pointer type to be consistent with boxing
                    if matches!(field.type_.kind, TypeKind::Primitive(_)) {
                        self.context.ptr_type(AddressSpace::default()).into()
                    } else {
                        self.llvm_type_from_mux_type(&field.type_)?
                    }
                }
            } else {
                // for primitive fields, use pointer type to be consistent with boxing
                if matches!(field.type_.kind, TypeKind::Primitive(_)) {
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    self.llvm_type_from_mux_type(&field.type_)?
                }
            };
            field_types.push(field_type);
            field_indices.insert(field.name.clone(), field_types.len() - 1);
        }

        let struct_type = self.context.struct_type(&field_types, false);
        self.type_map.insert(name.to_string(), struct_type.into());
        self.field_map.insert(name.to_string(), field_indices);
        self.field_types_map.insert(name.to_string(), field_types);

        Ok(())
    }

    pub(super) fn generate_class_vtables(
        &mut self,
        class_name: &str,
        interfaces: &HashMap<String, HashMap<String, MethodSig>>,
    ) -> Result<(), String> {
        for (interface_name, interface_methods) in interfaces {
            let mut vtable_values = Vec::new();
            for method_name in interface_methods.keys() {
                let class_method_name = format!("{}.{}", class_name, method_name);
                let func = self.functions.get(&class_method_name).ok_or_else(|| {
                    format!(
                        "Class {} does not implement method {} for interface {}",
                        class_name, method_name, interface_name
                    )
                })?;
                vtable_values.push(func.as_global_value().as_pointer_value().into());
            }
            // get vtable struct type
            let vtable_type = self
                .vtable_type_map
                .get(interface_name)
                .expect("vtable_type should be registered during interface generation");
            let vtable_const = vtable_type.const_named_struct(&vtable_values);
            // create global
            let vtable_name = format!("{}_{}_vtable", class_name, interface_name);
            let global =
                self.module
                    .add_global(vtable_type.as_basic_type_enum(), None, &vtable_name);
            global.set_initializer(&vtable_const);
            self.vtable_map.insert(
                format!("{}_{}", class_name, interface_name),
                global.as_pointer_value(),
            );
        }
        Ok(())
    }

    pub(super) fn generate_interface_type(&mut self, name: &str) -> Result<(), String> {
        // generate LLVM struct for interface: { *mut vtable, field1, field2, ... }
        // for simplicity, vtable is struct of void* function pointers
        let symbol = self
            .analyzer
            .all_symbols()
            .get(name)
            .ok_or_else(|| format!("Interface symbol '{}' not found in symbol table", name))?;
        let interface_methods = symbol
            .interfaces
            .get(name)
            .ok_or_else(|| format!("Interface methods for '{}' not found", name))?;

        // create vtable as struct of function pointers (all (void*) -> void* for now)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let fn_ptr_type = ptr_type; // since fn_type.ptr_type deprecated, use ptr_type

        let vtable_types = vec![fn_ptr_type.into(); interface_methods.len()];

        // vtable type: struct of function pointers
        let vtable_struct_type = self.context.struct_type(&vtable_types, false);
        self.vtable_type_map
            .insert(name.to_string(), vtable_struct_type);
        let vtable_ptr_type = self.context.ptr_type(AddressSpace::default());

        // interface struct: { vtable_ptr, field1, field2, ... }
        let mut struct_fields = vec![vtable_ptr_type.into()];

        // Add interface fields to the struct
        for (field_type, _) in symbol.fields.values() {
            let llvm_field_type = self.semantic_type_to_llvm(field_type)?;
            struct_fields.push(llvm_field_type);
        }

        let interface_struct_type = self.context.struct_type(&struct_fields, false);
        self.type_map
            .insert(name.to_string(), interface_struct_type.into());

        Ok(())
    }

    pub(super) fn generate_enum_type(
        &mut self,
        name: &str,
        variants: &[EnumVariant],
    ) -> Result<(), String> {
        // Tagged union: {i32 discriminant, <union fields...>}
        // Union fields are determined by analyzing all variant field types.
        let i32_type = self.context.i32_type();
        let mut variant_names = Vec::new();
        let mut variant_fields = HashMap::new();
        let mut max_fields = 0;
        for variant in variants {
            variant_names.push(variant.name.clone());
            let field_types = variant.data.as_ref().unwrap_or(&vec![]).clone();
            max_fields = max_fields.max(field_types.len());
            variant_fields.insert(variant.name.clone(), field_types);
        }
        self.enum_variants.insert(name.to_string(), variant_names);
        self.enum_variant_fields
            .insert(name.to_string(), variant_fields);

        // create struct type with discriminant + actual field types from variants
        let mut struct_fields = vec![i32_type.into()]; // discriminant first
        let union_field_types = self.get_enum_union_field_types(name);
        struct_fields.extend(union_field_types);
        let struct_type = self.context.struct_type(&struct_fields, false);
        self.type_map.insert(name.to_string(), struct_type.into());
        Ok(())
    }

    pub(super) fn get_variant_index(
        &self,
        enum_name: &str,
        variant_name: &str,
    ) -> Result<usize, String> {
        // hardcode indices for built-in enums to ensure deterministic behavior
        match (enum_name, variant_name) {
            ("Optional", "Some") => Ok(0),
            ("Optional", "None") => Ok(1),
            ("Result", "Ok") => Ok(0),
            ("Result", "Err") => Ok(1),
            _ => {
                // for user-defined enums, use HashMap lookup
                if let Some(variants) = self.enum_variants.get(enum_name) {
                    variants
                        .iter()
                        .position(|v| v == variant_name)
                        .ok_or_else(|| {
                            format!("Variant {} not found in enum {}", variant_name, enum_name)
                        })
                } else {
                    Err(format!("Enum {} not found", enum_name))
                }
            }
        }
    }

    /// load the discriminant from an enum value as an i32
    /// this function centralizes discriminant loading logic and ensures type safety
    pub(super) fn load_enum_discriminant(
        &self,
        enum_name: &str,
        enum_value: BasicValueEnum<'a>,
    ) -> Result<IntValue<'a>, String> {
        match enum_name {
            "Optional" | "Result" => {
                // for built-in enums, use runtime functions
                let discriminant_func = if enum_name == "Optional" {
                    "mux_optional_discriminant"
                } else {
                    "mux_result_discriminant"
                };
                let func = self
                    .module
                    .get_function(discriminant_func)
                    .ok_or(format!("{} not found", discriminant_func))?;

                let discriminant_call = self
                    .builder
                    .build_call(func, &[enum_value.into()], "discriminant_call")
                    .map_err(|e| e.to_string())?;

                Ok(discriminant_call
                    .try_as_basic_value()
                    .left()
                    .expect("mux_get_discriminant should return a basic value")
                    .into_int_value())
            }
            _ => {
                // for user-defined enums, load discriminant field directly
                let struct_type = self
                    .type_map
                    .get(enum_name)
                    .ok_or_else(|| format!("Enum {} not found in type map", enum_name))?
                    .into_struct_type();

                // allocate temporary storage for the enum value
                let temp_ptr = self
                    .builder
                    .build_alloca(struct_type, "temp_enum")
                    .map_err(|e| e.to_string())?;

                // store the enum value
                self.builder
                    .build_store(temp_ptr, enum_value)
                    .map_err(|e| e.to_string())?;

                // get pointer to discriminant field (index 0)
                let discriminant_ptr = self
                    .builder
                    .build_struct_gep(struct_type, temp_ptr, 0, "discriminant_ptr")
                    .map_err(|e| e.to_string())?;

                // load discriminant as i32
                let discriminant = self
                    .builder
                    .build_load(self.context.i32_type(), discriminant_ptr, "discriminant")
                    .map_err(|e| e.to_string())?
                    .into_int_value();

                Ok(discriminant)
            }
        }
    }

    /// create a type-safe comparison between discriminant and variant index
    /// this ensures both operands are i32 values and returns a boolean for branching
    pub(super) fn build_discriminant_comparison(
        &self,
        discriminant: IntValue<'a>,
        variant_index: usize,
    ) -> Result<IntValue<'a>, String> {
        let index_val = self
            .context
            .i32_type()
            .const_int(variant_index as u64, false);

        let result = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                discriminant,
                index_val,
                "match_cmp",
            )
            .map_err(|e| e.to_string())?;

        Ok(result)
    }

    /// determine the union field types for an enum based on its variants
    /// this replaces the hardcoded f64 assumption with actual field types
    pub(super) fn get_enum_union_field_types(&self, enum_name: &str) -> Vec<BasicTypeEnum<'a>> {
        let mut union_types = Vec::new();

        if let Some(variant_fields) = self.enum_variant_fields.get(enum_name) {
            // find the maximum number of fields across all variants
            let max_fields = variant_fields
                .values()
                .map(|fields| fields.len())
                .max()
                .unwrap_or(0);

            // for each field position, determine the appropriate union type
            for field_idx in 0..max_fields {
                let mut field_types = Vec::new();

                // collect all types used in this field position across variants
                for field_list in variant_fields.values() {
                    if field_idx < field_list.len() {
                        field_types.push(&field_list[field_idx]);
                    }
                }

                // determine the union type for this field position
                let union_type = self.determine_union_field_type(&field_types);
                union_types.push(union_type);
            }
        }

        union_types
    }

    /// determine the appropriate LLVM type for a union field position
    /// for now, use the largest common type or pointer for complex types
    pub(super) fn determine_union_field_type(
        &self,
        field_types: &[&TypeNode],
    ) -> BasicTypeEnum<'a> {
        if field_types.is_empty() {
            // no fields in this position, use i32 as default
            return self.context.i32_type().into();
        }

        // for simplicity, check if all types are the same
        let first_type = field_types[0];
        let all_same = field_types.iter().all(|t| t.kind == first_type.kind);

        if all_same {
            // all variants use the same type for this field
            // use the same types as llvm_type_from_mux_type for consistency
            match &first_type.kind {
                TypeKind::Primitive(PrimitiveType::Int) => self.context.i64_type().into(),
                TypeKind::Primitive(PrimitiveType::Float) => self.context.f64_type().into(),
                TypeKind::Primitive(PrimitiveType::Bool) => self.context.bool_type().into(),
                TypeKind::Primitive(PrimitiveType::Str) => {
                    self.context.ptr_type(AddressSpace::default()).into()
                }
                _ => self.context.ptr_type(AddressSpace::default()).into(), // default to pointer
            }
        } else {
            // mixed types - use pointer for now (could be improved with proper union types)
            self.context.ptr_type(AddressSpace::default()).into()
        }
    }
}
