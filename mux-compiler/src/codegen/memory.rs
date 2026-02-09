//! Reference counting (RC) scope management for CodeGenerator.
//!
//! This module handles tracking RC-allocated variables and generating cleanup code.

use super::CodeGenerator;
use crate::semantics::Type;
use inkwell::AddressSpace;
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'a> CodeGenerator<'a> {
    /// Push a new RC scope onto the stack. Call this when entering a new scope
    /// (function, if/else block, loop body, match arm, etc.)
    pub(super) fn push_rc_scope(&mut self) {
        self.rc_scope_stack.push(Vec::new());
    }
    /// Generate cleanup code for all scopes (used before return statements).
    /// This doesn't pop the scopes - just generates the cleanup code.
    pub(super) fn generate_all_scopes_cleanup(&mut self) -> Result<(), String> {
        // Collect all variables from all scopes to avoid borrow issues
        let all_vars: Vec<(String, PointerValue<'a>)> = self
            .rc_scope_stack
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().cloned())
            .collect();

        self.generate_cleanup_for_vars(&all_vars)
    }

    /// Generate mux_rc_dec calls for a list of variables.
    pub(super) fn generate_cleanup_for_vars(
        &mut self,
        vars: &[(String, PointerValue<'a>)],
    ) -> Result<(), String> {
        let rc_dec = self
            .module
            .get_function("mux_rc_dec")
            .ok_or("mux_rc_dec not found")?;
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        for (name, alloca) in vars {
            // Load the pointer value from the alloca
            let value = self
                .builder
                .build_load(ptr_type, *alloca, &format!("rc_load_{}", name))
                .map_err(|e| e.to_string())?;

            // Call mux_rc_dec
            self.builder
                .build_call(rc_dec, &[value.into()], &format!("rc_dec_{}", name))
                .map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    /// Track an RC-allocated variable in the current scope.
    /// The variable will have mux_rc_dec called on it when the scope ends.
    pub(super) fn track_rc_variable(&mut self, name: &str, alloca: PointerValue<'a>) {
        if let Some(current_scope) = self.rc_scope_stack.last_mut() {
            current_scope.push((name.to_string(), alloca));
        }
    }

    /// Check if a type requires RC tracking.
    /// Currently all boxed values (primitives, strings, objects) use RC.
    pub(super) fn type_needs_rc_tracking(&self, ty: &Type) -> bool {
        match ty {
            // Primitives are boxed, so they need RC tracking
            Type::Primitive(_) => true,
            // Named types (classes) are RC-allocated
            Type::Named(_, _) => true,
            // Generic types that resolve to RC types
            Type::Generic(_) | Type::Variable(_) => true,
            // Collections contain Values which are RC-allocated
            Type::List(_) | Type::Map(_, _) | Type::Set(_) => true,
            // Tuples contain Values which are RC-allocated
            Type::Tuple(_, _) => true,
            // Optional contains boxed values
            Type::Optional(_) => true,
            // References are pointers to RC values
            Type::Reference(_) => true,
            // Function types are pointers, not RC
            Type::Function { .. } => false,
            // Void doesn't need tracking
            Type::Void | Type::Never => false,
            // Empty collections don't need tracking
            Type::EmptyList | Type::EmptyMap | Type::EmptySet => false,
            // Instantiated types (like Pair<string, bool>) need RC
            Type::Instantiated(_, _) => true,
            // Module references don't need RC
            Type::Module(_) => false,
        }
    }

    /// Increment the RC of a value if it's an RC-allocated pointer.
    /// Returns the same value. Use this before cleanup when returning a value.
    pub(super) fn rc_inc_if_pointer(
        &mut self,
        value: BasicValueEnum<'a>,
    ) -> Result<BasicValueEnum<'a>, String> {
        if value.is_pointer_value() {
            let rc_inc = self
                .module
                .get_function("mux_rc_inc")
                .ok_or("mux_rc_inc not found")?;
            self.builder
                .build_call(rc_inc, &[value.into()], "rc_inc_return")
                .map_err(|e| e.to_string())?;
        }
        Ok(value)
    }
}
