use crate::lexer::Span;
use crate::semantics::error::SemanticError;
use crate::semantics::types::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// The canonical stdlib items live in `crate::semantics::stdlib`.

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
        self.get_cloned(name).is_some()
    }

    fn add_symbol_to_current_scope(
        &mut self,
        name: &str,
        symbol: Symbol,
    ) -> Result<(), SemanticError> {
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
        if self.should_track_global_symbol(name, &symbol) {
            self.all_symbols.insert(name.to_string(), symbol);
        }
        Ok(())
    }

    pub fn add_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        self.add_symbol_to_current_scope(name, symbol)
    }

    fn should_track_global_symbol(&self, name: &str, symbol: &Symbol) -> bool {
        let _ = symbol;
        name != "self"
    }

    pub fn add_imported_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        self.add_symbol_to_current_scope(name, symbol)
    }

    pub fn global_scope_symbols(&self) -> HashMap<String, Symbol> {
        self.scopes
            .first()
            .map(|scope| scope.borrow().symbols.clone())
            .unwrap_or_default()
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

        let mut best: Option<(String, usize, u8)> = None;

        // Check all scopes
        for scope in self.scopes.iter().rev() {
            let scope_borrow = scope.borrow();
            best = Self::find_best_match(name, threshold, 0, scope_borrow.symbols.keys(), best);
        }

        // Also check all_symbols (hoisted functions, classes, etc.)
        best = Self::find_best_match(name, threshold, 1, self.all_symbols.keys(), best);

        // Check built-in functions
        best = Self::find_best_match(
            name,
            threshold,
            2,
            crate::semantics::stdlib::BUILT_IN_FUNCTIONS.keys().copied(),
            best,
        );

        best.map(|(name, _, _)| name)
    }

    fn find_best_match<S: AsRef<str>>(
        name: &str,
        threshold: usize,
        source_rank: u8,
        candidates: impl Iterator<Item = S>,
        best: Option<(String, usize, u8)>,
    ) -> Option<(String, usize, u8)> {
        let mut current_best = best;
        for candidate in candidates {
            let s = candidate.as_ref();
            let dist = edit_distance(name, s);
            if dist > threshold {
                continue;
            }

            if current_best
                .as_ref()
                .is_none_or(|(best_name, best_dist, best_rank)| {
                    dist < *best_dist
                        || (dist == *best_dist
                            && (source_rank < *best_rank
                                || (source_rank == *best_rank && s < best_name.as_str())))
                })
            {
                current_best = Some((s.to_string(), dist, source_rank));
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
