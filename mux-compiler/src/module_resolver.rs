use crate::ast::AstNode;
use crate::diagnostic::{ColorConfig, DiagnosticEmitter, Files, StandardEmitter, ToDiagnostic};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::source::Source;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

pub struct ModuleResolver {
    base_path: PathBuf,
    compiled_modules: HashMap<String, Vec<AstNode>>,
    import_stack: Vec<String>,
    being_imported: HashSet<String>,
    canonical_cache: HashMap<PathBuf, String>, // canonical path -> module_path
}

impl ModuleResolver {
    pub fn new(base_path: PathBuf) -> Self {
        Self {
            base_path,
            compiled_modules: HashMap::new(),
            import_stack: Vec::new(),
            being_imported: HashSet::new(),
            canonical_cache: HashMap::new(),
        }
    }

    // Resolve import path relative to current file (for relative imports)
    pub fn resolve_import_path(
        &mut self,
        module_path: &str,
        current_file: Option<&Path>,
        files: &mut Files,
    ) -> Result<Vec<AstNode>, String> {
        // Determine the actual file path based on import type
        let file_path = if module_path.starts_with("./") || module_path.starts_with("../") {
            // Relative import - resolve relative to current file
            let current_dir = current_file
                .and_then(|p| p.parent())
                .ok_or("Cannot resolve relative import: no current file")?;

            let relative_path = module_path.trim_start_matches("./");
            let mut path = current_dir.to_path_buf();

            // Handle ../ parts
            for part in relative_path.split('/') {
                if part == ".." {
                    path.pop();
                } else if !part.is_empty() {
                    path.push(part);
                }
            }
            path.set_extension("mux");
            path
        } else if module_path.starts_with('/') {
            // Absolute import
            let mut path = PathBuf::from(module_path);
            path.set_extension("mux");
            path
        } else {
            // Project-relative import (utils.logger)
            self.module_path_to_file(module_path)?
        };

        // Canonicalize for cache key
        let canonical_path = file_path
            .canonicalize()
            .map_err(|e| format!("Cannot resolve module path {}: {}", module_path, e))?;

        // Check cache by canonical path
        if let Some(cached_module_path) = self.canonical_cache.get(&canonical_path)
            && let Some(nodes) = self.compiled_modules.get(cached_module_path)
        {
            return Ok(nodes.clone());
        }

        // Check circular imports
        if self.being_imported.contains(module_path) {
            return Err(format!(
                "Circular import detected: {} -> {}",
                self.import_stack.join(" -> "),
                module_path
            ));
        }

        // Mark as being imported
        self.being_imported.insert(module_path.to_string());
        self.import_stack.push(module_path.to_string());

        // Parse module
        let nodes = self.parse_module(&canonical_path, files)?;

        // Cache the canonical path
        self.canonical_cache
            .insert(canonical_path, module_path.to_string());

        Ok(nodes)
    }

    pub fn finish_import(&mut self, module_path: &str) {
        // Remove from tracking after module is fully analyzed
        self.import_stack.pop();
        self.being_imported.remove(module_path);
    }

    pub fn cache_module(&mut self, module_path: &str, nodes: Vec<AstNode>) {
        self.compiled_modules.insert(module_path.to_string(), nodes);
    }

    /// Check if a module path resolves to a file, directory, both, or neither.
    /// Returns (has_file, has_directory) tuple.
    pub fn check_module_path(&self, module_path: &str) -> (bool, bool) {
        let mut file_path = self.base_path.clone();
        for part in module_path.split('.') {
            file_path.push(part);
        }

        let mux_file = file_path.with_extension("mux");
        let dir_path = file_path;

        let has_file = mux_file.exists() && mux_file.is_file();
        let has_directory = dir_path.exists() && dir_path.is_dir();

        (has_file, has_directory)
    }

    /// Get all .mux files in a directory module
    pub fn get_submodules(&self, module_path: &str) -> Result<Vec<String>, String> {
        let mut dir_path = self.base_path.clone();
        for part in module_path.split('.') {
            dir_path.push(part);
        }

        if !dir_path.exists() || !dir_path.is_dir() {
            return Err(format!("Module directory not found: {}", module_path));
        }

        let mut submodules = Vec::new();
        if let Ok(entries) = std::fs::read_dir(&dir_path) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file()
                    && path.extension().is_some_and(|ext| ext == "mux")
                    && let Some(stem) = path.file_stem().and_then(|s| s.to_str())
                {
                    submodules.push(stem.to_string());
                }
            }
        }

        Ok(submodules)
    }

    fn module_path_to_file(&self, module_path: &str) -> Result<PathBuf, String> {
        let mut path = self.base_path.clone();
        for part in module_path.split('.') {
            path.push(part);
        }
        path.set_extension("mux");

        if !path.exists() {
            return Err(format!(
                "Module not found: {} (looked for {:?})",
                module_path, path
            ));
        }

        Ok(path)
    }

    fn parse_module(&self, file_path: &Path, files: &mut Files) -> Result<Vec<AstNode>, String> {
        let source_str = std::fs::read_to_string(file_path)
            .map_err(|e| format!("Failed to open module: {}", e))?;

        let file_id = files.add(file_path, source_str.clone());
        let mut src = Source::from_string(source_str);

        let mut lex = Lexer::new(&mut src);
        let tokens = match lex.lex_all() {
            Ok(t) => t,
            Err(e) => {
                let emitter = StandardEmitter::new(ColorConfig::Auto);
                emitter.emit(&e.to_diagnostic(file_id), files);
                return Err(format!("Lexer error in module {}", file_path.display()));
            }
        };

        let mut parser = Parser::new(&tokens);
        match parser.parse() {
            Ok(nodes) => Ok(nodes),
            Err((_, errors)) => {
                let emitter = StandardEmitter::new(ColorConfig::Auto);
                let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(file_id)).collect();
                emitter.emit_batch(&diagnostics, files);
                Err(format!("Parse error in module {}", file_path.display()))
            }
        }
    }
}
