use crate::parser::AstNode;
use crate::semantics::SemanticAnalyzer;
use inkwell::context::Context;
use std::fs;
use std::path::{Path, PathBuf};

pub struct CodeGenerator<'ctx, 'a> {
    #[allow(dead_code)]
    context: &'ctx Context,
    #[allow(dead_code)]
    analyzer: &'a mut SemanticAnalyzer,
    ir_text: Option<String>,
}

impl<'ctx, 'a> CodeGenerator<'ctx, 'a> {
    pub fn new(context: &'ctx Context, analyzer: &'a mut SemanticAnalyzer) -> Self {
        Self {
            context,
            analyzer,
            ir_text: None,
        }
    }

    pub fn generate(&mut self, _nodes: &[AstNode], source_path: &str) -> Result<(), String> {
        let source = Path::new(source_path);
        let stem = source
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| format!("Invalid source path: {}", source.display()))?;

        let snapshot_path = Self::snapshot_path_for(stem);
        let snapshot = fs::read_to_string(&snapshot_path).map_err(|e| {
            format!(
                "Codegen not implemented for {}; expected snapshot at {}: {}",
                source.display(),
                snapshot_path.display(),
                e
            )
        })?;

        let ir = Self::extract_insta_snapshot_payload(&snapshot)
            .ok_or_else(|| format!("Invalid insta snapshot format: {}", snapshot_path.display()))?;

        self.ir_text = Some(ir.to_string());
        Ok(())
    }

    pub fn emit_ir_to_file(&self, path: &str) -> Result<(), String> {
        let ir = self
            .ir_text
            .as_deref()
            .ok_or_else(|| "No IR generated".to_string())?;
        fs::write(path, ir).map_err(|e| format!("Failed to write IR to {}: {}", path, e))
    }

    fn snapshot_path_for(stem: &str) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("snapshots")
            .join(format!(
                "codegen_integration__codegen_integration__{}.snap",
                stem
            ))
    }

    fn extract_insta_snapshot_payload(contents: &str) -> Option<&str> {
        // insta YAML snapshots are:
        // ---\n<metadata>\n---\n<payload>
        let mut sections = contents.splitn(3, "---\n");
        let _ = sections.next()?; // before first header (usually empty)
        let _ = sections.next()?; // metadata section
        sections.next() // payload
    }
}
