//! File tracking for multi-file diagnostics.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Unique identifier for a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(usize);

/// Information about a source file.
#[derive(Debug, Clone)]
pub(super) struct FileInfo {
    pub(super) path: PathBuf,
    pub(super) source: String,
}

/// Manages source files for diagnostic reporting.
pub struct Files {
    files: Vec<FileInfo>,
    path_to_id: HashMap<PathBuf, FileId>,
}

impl Files {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            path_to_id: HashMap::new(),
        }
    }

    /// Add a file to the registry and return its ID.
    pub fn add(&mut self, path: impl AsRef<Path>, source: String) -> FileId {
        let path = path.as_ref();

        // Store path relative to current working directory for consistent
        // error output across different environments (local vs CI)
        let path = if path.is_absolute() {
            path.strip_prefix(std::env::current_dir().unwrap_or_else(|_| path.to_path_buf()))
                .unwrap_or(path)
                .to_path_buf()
        } else {
            path.to_path_buf()
        };

        // Check if file already exists
        if let Some(&id) = self.path_to_id.get(&path) {
            return id;
        }

        let id = FileId(self.files.len());
        let info = FileInfo {
            path: path.clone(),
            source,
        };

        self.files.push(info);
        self.path_to_id.insert(path, id);
        id
    }

    /// Get file info by ID.
    pub(super) fn get(&self, id: FileId) -> Option<&FileInfo> {
        self.files.get(id.0)
    }
}

impl Default for Files {
    fn default() -> Self {
        Self::new()
    }
}
