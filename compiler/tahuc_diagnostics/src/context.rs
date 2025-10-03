use std::collections::HashMap;

use tahuc_span::{FileId, SourceFile};

#[derive(Debug)]
pub struct DiagnosticContext {
    files: HashMap<FileId, SourceFile>,
    next_file_id: u32,
}

impl DiagnosticContext {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            next_file_id: 0,
        }
    }

    pub fn add_file(&mut self, path: String, content: &String) -> FileId {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        
        let source_file = SourceFile::new(file_id, path, content);
        self.files.insert(file_id, source_file);
        
        file_id
    }

    pub fn get_file(&self, file_id: FileId) -> Option<&SourceFile> {
        self.files.get(&file_id)
    }

    pub fn file_path(&self, file_id: FileId) -> Option<&str> {
        self.files.get(&file_id).map(|f| f.path.as_str())
    }
}

impl Default for DiagnosticContext {
    fn default() -> Self {
        Self::new()
    }
}