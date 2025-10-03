use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs::read,
    path::{Path, PathBuf},
};

use tahuc_ast::{nodes::declarations::DeclarationKind, Module};
use tahuc_diagnostics::{
    context::DiagnosticContext, diagnostic::Diagnostic, reporter::DiagnosticReporter,
};
use tahuc_lexer::Lexer;
use tahuc_parser::{Parser, ParserResult};
use tahuc_span::FileId;

use crate::{config::{Config, FileInput}, error::ResolveError};

#[derive(Debug, Clone)]
pub struct ModuleResult {
    pub file_id: FileId,
    pub name: String,
    pub module: Module,
    pub path: PathBuf,
}

pub struct Import {
    pub name: String,
    pub dir: PathBuf,
    pub path: PathBuf,
    pub span: tahuc_span::Span,
}

impl From<Import> for FileInput {
    fn from(value: Import) -> Self {
        Self {
            name: value.name,
            dir: value.dir,
            path: value.path,
        }
    }
}

pub struct ModuleResolver<'a> {
    config: Config,
    context: &'a mut DiagnosticContext,
    reporter: &'a mut DiagnosticReporter,

    cache: HashMap<String, PathBuf>,
}

impl<'a> ModuleResolver<'a> {
    pub fn new(
        config: &Config,
        context: &'a mut DiagnosticContext,
        reporter: &'a mut DiagnosticReporter,
    ) -> Self {
        Self {
            config: config.clone(),
            context,
            reporter,

            cache: HashMap::new(),
        }
    }

    pub fn parse_modules(&mut self) -> HashMap<FileId, ModuleResult> {
        let mut module = HashMap::new();
        // let mut modules = HashMap::new();
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        // Queue entry files
        for file in &self.config.files {
            queue.push_back((file.clone(), Vec::new()));
        }

        while let Some((file_path, import_chain)) = queue.pop_front() {
            // Circular import detection
            if import_chain.contains(&file_path.path) {
                let mut chain = import_chain.clone();
                chain.push(file_path.path);
                self.report_error(ResolveError::CircularImport(chain).to_string());
                continue;
            }

            // Canonicalize path
            let canonical = match file_path.path.canonicalize() {
                Ok(f) => f,
                Err(err) => {
                    self.report_error(format!(
                        "Cannot resolve path `{}`: {}",
                        file_path.path.display(),
                        err
                    ));
                    continue;
                }
            };

            // Skip if already visited
            if visited.contains(&canonical) {
                continue;
            }
            visited.insert(canonical.clone());

            // Parse file
            match self.parse(&canonical) {
                Ok(result) => {
                    // Extract imports
                    let imports = self.extract_imports(&result, &canonical);
                    
                    // Queue imports with updated chain
                    let mut new_chain = import_chain.clone();
                    new_chain.push(canonical.clone());
                    
                    for import_path in imports {
                        queue.push_back((import_path.into(), new_chain.clone()));
                    }
                    
                    // Store parsed result
                    let file_id = result.module.file;
                    // modules.insert(file_id, (result.module, canonical));
                    module.insert(file_id, ModuleResult {
                        file_id,
                        name: file_path.name,
                        module: result.module,
                        path: canonical,
                    });
                }
                Err(err) => {
                    self.report_error(err.to_string());
                    continue;
                }
            }
        }

        module
    }

    fn parse(&mut self, path: &Path) -> Result<ParserResult, ResolveError> {
        // Read file
        let bytes = read(path)
            .map_err(|e| ResolveError::FailedToReadFile {
                path: path.to_path_buf(),
                error: e.to_string(),
            })?;

        // Convert to UTF-8
        let content = String::from_utf8(bytes)
            .map_err(|e| ResolveError::InvalidUtf8 {
                path: path.to_path_buf(),
                error: e.to_string(),
            })?;

        // Tokenize and parse
        let file_id = self.context.add_file(
            path.display().to_string(),
            &content,
        );
        
        let mut lexer = Lexer::new(&content, file_id, self.reporter);
        let lexer_result = lexer.tokenize();
        
        let mut parser = Parser::new(file_id, lexer_result, self.reporter);
        Ok(parser.parse())
    }

    fn extract_imports(
        &mut self,
        result: &ParserResult,
        current_file: &Path,
    ) -> Vec<Import> {
        let mut imports = Vec::new();

        for declaration in &result.module.declaration {
            if let DeclarationKind::Import(import) = &declaration.kind {
                match self.resolve(&import.path, current_file) {
                    Ok(resolved_path) => {
                        // imports.push(resolved_path);
                        imports.push(Import {
                            name: import.path.clone(),
                            dir: resolved_path.parent().unwrap().to_path_buf(),
                            path: resolved_path,
                            span: declaration.span,
                        });
                    }
                    Err(err) => {
                        self.reporter.report(
                            Diagnostic::error(err.to_string())
                                .with_span(declaration.span)
                        );
                    }
                }
            }
        }

        imports
    }

    fn resolve(
        &mut self,
        import_path: &str,
        current_file: &Path,
    ) -> Result<PathBuf, ResolveError> {
        // Check cache
        let cache_key = format!("{}:{}", current_file.display(), import_path);
        if let Some(cached) = self.cache.get(&cache_key) {
            return Ok(cached.clone());
        }

        // Resolve based on import type
        let resolved = if import_path.starts_with("./") || import_path.starts_with("../") {
            // Relative import
            self.resolve_relative(import_path, current_file)?
        } else if import_path.starts_with('/') {
            // Absolute import (root project)
            self.resolve_absolute(import_path)?
        } else {
            // Package import (search in include_dirs)
            self.resolve_package(import_path)?
        };

        // Cache hasil
        self.cache.insert(cache_key, resolved.clone());
        Ok(resolved)
    }

    fn resolve_relative(
        &mut self,
        import_path: &str,
        current_file: &Path,
    ) -> Result<PathBuf, ResolveError> {
        let current_dir = current_file.parent()
            .ok_or_else(|| ResolveError::InvalidPath {
                path: current_file.to_path_buf(),
            })?;

        let mut resolved = current_dir.join(import_path);

        // Try dengan extension .tahu jika belum ada
        if !resolved.exists() {
            resolved.set_extension("tahu");
        }

        // Handle directory imports
        if resolved.is_dir() {
            if let Some(entry) = self.find_entry_point(&resolved) {
                return Ok(entry);
            }
            return Err(ResolveError::DirectoryWithoutEntry {
                path: resolved,
            });
        }

        // Check if file exists
        if !resolved.exists() {
            return Err(ResolveError::FileNotFound {
                path: resolved,
            });
        }

        Ok(resolved)
    }

    fn resolve_absolute(&mut self, import_path: &str) -> Result<PathBuf, ResolveError> {
        let import_path = import_path.trim_start_matches('/');
        let mut resolved = self.config.current_dir.join(import_path);

        if !resolved.exists() {
            resolved.set_extension("tahu");
        }

        if resolved.is_dir() {
            if let Some(entry) = self.find_entry_point(&resolved) {
                return Ok(entry);
            }
            return Err(ResolveError::DirectoryWithoutEntry {
                path: resolved,
            });
        }

        if !resolved.exists() {
            return Err(ResolveError::FileNotFound {
                path: resolved,
            });
        }

        Ok(resolved)
    }

    fn resolve_package(&mut self, package_name: &str) -> Result<PathBuf, ResolveError> {
        let current_dir = self.config.current_dir.clone();
        // search in current dir
        let as_file = current_dir.join(package_name).with_extension("tahu");
        if as_file.exists() {
            return Ok(as_file);
        }

        // Search dalam include_dirs (dari -I flags)
        for include_dir in &self.config.include_dirs {
            let base_path = PathBuf::from(include_dir);

            // Try sebagai file langsung
            let as_file = base_path.join(package_name).with_extension("tahu");
            if as_file.exists() {
                return Ok(as_file);
            }

            // Try sebagai directory
            let as_dir = base_path.join(package_name);
            if as_dir.is_dir() {
                if let Some(entry) = self.find_entry_point(&as_dir) {
                    return Ok(entry);
                }
            }
        }

        // Not found di semua include dirs
        Err(ResolveError::PackageNotFound {
            name: package_name.to_string(),
            search_paths: self.config.include_dirs.clone(),
        })
    }

    fn find_entry_point(&self, dir: &Path) -> Option<PathBuf> {
        // Try common entry point names
        for entry_name in &["index.tahu", "lib.tahu", "main.tahu"] {
            let entry = dir.join(entry_name);
            if entry.exists() {
                return Some(entry);
            }
        }
        None
    }

    fn report_error(&mut self, msg: String) {
        self.reporter.report(Diagnostic::error(msg));
    }
}
