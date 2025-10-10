use std::{
    collections::{HashMap, HashSet},
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
    pub deps: Vec<FileId>,
}

struct ModuleDeps {
    pub file_id: FileId,
    pub name: String,
    pub module: Module,
    pub path: PathBuf,
    pub deps: Vec<Import>,
}

#[derive(Debug, Clone)]
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
    context: &'a mut DiagnosticContext,
    reporter: &'a mut DiagnosticReporter,

    cache: HashMap<String, PathBuf>,
}

impl<'a> ModuleResolver<'a> {
    pub fn new(
        context: &'a mut DiagnosticContext,
        reporter: &'a mut DiagnosticReporter,
    ) -> Self {
        Self {
            context,
            reporter,

            cache: HashMap::new(),
        }
    }

    fn update_timings(&mut self, config: &mut Config, start_time: std::time::Instant) {
        config.timings.entry(crate::config::Timings::ModuleResolver)
            .and_modify(|e| *e += start_time.elapsed())
            .or_insert(start_time.elapsed());
    }

    pub fn parse_modules(&mut self, config: &mut Config) -> HashMap<FileId, ModuleResult> {
        let mut module = Vec::new();
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut order = Vec::new(); // Track post-order

        // Process each entry file with DFS
        for file in config.files.clone() {
            self.dfs_visit(
                file.clone(),
                &mut module,
                &mut visited,
                &mut rec_stack,
                &mut Vec::new(),
                &mut order,
                config,
            );
        }

        let start_time = std::time::Instant::now();
        // Save order to config
        config.module_order = order;

        // Convert ModuleDeps to ModuleResult
        let mut modules = HashMap::new();
        let mut save_module = HashMap::new();

        for modul in module.iter() {
            save_module.insert(modul.path.clone(), modul.file_id);
        }

        for modul in module {
            let mut deps = Vec::new();
            for dep in modul.deps.iter() {
                if let Some(dep_id) = save_module.get(&dep.path) {
                    deps.push(*dep_id);
                }
            }
            modules.insert(modul.file_id, ModuleResult {
                file_id: modul.file_id,
                name: modul.name,
                module: modul.module,
                path: modul.path,
                deps,
            });
        }

        self.update_timings(config, start_time);
        modules
    }

    fn dfs_visit(
        &mut self,
        file_path: FileInput,
        module: &mut Vec<ModuleDeps>,
        visited: &mut HashSet<PathBuf>,
        rec_stack: &mut HashSet<PathBuf>,
        import_chain: &mut Vec<PathBuf>,
        order: &mut Vec<FileId>, // Track order
        config: &mut Config,
    ) {
        // Parse file
        // Canonicalize path
        let canonical = match file_path.path.canonicalize() {
            Ok(f) => f,
            Err(err) => {
                self.report_error(format!(
                    "Cannot resolve path `{}`: {}",
                    file_path.path.display(),
                    err
                ));
                return;
            }
        };

        // Circular import detection
        if rec_stack.contains(&canonical) {
            let mut chain = import_chain.clone();
            chain.push(canonical);
            self.report_error(ResolveError::CircularImport(chain).to_string());
            return;
        }

        // Skip if already visited
        if visited.contains(&canonical) {
            return;
        }

        // Mark as being processed
        rec_stack.insert(canonical.clone());
        import_chain.push(canonical.clone());

        // Parse file
        match self.parse(&canonical, config) {
            Ok(result) => {
                // Extract imports
                let imports = self.extract_imports(&result, &canonical, config);
                
                // Recursively visit all dependencies first (DFS)
                for import in &imports {
                    self.dfs_visit(
                        import.clone().into(),
                        module,
                        visited,
                        rec_stack,
                        import_chain,
                        order,
                        config,
                    );
                }
                
                let start_time = std::time::Instant::now();
                // Post-order: process current module after all dependencies
                let file_id = result.module.file;
                order.push(file_id); // Track order
                module.push(ModuleDeps {
                    file_id,
                    name: file_path.name,
                    module: result.module,
                    path: canonical.clone(),
                    deps: imports,
                });

                // Mark as fully processed
                visited.insert(canonical.clone());
                self.update_timings(config, start_time);
            }
            Err(err) => {
                self.report_error(err.to_string());
            }
        }

        // Remove from recursion stack
        import_chain.pop();
        rec_stack.remove(&canonical);
    }

    fn parse(&mut self, path: &Path, config: &mut Config) -> Result<ParserResult, ResolveError> {
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

        let start_time = std::time::Instant::now();
        // Tokenize and parse
        let file_id = self.context.add_file(
            path.display().to_string(),
            &content,
        );
        self.update_timings(config, start_time);

        let Some(content) = self.context.get_file(file_id) else {
            return Err(ResolveError::FileNotFound { path: path.to_path_buf() });
        };
        
        let start_time_lexer = std::time::Instant::now();
        let mut lexer = Lexer::new(&mut config.str_pool, content, file_id, self.reporter);
        let lexer_result = lexer.tokenize();
        config.timings.entry(crate::config::Timings::Lexer)
            .and_modify(|e| *e += start_time_lexer.elapsed())
            .or_insert(start_time_lexer.elapsed());
        
        let start_time_parser = std::time::Instant::now();
        let mut parser = Parser::new(content, &mut config.str_pool, lexer_result, self.reporter);
        let result = parser.parse();
        config.timings.entry(crate::config::Timings::Parser)
            .and_modify(|e| *e += start_time_parser.elapsed())
            .or_insert(start_time_parser.elapsed());
        
        Ok(result)
    }

    fn extract_imports(
        &mut self,
        result: &ParserResult,
        current_file: &Path,
        config: &mut Config,
    ) -> Vec<Import> {
        let mut imports = Vec::new();

        for declaration in &result.module.declaration {
            if let DeclarationKind::Import(import) = &declaration.kind {
                match self.resolve(&import.path, current_file, config) {
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
        config: &mut Config,
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
            self.resolve_absolute(import_path, config)?
        } else {
            // Package import (search in include_dirs)
            self.resolve_package(import_path, config)?
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

    fn resolve_absolute(&mut self, import_path: &str, config: &mut Config) -> Result<PathBuf, ResolveError> {
        let import_path = import_path.trim_start_matches('/');
        let mut resolved = config.current_dir.join(import_path);

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

    fn resolve_package(&mut self, package_name: &str, config: &mut Config) -> Result<PathBuf, ResolveError> {
        let current_dir = config.current_dir.clone();
        // search in current dir
        let as_file = current_dir.join(package_name).with_extension("tahu");
        if as_file.exists() {
            return Ok(as_file);
        }

        // Search dalam include_dirs (dari -I flags)
        for include_dir in &config.include_dirs {
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
            search_paths: config.include_dirs.clone(),
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
        self.reporter.report(Diagnostic::fatal(msg));
    }
}
