// In error.rs
use std::path::PathBuf;

#[derive(Debug)]
pub enum ResolveError {
    FailedToReadFile {
        path: PathBuf,
        error: String,
    },
    InvalidUtf8 {
        path: PathBuf,
        error: String,
    },
    InvalidPath {
        path: PathBuf,
    },
    FileNotFound {
        path: PathBuf,
    },
    DirectoryWithoutEntry {
        path: PathBuf,
    },
    PackageNotFound {
        name: String,
        search_paths: Vec<String>,
    },
    CircularImport(Vec<PathBuf>),
}

fn pretty_path(path: &PathBuf) -> String {
    let s = path.display().to_string();
    if cfg!(windows) {
        s.trim_start_matches(r"\\?\").to_string()
    } else {
        s
    }
}

fn pretty_path_from_string(path: &str) -> String {
    let path = PathBuf::from(path);
    pretty_path(&path)
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::FailedToReadFile { path, error } => {
                write!(f, "Failed to read file `{}`: {}", pretty_path(path), error)
            }
            ResolveError::InvalidUtf8 { path, error } => {
                write!(f, "File `{}` contains invalid UTF-8: {}", pretty_path(path), error)
            }
            ResolveError::InvalidPath { path } => {
                write!(f, "Invalid path: {}", pretty_path(path))
            }
            ResolveError::FileNotFound { path } => {
                write!(f, "Module not found: {}", pretty_path(path))
            }
            ResolveError::DirectoryWithoutEntry { path } => {
                write!(
                    f,
                    "Directory `{}` requires an entry point (index.tahu, lib.tahu, or main.tahu)",
                    pretty_path(path)
                )
            }
            ResolveError::PackageNotFound { name, search_paths } => {
                write!(f, "Package `{}` not found in search paths:", name)?;
                for path in search_paths {
                    write!(f, "\n  - {}", pretty_path_from_string(path))?;
                }
                write!(f, "\n\nTry adding the package directory with -I flag")
            }
            ResolveError::CircularImport(chain) => {
                write!(f, "Circular import detected:")?;
                for (i, path) in chain.iter().enumerate() {
                    write!(f, "\n  ")?;
                    if i > 0 {
                        write!(f, "-> ")?;
                    }
                    write!(f, "{}", pretty_path(path))?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for ResolveError {}