use std::{collections::HashMap, path::PathBuf};

use tahuc_span::{FileId, StrPool};

#[derive(Debug, Clone)]
pub enum OptLevel {
    None,
    Low,
    Medium,
    High,
}

impl Default for OptLevel {
    fn default() -> Self {
        OptLevel::Medium
    }
}

#[derive(Debug, Clone)]
pub enum Output {
    Asm,
    Object,
    Executable,
    Mir,
    Llvm,
}

impl Default for Output {
    fn default() -> Self {
        Output::Executable
    }
}

#[derive(Debug, Clone)]
pub struct FileInput {
    pub name: String,
    pub dir: PathBuf,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub enum Os {
    Windows,
    Linux,
    MacOs,
}

impl Os {
    pub fn is_windows(&self) -> bool {
        matches!(self, Os::Windows)
    }

    pub fn is_linux(&self) -> bool {
        matches!(self, Os::Linux)
    }

    pub fn is_macos(&self) -> bool {
        matches!(self, Os::MacOs)
    }
}

#[derive(Debug, Clone)]
pub struct Target {
    pub os: Os,
    pub arch: String,
    pub extension: String,
    pub executable: String,
}

impl Target {
    pub fn new(os: Os, arch: String, extension: String, executable: String) -> Self {
        Self {
            os,
            arch,
            extension,
            executable,
        }
    }

    pub fn os(&self) -> &Os {
        &self.os
    }
}

impl Default for Target {
    fn default() -> Self {
        let os = if cfg!(windows) {
            Os::Windows
        } else if cfg!(target_os = "linux") {
            Os::Linux
        } else if cfg!(target_os = "macos") {
            Os::MacOs
        } else {
            panic!("Unsupported OS");
        };

        let arch = if cfg!(target_arch = "x86_64") {
            "x86_64".to_string()
        } else if cfg!(target_arch = "aarch64") {
            "aarch64".to_string()
        } else {
            panic!("Unsupported architecture");
        };

        let extension = if cfg!(windows) {
            "exe".to_string()
        } else {
            "".to_string()
        };

        let executable = if cfg!(windows) {
            "exe".to_string()
        } else {
            "".to_string()
        };

        Self::new(os, arch, extension, executable)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub enum Timings {
    Lexer = 0,
    Parser = 1,
    ModuleResolver = 2,
    SemanticCollector = 3,
    SemanticSymbolResolver = 4,
    SemanticTypeResolver = 5,
    SemanticControlFlow = 6,
    SemanticTotal = 7,
    Hir = 8,
    Mir = 9,
    Llvm = 10,
    // io
    Write = 11,
    // io
    Linking = 12,
    Total = 13,
}

#[derive(Debug)]
pub struct Config<'a> {
    /// pre initialize before running
    /// 
    /// String interning or string pool
    pub str_pool: &'a mut StrPool,

    /// compiler directory
    pub compiler_dir: PathBuf,

    /// compiler target output
    pub target: Target,

    /// Locate file path
    pub current_dir: PathBuf,

    /// Input files .tahu
    pub files: Vec<FileInput>,

    /// module ordering dfs
    pub module_order: Vec<FileId>,

    /// Output file name -o <filename>
    pub output_name: Option<String>,

    /// Include directory -I/path
    pub include_dirs: Vec<String>,

    /// linking library directory -L/path
    pub link_dir: Vec<String>,

    /// linking library -l/lib
    pub linking: Vec<String>,

    /// Optimization level
    pub opt: OptLevel,

    /// Write output
    pub output: Output,

    /// run test without create object or executable
    pub is_test: bool,

    /// timings of compiler
    pub timings: HashMap<Timings, std::time::Duration>,
}

impl<'a> Config<'a> {
    pub fn new(str_pool: &'a mut StrPool) -> Self {
        Self {
            str_pool: str_pool,
            
            compiler_dir: PathBuf::new(),
            target: Target::default(),
            current_dir: PathBuf::new(),
            files: Vec::new(),
            module_order: Vec::new(),
            output_name: None,
            include_dirs: Vec::new(),

            // linking
            link_dir: Vec::new(),
            linking: Vec::new(),

            // compiler options
            opt: OptLevel::default(),
            output: Output::default(),

            is_test: false,
            timings: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, file: FileInput) {
        self.files.push(file);
    }

    pub fn set_opt(&mut self, opt: OptLevel) {
        self.opt = opt;
    }

    pub fn set_output(&mut self, output: Output) {
        self.output = output;
    }

    pub fn print_timings(&self) {
        // order by key
        let mut order = Vec::new();
        for (key, _) in &self.timings {
            order.push(*key);
        }
        order.sort();
        
        println!("Compiler Timings with IO:");
        for key in order.clone() {
            if key == Timings::Total {
                continue;
            }
            let duration = self.timings.get(&key).unwrap();
            println!("  {:?}: {:?}", key, duration);
        }
        println!("Total: {:?}", self.timings.get(&Timings::Total).unwrap());

        println!("Without IO:");
        let io = vec![
            Timings::Write,
            Timings::Linking,
            Timings::Total,
        ];
        let mut total = std::time::Duration::new(0, 0);
        for key in order {
            if io.contains(&key) {
                continue;
            }
            let Some(duration) = self.timings.get(&key).cloned() else {
                continue;
            };
            total += duration;
        }
        println!("  {:?}: {:?}", Timings::Total, total);
    }
}
