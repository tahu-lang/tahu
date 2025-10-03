use std::path::PathBuf;

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
pub struct Config {
    pub compiler_dir: PathBuf,

    /// Locate file path
    pub current_dir: PathBuf,

    /// Input files .tahu
    pub files: Vec<FileInput>,

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
}

impl Config {
    pub fn new() -> Self {
        Self {
            compiler_dir: PathBuf::new(),
            current_dir: PathBuf::new(),
            files: Vec::new(),
            output_name: None,
            include_dirs: Vec::new(),

            // linking
            link_dir: Vec::new(),
            linking: Vec::new(),

            // compiler options
            opt: OptLevel::default(),
            output: Output::default(),

            is_test: false,
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
}
