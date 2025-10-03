use std::{collections::HashMap, path::PathBuf, process::Command, time::Instant};

use tahuc_codegen_llvm::{Codegen};
use tahuc_diagnostics::{
    context::DiagnosticContext,
    emitter::terminal::stderr_emitter,
    reporter::DiagnosticReporter,
};
use tahuc_hir::{hir::HirModule, Hir};
use tahuc_module::{config::Config, resolver::{ModuleResolver, ModuleResult}};
use tahuc_semantic::{database::Database, Analyzer};
use tahuc_span::FileId;

use crate::cmd::parse_command;

mod cmd;

fn report_error(reporter: DiagnosticReporter, context: DiagnosticContext) {
    let mut terminal_emitter = stderr_emitter();
    reporter.emit_all(&mut terminal_emitter, &context);
    // terminal_emitter.emit_summary(reporter.clone());
    if reporter.has_errors() {
        std::process::exit(1);
    }
}

fn build_hir(db: &mut Database, module: &HashMap<FileId, ModuleResult>) -> HashMap<FileId, (HirModule, PathBuf)> {
    let mut hir = Hir::new(db);
    let module = hir.to_hir(&module);
    module
}

fn build_middle(config: &Config, modules: &HashMap<FileId, (HirModule, PathBuf)>) {
    let mut mir_builder = tahuc_mir::builder::builder::Builder::new();
    let context = Codegen::context();


    for (_, (module, path_buf)) in modules {
        let mir = mir_builder.build_module(module);
        let path = path_buf.to_str().unwrap().to_string();

        let mut codegen = Codegen::new(context, path.clone());
        codegen.compile_module(&mir);

        codegen.verify();
        if !config.is_test {
            let path = replace_extension(&path_buf, "o");
            codegen.write_object_file(&path);
        }
    }
}

fn linker(config: Config, modules: HashMap<FileId, ModuleResult>) {
    let extension = get_executable_name();

    for file in config.files {
        let mut cmd = Command::new("gcc");
        let exe = replace_extension(&file.path, &extension);

        if let Some(output) = &config.output_name {
            cmd.arg("-o");
            cmd.arg(output);
        } else {
            cmd.arg("-o");
            cmd.arg(exe);
        }

        let entry = replace_extension(&file.path, "o");
        cmd.arg(entry);

        for (_, module) in &modules {
            if module.name == file.name {
                continue;
            }
            let obj_import = replace_extension(&module.path, "o");
            cmd.arg(obj_import);
        }

        for lib_dir in &config.link_dir {
            cmd.arg("-L");
            cmd.arg(lib_dir);
        }

        for lib_file in &config.linking {
            cmd.arg("-l");
            cmd.arg(lib_file);
        }

        println!("Linking with command: {:?}", cmd);

        let output = cmd
            .output()
            .map_err(|e| format!("Failed to execute linker: {}", e));

        match output {
            Ok(output) => {
                if !output.status.success() {
                    println!("{}", format!(
                        "Linking failed: {}",
                        String::from_utf8_lossy(&output.stderr)
                    ));
                }
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}

fn get_executable_name() -> String {
    #[cfg(target_os = "windows")]
    {
        return "exe".to_string();
    }

    "".to_string()
}

fn replace_extension(path: &PathBuf, new_ext: &str) -> String {
    let mut binding = path.clone();
    binding.set_extension(new_ext);
    pretty_path(&binding)
}

fn pretty_path(path: &PathBuf) -> String {
    let s = path.display().to_string();
    if cfg!(windows) {
        s.trim_start_matches(r"\\?\").to_string()
    } else {
        s
    }
}


pub fn main() {
    let start_time = Instant::now();
    let config = parse_command();

    let mut context = DiagnosticContext::new();
    let mut reporter = DiagnosticReporter::new();
    
    let start_module = Instant::now();
    let mut module_resolver = ModuleResolver::new(&config, &mut context, &mut reporter);
    let results = module_resolver.parse_modules();
    println!("Module resolution took {:?}", start_module.elapsed());

    let mut analyzer = Analyzer::new(&mut reporter);
    let mut db = analyzer.analyze_depedencies(&results);

    report_error(reporter, context);

    let hir = build_hir(&mut db, &results);

    build_middle(&config, &hir);

    if !config.is_test {
        linker(config, results);
    }

    println!("Compilation success {:?}", start_time.elapsed());
}