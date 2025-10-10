use std::{collections::HashMap, path::PathBuf, process::Command, time::Instant};

use tahuc_codegen_llvm::{Codegen};
use tahuc_diagnostics::{
    context::DiagnosticContext,
    emitter::terminal::stderr_emitter,
    reporter::DiagnosticReporter,
};
use tahuc_hir::{hir::HirModule, HirBuilder};
use tahuc_mir::builder::builder::Builder;
use tahuc_module::{config::Config, resolver::{ModuleResolver, ModuleResult}};
use tahuc_semantic::{database::database::Database, Analyzer};
use tahuc_span::{FileId, StrPool};

use crate::cmd::parse_command;

mod cmd;

fn report_error(reporter: &mut DiagnosticReporter, context: &mut DiagnosticContext) {
    let mut terminal_emitter = stderr_emitter();
    reporter.emit_all(&mut terminal_emitter, &context);
    // terminal_emitter.emit_summary(reporter.clone());
    if reporter.has_errors() {
        std::process::exit(1);
    }
}

fn build_hir(config: &mut Config, db: &mut Database, module: &HashMap<FileId, ModuleResult>) -> HashMap<FileId, (HirModule, PathBuf)> {
    let start_time = Instant::now();
    let mut hir = HirBuilder::new(&config, db);
    let module = hir.to_hir(&module);
    // tahuc_hir::printer::HirPrinter::new(&config).print_program(&module);
    config.timings.insert(tahuc_module::config::Timings::Hir, start_time.elapsed());
    module
}

fn build_middle(config: &mut Config, modules: &HashMap<FileId, (HirModule, PathBuf)>) {
    let mut mir_builder = Builder::new();
    let context = Codegen::context();


    for (_, (module, path_buf)) in modules {
        let start_time_mir = Instant::now();
        let mir = mir_builder.build_module(config, module);
        config.timings.entry(tahuc_module::config::Timings::Mir)
            .and_modify(|e| *e += start_time_mir.elapsed())
            .or_insert(start_time_mir.elapsed());
        // tahuc_mir::printer::print_mir(&config.str_pool, &mir);
        
        let path = path_buf.to_str().unwrap().to_string();

        let start_time_codegen = Instant::now();
        let mut codegen = Codegen::new(context, path);
        codegen.compile_module(config, &mir);
        config.timings.entry(tahuc_module::config::Timings::Llvm)
            .and_modify(|e| *e += start_time_codegen.elapsed())
            .or_insert(start_time_codegen.elapsed());

        // println!("{}", codegen.to_string());

        codegen.verify();
        if !config.is_test {
            let start_time = Instant::now();
            let path = replace_extension(&path_buf, "o");
            codegen.write_object_file(&config, &path);
            config.timings.entry(tahuc_module::config::Timings::Write)
                .and_modify(|e| *e += start_time.elapsed())
                .or_insert(start_time.elapsed());
        }
    }
}

fn linker(config: &mut Config, modules: HashMap<FileId, ModuleResult>) {
    let start_time = Instant::now();

    let extension = get_executable_name();

    for file in &config.files {
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

        config.timings.entry(tahuc_module::config::Timings::Linking)
            .and_modify(|e| *e += start_time.elapsed())
            .or_insert(start_time.elapsed());
    }
}

fn get_executable_name() -> String {
    if cfg!(windows) {
        return "exe".to_string();
    } else {
        "".to_string()
    }
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
    let mut pool = StrPool::default();
    let mut config = parse_command(&mut pool);

    let mut context = DiagnosticContext::new();
    let mut reporter = DiagnosticReporter::new();
    
    let start_module = Instant::now();
    let mut module_resolver = ModuleResolver::new(&mut context, &mut reporter);
    let results = module_resolver.parse_modules(&mut config);
    println!("Module resolution took {:?}", start_module.elapsed());
    report_error(&mut reporter, &mut context);

    let start_analyzer = Instant::now();
    let mut analyzer = Analyzer::new(&mut reporter);
    let mut db = analyzer.analyze_depedencies(&mut config, &results);
    config.timings.insert(tahuc_module::config::Timings::SemanticTotal, start_analyzer.elapsed());

    // db.print_debug();

    report_error(&mut reporter, &mut  context);

    let hir = build_hir(&mut config, &mut db, &results);

    let start_middle = Instant::now();
    build_middle(&mut config, &hir);
    println!("Build middle took {:?}", start_middle.elapsed());

    if !config.is_test {
        linker(&mut config, results);
    }

    config.timings.insert(tahuc_module::config::Timings::Total, start_time.elapsed());

    // config.print_timings();
    println!("Compilation success");
}