use std::collections::HashMap;

use tahuc_ast::Module;
use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_module::{config::Config, resolver::ModuleResult};
use tahuc_span::FileId;

use crate::{
    database::database::Database,
    resolver::{
        collector::collector::Collector, control_flow::ControlFlowAnalyzer, symbol_resolver::SymbolResolution,
        type_resolver::TypeAnalyzer,
    },
};

pub mod database;
mod error;
mod resolver;

pub struct Analyzer<'a> {
    // config: &'a Config<'a>,
    database: Database,
    reporter: &'a mut DiagnosticReporter,
}

impl<'a> Analyzer<'a> {
    pub fn new(reporter: &'a mut DiagnosticReporter) -> Self {
        Self {
            database: Database::new(),
            reporter: reporter,
        }
    }

    pub fn analyze_depedencies(&mut self, config: &mut Config, modules: &HashMap<FileId, ModuleResult>) -> Database {
        // phase 1 collect all symbols, type
        for (file_id, module) in modules {
            self.database.add_file(*file_id);
            self.database.add_import_name(module.name.clone(), *file_id);
            let size = module.module.declaration.len();
            self.database.allocate_size(*file_id, size);
        }

        self.collector(config, modules);

        // phase 2 resolve symbol reference
        self.resolve_reference(config, &modules);

        for (_, res) in modules {
            // phase 3 type checking
            self.type_checking(config, &res.module);

            // phase 4 control flow analyze
            self.control_flow_analyze(config, &res.module);
        }

        // collect error and report
        self.compile_error();

        self.database.clone()
    }

    fn collector(&mut self, config: &mut Config, modules: &HashMap<FileId, ModuleResult>) {
        let start_time = std::time::Instant::now();
        let mut collector = Collector::new(config.str_pool,&mut self.database);
        collector.analyze_module(config, modules);
        config.timings.entry(tahuc_module::config::Timings::SemanticCollector)
            .and_modify(|e| *e += start_time.elapsed())
            .or_insert(start_time.elapsed());
    }

    fn resolve_reference(&mut self, config: &mut Config, module: &HashMap<FileId, ModuleResult>) {
        let start_time = std::time::Instant::now();

        let mut resolver = SymbolResolution::new(config, &mut self.database);
        resolver.analyze_module(module);

        config.timings.entry(tahuc_module::config::Timings::SemanticSymbolResolver)
            .and_modify(|e| *e += start_time.elapsed())
            .or_insert(start_time.elapsed());
    }

    fn type_checking(&mut self, config: &mut Config, module: &Module) {
        let start_time = std::time::Instant::now();

        let mut resolver = TypeAnalyzer::new(config, &mut self.database);
        resolver.analyze_module(module);

        config.timings.entry(tahuc_module::config::Timings::SemanticTypeResolver)
            .and_modify(|e| *e += start_time.elapsed())
            .or_insert(start_time.elapsed());
    }

    fn control_flow_analyze(&mut self, config: &mut Config, module: &Module) {
        let start_time = std::time::Instant::now();

        let mut analyzer = ControlFlowAnalyzer::new(config, &mut self.database);
        analyzer.analyze_module(module);

        config.timings.entry(tahuc_module::config::Timings::SemanticControlFlow)
            .and_modify(|e| *e += start_time.elapsed())
            .or_insert(start_time.elapsed());
    }

    fn compile_error(&mut self) {
        self.database.get_errors().iter().for_each(|s| {
            self.reporter.report(s.to_diagnostic());
        });
    }
}
