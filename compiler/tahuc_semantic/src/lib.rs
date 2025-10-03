use std::collections::HashMap;

use tahuc_ast::Module;
use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_module::resolver::ModuleResult;
use tahuc_span::FileId;

use crate::{
    database::Database,
    resolver::{
        collector::Collector, control_flow::ControlFlowAnalyzer, symbol_resolver::SymbolResolution,
        type_resolver::TypeAnalyzer,
    },
};

pub mod database;
mod error;
mod resolver;
mod scope;
pub mod symbol;
mod type_manager;

pub struct Analyzer<'a> {
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

    pub fn analyze_depedencies(&mut self, modules: &HashMap<FileId, ModuleResult>) -> Database {
        // phase 1 collect all ymbols, type
        for (file_id, _) in modules {
            self.database.add_file(*file_id);
        }
        self.collector(modules);

        // phase 2 resolve symbol reference
        self.resolve_reference(&modules);

        for (_, res) in modules {
            // phase 3 type checking
            self.type_checking(&res.module);

            // phase 4 control flow analyze
            self.control_flow_analyze(&res.module);
        }

        // collect error and report
        self.compile_error();

        self.database.clone()
    }

    fn collector(&mut self, modules: &HashMap<FileId, ModuleResult>) {
        let mut collector = Collector::new(&mut self.database);
        collector.analyze_module(modules);
    }

    fn resolve_reference(&mut self, module: &HashMap<FileId, ModuleResult>) {
        let mut resolver = SymbolResolution::new(&mut self.database);
        resolver.analyze_module(module);
    }

    fn type_checking(&mut self, module: &Module) {
        let mut resolver = TypeAnalyzer::new(&mut self.database);
        resolver.analyze_module(module);
    }

    fn control_flow_analyze(&mut self, module: &Module) {
        let mut analyzer = ControlFlowAnalyzer::new(&mut self.database);
        analyzer.analyze_module(module);
    }

    fn compile_error(&mut self) {
        self.database.get_errors().iter().for_each(|s| {
            self.reporter.report(s.to_diagnostic());
        });
    }
}
