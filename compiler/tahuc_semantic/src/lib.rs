use tahuc_ast::Module;
use tahuc_diagnostics::reporter::DiagnosticReporter;

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

    pub fn analyze_depedencies(&mut self, modules: &Vec<Module>) -> Database {
        // phase 1 collect all ymbols, type
        for module in modules {
            self.database.add_file(module.file);
        }
        self.collector(modules);
        
        for module in modules {
            // phase 2 resolve symbol reference
            self.resolve_reference(module);

            // phase 3 type checking
            self.type_checking(module);
            
            // phase 4 control flow analyze
            self.control_flow_analyze(module);
        }

        // collect error and report
        self.compile_error();

        self.database.clone()
    }

    fn collector(&mut self, modules: &Vec<Module>) {
        let mut collector = Collector::new(&mut self.database);
        collector.analyze_module(modules);
    }

    fn resolve_reference(&mut self, module: &Module) {
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
