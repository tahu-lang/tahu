use tahuc_ast::{
    Module,
    nodes::declarations::{Declaration, DeclarationKind},
};
use tahuc_span::FileId;

use crate::{
    database::Database, error::SemanticError, symbol::{FunctionSymbol, ParameterInfo, Symbol, SymbolKind, VariableSymbol}
};

pub struct Collector<'a> {
    db: &'a mut Database,
    file_id: FileId,
}

impl<'a> Collector<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            file_id: FileId(0),
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();
        self.file_id = module.file;
        for declaration in &module.declaration {
            self.collect_declaration(declaration);
        }
    }

    fn collect_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                let result = self.add_symbol(Symbol {
                    name: func.kind.name.clone(),
                    span: func.span,
                    kind: SymbolKind::Function(FunctionSymbol {
                        file_id: self.file_id,
                        name: func.kind.name.clone(),
                        parameters: func
                            .kind
                            .parameters
                            .iter()
                            .map(|p| ParameterInfo {
                                name: p.kind.name.clone(),
                                var_type: p.kind.r#type.clone(),
                                span: p.span,
                            })
                            .collect(),
                        return_type: func.kind.return_type.clone(),
                        visibility: func.kind.visibility.clone(),
                        span: func.span,
                    }),
                });

                match result {
                    Err(_) => {
                        let prev_name = self.lookup_symbol(func.kind.name.clone()).unwrap().name;
                        let prev_span = self.lookup_symbol(func.kind.name.clone()).unwrap().span;
                        self.db.report_error(SemanticError::Duplicate {
                            name: func.kind.name.clone(),
                            span: func.span,
                            previous: prev_name,
                            previous_span: prev_span,
                        });
                    }
                    Ok(_) => {}
                }
            }
            DeclarationKind::Extern(extern_func) => {
                let symbol = Symbol {
                    name: extern_func.name.clone(),
                    span: extern_func.span,
                    kind: SymbolKind::Function(FunctionSymbol {
                        file_id: self.file_id,
                        name: extern_func.name.clone(),
                        parameters: extern_func
                            .parameters
                            .iter()
                            .map(|p| ParameterInfo {
                                name: p.kind.name.clone(),
                                var_type: p.kind.r#type.clone(),
                                span: p.span,
                            })
                            .collect(),
                        return_type: extern_func.return_type.clone(),
                        visibility: tahuc_ast::nodes::declarations::Visibility::Public,
                        span: extern_func.span,
                    }),
                };
                let result = self.add_symbol(symbol);

                match result {
                    Err(_) => {
                        let prev_name = self.lookup_symbol(extern_func.name.clone()).unwrap().name;
                        let prev_span = self.lookup_symbol(extern_func.name.clone()).unwrap().span;
                        self.db.report_error(SemanticError::DuplicateExternFn {
                            name: extern_func.name.clone(),
                            span: extern_func.span,
                            previous: prev_name,
                            previous_span: prev_span,
                        });
                    }
                    Ok(_) => {}
                }
            }
            DeclarationKind::Variable(var) => {
                let symbol = Symbol {
                    name: var.name.clone(),
                    span: var.span,
                    kind: SymbolKind::Variable(VariableSymbol {
                        name: var.name.clone(),
                        declared_type: var.variable_type.clone(),
                        initializer: None,

                        need_inferred: var.variable_type.is_inferred(),
                        inferred_type: None,
                        final_type: var.variable_type.clone(),
                    }),
                };

                let result = self.add_symbol(symbol);
                match result {
                    Err(_) => {
                        let prev_name = self.lookup_symbol(var.name.clone()).unwrap().name;
                        let prev_span = self.lookup_symbol(var.name.clone()).unwrap().span;
                        self.db.report_error(SemanticError::Duplicate {
                            name: var.name.clone(),
                            span: var.span,
                            previous: prev_name,
                            previous_span: prev_span,
                        });
                    }
                    Ok(_) => {}
                }
            }
            _ => {}
        }
    }

    fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        self.db.add_symbol(self.file_id, symbol)
    }

    fn lookup_symbol(&mut self, name: String) -> Option<Symbol> {
        self.db.lookup_symbol(self.file_id, name)
    }
}
