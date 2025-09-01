use tahuc_ast::{
    Module,
    nodes::declarations::{Declaration, DeclarationKind},
};

use crate::{
    database::Database, error::SemanticError, symbol::{FunctionSymbol, ParameterInfo, Symbol, SymbolKind, VariableSymbol}
};

pub struct Collector<'a> {
    db: &'a mut Database,
}

impl<'a> Collector<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();
        for declaration in &module.declaration {
            self.collect_declaration(declaration);
        }
    }

    fn collect_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                let result = self.db.add_symbol(Symbol {
                    name: func.kind.name.clone(),
                    span: func.span,
                    kind: SymbolKind::Function(FunctionSymbol {
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
                    }),
                });

                match result {
                    Err(_) => {
                        let prev_name = self.db.lookup_symbol(func.kind.name.clone()).unwrap().name;
                        let prev_span = self.db.lookup_symbol(func.kind.name.clone()).unwrap().span;
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
                    }),
                };
                let result = self.db.add_symbol(symbol);

                match result {
                    Err(_) => {
                        let prev_name = self.db.lookup_symbol(extern_func.name.clone()).unwrap().name;
                        let prev_span = self.db.lookup_symbol(extern_func.name.clone()).unwrap().span;
                        self.db.report_error(SemanticError::Duplicate {
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

                let result = self.db.add_symbol(symbol);
                match result {
                    Err(_) => {
                        let prev_name = self.db.lookup_symbol(var.name.clone()).unwrap().name;
                        let prev_span = self.db.lookup_symbol(var.name.clone()).unwrap().span;
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
}
