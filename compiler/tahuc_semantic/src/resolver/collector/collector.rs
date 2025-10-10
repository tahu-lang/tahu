use std::{collections::HashMap, rc::Rc};

use tahuc_ast::{
    nodes::{
        ast::NodeId,
        declarations::{Declaration, DeclarationKind},
    },
    ty::Type,
};
use tahuc_module::{config::Config, resolver::ModuleResult};
use tahuc_span::{FileId, Span, StrId, StrPool};

use crate::{
    database::database::Database,
    database::symbol::{
        FunctionSymbol, ParameterInfo, StructFieldSymbol, StructSymbol, Symbol, VariableSymbol,
    },
    error::SemanticError,
};

pub struct Collector<'a> {
    pub(crate) str_pool: &'a StrPool,
    pub(crate) db: &'a mut Database,
    pub(crate) file_id: FileId,
    pub(crate) pendings_imports: HashMap<FileId, Vec<(FileId, Span)>>,
    pub(crate) imports: HashMap<StrId, (StrId, (FileId, Span))>,
}

impl<'a> Collector<'a> {
    pub fn new(str_pool: &'a StrPool, db: &'a mut Database) -> Self {
        Self {
            str_pool,
            db,
            file_id: FileId(0),
            pendings_imports: HashMap::new(),
            imports: HashMap::new(),
        }
    }

    pub fn analyze_module(&mut self, config: &Config, modules: &'a HashMap<FileId, ModuleResult>) {
        for file_id in &config.module_order {
            self.file_id = *file_id;
            self.pendings_imports.clear();
            self.imports.clear();

            let module = modules.get(file_id).unwrap();

            for declaration in &module.module.declaration {
                self.collect_import(declaration);
            }

            self.resolve_import();

            for declaration in &module.module.declaration {
                if let DeclarationKind::Import(_) = &declaration.kind {
                    continue;
                }
                self.collect_declaration(declaration);
            }
            
            for declaration in &module.module.declaration {
                if let DeclarationKind::Import(_) = &declaration.kind {
                    continue;
                }
                self.resolve_type(declaration);
            }
        }

        // self.resolve_pending();
    }

    fn collect_declaration(&mut self, declaration: &'a Declaration) {
        match &declaration.kind {
            DeclarationKind::Struct(struct_decl) => {
                let structs = self.lookup_struct_with_import(struct_decl.name);
                if structs.len() > 0 {
                    let name = self.str_pool.get(struct_decl.name);

                    self.db.report_error(SemanticError::Ambiguous {
                        name: name.to_string(),
                        span: struct_decl.span,
                        related: structs
                            .iter()
                            .map(|s| (s.span, self.str_pool.get(s.name).to_string()))
                            .collect(),
                    });
                    return;
                }

                let mut fields = Vec::with_capacity(struct_decl.fields.len());
                for field in &struct_decl.fields {
                    fields.push(StructFieldSymbol {
                        id: field.id,
                        name: field.kind.name,
                        r#type: field.kind.r#type.clone(),
                        span: field.span,
                    });
                }

                let struct_symbol = StructSymbol {
                    file_id: self.file_id,
                    id: declaration.id,
                    name: struct_decl.name,
                    fields: struct_decl
                        .fields
                        .iter()
                        .map(|field| StructFieldSymbol {
                            id: field.id,
                            name: field.kind.name,
                            r#type: field.kind.r#type.clone(),
                            span: field.span,
                        })
                        .collect(),
                    visibility: struct_decl.visibility,
                    ty: Rc::clone(&struct_decl.ty),
                    span: struct_decl.span,
                };

                if let Err(_) = self.add_struct(struct_symbol.clone()) {
                    if let Some(exist) = self.lookup_struct(struct_decl.name) {
                        let exist = exist.into();
                        self.add_error(struct_decl.name, struct_decl.span, exist);
                    } else {
                        let name = self.str_pool.get(struct_decl.name);
                        self.db.report_error(SemanticError::Raw {
                            message: format!("Struct `{}` not found", name),
                            span: struct_decl.span,
                        });
                    }
                }

                self.set_symbol(declaration.id, struct_symbol.into());
            }
            DeclarationKind::Fn(func) => {
                let functions = self.lookup_function_with_import(func.kind.name);
                if functions.len() > 0 {
                    let name = self.str_pool.get(func.kind.name);

                    self.db.report_error(SemanticError::Ambiguous {
                        name: name.to_string(),
                        span: func.span,
                        related: functions
                            .iter()
                            .map(|s| (s.span, self.str_pool.get(s.name).to_string()))
                            .collect(),
                    });
                    return;
                }

                let mut params = Vec::new();
                for param in &func.kind.parameters {
                    params.push(ParameterInfo {
                        id: param.id,
                        name: param.kind.name,
                        var_type: param.kind.r#type.clone(),
                        span: param.span,
                    });
                }

                let func_symbol = FunctionSymbol {
                    file_id: self.file_id,
                    id: declaration.id,
                    name: func.kind.name,
                    parameters: params,
                    return_type: func.kind.return_type.clone(),
                    visibility: func.kind.visibility,
                    span: func.span,
                };

                if let Err(_) = self.add_function(func_symbol.clone()) {
                    if let Some(exist) = self.lookup_function(func.kind.name) {
                        let exist = exist.into();
                        self.add_error(func.kind.name, func.span, exist);
                    } else {
                        let name = self.str_pool.get(func.kind.name);
                        self.db.report_error(SemanticError::Raw {
                            message: format!("Function `{}` not found", name),
                            span: func.span,
                        });
                    }
                }

                self.set_symbol(declaration.id, func_symbol.into());
            }
            DeclarationKind::Extern(extern_func) => {
                let functions = self.lookup_function_with_import(extern_func.name);
                if functions.len() > 0 {
                    let name = self.str_pool.get(extern_func.name);

                    self.db.report_error(SemanticError::Ambiguous {
                        name: name.to_string(),
                        span: extern_func.span,
                        related: functions
                            .iter()
                            .map(|s| (s.span, self.str_pool.get(s.name).to_string()))
                            .collect(),
                    });
                    return;
                }

                let func_symbol = FunctionSymbol {
                    file_id: self.file_id,
                    id: declaration.id,
                    name: extern_func.name,
                    parameters: extern_func
                        .parameters
                        .iter()
                        .map(|p| ParameterInfo {
                            id: p.id,
                            name: p.kind.name,
                            var_type: p.kind.r#type.clone(),
                            span: p.span,
                        })
                        .collect(),
                    return_type: extern_func.return_type.clone(),
                    visibility: tahuc_ast::nodes::declarations::Visibility::Private,
                    span: extern_func.span,
                };
                if let Err(_) = self.add_function(func_symbol.clone()) {
                    if let Some(exist) = self.lookup_function(extern_func.name) {
                        let exist = exist.into();
                        self.add_error(extern_func.name, extern_func.span, exist);
                    } else {
                        let name = self.str_pool.get(extern_func.name);
                        self.db.report_error(SemanticError::Raw {
                            message: format!("Function `{}` not found", name),
                            span: extern_func.span,
                        });
                    }
                }

                self.set_symbol(declaration.id, func_symbol.into());
            }
            DeclarationKind::Variable(var) => {
                let variables = self.lookup_variable_with_import(var.name);
                if variables.len() > 0 {
                    let name = self.str_pool.get(var.name);

                    self.db.report_error(SemanticError::Ambiguous {
                        name: name.to_string(),
                        span: var.span,
                        related: variables
                            .iter()
                            .map(|s| (s.span, self.str_pool.get(s.name).to_string()))
                            .collect(),
                    });
                    return;
                }

                let var_symbol = VariableSymbol {
                    file_id: self.file_id,
                    id: declaration.id,
                    name: var.name,
                    declared_type: var.variable_type.clone(),
                    initializer: None,
                    is_mutable: true,
                    visibility: var.visibility,

                    need_inferred: var.variable_type.is_inferred(),
                    inferred_type: None,
                    final_type: var.variable_type.clone(),
                    span: var.span,
                };
                if let Err(_) = self.add_variable(var_symbol.clone()) {
                    if let Some(exist) = self.lookup_variable(var.name) {
                        let exist = exist.into();
                        self.add_error(var.name, var.span, exist);
                    } else {
                        let name = self.str_pool.get(var.name);
                        self.db.report_error(SemanticError::Raw {
                            message: format!("Cannot initialize `{name}` as variable"),
                            span: var.span,
                        });
                    }
                }

                self.set_symbol(declaration.id, var_symbol.into());
            }
            _ => {}
        }
    }

    fn add_error(&mut self, name: StrId, span: Span, symbol: Symbol) {
        let name = self.str_pool.get(name);

        let prev_name = self.str_pool.get(symbol.name);
        let prev_span = symbol.span;
        self.db.report_error(SemanticError::Duplicate {
            name: name.to_string(),
            span,
            previous: prev_name.to_string(),
            previous_span: prev_span,
        });
    }

    fn add_struct(&mut self, struct_symbol: StructSymbol) -> Result<(), String> {
        self.db
            .add_struct(self.file_id, struct_symbol.name, struct_symbol)
    }

    pub(crate) fn lookup_struct(&mut self, name: StrId) -> Option<&StructSymbol> {
        self.db.get_struct_top_level(self.file_id, name)
    }

    pub(crate) fn lookup_struct_with_import(&mut self, name: StrId) -> Vec<StructSymbol> {
        self.db.lookup_struct_with_import(self.file_id, name, true)
    }

    fn add_function(&mut self, func_symbol: FunctionSymbol) -> Result<(), String> {
        self.db.add_fn(self.file_id, func_symbol.name, func_symbol)
    }

    pub(crate) fn lookup_function(&mut self, name: StrId) -> Option<&FunctionSymbol> {
        self.db.get_fn_top_level(self.file_id, name)
    }

    pub(crate) fn lookup_function_with_import(&mut self, name: StrId) -> Vec<FunctionSymbol> {
        self.db.lookup_fn_with_import(self.file_id, name, true)
    }

    fn add_variable(&mut self, var_symbol: VariableSymbol) -> Result<(), String> {
        self.db.add_var(self.file_id, var_symbol.name, var_symbol)
    }

    pub(crate) fn lookup_variable(&mut self, name: StrId) -> Option<&VariableSymbol> {
        self.db.get_var_top_level(self.file_id, name)
    }

    pub(crate) fn lookup_variable_with_import(&mut self, name: StrId) -> Vec<VariableSymbol> {
        self.db.lookup_var_with_import(self.file_id, name, true)
    }

    fn set_symbol(&mut self, id: NodeId, symbol: Symbol) {
        self.db.set_symbol(self.file_id, id, symbol);
    }

    pub(crate) fn set_type(&mut self, id: u32, ty: Rc<Type>) {
        self.db.add_type(self.file_id, id, ty);
    }
}
