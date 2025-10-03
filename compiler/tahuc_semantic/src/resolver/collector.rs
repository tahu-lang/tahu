use std::collections::{HashMap, VecDeque};

use tahuc_ast::{
    nodes::declarations::{Declaration, DeclarationKind},
    ty::Type,
};
use tahuc_module::resolver::ModuleResult;
use tahuc_span::{FileId, Span};

use crate::{
    database::Database,
    error::SemanticError,
    symbol::{
        FunctionSymbol, ParameterInfo, StructFieldSymbol, StructSymbol, Symbol, SymbolKind,
        VariableSymbol,
    },
};

pub struct Collector<'a> {
    db: &'a mut Database,
    file_id: FileId,
    pendings: VecDeque<Declaration>,
}

impl<'a> Collector<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            file_id: FileId(0),
            pendings: VecDeque::new(),
        }
    }

    pub fn analyze_module(&mut self, modules: &HashMap<FileId, ModuleResult>) {
        for (file_id, result) in modules {
            self.db.reset_scope();
            self.file_id = *file_id;
            for declaration in &result.module.declaration {
                self.collect_declaration(declaration);
            }
        }

        self.resolve_pending();
    }

    fn collect_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Struct(struct_decl) => {
                for field in &struct_decl.fields {
                    if field.kind.r#type.is_named() {
                        self.pendings.push_back(declaration.clone());
                        return;
                    } else {
                        self.add_type(field.id, field.kind.r#type.clone());
                    }
                }
                let mut fields = Vec::new();
                let mut fields_ty = Vec::new();
                for field in &struct_decl.fields {
                    let ty = self.get_type(field.id).unwrap().clone();
                    fields.push(StructFieldSymbol {
                        name: field.kind.name.clone(),
                        r#type: ty.clone(),
                        span: field.span,
                    });
                    fields_ty.push((field.kind.name.clone(), ty));
                }

                let final_ty = Type::Struct {
                    name: struct_decl.name.clone(),
                    fields: fields_ty,
                };
                self.add_type(declaration.id, final_ty);

                let result = self.add_symbol(Symbol {
                    name: struct_decl.name.clone(),
                    span: struct_decl.span,
                    kind: SymbolKind::Struct(StructSymbol {
                        file_id: self.file_id,
                        name: struct_decl.name.clone(),
                        fields: fields,
                        visibility: struct_decl.visibility.clone(),
                        ty: struct_decl.ty.clone(),
                        span: struct_decl.span,
                    }),
                });
                if let Err(_) = result {
                    self.add_error(struct_decl.name.clone(), struct_decl.span);
                }
            }
            DeclarationKind::Fn(func) => {
                for param in &func.kind.parameters {
                    if param.kind.r#type.is_named() {
                        self.pendings.push_back(declaration.clone());
                        return;
                    } else {
                        self.add_type(param.id, param.kind.r#type.clone());
                    }
                }
                if func.kind.return_type.is_named() {
                    self.pendings.push_back(declaration.clone());
                    return;
                } else {
                    self.add_type(declaration.id, func.kind.return_type.clone());
                }
                let mut params = Vec::new();
                for param in &func.kind.parameters {
                    let ty = self.get_type(param.id).unwrap().clone();
                    params.push(ParameterInfo {
                        name: param.kind.name.clone(),
                        var_type: ty,
                        span: param.span,
                    });
                }
                let ret_ty = self.get_type(declaration.id).unwrap().clone();
                let result = self.add_symbol(Symbol {
                    name: func.kind.name.clone(),
                    span: func.span,
                    kind: SymbolKind::Function(FunctionSymbol {
                        file_id: self.file_id,
                        name: func.kind.name.clone(),
                        parameters: params,
                        return_type: ret_ty,
                        visibility: func.kind.visibility.clone(),
                        span: func.span,
                    }),
                });

                if let Err(_) = result {
                    self.add_error(func.kind.name.clone(), func.span);
                }
            }
            DeclarationKind::Extern(extern_func) => {
                for param in &extern_func.parameters {
                    self.add_type(param.id, param.kind.r#type.clone());
                }
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

                if let Err(_) = result {
                    self.add_error(extern_func.name.clone(), extern_func.span);
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
                        is_mutable: true,

                        need_inferred: var.variable_type.is_inferred(),
                        inferred_type: None,
                        final_type: var.variable_type.clone(),
                    }),
                };

                let result = self.add_symbol(symbol);
                if let Err(_) = result {
                    self.add_error(var.name.clone(), var.span);
                }
            }
            _ => {}
        }
    }

    fn resolve_pending(&mut self) {
        let mut max = 100 + self.pendings.len();
        while let Some(declaration) = self.pendings.pop_front() {
            self.file_id = declaration.file_id;
            self.resolve_declaration(&declaration);
            max -= 1;
            if max == 0 {
                break;
            }
        }
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Struct(struct_decl) => {
                let mut fields = Vec::new();
                let mut final_ty = Vec::new();

                struct_decl.fields.iter().for_each(|field| {
                    if field.kind.r#type.is_named() {
                        if let Some(symbol) =
                            self.lookup_symbol(field.kind.r#type.get_ty_named().unwrap())
                        {
                            fields.push(StructFieldSymbol {
                                name: field.kind.name.clone(),
                                r#type: symbol.get_type(),
                                span: field.span,
                            });
                            final_ty.push((field.kind.name.clone(), symbol.get_type()))
                        } else {
                            self.pendings.push_back(declaration.clone());
                            return;
                        }
                    } else {
                        fields.push(StructFieldSymbol {
                            name: field.kind.name.clone(),
                            r#type: field.kind.r#type.clone(),
                            span: field.span,
                        });
                        final_ty.push((field.kind.name.clone(), field.kind.r#type.clone()))
                    }
                });

                let ty = Type::Struct {
                    name: struct_decl.name.clone(),
                    fields: final_ty,
                };
                self.add_type(declaration.id, ty.clone());

                let struct_symbol = StructSymbol {
                    file_id: self.file_id,
                    name: struct_decl.name.clone(),
                    fields: fields,
                    visibility: struct_decl.visibility.clone(),
                    ty,
                    span: struct_decl.span,
                };

                let result = self.add_symbol(Symbol {
                    name: struct_decl.name.clone(),
                    span: struct_decl.span,
                    kind: SymbolKind::Struct(struct_symbol),
                });
                if let Err(_) = result {
                    self.add_error(struct_decl.name.clone(), struct_decl.span);
                }
            }
            DeclarationKind::Fn(func) => {
                let mut parameters = Vec::new();
                let mut final_ty = Vec::new();
                func.kind.parameters.iter().for_each(|param| {
                    if param.kind.r#type.is_named() {
                        if let Some(symbol) =
                            self.lookup_symbol(param.kind.r#type.get_ty_named().unwrap())
                        {
                            parameters.push(ParameterInfo {
                                name: param.kind.name.clone(),
                                var_type: symbol.get_type(),
                                span: param.span,
                            });
                            self.add_type(param.id, symbol.get_type());
                            final_ty.push((param.kind.name.clone(), symbol.get_type()))
                        } else {
                            self.pendings.push_back(declaration.clone());
                            return;
                        }
                    } else {
                        parameters.push(ParameterInfo {
                            name: param.kind.name.clone(),
                            var_type: param.kind.r#type.clone(),
                            span: param.span,
                        });
                        self.add_type(param.id, param.kind.r#type.clone());
                        final_ty.push((param.kind.name.clone(), param.kind.r#type.clone()))
                    }
                });

                if func.kind.return_type.is_named() {
                    let ty = func.kind.return_type.get_ty_named().unwrap();
                    if let Some(symbol) = self.lookup_symbol(ty) {
                        self.add_type(declaration.id, symbol.get_type());
                    } else {
                        self.pendings.push_back(declaration.clone());
                        return;
                    }
                }

                self.add_type(declaration.id, func.kind.return_type.clone());

                let ret_ty = self.get_type(declaration.id).unwrap().clone();

                let result = self.add_symbol(Symbol {
                    name: func.kind.name.clone(),
                    span: func.span,
                    kind: SymbolKind::Function(FunctionSymbol {
                        file_id: self.file_id,
                        name: func.kind.name.clone(),
                        parameters: parameters,
                        return_type: ret_ty,
                        visibility: func.kind.visibility.clone(),
                        span: func.span,
                    }),
                });

                if let Err(_) = result {
                    self.add_error(func.kind.name.clone(), func.span);
                }
            }
            _ => {}
        }
    }

    fn add_error(&mut self, name: String, span: Span) {
        let symbol = self.lookup_symbol(name.clone()).unwrap();
        let prev_name = symbol.name;
        let prev_span = symbol.span;
        self.db.report_error(SemanticError::Duplicate {
            name,
            span,
            previous: prev_name,
            previous_span: prev_span,
        });
    }

    fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        self.db.add_symbol(self.file_id, symbol)
    }

    fn lookup_symbol(&mut self, name: String) -> Option<Symbol> {
        self.db.lookup_symbol(self.file_id, name)
    }

    fn add_type(&mut self, id: u32, ty: Type) {
        self.db.add_type(self.file_id, id, ty);
    }

    fn get_type(&mut self, id: u32) -> Option<&Type> {
        self.db.get_type(self.file_id, id)
    }
}
