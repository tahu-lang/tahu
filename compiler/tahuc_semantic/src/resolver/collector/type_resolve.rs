use std::rc::Rc;

use tahuc_ast::{nodes::{ast::{AstNode, NodeId}, declarations::{Declaration, DeclarationKind, ExternFn, FunctionKind, Struct}}, ty::Type};
use tahuc_span::{Span, StrId};

use crate::{database::symbol::Symbol, error::SemanticError, resolver::collector::collector::Collector};

impl<'a> Collector<'a> {
    pub(crate) fn resolve_type(&mut self, declaration: &'a Declaration) {
        match &declaration.kind {
            DeclarationKind::Struct(struct_decl) => self.resolve_struct_type(declaration.id, struct_decl),
            DeclarationKind::Fn(func) => self.resolve_function_type(declaration.id, func),
            DeclarationKind::Extern(extern_func) => self.resolve_extern_function_type(declaration.id, extern_func),
            _ => {}
        }
    }

    fn resolve_struct_type(&mut self, id: NodeId, struct_decl: &Struct) {
        let mut fields = Vec::with_capacity(struct_decl.fields.len());
        for (i, field) in struct_decl.fields.iter().enumerate() {
            let ty = field.kind.r#type.clone();
            let span = field.span;
            if let Ok(result) = self.resolve_type_named(ty, span) {
                self.update_symbol_db(id, |symbol| {
                    symbol.update_struct(|struct_symbol| {
                        struct_symbol.update_field_at(i, |f| {
                            f.r#type = result.clone();
                        });
                    });
                });

                self.set_type(field.id, result.clone());
                fields.push((field.kind.name, result.clone()));
                continue;
            }
        }

        let final_ty = Rc::new(Type::Struct {
            name: struct_decl.name,
            fields: Rc::new(fields),
        });

        self.update_symbol_db(id, |symbol| {
            symbol.update_struct(|struct_symbol| {
                struct_symbol.ty = final_ty.clone();
            });
        });

        self.set_type(id, final_ty);
    }

    fn resolve_function_type(&mut self, id: NodeId, func: &AstNode<FunctionKind>) {
        let mut parameters = Vec::new();
        for (i, param) in func.kind.parameters.iter().enumerate() {
            let ty = param.kind.r#type.clone();
            let span = param.span;
            if let Ok(result) = self.resolve_type_named(ty, span) {
                self.update_symbol_db(id, |symbol| {
                    symbol.update_function(|function| {
                        function.update_parameter_at(i, |paramater| {
                            paramater.var_type = result.clone();
                        });
                    });
                });

                self.set_type(param.id, result.clone());
                parameters.push((param.kind.name, result));
                continue;
            }
        }

        let ty = func.kind.return_type.clone();
        let span = func.span;
        if let Ok(result) = self.resolve_type_named(ty, span) {
            self.update_symbol_db(id, |symbol| {
                symbol.update_function(|function| {
                    function.return_type = result.clone();
                });
            });
            self.set_type(id, result);
        }
    }

    fn resolve_extern_function_type(&mut self, id: NodeId, extern_func: &ExternFn) {
        let mut parameters = Vec::new();
        for (i, param) in extern_func.parameters.iter().enumerate() {
            let ty = param.kind.r#type.clone();
            let span = param.span;
            if let Ok(result) = self.resolve_type_named(ty, span) {
                self.update_symbol_db(id, |symbol| {
                    symbol.update_function(|function| {
                        function.update_parameter_at(i, |paramater| {
                            paramater.var_type = result.clone();
                        });
                    });
                });

                self.set_type(param.id, result.clone());
                parameters.push((param.kind.name, result.clone()));
                continue;
            }
        }

        let ty = extern_func.return_type.clone();
        let span = extern_func.span;
        if let Ok(result) = self.resolve_type_named(ty, span) {
            self.update_symbol_db(id, |symbol| {
                symbol.update_function(|function| {
                    function.return_type = result.clone();
                });
            });
            self.set_type(id, result);
        }
    }

    fn resolve_type_named(&mut self, ty: Rc<Type>, span: Span) -> Result<Rc<Type>, ()> {
        match &*ty {
            Type::Named(name) => {
                if let Ok((_, result)) = self.lookup_type(*name, span) {
                    return Ok(result);
                }
                Err(())
            }
            Type::Pointer(inner) => {
                Ok(Rc::new(Type::Pointer(self.resolve_type_named(inner.clone(), span)?)))
            }
            Type::Array { ty, size } => {
                Ok(Rc::new(Type::Array {
                    ty: self.resolve_type_named(ty.clone(), span)?,
                    size: *size,
                }))
            }
            Type::Struct { name, fields } => {
                let mut new_fields = Vec::new();
                for (name, ty) in fields.iter() {
                    new_fields.push((name.clone(), self.resolve_type_named(ty.clone(), span)?));
                }
                Ok(Rc::new(Type::Struct {
                    name: *name,
                    fields: Rc::new(new_fields),
                }))
            }
            _ => Ok(ty)
        }
    }

    fn lookup_type(&mut self, name: StrId, span: Span) -> Result<(StrId, Rc<Type>), ()> {
        // lookup struct, 
        // enum, type alias (future feature)
        let structs = self.db.lookup_struct_with_import(self.file_id, name, false);

        if structs.len() == 1 {
            let symbol = structs[0].clone();
            if self.file_id != symbol.file_id {
                if !structs[0].visibility.is_public() {
                    let name = self.str_pool.get(symbol.name);
                    self.db.report_error(SemanticError::PrivateStruct {
                        name: name.to_string(),
                        span: symbol.span,
                    });
                    return Err(());
                }
            }

            return Ok((name, structs[0].ty.clone()));
        } else if structs.len() > 1 {
            self.db.report_error(SemanticError::AmbiguousType {
                name: self.str_pool.get(name).to_string(),
                span: span,
                related: structs
                    .iter()
                    .map(|s| (s.span, self.str_pool.get(s.name).to_string()))
                    .collect(),
            });
            Err(())
        } else {
            let name = self.str_pool.get(name);
            self.db.report_error(SemanticError::Raw {
                message: format!("Cannot resolve type `{}`", name),
                span: span,
            });
            Err(())
        }
    }

    pub fn update_symbol_db<F>(&mut self, id: NodeId, f: F)
    where
        F: FnMut(&mut Symbol)
    {
        self.db.update_symbol_db(self.file_id, id, f);
    }
}