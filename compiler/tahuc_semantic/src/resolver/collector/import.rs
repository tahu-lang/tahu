use tahuc_ast::nodes::declarations::{Declaration, DeclarationKind};

use crate::{error::SemanticError, resolver::collector::collector::Collector};

impl<'a> Collector<'a> {
    pub(crate) fn collect_import(&mut self, declaration: &'a Declaration) {
        match &declaration.kind {
            DeclarationKind::Import(import) => {
                let import_file_id = self.db.get_import_file(&import.path).unwrap();
                if import.is_wildcard {
                    self.pendings_imports
                        .entry(self.file_id)
                        .or_default()
                        .push((import_file_id, import.span));
                } else {
                    for field in &import.imports {
                        let original_name = field.kind.name;
                        let name = field.kind.alias.unwrap_or(original_name);
                        if self.imports.contains_key(&name) {
                            let (name_conflict, (_, conflict_span)) =
                                self.imports.get(&name).unwrap();
                            self.db.report_error(SemanticError::ImportRedefined {
                                name: self.str_pool.get(name).to_string(),
                                span: field.span,
                                previous: self.str_pool.get(*name_conflict).to_string(),
                                previous_span: *conflict_span,
                            });
                            continue;
                        }

                        self.imports
                            .insert(name, (original_name, (import_file_id, field.span)));
                    }
                }
            }
            _ => {}
        }
    }

    pub(crate) fn resolve_import(&mut self) {
        self.resolve_import_wildcard();
        self.resolve_import_named();
    }

    fn resolve_import_wildcard(&mut self) {
        let current_file_id = self.file_id;
        for (_, files) in self.pendings_imports.iter() {
            for (file, span) in files {
                if let Some(struct_symbol) = self.db.get_all_struct_top_level(*file).cloned() {
                    let names = struct_symbol
                        .iter()
                        .map(|(name, _)| (*name, (*file, *name)))
                        .collect();
                    if let Err(_) = self.db.add_import_structs(current_file_id, names) {
                        self.db
                            .report_error(SemanticError::ImportWildcardDuplicate { span: *span });
                        continue;
                    }
                }
                if let Some(fn_symbol) = self.db.get_all_fn_top_level(*file).cloned() {
                    let names = fn_symbol
                        .iter()
                        .map(|(name, _)| (*name, (*file, *name)))
                        .collect();
                    if let Err(_) = self.db.add_import_fns(current_file_id, names) {
                        self.db
                            .report_error(SemanticError::ImportWildcardDuplicate { span: *span });
                        continue;
                    }
                }

                if let Some(var_symbol) = self.db.get_all_var_top_level(*file).cloned() {
                    let names = var_symbol
                        .iter()
                        .map(|(name, _)| (*name, (*file, *name)))
                        .collect();
                    if let Err(_) = self.db.add_import_vars(current_file_id, names) {
                        self.db
                            .report_error(SemanticError::ImportWildcardDuplicate { span: *span });
                        continue;
                    }
                }
            }
        }
    }

    fn resolve_import_named(&mut self) {
        let current_file_id = self.file_id;
        for (alias, (ori_name, (file_id, span))) in &self.imports {
            if let Some(struct_symbol) = self.db.get_struct_top_level(*file_id, *ori_name).cloned()
            {
                let visibility = struct_symbol.visibility;
                if let Err(_) = self.db.add_import_struct(
                    current_file_id,
                    *alias,
                    (*file_id, struct_symbol.name),
                ) {
                    self.db.report_error(SemanticError::ImportDuplicate {
                        name: self.str_pool.get(*ori_name).to_string(),
                        span: *span,
                    });
                }
                if !visibility.is_public() {
                    self.db.report_error(SemanticError::PrivateStruct {
                        name: self.str_pool.get(*ori_name).to_string(),
                        span: *span,
                    });
                }
            } else if let Some(fn_symbol) = self.db.get_fn_top_level(*file_id, *ori_name).cloned() {
                let visibility = fn_symbol.visibility;
                if let Err(_) =
                    self.db
                        .add_import_fn(current_file_id, *alias, (*file_id, fn_symbol.name))
                {
                    self.db.report_error(SemanticError::ImportDuplicate {
                        name: self.str_pool.get(*ori_name).to_string(),
                        span: *span,
                    });
                }
                if !visibility.is_public() {
                    self.db.report_error(SemanticError::PrivateFunction {
                        name: self.str_pool.get(*ori_name).to_string(),
                        span: *span,
                    });
                }
            } else if let Some(var_symbol) = self.db.get_var_top_level(*file_id, *ori_name).cloned()
            {
                let visibility = var_symbol.visibility;
                if let Err(_) =
                    self.db
                        .add_import_var(current_file_id, *alias, (*file_id, var_symbol.name))
                {
                    self.db.report_error(SemanticError::ImportDuplicate {
                        name: self.str_pool.get(*ori_name).to_string(),
                        span: *span,
                    });
                }
                if !visibility.is_public() {
                    self.db.report_error(SemanticError::PrivateVariable {
                        name: self.str_pool.get(*ori_name).to_string(),
                        span: *span,
                    });
                }
            } else {
                self.db.report_error(SemanticError::Raw {
                    message: format!(
                        "cannot find import {}",
                        self.str_pool.get(*ori_name).to_string()
                    ),
                    span: *span,
                });
            }
        }
    }
}
