use std::{collections::HashMap, rc::Rc};

use tahuc_ast::{nodes::ast::NodeId, ty::Type};
use tahuc_span::{FileId, StrId};

use crate::{database::{scope::ScopeManager, symbol::{FunctionSymbol, StructSymbol, Symbol, VariableSymbol}, symbol_database::SymbolDatabase, symbol_manager::SymbolManager, type_manager::TypeManager}, error::SemanticError};

#[derive(Debug, Clone)]
pub struct Database {
    // scope manager for entering scope and exit scope
    scope_manager: ScopeManager,

    // import map
    import_map: HashMap<String, FileId>,

    // symbol manager for all information about symbol
    symbol_manager: SymbolManager,

    // all symbol fixed
    symbol_db: SymbolDatabase,

    // type manager, all information about type by node id
    type_manager: TypeManager,

    // files
    files: Vec<FileId>,

    // error reporting
    errors: Vec<SemanticError>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            scope_manager: ScopeManager::new(),
            import_map: HashMap::new(),
            symbol_manager: SymbolManager::new(),
            symbol_db: SymbolDatabase::new(),
            type_manager: TypeManager::new(),
            files: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn allocate_size(&mut self, file_id: FileId, size: usize) {
        self.symbol_manager.allocate_size(file_id, size);
    }

    pub fn report_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    pub fn get_errors(&self) -> &Vec<SemanticError> {
        &self.errors
    }

    pub fn add_file(&mut self, file_id: FileId) {
        self.files.push(file_id);
        self.symbol_manager.add_file(file_id);
    }

    pub fn add_import_name(&mut self, name: String, file_id: FileId) {
        self.import_map.insert(name, file_id);
    }

    pub fn get_import_file(&self, name: &String) -> Option<FileId> {
        self.import_map.get(name).cloned()
    }

    pub fn add_import_struct(&mut self, file_id: FileId, name: StrId, import: (FileId, StrId)) -> Result<(), String> {
        self.symbol_manager.insert_import_struct(file_id, name, import)
    }

    pub fn add_import_structs(&mut self, file_id: FileId, structs: HashMap<StrId, (FileId, StrId)>) -> Result<(), String> {
        self.symbol_manager.insert_import_structs(file_id, structs)
    }

    pub fn add_import_fn(&mut self, file_id: FileId, name: StrId, import: (FileId, StrId)) -> Result<(), String> {
        self.symbol_manager.insert_import_fn(file_id, name, import)
    }

    pub fn add_import_fns(&mut self, file_id: FileId, fns: HashMap<StrId, (FileId, StrId)>) -> Result<(), String> {
        self.symbol_manager.insert_import_fns(file_id, fns)
    }

    pub fn add_import_var(&mut self, file_id: FileId, name: StrId, import: (FileId, StrId)) -> Result<(), String> {
        self.symbol_manager.insert_import_var(file_id, name, import)
    }

    pub fn add_import_vars(&mut self, file_id: FileId, vars: HashMap<StrId, (FileId, StrId)>) -> Result<(), String> {
        self.symbol_manager.insert_import_vars(file_id, vars)
    }

    pub fn reset_scope(&mut self) {
        self.scope_manager.reset();
        self.symbol_manager.reset_state();
    }

    pub fn enter_scope(&mut self) {
        self.scope_manager.next();
    }

    pub fn exit_scope(&mut self) {
        self.scope_manager.pop();
    }

    pub fn add_struct(&mut self, file_id: FileId, name: StrId, struct_symbol: StructSymbol) -> Result<(), String> {
        let id = self.scope_manager.current();
        self.symbol_manager.insert_struct(file_id, id, name, struct_symbol)
    }

    pub fn add_fn(&mut self, file_id: FileId, name: StrId, func: FunctionSymbol) -> Result<(), String> {
        let id = self.scope_manager.current();
        self.symbol_manager.insert_fn(file_id, id, name, func)
    }

    pub fn add_var(&mut self, file_id: FileId, name: StrId, var: VariableSymbol) -> Result<(), String> {
        let id = self.scope_manager.current();
        self.symbol_manager.insert_var(file_id, id, name, var)
    }

    pub fn get_all_struct_top_level(&mut self, file_id: FileId) -> Option<&HashMap<StrId, StructSymbol>> {
        self.symbol_manager.get_all_struct(file_id, 0)
    }

    pub fn get_struct_top_level(&mut self, file_id: FileId, name: StrId) -> Option<&StructSymbol> {
        self.symbol_manager.get_struct(file_id, 0, name)
    }

    pub fn lookup_struct(&mut self, file_id: FileId, name: StrId) -> Option<&StructSymbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get_struct(file_id, scope_id, name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn lookup_struct_with_import(&mut self, file_id: FileId, name: StrId, public_only: bool) -> Vec<StructSymbol> {
        let mut struct_decl = Vec::new();
        if let Some(symbol) = self.symbol_manager.get_struct(file_id, 0, name) {
            if public_only {
                if symbol.visibility.is_public() {
                    struct_decl.push(symbol.clone());
                }
            } else {
                struct_decl.push(symbol.clone());
            }
        }
        if let Some((source_file, name)) = self.symbol_manager.get_struct_import(file_id, name) {
            if let Some(symbol) = self.symbol_manager.get_struct(*source_file, 0, *name) {
                if public_only {
                    if symbol.visibility.is_public() {
                        struct_decl.push(symbol.clone());
                    }
                }  else {
                    struct_decl.push(symbol.clone());
                }
            }
        }

        struct_decl
    }

    pub fn get_all_fn_top_level(&mut self, file_id: FileId) -> Option<&HashMap<StrId, FunctionSymbol>> {
        self.symbol_manager.get_all_fn(file_id, 0)
    }

    pub fn get_fn_top_level(&mut self, file_id: FileId, name: StrId) -> Option<&FunctionSymbol> {
        self.symbol_manager.get_fn(file_id, 0, name)
    }

    pub fn lookup_fn(&mut self, file_id: FileId, name: StrId) -> Option<&FunctionSymbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get_fn(file_id, scope_id, name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn lookup_fn_with_import(&mut self, file_id: FileId, name: StrId, public_only: bool) -> Vec<FunctionSymbol> {
        let mut fn_decl = Vec::new();
        if let Some(symbol) = self.symbol_manager.get_fn(file_id, 0, name) {
            if public_only {
                if symbol.visibility.is_public() {
                    fn_decl.push(symbol.clone());
                }
            } else {
                fn_decl.push(symbol.clone());
            }
        }
        if let Some((cource_file, symbol)) = self.symbol_manager.get_fn_import(file_id, name) {
            if let Some(symbol) = self.symbol_manager.get_fn(*cource_file, 0, *symbol) {
                if public_only {
                    if symbol.visibility.is_public() {
                        fn_decl.push(symbol.clone());
                    }
                } else {
                    fn_decl.push(symbol.clone());
                }
            }
        }

        fn_decl
    }

    pub fn get_all_var_top_level(&mut self, file_id: FileId) -> Option<&HashMap<StrId, VariableSymbol>> {
        self.symbol_manager.get_all_var(file_id, 0)
    }

    pub fn get_var_top_level(&mut self, file_id: FileId, name: StrId) -> Option<&VariableSymbol> {
        self.symbol_manager.get_var(file_id, 0, name)
    }

    pub fn lookup_var(&mut self, file_id: FileId, name: StrId) -> Option<&VariableSymbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get_var(file_id, scope_id, name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn lookup_var_with_import(&mut self, file_id: FileId, name: StrId, public_only: bool) -> Vec<VariableSymbol> {
        let mut var_decl = Vec::new();
        if let Some(symbol) = self.symbol_manager.get_var(file_id, 0, name) {
            if public_only {
                if symbol.visibility.is_public() {
                    var_decl.push(symbol.clone());
                }
            } else {
                var_decl.push(symbol.clone());
            }
        }
        if let Some((source_file, symbol)) = self.symbol_manager.get_var_import(file_id, name) {
            if let Some(symbol) = self.symbol_manager.get_var(*source_file, 0, *symbol) {
                if public_only {
                    if symbol.visibility.is_public() {
                        var_decl.push(symbol.clone());
                    }
                } else {
                    var_decl.push(symbol.clone());
                }
            }
        }

        var_decl
    }

    pub fn lookup_all(&mut self, file_id: FileId, name: StrId) -> Vec<Symbol> {
        let mut results = Vec::new();
        let mut found = false;
        for scope_id in self.scope_manager.iter_rev() {
            if let Some(struct_symbol) = self.symbol_manager.get_struct(file_id, *scope_id, name) {
                found = true;
                results.push(struct_symbol.into());
            }

            if let Some(fn_symbol) = self.symbol_manager.get_fn(file_id, *scope_id, name) {
                found = true;
                results.push(fn_symbol.into());
            }

            if let Some(var_symbol) = self.symbol_manager.get_var(file_id, *scope_id, name) {
                found = true;
                results.push(var_symbol.into());
            }

            if found {
                break;
            }
        }

        results
    }

    pub fn lookup_all_top_level(&mut self, file_id: FileId, name: StrId) -> Vec<Symbol> {
        let mut results = Vec::new();
        if let Some(struct_symbol) = self.symbol_manager.get_struct(file_id, 0, name) {
            results.push(struct_symbol.into());
        }

        if let Some(fn_symbol) = self.symbol_manager.get_fn(file_id, 0, name) {
            results.push(fn_symbol.into());
        }

        if let Some(var_symbol) = self.symbol_manager.get_var(file_id, 0, name) {
            results.push(var_symbol.into());
        }

        results
    }

    pub fn lookup_all_top_level_with_import(&mut self, file_id: FileId, name: StrId, public: bool) -> Vec<Symbol> {
        let mut results = Vec::new();

        let structs =self.lookup_struct_with_import(file_id, name, public);
        if !structs.is_empty() {
            results.extend(structs.into_iter().map(|s| s.into()));
        }

        let fns = self.lookup_fn_with_import(file_id, name, public);
        if !fns.is_empty() {
            results.extend(fns.into_iter().map(|s| s.into()));
        }

        let vars = self.lookup_var_with_import(file_id, name, public);
        if !vars.is_empty() {
            results.extend(vars.into_iter().map(|s| s.into()));
        }

        results
    }

    pub fn all_symbol_import(&mut self, file_id: FileId) -> Option<&std::collections::HashMap<StrId, Symbol>> {
        self.symbol_manager.get_all_symbol_file(file_id, 0)
    }

    // pub fn lookup_symbol(&mut self, file_id: FileId, name: StrId) -> Option<Symbol> {
    //     for &scope_id in self.scope_manager.iter_rev() {
    //         if let Some(symbol) = self.symbol_manager.get(file_id, scope_id, name) {
    //             return Some(symbol.clone());
    //         }
    //     }

        // for file in &self.files {
        //     if file_id == *file {
        //         continue;
        //     }
        //     if let Some(symbol) = self.symbol_manager.get(*file, 0, name) {
        //         return Some(symbol.clone());
        //     }
        // }

    //     None
    // }

    pub fn lookup_symbol_scope_file(&mut self, file_id: FileId, name: StrId) -> Option<Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get(file_id, scope_id, name) {
                return Some(symbol.clone());
            }
        }

        None
    }

    pub fn lookup_global_scope(&mut self, file_id: FileId, name: StrId) -> Option<Symbol> {
        if let Some(symbol) = self.symbol_manager.get(file_id, 0, name) {
            return Some(symbol.clone());
        }
        None
    }

    pub fn lookup_symbol_current_scope(&mut self, file_id: FileId, name: StrId) -> Option<Symbol> {
        let id = self.scope_manager.current();
        if let Some(symbol) = self.symbol_manager.get(file_id, id, name) {
            return Some(symbol.clone());
        }
        None
    }

    pub fn lookup_symbol_mut(&mut self, file_id: FileId, name: StrId) -> Option<&mut Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(_) = self.symbol_manager.get(file_id, scope_id, name) {
                return self.symbol_manager.get_mut(file_id, scope_id, name);
            }
        }
        None
    }
    

    pub fn update_variable<F>(&mut self, file_id: FileId, name: StrId, f: F)
    where
        F: FnOnce(&mut VariableSymbol),
    {
        if let Some(symbol) = self.lookup_symbol_mut(file_id, name) {
            if let Some(var) = symbol.get_variable_mut() {
                f(var)
            }
        }
    }

    pub fn set_symbol(&mut self, file_id: FileId, id: NodeId, symbol: Symbol) {
        self.symbol_db.add_symbol(file_id, id, symbol);
    }

    pub fn get_symbol(&self, file_id: FileId, id: NodeId) -> Option<&Symbol> {
        self.symbol_db.get_symbol(file_id, id)
    }

    pub fn update_symbol_db<F>(&mut self, file_id: FileId, id: NodeId, f: F)
    where
        F: FnMut(&mut Symbol)
    {
        self.symbol_db.update_symbol(file_id, id, f);
    }

    pub fn add_type(&mut self, file_id: FileId, id: NodeId, ty: Rc<Type>) {
        self.type_manager.set_type(file_id, id, ty);
    }

    pub fn get_type(&self, file_id: FileId, id: NodeId) -> Option<&Rc<Type>> {
        self.type_manager.get_type(file_id, id)
    }

    pub fn print_debug(&mut self) {
        println!("==== Symbol Manager ====");
        self.symbol_manager.print_debug();
        println!("==== Symbol Database ====");
        self.symbol_db.print_debug();
        println!("==== Type Manager ====");
        self.type_manager.dump();
    }
}
