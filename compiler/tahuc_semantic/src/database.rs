use tahuc_ast::{nodes::ast::NodeId, ty::Type};
use tahuc_span::FileId;

use crate::{error::SemanticError, scope::ScopeManager, symbol::{Symbol, SymbolDatabase, SymbolManager, VariableSymbol}, type_manager::TypeManager};

#[derive(Debug, Clone)]
pub struct Database {
    // scope manager for entering scope and exit scope
    scope_manager: ScopeManager,

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
            symbol_manager: SymbolManager::new(),
            symbol_db: SymbolDatabase::new(),
            type_manager: TypeManager::new(),
            files: Vec::new(),
            errors: Vec::new(),
        }
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

    pub fn add_symbol(&mut self, file_id: FileId, symbol: Symbol) -> Result<(), String> {
        let id = self.scope_manager.current();
        self.symbol_manager.insert(file_id, id, symbol.name.clone(), symbol)
    }

    pub fn all_symbol_public(&mut self, file_id: FileId) -> Option<&std::collections::HashMap<String, Symbol>> {
        self.symbol_manager.get_all_symbol_public(file_id, 0)
    }

    pub fn lookup_symbol(&mut self, file_id: FileId, name: String) -> Option<Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get(file_id, scope_id, name.to_string()) {
                return Some(symbol.clone());
            }
        }

        for file in &self.files {
            if file_id == *file {
                continue;
            }
            if let Some(symbol) = self.symbol_manager.get(*file, 0, name.to_string()) {
                return Some(symbol.clone());
            }
        }

        None
    }

    pub fn lookup_symbol_scope_file(&mut self, file_id: FileId, name: String) -> Option<Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get(file_id, scope_id, name.to_string()) {
                return Some(symbol.clone());
            }
        }

        None
    }

    pub fn lookup_global_scope(&mut self, file_id: FileId, name: String) -> Option<Symbol> {
        if let Some(symbol) = self.symbol_manager.get(file_id, 0, name.to_string()) {
            return Some(symbol.clone());
        }
        None
    }

    pub fn lookup_symbol_current_scope(&mut self, file_id: FileId, name: String) -> Option<Symbol> {
        let id = self.scope_manager.current();
        if let Some(symbol) = self.symbol_manager.get(file_id, id, name.to_string()) {
            return Some(symbol.clone());
        }
        None
    }

    pub fn lookup_symbol_mut(&mut self, file_id: FileId, name: String) -> Option<&mut Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(_) = self.symbol_manager.get(file_id, scope_id, name.clone()) {
                return self.symbol_manager.get_mut(file_id, scope_id, name.clone());
            }
        }
        None
    }
    

    pub fn update_variable<F>(&mut self, file_id: FileId, name: String, f: F)
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

    pub fn add_type(&mut self, file_id: FileId, id: NodeId, ty: Type) {
        self.type_manager.set_type(file_id, id, ty);
    }

    pub fn get_type(&self, file_id: FileId, id: NodeId) -> Option<&Type> {
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
