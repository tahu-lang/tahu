use tahuc_ast::{nodes::ast::NodeId, Type};

use crate::{error::SemanticError, scope::ScopeManager, symbol::{Symbol, SymbolManager, VariableSymbol}, type_manager::TypeManager};

#[derive(Debug, Clone)]
pub struct Database {
    // scope manager for entering scope and exit scope
    scope_manager: ScopeManager,

    // symbol manager for all information about symbol
    symbol_manager: SymbolManager,

    // type manager, all information about type by node id
    type_manager: TypeManager,

    // error reporting
    errors: Vec<SemanticError>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            scope_manager: ScopeManager::new(),
            symbol_manager: SymbolManager::new(),
            type_manager: TypeManager::new(),
            errors: Vec::new(),
        }
    }

    pub fn report_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    pub fn get_errors(&self) -> &Vec<SemanticError> {
        &self.errors
    }

    pub fn reset_scope(&mut self) {
        self.scope_manager.reset();
        self.symbol_manager.reset_keep_global();
    }

    pub fn enter_scope(&mut self) {
        self.scope_manager.next();
    }

    pub fn exit_scope(&mut self) {
        self.scope_manager.pop();
    }

    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        let id = self.scope_manager.current();
        self.symbol_manager.insert(id, symbol.name.clone(), symbol)
    }

    pub fn lookup_symbol(&mut self, name: String) -> Option<Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(symbol) = self.symbol_manager.get(scope_id, name.to_string()) {
                return Some(symbol.clone());
            }
        }
        None
    }

    pub fn lookup_symbol_current_scope(&mut self, name: String) -> Option<Symbol> {
        let id = self.scope_manager.current();
        if let Some(symbol) = self.symbol_manager.get(id, name.to_string()) {
            return Some(symbol.clone());
        }
        None
    }

    pub fn lookup_symbol_mut(&mut self, name: String) -> Option<&mut Symbol> {
        for &scope_id in self.scope_manager.iter_rev() {
            if let Some(_) = self.symbol_manager.get(scope_id, name.clone()) {
                return self.symbol_manager.get_mut(scope_id, name.clone());
            }
        }
        None
    }
    

    pub fn update_variable<F>(&mut self, name: String, f: F)
    where
        F: FnOnce(&mut VariableSymbol),
    {
        if let Some(symbol) = self.lookup_symbol_mut(name) {
            if let Some(var) = symbol.get_variable_mut() {
                f(var)
            }
        }
    }

    pub fn add_type(&mut self, id: NodeId, ty: Type) {
        self.type_manager.set_type(id, ty);
    }

    pub fn get_type(&self, id: NodeId) -> Option<&Type> {
        self.type_manager.get_type(id)
    }

    pub fn print_debug(&mut self) {
        self.symbol_manager.print_debug();
        println!("==== Type Manager ====");
        self.type_manager.dump();
    }
}
