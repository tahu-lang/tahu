use std::collections::HashMap;

use tahuc_ast::{Type, nodes::ast::NodeId};
use tahuc_span::Span;

#[derive(Debug, Clone)]
pub struct SymbolManager {
    pub symbols: HashMap<usize, HashMap<String, Symbol>>,
}

impl SymbolManager {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn reset_keep_global(&mut self) {
        if let Some(global) = self.symbols.get(&0).cloned() {
            self.symbols.clear();
            self.symbols.insert(0, global); // restore global only
        } else {
            self.symbols.clear();
            self.symbols.insert(0, HashMap::new());
        }
    }

    pub fn insert(&mut self, id: usize, name: String, symbol: Symbol) -> Result<(), String> {
        if !self.symbols.contains_key(&id) {
            self.symbols.insert(id, HashMap::new());
        }

        // Cek apakah nama sudah ada di scope ini
        if self.symbols.get(&id).unwrap().contains_key(&name) {
            return Err(format!("name '{}' already declared in scope {}", name, id));
        }

        self.symbols.get_mut(&id).unwrap().insert(name, symbol);
        Ok(())
    }

    pub fn get(&self, id: usize, name: String) -> Option<&Symbol> {
        if self.symbols.get(&id).is_some() {
            self.symbols.get(&id).unwrap().get(&name)
        } else {
            None
        }
        // self.symbols.get(&id)
        // self.symbols.get(&id).unwrap().get(&name)
    }

    pub fn get_mut(&mut self, id: usize, name: String) -> Option<&mut Symbol> {
        self.symbols.get_mut(&id).unwrap().get_mut(&name)
    }

    pub fn print_debug(&mut self) {
        println!("==== Symbol Manager ====");
        self.symbols.iter().for_each(|(id, symbols)| {
            println!("Id : {} count: {}", id, symbols.capacity());
            symbols.iter().for_each(|(name, symbol)| {
                if let Some(f) = symbol.get_function() {
                    println!(
                        "  fn {}({}) -> {}",
                        name,
                        f.parameters
                            .iter()
                            .map(|p| { format!("{}: {}", p.name.clone(), p.var_type.clone()) })
                            .collect::<Vec<_>>().join(", ")
                            ,
                        f.return_type
                    );
                } else if let Some(v) = symbol.get_variable() {
                    println!(
                        "  var {}: {} - final type {}",
                        name, v.declared_type, v.final_type
                    );
                }
            });
            println!();
        });
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub span: Span,
    pub kind: SymbolKind,
}

impl Symbol {
    pub fn get_variable(&self) -> Option<&VariableSymbol> {
        match &self.kind {
            SymbolKind::Variable(var) => {
                return Some(var);
            }
            _ => None,
        }
    }

    pub fn get_variable_mut(&mut self) -> Option<&mut VariableSymbol> {
        if let SymbolKind::Variable(v) = &mut self.kind {
            return Some(v);
        } else {
            None
        }
    }

    pub fn get_function(&self) -> Option<&FunctionSymbol> {
        match &self.kind {
            SymbolKind::Function(func) => {
                return Some(func);
            }
            _ => None,
        }
    }

    pub fn get_function_mut(&mut self) -> Option<&mut FunctionSymbol> {
        if let SymbolKind::Function(v) = &mut self.kind {
            return Some(v);
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Variable(VariableSymbol),
    Function(FunctionSymbol),
}

#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub name: String,
    pub declared_type: Type,
    pub initializer: Option<NodeId>,

    /// mark if type is Type::Inference
    /// use
    /// ```
    /// Type.is_inferred()
    /// ```
    pub need_inferred: bool,

    /// update after type checking
    pub inferred_type: Option<Type>,

    /// final type resolved type
    pub final_type: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    // default information
    pub name: String, // identifier for function
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Type,
    // for now skip type inference
    // /// mark if type is Type::Inference
    // pub is_inferred: bool,
    // pub return_type_inferred: Option<Type>,
}

/// for now just static typing
#[derive(Debug, Clone)]
pub struct ParameterInfo {
    pub name: String,
    pub var_type: Type,
    pub span: Span,
    // // type inference
    // pub is_inferred: bool,
    // pub inferred_type: Option<Type>, // update after type checking
}
