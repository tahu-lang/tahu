use std::collections::{HashMap, HashSet};

use tahuc_ast::{nodes::{ast::NodeId, declarations::Visibility}, ty::Type};
use tahuc_span::{FileId, Span};

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct ScopeKey {
    pub file_id: FileId,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolManager {
    pub symbols: HashMap<ScopeKey, HashMap<String, Symbol>>,
    files: HashSet<FileId>,
}

impl SymbolManager {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            files: HashSet::new(),
        }
    }

    pub fn add_file(&mut self, file_id: FileId) {
        self.files.insert(file_id);
    }

    pub fn reset_state(&mut self) {
        let mut symbols = Vec::new();

        for file in self.files.iter() {
            let queries = ScopeKey { file_id: *file, id: 0 };
            let symbol = self.symbols.get(&queries).cloned().unwrap_or_else(|| HashMap::new());
            symbols.push((queries, symbol));
        }

        self.symbols.clear();
        
        for (queries, symbol) in symbols {
            self.symbols.insert(queries, symbol);
        }
    }

    pub fn insert(&mut self, file_id: FileId, id: usize, name: String, symbol: Symbol) -> Result<(), String> {
        let queries = self.query(file_id, id);

        // if name is exist in current scope return error
        if let Some(symbol) = self.symbols.get(&queries) {
            if symbol.contains_key(&name) {
                return Err(format!("name '{}' already declared in scope {:?}", name, queries));
            }
        } else {
            self.symbols.insert(queries.clone(), HashMap::new());
        }

        self.symbols.get_mut(&queries).unwrap().insert(name, symbol);
        Ok(())
    }

    pub fn get(&self, file_id: FileId, id: usize, name: String) -> Option<&Symbol> {
        let queries = self.query(file_id, id);

        if self.symbols.get(&queries).is_some() {
            self.symbols.get(&queries).unwrap().get(&name)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, file_id: FileId, id: usize, name: String) -> Option<&mut Symbol> {
        let queries = self.query(file_id, id);
        self.symbols.get_mut(&queries).unwrap().get_mut(&name)
    }

    pub fn get_all_symbol_public(&mut self, file_id: FileId, id: usize) -> Option<&HashMap<String, Symbol>> {
        let queries = self.query(file_id, id);
        self.symbols.get(&queries)
    }

    fn query(&self, file_id: FileId, id: usize) -> ScopeKey {
        ScopeKey { file_id, id }
    }

    pub fn print_debug(&mut self) {
        self.symbols.iter().for_each(|(scope, symbols)| {
            println!("file: {} , Id : {} count: {}", scope.file_id.0, scope.id, symbols.capacity());
            symbols.iter().for_each(|(name, symbol)| {
                if let Some(s) = symbol.get_struct() {
                    println!("  struct {} {{", name);
                    s.fields.iter().for_each(|field| {
                        println!("    {}: {}", field.name, field.r#type);
                    });
                    println!("  }}");
                } else if let Some(f) = symbol.get_function() {
                    println!(
                        "  fn {}({}) -> {}",
                        name,
                        f.parameters
                            .iter()
                            .map(|p| { format!("{}: {}", p.name.clone(), p.var_type.clone()) })
                            .collect::<Vec<_>>()
                            .join(", "),
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
pub struct SymbolDatabase {
    symbol: HashMap<FileId, HashMap<NodeId, Symbol>>,

    symbol_lookup: HashMap<FileId, HashMap<Symbol, NodeId>>,

    // left node_id, right owned id
    constain_id: HashMap<NodeId, NodeId>,
}

impl SymbolDatabase {
    pub fn new() -> Self {
        Self {
            symbol: HashMap::new(),
            symbol_lookup: HashMap::new(),
            constain_id: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, file_id: FileId, id: NodeId, symbol: Symbol) {
        let file = self.symbol.entry(file_id).or_default();
        let lookup = self.symbol_lookup.entry(file_id).or_default();

        if file.contains_key(&id) {
            return;
        }

        if let Some(&existing_id) = lookup.get(&symbol) {
            self.constain_id.insert(id, existing_id);
            return;
        }

        file.insert(id, symbol.clone());
        lookup.insert(symbol, id);
    }

    pub fn get_symbol(&self, file_id: FileId, id: NodeId) -> Option<&Symbol> {
        let file = self.symbol.get(&file_id)?;
        let lookup_id = self.constain_id.get(&id).unwrap_or(&id);
        file.get(lookup_id)
    }

    pub fn update_symbol<F>(&mut self, file_id: FileId, id: NodeId, mut f: F)
    where
        F: FnMut(&mut Symbol),
    {
        if let Some(file) = self.symbol.get_mut(&file_id) {
            if let Some(symbol) = file.get_mut(&id) {
                f(symbol);
            }
        }
    }

    pub fn print_debug(&mut self) {
        self.symbol.iter().for_each(|(file_id, symbols)| {
            println!("  FileId: {:?}", file_id);
            symbols.iter().for_each(|(node_id, symbol)| {
                println!("    NodeId: {:?}, Symbol: {:?}", node_id, symbol);
            });
            println!();
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: String,
    pub span: Span,
    pub kind: SymbolKind,
}

impl Symbol {
    pub fn get_struct(&self) -> Option<&StructSymbol> {
        match &self.kind {
            SymbolKind::Struct(struct_symbol) => {
                return Some(struct_symbol);
            }
            _ => None,
        }
    }

    pub fn get_struct_mut(&mut self) -> Option<&mut StructSymbol> {
        if let SymbolKind::Struct(v) = &mut self.kind {
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

    pub fn update_variable<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut VariableSymbol),
    {
        if let SymbolKind::Variable(var) = &mut self.kind {
            f(var);
        }
    }

    pub fn get_type(&self) -> Type {
        match &self.kind {
            SymbolKind::Struct(struct_symbol) => {
                struct_symbol.ty.clone()
            }
            SymbolKind::Function(func) => {
                func.return_type.clone()
            }
            SymbolKind::Variable(var) => {
                var.final_type.clone()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Struct(StructSymbol),
    Function(FunctionSymbol),
    Variable(VariableSymbol),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructSymbol {
    pub file_id: FileId,
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
    pub visibility: Visibility,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructFieldSymbol {
    pub name: String,
    pub r#type: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSymbol {
    // default information
    pub file_id: FileId,
    pub name: String, // identifier for function
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Type,
    pub visibility: Visibility,
    pub span: Span,
    // for now skip type inference
    // /// mark if type is Type::Inference
    // pub is_inferred: bool,
    // pub return_type_inferred: Option<Type>,
}

/// for now just static typing
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterInfo {
    pub name: String,
    pub var_type: Type,
    pub span: Span,
    // // type inference
    // pub is_inferred: bool,
    // pub inferred_type: Option<Type>, // update after type checking
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableSymbol {
    pub name: String,
    pub declared_type: Type,
    pub initializer: Option<NodeId>,

    pub is_mutable: bool,

    /// mark if type is Type::Inference
    /// use
    /// ```no run
    /// Type.is_inferred()
    /// ```
    pub need_inferred: bool,

    /// update after type checking
    pub inferred_type: Option<Type>,

    /// final type resolved type
    pub final_type: Type,
}
