use std::{collections::{HashMap, HashSet}, hash::Hash};
use tahuc_span::{FileId, StrId};

use crate::database::symbol::{FunctionSymbol, StructSymbol, Symbol, VariableSymbol};

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct ScopeKey {
    pub file_id: FileId,
    pub id: usize,
}

// impl Hash for ScopeKey {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.file_id.hash(state);
//         self.id.hash(state);
//     }
// }

#[derive(Debug, Clone)]
pub struct SymbolManager {
    symbols: HashMap<ScopeKey, HashMap<StrId, Symbol>>,
    structs: HashMap<ScopeKey, HashMap<StrId, StructSymbol>>,
    functions: HashMap<ScopeKey, HashMap<StrId, FunctionSymbol>>,
    variables: HashMap<ScopeKey, HashMap<StrId, VariableSymbol>>,

    // import map
    import_struct: HashMap<FileId, HashMap<StrId, (FileId, StrId)>>,
    import_function: HashMap<FileId, HashMap<StrId, (FileId, StrId)>>,
    import_variable: HashMap<FileId, HashMap<StrId, (FileId, StrId)>>,

    files: HashSet<FileId>,
}

impl SymbolManager {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),

            import_struct: HashMap::new(),
            import_function: HashMap::new(),
            import_variable: HashMap::new(),

            files: HashSet::new(),
        }
    }

    pub fn allocate_size(&mut self, file_id: FileId, size: usize) {
        let file = self.symbols.entry(ScopeKey { file_id, id: size }).or_default();
        file.reserve(size);
        let file = self.structs.entry(ScopeKey { file_id, id: size }).or_default();
        file.reserve(size);
        let file = self.functions.entry(ScopeKey { file_id, id: size }).or_default();
        file.reserve(size);
        let file = self.variables.entry(ScopeKey { file_id, id: size }).or_default();
        file.reserve(size);

        let file = self.import_struct.entry(file_id).or_default();
        file.reserve(size * 4);
        let file = self.import_function.entry(file_id).or_default();
        file.reserve(size * 4);
        let file = self.import_variable.entry(file_id).or_default();
        file.reserve(size * 4);
    }

    pub fn add_file(&mut self, file_id: FileId) {
        self.files.insert(file_id);
    }

    pub fn reset_state(&mut self) {
        self.structs.retain(|key, _| key.id == 0);
        self.functions.retain(|key, _| key.id == 0);
        self.variables.retain(|key, _| key.id == 0);
        self.symbols.retain(|key, _| key.id == 0);
    }

    pub fn insert_import_struct(&mut self, file_id: FileId, import_name: StrId, import: (FileId, StrId)) -> Result<(), String> {
        let file = self.import_struct.entry(file_id).or_default();
        if file.contains_key(&import_name) {
            return Err(format!("struct '{}' already declared", import_name));
        }
        file.insert(import_name, import);
        Ok(())
    }

    pub fn insert_import_structs(&mut self, file_id: FileId, structs: HashMap<StrId, (FileId, StrId)>) -> Result<(), String> {
        let file = self.import_struct.entry(file_id).or_default();
        for (name, struct_symbol) in structs {
            if file.contains_key(&name) {
                return Err(format!("struct '{}' already declared", name));
            }
            file.insert(name, struct_symbol);
        }
        Ok(())
    }

    pub fn insert_import_fn(&mut self, file_id: FileId, name: StrId, import: (FileId, StrId)) -> Result<(), String> {
        let file = self.import_function.entry(file_id).or_default();
        if file.contains_key(&name) {
            return Err(format!("function '{}' already declared", name));
        }
        file.insert(name, import);
        Ok(())
    }

    pub fn insert_import_fns(&mut self, file_id: FileId, functions: HashMap<StrId, (FileId, StrId)>) -> Result<(), String> {
        let file = self.import_function.entry(file_id).or_default();
        for (name, func) in functions {
            if file.contains_key(&name) {
                return Err(format!("function '{}' already declared", name));
            }
            file.insert(name, func);
        }
        Ok(())
    }

    pub fn insert_import_var(&mut self, file_id: FileId, name: StrId, import: (FileId, StrId)) -> Result<(), String> {
        let file = self.import_variable.entry(file_id).or_default();
        if file.contains_key(&name) {
            return Err(format!("variable '{}' already declared", name));
        }
        file.insert(name, import);
        Ok(())
    }

    pub fn insert_import_vars(&mut self, file_id: FileId, variables: HashMap<StrId, (FileId, StrId)>) -> Result<(), String> {
        let file = self.import_variable.entry(file_id).or_default();
        for (name, var) in variables {
            if file.contains_key(&name) {
                return Err(format!("variable '{}' already declared", name));
            }
            file.insert(name, var);
        }
        Ok(())
    }

    pub fn insert_struct(
        &mut self,
        file_id: FileId,
        id: usize,
        name: StrId,
        struct_symbol: StructSymbol,
    ) -> Result<(), String> {
        let queries = self.query(file_id, id);

        let file = self.structs.entry(queries).or_default();

        if file.contains_key(&name) {
            return Err(format!("struct '{}' already declared", name));
        }
        file.insert(name, struct_symbol);

        Ok(())
    }

    pub fn get_struct(&self, file_id: FileId, id: usize, name: StrId) -> Option<&StructSymbol> {
        let queries = self.query(file_id, id);
        self.structs.get(&queries).and_then(|file| file.get(&name))
    }

    pub fn get_struct_import(&self, file_id: FileId, name: StrId) -> Option<&(FileId, StrId)> {
        self.import_struct.get(&file_id).and_then(|file| file.get(&name))
    }

    pub fn get_all_struct(&self, file_id: FileId, id: usize) -> Option<&HashMap<StrId, StructSymbol>> {
        let queries = self.query(file_id, id);
        self.structs.get(&queries)
    }

    pub fn insert_fn(
        &mut self,
        file_id: FileId,
        id: usize,
        name: StrId,
        func: FunctionSymbol,
    ) -> Result<(), String> {
        let queries = self.query(file_id, id);

        let file = self.functions.entry(queries.clone()).or_default();

        if file.contains_key(&name) {
            return Err(format!(
                "function '{}' already declared in scope {:?}",
                name, queries
            ));
        }

        file.insert(name, func);

        Ok(())
    }

    pub fn get_fn(&self, file_id: FileId, id: usize, name: StrId) -> Option<&FunctionSymbol> {
        let queries = self.query(file_id, id);
        self.functions
            .get(&queries)
            .and_then(|file| file.get(&name))
    }

    pub fn get_fn_import(&self, file_id: FileId, name: StrId) -> Option<&(FileId, StrId)> {
        self.import_function.get(&file_id).and_then(|file| file.get(&name))
    }

    pub fn get_all_fn(&self, file_id: FileId, id: usize) -> Option<&HashMap<StrId, FunctionSymbol>> {
        let queries = self.query(file_id, id);
        self.functions.get(&queries)
    }

    pub fn insert_var(
        &mut self,
        file_id: FileId,
        id: usize,
        name: StrId,
        var: VariableSymbol,
    ) -> Result<(), String> {
        let queries = self.query(file_id, id);

        let file = self.variables.entry(queries.clone()).or_default();

        if file.contains_key(&name) {
            return Err(format!(
                "variable '{}' already declared in scope {:?}",
                name, queries
            ));
        }

        file.insert(name, var);

        Ok(())
    }

    pub fn get_var(&self, file_id: FileId, id: usize, name: StrId) -> Option<&VariableSymbol> {
        let queries = self.query(file_id, id);
        self.variables
            .get(&queries)
            .and_then(|file| file.get(&name))
    }

    pub fn get_var_import(&self, file_id: FileId, name: StrId) -> Option<&(FileId, StrId)> {
        self.import_variable.get(&file_id).and_then(|file| file.get(&name))
    }

    pub fn get_all_var(&self, file_id: FileId, id: usize) -> Option<&HashMap<StrId, VariableSymbol>> {
        let queries = self.query(file_id, id);
        self.variables.get(&queries)
    }

    // pub fn insert(
    //     &mut self,
    //     file_id: FileId,
    //     id: usize,
    //     name: StrId,
    //     symbol: Symbol,
    // ) -> Result<(), String> {
    //     let queries = self.query(file_id, id);

    //     // if name is exist in current scope return error
    //     if let Some(symbol) = self.symbols.get(&queries) {
    //         if symbol.contains_key(&name) {
    //             return Err(format!(
    //                 "name '{}' already declared in scope {:?}",
    //                 name, queries
    //             ));
    //         }
    //     } else {
    //         self.symbols.insert(queries.clone(), HashMap::new());
    //     }

    //     self.symbols.get_mut(&queries).unwrap().insert(name, symbol);
    //     Ok(())
    // }

    pub fn get(&self, file_id: FileId, id: usize, name: StrId) -> Option<&Symbol> {
        let queries = self.query(file_id, id);

        if self.symbols.get(&queries).is_some() {
            self.symbols.get(&queries).unwrap().get(&name)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, file_id: FileId, id: usize, name: StrId) -> Option<&mut Symbol> {
        let queries = self.query(file_id, id);
        self.symbols.get_mut(&queries).unwrap().get_mut(&name)
    }

    pub fn get_all_symbol_file(
        &mut self,
        file_id: FileId,
        id: usize,
    ) -> Option<&HashMap<StrId, Symbol>> {
        let queries = self.query(file_id, id);
        self.symbols.get(&queries)
    }

    fn query(&self, file_id: FileId, id: usize) -> ScopeKey {
        ScopeKey { file_id, id }
    }

    pub fn print_debug(&mut self) {
        self.symbols.iter().for_each(|(scope, symbols)| {
            println!(
                "file: {} , Id : {} count: {}",
                scope.file_id.0,
                scope.id,
                symbols.capacity()
            );
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
