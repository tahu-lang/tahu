use std::collections::HashMap;

use tahuc_ast::nodes::ast::NodeId;
use tahuc_span::FileId;

use crate::database::symbol::Symbol;

#[derive(Debug, Clone)]
pub struct SymbolDatabase {
    symbol: HashMap<FileId, HashMap<NodeId, Symbol>>,

    symbol_lookup: HashMap<FileId, HashMap<Symbol, NodeId>>,

    // left node_id, right owned id
    constain_id: HashMap<FileId, HashMap<NodeId, NodeId>>,
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
            self.constain_id
                .entry(file_id)
                .or_default()
                .insert(id, existing_id);
            return;
        }

        file.insert(id, symbol.clone());
        lookup.insert(symbol, id);
    }

    pub fn get_symbol(&self, file_id: FileId, id: NodeId) -> Option<&Symbol> {
        let file = self.symbol.get(&file_id)?;
        let lookup_id = self
            .constain_id
            .get(&file_id)
            .and_then(|file| file.get(&id))
            .unwrap_or(&id);
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
                println!(
                    "    NodeId: {}, Symbol: name {} file {}",
                    node_id, symbol.name, symbol.file_id
                );
            });
            println!();
            println!();
        });

        self.constain_id.iter().for_each(|(file_id, symbols)| {
            println!("  FileId: {:?}", file_id);
            symbols.iter().for_each(|(node_id, owned_id)| {
                println!("    NodeId: {:?}, OwnedId: {:?}", node_id, owned_id);
            });
            println!();
            println!();
        });
    }
}
