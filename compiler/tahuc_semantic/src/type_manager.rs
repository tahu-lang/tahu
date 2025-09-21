use std::collections::HashMap;

use tahuc_ast::{nodes::ast::NodeId, Type};
use tahuc_span::FileId;

#[derive(Debug, Clone)]
pub struct TypeManager {
    pub types: HashMap<FileId, HashMap<NodeId, Type>>,
}

impl TypeManager {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn get_type(&self, file_id: FileId, node_id: NodeId) -> Option<&Type> {
        if let Some(file) = self.types.get(&file_id) {
            file.get(&node_id)
        } else {
            None
        }
    }

    pub fn set_type(&mut self, file_id: FileId, node_id: NodeId, ty: Type) {
        if !self.types.contains_key(&file_id) {
            self.types.insert(file_id, HashMap::new());
        }
        let file = self.types.get_mut(&file_id).unwrap();
        file.insert(node_id, ty);
    }

    pub fn dump(&self) {
        for (file_id, map_ty) in &self.types {
            for (node_id, ty) in map_ty {
                println!("{} - {}: {}", file_id.0, node_id, ty);
            }
        }
    }
}