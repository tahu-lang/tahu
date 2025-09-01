use std::collections::HashMap;

use tahuc_ast::{nodes::ast::NodeId, Type};

#[derive(Debug, Clone)]
pub struct TypeManager {
    pub types: HashMap<NodeId, Type>,
}

impl TypeManager {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn get_type(&self, node_id: NodeId) -> Option<&Type> {
        self.types.get(&node_id)
    }

    pub fn set_type(&mut self, node_id: NodeId, ty: Type) {
        self.types.insert(node_id, ty);
    }

    pub fn dump(&self) {
        for (node_id, ty) in &self.types {
            println!("{}: {:?}", node_id, ty);
        }
    }
}