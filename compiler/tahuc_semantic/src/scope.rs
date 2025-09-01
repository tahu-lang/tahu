use std::iter::Rev;

#[derive(Debug, Clone)]
pub struct ScopeManager {
    scope_id: Vec<usize>,
    next_id: usize,
}

impl ScopeManager {
    pub fn new() -> Self {
        let mut scope_manager = Self {
            scope_id: Vec::new(),
            next_id: 1,
        };

        scope_manager.scope_id.push(0);
        scope_manager
    }

    pub fn reset(&mut self) {
        self.scope_id.clear();
        self.scope_id.push(0);
        self.next_id = 1;
    }

    pub fn current(&self) -> usize {
        *self.scope_id.last().unwrap()
    }

    pub fn next(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        self.scope_id.push(id);
        id
    }

    pub fn pop(&mut self) {
        self.scope_id.pop();
    }

    pub fn iter_rev(&'_ mut self) -> Rev<std::slice::Iter<'_, usize>> {
        self.scope_id.iter().rev()
    }
}