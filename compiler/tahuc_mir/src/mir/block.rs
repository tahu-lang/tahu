use crate::mir::{instruction::{MirInstruction, MirOperand}, BasicBlockId};

#[derive(Debug, Clone)]
pub struct MirBasicBlock {
    pub id: BasicBlockId,
    pub name: String,
    pub instructions: Vec<MirInstruction>,
    pub terminator: MirTerminator,
    pub predecessors: Vec<BasicBlockId>,
    pub successors: Vec<BasicBlockId>,
}

#[derive(Debug, Clone)]
pub enum MirTerminator {
    Return {
        value: Option<MirOperand>,
    },

    Jump {
        target: BasicBlockId,
    },

    Branch {
        condition: MirOperand,
        true_target: BasicBlockId,
        false_target: BasicBlockId,
    },

    Unreachable,
}

impl MirBasicBlock {
    pub fn add_instruction(&mut self, instruction: MirInstruction) {
        self.instructions.push(instruction);
    }

    pub fn set_terminator(&mut self, terminator: MirTerminator) {
        self.set_successors_from_terminator(&terminator);
        self.terminator = terminator;
    }

    fn set_successors_from_terminator(&mut self, terminator: &MirTerminator) {
        // Clear old successors
        self.successors.clear();

        // Add new successors based on terminator
        match &terminator {
            MirTerminator::Jump { target } => {
                self.successors.push(*target);
            }
            MirTerminator::Branch {
                true_target,
                false_target,
                ..
            } => {
                self.successors.push(*true_target);
                self.successors.push(*false_target);
            }
            MirTerminator::Return { .. } | MirTerminator::Unreachable => {
                // No successors
            }
        }
    }
}