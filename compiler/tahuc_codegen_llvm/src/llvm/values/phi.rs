use tahuc_llvm::{core::LLVMAddIncoming, opaque::{LLVMBasicBlockRef, LLVMValueRef}};

use crate::llvm::{AsValueRef, BasicBlock, BasicValue, Values};

pub struct PhiValue {
    value: Values,
}

impl PhiValue {
    pub fn new(value: Values) -> Self {
        Self { value }
    }

    pub fn as_value(self) -> Values {
        self.value
    }

    pub fn as_value_ref(&self) -> LLVMValueRef {
        self.value.into()
    }

    pub fn add_incoming(&mut self, incoming: Vec<(BasicValue, BasicBlock)>) {
        let (mut value, mut blocks): (Vec<LLVMValueRef>, Vec<LLVMBasicBlockRef>) = 
            incoming
            .iter()
            .map(|(v, bb)| (v.as_value_ref(), bb.as_block_ref()))
            .collect();

        unsafe {
            LLVMAddIncoming(
                self.as_value_ref(),
                value.as_mut_ptr(),
                blocks.as_mut_ptr(),
                incoming.len() as u32,
            );
        }
    }
}