use tahuc_llvm::opaque::LLVMBasicBlockRef;

#[derive(Debug, Clone, Copy)]
pub struct BasicBlock {
    block: LLVMBasicBlockRef
}

impl BasicBlock {
    pub fn new(block: LLVMBasicBlockRef) -> Self {
        Self {
            block
        }
    }

    pub fn as_block_ref(self) -> LLVMBasicBlockRef {
        self.block
    }
}