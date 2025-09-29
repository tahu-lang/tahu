use tahuc_llvm::opaque::LLVMTypeRef;

use crate::llvm::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionType {
    ty: Type,
}

impl FunctionType {
    pub fn new(ty: Type) -> Self {
        Self { ty }
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty.as_type_ref()
    }
}
