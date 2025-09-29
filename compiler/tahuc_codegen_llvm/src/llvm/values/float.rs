use tahuc_llvm::opaque::LLVMValueRef;

use crate::llvm::Values;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatValue {
    value: Values,
}

impl FloatValue {
    pub fn new(value: Values) -> Self {
        Self { value }
    }

    pub fn as_value(self) -> Values {
        self.value
    }

    pub fn as_value_ref(self) -> LLVMValueRef {
        self.value.into()
    }
}