use tahuc_llvm::opaque::LLVMValueRef;

use crate::llvm::{Type, Values};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayValue {
    value: Values,
}

impl ArrayValue {
    pub fn new(value: Values) -> Self {
        Self { value }
    }

    pub fn as_value_ref(self) -> LLVMValueRef {
        self.value.into()
    }

    pub fn get_type(self) -> Type {
        Type::new(self.value.get_type())
    }
}