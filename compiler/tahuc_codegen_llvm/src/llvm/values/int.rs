use tahuc_llvm::{opaque::LLVMValueRef};

use crate::llvm::{IntType, Type, Values};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntValue {
    value: Values
}

impl IntValue {
    pub fn new(value: Values) -> Self {
        Self {
            value,
        }
    }

    pub fn as_value_ref(self) -> LLVMValueRef {
        self.value.into()
    }

    pub fn as_value(self) -> Values {
        self.value
    }

    pub fn get_ty(self) -> IntType {
        IntType::new(Type::new(self.value.get_type()))
    }
}