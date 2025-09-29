use tahuc_llvm::{core::{LLVMSetInitializer, LLVMSetLinkage, LLVMSetVisibility}, opaque::LLVMValueRef,};

use crate::llvm::{AsValueRef, BasicValue, Linkage, PointerValue, Type, Values, Visibility};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalValue {
    value: Values,
}

impl GlobalValue {
    pub fn new(value: Values) -> Self {
        Self { value }
    }

    pub fn as_value_ref(self) -> LLVMValueRef {
        self.value.into()
    }

    pub fn as_value(self) -> Values {
        self.value
    }

    pub fn get_type(self) -> Type {
        Type::new(self.value.get_type())
    }

    pub fn set_linkage(self, linkage: Linkage) {
        unsafe { LLVMSetLinkage(self.as_value_ref(), linkage.as_llvm_linkage()) };
    }

    pub fn set_visibility(self, visibility: Visibility) {
        unsafe { LLVMSetVisibility(self.as_value_ref(), visibility.as_llvm_visibility()) };
    }

    pub fn as_ptr(self) -> PointerValue {
        PointerValue::new(self.value)
    }

    pub fn set_initializer(self, value: BasicValue) {
        unsafe { LLVMSetInitializer(self.as_value_ref(), value.as_value_ref()) }
    }
}
