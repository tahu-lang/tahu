use tahuc_llvm::{
    core::{LLVMGlobalGetValueType, LLVMSetFunctionCallConv, LLVMSetLinkage, LLVMSetVisibility},
    opaque::LLVMValueRef,
};

use crate::llvm::{BasicValue, CallConvention, FunctionType, Linkage, Type, Values, Visibility};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionValue {
    value: Values,
}

impl FunctionValue {
    pub fn new(value: Values) -> Self {
        Self { value }
    }

    pub fn as_value_ref(self) -> LLVMValueRef {
        self.value.into()
    }

    pub fn set_linkage(self, linkage: Linkage) {
        unsafe { LLVMSetLinkage(self.as_value_ref(), linkage.as_llvm_linkage()) };
    }

    pub fn set_visibility(self, visibility: Visibility) {
        unsafe { LLVMSetVisibility(self.as_value_ref(), visibility.as_llvm_visibility()) };
    }

    pub fn set_call_convention(self, cc: CallConvention) {
        unsafe {
            LLVMSetFunctionCallConv(self.as_value_ref(), cc.as_llvm_call_conv());
        }
    }

    pub fn get_ty(self) -> FunctionType {
        let ty = Type::new(unsafe { LLVMGlobalGetValueType(self.as_value_ref()) });
        FunctionType::new(ty)
    }

    pub fn get_param_at(self, index: usize) -> BasicValue {
        let param = unsafe { tahuc_llvm::core::LLVMGetParam(self.as_value_ref(), index as u32) };
        BasicValue::new(Values::new(param))
    }
}