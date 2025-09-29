use std::mem::MaybeUninit;

use tahuc_llvm::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
    core::{LLVMAddFunction, LLVMAddGlobalInAddressSpace, LLVMPrintModuleToString},
    opaque::LLVMModuleRef,
};

use crate::llvm::{FunctionType, FunctionValue, GlobalValue, Type, Values};

#[derive(Debug, Clone, Copy)]
pub struct Module {
    module: LLVMModuleRef,
}

impl Module {
    pub fn new(module: LLVMModuleRef) -> Self {
        Self { module }
    }

    pub fn add_function(&self, name: &str, ty: FunctionType) -> FunctionValue {
        let name = std::ffi::CString::new(name).unwrap();
        let name = name.as_ptr();

        let value = Values::new(unsafe { LLVMAddFunction(self.module, name, ty.as_type_ref()) });

        FunctionValue::new(value)
    }

    pub fn add_global(&self, ty: Type, name: &str) -> GlobalValue {
        let name = std::ffi::CString::new(name).unwrap();
        let name = name.as_ptr();
        let value = Values::new(unsafe {
            LLVMAddGlobalInAddressSpace(self.module, ty.as_type_ref(), name, 0)
        });

        GlobalValue::new(value)
    }

    pub fn verify(&self) -> Result<(), String> {
        let mut err_str = MaybeUninit::uninit();
        let action = LLVMVerifierFailureAction::LLVMPrintMessageAction;

        let code = unsafe { LLVMVerifyModule(self.module, action, err_str.as_mut_ptr()) };

        let err_str = unsafe { err_str.assume_init() };
        if code == 1 && !err_str.is_null() {
            let err_str = unsafe { std::ffi::CStr::from_ptr(err_str) };
            return Err(err_str.to_string_lossy().into_owned());
        }

        Ok(())
    }

    pub fn print_to_string(&self) -> String {
        let str = unsafe { LLVMPrintModuleToString(self.module) };
        let str = unsafe { std::ffi::CStr::from_ptr(str) };
        str.to_string_lossy().into_owned()
    }
}

impl From<Module> for LLVMModuleRef {
    fn from(value: Module) -> Self {
        value.module
    }
}
