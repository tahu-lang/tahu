use super::super::opaque::*;

// Core
unsafe extern "C" {
    pub unsafe fn LLVMContextCreate() -> LLVMContextRef;
    pub unsafe fn LLVMContextDispose(context: LLVMContextRef);
}

// Module
unsafe extern "C" {
    pub unsafe fn LLVMModuleCreateWithNameInContext(
        ModuleID: *const std::ffi::c_char,
        C: LLVMContextRef,
    ) -> LLVMModuleRef;
    pub unsafe fn LLVMModuleDispose(module: LLVMModuleRef);

    pub unsafe fn LLVMPrintModuleToFile(
        M: LLVMModuleRef,
        Filename: *const std::ffi::c_char,
        ErrorMessage: *mut *mut std::ffi::c_char,
    ) -> LLVMBool;
    pub unsafe fn LLVMPrintModuleToString(M: LLVMModuleRef) -> *mut std::ffi::c_char;

    pub unsafe fn LLVMAddFunction(
        M: LLVMModuleRef,
        Name: *const std::ffi::c_char,
        FunctionTy: LLVMTypeRef,
    ) -> LLVMValueRef;
}

// value
unsafe extern "C" {
    // Core->Values->Constants->Scalar
    pub unsafe fn LLVMConstInt(
        IntTy: LLVMTypeRef,
        N: std::ffi::c_ulonglong,
        SignExtend: LLVMBool,
    ) -> LLVMValueRef;
}
