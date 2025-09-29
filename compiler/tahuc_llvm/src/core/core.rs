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
    pub unsafe fn LLVMDisposeModule(module: LLVMModuleRef);

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

    pub unsafe fn LLVMConstReal(RealTy: LLVMTypeRef, N: std::ffi::c_double) -> LLVMValueRef;
    pub unsafe fn LLVMConstPointerNull(Ty: LLVMTypeRef) -> LLVMValueRef;
    pub unsafe fn LLVMConstStringInContext(
        C: LLVMContextRef,
        Str: *const std::ffi::c_char,
        Length: std::ffi::c_uint,
        DontNullTerminate: LLVMBool,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMAddGlobalInAddressSpace(
        M: LLVMModuleRef,
        Ty: LLVMTypeRef,
        Name: *const std::ffi::c_char,
        AddressSpace: std::ffi::c_uint,
    ) -> LLVMValueRef;

    // Function Value
    pub unsafe fn LLVMCountParams(Fn: LLVMValueRef) -> std::ffi::c_uint;
    pub unsafe fn LLVMGetParam(Fn: LLVMValueRef, Index: std::ffi::c_uint) -> LLVMValueRef;
    pub unsafe fn LLVMGetNextParam(Arg: LLVMValueRef) -> LLVMValueRef;
}
