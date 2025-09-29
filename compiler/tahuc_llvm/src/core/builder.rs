use crate::LLVMOpcode;

use super::super::opaque::*;

unsafe extern "C" {
    // core builder
    pub unsafe fn LLVMCreateBuilderInContext(C: LLVMContextRef) -> LLVMBuilderRef;
    pub unsafe fn LLVMDisposeBuilder(builder: LLVMBuilderRef);

    pub unsafe fn LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef);

    // Terminators
    pub unsafe fn LLVMBuildRetVoid(arg1: LLVMBuilderRef) -> LLVMValueRef;

    pub unsafe fn LLVMBuildRet(arg1: LLVMBuilderRef, V: LLVMValueRef) -> LLVMValueRef;

    pub unsafe fn LLVMBuildBr(arg1: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef;

    pub unsafe fn LLVMBuildCondBr(
        arg1: LLVMBuilderRef,
        If: LLVMValueRef,
        Then: LLVMBasicBlockRef,
        Else: LLVMBasicBlockRef,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildSwitch(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Else: LLVMBasicBlockRef,
        NumCases: std::ffi::c_uint,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildIndirectBr(
        B: LLVMBuilderRef,
        Addr: LLVMValueRef,
        NumDests: std::ffi::c_uint,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildInvoke2(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Fn: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: std::ffi::c_uint,
        Then: LLVMBasicBlockRef,
        Catch: LLVMBasicBlockRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildUnreachable(B: LLVMBuilderRef) -> LLVMValueRef;

    // call
    pub unsafe fn LLVMBuildCall2(
        arg1: LLVMBuilderRef,
        arg2: LLVMTypeRef,
        Fn: LLVMValueRef,
        Args: *mut LLVMValueRef,
        NumArgs: std::ffi::c_uint,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// Add a case to a `switch` instruction
    pub unsafe fn LLVMAddCase(Switch: LLVMValueRef, OnVal: LLVMValueRef, Dest: LLVMBasicBlockRef);

    pub unsafe fn LLVMCreateBasicBlockInContext(
        C: LLVMContextRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMBasicBlockRef;
    pub unsafe fn LLVMAppendBasicBlockInContext(
        C: LLVMContextRef,
        Fn: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMBasicBlockRef;
    pub unsafe fn LLVMAppendBasicBlock(
        Fn: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMBasicBlockRef;
    pub unsafe fn LLVMInsertBasicBlockInContext(
        C: LLVMContextRef,
        BB: LLVMBasicBlockRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMBasicBlockRef;
    pub unsafe fn LLVMInsertBasicBlock(
        InsertBeforeBB: LLVMBasicBlockRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMBasicBlockRef;

    // memory
    pub unsafe fn LLVMBuildAlloca(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildLoad2(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        PointerVal: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildStore(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        Ptr: LLVMValueRef,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildGEP2(
        B: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Pointer: LLVMValueRef,
        Indices: *mut LLVMValueRef,
        NumIndices: std::ffi::c_uint,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildInBoundsGEP2(
        B: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Pointer: LLVMValueRef,
        Indices: *mut LLVMValueRef,
        NumIndices: std::ffi::c_uint,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    // phi instruction
    pub unsafe fn LLVMBuildPhi(
        arg1: LLVMBuilderRef,
        Ty: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMAddIncoming(
        PhiNode: LLVMValueRef,
        IncomingValues: *mut LLVMValueRef,
        IncomingBlocks: *mut LLVMBasicBlockRef,
        Count: std::ffi::c_uint,
    );

    pub unsafe fn LLVMBuildSelect(
        arg1: LLVMBuilderRef,
        If: LLVMValueRef,
        Then: LLVMValueRef,
        Else: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    // Casts
    pub unsafe fn LLVMBuildTrunc(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildZExt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildSExt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildFPToUI(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildFPToSI(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildUIToFP(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildSIToFP(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildFPTrunc(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildFPExt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildPtrToInt(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildIntToPtr(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildBitCast(
        arg1: LLVMBuilderRef,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildCast(
        B: LLVMBuilderRef,
        Op: LLVMOpcode,
        Val: LLVMValueRef,
        DestTy: LLVMTypeRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    // unary
    pub unsafe fn LLVMBuildNeg(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildFNeg(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    pub unsafe fn LLVMBuildNot(
        arg1: LLVMBuilderRef,
        V: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
}

// // Core->Pass managers
// unsafe extern "C" {
//     pub unsafe fn LLVMCreatePassManager() -> LLVMPassManagerRef;
//     pub unsafe fn LLVMCreateFunctionPassManagerForModule(M: LLVMModuleRef) -> LLVMPassManagerRef;
//     pub unsafe fn LLVMCreateFunctionPassManager(MP: LLVMModuleProviderRef) -> LLVMPassManagerRef;
//     pub unsafe fn LLVMRunPassManager(PM: LLVMPassManagerRef, M: LLVMModuleRef) -> LLVMBool;
//     pub unsafe fn LLVMInitializeFunctionPassManager(FPM: LLVMPassManagerRef) -> LLVMBool;
//     pub unsafe fn LLVMRunFunctionPassManager(FPM: LLVMPassManagerRef, F: LLVMValueRef) -> LLVMBool;
//     pub unsafe fn LLVMFinalizeFunctionPassManager(FPM: LLVMPassManagerRef) -> LLVMBool;
//     pub unsafe fn LLVMDisposePassManager(PM: LLVMPassManagerRef);
// }
