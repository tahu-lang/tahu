use crate::{LLVMIntPredicate, LLVMRealPredicate};

use super::super::opaque::*;

unsafe extern "C" {
    /// int add
    pub unsafe fn LLVMBuildAdd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// float add
    pub unsafe fn LLVMBuildFAdd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int sub
    pub unsafe fn LLVMBuildSub(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// float sub
    pub unsafe fn LLVMBuildFSub(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int mul
    pub unsafe fn LLVMBuildMul(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// float mul
    pub unsafe fn LLVMBuildFMul(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int signed div
    pub unsafe fn LLVMBuildSDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int unsigned div
    pub unsafe fn LLVMBuildExactSDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// float div
    pub unsafe fn LLVMBuildFDiv(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int signed rem
    pub unsafe fn LLVMBuildSRem(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int unsigned rem
    pub unsafe fn LLVMBuildURem(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// float rem
    pub unsafe fn LLVMBuildFRem(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    // Comparisons
    /// int comparison
    pub unsafe fn LLVMBuildICmp(
        arg1: LLVMBuilderRef,
        Op: LLVMIntPredicate,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;
    
    /// float comparison
    pub unsafe fn LLVMBuildFCmp(
        arg1: LLVMBuilderRef,
        Op: LLVMRealPredicate,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int shl
    pub unsafe fn LLVMBuildShl(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int signed shr
    pub unsafe fn LLVMBuildLShr(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int unsigned shr
    pub unsafe fn LLVMBuildAShr(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    /// int and
    pub unsafe fn LLVMBuildAnd(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildOr(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;

    pub unsafe fn LLVMBuildXor(
        arg1: LLVMBuilderRef,
        LHS: LLVMValueRef,
        RHS: LLVMValueRef,
        Name: *const std::ffi::c_char,
    ) -> LLVMValueRef;


}