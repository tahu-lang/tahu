use crate::{LLVMLinkage, LLVMTypeKind, LLVMVisibility};

use super::super::opaque::*;

// Type
unsafe extern "C" {
    // Core->Types->Integer
    pub unsafe fn LLVMInt1TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
    pub unsafe fn LLVMInt8TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
    pub unsafe fn LLVMInt16TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
    pub unsafe fn LLVMInt32TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
    pub unsafe fn LLVMInt64TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
    pub unsafe fn LLVMInt128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
    pub unsafe fn LLVMIntTypeInContext(C: LLVMContextRef, NumBits: std::ffi::c_uint) -> LLVMTypeRef;
    pub unsafe fn LLVMGetIntTypeWidth(IntegerTy: LLVMTypeRef) -> std::ffi::c_uint;

    // Core->Types->Floating-Point
    /**
     * Obtain a 16-bit floating point type from a context.
     */
    pub unsafe fn LLVMHalfTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    /**
     * Obtain a 16-bit brain floating point type from a context.
     */
    pub unsafe fn LLVMBFloatTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    /**
     * Obtain a 32-bit floating point type from a context.
     */
    pub unsafe fn LLVMFloatTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    /**
     * Obtain a 64-bit floating point type from a context.
     */
    pub unsafe fn LLVMDoubleTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    /**
     * Obtain a 80-bit floating point type (X87) from a context.
     */
    pub unsafe fn LLVMX86FP80TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    /**
     * Obtain a 128-bit floating point type (112-bit mantissa) from a
     * context.
     */
    pub unsafe fn LLVMFP128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    /**
     * Obtain a 128-bit floating point type (two 64-bits) from a context.
     */
    pub unsafe fn LLVMPPCFP128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    // Core->Types->Function
    pub unsafe fn LLVMFunctionType(
        ReturnType: LLVMTypeRef,
        ParamTypes: *mut LLVMTypeRef,
        ParamCount: std::ffi::c_uint,
        IsVarArg: LLVMBool,
    ) -> LLVMTypeRef;

    pub unsafe fn LLVMGetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
    pub unsafe fn LLVMGlobalGetValueType(Global: LLVMValueRef) -> LLVMTypeRef;
    pub unsafe fn LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef);


    // Core->Types->Struct
    pub unsafe fn LLVMStructCreateNamed(C: LLVMContextRef, Name: *const std::ffi::c_char) -> LLVMTypeRef;
    pub unsafe fn LLVMStructSetBody(
        StructTy: LLVMTypeRef,
        ElementTypes: *mut LLVMTypeRef,
        ElementCount: std::ffi::c_uint,
        Packed: LLVMBool,
    );
    
    // Core->Types->Sequential
    /// Create a fixed size array type that refers to a specific type.
    ///
    /// The created type will exist in the context that its element type
    /// exists in.
    pub unsafe fn LLVMArrayType(ElementType: LLVMTypeRef, ElementCount: std::ffi::c_uint) -> LLVMTypeRef;

    pub unsafe fn LLVMPointerTypeInContext(C: LLVMContextRef, AddressSpace: std::ffi::c_uint)
        -> LLVMTypeRef;

    // Core->Types->Other
    pub unsafe fn LLVMVoidTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;

    // helper
    pub unsafe fn LLVMGetTypeKind(Ty: LLVMTypeRef) -> LLVMTypeKind;
    pub unsafe fn LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;

    pub unsafe fn LLVMSetLinkage(Global: LLVMValueRef, Linkage: LLVMLinkage);
    pub unsafe fn LLVMSetVisibility(Global: LLVMValueRef, Viz: LLVMVisibility);

    pub unsafe fn LLVMSetFunctionCallConv(Fn: LLVMValueRef, CC: std::ffi::c_uint);
    pub unsafe fn LLVMSetInstructionCallConv(Instr: LLVMValueRef, CC: std::ffi::c_uint);
}