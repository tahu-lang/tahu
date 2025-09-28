#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

pub mod core;
pub mod target_machine;
pub mod analysis;
pub mod target;

pub enum LLVMContext {}

pub enum LLVMModule {}

pub enum LLVMBuilder {}

pub enum LLVMBasicBlock {}

pub enum LLVMType {}

pub enum LLVMValue {}

pub enum LLVMPassManager {}

pub enum LLVMMemoryBuffer {}

pub mod opaque {
    pub type LLVMBool = std::ffi::c_int;
    pub type LLVMContextRef = *mut super::LLVMContext;
    pub type LLVMModuleRef = *mut super::LLVMModule;
    pub type LLVMBuilderRef = *mut super::LLVMBuilder;
    pub type LLVMBasicBlockRef = *mut super::LLVMBasicBlock;
    pub type LLVMTypeRef = *mut super::LLVMType;
    pub type LLVMValueRef = *mut super::LLVMValue;
    pub type LLVMPassManagerRef = *mut super::LLVMPassManager;
    pub type LLVMMemoryBufferRef = *mut super::LLVMMemoryBuffer;
}

#[repr(C)]
pub enum LLVMOpcode {
    /* Terminator Instructions */
    LLVMRet            = 1,
    LLVMBr             = 2,
    LLVMSwitch         = 3,
    LLVMIndirectBr     = 4,
    LLVMInvoke         = 5,
    /* removed 6 due to API changes */
    LLVMUnreachable    = 7,
    LLVMCallBr         = 67,
    
    /* Standard Unary Operators */
    LLVMFNeg           = 66,
    
    /* Standard Binary Operators */
    LLVMAdd            = 8,
    LLVMFAdd           = 9,
    LLVMSub            = 10,
    LLVMFSub           = 11,
    LLVMMul            = 12,
    LLVMFMul           = 13,
    LLVMUDiv           = 14,
    LLVMSDiv           = 15,
    LLVMFDiv           = 16,
    LLVMURem           = 17,
    LLVMSRem           = 18,
    LLVMFRem           = 19,
    
    /* Logical Operators */
    LLVMShl            = 20,
    LLVMLShr           = 21,
    LLVMAShr           = 22,
    LLVMAnd            = 23,
    LLVMOr             = 24,
    LLVMXor            = 25,
    
    /* Memory Operators */
    LLVMAlloca         = 26,
    LLVMLoad           = 27,
    LLVMStore          = 28,
    LLVMGetElementPtr  = 29,
    
    /* Cast Operators */
    LLVMTrunc          = 30,
    LLVMZExt           = 31,
    LLVMSExt           = 32,
    LLVMFPToUI         = 33,
    LLVMFPToSI         = 34,
    LLVMUIToFP         = 35,
    LLVMSIToFP         = 36,
    LLVMFPTrunc        = 37,
    LLVMFPExt          = 38,
    LLVMPtrToInt       = 39,
    LLVMIntToPtr       = 40,
    LLVMBitCast        = 41,
    LLVMAddrSpaceCast  = 60,
    
    /* Other Operators */
    LLVMICmp           = 42,
    LLVMFCmp           = 43,
    LLVMPHI            = 44,
    LLVMCall           = 45,
    LLVMSelect         = 46,
    LLVMUserOp1        = 47,
    LLVMUserOp2        = 48,
    LLVMVAArg          = 49,
    LLVMExtractElement = 50,
    LLVMInsertElement  = 51,
    LLVMShuffleVector  = 52,
    LLVMExtractValue   = 53,
    LLVMInsertValue    = 54,
    LLVMFreeze         = 68,
    
    /* Atomic operators */
    LLVMFence          = 55,
    LLVMAtomicCmpXchg  = 56,
    LLVMAtomicRMW      = 57,
    
    /* Exception Handling Operators */
    LLVMResume         = 58,
    LLVMLandingPad     = 59,
    LLVMCleanupRet     = 61,
    LLVMCatchRet       = 62,
    LLVMCatchPad       = 63,
    LLVMCleanupPad     = 64,
    LLVMCatchSwitch    = 65
}

#[repr(C)]
#[derive(Debug)]
pub enum LLVMTypeKind {
    /**< type with no size */
    LLVMVoidTypeKind,      
    /**< 16 bit floating point type */
    LLVMHalfTypeKind,      
    /**< 32 bit floating point type */
    LLVMFloatTypeKind,     
    /**< 64 bit floating point type */
    LLVMDoubleTypeKind,    
    /**< 80 bit floating point type (X87) */
    LLVMX86_FP80TypeKind,  
    /**< 128 bit floating point type (112-bit mantissa)*/
    LLVMFP128TypeKind,     
    /**< 128 bit floating point type (two 64-bits) */
    LLVMPPC_FP128TypeKind, 
    /**< Labels */
    LLVMLabelTypeKind,    
    /**< Arbitrary bit width integers */
    LLVMIntegerTypeKind,   
    /**< Functions */
    LLVMFunctionTypeKind,  
    /**< Structures */
    LLVMStructTypeKind,    
    /**< Arrays */
    LLVMArrayTypeKind,     
    /**< Pointers */
    LLVMPointerTypeKind,   
    /**< Fixed width SIMD vector type */
    LLVMVectorTypeKind,    
    /**< Metadata */
    LLVMMetadataTypeKind,  
    /**< X86 MMX */
    LLVMX86_MMXTypeKind,   
    /**< Tokens */
    LLVMTokenTypeKind,     
    /**< Scalable SIMD vector type */
    LLVMScalableVectorTypeKind, 
    /**< 16 bit brain floating point type */
    LLVMBFloatTypeKind,    
    /**< X86 AMX */
    LLVMX86_AMXTypeKind,   
    /**< Target extension type */
    LLVMTargetExtTypeKind, 
}

#[repr(C)]
pub enum LLVMLinkage {
    /**< Externally visible function */
    LLVMExternalLinkage,    
    LLVMAvailableExternallyLinkage,
    /**< Keep one copy of function when linking (inline)*/
    LLVMLinkOnceAnyLinkage, 
    /**< Same, but only replaced by something equivalent. */
    LLVMLinkOnceODRLinkage, 
    /**< Obsolete */
    LLVMLinkOnceODRAutoHideLinkage, 
    /**< Keep one copy of function when linking (weak) */
    LLVMWeakAnyLinkage,     
    /**< Same, but only replaced by something equivalent. */
    LLVMWeakODRLinkage,     
    /**< Special purpose, only applies to global arrays */
    LLVMAppendingLinkage,   
    /**< Rename collisions when linking (static functions) */
    LLVMInternalLinkage,    
    /**< Like Internal, but omit from symbol table */
    LLVMPrivateLinkage,     
    /**< Obsolete */
    LLVMDLLImportLinkage,   
    /**< Obsolete */
    LLVMDLLExportLinkage,   
    /**< ExternalWeak linkage description */
    LLVMExternalWeakLinkage,
    /**< Obsolete */
    LLVMGhostLinkage,       
    /**< Tentative definitions */
    LLVMCommonLinkage,      
    /**< Like Private, but linker removes. */
    LLVMLinkerPrivateLinkage, 
    /**< Like LinkerPrivate, but is weak. */
    LLVMLinkerPrivateWeakLinkage 
}

#[repr(C)]
pub enum LLVMVisibility {
    /**< The GV is visible */
    LLVMDefaultVisibility,  
    /**< The GV is hidden */
    LLVMHiddenVisibility,   
    /**< The GV is protected */
    LLVMProtectedVisibility 
}

#[repr(C)]
pub enum LLVMCallConv {
  LLVMCCallConv             = 0,
  LLVMFastCallConv          = 8,
  LLVMColdCallConv          = 9,
  LLVMGHCCallConv           = 10,
  LLVMHiPECallConv          = 11,
  LLVMAnyRegCallConv        = 13,
  LLVMPreserveMostCallConv  = 14,
  LLVMPreserveAllCallConv   = 15,
  LLVMSwiftCallConv         = 16,
  LLVMCXXFASTTLSCallConv    = 17,
  LLVMX86StdcallCallConv    = 64,
  LLVMX86FastcallCallConv   = 65,
  LLVMARMAPCSCallConv       = 66,
  LLVMARMAAPCSCallConv      = 67,
  LLVMARMAAPCSVFPCallConv   = 68,
  LLVMMSP430INTRCallConv    = 69,
  LLVMX86ThisCallCallConv   = 70,
  LLVMPTXKernelCallConv     = 71,
  LLVMPTXDeviceCallConv     = 72,
  LLVMSPIRFUNCCallConv      = 75,
  LLVMSPIRKERNELCallConv    = 76,
  LLVMIntelOCLBICallConv    = 77,
  LLVMX8664SysVCallConv     = 78,
  LLVMWin64CallConv         = 79,
  LLVMX86VectorCallCallConv = 80,
  LLVMHHVMCallConv          = 81,
  LLVMHHVMCCallConv         = 82,
  LLVMX86INTRCallConv       = 83,
  LLVMAVRINTRCallConv       = 84,
  LLVMAVRSIGNALCallConv     = 85,
  LLVMAVRBUILTINCallConv    = 86,
  LLVMAMDGPUVSCallConv      = 87,
  LLVMAMDGPUGSCallConv      = 88,
  LLVMAMDGPUPSCallConv      = 89,
  LLVMAMDGPUCSCallConv      = 90,
  LLVMAMDGPUKERNELCallConv  = 91,
  LLVMX86RegCallCallConv    = 92,
  LLVMAMDGPUHSCallConv      = 93,
  LLVMMSP430BUILTINCallConv = 94,
  LLVMAMDGPULSCallConv      = 95,
  LLVMAMDGPUESCallConv      = 96
}

#[repr(C)]
pub enum LLVMValueKind {
  LLVMArgumentValueKind,
  LLVMBasicBlockValueKind,
  LLVMMemoryUseValueKind,
  LLVMMemoryDefValueKind,
  LLVMMemoryPhiValueKind,

  LLVMFunctionValueKind,
  LLVMGlobalAliasValueKind,
  LLVMGlobalIFuncValueKind,
  LLVMGlobalVariableValueKind,
  LLVMBlockAddressValueKind,
  LLVMConstantExprValueKind,
  LLVMConstantArrayValueKind,
  LLVMConstantStructValueKind,
  LLVMConstantVectorValueKind,

  LLVMUndefValueValueKind,
  LLVMConstantAggregateZeroValueKind,
  LLVMConstantDataArrayValueKind,
  LLVMConstantDataVectorValueKind,
  LLVMConstantIntValueKind,
  LLVMConstantFPValueKind,
  LLVMConstantPointerNullValueKind,
  LLVMConstantTokenNoneValueKind,

  LLVMMetadataAsValueValueKind,
  LLVMInlineAsmValueKind,

  LLVMInstructionValueKind,
  LLVMPoisonValueValueKind,
  LLVMConstantTargetNoneValueKind,
}

#[repr(C)]
pub enum LLVMIntPredicate {
    /**< equal */
  LLVMIntEQ = 32, 
  /**< not equal */
  LLVMIntNE,      
  /**< unsigned greater than */
  LLVMIntUGT,     
  /**< unsigned greater or equal */
  LLVMIntUGE,     
  /**< unsigned less than */
  LLVMIntULT,     
  /**< unsigned less or equal */
  LLVMIntULE,     
  /**< signed greater than */
  LLVMIntSGT,     
  /**< signed greater or equal */
  LLVMIntSGE,     
  /**< signed less than */
  LLVMIntSLT,     
  /**< signed less or equal */
  LLVMIntSLE      
}

#[repr(C)]
pub enum LLVMRealPredicate {
    /**< Always false (always folded) */
  LLVMRealPredicateFalse, 
  /**< True if ordered and equal */
  LLVMRealOEQ,            
  /**< True if ordered and greater than */
  LLVMRealOGT,            
  /**< True if ordered and greater than or equal */
  LLVMRealOGE,            
  /**< True if ordered and less than */
  LLVMRealOLT,            
  /**< True if ordered and less than or equal */
  LLVMRealOLE,            
  /**< True if ordered and operands are unequal */
  LLVMRealONE,            
  /**< True if ordered (no nans) */
  LLVMRealORD,            
  /**< True if unordered: isnan(X) | isnan(Y) */
  LLVMRealUNO,            
  /**< True if unordered or equal */
  LLVMRealUEQ,            
  /**< True if unordered or greater than */
  LLVMRealUGT,            
  /**< True if unordered, greater than, or equal */
  LLVMRealUGE,            
  /**< True if unordered or less than */
  LLVMRealULT,            
  /**< True if unordered, less than, or equal */
  LLVMRealULE,            
  /**< True if unordered or not equal */
  LLVMRealUNE,            
  /**< Always true (always folded) */
  LLVMRealPredicateTrue   
}

/**
 * Tail call kind for LLVMSetTailCallKind and LLVMGetTailCallKind.
 *
 * Note that 'musttail' implies 'tail'.
 *
 * @see CallInst::TailCallKind
 */
#[repr(C)]
pub enum LLVMTailCallKind {
  LLVMTailCallKindNone = 0,
  LLVMTailCallKindTail = 1,
  LLVMTailCallKindMustTail = 2,
  LLVMTailCallKindNoTail = 3,
}