use tahuc_llvm::LLVMOpcode;

pub enum LLVMCastKind {
    Trunc,
    ZExt,
    SExt,
    FPToUI,
    FPToSI,
    UIToFP,
    SIToFP,
    FPTrunc,
    FPExt,
    PtrToInt,
    IntToPtr,
    BitCast,
}

impl LLVMCastKind {
    pub fn as_llvm_opcode(&self) -> LLVMOpcode {
        match self {
            LLVMCastKind::Trunc => LLVMOpcode::LLVMTrunc,
            LLVMCastKind::ZExt => LLVMOpcode::LLVMZExt,
            LLVMCastKind::SExt => LLVMOpcode::LLVMSExt,
            LLVMCastKind::FPToUI => LLVMOpcode::LLVMFPToUI,
            LLVMCastKind::FPToSI => LLVMOpcode::LLVMFPToSI,
            LLVMCastKind::UIToFP => LLVMOpcode::LLVMUIToFP,
            LLVMCastKind::SIToFP => LLVMOpcode::LLVMSIToFP,
            LLVMCastKind::FPTrunc => LLVMOpcode::LLVMFPTrunc,
            LLVMCastKind::FPExt => LLVMOpcode::LLVMFPExt,
            LLVMCastKind::PtrToInt => LLVMOpcode::LLVMPtrToInt,
            LLVMCastKind::IntToPtr => LLVMOpcode::LLVMIntToPtr,
            LLVMCastKind::BitCast => LLVMOpcode::LLVMBitCast,
        }
    }
}