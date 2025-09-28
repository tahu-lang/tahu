//! Target information

#[derive(Debug)]
pub enum LLVMOpaqueTargetData {}

pub type LLVMTargetDataRef = *mut LLVMOpaqueTargetData;

unsafe extern "C" {
    pub unsafe fn LLVMInitializeAMDGPUTargetInfo();
    pub unsafe fn LLVMInitializeAMDGPUTarget();
    pub unsafe fn LLVMInitializeAMDGPUTargetMC();
    pub unsafe fn LLVMInitializeAMDGPUAsmPrinter();
    pub unsafe fn LLVMInitializeAMDGPUAsmParser();
    // Disassembler?

    pub unsafe fn LLVMInitializeSystemZTargetInfo();
    pub unsafe fn LLVMInitializeSystemZTarget();
    pub unsafe fn LLVMInitializeSystemZTargetMC();
    pub unsafe fn LLVMInitializeSystemZAsmPrinter();
    pub unsafe fn LLVMInitializeSystemZAsmParser();
    pub unsafe fn LLVMInitializeSystemZDisassembler();

    pub unsafe fn LLVMInitializeHexagonTargetInfo();
    pub unsafe fn LLVMInitializeHexagonTarget();
    pub unsafe fn LLVMInitializeHexagonTargetMC();
    pub unsafe fn LLVMInitializeHexagonAsmPrinter();
    // AsmParser?
    pub unsafe fn LLVMInitializeHexagonDisassembler();

    pub unsafe fn LLVMInitializeNVPTXTargetInfo();
    pub unsafe fn LLVMInitializeNVPTXTarget();
    pub unsafe fn LLVMInitializeNVPTXTargetMC();
    pub unsafe fn LLVMInitializeNVPTXAsmPrinter();
    // AsmParser?

    pub unsafe fn LLVMInitializeMSP430TargetInfo();
    pub unsafe fn LLVMInitializeMSP430Target();
    pub unsafe fn LLVMInitializeMSP430TargetMC();
    pub unsafe fn LLVMInitializeMSP430AsmPrinter();
    // AsmParser?

    pub unsafe fn LLVMInitializeXCoreTargetInfo();
    pub unsafe fn LLVMInitializeXCoreTarget();
    pub unsafe fn LLVMInitializeXCoreTargetMC();
    pub unsafe fn LLVMInitializeXCoreAsmPrinter();
    // AsmParser?
    pub unsafe fn LLVMInitializeXCoreDisassembler();

    pub unsafe fn LLVMInitializeMipsTargetInfo();
    pub unsafe fn LLVMInitializeMipsTarget();
    pub unsafe fn LLVMInitializeMipsTargetMC();
    pub unsafe fn LLVMInitializeMipsAsmPrinter();
    pub unsafe fn LLVMInitializeMipsAsmParser();
    pub unsafe fn LLVMInitializeMipsDisassembler();

    pub unsafe fn LLVMInitializeAArch64TargetInfo();
    pub unsafe fn LLVMInitializeAArch64Target();
    pub unsafe fn LLVMInitializeAArch64TargetMC();
    pub unsafe fn LLVMInitializeAArch64AsmPrinter();
    pub unsafe fn LLVMInitializeAArch64AsmParser();
    pub unsafe fn LLVMInitializeAArch64Disassembler();

    pub unsafe fn LLVMInitializeARMTargetInfo();
    pub unsafe fn LLVMInitializeARMTarget();
    pub unsafe fn LLVMInitializeARMTargetMC();
    pub unsafe fn LLVMInitializeARMAsmPrinter();
    pub unsafe fn LLVMInitializeARMAsmParser();
    pub unsafe fn LLVMInitializeARMDisassembler();

    pub unsafe fn LLVMInitializePowerPCTargetInfo();
    pub unsafe fn LLVMInitializePowerPCTarget();
    pub unsafe fn LLVMInitializePowerPCTargetMC();
    pub unsafe fn LLVMInitializePowerPCAsmPrinter();
    pub unsafe fn LLVMInitializePowerPCAsmParser();
    pub unsafe fn LLVMInitializePowerPCDisassembler();

    pub unsafe fn LLVMInitializeSparcTargetInfo();
    pub unsafe fn LLVMInitializeSparcTarget();
    pub unsafe fn LLVMInitializeSparcTargetMC();
    pub unsafe fn LLVMInitializeSparcAsmPrinter();
    pub unsafe fn LLVMInitializeSparcAsmParser();
    pub unsafe fn LLVMInitializeSparcDisassembler();

    pub unsafe fn LLVMInitializeX86TargetInfo();
    pub unsafe fn LLVMInitializeX86Target();
    pub unsafe fn LLVMInitializeX86TargetMC();
    pub unsafe fn LLVMInitializeX86AsmPrinter();
    pub unsafe fn LLVMInitializeX86AsmParser();
    pub unsafe fn LLVMInitializeX86Disassembler();

    pub unsafe fn LLVMInitializeBPFTargetInfo();
    pub unsafe fn LLVMInitializeBPFTarget();
    pub unsafe fn LLVMInitializeBPFTargetMC();
    pub unsafe fn LLVMInitializeBPFAsmPrinter();
    pub unsafe fn LLVMInitializeBPFAsmParser();
    pub unsafe fn LLVMInitializeBPFDisassembler();

    pub unsafe fn LLVMInitializeLanaiTargetInfo();
    pub unsafe fn LLVMInitializeLanaiTarget();
    pub unsafe fn LLVMInitializeLanaiTargetMC();
    pub unsafe fn LLVMInitializeLanaiAsmPrinter();
    pub unsafe fn LLVMInitializeLanaiAsmParser();
    pub unsafe fn LLVMInitializeLanaiDisassembler();

    pub unsafe fn LLVMInitializeRISCVTargetInfo();
    pub unsafe fn LLVMInitializeRISCVTarget();
    pub unsafe fn LLVMInitializeRISCVTargetMC();
    pub unsafe fn LLVMInitializeRISCVAsmPrinter();
    pub unsafe fn LLVMInitializeRISCVAsmParser();
    pub unsafe fn LLVMInitializeRISCVDisassembler();

    pub unsafe fn LLVMInitializeLoongArchTargetInfo();
    pub unsafe fn LLVMInitializeLoongArchTarget();
    pub unsafe fn LLVMInitializeLoongArchTargetMC();
    pub unsafe fn LLVMInitializeLoongArchAsmPrinter();
    pub unsafe fn LLVMInitializeLoongArchAsmParser();
    pub unsafe fn LLVMInitializeLoongArchDisassembler();

    pub unsafe fn LLVMInitializeWebAssemblyTargetInfo();
    pub unsafe fn LLVMInitializeWebAssemblyTarget();
    pub unsafe fn LLVMInitializeWebAssemblyTargetMC();
    pub unsafe fn LLVMInitializeWebAssemblyAsmPrinter();
    pub unsafe fn LLVMInitializeWebAssemblyAsmParser();
    pub unsafe fn LLVMInitializeWebAssemblyDisassembler();
}
