//! Target machine information, to generate assembly or object files.

use super::opaque::*;
use super::target::LLVMTargetDataRef;

#[derive(Debug)]
pub enum LLVMOpaqueTargetMachine {}

pub type LLVMTargetMachineRef = *mut LLVMOpaqueTargetMachine;

#[derive(Debug)]
pub enum LLVMOpaqueTargetMachineOptions {}

pub type LLVMTargetMachineOptionsRef = *mut LLVMOpaqueTargetMachineOptions;

#[derive(Debug)]
pub enum LLVMTarget {}

pub type LLVMTargetRef = *mut LLVMTarget;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLVMCodeGenOptLevel {
    LLVMCodeGenLevelNone = 0,
    LLVMCodeGenLevelLess = 1,
    LLVMCodeGenLevelDefault = 2,
    LLVMCodeGenLevelAggressive = 3,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLVMRelocMode {
    LLVMRelocDefault = 0,
    LLVMRelocStatic = 1,
    LLVMRelocPIC = 2,
    LLVMRelocDynamicNoPic = 3,
    LLVMRelocROPI = 4,
    LLVMRelocRWPI = 5,
    LLVMRelocROPI_RWPI = 6,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLVMCodeModel {
    LLVMCodeModelDefault = 0,
    LLVMCodeModelJITDefault = 1,
    LLVMCodeModelTiny = 2,
    LLVMCodeModelSmall = 3,
    LLVMCodeModelKernel = 4,
    LLVMCodeModelMedium = 5,
    LLVMCodeModelLarge = 6,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLVMCodeGenFileType {
    LLVMAssemblyFile = 0,
    LLVMObjectFile = 1,
}

unsafe extern "C" {
    pub unsafe fn LLVMGetTargetFromName(Name: *const std::ffi::c_char) -> LLVMTargetRef;
    pub unsafe fn LLVMGetTargetFromTriple(
        Triple: *const std::ffi::c_char,
        T: *mut LLVMTargetRef,
        ErrorMessage: *mut *mut std::ffi::c_char,
    ) -> LLVMBool;
    pub unsafe fn LLVMGetTargetName(T: LLVMTargetRef) -> *const std::ffi::c_char;

    pub unsafe fn LLVMCreateTargetMachine(
        T: LLVMTargetRef,
        Triple: *const std::ffi::c_char,
        CPU: *const std::ffi::c_char,
        Features: *const std::ffi::c_char,
        Level: LLVMCodeGenOptLevel,
        Reloc: LLVMRelocMode,
        CodeModel: LLVMCodeModel,
    ) -> LLVMTargetMachineRef;
    pub unsafe fn LLVMDisposeTargetMachine(T: LLVMTargetMachineRef);

    pub unsafe fn LLVMGetTargetMachineTarget(T: LLVMTargetMachineRef) -> LLVMTargetRef;
    pub unsafe fn LLVMGetTargetMachineTriple(T: LLVMTargetMachineRef) -> *mut std::ffi::c_char;
    pub unsafe fn LLVMGetTargetMachineCPU(T: LLVMTargetMachineRef) -> *mut std::ffi::c_char;
    pub unsafe fn LLVMGetTargetMachineFeatureString(T: LLVMTargetMachineRef) -> *mut std::ffi::c_char;

    /// Create a DataLayout based on the target machine.
    pub unsafe fn LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
    pub unsafe fn LLVMSetTargetMachineAsmVerbosity(T: LLVMTargetMachineRef, VerboseAsm: LLVMBool);

    pub unsafe fn LLVMTargetMachineEmitToFile(
        T: LLVMTargetMachineRef,
        M: LLVMModuleRef,
        Filename: *const std::ffi::c_char,
        codegen: LLVMCodeGenFileType,
        ErrorMessage: *mut *mut std::ffi::c_char,
    ) -> LLVMBool;

    pub unsafe fn LLVMGetDefaultTargetTriple() -> *mut std::ffi::c_char;


    pub unsafe fn LLVMAddAnalysisPasses(T: LLVMTargetMachineRef, PM: LLVMPassManagerRef);
}
