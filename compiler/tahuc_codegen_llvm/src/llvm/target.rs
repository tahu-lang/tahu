use std::{mem::MaybeUninit};

use tahuc_llvm::{
    target::*,
    target_machine::*,
};

use crate::llvm::Module;

pub enum OptLevel {
    None = 0,
    Less = 1,
    Default = 2,
    Aggressive = 3,
}

impl From<OptLevel> for LLVMCodeGenOptLevel {
    fn from(level: OptLevel) -> Self {
        match level {
            OptLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
            OptLevel::Less => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
            OptLevel::Default => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            OptLevel::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
        }
    }
}

pub enum CodeModel {
    Default,
    Small,
    Kernel,
    Medium,
    Large,
}

impl From<CodeModel> for LLVMCodeModel {
    fn from(value: CodeModel) -> Self {
        match value {
            CodeModel::Default => LLVMCodeModel::LLVMCodeModelDefault,
            CodeModel::Small => LLVMCodeModel::LLVMCodeModelSmall,
            CodeModel::Kernel => LLVMCodeModel::LLVMCodeModelKernel,
            CodeModel::Medium => LLVMCodeModel::LLVMCodeModelMedium,
            CodeModel::Large => LLVMCodeModel::LLVMCodeModelLarge,
        }
    }
}

pub enum RelocMode {
    Default,
    Static,
    PIC,
    DynamicNoPIC,
}

impl From<RelocMode> for LLVMRelocMode {
    fn from(value: RelocMode) -> Self {
        match value {
            RelocMode::Default => LLVMRelocMode::LLVMRelocDefault,
            RelocMode::Static => LLVMRelocMode::LLVMRelocStatic,
            RelocMode::PIC => LLVMRelocMode::LLVMRelocPIC,
            RelocMode::DynamicNoPIC => LLVMRelocMode::LLVMRelocDynamicNoPic,
        }
    }
}

pub enum FileType {
    Assembly,
    Object,
}

impl FileType {
    fn as_llvm_file_type(&self) -> LLVMCodeGenFileType {
        match *self {
            FileType::Assembly => LLVMCodeGenFileType::LLVMAssemblyFile,
            FileType::Object => LLVMCodeGenFileType::LLVMObjectFile,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CString {
    ptr: *const std::ffi::c_char,
}

impl CString {
    pub fn new(str: &str) -> Self {
        let ptr = std::ffi::CString::new(str).unwrap();
        Self { ptr: ptr.into_raw() }
    }

    pub fn from_raw(ptr: *mut i8) -> String {
        let str = unsafe { std::ffi::CString::from_raw(ptr) };
        str.to_string_lossy().into_owned()
    }

    pub fn as_ptr(&self) -> *const std::ffi::c_char {
        self.ptr
    }

    pub fn to_string(&self) -> String {
        let str = unsafe { std::ffi::CStr::from_ptr(self.ptr) };
        str.to_string_lossy().into_owned()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TargetTriple {
    triple: CString,
}

impl TargetTriple {
    pub fn new(triple: &str) -> Self {
        Self {
            triple: CString::new(triple),
        }
    }

    pub fn create(triple: &str) -> Self {
        Self {
            triple: CString::new(triple),
        }
    }

    pub fn default() -> Self {
        let triple = unsafe { LLVMGetDefaultTargetTriple() };
        Self {
            triple: CString::new(
                unsafe { std::ffi::CStr::from_ptr(triple) }
                    .to_string_lossy()
                    .as_ref(),
            ),
        }
    }
}

pub struct Target {
    target: LLVMTargetRef,
}

impl Target {
    pub fn new(target: LLVMTargetRef) -> Self {
        Self { target }
    }

    pub fn from_triple(triple: TargetTriple) -> Self {
        let mut target = std::ptr::null_mut();
        let mut err_string = MaybeUninit::uninit();

        unsafe {
            LLVMGetTargetFromTriple(triple.triple.as_ptr(), &mut target, err_string.as_mut_ptr())
        };
        Target::new(target)
    }

    pub fn init_x86() {
        Init::x86();
    }

    pub fn init_arm() {
        Init::arm();
    }

    pub fn init_arch64() {
        Init::arch64();
    }

    pub fn init_amdgpu() {
        Init::amdgpu();
    }

    pub fn init_loongarch() {
        Init::loongarch();
    }

    pub fn init_mips() {
        Init::mips();
    }

    pub fn init_wasm() {
        Init::wasm();
    }

    pub fn create_target_machine(
        &self,
        triple: TargetTriple,
        cpu: &str,
        features: &str,
        opt_level: OptLevel,
        reloc_mode: RelocMode,
        code_model: CodeModel,
    ) -> TargetMachine {
        let cpu = CString::new(cpu);
        let features = CString::new(features);

        let target = unsafe {
            LLVMCreateTargetMachine(
                self.target,
                triple.triple.as_ptr(),
                cpu.as_ptr(),
                features.as_ptr(),
                opt_level.into(),
                reloc_mode.into(),
                code_model.into(),
            )
        };

        TargetMachine::new(target)
    }
}

struct Init;

impl Init {
    pub fn x86() {
        unsafe {
            LLVMInitializeX86TargetInfo();
            LLVMInitializeX86Target();
            LLVMInitializeX86TargetMC();
            LLVMInitializeX86AsmPrinter();
            LLVMInitializeX86AsmParser();
        }
    }

    pub fn arm() {
        unsafe {
            LLVMInitializeARMTargetInfo();
            LLVMInitializeARMTarget();
            LLVMInitializeARMTargetMC();
            LLVMInitializeARMAsmPrinter();
            LLVMInitializeARMAsmParser();
        }
    }

    pub fn arch64() {
        unsafe {
            LLVMInitializeAArch64TargetInfo();
            LLVMInitializeAArch64Target();
            LLVMInitializeAArch64TargetMC();
            LLVMInitializeAArch64AsmPrinter();
            LLVMInitializeAArch64AsmParser();
        }
    }

    pub fn amdgpu() {
        unsafe {
            LLVMInitializeAMDGPUTargetInfo();
            LLVMInitializeAMDGPUTarget();
            LLVMInitializeAMDGPUTargetMC();
            LLVMInitializeAMDGPUAsmPrinter();
            LLVMInitializeAMDGPUAsmParser();
        }
    }

    pub fn loongarch() {
        unsafe {
            LLVMInitializeLoongArchTargetInfo();
            LLVMInitializeLoongArchTarget();
            LLVMInitializeLoongArchTargetMC();
            LLVMInitializeLoongArchAsmPrinter();
            LLVMInitializeLoongArchAsmParser();
        }
    }

    pub fn mips() {
        unsafe {
            LLVMInitializeMipsTargetInfo();
            LLVMInitializeMipsTarget();
            LLVMInitializeMipsTargetMC();
            LLVMInitializeMipsAsmPrinter();
            LLVMInitializeMipsAsmParser();
        }
    }

    pub fn wasm() {
        unsafe {
            LLVMInitializeWebAssemblyTargetInfo();
            LLVMInitializeWebAssemblyTarget();
            LLVMInitializeWebAssemblyTargetMC();
            LLVMInitializeWebAssemblyAsmPrinter();
            LLVMInitializeWebAssemblyAsmParser();
        }
    }
}

pub struct TargetMachine {
    machine: LLVMTargetMachineRef,
}

impl TargetMachine {
    pub fn new(machine: LLVMTargetMachineRef) -> Self {
        Self { machine }
    }

    pub fn emit_to_file(
        &self,
        module: Module,
        filename: &str,
        file_type: FileType,
    ) -> Result<(), String> {
        let filename = CString::new(filename);
        let mut error: *mut std::ffi::c_char = std::ptr::null_mut();

        let result = unsafe {
            LLVMTargetMachineEmitToFile(
                self.machine,
                module.into(),
                filename.as_ptr() as *mut std::ffi::c_char,
                file_type.as_llvm_file_type(),
                &mut error,
            )
        };

        if result == 0 {
            Ok(())
        } else {
            let err_msg = unsafe { std::ffi::CStr::from_ptr(error) }
                .to_string_lossy()
                .into_owned();
            Err(err_msg)
        }
    }
}

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe { LLVMDisposeTargetMachine(self.machine) };
    }
}
