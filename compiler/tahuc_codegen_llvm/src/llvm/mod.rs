mod basic_block;
mod binary;
mod builder;
mod context;
mod module;
mod types;
mod values;
mod target;
mod cast;
mod call;

pub use basic_block::*;
pub use builder::*;
pub use context::*;
pub use module::*;
pub use types::*;
pub use values::*;
pub use target::*;
pub use cast::*;
pub use call::*;

pub enum Visibility {
    /// default
    Public,

    /// private or hidden
    Private,

    /// protected
    Protected,
}

impl Visibility {
    pub fn as_llvm_visibility(self) -> tahuc_llvm::LLVMVisibility {
        match self {
            Visibility::Public => tahuc_llvm::LLVMVisibility::LLVMDefaultVisibility,
            Visibility::Private => tahuc_llvm::LLVMVisibility::LLVMHiddenVisibility,
            Visibility::Protected => tahuc_llvm::LLVMVisibility::LLVMProtectedVisibility,
        }
    }
}

pub enum Linkage {
    /// external
    External,

    /// available externally
    AvailableExternally,

    /// link once
    LinkOnce,

    /// weak
    Weak,

    /// appending
    Appending,

    /// internal
    Internal,

    /// private
    Private,

    /// external weak
    ExternalWeak,

    /// common
    Common,
}

impl Linkage {
    pub fn as_llvm_linkage(self) -> tahuc_llvm::LLVMLinkage {
        match self {
            Linkage::External => tahuc_llvm::LLVMLinkage::LLVMExternalLinkage,
            Linkage::AvailableExternally => tahuc_llvm::LLVMLinkage::LLVMAvailableExternallyLinkage,
            Linkage::LinkOnce => tahuc_llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage,
            Linkage::Weak => tahuc_llvm::LLVMLinkage::LLVMWeakAnyLinkage,
            Linkage::Appending => tahuc_llvm::LLVMLinkage::LLVMAppendingLinkage,
            Linkage::Internal => tahuc_llvm::LLVMLinkage::LLVMInternalLinkage,
            Linkage::Private => tahuc_llvm::LLVMLinkage::LLVMPrivateLinkage,
            Linkage::ExternalWeak => tahuc_llvm::LLVMLinkage::LLVMExternalWeakLinkage,
            Linkage::Common => tahuc_llvm::LLVMLinkage::LLVMCommonLinkage,
        }
    }
}