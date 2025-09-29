use tahuc_llvm::{
    core::{LLVMGetTypeKind, LLVMTypeOf}, opaque::{LLVMTypeRef, LLVMValueRef}, LLVMTypeKind
};

mod enums;
mod float;
mod fn_value;
mod int;
mod pointer_value;
mod array;
mod struct_value;
mod global;
mod phi;

pub use enums::*;
pub use float::*;
pub use fn_value::*;
pub use int::*;
pub use pointer_value::*;
pub use array::*;
pub use struct_value::*;
pub use global::*;
pub use phi::*;

pub trait AsValueRef {
    fn as_value_ref(&self) -> LLVMValueRef;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Values {
    value: LLVMValueRef,
}

impl Values {
    pub fn new(value: LLVMValueRef) -> Self {
        Self { value }
    }

    pub fn into(self) -> LLVMValueRef {
        self.value
    }

    pub fn kind(self) -> LLVMTypeKind {
        unsafe { LLVMGetTypeKind(LLVMTypeOf(self.value)) }
    }

    fn get_type(self) -> LLVMTypeRef {
        unsafe { LLVMTypeOf(self.value) }
    }

    pub fn is_void(self) -> bool {
        matches!(self.kind(), LLVMTypeKind::LLVMVoidTypeKind)
    }
}
