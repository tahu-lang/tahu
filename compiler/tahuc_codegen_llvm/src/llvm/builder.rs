use std::ffi::CString;

use tahuc_llvm::{
    core::*,
    opaque::{LLVMBuilderRef, LLVMValueRef},
};

use crate::llvm::{
    AsValueRef, BasicBlock, BasicValue, FloatValue, FunctionValue, IntValue, LLVMCastKind, PhiValue, PointerValue, Type, Values
};

pub struct Builder {
    pub(crate) inner: LLVMBuilderRef,
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder());
        }
    }
}

impl Builder {
    pub fn new(llvm_builder: LLVMBuilderRef) -> Self {
        Self {
            inner: llvm_builder,
        }
    }

    pub fn ret_void(&mut self) -> Values {
        Values::new(unsafe { LLVMBuildRetVoid(self.builder()) })
    }

    pub fn ret(&mut self, value: BasicValue) -> Values {
        Values::new(unsafe { LLVMBuildRet(self.builder(), value.as_value_ref()) })
    }

    pub fn jump(&mut self, dest: BasicBlock) -> Values {
        Values::new(unsafe { LLVMBuildBr(self.builder(), dest.as_block_ref()) })
    }

    pub fn branch(
        &mut self,
        condition: BasicValue,
        then_branch: BasicBlock,
        else_branch: BasicBlock,
    ) -> Values {
        Values::new(unsafe {
            LLVMBuildCondBr(
                self.builder(),
                condition.as_value_ref(),
                then_branch.as_block_ref(),
                else_branch.as_block_ref(),
            )
        })
    }

    pub fn unreachable(&mut self) -> Values {
        Values::new(unsafe { LLVMBuildUnreachable(self.builder()) })
    }

    pub fn position_at_end(&mut self, block: BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.builder(), block.as_block_ref()) }
    }

    pub fn alloca(&mut self, ty: Type) -> PointerValue {
        let name = self.default_zero_name();

        let value = Values::new(unsafe { LLVMBuildAlloca(self.builder(), ty.as_type_ref(), name) });

        PointerValue::new(value)
    }

    pub fn load(&mut self, ty: Type, ptr: PointerValue) -> BasicValue {
        let name = self.default_zero_name();
        let value = Values::new(unsafe {
            LLVMBuildLoad2(self.builder(), ty.as_type_ref(), ptr.as_value_ref(), name)
        });

        BasicValue::new(value)
    }

    pub fn store(&mut self, ptr: PointerValue, value: BasicValue) {
        unsafe {
            LLVMBuildStore(self.builder(), value.as_value_ref(), ptr.as_value_ref());
        }
    }

    pub fn select(&mut self, cond: BasicValue, then: BasicValue, else_: BasicValue) -> BasicValue {
        let name = self.default_zero_name();
        let value = Values::new(unsafe {
            LLVMBuildSelect(
                self.builder(),
                cond.as_value_ref(),
                then.as_value_ref(),
                else_.as_value_ref(),
                name,
            )
        });

        BasicValue::new(value)
    }

    pub fn gep(&mut self, ty: Type, ptr: PointerValue, indices: &[IntValue]) -> PointerValue {
        let name = self.default_zero_name();
        let mut indices: Vec<LLVMValueRef> = indices.iter().map(|val| val.as_value_ref()).collect();
        let value = Values::new(unsafe {
            LLVMBuildInBoundsGEP2(
                self.builder(),
                ty.as_type_ref(),
                ptr.as_value_ref(),
                indices.as_mut_ptr(),
                indices.len() as u32,
                name,
            )
        });

        PointerValue::new(value)
    }

    pub fn call(&mut self, function: FunctionValue, args: &[BasicValue]) -> Values {
        let ty = function.get_ty();
        let func = function.as_value_ref();
        let mut args: Vec<LLVMValueRef> = args.iter().map(|val| val.as_value_ref()).collect();
        let name = self.default_zero_name();

        let value = Values::new(unsafe {
            LLVMBuildCall2(
                self.builder(),
                ty.as_type_ref(),
                func,
                args.as_mut_ptr(),
                args.len() as u32,
                name,
            )
        });

        value
    }

    pub fn cast(&mut self, op: LLVMCastKind, value: BasicValue, ty: Type) -> BasicValue {
        let name = self.default_zero_name();
        let value = Values::new(unsafe {
            LLVMBuildCast(
                self.builder(),
                op.as_llvm_opcode(),
                value.as_value_ref(),
                ty.as_type_ref(),
                name,
            )
        });

        BasicValue::new(value)
    }

    pub fn phi(&mut self, ty: Type) -> PhiValue {
        let name = self.default_zero_name();
        let value = Values::new(unsafe { LLVMBuildPhi(self.builder(), ty.as_type_ref(), name) });

        PhiValue::new(value)
    }

    // unary
    pub fn i_neg(&mut self, value: IntValue) -> IntValue {
        let name = self.default_zero_name();
        let value =
            Values::new(unsafe { LLVMBuildNeg(self.builder(), value.as_value_ref(), name) });

        IntValue::new(value)
    }

    pub fn f_neg(&mut self, value: FloatValue) -> FloatValue {
        let name = self.default_zero_name();
        let value = Values::new(unsafe { LLVMBuildFNeg(self.builder(), value.as_value_ref(), name) });

        FloatValue::new(value)
    }

    pub fn i_not(&mut self, value: IntValue) -> IntValue {
        let name = self.default_zero_name();
        let value =
            Values::new(unsafe { LLVMBuildNot(self.builder(), value.as_value_ref(), name) });

        IntValue::new(value)
    }

    pub(crate) fn builder(&mut self) -> LLVMBuilderRef {
        self.inner
    }

    pub(crate) fn default_zero_name(&self) -> *const i8 {
        let res = CString::new("").unwrap();
        res.as_ptr()
    }
}
