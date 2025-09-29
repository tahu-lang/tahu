use tahuc_llvm::core::{LLVMBuildAnd, LLVMBuildOr, LLVMBuildXor};

use crate::llvm::{Builder, IntValue, Values};

mod float;
mod int;

impl Builder {
    pub fn and(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildAnd(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    pub fn or(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildOr(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    pub fn xor(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildXor(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }
}