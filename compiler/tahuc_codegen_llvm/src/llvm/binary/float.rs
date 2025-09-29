use tahuc_llvm::{
    LLVMRealPredicate,
    core::{
        LLVMBuildFAdd, LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFRem, LLVMBuildFSub,
    },
};

use crate::llvm::{Builder, FloatValue, Values};

impl Builder {
    pub fn f_add(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        let value = Values::new(unsafe {
            LLVMBuildFAdd(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        FloatValue::new(value)
    }

    pub fn f_sub(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        let value = Values::new(unsafe {
            LLVMBuildFSub(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        FloatValue::new(value)
    }

    pub fn f_mul(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        let value = Values::new(unsafe {
            LLVMBuildFMul(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        FloatValue::new(value)
    }

    pub fn f_div(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        let value = Values::new(unsafe {
            LLVMBuildFDiv(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        FloatValue::new(value)
    }

    pub fn f_rem(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        let value = Values::new(unsafe {
            LLVMBuildFRem(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        FloatValue::new(value)
    }

    fn build_float_compare(
        &mut self,
        op: LLVMRealPredicate,
        lhs: FloatValue,
        rhs: FloatValue,
    ) -> FloatValue {
        let value = Values::new(unsafe {
            LLVMBuildFCmp(
                self.builder(),
                op,
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        FloatValue::new(value)
    }

    pub fn f_eq(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        self.build_float_compare(LLVMRealPredicate::LLVMRealOEQ, lhs, rhs)
    }

    pub fn f_ne(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        self.build_float_compare(LLVMRealPredicate::LLVMRealONE, lhs, rhs)
    }

    pub fn f_lt(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        self.build_float_compare(LLVMRealPredicate::LLVMRealOLT, lhs, rhs)
    }

    pub fn f_le(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        self.build_float_compare(LLVMRealPredicate::LLVMRealOLE, lhs, rhs)
    }

    pub fn f_gt(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        self.build_float_compare(LLVMRealPredicate::LLVMRealOGT, lhs, rhs)
    }

    pub fn f_ge(&mut self, lhs: FloatValue, rhs: FloatValue) -> FloatValue {
        self.build_float_compare(LLVMRealPredicate::LLVMRealOGE, lhs, rhs)
    }
}
