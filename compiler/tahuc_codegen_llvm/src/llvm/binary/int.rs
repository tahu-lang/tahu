use tahuc_llvm::{
    LLVMIntPredicate,
    core::{
        LLVMBuildAShr, LLVMBuildAdd, LLVMBuildExactSDiv, LLVMBuildICmp, LLVMBuildLShr,
        LLVMBuildMul, LLVMBuildSDiv, LLVMBuildSRem, LLVMBuildShl, LLVMBuildSub, LLVMBuildURem,
    },
};

use crate::llvm::{Builder, IntValue, Values};

impl Builder {
    /// int add
    pub fn i_add(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildAdd(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    pub fn i_sub(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildSub(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    pub fn i_mul(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildMul(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    // todo safe div
    pub fn i_div(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        // todo call safe_div
        let value = Values::new(unsafe {
            if is_signed {
                LLVMBuildSDiv(
                    self.builder(),
                    lhs.as_value_ref(),
                    rhs.as_value_ref(),
                    self.default_zero_name(),
                )
            } else {
                LLVMBuildExactSDiv(
                    self.builder(),
                    lhs.as_value_ref(),
                    rhs.as_value_ref(),
                    self.default_zero_name(),
                )
            }
        });

        IntValue::new(value)
    }

    pub fn i_rem(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        let value = Values::new(unsafe {
            if is_signed {
                LLVMBuildSRem(
                    self.builder(),
                    lhs.as_value_ref(),
                    rhs.as_value_ref(),
                    self.default_zero_name(),
                )
            } else {
                LLVMBuildURem(
                    self.builder(),
                    lhs.as_value_ref(),
                    rhs.as_value_ref(),
                    self.default_zero_name(),
                )
            }
        });

        IntValue::new(value)
    }

    fn build_icmp(
        &mut self,
        op: tahuc_llvm::LLVMIntPredicate,
        lhs: IntValue,
        rhs: IntValue,
    ) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildICmp(
                self.builder(),
                op,
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    pub fn i_eq(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        self.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs)
    }

    pub fn i_lt(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        if is_signed {
            self.build_icmp(LLVMIntPredicate::LLVMIntSLT, lhs, rhs)
        } else {
            self.build_icmp(LLVMIntPredicate::LLVMIntULT, lhs, rhs)
        }
    }

    pub fn i_le(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        if is_signed {
            self.build_icmp(LLVMIntPredicate::LLVMIntSLE, lhs, rhs)
        } else {
            self.build_icmp(LLVMIntPredicate::LLVMIntULE, lhs, rhs)
        }
    }

    pub fn i_ne(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        self.build_icmp(LLVMIntPredicate::LLVMIntNE, lhs, rhs)
    }

    pub fn i_ge(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        if is_signed {
            self.build_icmp(LLVMIntPredicate::LLVMIntSGE, lhs, rhs)
        } else {
            self.build_icmp(LLVMIntPredicate::LLVMIntUGE, lhs, rhs)
        }
    }

    pub fn i_gt(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        if is_signed {
            self.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs, rhs)
        } else {
            self.build_icmp(LLVMIntPredicate::LLVMIntUGT, lhs, rhs)
        }
    }

    pub fn i_shl(&mut self, lhs: IntValue, rhs: IntValue) -> IntValue {
        let value = Values::new(unsafe {
            LLVMBuildShl(
                self.builder(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                self.default_zero_name(),
            )
        });

        IntValue::new(value)
    }

    pub fn i_shr(&mut self, lhs: IntValue, rhs: IntValue, is_signed: bool) -> IntValue {
        let value = Values::new(unsafe {
            if is_signed {
                LLVMBuildAShr(
                    self.builder(),
                    lhs.as_value_ref(),
                    rhs.as_value_ref(),
                    self.default_zero_name(),
                )
            } else {
                LLVMBuildLShr(
                    self.builder(),
                    lhs.as_value_ref(),
                    rhs.as_value_ref(),
                    self.default_zero_name(),
                )
            }
        });

        IntValue::new(value)
    }
}
