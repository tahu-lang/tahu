use core::panic;

use tahuc_ast::nodes::op::{BinaryOp, UnaryOp};
use tahuc_mir::mir::{
    block::{MirBasicBlock, MirTerminator},
    instruction::{CastKind, MirInstruction, MirOperand},
    ty::{MirType, MirTypeKind},
};

use crate::{
    codegen::Codegen,
    llvm::{BasicBlock, BasicValue, FloatValue, IntType, IntValue, LLVMCastKind},
};

impl Codegen {
    pub(crate) fn compile_basic_block(&mut self, block: &MirBasicBlock) {
        let bb = self.basic_blocks_map[&block.id];
        self.builder.position_at_end(bb);

        for instruction in &block.instructions {
            self.compile_instruction(instruction);
        }

        self.compile_terminator(&block.terminator);
    }

    pub(crate) fn compile_instruction(&mut self, instruction: &MirInstruction) {
        match &instruction {
            MirInstruction::Alloca { target, ty } => {
                let ty = self.ty_to_llvm(ty);
                let ptr = self.builder.alloca(ty);

                self.add_ptr(*target, ptr);
            }
            MirInstruction::Load { target, ptr, ty } => {
                let ty = self.ty_to_llvm(ty);
                let ptr = self.operand_ptr(ptr).unwrap();
                let value = self.builder.load(ty, ptr);

                self.add_value(*target, value);
            }
            MirInstruction::Store { ptr, value, .. } => {
                let ptr = self.operand_ptr(ptr).unwrap();
                let value = self.operand_value(value).unwrap();

                self.builder.store(ptr, value);
            }
            MirInstruction::GetElementPtr {
                target,
                base,
                indices,
                inner_ty,
                ..
            } => {
                let ty = self.ty_to_llvm(inner_ty);
                let ptr = self.operand_ptr(base).unwrap();
                let binding: Result<Vec<IntValue>, _> = indices
                    .iter()
                    .map(|val| self.operand_value(val).map(|a| a.into_int_value()))
                    .collect();
                let indices = binding.unwrap();

                let result = self.builder.gep(ty, ptr, indices.as_slice());

                self.add_ptr(*target, result);
            }
            MirInstruction::Binary {
                target,
                left,
                op,
                right,
                ty,
            } => {
                let left_val = self.operand_value(left).unwrap();
                let right_val = self.operand_value(right).unwrap();

                let value = self.build_binary_op(op, left_val, right_val, ty).unwrap();

                self.add_value(*target, value);
            }
            MirInstruction::Unary {
                target,
                op,
                value,
                ty,
            } => {
                let value = self.build_unary(op, value, ty);

                self.add_value(*target, value);
            }
            MirInstruction::Call {
                target,
                function,
                arguments,
                ..
            } => {
                let func = self.functions_map[function];

                let args = arguments
                    .iter()
                    .map(|arg| self.operand_value(arg).unwrap())
                    .collect::<Vec<_>>();

                let call_result = self.builder.call(func, &args);

                if !call_result.is_void() {
                    let value = BasicValue::new(call_result);
                    self.add_value(*target, value);
                }
            }
            MirInstruction::Phi { target, values, ty } => {
                let ty = self.ty_to_llvm(ty);
                let mut phi = self.builder.phi(ty);

                let incomings: Vec<(BasicValue, BasicBlock)> = values
                    .iter()
                    .map(|(v, bb)| (self.operand_value(v).unwrap(), self.basic_blocks_map[bb]))
                    .collect();

                phi.add_incoming(incomings);

                let basic_value = BasicValue::new(phi.as_value());

                self.add_value(*target,basic_value);
            }
            MirInstruction::Select {
                target,
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.operand_value(condition).unwrap();
                let then = self.operand_value(then_branch).unwrap();
                let else_branch = self.operand_value(else_branch).unwrap();

                let value = self.builder.select(cond, then, else_branch);

                self.add_value(*target, value);
            }
            MirInstruction::Cast {
                target,
                kind,
                value,
                to,
                ..
            } => {
                let op = self.cast_op_to_llvm(kind);
                let value = self.operand_value(value).unwrap();
                let to = self.ty_to_llvm(to);

                let value = self.builder.cast(op, value, to);

                self.add_value(*target, value);
            }
        }
    }

    fn cast_op_to_llvm(&self, op: &CastKind) -> LLVMCastKind {
        match op {
            CastKind::Trunc => LLVMCastKind::Trunc,
            CastKind::ZExt => LLVMCastKind::ZExt,
            CastKind::SExt => LLVMCastKind::SExt,
            CastKind::FPToUI => LLVMCastKind::FPToUI,
            CastKind::FPToSI => LLVMCastKind::FPToSI,
            CastKind::UIToFP => LLVMCastKind::UIToFP,
            CastKind::SIToFP => LLVMCastKind::SIToFP,
            CastKind::PtrToInt => LLVMCastKind::PtrToInt,
            CastKind::IntToPtr => LLVMCastKind::IntToPtr,
            CastKind::BitCast => LLVMCastKind::BitCast,
            CastKind::NoOp => panic!("NoOp cast should not generate code"),
        }
    }

    pub(crate) fn compile_terminator(&mut self, terminator: &MirTerminator) {
        match terminator {
            MirTerminator::Return { value } => {
                if let Some(value) = value {
                    let value = self.operand_value(value).unwrap();
                    self.builder.ret(value);
                } else {
                    self.builder.ret_void();
                }
            }
            MirTerminator::Jump { target } => {
                let block = self.basic_blocks_map[target];
                self.builder.jump(block);
            }
            MirTerminator::Branch {
                condition,
                true_target,
                false_target,
            } => {
                let condition = self.operand_value(condition).unwrap();
                let then_branch = self.basic_blocks_map[true_target];
                let else_branch = self.basic_blocks_map[false_target];

                self.builder.branch(condition, then_branch, else_branch);
            }
            MirTerminator::Unreachable => {
                self.builder.unreachable();
            }
        }
    }

    fn build_binary_op(
        &mut self,
        op: &BinaryOp,
        left: BasicValue,
        right: BasicValue,
        ty: &MirType,
    ) -> Result<BasicValue, String> {
        match ty.kind() {
            MirTypeKind::SignedInt => {
                let result =
                    self.build_int_bin_op(op, left.into_int_value(), right.into_int_value(), true);
                Ok(result.into())
            }
            MirTypeKind::UnsignedInt => {
                let result =
                    self.build_int_bin_op(op, left.into_int_value(), right.into_int_value(), false);
                Ok(result.into())
            }
            MirTypeKind::Float => {
                let result =
                    self.build_float_bin_op(op, left.into_float_value(), right.into_float_value());
                Ok(result.into())
            }
            MirTypeKind::Bool => {
                // boolean ops: And/Or
                match op {
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::Le
                    | BinaryOp::Gt
                    | BinaryOp::Ge => {
                        match ty.kind() {
                            MirTypeKind::Bool => {
                                // hasilnya bool, tapi operandnya numeric
                                if left.is_int_value() {
                                    let lhs = left.into_int_value();
                                    let rhs = right.into_int_value();
                                    let res = self.build_int_bin_op(op, lhs, rhs, true); // or false for unsigned
                                    Ok(res.into())
                                } else if left.is_float_value() {
                                    let lhs = left.into_float_value();
                                    let rhs = right.into_float_value();
                                    let res = self.build_float_cmp(op, lhs, rhs); // kamu bikin helper khusus float cmp
                                    Ok(res.into())
                                } else {
                                    Err("Invalid operand type for comparison".to_string())
                                }
                            }
                            _ => Err("Comparison must return bool".to_string()),
                        }
                    }
                    BinaryOp::And => {
                        let result = self
                            .builder
                            .and(left.into_int_value(), right.into_int_value());
                        Ok(result.into())
                    }
                    BinaryOp::Or => {
                        let result = self
                            .builder
                            .or(left.into_int_value(), right.into_int_value());
                        Ok(result.into())
                    }
                    // comparison: reuse signed int predicates
                    _ => {
                        let target_ty = self.ty_to_llvm(ty);
                        let left_val = left.into_int_value();
                        let right_val = right.into_int_value();

                        let left = self.cast_int_to(left_val, target_ty.into_int_ty(), true)?;
                        let right = self.cast_int_to(right_val, target_ty.into_int_ty(), true)?;
                        let result = self.build_int_bin_op(
                            op,
                            left.into_int_value(),
                            right.into_int_value(),
                            false,
                        );
                        Ok(result.into())
                    }
                }
                // match op {
                //     // Ok(self.build_int_bin_op(&self.builder, op, left.into_int_value(), right.into_int_value(), false).into()),
                // }
            }
            _ => Err(format!(
                "Unsupported binary operation {:?} for type {:?}",
                op, ty
            )),
        }
    }

    fn build_int_bin_op(
        &mut self,
        op: &BinaryOp,
        left: IntValue,
        right: IntValue,
        signed: bool,
    ) -> IntValue {
        use BinaryOp::*;
        match op {
            Add => self.builder.i_add(left, right),
            Sub => self.builder.i_sub(left, right),
            Mul => self.builder.i_mul(left, right),
            Div => self.builder.i_div(left, right, signed),
            Rem => self.builder.i_rem(left, right, signed),
            Eq => self.builder.i_eq(left, right),
            Ne => self.builder.i_ne(left, right),
            Lt => self.builder.i_lt(left, right, signed),
            Le => self.builder.i_le(left, right, signed),
            Gt => self.builder.i_gt(left, right, signed),
            Ge => self.builder.i_ge(left, right, signed),
            Shl => self.builder.i_shl(left, right),
            Shr => self.builder.i_shr(left, right, signed),
            BitAnd => self.builder.and(left, right),
            _ => panic!("Unsupported int op {:?}", op),
        }
    }

    fn build_float_bin_op(
        &mut self,
        op: &BinaryOp,
        left: FloatValue,
        right: FloatValue,
    ) -> FloatValue {
        use BinaryOp::*;
        match op {
            Add => self.builder.f_add(left, right),
            Sub => self.builder.f_sub(left, right),
            Mul => self.builder.f_mul(left, right),
            Div => self.builder.f_div(left, right),
            _ => panic!("Unsupported float op {:?}", op),
        }
    }

    fn build_float_cmp(
        &mut self,
        op: &BinaryOp,
        left: FloatValue,
        right: FloatValue,
    ) -> FloatValue {
        use BinaryOp::*;
        match op {
            Eq => self.builder.f_eq(left, right),
            Ne => self.builder.f_ne(left, right),
            Lt => self.builder.f_lt(left, right),
            Le => self.builder.f_le(left, right),
            Gt => self.builder.f_gt(left, right),
            Ge => self.builder.f_ge(left, right),
            _ => panic!("Unsupported float comparison op {:?}", op),
        }
    }

    fn cast_int_to(
        &mut self,
        val: IntValue,
        target_ty: IntType,
        signed: bool,
    ) -> Result<BasicValue, String> {
        let val_bits = val.get_ty().get_bit_width();
        let target_bits = target_ty.get_bit_width();
        let basic = BasicValue::new(val.as_value());
        if val_bits < target_bits {
            if signed {
                let result = self
                    .builder
                    .cast(LLVMCastKind::SExt, basic, target_ty.as_ty());
                Ok(result)
            } else {
                let result = self
                    .builder
                    .cast(LLVMCastKind::ZExt, basic, target_ty.as_ty());
                Ok(result)
            }
        } else if val_bits > target_bits {
            let result = self
                .builder
                .cast(LLVMCastKind::Trunc, basic, target_ty.as_ty());
            Ok(result)
        } else {
            Ok(basic)
        }
    }

    fn build_unary(&mut self, op: &UnaryOp, value: &MirOperand, ty: &MirType) -> BasicValue {
        match op {
            UnaryOp::Minus => {
                let value = self.operand_value(value).unwrap();

                let result = if ty.clone().is_integer() {
                    let value = self
                    .builder
                    .i_neg(value.into_int_value());
                    BasicValue::new(value.as_value())
                } else if ty.clone().is_float() {
                    let value = self
                    .builder
                    .f_neg(value.into_float_value());
                    BasicValue::new(value.as_value())
                } else {
                    panic!("Unsupported unary operation {:?}", op)
                };

                result
            }
            UnaryOp::Not => {
                let value = self.operand_value(value).unwrap();
                let result = self
                    .builder
                    .i_not(value.into_int_value());
                BasicValue::new(result.as_value())
            }
            _ => panic!("Unsupported unary operation {:?}", op),
        }
    }
}
