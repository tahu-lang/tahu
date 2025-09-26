use tahuc_hir::hir::{HirElseBranch, HirStatement};

use crate::{builder::builder::{Builder, Termination}, mir::{ty::ToMirType, BasicBlockId}};

pub(crate) mod assignment;
pub(crate) mod variable;

impl Builder {
    pub(crate) fn build_statement(&mut self, statement: &HirStatement) -> Termination {
        match statement {
            HirStatement::Expression(expr) => {
                self.build_expression(expr, false);
                Termination::Fallthrough
            }
            HirStatement::Variable {
                variable,
                initializer,
            } => {
                self.build_variable_statement(variable, initializer);
                Termination::Fallthrough
            }
            HirStatement::Assignment { target, op, value } => {
                self.build_assigment_stmt(target, op.clone(), value);

                Termination::Fallthrough
            }
            HirStatement::Return { value } => {
                let value = if let Some(value) = value {
                    let need_addr = value.get_type().to_mir_ty().is_aggregate();
                    let value = self.build_expression(value, need_addr);
                    Some(value)
                } else {
                    None
                };

                self.emit_return(value);
                Termination::Terminated
            }
            HirStatement::If(if_stmt) => {
                let condition = self.build_expression(&if_stmt.condition, false);
                let then_block = self.new_block("if_then");
                let else_block = if let Some(_) = &if_stmt.else_branch {
                    Some(self.new_block("if_else"))
                } else {
                    None
                };
                let merge_block = self.new_block("if_merge");

                // if dont have any else fallback to merge
                let false_target = else_block.unwrap_or(merge_block);

                // set condition terminator
                self.emit_branch(condition, then_block, false_target);
                self.switch_to_block(then_block);
                let termination = self.build_block(&if_stmt.then_branch);
                if !termination.is_terminated() {
                    self.emit_jump(merge_block);
                }

                if let Some(else_branch) = &if_stmt.else_branch {
                    self.switch_to_block(else_block.unwrap());
                    self.build_if_statement(else_branch, merge_block);
                } else {
                    self.switch_to_block(merge_block);
                }

                self.switch_to_block(merge_block);

                Termination::Fallthrough
            }
            HirStatement::While { condition, body } => {
                let loop_header = self.new_block("loop_header");
                let loop_body = self.new_block("loop_body");
                let loop_exit = self.new_block("loop_exit");

                self.new_loop(loop_exit, loop_header);
                self.emit_jump(loop_header);
                self.switch_to_block(loop_header);
                let condition = self.build_expression(condition, false);
                self.emit_branch(condition, loop_body, loop_exit);

                self.switch_to_block(loop_body);
                let termination = self.build_block(body);
                if !termination.is_terminated() {
                    self.emit_jump(loop_header);
                }

                self.pop_loop();
                self.switch_to_block(loop_exit);

                Termination::Fallthrough
            }
            HirStatement::Continue => {
                self.jump_to_continue();
                Termination::Terminated
            }
            HirStatement::Break => {
                self.jump_to_break();
                Termination::Terminated
            }
        }
    }

    fn build_if_statement(&mut self, else_branch: &HirElseBranch, merge_block: BasicBlockId) {
        match else_branch {
            HirElseBranch::If(if_stmt) => {
                let condition = self.build_expression(&if_stmt.condition, false);
                let then_block = self.new_block("if_then");
                let else_block = if let Some(_) = if_stmt.else_branch {
                    Some(self.new_block("if_else"))
                } else {
                    None
                };
                let false_target = else_block.unwrap_or(merge_block);

                self.emit_branch(condition, then_block, false_target);
                self.switch_to_block(then_block);
                let termination = self.build_block(&if_stmt.then_branch);
                if !termination.is_terminated() {
                    self.emit_jump(merge_block);
                }

                if let Some(else_branch) = &if_stmt.else_branch {
                    self.switch_to_block(else_block.unwrap());
                    self.build_if_statement(&else_branch, merge_block);
                }
            }
            HirElseBranch::Block(block) => {
                let termination = self.build_block(block);
                if !termination.is_terminated() {
                    self.emit_jump(merge_block);   
                }
            }
        }
    }
}
