use std::collections::HashMap;

use tahuc_ast::Type;
use tahuc_hir::hir::{HirBlock, HirLiteral, HirModule, VariableId};

use crate::mir::{
    BasicBlockId, LocalId, MirModule,
    block::MirTerminator,
    function::{MirExternFunction, MirFunction},
    instruction::MirOperand,
    ty::{MirConstant, MirType, ToMirType},
};

/// Loop context
pub(crate) struct LoopContext {
    /// jump to break block
    ///
    /// in loop jump to exit loop
    pub(crate) break_block: BasicBlockId,

    /// jump to continue block
    ///
    /// in loop jump to loop header
    pub(crate) continue_block: BasicBlockId,
}

/// Termination context for loop context
#[derive(Debug, Clone)]
pub(crate) enum Termination {
    /// terminated if return, break, continue
    Terminated,

    /// nothing termination
    Fallthrough,
}

impl Termination {
    pub fn is_terminated(&self) -> bool {
        match self {
            Termination::Terminated => true,
            Termination::Fallthrough => false,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Variable {
    pub(crate) id: LocalId,
    pub(crate) ty: MirType,
    pub(crate) inner_ty: Option<MirType>,
    pub(crate) ctx: VariableContext,
}

#[derive(Debug, Clone)]
pub(crate) enum VariableContext {
    Parameter,
    ParameterNullable,
    Nullable,
    Local,
}

impl VariableContext {
    pub fn is_nullable(&self) -> bool {
        match self {
            VariableContext::Nullable => true,
            VariableContext::ParameterNullable => true,
            _ => false,
        }
    }

    pub fn is_parameter(&self) -> bool {
        match self {
            VariableContext::Parameter => true,
            VariableContext::ParameterNullable => true,
            _ => false,
        }
    }
}

pub struct Builder {
    /// current function
    pub(crate) current_function: Option<MirFunction>,

    /// current block id
    pub(crate) current_block: Option<BasicBlockId>,

    /// tracking variable mapping
    pub(crate) variable_map: HashMap<VariableId, Variable>,

    /// Looping stack
    pub(crate) loop_stack: Vec<LoopContext>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            current_function: None,
            current_block: None,
            variable_map: HashMap::new(),
            loop_stack: Vec::new(),
        }
    }

    pub fn build_module(&mut self, hir_module: &HirModule) -> MirModule {
        let mut extern_functions = Vec::new();
        let mut functions = Vec::new();

        for function in hir_module.functions.iter() {
            functions.push(self.build_function(function));
        }

        for function in hir_module.extern_functions.iter() {
            extern_functions.push(MirExternFunction {
                id: function.id,
                name: function.name.clone(),
                return_type: function.return_type.to_mir_ty(),
                parameters: function
                    .parameters
                    .iter()
                    .map(|p| p.ty.to_mir_ty())
                    .collect(),
            });
        }

        MirModule {
            file_id: hir_module.file_id,
            functions,
            extern_functions,
        }
    }

    pub(crate) fn build_block(&mut self, block: &HirBlock) -> Termination {
        for statement in block.statements.iter() {
            let termination = self.build_statement(statement);
            if termination.is_terminated() {
                return termination;
            }
        }

        Termination::Fallthrough
    }

    pub(crate) fn set_fn(&mut self, func: MirFunction) {
        self.current_function = Some(func);
    }

    /// reset state for new function
    pub(crate) fn reset_state(&mut self) {
        self.current_function = None;
        self.current_block = None;
        self.variable_map.clear();
        self.loop_stack.clear();
    }

    pub(crate) fn build_literal(&mut self, literal: &HirLiteral) -> MirConstant {
        match literal {
            HirLiteral::String(s) => MirConstant::String(s.clone()),
            HirLiteral::Char(c) => MirConstant::Char(*c),
            HirLiteral::Integer(i) => MirConstant::Integer(*i),
            HirLiteral::Float(d) => MirConstant::Float(*d),
            HirLiteral::Boolean(b) => MirConstant::Boolean(*b),
            HirLiteral::Null(ty) => match ty {
                Type::Nullable(inner) => MirConstant::Null(inner.to_mir_ty()),
                _ => MirConstant::Null(ty.to_mir_ty()),
            },
        }
    }

    pub(crate) fn new_loop(&mut self, break_block: BasicBlockId, continue_block: BasicBlockId) {
        self.loop_stack.push(LoopContext {
            break_block,
            continue_block,
        });
    }

    pub(crate) fn pop_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub(crate) fn jump_to_continue(&mut self) {
        if let Some(ctx) = self.loop_stack.last() {
            self.emit_jump(ctx.continue_block);
        }
    }

    pub(crate) fn jump_to_break(&mut self) {
        if let Some(ctx) = self.loop_stack.last() {
            self.emit_jump(ctx.break_block);
        }
    }

    pub(crate) fn new_variabel_nullable(
        &self,
        id: LocalId,
        ty: MirType,
        inner: Option<MirType>,
    ) -> Variable {
        Variable {
            id,
            ty,
            inner_ty: inner,
            ctx: VariableContext::Nullable,
        }
    }

    pub(crate) fn add_variable(&mut self, var_id: VariableId, local_id: LocalId, ty: MirType) {
        self.variable_map.insert(
            var_id,
            Variable {
                id: local_id,
                ty: ty,
                inner_ty: None,
                ctx: VariableContext::Local,
            },
        );
    }

    pub(crate) fn add_variable_cfg(&mut self, var_id: VariableId, variable: Variable) {
        self.variable_map.insert(var_id, variable);
    }

    pub(crate) fn read_variable(&self, var_id: VariableId) -> Variable {
        self.variable_map
            .get(&var_id)
            .expect("Variable not found")
            .clone()
    }

    pub(crate) fn new_local(&mut self, ty: MirType) -> LocalId {
        let func = self.current_function.as_mut().unwrap();
        func.new_local(ty)
    }

    pub(crate) fn new_block(&mut self, name: &str) -> BasicBlockId {
        let func = self.current_function.as_mut().unwrap();
        func.new_block(name)
    }

    pub(crate) fn switch_to_block(&mut self, block_id: BasicBlockId) {
        self.current_block = Some(block_id);
    }

    pub(crate) fn current_block(&self) -> BasicBlockId {
        self.current_block.expect("No current block set")
    }

    pub(crate) fn set_terminator(&mut self, terminator: MirTerminator) {
        let block_id = self.current_block();
        let func = self.current_function.as_mut().unwrap();
        if let Some(block) = func.get_block_mut(block_id) {
            block.set_terminator(terminator.clone());
        }

        // Update edges
        match &terminator {
            MirTerminator::Jump { target } => {
                let func = self.current_function.as_mut().unwrap();
                func.add_edge(block_id, *target);
            }
            MirTerminator::Branch {
                true_target,
                false_target,
                ..
            } => {
                let func = self.current_function.as_mut().unwrap();
                func.add_edge(block_id, *true_target);
                func.add_edge(block_id, *false_target);
            }
            _ => {}
        }
    }

    pub(crate) fn emit_return(&mut self, value: Option<MirOperand>) {
        self.set_terminator(MirTerminator::Return { value });
    }

    pub(crate) fn emit_branch(
        &mut self,
        condition: MirOperand,
        true_target: BasicBlockId,
        false_target: BasicBlockId,
    ) {
        self.set_terminator(MirTerminator::Branch {
            condition,
            true_target,
            false_target,
        });
    }

    pub(crate) fn emit_jump(&mut self, target: BasicBlockId) {
        self.set_terminator(MirTerminator::Jump { target });
    }
}
