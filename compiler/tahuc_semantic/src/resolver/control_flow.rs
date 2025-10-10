use std::{collections::HashMap, rc::Rc};

use tahuc_ast::{
    nodes::{
        ast::NodeId, declarations::{Declaration, DeclarationKind}, expressions::{Argument, ExpressionKind}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind}, Expression
    }, ty::Type, Module
};
use tahuc_module::config::Config;
use tahuc_span::{FileId, StrId};

use crate::{
    database::{database::Database, symbol::{FunctionSymbol, Symbol}},
    error::SemanticError,
};

#[derive(Debug, Clone, Copy)]
enum ControlFlowStatus {
    Fallthrough,
    Continue,
    Break,
    Return,
}

impl ControlFlowStatus {
    pub fn is_unreachable(&self) -> bool {
        matches!(self, ControlFlowStatus::Break | ControlFlowStatus::Return)
    }

    pub fn is_break(&self) -> bool {
        matches!(self, ControlFlowStatus::Break)
    }

    pub fn is_return(&self) -> bool {
        matches!(self, ControlFlowStatus::Return)
    }
}

pub struct ControlFlowAnalyzer<'a> {
    config: &'a Config<'a>,
    db: &'a mut Database,
    file_id: FileId,
    current_function_name: Option<StrId>,
    current_function_return_type: Rc<Type>,
    is_optional_return: bool,
    has_return: bool,

    // Track definite assignment status for variables
    assigned_vars: HashMap<StrId, (Rc<Type>, bool)>,
    // Track unreachable code
    flow_status: ControlFlowStatus,
}

impl<'a> ControlFlowAnalyzer<'a> {
    pub fn new(config: &'a Config, db: &'a mut Database) -> Self {
        Self {
            config,
            db,
            file_id: FileId(0),
            current_function_name: None,
            current_function_return_type: Rc::new(Type::Inferred),
            is_optional_return: false,
            has_return: false,
            assigned_vars: HashMap::new(),
            flow_status: ControlFlowStatus::Fallthrough,
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();
        self.file_id = module.file;

        for declaration in &module.declaration {
            self.analyze_declaration(declaration);
        }
    }

    fn analyze_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                self.current_function_name = Some(func.kind.name);
                self.current_function_return_type = func.kind.return_type.clone();
                self.is_optional_return = func.kind.return_type.is_unit();
                self.assigned_vars.clear();
                self.has_return = false;
                self.flow_status = ControlFlowStatus::Fallthrough;

                if let Some(_) = self.lookup_function(func.kind.name) {
                    self.db.enter_scope();

                    for param in &func.kind.parameters {
                        let ty = self.get_type(param.id).unwrap().clone();
                        self.assigned_vars.insert(param.kind.name, (ty, true));
                    }

                    let _ = self.analyze_block(&func.kind.body, false);

                    self.db.exit_scope();
                }

                if !self.is_optional_return {
                    if !self.has_return {
                        self.db.report_error(SemanticError::MissingReturn {
                            name: self.config.str_pool.get(func.kind.name).to_string(),
                            span: func.span,
                        });
                    }
                }

                self.current_function_name = None;
                self.current_function_return_type = Rc::new(Type::Inferred);
            }
            DeclarationKind::Variable(_) => {}
            _ => {}
        }
    }

    fn analyze_block(&mut self, block: &Block, can_flow: bool) -> ControlFlowStatus {
        // let mut has_return = false;
        let mut current_status = ControlFlowStatus::Fallthrough;

        for statement in &block.statements {
            match &self.flow_status {
                ControlFlowStatus::Break => {
                    self.db.report_error(SemanticError::Unreachable {
                        message: format!("Unreachable code detected after break statement"),
                        span: statement.span
                    });
                    // break;
                }
                ControlFlowStatus::Return => {
                    self.db.report_error(SemanticError::Unreachable {
                        message: format!("Unreachable code detected after return statement"),
                        span: statement.span
                    });
                    // break;
                }
                _ => {
                    let status =  self.analyze_statement(statement, can_flow);
                    
                    current_status = status;
                }
            }

            // if self.analyze_statement(statement) {
            //     has_return = true;
            //     self.is_unreachable = true;
            // }
        }

        // has_return
        current_status
    }

    fn analyze_statement(&mut self, statement: &Statement, can_flow: bool) -> ControlFlowStatus {
        match &statement.kind {
            StatementKind::Variable(var) => {
                // // Check if variable name already exists in current scope
                // if let Some(existing) = self.db.lookup_symbol_current_scope(var.name.clone()) {
                //     println!("Error: Variable '{}' is already declared in this scope", var.name);
                // }

                let ty = self.get_type(statement.id).unwrap().clone();
                if var.initializer.is_some() {
                    self.assigned_vars.insert(var.name.clone(), (ty, true));
                    if let Some(init) = &var.initializer {
                        self.analyze_expression(init);
                    }
                } else {
                    self.assigned_vars.insert(var.name.clone(), (ty, false));
                }
                // let _ = self.add_variable(var);
                ControlFlowStatus::Fallthrough
            }

            StatementKind::Assignment { left, right, .. } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
                ControlFlowStatus::Fallthrough
            }

            StatementKind::Expression(expr) => {
                self.analyze_expression(expr);
                ControlFlowStatus::Fallthrough
            }

            StatementKind::Return(ret_expr) => {
                if let Some(expr) = ret_expr {
                    self.analyze_expression(expr);
                }
                self.set_status(ControlFlowStatus::Return);
                self.has_return = true;
                ControlFlowStatus::Return
            }
            StatementKind::IfStatement(if_stmt) => self.analyze_if_statement(if_stmt, can_flow),
            StatementKind::WhileStatement(while_stmt) => {
                self.analyze_expression(&while_stmt.condition);
                // let status = 
                self.analyze_block(&while_stmt.body, true);
                // if status.is_unreachable() {
                //     self.set_status(status);
                //     return status;
                // }
                ControlFlowStatus::Fallthrough
            }
            StatementKind::Continue => {
                if !can_flow {
                    self.db.report_error(SemanticError::InvalidContinue {
                        span: statement.span,
                    });
                    return ControlFlowStatus::Fallthrough;
                }

                self.set_status(ControlFlowStatus::Continue);
                ControlFlowStatus::Continue
            }
            StatementKind::Break => {
                if !can_flow {
                    self.db.report_error(SemanticError::InvalidBreak {
                        span: statement.span,
                    });
                    return ControlFlowStatus::Fallthrough;
                }

                self.set_status(ControlFlowStatus::Break);
                ControlFlowStatus::Break
            },
            _ => ControlFlowStatus::Fallthrough,
        }
    }

    fn analyze_if_statement(&mut self, if_stmt: &IfStatement, can_flow: bool) -> ControlFlowStatus {
        self.analyze_expression(&if_stmt.condition);
        let saved_status = self.flow_status;
        let mut status_state1 = ControlFlowStatus::Fallthrough;
        let mut status_state2 = ControlFlowStatus::Fallthrough;
        
        
        let saved_state = self.checkpoint_assignments();
        let status = self.analyze_block(&if_stmt.then_branch, can_flow);
        let state1 = self.checkpoint_assignments();
        if status.is_unreachable() {
            status_state1 = status;
        }

        self.reset_status(saved_status);

        self.restore_assignments(saved_state);
        if let Some(else_block) = &if_stmt.else_branch {
            let status = self.analyze_else_branch(else_block, can_flow);
            let state2 = self.checkpoint_assignments();
            if status.is_unreachable() {
                status_state2 = status;
            }
            self.merge_assignment_states(&state1, &state2);
        }

        if status_state1.is_return() && status_state2.is_return() {
            self.set_status(ControlFlowStatus::Return);
            return ControlFlowStatus::Return;
        } else if status_state1.is_break() && status_state2.is_break() {
            self.set_status(ControlFlowStatus::Break);
            return ControlFlowStatus::Break;
        }

        ControlFlowStatus::Fallthrough
    }

    fn analyze_else_branch(&mut self, else_branch: &ElseBranch, can_flow: bool) -> ControlFlowStatus {
        match else_branch {
            ElseBranch::Block(block) => self.analyze_block(block, true),
            ElseBranch::If(if_stmt) => self.analyze_if_statement(if_stmt, can_flow),
        }
    }

    fn analyze_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::Identifier(ident) => {
                // Check definite assignment
                if let Some((ty, is_assigned)) = self.assigned_vars.get(ident).cloned() {
                    if !is_assigned {
                        if !ty.is_array() {
                            let name = self.config.str_pool.get(*ident);
                            self.db.report_error(SemanticError::UninitializedVariable {
                                name: name.to_string(),
                                span: expression.span,
                            });
                        }
                    }
                } else if self.get_symbol(expression.id).is_some() {
                    // Variable exists but not tracked locally (probably from outer scope)
                    // This is fine for variables declared outside current function
                } else {
                    // Variable doesn't exist - should have been caught in symbol resolution
                    // println!("Error: Undefined variable '{}'", ident);
                }
            }

            ExpressionKind::Binary { left, right, .. } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
            }

            ExpressionKind::Unary { operand, .. } => {
                self.analyze_expression(operand);
            }

            ExpressionKind::FunctionCall(call) => {
                self.analyze_expression(&call.function);

                for arg in &call.arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.analyze_expression(expr);
                        }
                        Argument::Named { value, .. } => {
                            self.analyze_expression(value);
                        }
                    }
                }
            }

            ExpressionKind::MemberAccess { object, .. } => {
                self.analyze_expression(object);
            }

            ExpressionKind::ArrayAccess { array, index } => {
                self.analyze_expression(array);
                self.analyze_expression(index);
            }

            ExpressionKind::Ternary {
                condition,
                then,
                otherwise,
            } => {
                self.analyze_expression(condition);

                // Save state before analyzing branches
                let saved_state = self.checkpoint_assignments();

                // Analyze then branch
                self.analyze_expression(then);
                let then_state = self.checkpoint_assignments();

                // Restore and analyze else branch
                self.restore_assignments(saved_state);
                self.analyze_expression(otherwise);
                let else_state = self.checkpoint_assignments();

                // Merge states from both branches
                self.merge_assignment_states(&then_state, &else_state);
            }

            ExpressionKind::Grouping(grouped) => {
                self.analyze_expression(grouped);
            }

            ExpressionKind::Literal(_) => {
                // Literals don't need analysis
            }
            _ => {}
        }
    }

    /// Create a checkpoint of current assignment state
    fn checkpoint_assignments(&self) -> HashMap<StrId, (Rc<Type>, bool)> {
        self.assigned_vars.clone()
    }

    /// Restore assignment state from checkpoint
    fn restore_assignments(&mut self, checkpoint: HashMap<StrId, (Rc<Type>, bool)>) {
        self.assigned_vars = checkpoint;
    }

    /// Merge assignment states from different branches
    fn merge_assignment_states(
        &mut self,
        state1: &HashMap<StrId, (Rc<Type>, bool)>,
        state2: &HashMap<StrId, (Rc<Type>, bool)>,
    ) {
        for (var_name, (ty1, assigned1)) in state1.clone() {
            let (_, assigned2) = state2.get(&var_name).cloned().unwrap_or((Rc::new(Type::Inferred), false));
            // if ty1 != ty2 {
            //     self.db.report_error(SemanticError::IncompatibleTypes {
            //         span: None,
            //         ty1: ty1.clone(),
            //         ty2: ty2.clone(),
            //     });
            // }
            // Variable is definitely assigned only if assigned in BOTH branches
            self.assigned_vars
                .insert(var_name, (ty1, assigned1 && assigned2));
        }

        // Also check variables that might be in state2 but not state1
        for (var_name, &_) in state2 {
            if !state1.contains_key(var_name) {
                self.assigned_vars.insert(*var_name, (Rc::new(Type::Inferred), false));
            }
        }
    }

    fn set_status(&mut self, status: ControlFlowStatus) {
        if !self.flow_status.is_unreachable() {
            self.flow_status = status;
        }
    }

    fn reset_status(&mut self, status: ControlFlowStatus) {
        self.flow_status = status;
    }


    // fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
    //     self.db.add_symbol(self.file_id, symbol)
    // }

    fn get_symbol(&mut self, id: NodeId) -> Option<&Symbol> {
        self.db.get_symbol(self.file_id, id)
    }

    fn lookup_function(&mut self, name: StrId) -> Option<&FunctionSymbol> {
        self.db.lookup_fn(self.file_id, name)
    }

    fn get_type(&mut self, id: NodeId) -> Option<&Rc<Type>> {
        self.db.get_type(self.file_id, id)
    }

    // fn lookup_symbol(&mut self, name: StrId) -> Option<Symbol> {
    //     self.db.lookup_symbol(self.file_id, name)
    // }
}
