use std::collections::HashMap;

use tahuc_ast::{
    Module, Type,
    nodes::{
        Expression,
        declarations::{Declaration, DeclarationKind},
        expressions::{Argument, ExpressionKind},
        statements::{Block, Statement, StatementKind, Variable},
    },
};

use crate::{
    database::Database,
    error::SemanticError,
    symbol::{Symbol, SymbolKind, VariableSymbol},
};

pub struct ControlFlowAnalyzer<'a> {
    db: &'a mut Database,
    current_function_name: Option<String>,
    current_function_return_type: Type,

    // Track definite assignment status for variables
    assigned_vars: HashMap<String, bool>,
    // Track unreachable code
    is_unreachable: bool,
}

impl<'a> ControlFlowAnalyzer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            current_function_name: None,
            current_function_return_type: Type::Inferred,
            assigned_vars: HashMap::new(),
            is_unreachable: false,
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();

        for declaration in &module.declaration {
            self.analyze_declaration(declaration);
        }
    }

    fn add_variable(&mut self, var: &Variable) -> Result<(), String> {
        let symbol = Symbol {
            name: var.name.clone(),
            span: var.span,
            kind: SymbolKind::Variable(VariableSymbol {
                name: var.name.clone(),
                declared_type: var.variable_type.clone(),
                need_inferred: var.variable_type.is_inferred(),
                inferred_type: None,
                initializer: None,
                final_type: var.variable_type.clone(),
            }),
        };

        self.db.add_symbol(symbol)
    }

    fn analyze_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                self.current_function_name = Some(func.kind.name.clone());
                self.current_function_return_type = func.kind.return_type.clone();
                self.assigned_vars.clear();
                self.is_unreachable = false;

                if let Some(_) = self.db.lookup_symbol(func.kind.name.clone()) {
                    self.db.enter_scope();

                    for param in &func.kind.parameters {
                        self.assigned_vars.insert(param.kind.name.clone(), true);
                    }

                    let _ = self.analyze_block(&func.kind.body);

                    self.db.exit_scope();
                }

                self.current_function_name = None;
                self.current_function_return_type = Type::Inferred;
            }
            DeclarationKind::Variable(_) => {}
            _ => {}
        }
    }

    fn analyze_block(&mut self, block: &Block) -> bool {
        let mut has_return = false;

        for statement in &block.statements {
            if self.is_unreachable {
                self.db.report_error(SemanticError::Unreachable {
                    span: statement.span.merge(block.statements.last().unwrap().span),
                });
                // println!("Warning: Unreachable code detected after return statement");
                break;
            }

            if self.analyze_statement(statement) {
                has_return = true;
                self.is_unreachable = true;
            }
        }

        has_return
    }

    fn analyze_statement(&mut self, statement: &Statement) -> bool {
        match &statement.kind {
            StatementKind::Variable(var) => {
                // // Check if variable name already exists in current scope
                // if let Some(existing) = self.db.lookup_symbol_current_scope(var.name.clone()) {
                //     println!("Error: Variable '{}' is already declared in this scope", var.name);
                // }

                if var.initializer.is_some() {
                    self.assigned_vars.insert(var.name.clone(), true);
                    if let Some(init) = &var.initializer {
                        self.analyze_expression(init);
                    }
                } else {
                    self.assigned_vars.insert(var.name.clone(), false);
                }
                let _ = self.add_variable(var);
                false
            }

            StatementKind::Expression(expr) => {
                self.analyze_expression(expr);
                false
            }

            StatementKind::Return(ret_expr) => {
                if let Some(expr) = ret_expr {
                    self.analyze_expression(expr);
                }
                true // This path returns
            }

            // StatementKind::Block(block) => {
            //     // Save current state for block scope
            //     let saved_vars = self.assigned_vars.clone();
            //     let saved_unreachable = self.is_unreachable;

            //     self.db.enter_scope();
            //     let has_return = self.analyze_block(block);
            //     self.db.exit_scope();

            //     // Restore state after block
            //     self.assigned_vars = saved_vars;
            //     self.is_unreachable = saved_unreachable;

            //     has_return
            // }
            _ => false,
        }
    }

    fn analyze_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::Identifier(ident) => {
                // Check definite assignment
                if let Some(&is_assigned) = self.assigned_vars.get(ident) {
                    if !is_assigned {
                        println!("Error: Use of uninitialized variable '{}'", ident);
                    }
                } else if self.db.lookup_symbol(ident.clone()).is_some() {
                    // Variable exists but not tracked locally (probably from outer scope)
                    // This is fine for variables declared outside current function
                } else {
                    // Variable doesn't exist - should have been caught in symbol resolution
                    println!("Error: Undefined variable '{}'", ident);
                }
            }

            // ExpressionKind::Assignment { left, right, .. } => {
            //     // Analyze right side first
            //     self.analyze_expression(right);

            //     // Handle assignment to identifier
            //     if let ExpressionKind::Identifier(var_name) = &left.kind {
            //         // Check if variable exists
            //         if let Some(symbol) = self.db.lookup_symbol(var_name.clone()) {
            //             if let Some(var_symbol) = symbol.get_variable() {
            //                 // Check mutability
            //                 if !var_symbol.name.is_empty() {
            //                     // placeholder for is_mutable check
            //                     self.assigned_vars.insert(var_name.clone(), true);
            //                 } else {
            //                     // println!(
            //                     //     "Error: Cannot assign to immutable variable '{}'",
            //                     //     var_name
            //                     // );
            //                     self.db.report_error(SemanticError::CannotAssignImmutable {
            //                         name: var_name.clone(),
            //                         span: left.span.merge(right.span),
            //                     });
            //                 }
            //             }
            //         } 
            //         // else {
            //         //     // println!("Error: Cannot assign to undefined variable '{}'", var_name);
            //         // }
            //     }

                // self.analyze_expression(left);
            // }

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
    fn checkpoint_assignments(&self) -> HashMap<String, bool> {
        self.assigned_vars.clone()
    }

    /// Restore assignment state from checkpoint
    fn restore_assignments(&mut self, checkpoint: HashMap<String, bool>) {
        self.assigned_vars = checkpoint;
    }

    /// Merge assignment states from different branches
    fn merge_assignment_states(
        &mut self,
        state1: &HashMap<String, bool>,
        state2: &HashMap<String, bool>,
    ) {
        for (var_name, &assigned1) in state1 {
            let assigned2 = state2.get(var_name).copied().unwrap_or(false);
            // Variable is definitely assigned only if assigned in BOTH branches
            self.assigned_vars
                .insert(var_name.clone(), assigned1 && assigned2);
        }

        // Also check variables that might be in state2 but not state1
        for (var_name, &_) in state2 {
            if !state1.contains_key(var_name) {
                self.assigned_vars.insert(var_name.clone(), false);
            }
        }
    }
}
