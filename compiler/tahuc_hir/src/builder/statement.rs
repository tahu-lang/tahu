use tahuc_ast::nodes::{
    Expression,
    expressions::ExpressionKind,
    op::UnaryOp,
    statements::{Block, ElseBranch, IfStatement, Statement, StatementKind},
};

use crate::{
    builder::builder::HirBuilder,
    hir::{HirBlock, HirElseBranch, HirIfStatement, HirLValue, HirStatement, HirVariable},
};

impl<'a> HirBuilder<'a> {
    pub(crate) fn lower_block(&mut self, block: &Block) -> HirBlock {
        self.scope.next();

        let mut hir_statements: Vec<HirStatement> = Vec::new();
        for statement in &block.statements {
            let res = self.lower_statement(statement);
            match res.clone() {
                Ok(_) => {}
                Err(_) => {
                    println!("Error block {:?}", statement);
                    println!();
                }
            }
            hir_statements.push(res.unwrap());
        }

        self.scope.pop();

        HirBlock {
            statements: hir_statements,
        }
    }

    fn lower_statement(&mut self, statement: &Statement) -> Result<HirStatement, String> {
        match &statement.kind {
            StatementKind::Expression(expr) => {
                let hir_expr = self.lower_expression(expr)?;
                Ok(HirStatement::Expression(hir_expr))
            }
            StatementKind::Variable(var) => {
                let hir_var = HirVariable {
                    id: self.next_id_variable(),
                    name: var.name,
                    ty: self
                        .get_type(statement.id)
                        .unwrap_or(&var.variable_type)
                        .clone(),
                    span: var.span,
                };

                let initializer = if let Some(init) = &var.initializer {
                    Some(self.lower_expression(init)?)
                } else {
                    None
                };

                self.variables
                    .entry(self.scope.current())
                    .or_default()
                    .insert(var.name, hir_var.clone());

                Ok(HirStatement::Variable {
                    variable: hir_var,
                    initializer: initializer,
                })
            }
            StatementKind::Assignment { left, op, right } => {
                let left = self.get_left_value(left)?;
                let right = self.lower_expression(right)?;

                Ok(HirStatement::Assignment {
                    target: left,
                    op: op.clone(),
                    value: right,
                })
            }
            StatementKind::Return(ret) => {
                let return_value = if let Some(expr) = &ret {
                    Some(self.lower_expression(expr)?)
                } else {
                    None
                };

                Ok(HirStatement::Return {
                    value: return_value,
                })
            }
            StatementKind::IfStatement(if_stmt) => {
                Ok(HirStatement::If(self.lower_if_statement(if_stmt)))
            }
            StatementKind::WhileStatement(while_stmt) => {
                let condition = self.lower_expression(&while_stmt.condition)?;
                let body = self.lower_block(&while_stmt.body);

                Ok(HirStatement::While {
                    condition: condition,
                    body: Box::new(body),
                })
            }
            // StatementKind::ForStatement(for_stmt) => {}
            // StatementKind::ForInStatement(forin) => {}
            StatementKind::Continue => Ok(HirStatement::Continue),
            StatementKind::Break => Ok(HirStatement::Break),
            _ => Err("Not Support statement".to_string()),
        }
    }

    fn lower_if_statement(&mut self, if_stmt: &IfStatement) -> HirIfStatement {
        let condition = self.lower_expression(&if_stmt.condition).unwrap();
        let then_branch = self.lower_block(&if_stmt.then_branch);
        let else_branch = if let Some(else_branch) = &if_stmt.else_branch {
            let else_block = match else_branch {
                ElseBranch::If(if_stmt) => {
                    HirElseBranch::If(Box::new(self.lower_if_statement(if_stmt)))
                }
                ElseBranch::Block(block) => HirElseBranch::Block(self.lower_block(block)),
            };
            Some(else_block)
        } else {
            None
        };

        HirIfStatement {
            condition: condition,
            then_branch: then_branch,
            else_branch: else_branch,
        }
    }

    fn get_left_value(&mut self, left: &Expression) -> Result<HirLValue, String> {
        match &left.kind {
            ExpressionKind::ArrayAccess { array, index } => {
                let arr_ty = self.get_type(array.id).unwrap().clone();
                let ty = self.get_type(left.id).unwrap().clone();
                let arr = self.lower_expression(&array)?;
                let idx = self.lower_expression(&index)?;
                Ok(HirLValue::ArrayAccess {
                    array: Box::new(arr),
                    index: Box::new(idx),
                    arr_ty,
                    ty,
                    span: left.span,
                })
            }
            ExpressionKind::Identifier(id) => {
                for scope_id in self.scope.iter_rev() {
                    if let Some(scope) = self.variables.get(scope_id) {
                        if let Some(var) = scope.get(id) {
                            return Ok(HirLValue::Variable {
                                id: var.id,
                                ty: var.ty.clone(),
                                span: left.span,
                            });
                        }
                    }
                }
                let name = self.config.str_pool.get(*id);
                Err(format!("Left value identifier not found {} {} {}", name, id, self.file_id))
            }
            ExpressionKind::Unary { op, operand } => match op {
                UnaryOp::Deref => {
                    let operand = self.lower_expression(&operand)?;
                    Ok(HirLValue::Deref {
                        value: operand,
                        ty: self.get_type(left.id).unwrap().clone(),
                        span: left.span,
                    })
                }
                _ => Err(format!("Not Support unary expression {:?}", op)),
            },
            ExpressionKind::MemberAccess { .. } => {
                let result = self.lower_expression(&left)?;
                let l_result = result.to_l_value();
                if let Some(result) = l_result {
                    Ok(result)
                } else {
                    Err(format!("Not Support expression {:?}", l_result))
                }
            }
            _ => Err(format!("Not Support expression {:?}", left.kind)),
        }
    }
}
