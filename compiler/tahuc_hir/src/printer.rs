use crate::hir::{HirBlock, HirElseBranch, HirExpression, HirIfStatement, HirLValue, HirModule, HirStatement};

pub struct HirPrinter {
    is_last: Vec<bool>,
}

impl HirPrinter {
    pub fn new() -> Self {
        Self {
            is_last: Vec::new(),
        }
    }

    fn get_prefix(&self) -> String {
        let mut prefix = String::new();

        for (i, &is_last) in self.is_last.iter().enumerate() {
            if i == self.is_last.len() - 1 {
                if is_last {
                    prefix.push_str("└── ");
                } else {
                    prefix.push_str("├── ");
                }
            } else {
                if is_last {
                    prefix.push_str("    ");
                } else {
                    prefix.push_str("│   ");
                }
            }
        }

        prefix
    }

    fn print_node(&mut self, name: &str) {
        println!("{}{}", self.get_prefix(), name);
    }

    fn with_child<F>(&mut self, is_last: bool, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.is_last.push(is_last);
        f(self);
        self.is_last.pop();
    }

    pub fn print_module(&mut self, module: &HirModule) {
        self.print_node("Module");
        self.with_child(true, |printer| {
            for (i, ty) in module.structs.iter().enumerate() {
                let is_last = i == module.structs.len() - 1;
                printer.with_child(is_last, |printer| {
                    printer.print_node(&format!("Struct: {}", &ty.name));
                    printer.with_child(true, |printer| {
                        printer.print_node("Fields");
                        for (i, field) in ty.fields.iter().enumerate() {
                            let is_last = i == ty.fields.len() - 1;
                            printer.with_child(is_last, |printer| {
                                printer.print_node(&format!("Field: {}", &field.name));
                                printer.with_child(true, |printer| {
                                    printer.print_node(&format!("Type: {}", &field.ty));
                                });
                            });
                        }
                    });
                });
            }
        });
        self.with_child(true, |printer| {
            for (i, function) in module.extern_functions.iter().enumerate() {
                let is_last = i == module.functions.len() - 1;
                printer.with_child(is_last, |printer| {
                    printer.print_node(&format!("Extern Function: {} -> {}", &function.name, function.return_type));
                    printer.with_child(true, |printer| {
                        printer.print_node("Parameters");
                        printer.with_child(true, |printer| {
                            for parameter in &function.parameters {
                                printer.print_node(&format!("Parameter: {}", &parameter.name));
                                printer.with_child(true, |printer| {
                                    printer.print_node(&format!("Type: {}", &parameter.ty));
                                });
                            }
                        });
                    });
                });
            }
        });
        self.with_child(true, |printer| {
            for (i, function) in module.functions.iter().enumerate() {
                let is_last = i == module.functions.len() - 1;
                printer.with_child(is_last, |printer| {
                    printer.print_node(&format!("Function: {} -> {}", &function.name, &function.return_type));
                    printer.with_child(false, |printer| {
                        printer.print_node("Parameters");
                        for (i, parameter) in function.parameters.iter().enumerate() {
                            let is_last = i == function.parameters.len() - 1;
                            printer.with_child(is_last, |printer| {
                                printer.print_node(&format!("Parameter: {}", &parameter.name));
                                printer.with_child(true, |printer| {
                                    printer.print_node(&format!("Type: {}", &parameter.ty));
                                });
                            });
                        }
                    });

                    printer.with_child(true, |printer| {
                        printer.print_node("Blocks");
                        printer.print_block(&function.body);
                    });
                });
            }
        });
    }

    fn print_block(&mut self, block: &HirBlock) {
        for (i, stmt) in block.statements.iter().enumerate() {
            let is_last = i == block.statements.len() - 1;
            self.with_child(is_last, |printer| {
                printer.print_statement(stmt);
            });
        }
    }

    fn print_statement(&mut self, statement: &HirStatement) {
        self.print_node("Statement");
        self.with_child(true, |printer| {
            match statement {
                HirStatement::Expression(expr) => {
                    printer.print_expression(expr);
                }
                HirStatement::Variable { variable, initializer } => {
                    printer.print_node(&format!("Variable {}: {}", variable.name, variable.ty));
                    if let Some(init) = initializer {
                        printer.with_child(true, |printer| {
                            printer.print_expression(init);
                        });
                    }
                }
                HirStatement::Assignment { target, op, value } => {
                    printer.print_node(&format!("Assigment {:?} ", op));
                    printer.with_child(true, |printer| {
                        match target {
                            HirLValue::Variable { id, .. } => {
                                printer.print_node(&format!("Var {}", id));
                                printer.with_child(true, |printer| {
                                    printer.print_expression(value);
                                });
                            }
                            HirLValue::ArrayAccess { array, index, .. } => {
                                printer.print_expression(&array);
                                printer.with_child(true, |printer| {
                                    printer.print_expression(&index);
                                    printer.with_child(true, |printer| {
                                        printer.print_expression(value);
                                    });
                                });
                            }
                            _ => {}
                        }
                    });
                }
                HirStatement::Return { value } => {
                    printer.print_node("Return");
                    if let Some(value) = value {
                        printer.with_child(true, |printer| {
                            printer.print_expression(value);
                        });
                    }
                }
                HirStatement::If(if_stmt) => {
                    printer.print_node("If Statement");

                    printer.with_child(false, |printer| {
                        printer.print_node("Condition");
                        printer.with_child(true, |printer| {
                            printer.print_expression(&if_stmt.condition);
                        });
                    });

                    printer.with_child(if_stmt.else_branch.is_none(), |printer| {
                        printer.print_node("Then");
                        printer.print_block(&if_stmt.then_branch);
                    });

                    if let Some(_) = &if_stmt.else_branch {
                        printer.with_child(true, |printer| {
                            printer.print_node("Else");
                            if let Some(else_branch) = &if_stmt.else_branch {
                                match else_branch {
                                    HirElseBranch::Block(block) => {
                                        printer.print_block(&block);
                                    }
                                    HirElseBranch::If(chained_if) => {
                                        printer.print_if_statement(&chained_if);
                                    }
                                }
                            }
                        });
                    }
                },
                // HirStatement::If(if_stmt) => printer.print_if_statement(if_stmt),
                HirStatement::While { condition, body } => {
                    printer.print_node("While Statement");
                    printer.with_child(false, |printer| {
                        printer.print_node("Condition");
                        printer.with_child(true, |printer|{
                            printer.print_expression(condition);
                        });
                    });
                    printer.with_child(true, |printer| {
                        printer.print_node("Body");
                        printer.print_block(body);
                    });
                }
                HirStatement::Continue => {
                    printer.print_node("Continue");
                }
                HirStatement::Break => {
                    printer.print_node("Break");
                }
                // _ => {
                //     self.print_node("Unsupported Statement");
                // }
            }
        });
    }

    fn print_if_statement(&mut self, if_stmt: &HirIfStatement) {
        self.with_child(true, |printer| {
            printer.print_node("If Statement");

            printer.with_child(false, |printer| {
                printer.print_node("Condition");
                printer.with_child(true, |printer| {
                    printer.print_expression(&if_stmt.condition);
                });
            });

            printer.with_child(if_stmt.else_branch.is_none(), |printer| {
                printer.print_node("Then");
                printer.print_block(&if_stmt.then_branch);
            });

            if let Some(_) = &if_stmt.else_branch {
                printer.with_child(true, |printer| {
                    printer.print_node("Else");
                    if let Some(else_branch) = &if_stmt.else_branch {
                        match else_branch {
                            HirElseBranch::Block(block) => {
                                printer.print_block(&block);
                            }
                            HirElseBranch::If(chained_if) => {
                                printer.print_if_statement(&chained_if);
                            }
                        }
                    }
                });
            }
        });
    }

    fn print_expression(&mut self, expression: &HirExpression) {
        match expression {
            HirExpression::Literal { value, .. } => {
                self.print_node(&format!("Literal {:?}", value));
            }
            HirExpression::Variable { id, ty, .. } => {
                self.print_node(&format!("Variable {}: {}", id, ty));
            }
            HirExpression::Ternary { condition, then_branch, else_branch, .. } => {
                self.print_node("Ternary");
                self.with_child(false, |printer| {
                    printer.print_expression(condition);
                });
                self.with_child(false, |printer| {
                    printer.print_expression(then_branch);
                });
                self.with_child(true, |printer| {
                    printer.print_expression(else_branch);
                });
            }
            HirExpression::Binary { left, op, right, .. } => {
                self.print_node(&format!("Binary {:?}", op));
                self.with_child(false, |printer| {
                    printer.print_expression(left);
                });
                self.with_child(true, |printer| {
                    printer.print_expression(right);
                });
            }
            HirExpression::Unary { op, operand, .. } => {
                self.print_node(&format!("Unary {:?}", op));
                self.with_child(true, |printer| {
                    printer.print_expression(operand);
                });
            }
            HirExpression::ArrayLiteral { elements, .. } => {
                self.print_node("Array Literal");
                for (i, expr) in elements.iter().enumerate() {
                    let is_last = i == elements.len() - 1;
                    self.with_child(is_last, |printer| {
                        printer.print_expression(expr);
                    });
                }
            }
            HirExpression::ArrayAccess { array, index, .. } => {
                self.print_node("Array Access");
                self.with_child(true, |printer| {
                    printer.print_expression(array);
                    printer.with_child(true, |printer| {
                        printer.print_expression(index);
                    });
                });
            }
            HirExpression::Call { callee, arguments, ty, .. } => {
                self.print_node("Call");

                self.with_child(false, |printer| {
                    printer.print_node("Function");
                    printer.with_child(true, |printer| {
                        printer.print_node(&format!("{} -> {}", callee, ty));
                    });
                });

                self.with_child(true, |printer| {
                    if !arguments.is_empty() {
                        printer.print_node("Arguments");
                        for (i, arg) in arguments.iter().enumerate() {
                            let is_last = i == arguments.len() - 1;
                            printer.with_child(is_last, |printer| {
                                printer.print_expression(arg);
                            });
                        }
                    } else {
                        printer.print_node("Arguments: (none)");
                    }
                });
            }
            HirExpression::StructLiteral { object, fields, .. } => {
                self.print_node("Struct Literal");
                self.with_child(false, |printer| {
                    printer.print_expression(object);
                });
                self.with_child(true, |printer| {
                    printer.print_node("Fields");
                    for (i, (_id, expr)) in fields.iter().enumerate() {
                        let is_last = i == fields.len() - 1;
                        printer.with_child(is_last, |printer| {
                            printer.print_expression(expr);
                        });
                    }
                });
            }
            HirExpression::FieldAccess { object, field_name, field_id, ty, .. } => {
                self.print_node(&format!("Field Access: {} ({}) -> {}", field_name, field_id, ty));
                self.with_child(true, |printer| {
                    printer.print_expression(object);
                });
            }
            HirExpression::MemberAccess { object, member, ty, .. } => {
                self.print_node(&format!("Member Access: {} -> {}", member, ty));
                self.with_child(true, |printer| {
                    printer.print_expression(object);
                });
            }
            HirExpression::Grouping { value, ty, .. } => {
                self.print_node(&format!("Grouping: {}", ty));
                self.with_child(true, |printer| {
                    printer.print_expression(value);
                });
            }
            HirExpression::Cast { value, from, ty, .. } => {
                self.print_node(&format!("Cast: {} -> {}", from, ty));
                self.with_child(true, |printer| {
                    printer.print_expression(value);
                });
            }
            _ => {}
        }
    }

}