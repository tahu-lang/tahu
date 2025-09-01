use tahuc_ast::{
    Module, Type,
    nodes::{
        Expression,
        declarations::{Declaration, DeclarationKind},
        expressions::ExpressionKind,
        op::{AssignmentOp, BinaryOp, UnaryOp},
        statements::{Statement, StatementKind},
    },
};
use tahuc_lexer::token::Literal;

use crate::{
    database::Database,
    error::SemanticError,
    symbol::{Symbol, SymbolKind, VariableSymbol},
};

pub struct TypeAnalyzer<'a> {
    db: &'a mut Database,
    current_function_return_type: Type,
}

impl<'a> TypeAnalyzer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            current_function_return_type: Type::Inferred,
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();

        for declaration in &module.declaration {
            self.resolve_declaration(declaration);
        }
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                if let Some(_) = self.db.lookup_symbol(func.kind.name.clone()) {
                    self.current_function_return_type = func.kind.return_type.clone();

                    self.db.enter_scope();

                    for p in func.kind.parameters.iter() {
                        if let Err(_) = self.db.add_symbol(Symbol {
                            name: p.kind.name.clone(),
                            span: p.span,
                            kind: SymbolKind::Variable(VariableSymbol {
                                name: p.kind.name.clone(),
                                declared_type: p.kind.r#type.clone(),
                                initializer: None,

                                need_inferred: false,
                                inferred_type: None,
                                final_type: p.kind.r#type.clone(),
                            }),
                        }) {
                            let _ = self
                                .db
                                .add_symbol(Symbol {
                                    name: p.kind.name.clone(),
                                    span: p.span,
                                    kind: SymbolKind::Variable(VariableSymbol {
                                        name: p.kind.name.clone(),
                                        declared_type: Type::Error,
                                        initializer: None,

                                        need_inferred: false,
                                        inferred_type: None,
                                        final_type: Type::Error,
                                    }),
                                })
                                .ok();
                        }
                    }

                    func.kind.body.statements.iter().for_each(|statement| {
                        self.resolve_statement(statement);
                    });

                    self.db.exit_scope();
                }
                self.current_function_return_type = Type::Inferred;
            }
            DeclarationKind::Variable(var) => {
                // placeholder
                if let Some(symbol_var) = self.db.lookup_symbol(var.name.clone()) {
                    if let Some(initializer) = &var.initializer {
                        self.resolve_expression(initializer);
                    } else {
                        if let Some(_) = symbol_var.get_variable() {
                            // variable.
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn resolve_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Block(block) => {
                block.statements.iter().for_each(|s| {
                    self.resolve_statement(s);
                });
            }
            StatementKind::Expression(exprs) => {
                self.resolve_expression(exprs);
            }
            StatementKind::Return(ret) => {
                if let Some(return_type) = &ret {
                    let result = self.resolve_expression(return_type);

                    if result == Type::Error {
                        return;
                    }

                    self.db.add_type(statement.id, result.clone());

                    // placeholder return type is match with function
                    let expected = self.current_function_return_type.clone();
                    if !self.is_type_compatible(&expected, &result) {
                        self.db.report_error(SemanticError::TypeMismatch {
                            expected: expected.to_string(),
                            found: result.to_string(),
                            span: return_type.span,
                        });
                        // println!(
                        //     "return type mismatch expected {:?} but got {:?}",
                        //     expected, result
                        // );
                    }
                }
            }
            StatementKind::Variable(var) => {
                if let Some(initializer) = &var.initializer {
                    let result = self.resolve_expression(initializer);

                    let symbol = Symbol {
                        name: var.name.clone(),
                        span: var.span,
                        kind: SymbolKind::Variable(VariableSymbol {
                            name: var.name.clone(),
                            declared_type: var.variable_type.clone(),
                            need_inferred: var.variable_type.is_inferred(),
                            inferred_type: Some(result.clone()),
                            initializer: None,
                            final_type: result.clone(),
                        }),
                    };

                    let _ = self.db.add_symbol(symbol);
                    self.db.add_type(statement.id, result.clone());

                    if var.variable_type != Type::Inferred && var.variable_type != result {
                        self.db.report_error(SemanticError::TypeMismatch {
                            expected: var.variable_type.to_string(),
                            found: result.to_string(),
                            span: var.span,
                        });
                        // println!(
                        //     "Type mismatch expected {:?} but got {:?}",
                        //     var.variable_type.clone(),
                        //     result
                        // );
                    }
                } else {
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

                    let _ = self.db.add_symbol(symbol);
                    self.db.add_type(statement.id, var.variable_type.clone());
                }
            }
        }
    }

    fn resolve_expression(&mut self, expression: &Expression) -> Type {
        match &expression.kind {
            ExpressionKind::Literal(literal) => {
                let result = match literal {
                    Literal::String(_) => Type::String,
                    Literal::Integer(_) => Type::Int,
                    Literal::Boolean(_) => Type::Boolean,
                    Literal::Double(_) => Type::Double,
                    Literal::Null => Type::Null,
                };

                self.db.add_type(expression.id, result.clone()); // ← TAMBAHKAN
                result
            }
            ExpressionKind::Identifier(ident) => {
                if let Some(symbol) = self.db.lookup_symbol(ident.clone()) {
                    if let Some(func) = symbol.get_function() {
                        let result = func.return_type.clone();
                        self.db.add_type(expression.id, result.clone()); // ← TAMBAHKAN
                        return result;
                    } else if let Some(var) = symbol.get_variable() {
                        let result = var
                            .inferred_type
                            .clone()
                            .unwrap_or(var.declared_type.clone());
                        self.db.add_type(expression.id, result.clone()); // ← TAMBAHKAN
                        return result;
                    }
                }

                self.db.add_type(expression.id, Type::Error); // ← TAMBAHKAN untuk error case
                Type::Error
            }
            ExpressionKind::Assignment { left, op, right } => {
                let left_type = self.resolve_expression(&left);
                let right_type = self.resolve_expression(&right);

                if left_type == Type::Error || right_type == Type::Error {
                    self.db.add_type(expression.id, Type::Error);
                    return Type::Error;
                }

                let result = self.infer_assingment_operator_type(&left_type, &right_type, op);

                match result {
                    Ok(ty) => {
                        self.db.add_type(expression.id, ty.clone());
                        return ty;
                    }
                    Err(err) => {
                        self.db.add_type(expression.id, Type::Error);
                        self.db.report_error(SemanticError::Raw { message: err, span: left.span.merge(right.span) });
                        // println!("Assignment error: {}", err);
                        // println!("{}", err);
                        return Type::Error;
                    }
                };
            }
            ExpressionKind::Binary { left, op, right } => {
                let left_type = self.resolve_expression(&left);
                let right_type = self.resolve_expression(&right);

                if left_type == Type::Error || right_type == Type::Error {
                    self.db.add_type(expression.id, Type::Error);
                    return Type::Error;
                }

                let result = self.infer_binary_operation_type(&left_type, &right_type, op);

                match result {
                    Ok(ty) => {
                        self.db.add_type(expression.id, ty.clone());
                        return ty;
                    }
                    Err(err) => {
                        self.db.add_type(expression.id, Type::Error);
                        self.db.report_error(SemanticError::Raw { message: err, span: left.span.merge(right.span) });
                        return Type::Error;
                    }
                };
            }
            ExpressionKind::Unary { operand, op } => {
                let operand_type = self.resolve_expression(&operand);

                let result = match op {
                    UnaryOp::AddressOf => {
                        if operand_type == Type::Error {
                            // println!("Error: operand resolved to Error type");
                            self.db.add_type(expression.id, Type::Error);
                            return Type::Error;
                        }
                        Type::Pointer(Box::new(operand_type))
                    }
                    UnaryOp::Deref => {
                        if let Type::Pointer(inner) = operand_type {
                            let result = *inner;
                            result
                        } else {
                            // println!("Cannot dereference non-pointer type {:?}", operand_type);
                            self.db.report_error(SemanticError::InvalidPointerOp {
                                op: op.clone(),
                                operand_type: operand_type,
                                span: operand.span,
                                reason: "Cannot dereference non-pointer type".to_string(),
                            });
                            self.db.add_type(expression.id, Type::Error);
                            Type::Error
                        }
                    }
                    UnaryOp::Minus => operand_type,
                    _ => {
                        // println!("Unknown unary operator: {:?}", op);
                        self.db.report_error(SemanticError::InvalidUnaryOp {
                            op: op.clone(),
                            span: operand.span,
                            reason: "Unknown unary operator".to_string()
                        });
                        Type::Error
                    }
                };

                self.db.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::FunctionCall(func) => {
                // placeholder checking argument type
                // placeholder checking return type
                for arg in &func.arguments {
                    let arg_expr = match arg {
                        tahuc_ast::nodes::expressions::Argument::Positional(expr) => expr,
                        tahuc_ast::nodes::expressions::Argument::Named { value, .. } => value,
                    };
                    self.resolve_expression(arg_expr);
                }

                let return_type = self.resolve_expression(&func.function);

                // println!("fn call {:?} -> return {:?}", func.function.kind, return_type);
                self.db.add_type(expression.id, return_type.clone());

                return_type
            }
            ExpressionKind::Grouping(grouping) => {
                let result = self.resolve_expression(&grouping);
                self.db.add_type(expression.id, result.clone());
                result
            }
            _ => {
                self.db.report_error(SemanticError::Raw {
                    message: "Not supported expression".to_string(),
                    span: expression.span, 
                });
                self.db.add_type(expression.id, Type::Error);
                Type::Error
            }
        }
    }

    fn infer_assingment_operator_type(
        &mut self,
        left: &Type,
        right: &Type,
        operator: &AssignmentOp,
    ) -> Result<Type, String> {
        match operator {
            AssignmentOp::Assign => match (left, right) {
                (Type::Int, Type::Int) => Ok(Type::Int),
                _ => Err(format!(
                    "Cannot apply assigment operator '{:?}' to {:?} and {:?}",
                    operator, left, right
                )),
            },
            AssignmentOp::AddAssign
            | AssignmentOp::SubAssign
            | AssignmentOp::MulAssign
            | AssignmentOp::DivAssign
            | AssignmentOp::RemAssign => {
                //  "+" | "-" | "*" | "/" | "%"
                match (left, right) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Double, Type::Double) => Ok(Type::Double),
                    (Type::Int, Type::Double) | (Type::Double, Type::Int) => Ok(Type::Double),
                    // (Type::String, _) | (_, Type::String) if operator == "+" => Ok(Type::String),
                    _ => Err(format!(
                        "Cannot apply operator '{:?}' to {:?} and {:?}",
                        operator, left, right
                    )),
                }
            }
            _ => {
                println!("Assignment type mismatch!");
                Err(format!("Unknow assingment opration {:?}", operator))
            }
        }
    }

    fn infer_binary_operation_type(
        &self,
        left: &Type,
        right: &Type,
        operator: &BinaryOp,
    ) -> Result<Type, String> {
        match operator {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                //  "+" | "-" | "*" | "/" | "%" => {
                match (left, right) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Double, Type::Double) => Ok(Type::Double),
                    (Type::Int, Type::Double) | (Type::Double, Type::Int) => Ok(Type::Double),
                    // (Type::String, _) | (_, Type::String) if operator == "+" => Ok(Type::String),
                    // (Type::String, _) | (_, Type::String) if operator == &BinaryOp::Add => Ok(Type::String),
                    _ => Err(format!(
                        "Cannot apply operator '{:?}' to {:?} and {:?}",
                        operator, left, right
                    )),
                }
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge => {
                // "==" | "!=" | "<" | "<=" | ">" | ">=" => {
                if self.is_type_compatible(left, right) || self.is_type_compatible(right, left) {
                    Ok(Type::Boolean)
                } else {
                    Err(format!("Cannot compare {:?} and {:?}", left, right))
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                // "&&" | "||" => {
                match (left, right) {
                    (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
                    _ => Err(format!(
                        "Logical operator '{:?}' requires boolean operands, got {:?} and {:?}",
                        operator, left, right
                    )),
                }
            }
            _ => Err(format!("Unknown binary operator: {:?}", operator)),
        }
    }

    pub fn is_type_compatible(&self, from_type: &Type, to_type: &Type) -> bool {
        match (from_type, to_type) {
            // Exact match
            (a, b) if a == b => true,

            // Implicit conversions
            (Type::Int, Type::Double) => true,

            // Null compatibility (jika ada Type::Null)
            // (Type::Null, _) => true,

            // TODO: Add more sophisticated type compatibility rules
            // - Subtyping
            // - Generic type compatibility
            // - Interface implementation checking
            (Type::Pointer(inner_a), Type::Pointer(inner_b)) => {
                self.is_type_compatible(inner_a, inner_b)
            }

            _ => false,
        }
    }
}
