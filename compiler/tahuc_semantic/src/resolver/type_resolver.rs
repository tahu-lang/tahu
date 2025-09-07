use tahuc_ast::{
    nodes::{
        declarations::{Declaration, DeclarationKind}, expressions::ExpressionKind, op::{AssignmentOp, BinaryOp, UnaryOp}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind}, Expression
    }, Module, Type
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
                        self.resolve_expression(initializer, Some(var.variable_type.clone()));
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
            StatementKind::Expression(exprs) => {
                self.resolve_expression(exprs, None);
            }
            StatementKind::Variable(var) => {
                if let Some(initializer) = &var.initializer {
                    let result = self.resolve_expression(initializer, Some(var.variable_type.clone()));

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
            StatementKind::Assignment { left, op, right } => {
                let left_ty = self.resolve_expression(&left, None);
                let right_ty = self.resolve_expression(&right, Some(left_ty.clone()));

                if left_ty != right_ty {
                    self.db.report_error(SemanticError::TypeMismatch {
                        expected: left_ty.to_string(),
                        found: right_ty.to_string(),
                        span: statement.span,
                    });
                } else {
                    self.db.add_type(statement.id, left_ty);
                }
            }
            StatementKind::Return(ret) => {
                if let Some(return_type) = &ret {
                    let result = self.resolve_expression(return_type, Some(self.current_function_return_type.clone()));

                    if result == Type::Error {
                        return;
                    }

                    self.db.add_type(statement.id, result.clone());

                    let expected = self.current_function_return_type.clone();
                    if !self.is_type_compatible(&expected, &result) {
                        self.db.report_error(SemanticError::TypeMismatch {
                            expected: expected.to_string(),
                            found: result.to_string(),
                            span: return_type.span,
                        });
                    }
                }
            }
            StatementKind::IfStatement(if_stmt) => {
                self.resolve_if_statement(if_stmt);
            }
            StatementKind::WhileStatement(while_stmt) => {
                let condition = self.resolve_expression(&while_stmt.condition, None);
                self.resolve_block(&while_stmt.body);

                self.db.add_type(statement.id, condition);
            }
            _ => {}
        }
    }

    fn resolve_block(&mut self, block: &Block) {
        self.db.enter_scope();
        for statement in &block.statements {
            self.resolve_statement(statement);
        }
        self.db.exit_scope();
    }

    fn resolve_if_statement(&mut self, if_stmt: &IfStatement) {
        let ty = self.resolve_expression(&if_stmt.condition, None);
        self.db.add_type(if_stmt.condition.id, ty.clone());
        self.resolve_block(&if_stmt.then_branch);

        if let Some(else_branch) = &if_stmt.else_branch {
            match &else_branch {
                ElseBranch::Block(block) => self.resolve_block(block),
                ElseBranch::If(else_if_stmt) => self.resolve_if_statement(else_if_stmt),
            }
        }
    }

    fn resolve_expression(&mut self, expression: &Expression, expected: Option<Type>) -> Type {
        match &expression.kind {
            ExpressionKind::Literal(literal) => {
                let result = match literal {
                    Literal::String(_) => Type::String,
                    Literal::Integer(_) => Type::Int,
                    Literal::Boolean(_) => Type::Boolean,
                    Literal::Double(_) => Type::Double,
                    Literal::Null => {
                        match expected {
                            Some(Type::Nullable(inner)) => Type::Nullable(inner.clone()),
                            Some(Type::Pointer(inner))  => Type::Pointer(inner.clone()),
                            Some(ty) => {
                                self.db.report_error(SemanticError::TypeMismatch {
                                    expected: ty.to_string(),
                                    found: "null".to_string(),
                                    span: expression.span,
                                });
                                Type::Error
                            }
                            None => Type::Inferred,
                        }
                    }
                };

                self.db.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::TemplateString { parts } => {
                let result = Type::String;
                self.db.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::Identifier(ident) => {
                if let Some(symbol) = self.db.lookup_symbol(ident.clone()) {
                    if let Some(func) = symbol.get_function() {
                        let result = func.return_type.clone();
                        self.db.add_type(expression.id, result.clone());
                        return result;
                    } else if let Some(var) = symbol.get_variable() {
                        let result = var
                            .inferred_type
                            .clone()
                            .unwrap_or(var.declared_type.clone());
                        self.db.add_type(expression.id, result.clone());
                        return result;
                    }
                }

                self.db.add_type(expression.id, Type::Error);
                Type::Error
            }
            // ExpressionKind::Assignment { left, op, right } => {
            //     let left_type = self.resolve_expression(&left, None);
            //     let right_type = self.resolve_expression(&right, None);

            //     if left_type == Type::Error || right_type == Type::Error {
            //         self.db.add_type(expression.id, Type::Error);
            //         return Type::Error;
            //     }

            //     let result = self.infer_assingment_operator_type(&left_type, &right_type, op);

            //     match result {
            //         Ok(ty) => {
            //             self.db.add_type(expression.id, ty.clone());
            //             return ty;
            //         }
            //         Err(err) => {
            //             self.db.add_type(expression.id, Type::Error);
            //             self.db.report_error(SemanticError::Raw { message: err, span: left.span.merge(right.span) });
            //             // println!("Assignment error: {}", err);
            //             // println!("{}", err);
            //             return Type::Error;
            //         }
            //     };
            // }
            ExpressionKind::Binary { left, op, right } => {
                let left_type = self.resolve_expression(&left, None);
                let right_type = self.resolve_expression(&right, None);

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
                let operand_type = self.resolve_expression(&operand, None);

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
                    // UnaryOp::Minus => operand_type,
                    _ => {
                        if operand_type == Type::Error {
                            self.db.report_error(SemanticError::InvalidUnaryOp {
                                op: op.clone(),
                                span: operand.span,
                                reason: "Operand resolved to Error type".to_string(),
                            });
                            return Type::Error;
                        }

                        operand_type

                        // // println!("Unknown unary operator: {:?}", op);
                        // self.db.report_error(SemanticError::InvalidUnaryOp {
                        //     op: op.clone(),
                        //     span: operand.span,
                        //     reason: "Unknown unary operator".to_string()
                        // });
                        // Type::Error
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
                    self.resolve_expression(arg_expr, None);
                }

                let return_type = self.resolve_expression(&func.function, None);

                self.db.add_type(expression.id, return_type.clone());

                return_type
            }
            ExpressionKind::Grouping(grouping) => {
                let result = self.resolve_expression(&grouping, None);
                self.db.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::ArrayLiteral { elements } => {
                if elements.is_empty() {
                    if let Some(expected_ty) = expected {
                        self.db.add_type(expression.id, expected_ty.clone());
                        return expected_ty;
                    } else {
                        self.db.report_error(SemanticError::Raw {
                            message: "Cannot infer type of array".to_string(),
                            span: expression.span,
                        });
                        return Type::Error;
                    }
                }

                let element_expected = match expected {
                    Some(Type::Array(inner)) => Some(*inner.clone()),
                    _ => None,
                };

                let first_ty = self.resolve_expression(&elements[0], element_expected);

                for element in &elements[1..] {
                    let elem_ty = self.resolve_expression(element, Some(first_ty.clone()));
                    if elem_ty != first_ty {
                        self.db.report_error(SemanticError::TypeMismatch {
                            expected: first_ty.to_string(),
                            found: elem_ty.to_string(),
                            span: element.span
                        });
                        return Type::Error;
                    }
                }

                let result = Type::Array(Box::new(first_ty));
                self.db.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let arr_ty = self.resolve_expression(&array, None);
                let idx_ty = self.resolve_expression(&index, None);

                if idx_ty != Type::Int {
                    self.db.report_error(SemanticError::Raw {
                        message: format!("Array index must be Int, Found {:?}", idx_ty),
                        span: index.span,
                    });
                }

                let elem_type = match arr_ty {
                    Type::Array(inner) => *inner, // ambil tipe elemen
                    _ => {
                        // error: trying to index non-array
                        Type::Error
                    }
                };
                
                self.db.add_type(expression.id, elem_type.clone());

                elem_type
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
                    (Type::Pointer(_), Type::Int) => Ok(Type::Pointer(Box::new(Type::Int))),
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
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor 
            | BinaryOp::Shl | BinaryOp::Shr => {
                match (left, right) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    _ => Err(format!(
                        "Bitwise operator '{:?}' requires integer operands, got {:?} and {:?}",
                        operator, left, right
                    )),
                }
            }
            // _ => Err(format!("Unknown binary operator: {:?}", operator)),
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
