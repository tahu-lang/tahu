use tahuc_ast::{
    nodes::{
        ast::NodeId, declarations::{Declaration, DeclarationKind}, expressions::ExpressionKind, op::{BinaryOp, UnaryOp}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind}, Expression
    }, ty::{PrimitiveType, Type}, Module
};
use tahuc_lexer::token::Literal;
use tahuc_span::{FileId, Span};

use crate::{
    database::Database,
    error::SemanticError,
    symbol::{Symbol, SymbolKind, VariableSymbol},
};

pub struct TypeAnalyzer<'a> {
    db: &'a mut Database,
    file_id: FileId,
    current_function_return_type: Type,
}

impl<'a> TypeAnalyzer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            file_id: FileId(0),
            current_function_return_type: Type::Inferred,
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();
        self.file_id = module.file;

        for declaration in &module.declaration {
            self.resolve_declaration(declaration);
        }
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                if let Some(_) = self.lookup_symbol(func.kind.name.clone()) {
                    let ret_ty = self.get_type(declaration.id).unwrap().clone();
                    self.current_function_return_type = ret_ty;

                    self.db.enter_scope();

                    for p in func.kind.parameters.iter() {
                        let ty = self.get_type(p.id).unwrap().clone();
                        if let Err(_) = self.add_symbol(Symbol {
                            name: p.kind.name.clone(),
                            span: p.span,
                            kind: SymbolKind::Variable(VariableSymbol {
                                name: p.kind.name.clone(),
                                declared_type: ty.clone(),
                                initializer: None,
                                is_mutable: true,

                                need_inferred: false,
                                inferred_type: None,
                                final_type: ty.clone(),
                            }),
                        }) {
                            let _ = self
                                .add_symbol(Symbol {
                                    name: p.kind.name.clone(),
                                    span: p.span,
                                    kind: SymbolKind::Variable(VariableSymbol {
                                        name: p.kind.name.clone(),
                                        declared_type: Type::Error,
                                        initializer: None,
                                        is_mutable: true,

                                        need_inferred: false,
                                        inferred_type: None,
                                        final_type: Type::Error,
                                    }),
                                })
                                .ok();
                        }
                    }

                    self.resolve_block(&func.kind.body);

                    self.db.exit_scope();
                }
                self.current_function_return_type = Type::Inferred;
            }
            DeclarationKind::Variable(var) => {
                // placeholder
                if let Some(symbol_var) = self.lookup_symbol(var.name.clone()) {
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

                    let Some(_) = self.get_symbol(statement.id) else {
                        return;
                    };

                    self.update_symbol(statement.id, |symbol| {
                        symbol.update_variable(|variable| {
                            variable.final_type = result.clone();
                        });
                    });

                    self.add_type(statement.id, result.clone());

                    if var.variable_type.is_named() {
                        match &result {
                            Type::Struct { name, .. } => {
                                if *name != var.variable_type.get_ty_named().unwrap() {
                                    self.db.report_error(SemanticError::TypeMismatch {
                                        msg: format!("Variable type mismatch"),
                                        expected: var.variable_type.to_string(),
                                        found: result.to_string(),
                                        span: var.span,
                                    });
                                    return;
                                }
                            }
                            _ => {}
                        }
                        return;
                    }

                    if var.variable_type != Type::Inferred && var.variable_type != result {
                        self.db.report_error(SemanticError::TypeMismatch {
                            msg: format!("Variable type mismatch"),
                            expected: var.variable_type.to_string(),
                            found: result.to_string(),
                            span: var.span,
                        });
                    }
                } else {
                    self.add_type(statement.id, var.variable_type.clone());
                }
            }
            StatementKind::Assignment { left, right, .. } => {
                if self.can_assign_left_value(&left) {
                    let left_ty = self.resolve_expression(&left, None);
                    let right_ty = self.resolve_expression(&right, Some(left_ty.clone()));
    
                    if !self.is_assignable_type(&left_ty, &right_ty) {
                        self.db.report_error(SemanticError::TypeMismatch {
                            msg: format!("Assignment type mismatch"),
                            expected: left_ty.to_string(),
                            found: right_ty.to_string(),
                            span: statement.span,
                        });
                    } else {
                        self.add_type(statement.id, left_ty);
                    }
                }
            }
            StatementKind::Return(ret) => {
                if let Some(return_type) = &ret {
                    self.debug(format!("resolve return type {:?}", return_type));
                    let result = self.resolve_expression(return_type, Some(self.current_function_return_type.clone()));

                    if result == Type::Error {
                        return;
                    }

                    self.add_type(statement.id, result.clone());

                    let expected = self.current_function_return_type.clone();
                    if !self.is_type_compatible(&expected, &result) {
                        self.db.report_error(SemanticError::TypeMismatch {
                            msg: format!("Return type mismatch"),
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

                self.add_type(statement.id, condition);
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

        if !ty.is_bool() && !ty.is_integer() && !ty.is_float() {
            self.db.report_error(SemanticError::TypeMismatch {
                msg: format!("if condition must be boolean or integer or float"),
                expected: format!("bool or integer or float"),
                found: format!("{}", ty),
                span: if_stmt.condition.span,
            });
            return;
        } 

        self.add_type(if_stmt.condition.id, ty.clone());
        self.resolve_block(&if_stmt.then_branch);

        if let Some(else_branch) = &if_stmt.else_branch {
            match &else_branch {
                ElseBranch::Block(block) => self.resolve_block(block),
                ElseBranch::If(else_if_stmt) => self.resolve_if_statement(else_if_stmt),
            }
        }
    }

    fn can_assign_left_value(&mut self, expression: &Expression) -> bool {
        match &expression.kind {
            ExpressionKind::Identifier(ident) => {
                if let Some(symbol) = self.get_symbol(expression.id) {
                    if let SymbolKind::Variable(var) = &symbol.kind {
                        if !var.is_mutable {
                            self.db.report_error(SemanticError::CannotAssignImmutable {
                                name: ident.clone(),
                                span: expression.span,
                            });
                            return false;
                        }
                    }
                }

                true
            }
            ExpressionKind::ArrayAccess { array, .. } => {
                self.can_assign_left_value(array)
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.can_assign_left_value(object)
            }
            ExpressionKind::Unary { op, operand } => {
                match op {
                    UnaryOp::Deref => {
                        self.can_assign_left_value(operand)
                    }
                    _ => false
                }
            }
            _ => false
        }
    }

    fn resolve_expression(&mut self, expression: &Expression, expected: Option<Type>) -> Type {
        match &expression.kind {
            ExpressionKind::Literal(literal) => {
                let result = match literal {
                    Literal::String(_) => Type::String,
                    Literal::Char(_) => Type::Primitive(PrimitiveType::Char),
                    Literal::Integer(_) => {
                        if let Some(expected) = expected {
                            if expected.is_integer() {
                                expected
                            } else if expected.is_inferred() {
                                Type::Int
                            } else {
                                Type::Error
                            }
                        } else {
                            Type::Int
                        }
                    },
                    Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool),
                    Literal::Double(_) => {
                        if let Some(expected) = expected {
                            if expected.is_float() {
                                expected
                            } else if expected.is_inferred() {
                                Type::Double
                            } else {
                                Type::Error
                            }
                        } else {
                            Type::Double
                        }
                    },
                    Literal::Null => {
                        match expected {
                            Some(Type::Nullable(inner)) => {
                                Type::Nullable(inner.clone())
                            },
                            //  Type::Nullable(inner.clone()),
                            Some(Type::Pointer(inner))  => Type::Pointer(inner.clone()),
                            Some(ty) => {
                                self.db.report_error(SemanticError::TypeMismatch {
                                    msg: "Cannot infer type of null literal".to_string(),
                                    expected: ty.to_string(),
                                    found: "null".to_string(),
                                    span: expression.span,
                                });
                                Type::Error
                            }
                            None => {
                                println!("Cannot infer type of null literal {:?} and expected {:?}", literal, expected);
                                Type::Null
                            },
                        }
                    }
                };

                self.debug(format!("resolve literal id {} - {}", expression.id, result));

                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::TemplateString { .. } => {
                let result = Type::String;
                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::Identifier(_) => {
                self.read_variable(expression.id)
            }
            ExpressionKind::Ternary { condition, then, otherwise } => {
                let cond = self.resolve_expression(&condition, Some(Type::Primitive(PrimitiveType::Bool)));
                if cond != Type::Primitive(PrimitiveType::Bool) {
                    self.db.report_error(SemanticError::Raw {
                        message: "Ternary condition must be boolean".to_string(),
                        span: condition.span,
                    });
                    return Type::Error;
                }

                let then_ty = self.resolve_expression(&then, None);
                let otherwise_ty = self.resolve_expression(&otherwise, None);
                if then_ty != otherwise_ty {
                    self.db.report_error(SemanticError::TypeMismatch {
                        msg: "Ternary then and otherwise must have the same type".to_string(),
                        expected: then_ty.to_string(),
                        found: otherwise_ty.to_string(),
                        span: expression.span,
                    });
                    return Type::Error;
                }

                self.add_type(expression.id, then_ty.clone());
                then_ty
            }
            ExpressionKind::Binary { left, op, right } => {
                let l_ty = self.get_type(left.id).cloned();
                let r_ty = self.get_type(right.id).cloned();
                let left_type = self.resolve_expression(&left, r_ty);
                let right_type = self.resolve_expression(&right, l_ty);

                if left_type == Type::Error || right_type == Type::Error {
                    self.add_type(expression.id, Type::Error);
                    return Type::Error;
                }

                let result = self.infer_binary_operation_type(&left_type, &right_type, op);

                match result {
                    Ok(ty) => {
                        self.add_type(expression.id, ty.clone());
                        return ty;
                    }
                    Err(err) => {
                        self.add_type(expression.id, Type::Error);
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
                            self.add_type(expression.id, Type::Error);
                            return Type::Error;
                        }
                        Type::Pointer(Box::new(operand_type))
                    }
                    UnaryOp::Deref => {
                        if let Type::Pointer(inner) = operand_type {
                            let result = *inner;
                            result
                        } else {
                            self.db.report_error(SemanticError::InvalidPointerOp {
                                op: op.clone(),
                                operand_type: operand_type,
                                span: operand.span,
                                reason: "Cannot dereference non-pointer type".to_string(),
                            });
                            self.add_type(expression.id, Type::Error);
                            Type::Error
                        }
                    }
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
                    }
                };

                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::FunctionCall(func) => {
                // let Ok(function) = self.resolve_function(&func.function) else {
                //     self.db.report_error(SemanticError::Raw {
                //         message: "Function not found".to_string(),
                //         span: func.function.span,
                //     });
                //     return Type::Error;
                // };
                // let Some(symbol) = self.lookup_symbol(function.clone()) else {
                //     self.db.report_error(SemanticError::Undefined {
                //         name: function,
                //         span: func.function.span,
                //     });
    
                //     return Type::Error
                // };

                let Some(symbol) = self.get_symbol(func.function.id).cloned() else {
                    return Type::Error;
                };

                let Some(signature) = symbol.get_function() else {
                    return Type::Error;
                };

                for (i, arg) in func.arguments.iter().enumerate() {
                    let arg_expr = match arg {
                        tahuc_ast::nodes::expressions::Argument::Positional(expr) => expr,
                        tahuc_ast::nodes::expressions::Argument::Named { value, .. } => value,
                    };
                    let param_ty = signature.parameters[i].var_type.clone();
                    self.resolve_expression(arg_expr, Some(param_ty));
                }

                let return_type = self.resolve_expression(&func.function, None);

                self.add_type(expression.id, return_type.clone());

                return_type
            }
            ExpressionKind::Grouping(grouping) => {
                let result = self.resolve_expression(&grouping, None);
                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::ArrayLiteral { elements } => {
                if elements.is_empty() {
                    if let Some(expected_ty) = expected {
                        self.add_type(expression.id, expected_ty.clone());
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
                    Some(Type::Array { ty, .. }) => Some(*ty),
                    _ => None,
                };

                let first_ty = self.resolve_expression(&elements[0], element_expected);

                for element in &elements[1..] {
                    let elem_ty = self.resolve_expression(element, Some(first_ty.clone()));
                    if elem_ty != first_ty {
                        self.db.report_error(SemanticError::TypeMismatch {
                            msg: "Array elements must have the same type".to_string(),
                            expected: first_ty.to_string(),
                            found: elem_ty.to_string(),
                            span: element.span
                        });
                        return Type::Error;
                    }
                }

                let result = Type::Array { ty: Box::new(first_ty), size: elements.len() };
                self.add_type(expression.id, result.clone());
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
                    Type::Array { ty, .. } => *ty,
                    Type::String => Type::Primitive(PrimitiveType::Char),
                    Type::Pointer(inner) => *inner,
                    _ => {
                        println!("Error: trying to index non-array {}", arr_ty);
                        // error: trying to index non-array
                        Type::Error
                    }
                };
                
                self.add_type(expression.id, elem_type.clone());

                elem_type
            }
            ExpressionKind::MemberAccess { object, member } => {
                let obj_ty = self.resolve_expression(&object, None);

                match &obj_ty {
                    Type::Struct { fields, .. } => {
                        for (name, ty) in fields {
                            if name == member {
                                self.add_type(expression.id, ty.clone());
                                return ty.clone();
                            }
                        }
                    }
                    _ => {}
                }

                self.add_type(expression.id, obj_ty.clone());
                obj_ty.clone()
            }
            ExpressionKind::Cast { ty, expression: expr } => {
                let expr_ty = self.resolve_expression(&expr, None);
                if !self.is_type_compatible_casting(&expr_ty, &ty) {
                    self.db.report_error(SemanticError::Raw {
                        message: format!("Cannot cast {:?} to {:?}", expr_ty, ty),
                        span: expression.span,
                    });
                    return Type::Error;
                }
                self.add_type(expression.id, ty.clone());
                ty.clone()
            }
            ExpressionKind::StructLiteral { object, fields } => {
                let struct_ty = self.resolve_expression(&object, None);
                self.debug(format!("resolving struct literal {}", struct_ty));

                match &struct_ty {
                    Type::Struct { name, fields: fields_ty } => {
                        if fields.len() != fields_ty.len() {
                            self.db.report_error(SemanticError::Raw {
                                message: format!("Expected {} fields, found {}", fields_ty.len(), fields.len()),
                                span: expression.span,
                            });
                            return Type::Error;
                        }

                        for (idx, field_expr) in fields.iter().enumerate() {
                            self.debug(format!("fields {} - {:?}", idx, field_expr.kind));
                            let field_name = match &field_expr.kind.name.kind {
                                ExpressionKind::Identifier(name) => name,
                                _ => {
                                    self.db.report_error(SemanticError::Raw {
                                        message: "Field name must be identifier".to_string(),
                                        span: field_expr.span,
                                    });
                                    continue;
                                }
                            };

                            let expected_ty = fields_ty[idx].1.clone();

                            let ty = match &field_expr.kind.value {
                                Some(value) => {
                                    self.debug(format!("Resolving struct field '{}' with expected type {}", field_name, expected_ty));
                                    self.resolve_expression(value, Some(expected_ty))
                                },
                                None => {
                                    self.debug(format!("Resolving struct field '{}' with default value", field_name));
                                    let value = self.read_variable(field_expr.id);
                                    value
                                }
                            };

                            self.add_type(field_expr.id, ty.clone());

                            if idx >= fields_ty.len() || &fields_ty[idx].0 != field_name {
                                self.db.report_error(SemanticError::Raw {
                                    message: format!("Unexpected field name '{}'", field_name),
                                    span: field_expr.span,
                                });
                                continue;
                            }

                            self.infer_struct_literal(&fields_ty[idx].1, ty, name, field_expr.span);
                        }

                        self.add_type(expression.id, struct_ty.clone());
                    }
                    _ => {
                        self.db.report_error(SemanticError::Raw {
                            message: "Cannot instantiate non-struct type".to_string(),
                            span: expression.span, 
                        });
                    }
                }
                

                struct_ty
            }
            // _ => {
            //     self.db.report_error(SemanticError::Raw {
            //         message: "Not supported expression".to_string(),
            //         span: expression.span, 
            //     });
            //     self.add_type(expression.id, Type::Error);
            //     Type::Error
            // }
        }
    }

    fn read_variable(&mut self, id: NodeId) -> Type {
        if let Some(symbol) = self.get_symbol(id) {
            self.debug(format!("symbol `{}` found", id));
            let ty = symbol.get_type();
            self.add_type(id, ty.clone());
            return ty;
        }

        self.debug(format!("symbol `{}` not found", id));
        self.add_type(id, Type::Error);
        Type::Error
    }

    fn infer_struct_literal(&mut self, field_ty: &Type, result_ty: Type, _struct_name: &String, span: Span) {
        if field_ty.is_named() {
            if field_ty.get_ty_named().unwrap() != result_ty.to_string() {}
            if field_ty.get_ty_named().unwrap() != result_ty.to_string() {
                self.db.report_error(SemanticError::TypeMismatch {
                    msg: "Struct literal field type mismatch".to_string(),
                    expected: field_ty.to_string(),
                    found: result_ty.to_string(),
                    span: span,
                });
            }
        }

        if *field_ty != result_ty {
            self.db.report_error(SemanticError::TypeMismatch {
                msg: "Struct literal field type mismatch".to_string(),
                expected: field_ty.to_string(),
                found: result_ty.to_string(),
                span: span,
            });
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
                    (Type::Primitive(PrimitiveType::U8), Type::Primitive(PrimitiveType::U8)) => Ok(Type::Primitive(PrimitiveType::U8)),
                    (Type::Primitive(PrimitiveType::Char), Type::Primitive(PrimitiveType::U8)) => Ok(Type::Primitive(PrimitiveType::Char)),
                    (Type::Primitive(PrimitiveType::Char), Type::Int) => Ok(Type::Primitive(PrimitiveType::Char)),

                    (Type::Primitive(PrimitiveType::I32), Type::Primitive(PrimitiveType::I32)) => Ok(Type::Primitive(PrimitiveType::I32)),

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
                if left.can_compare(right) || right.can_compare(left) {
                    return Ok(Type::Primitive(PrimitiveType::Bool));
                }
                // if self.is_type_compatible(left, right) || self.is_type_compatible(right, left) {
                //     Ok(Type::Primitive(PrimitiveType::Bool))
                // } 
                else {
                    Err(format!("Cannot compare {:?} and {:?}", left, right))
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                // "&&" | "||" => {
                match (left, right) {
                    (Type::Primitive(PrimitiveType::Bool), Type::Primitive(PrimitiveType::Bool)) => Ok(Type::Primitive(PrimitiveType::Bool)),
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
                    (Type::Primitive(PrimitiveType::I32), Type::Int) => Ok(Type::Int),
                    (Type::Int, Type::Primitive(PrimitiveType::I32)) => Ok(Type::Int),
                    (Type::Primitive(PrimitiveType::U8), Type::Int) => Ok(Type::Primitive(PrimitiveType::U8)),
                    (a, b) if a.is_integer() && b.is_integer() => Ok(a.clone()),
                    _ => Err(format!(
                        "Bitwise operator '{:?}' requires integer operands, got {:?} and {:?}",
                        operator, left, right
                    )),
                }
            }
            // _ => Err(format!("Unknown binary operator: {:?}", operator)),
        }
    }

    fn is_assignable_type(&self, left: &Type, right: &Type) -> bool {
        match (left, right) {
            // exact match
            (a, b) if a == b => true,

            // implicit conversion
            (Type::Nullable(s), _) => self.is_assignable_type(s, right),

            // fallthrgouh
            _ => false,
        }
    }

    pub fn is_type_compatible_casting(&self, from_type: &Type, to_type: &Type) -> bool {
        match (from_type, to_type) {
            (Type::Int, Type::Double) => true,
            (Type::Primitive(PrimitiveType::Char), Type::Int) => true,
            (Type::Primitive(PrimitiveType::Char), Type::Primitive(PrimitiveType::U8)) => true,
            (Type::Int, Type::Primitive(PrimitiveType::U8)) => true,
            _ => false,
        }
    }

    pub fn is_type_compatible(&self, from_type: &Type, to_type: &Type) -> bool {
        match (from_type, to_type) {
            // Exact match
            (a, b) if a == b => true,

            // Implicit conversions
            (Type::Int, Type::Double) => true,
            (Type::Primitive(PrimitiveType::Char), Type::Int) => true,

            // Null compatibility (jika ada Type::Null)
            (Type::Nullable(_), _) => true,
            (Type::Null, _) => true,

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

    fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        self.db.add_symbol(self.file_id, symbol)
    }

    fn lookup_symbol(&mut self, name: String) -> Option<Symbol> {
        self.db.lookup_symbol(self.file_id, name)
    }

    fn add_type(&mut self, id: NodeId, ty: Type) {
        self.db.add_type(self.file_id, id, ty);
    }

    fn get_type(&mut self, id: NodeId) -> Option<&Type> {
        self.db.get_type(self.file_id, id)
    }

    fn get_symbol(&self, id: NodeId) -> Option<&Symbol> {
        self.db.get_symbol(self.file_id, id)
    }

    fn update_symbol<F>(&mut self, id: NodeId, f: F)
    where 
        F: FnMut(&mut Symbol)
    {
        self.db.update_symbol_db(self.file_id, id, f);
    }

    fn debug(&self, msg: String) {
        // println!("DEBUG [TYPE] - {}", msg);
    }
}
