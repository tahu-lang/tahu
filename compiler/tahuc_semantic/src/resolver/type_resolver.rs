use std::rc::Rc;

use tahuc_ast::{
    nodes::{
        ast::NodeId, declarations::{Declaration, DeclarationKind}, expressions::ExpressionKind, op::{BinaryOp, UnaryOp}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind}, Expression
    }, ty::{PrimitiveType, Type}, Module
};
use tahuc_lexer::token::Literal;
use tahuc_module::config::Config;
use tahuc_span::{FileId, Span, StrId};

use crate::{
    database::{database::Database, symbol::{FunctionSymbol, Symbol, SymbolKind, VariableSymbol}},
    error::SemanticError,
};

pub struct TypeAnalyzer<'a> {
    config: &'a Config<'a>,
    db: &'a mut Database,
    file_id: FileId,
    current_function_return_type: Rc<Type>,
}

impl<'a> TypeAnalyzer<'a> {
    pub fn new(config: &'a Config, db: &'a mut Database) -> Self {
        Self {
            config,
            db,
            file_id: FileId(0),
            current_function_return_type: Rc::new(Type::Inferred),
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
                if let Some(_) = self.lookup_function(func.kind.name) {
                    let ret_ty = self.get_type(declaration.id).unwrap().clone();
                    self.current_function_return_type = ret_ty;

                    self.db.enter_scope();

                    self.resolve_block(&func.kind.body);

                    self.db.exit_scope();
                }
                self.current_function_return_type = Rc::new(Type::Inferred);
            }
            DeclarationKind::Variable(var) => {
                // placeholder
                if let Some(_) = self.lookup_variable(var.name) {
                    if let Some(initializer) = &var.initializer {
                        self.resolve_expression(initializer, Some(var.variable_type.clone()));
                    } else {
                        // if let Some(_) = symbol_var.get_variable() {
                            // variable.
                        // }
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

                    if result.is_named() {
                        let name = result.get_ty_named().unwrap();
                        if let Ok((_, ty)) = self.lookup_type(name, initializer.span) {
                            self.add_type(statement.id, ty);
                        }
                    } else {
                        self.add_type(statement.id, result.clone());
                    }

                    let Some(_) = self.get_symbol(statement.id) else {
                        return;
                    };

                    self.update_symbol(statement.id, |symbol| {
                        symbol.update_variable(|variable| {
                            variable.final_type = result.clone();
                        });
                    });

                    if var.variable_type.is_named() {
                        match *result {
                            Type::Struct { name, .. } => {
                                if name != var.variable_type.get_ty_named().unwrap() {
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

                    if *var.variable_type != Type::Inferred && var.variable_type != result {
                        self.db.report_error(SemanticError::TypeMismatch {
                            msg: format!("Variable type mismatch"),
                            expected: var.variable_type.to_string(),
                            found: result.to_string(),
                            span: var.span,
                        });
                    }
                } else {
                    if var.variable_type.is_inferred() {
                        let name = self.config.str_pool.get(var.name);
                        self.db.report_error(SemanticError::UninitializedVariable {
                            name: name.to_string(),
                            span: var.span,
                        });
                    }

                    if var.variable_type.is_named() {
                        let name = var.variable_type.get_ty_named().unwrap();
                        if let Ok((_, ty)) = self.lookup_type(name, var.span) {
                            self.add_type(statement.id, ty);
                        }
                    } else {
                        self.add_type(statement.id, var.variable_type.clone());
                    }
                }
            }
            StatementKind::Assignment { left, right, .. } => {
                if self.can_assign_left_value(&left) {
                    let left_ty = self.resolve_expression(&left, None);
                    let right_ty = self.resolve_expression(&right, Some(left_ty.clone()));

                    let left_ty = if left_ty.is_named() {
                        let name = left_ty.get_ty_named().unwrap();
                        if let Ok((_, ty)) = self.lookup_type(name, left.span) {
                            ty
                        } else {
                            left_ty
                        }
                    } else {
                        left_ty
                    };

                    let right_ty = if right_ty.is_named() {
                        let name = right_ty.get_ty_named().unwrap();
                        if let Ok((_, ty)) = self.lookup_type(name, right.span) {
                            ty
                        } else {
                            right_ty
                        }
                    } else {
                        right_ty
                    };

    
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

                    if *result == Type::Error {
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
                            let ident = self.config.str_pool.get(*ident);
                            self.db.report_error(SemanticError::CannotAssignImmutable {
                                name: ident.to_string(),
                                span: expression.span,
                            });
                            return false
                        }
                        true
                    } else {
                        true
                    }
                } else {
                    false
                }
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

    fn resolve_expression(&mut self, expression: &Expression, expected: Option<Rc<Type>>) -> Rc<Type> {
        match &expression.kind {
            ExpressionKind::Literal(literal) => {
                let result = match literal {
                    Literal::String(_) => Rc::new(Type::String),
                    Literal::Char(_) => Rc::new(Type::Primitive(PrimitiveType::Char)),
                    Literal::Integer(_) => {
                        if let Some(expected) = expected {
                            if expected.is_integer() {
                                expected
                            } else if expected.is_inferred() {
                                Rc::new(Type::Int)
                            } else {
                                Rc::new(Type::Int)
                            }
                        } else {
                            Rc::new(Type::Int)
                        }
                    },
                    Literal::Bool(_) => Rc::new(Type::Primitive(PrimitiveType::Bool)),
                    Literal::Double(_) => {
                        if let Some(expected) = expected {
                            if expected.is_float() {
                                expected
                            } else if expected.is_inferred() {
                                Rc::new(Type::Double)
                            } else {
                                Rc::new(Type::Double)
                            }
                        } else {
                            Rc::new(Type::Double)
                        }
                    },
                    Literal::Null => {
                        match expected {
                            Some(ty) => {
                                match &*ty {
                                    Type::Nullable(inner) => {
                                        inner.clone()
                                    }
                                    Type::Pointer(inner) => {
                                        inner.clone()
                                    }
                                    _ => {
                                        self.db.report_error(SemanticError::TypeMismatch {
                                            msg: "Cannot infer type of null literal".to_string(),
                                            expected: ty.to_string(),
                                            found: "null".to_string(),
                                            span: expression.span,
                                        });
                                        Rc::new(Type::Error)
                                    }
                                }
                            }
                            None => {
                                Rc::new(Type::Null)
                            },
                        }
                    }
                };

                self.debug(format!("resolve literal id {} - {}", expression.id, result));

                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::TemplateString { .. } => {
                let result = Rc::new(Type::String);
                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::Identifier(_) => {
                self.read_variable(expression.id)
            }
            ExpressionKind::Ternary { condition, then, otherwise } => {
                let cond = self.resolve_expression(&condition, Some(Rc::new(Type::Primitive(PrimitiveType::Bool))));
                if *cond != Type::Primitive(PrimitiveType::Bool) {
                    self.db.report_error(SemanticError::Raw {
                        message: "Ternary condition must be boolean".to_string(),
                        span: condition.span,
                    });
                    return Rc::new(Type::Error);
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
                    return Rc::new(Type::Error);
                }

                self.add_type(expression.id, then_ty.clone());
                then_ty
            }
            ExpressionKind::Binary { left, op, right } => {
                let l_ty = self.get_type(left.id).cloned();
                let r_ty = self.get_type(right.id).cloned();
                let left_type = self.resolve_expression(&left, r_ty);
                let right_type = self.resolve_expression(&right, l_ty);

                if *left_type == Type::Error || *right_type == Type::Error {
                    self.add_type(expression.id, Rc::new(Type::Error));
                    return Rc::new(Type::Error);
                }

                let result = self.infer_binary_operation_type(&left_type, &right_type, op);

                match result {
                    Ok(ty) => {
                        let ty = Rc::new(ty);
                        self.add_type(expression.id, ty.clone());
                        return ty;
                    }
                    Err(err) => {
                        self.add_type(expression.id, Rc::new(Type::Error));
                        self.db.report_error(SemanticError::Raw { message: err, span: left.span.merge(right.span) });
                        return Rc::new(Type::Error);
                    }
                };
            }
            ExpressionKind::Unary { operand, op } => {
                let operand_type = self.resolve_expression(&operand, None);

                let result = match op {
                    UnaryOp::AddressOf => {
                        if *operand_type == Type::Error {
                            self.add_type(expression.id, Rc::new(Type::Error));
                            return Rc::new(Type::Error);
                        }
                        Rc::new(Type::Pointer(operand_type))
                    }
                    UnaryOp::Deref => {
                        if let Type::Pointer(inner) = &*operand_type {
                            inner.clone()
                        } else {
                            self.db.report_error(SemanticError::InvalidPointerOp {
                                op: op.clone(),
                                operand_type: operand_type.clone(),
                                span: operand.span,
                                reason: "Cannot dereference non-pointer type".to_string(),
                            });
                            self.add_type(expression.id, Rc::new(Type::Error));
                            Rc::new(Type::Error)
                        }
                    }
                    _ => {
                        if *operand_type == Type::Error {
                            self.db.report_error(SemanticError::InvalidUnaryOp {
                                op: op.clone(),
                                span: operand.span,
                                reason: "Operand resolved to Error type".to_string(),
                            });
                            return Rc::new(Type::Error);
                        }

                        operand_type
                    }
                };

                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::FunctionCall(func) => {
                let Some(symbol) = self.get_symbol(func.function.id).cloned() else {
                    println!("Function not found {:?}", func.function);
                    return Rc::new(Type::Error);
                };

                let Some(signature) = symbol.get_function() else {
                    return Rc::new(Type::Error);
                };

                for (i, arg) in func.arguments.iter().enumerate() {
                    let arg_expr = match arg {
                        tahuc_ast::nodes::expressions::Argument::Positional(expr) => expr,
                        tahuc_ast::nodes::expressions::Argument::Named { value, .. } => value,
                    };
                    let param_ty = signature.parameters[i].var_type.clone();
                    self.resolve_expression(arg_expr, Some(param_ty));
                }

                let mut return_type = self.resolve_expression(&func.function, None);

                if return_type.is_named() {
                    let name = return_type.get_ty_named().unwrap();
                    if let Ok((_, ty)) = self.lookup_type(name, func.function.span) {
                        return_type = ty;
                    }
                }

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
                        return Rc::new(Type::Error);
                    }
                }

                let element_expected = match expected {
                    Some(ty) => {
                        match &*ty {
                            Type::Array { ty, .. } => Some(ty.clone()),
                            _ => None,
                        }
                    }
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
                        return Rc::new(Type::Error);
                    }
                }

                let result = Rc::new(Type::Array { ty: first_ty, size: elements.len() });
                self.add_type(expression.id, result.clone());
                result
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let arr_ty = self.resolve_expression(&array, None);
                let idx_ty = self.resolve_expression(&index, None);

                if *idx_ty != Type::Int {
                    self.db.report_error(SemanticError::Raw {
                        message: format!("Array index must be Int, Found {:?}", idx_ty),
                        span: index.span,
                    });
                }

                let elem_type = match &*arr_ty {
                    Type::Array { ty, .. } => ty.clone(),
                    Type::String => Rc::new(Type::Primitive(PrimitiveType::Char)),
                    Type::Pointer(inner) => inner.clone(),
                    _ => {
                        println!("Error: trying to index non-array {}", arr_ty);
                        // error: trying to index non-array
                        Rc::new(Type::Error)
                    }
                };
                
                self.add_type(expression.id, elem_type.clone());

                elem_type
            }
            ExpressionKind::MemberAccess { object, member } => {
                let obj_ty = self.resolve_expression(&object, None);

                match &*obj_ty {
                    Type::Struct { fields, .. } => {
                        for (name, ty) in &**fields {
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
                    return Rc::new(Type::Error);
                }

                self.add_type(expression.id, ty.clone());
                ty.clone()
            }
            ExpressionKind::StructLiteral { object, fields } => {
                let struct_ty = self.resolve_expression(&object, None);
                self.debug(format!("resolving struct literal {}", struct_ty));

                match &*struct_ty {
                    Type::Struct { name, fields: fields_ty } => {
                        if fields.len() != fields_ty.len() {
                            self.db.report_error(SemanticError::Raw {
                                message: format!("Expected {} fields, found {}", fields_ty.len(), fields.len()),
                                span: expression.span,
                            });
                            return Rc::new(Type::Error);
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
                                    let value = self.read_variable(field_expr.kind.name.id);
                                    value
                                }
                            };

                            self.add_type(field_expr.id, ty.clone());

                            if idx >= fields_ty.len() || &fields_ty[idx].0 != field_name {
                                let field_struct = self.config.str_pool.get(fields_ty[idx].0);
                                let field_name = self.config.str_pool.get(*field_name);
                                self.db.report_error(SemanticError::Raw {
                                    message: format!("Expected field name '{}', found '{}'", field_struct, field_name),
                                    span: field_expr.span,
                                });
                                continue;
                            }

                            self.infer_struct_literal(&fields_ty[idx].1, ty, &name, field_expr.span);
                        }

                        self.add_type(expression.id, struct_ty.clone());
                    }
                    _ => {
                        self.db.report_error(SemanticError::Raw {
                            message: format!("Expected struct type, found {:?}", struct_ty),
                            span: expression.span, 
                        });
                    }
                }
                

                struct_ty
            }
        }
    }

    fn read_variable(&mut self, id: NodeId) -> Rc<Type> {
        if let Some(symbol) = self.get_symbol(id) {
            self.debug(format!("symbol `{}` found", id));
            let ty = symbol.get_type();
            self.add_type(id, ty.clone());
            return ty;
        }

        self.debug(format!("symbol `{}` not found", id));
        self.add_type(id, Rc::new(Type::Error));
        Rc::new(Type::Error)
    }

    fn infer_struct_literal(&mut self, field_ty: &Type, result_ty: Rc<Type>, _struct_name: &StrId, span: Span) {
        if *field_ty != *result_ty {
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
                    (Type::Pointer(_), Type::Int) => Ok(Type::Pointer(Rc::new(Type::Int))),
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

    fn lookup_function(&mut self, name: StrId) -> Option<&FunctionSymbol> {
        self.db.lookup_fn(self.file_id, name)
    }

    fn lookup_variable(&mut self, name: StrId) -> Option<&VariableSymbol> {
        self.db.lookup_var(self.file_id, name)
    }

    fn add_type(&mut self, id: NodeId, ty: Rc<Type>) {
        self.db.add_type(self.file_id, id, ty);
    }

    fn get_type(&mut self, id: NodeId) -> Option<&Rc<Type>> {
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

    fn debug(&self, _msg: String) {
        // println!("DEBUG [TYPE] - {}", _msg);
    }

    #[allow(dead_code)]
    fn resolve_type_named(&mut self, ty: Rc<Type>, span: Span) -> Result<Rc<Type>, ()> {
        match &*ty {
            Type::Named(name) => {
                if let Ok((_, result)) = self.lookup_type(*name, span) {
                    return Ok(result);
                }
                Err(())
            }
            Type::Pointer(inner) => {
                Ok(Rc::new(Type::Pointer(self.resolve_type_named(inner.clone(), span)?)))
            }
            Type::Array { ty, size } => {
                Ok(Rc::new(Type::Array {
                    ty: self.resolve_type_named(ty.clone(), span)?,
                    size: *size,
                }))
            }
            Type::Struct { name, fields } => {
                let mut new_fields = Vec::new();
                for (name, ty) in fields.iter() {
                    new_fields.push((name.clone(), self.resolve_type_named(ty.clone(), span)?));
                }
                Ok(Rc::new(Type::Struct {
                    name: *name,
                    fields: Rc::new(new_fields),
                }))
            }
            _ => Ok(ty)
        }
    }

    fn lookup_type(&mut self, name: StrId, span: Span) -> Result<(StrId, Rc<Type>), ()> {
        // lookup struct, 
        // enum, type alias (future feature)
        let structs = self.db.lookup_struct_with_import(self.file_id, name, false);
        if structs.len() == 1 {
            let symbol = structs[0].clone();
            if self.file_id != symbol.file_id {
                if !structs[0].visibility.is_public() {
                    let name = self.config.str_pool.get(symbol.name);
                    self.db.report_error(SemanticError::PrivateStruct {
                        name: name.to_string(),
                        span: symbol.span,
                    });
                    return Err(());
                }
            }

            return Ok((name, structs[0].ty.clone()));
        } else if structs.len() > 1 {
            Err(())
        } else {
            let name = self.config.str_pool.get(name);
            self.db.report_error(SemanticError::Raw {
                message: format!("Cannot resolve type `{}`", name),
                span: span,
            });
            Err(())
        }
    }
}
