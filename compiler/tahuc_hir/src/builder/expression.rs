use std::rc::Rc;

use tahuc_ast::{nodes::{expressions::{Argument, ExpressionKind}, Expression}, ty::Type};
use tahuc_lexer::token::Literal;

use crate::{builder::builder::HirBuilder, hir::{FunctionSignature, HirExpression, HirLiteral}};

impl<'a> HirBuilder<'a> {
    pub(crate) fn lower_expression(&mut self, expr: &Expression) -> Result<HirExpression, String> {
        match &expr.kind {
            ExpressionKind::Literal(literal) => {
                if let None = self.get_type(expr.id) {
                    println!("Error Literal {:?}", expr);
                }
                let ty = self.get_type(expr.id).unwrap().clone();
                Ok(HirExpression::Literal {
                    value: self.ast_literal_to_hir_literal(literal, ty.clone()),
                    ty: ty.clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::TemplateString { parts } => {
                let mut acc = HirExpression::Literal {
                    value: HirLiteral::String("".to_string()),
                    ty: self.get_type(expr.id).unwrap().clone(),
                    span: expr.span,
                };

                for part in parts {
                    let rhs = match part {
                        tahuc_ast::nodes::expressions::TemplatePart::Expression(inner) => {
                            self.lower_expression(inner)?
                        }
                        tahuc_ast::nodes::expressions::TemplatePart::Text(literal) => {
                            HirExpression::Literal {
                                value: HirLiteral::String(literal.to_string()),
                                ty: self.get_type(expr.id).unwrap().clone(),
                                span: expr.span,
                            }
                        }
                    };

                    // concat: acc = acc + rhs
                    acc = HirExpression::Binary {
                        left: Box::new(acc),
                        op: tahuc_ast::nodes::op::BinaryOp::Add,
                        right: Box::new(rhs),
                        ty: self.get_type(expr.id).unwrap().clone(),
                        span: expr.span,
                    };
                }

                Ok(acc)
            }
            ExpressionKind::Identifier(id) => {
                if let Some(symbol) = self.db.get_symbol(self.file_id, expr.id) {
                    if symbol.is_struct() {
                        if let Some(struct_map) = self.structs_map.get(&symbol.file_id) {
                            if let Some(hir_struct) = struct_map.get(&symbol.name) {
                                return Ok(HirExpression::StructType {
                                    id: hir_struct.id,
                                    ty: hir_struct.ty.clone(),
                                    span: expr.span,
                                })
                            }
                        }
                    } else if symbol.is_variable() {
                        for id in self.scope.iter_rev() {
                            if let Some(scope) = self.variables.get(id) {
                                if let Some(var) = scope.get(&symbol.name) {
                                    return Ok(HirExpression::Variable {
                                        id: var.id,
                                        ty: var.ty.clone(),
                                        span: expr.span,
                                    });
                                }
                            }
                        }
                    }
                }
                let name = self.config.str_pool.get(*id);
                Err(format!("Expression identifier not found {} {} {}", name, id, self.file_id))
            }
            ExpressionKind::Ternary {
                condition,
                then,
                otherwise,
            } => {
                let cond = self.lower_expression(&condition)?;
                let then_branch = self.lower_expression(&then)?;
                let else_branch = self.lower_expression(&otherwise)?;

                Ok(HirExpression::Ternary {
                    condition: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                    span: expr.span,
                })
            }
            ExpressionKind::Binary { left, op, right } => {
                let lhs = self.lower_expression(&left)?;
                let rhs = self.lower_expression(&right)?;

                Ok(HirExpression::Binary {
                    left: Box::new(lhs),
                    op: op.clone(),
                    right: Box::new(rhs),
                    ty: self.get_type(expr.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::Unary { op, operand } => {
                let right = self.lower_expression(&operand)?;

                Ok(HirExpression::Unary {
                    op: op.clone(),
                    operand: Box::new(right),
                    ty: self.get_type(expr.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::ArrayLiteral { elements } => {
                let mut arr = Vec::new();
                for element in elements {
                    arr.push(self.lower_expression(element)?);
                }

                Ok(HirExpression::ArrayLiteral {
                    elements: arr,
                    ty: self.get_type(expr.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let arr = self.lower_expression(&array)?;
                let idx = self.lower_expression(&index)?;

                Ok(HirExpression::ArrayAccess {
                    array: Box::new(arr),
                    index: Box::new(idx),
                    arr_ty: self.get_type(array.id).unwrap().clone(),
                    ty: self.get_type(expr.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::MemberAccess { object, member } => {
                let obj = self.lower_expression(&object)?;

                match &obj {
                    HirExpression::Variable { ty, .. } => {
                        if let Type::Struct { name, .. } = &**ty {
                            // Cari field dalam struct
                            if let Some(field_info) = self.find_struct_field(name, *member) {
                                return Ok(HirExpression::FieldAccess {
                                    object: Box::new(obj.clone()),
                                    field_name: *member,
                                    field_id: field_info.id,
                                    base_ty: ty.clone(),
                                    ty: field_info.ty.clone(),
                                    span: expr.span,
                                });
                            } else {
                                return Err(format!("Field '{}' not found in struct '{}'", member, name));
                            }
                        } else {
                            return Err(format!("Cannot access member '{}' on non-struct type {} id {}", member, ty, object.id));
                        }
                    }
                    _ => {}
                }

                Ok(HirExpression::MemberAccess {
                    object: Box::new(obj),
                    member: *member,
                    ty: self.get_type(expr.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::FunctionCall(callee) => {
                let mut hir_args: Vec<HirExpression> = Vec::new();

                for arg in &callee.arguments {
                    match arg {
                        Argument::Positional(arg) => {
                            hir_args.push(self.lower_expression(arg)?);
                        }
                        Argument::Named { value, .. } => {
                            hir_args.push(self.lower_expression(value)?);
                        }
                    }
                }

                match &callee.function.kind {
                    ExpressionKind::Identifier(ident) => {
                        let fid = self.get_function_id(callee.function.id, *ident).unwrap();
                        let signature = self.get_signature(fid);
                        return Ok(HirExpression::Call {
                            callee: fid,
                            arguments: hir_args,
                            signature: FunctionSignature {
                                args: signature.param_ty.clone(),
                            },
                            ty: self.get_type(callee.function.id).unwrap().clone(),
                            span: expr.span,
                        });
                    }
                    ExpressionKind::MemberAccess { .. } => {}
                    _ => {}
                }

                let fun = match &callee.function.kind {
                    ExpressionKind::Identifier(ident) => {
                        self.get_function_id(callee.function.id, *ident)
                    }
                    _ => None,
                };

                Ok(HirExpression::Call {
                    callee: fun.unwrap(),
                    signature: FunctionSignature {
                        args: Vec::new(),
                    },
                    arguments: hir_args,
                    ty: self.get_type(callee.function.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::Grouping(group) => {
                let hir_expr = self.lower_expression(&group)?;
                Ok(HirExpression::Grouping {
                    value: Box::new(hir_expr),
                    ty: self.get_type(group.id).unwrap().clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::Cast { ty, expression } => {
                let from_ty = self.get_type(expression.id).unwrap().clone();

                let hir_expr = self.lower_expression(&expression)?;

                Ok(HirExpression::Cast {
                    value: Box::new(hir_expr),
                    from: from_ty,
                    ty: ty.clone(),
                    span: expr.span,
                })
            }
            ExpressionKind::StructLiteral { object, fields } => {
                let obj = self.lower_expression(&object)?;

                let ty = obj.get_type();

                match *ty {
                    Type::Struct { .. } => {
                        let mut hir_fields: Vec<(usize, HirExpression)> = Vec::new();

                        for (index, field) in fields.iter().enumerate() {
                            if let Some(value) = field.kind.value.as_ref() {
                                hir_fields.push((index, self.lower_expression(value)?));
                            } else {
                                let value = self.lower_expression(&field.kind.name)?;
                                hir_fields.push((index, value));
                            }
                        }

                        Ok(HirExpression::StructLiteral {
                            object: Box::new(obj),
                            fields: hir_fields,
                            ty: self.get_type(object.id).unwrap().clone(),
                            span: expr.span,
                        })
                    }
                    _ => Err(format!("Struct literal must be a struct type, but got {:?}", ty))
                }
            
            }
        }
    }

    fn ast_literal_to_hir_literal(&mut self, literal: &Literal, ty: Rc<Type>) -> HirLiteral {
        match literal {
            Literal::String(str) => HirLiteral::String(str.to_string()),
            Literal::Char(char) => HirLiteral::Char(*char),
            Literal::Integer(int) => HirLiteral::Integer(*int),
            Literal::Double(float) => HirLiteral::Float(*float),
            Literal::Bool(bool) => HirLiteral::Bool(*bool),
            Literal::Null => HirLiteral::Null(ty),
        }
    }
}