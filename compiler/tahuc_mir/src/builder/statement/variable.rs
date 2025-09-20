use tahuc_ast::Type;
use tahuc_hir::hir::{HirExpression, HirLiteral, HirVariable};

use crate::{
    builder::builder::Builder,
    mir::{
        instruction::MirOperand,
        ty::{MirType, ToMirType},
    },
};

impl Builder {
    pub(crate) fn build_variable_statement(
        &mut self,
        variable: &HirVariable,
        initializer: &Option<HirExpression>,
    ) {
        if let Some(initializer) = initializer {
            self.build_initializer(variable, initializer);
        } else {
            let ty = variable.ty.to_mir_ty();
            let local_id = self.new_local(ty.clone());
            if variable.ty.is_nullable() {
                let inner_ty = match &variable.ty {
                    Type::Nullable(inner) => inner.to_mir_ty(),
                    _ => unreachable!(),
                };
                self.add_variable_cfg(
                    variable.id,
                    self.new_variabel_nullable(local_id, ty.clone(), Some(inner_ty)),
                );
            } else {
                self.add_variable(variable.id, local_id, ty.clone());
            }

            self.alloca(local_id, ty.clone());

            if variable.ty.is_nullable() {
                let inner = match &variable.ty {
                    Type::Nullable(inner) => inner.to_mir_ty(),
                    _ => unreachable!(),
                };
                let expr = HirExpression::Literal {
                    value: HirLiteral::Null(Type::Null),
                    ty: variable.ty.clone(),
                };
                self.build_nullable(&expr, MirOperand::Local(local_id), &ty.clone(), &inner);
            }
        }
    }

    fn build_initializer(&mut self, variable: &HirVariable, initializer: &HirExpression) {
        let init_ty = initializer.get_type().to_mir_ty();
        match initializer {
            HirExpression::ArrayLiteral { elements, ty } => {
                let element_ty = match ty {
                    Type::Array { ty, .. } => ty.as_ref().clone(),
                    _ => panic!("ArrayLiteral must have Array type"),
                };
                let ty = MirType::Array {
                    ty: Box::new(element_ty.to_mir_ty()),
                    size: elements.len(),
                };

                let local_id = self.new_local(ty.clone());
                self.add_variable(variable.id, local_id, ty.clone());

                self.alloca(local_id, ty.clone());

                self.build_aggregate(&initializer, MirOperand::Local(local_id), &init_ty);
            }
            _ => {
                let ty = variable.ty.to_mir_ty();
                let local_id = self.new_local(ty.clone());
                if variable.ty.is_nullable() {
                    let inner_ty = match &variable.ty {
                        Type::Nullable(inner) => inner.to_mir_ty(),
                        _ => unreachable!(),
                    };
                    self.add_variable_cfg(
                        variable.id,
                        self.new_variabel_nullable(local_id, ty.clone(), Some(inner_ty)),
                    );
                } else {
                    self.add_variable(variable.id, local_id, ty.clone());
                }

                self.alloca(local_id, ty.clone());

                if variable.ty.is_nullable() {
                    let inner = match &variable.ty {
                        Type::Nullable(inner) => inner.to_mir_ty(),
                        _ => unreachable!(),
                    };
                    self.build_nullable(&initializer, MirOperand::Local(local_id), &ty, &inner);
                } else if ty.is_aggregate() {
                    self.build_aggregate(&initializer, MirOperand::Local(local_id), &init_ty);
                } else {
                    let source = self.build_expression(initializer, false);
                    self.store(MirOperand::Local(local_id), source, ty);
                }
            }
        }
    }
}
