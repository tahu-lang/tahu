use std::rc::Rc;

use tahuc_ast::ty::Type;
use tahuc_hir::hir::{HirExpression, HirLiteral, HirVariable};
use tahuc_module::config::Config;

use crate::{
    builder::builder::Builder,
    mir::{instruction::MirOperand, ty::MirType},
};

impl Builder {
    pub(crate) fn build_variable_statement(
        &mut self,
        config: &mut Config,
        variable: &HirVariable,
        initializer: &Option<HirExpression>,
    ) {
        if let Some(initializer) = initializer {
            self.build_initializer(config, variable, initializer);
        } else {
            let ty = self.ty_to_mir(config, &variable.ty);
            let local_id = self.new_local(ty.clone());
            if variable.ty.is_nullable() {
                let inner_ty = match &*variable.ty {
                    Type::Nullable(inner) => self.ty_to_mir(config, &inner),
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
                let inner = match &*variable.ty {
                    Type::Nullable(inner) => self.ty_to_mir(config, inner),
                    _ => unreachable!(),
                };
                let expr = HirExpression::Literal {
                    value: HirLiteral::Null(Rc::new(Type::Null)),
                    ty: variable.ty.clone(),
                    span: variable.span.clone(),
                };
                self.build_nullable(config, &expr, MirOperand::Local(local_id), &ty.clone(), &inner);
            }
        }
    }

    fn build_initializer(&mut self, config: &mut Config, variable: &HirVariable, initializer: &HirExpression) {
        let init_ty = self.ty_to_mir(config, &initializer.get_type());
        match initializer {
            HirExpression::ArrayLiteral { elements, ty, .. } => {
                let element_ty = match &**ty {
                    Type::Array { ty, .. } => ty.as_ref().clone(),
                    _ => panic!("ArrayLiteral must have Array type"),
                };
                let element_ty = self.ty_to_mir(config, &element_ty);
                let ty = MirType::Array {
                    ty: Box::new(element_ty),
                    size: elements.len(),
                };

                let local_id = self.new_local(ty.clone());
                self.add_variable(variable.id, local_id, ty.clone());

                self.alloca(local_id, ty.clone());

                self.build_aggregate(config, &initializer, MirOperand::Local(local_id), &init_ty);
            }
            _ => {
                let ty = self.ty_to_mir(config, &variable.ty);
                let local_id = self.new_local(ty.clone());
                if variable.ty.is_nullable() {
                    let inner_ty = match &*variable.ty {
                        Type::Nullable(inner) => self.ty_to_mir(config, inner),
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
                    let inner = match &*variable.ty {
                        Type::Nullable(inner) => self.ty_to_mir(config, inner),
                        _ => unreachable!(),
                    };
                    self.build_nullable(config, &initializer, MirOperand::Local(local_id), &ty, &inner);
                } else if ty.is_aggregate() {
                    self.build_aggregate(config, &initializer, MirOperand::Local(local_id), &init_ty);
                } else {
                    let source = self.build_expression(config, initializer, false);
                    self.store(MirOperand::Local(local_id), source, ty);
                }
            }
        }
    }
}
