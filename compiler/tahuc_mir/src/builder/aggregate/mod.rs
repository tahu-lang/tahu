use tahuc_ast::Type;
use tahuc_hir::hir::HirExpression;

use crate::{
    builder::builder::Builder,
    mir::{
        instruction::MirOperand,
        ty::{MirType, ToMirType},
    },
};

impl Builder {
    pub(crate) fn build_aggregate(
        &mut self,
        expression: &HirExpression,
        addr: MirOperand,
        agg_ty: &MirType,
    ) -> MirOperand {
        match expression {
            HirExpression::ArrayLiteral { elements, ty } => {
                let element_ty = match ty {
                    Type::Array { ty, .. } => ty.as_ref().clone(),
                    _ => panic!("ArrayLiteral must have Array type"),
                };

                for (i, e_expr) in elements.iter().enumerate() {
                    let gep_operand = self.new_local(element_ty.to_mir_ty());

                    self.get_element_ptr(
                        gep_operand,
                        addr.clone(),
                        vec![
                            MirOperand::new_constant_int(0),
                            MirOperand::new_constant_int(i as i64),
                        ],
                        element_ty.to_mir_ty(),
                    );

                    // Build value
                    match e_expr.get_type().to_mir_ty() {
                        ty if ty.is_aggregate() => {
                            self.build_aggregate(e_expr, MirOperand::Local(gep_operand), agg_ty);
                        }
                        _ => {
                            let value = self.build_expression(e_expr, false);
                            self.store(
                                MirOperand::Local(gep_operand),
                                value,
                                element_ty.to_mir_ty(),
                            );
                        }
                    }
                }

                addr.clone()
            }
            _ => {
                panic!("Trying to build {:?}", expression);
            }
        }
    }
}
