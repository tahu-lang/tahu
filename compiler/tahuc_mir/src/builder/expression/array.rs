use tahuc_ast::Type;
use tahuc_hir::hir::HirExpression;

use crate::{builder::builder::Builder, mir::{instruction::MirOperand, ty::{MirType, ToMirType}}};

impl Builder {
    pub(crate) fn build_array_literal(&mut self, elements: &Vec<HirExpression>, ty: &Type) -> MirOperand {
        let element_ty = match ty {
            Type::Array { ty, .. } => ty.as_ref().clone(),
            _ => panic!("ArrayLiteral must have Array type"),
        }.to_mir_ty();

        // allocate array
        let arr_ty = MirType::Array { ty: Box::new(element_ty.clone()), size: elements.len() };
        let array_ptr_id = self.new_local(arr_ty.clone());

        self.alloca(array_ptr_id, arr_ty);

        for (i, e_expr) in elements.iter().enumerate() {
            let element_ptr_id = self.new_local(element_ty.clone());

            self.get_element_ptr(
                element_ptr_id,
                MirOperand::Local(array_ptr_id),
                vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::new_constant_int(i as i64),
                ],
                element_ty.clone(),
            );

            if e_expr.get_type().to_mir_ty().is_aggregate() {
                self.build_expression(e_expr, false);
            } else {
                let element_value = self.build_expression(e_expr, false);
                self.store(MirOperand::Local(element_ptr_id), element_value, element_ty.clone());
            }
        }

        MirOperand::Local(array_ptr_id)
    }

    pub(crate) fn array_access(&mut self, array: &HirExpression, index: &HirExpression, ty: MirType, need_addr: bool) -> MirOperand {
        let addr = self.build_expression(&array, true);
        let idx = self.build_expression(&index, false);

        let temp_ptr = self.new_local(ty.clone());
        self.get_element_ptr(temp_ptr, addr, vec![
            MirOperand::new_constant_int(0),
            idx
        ], ty.clone());

        if need_addr {
            MirOperand::Local(temp_ptr)
        } else {
            let value_id = self.new_local(ty.clone());
            self.load(value_id, MirOperand::Local(temp_ptr), ty.clone());
            MirOperand::Local(value_id)
        }
    }
}