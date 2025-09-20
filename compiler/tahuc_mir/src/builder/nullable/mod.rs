use tahuc_hir::hir::{HirExpression, HirLiteral};

use crate::{
    builder::builder::{Builder},
    mir::{
        instruction::MirOperand,
        ty::{MirConstant, MirType},
    },
};

impl Builder {
    pub(crate) fn build_nullable(
        &mut self,
        expression: &HirExpression,
        addr: MirOperand,
        base_ty: &MirType,
        inner_ty: &MirType,
    ) {
        // field 0: is_null
        let is_null_ptr = self.new_local(MirType::Boolean);
        self.get_element_ptr(
            is_null_ptr,
            addr.clone(),
            vec![
                MirOperand::new_constant_int(0),
                MirOperand::new_constant_int(0),
            ],
            base_ty.clone(),
        );

        // tentukan nilai is_null
        let is_null_value = match expression {
            HirExpression::Literal {
                value: HirLiteral::Null(_),
                ..
            } => MirOperand::Constant(MirConstant::Boolean(true)),
            _ => MirOperand::Constant(MirConstant::Boolean(false)),
        };

        self.store(
            MirOperand::Local(is_null_ptr),
            is_null_value,
            MirType::Boolean,
        );

        // field 1: value
        let value_ptr = self.new_local(inner_ty.clone());
        self.get_element_ptr(
            value_ptr,
            addr.clone(),
            vec![
                MirOperand::new_constant_int(0),
                MirOperand::new_constant_int(1),
            ],
            base_ty.clone(),
        );

        if let HirExpression::Literal {
            value: HirLiteral::Null(_),
            ..
        } = expression
        {
            // if null dont store value
        } else {
            let val = self.build_expression(expression, false);
            self.store(MirOperand::Local(value_ptr), val, inner_ty.clone());
        }
    }

    // pub(crate) fn nullable_field(
    //     &mut self,
    //     ptr: MirOperand,
    //     access: NullableAccess,
    //     base_ty: MirType,
    //     inner_ty: MirType,
    // ) -> MirOperand {
    //     let index = match access {
    //         NullableAccess::IsNull => 0,
    //         NullableAccess::Value => 1,
    //     };

    //     let field_ptr = self.new_local(base_ty.clone());
    //     self.get_element_ptr(
    //         field_ptr,
    //         ptr.clone(),
    //         vec![
    //             MirOperand::new_constant_int(0),
    //             MirOperand::new_constant_int(index),
    //         ],
    //         base_ty.clone(),
    //     );

    //     if matches!(access, NullableAccess::Value) {
    //         // Load hanya kalau butuh value
    //         let temp = self.new_local(inner_ty.clone());
    //         self.load(temp, MirOperand::Local(field_ptr), inner_ty.clone());
    //         MirOperand::Local(temp)
    //     } else {
    //         let temp = self.new_local(MirType::Boolean);
    //         self.load(temp, MirOperand::Local(field_ptr), MirType::Boolean);
    //         // is_null cukup ambil pointer
    //         MirOperand::Local(temp)
    //     }
    // }

    pub(crate) fn nullable_access_is_null(
        &mut self,
        ptr: MirOperand,
        base_ty: MirType,
    ) -> MirOperand {
        let field_ptr = self.new_local(base_ty.clone());
        self.get_element_ptr(
            field_ptr,
            ptr.clone(),
            vec![
                MirOperand::new_constant_int(0),
                MirOperand::new_constant_int(0),
            ],
            base_ty.clone(),
        );

        let temp = self.new_local(MirType::Boolean);
        self.load(temp, MirOperand::Local(field_ptr), MirType::Boolean);
        MirOperand::Local(temp)
    }

    pub(crate) fn nullable_access_value(
        &mut self,
        ptr: MirOperand,
        base_ty: MirType,
        inner_ty: MirType,
    ) -> MirOperand {
        let field_ptr = self.new_local(base_ty.clone());
        self.get_element_ptr(
            field_ptr,
            ptr.clone(),
            vec![
                MirOperand::new_constant_int(0),
                MirOperand::new_constant_int(1),
            ],
            base_ty.clone(),
        );

        let temp = self.new_local(inner_ty.clone());
        self.load(temp, MirOperand::Local(field_ptr), inner_ty.clone());
        MirOperand::Local(temp)
    }
}
