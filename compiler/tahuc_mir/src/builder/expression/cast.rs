use tahuc_ast::ty::Type;
use tahuc_hir::hir::HirExpression;

use crate::{
    builder::builder::Builder,
    mir::{
        instruction::{CastKind, MirOperand},
        ty::{MirType, ToMirType},
    },
};

impl Builder {
    pub(crate) fn build_cast(
        &mut self,
        value: &Box<HirExpression>,
        from: &Type,
        ty: &Type,
    ) -> MirOperand {
        let value = self.build_expression(value, false);
        let target = self.new_local(ty.to_mir_ty());

        let from_ty = from.to_mir_ty();
        let to_ty = ty.to_mir_ty();
        let kind = self.infer_casting(from_ty.clone(), to_ty.clone());

        self.cast(target, kind, value, from_ty, to_ty);
        MirOperand::Local(target)
    }

    fn is_signed_int(&self, ty: &MirType) -> bool {
        matches!(
            ty,
            MirType::I8 | MirType::I16 | MirType::I32 | MirType::I64 | MirType::Isize
        )
    }

    fn is_unsigned_int(&self, ty: &MirType) -> bool {
        matches!(
            ty,
            MirType::U8 | MirType::U16 | MirType::U32 | MirType::U64 | MirType::Usize
        )
    }

    fn is_int(&self, ty: &MirType) -> bool {
        self.is_signed_int(ty) || self.is_unsigned_int(ty)
    }

    fn is_float(&self, ty: &MirType) -> bool {
        matches!(ty, MirType::F32 | MirType::F64)
    }

    fn is_bool(&self, ty: &MirType) -> bool {
        matches!(ty, MirType::Bool)
    }

    fn int_width(&self, ty: &MirType) -> u32 {
        match ty {
            MirType::I8 | MirType::U8 => 8,
            MirType::I16 | MirType::U16 => 16,
            MirType::I32 | MirType::U32 => 32,
            MirType::I64 | MirType::U64 => 64,

            // placeholder
            // get width target
            MirType::Isize | MirType::Usize => 64,
            _ => 0,
        }
    }

    fn infer_casting(&self, from: MirType, to: MirType) -> CastKind {
        if from == to {
            return CastKind::NoOp;
        }

        // pointer casts
        if let (MirType::Pointer(_), MirType::Pointer(_)) = (&from, &to) {
            return CastKind::BitCast;
        }
        if let (MirType::Pointer(_), to_int) = (&from, &to) {
            if self.is_int(to_int) {
                return CastKind::PtrToInt;
            }
        }
        if let (from_int, MirType::Pointer(_)) = (&from, &to) {
            if self.is_int(from_int) {
                return CastKind::IntToPtr;
            }
        }

        // integer → integer
        if self.is_signed_int(&from) && self.is_signed_int(&to) {
            return if self.int_width(&from) < self.int_width(&to) {
                CastKind::SExt
            } else {
                CastKind::Trunc
            };
        }
        if self.is_unsigned_int(&from) && self.is_unsigned_int(&to) {
            return if self.int_width(&from) < self.int_width(&to) {
                CastKind::ZExt
            } else {
                CastKind::Trunc
            };
        }
        if (self.is_signed_int(&from) && self.is_unsigned_int(&to))
            || (self.is_unsigned_int(&from) && self.is_signed_int(&to))
        {
            return if self.int_width(&from) < self.int_width(&to) {
                CastKind::ZExt
            } else {
                CastKind::Trunc
            };
        }

        // integer → bool
        if self.is_int(&from) && self.is_bool(&to) {
            return CastKind::Trunc;
        }
        // bool → integer
        if self.is_bool(&from) && self.is_int(&to) {
            return CastKind::ZExt;
        }

        // integer → float
        if self.is_int(&from) && self.is_float(&to) {
            return if self.is_signed_int(&from) {
                CastKind::SIToFP
            } else {
                CastKind::UIToFP
            };
        }
        // float → integer
        if self.is_float(&from) && self.is_int(&to) {
            return if self.is_signed_int(&to) {
                CastKind::FPToSI
            } else {
                CastKind::FPToUI
            };
        }

        // float → float
        if self.is_float(&from) && self.is_float(&to) {
            let w_from = if from == MirType::F32 { 32 } else { 64 };
            let w_to = if to == MirType::F32 { 32 } else { 64 };
            return if w_from < w_to {
                CastKind::SIToFP
            } else {
                CastKind::Trunc
            };
        }

        // char → int
        if from == MirType::Char && self.is_signed_int(&to) {
            return CastKind::ZExt;
        }
        if self.is_signed_int(&from) && to == MirType::Char {
            return CastKind::SExt;
        }

        // fallback
        CastKind::NoOp
    }
}
