use tahuc_mir::mir::ty::MirType;

use crate::{codegen::Codegen, llvm::{FloatType, IntType, Type}};

impl Codegen {
    pub(crate) fn int_ty(&self, ty: &MirType) -> IntType {
        match ty {
            MirType::I8 | MirType::U8 => self.context.i8_ty(),
            MirType::I16 | MirType::U16 => self.context.i16_ty(),
            MirType::I32 | MirType::U32 => self.context.i32_ty(),
            MirType::I64 | MirType::U64 => self.context.i64_ty(),
            MirType::Isize | MirType::Usize => self.context.i32_ty(),
            _ => panic!("Unsupported type {:?}", ty),
        }
    }

    pub(crate) fn float_ty(&self, ty: &MirType) -> FloatType {
        match ty {
            MirType::F32 => self.context.f32_ty(),
            MirType::F64 => self.context.f64_ty(),
            _ => panic!("Unsupported type {:?}", ty),
        }
    }

    pub(crate) fn ty_to_llvm(&mut self, ty: &MirType) -> Type {
        match ty {
            // primitive
            MirType::I8 | MirType::U8 => self.context.i8_ty().as_ty(),
            MirType::I16 | MirType::U16 => self.context.i16_ty().as_ty(),
            MirType::I32 | MirType::U32 => self.context.i32_ty().as_ty(),
            MirType::I64 | MirType::U64 => self.context.i64_ty().as_ty(),

            MirType::F32 => self.context.f32_ty().as_ty(),
            MirType::F64 => self.context.f64_ty().as_ty(),

            MirType::Bool => self.context.bool_ty().as_ty(),
            MirType::Char => self.context.i8_ty().as_ty(),
            MirType::Unit => self.context.void_ty().as_ty(),

            MirType::Pointer(_) => self.context.ptr_ty().as_ty(),

            MirType::String => self.context.ptr_ty().as_ty(),
            MirType::Array { ty, size } => {
                let inner = self.ty_to_llvm(ty);
                self.context.array_ty(inner, *size as u32).as_ty()
            }
            MirType::Struct { name, fields } => {
                if let Some(ty) = self.struct_types.get(name) {
                    return ty.as_ty();
                }
                let str = self.context.struct_ty(name);
                self.struct_types.insert(name.clone(), str.clone());

                let element_types: Vec<Type> =
                    fields.iter().map(|f| self.ty_to_llvm(&f.1)).collect();
                let packed = false;

                str.set_body(element_types.as_slice(), packed);

                str.as_ty()
            }
            _ => panic!("not implemented type {}", ty),
        }
    }
}