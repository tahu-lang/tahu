use tahuc_llvm::{opaque::LLVMValueRef, LLVMTypeKind};

use super::super::values::{ArrayValue, FloatValue, FunctionValue, IntValue, PointerValue, StructValue};
use super::{AsValueRef, Values};

macro_rules! enum_value_set {
    ($enum_name:ident: $($args:ident),*) => (
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum $enum_name {
            $(
                $args($args),
            )*
        }

        impl AsValueRef for $enum_name {
            fn as_value_ref(&self) -> LLVMValueRef {
                match *self {
                    $(
                        $enum_name::$args(ref t) => t.as_value_ref(),
                    )*
                }
            }
        }

        $(
            impl From<$args> for $enum_name {
                fn from(value: $args) -> $enum_name {
                    $enum_name::$args(value)
                }
            }

            impl PartialEq<$args> for $enum_name {
                fn eq(&self, other: &$args) -> bool {
                    self.as_value_ref() == other.as_value_ref()
                }
            }

            impl PartialEq<$enum_name> for $args {
                fn eq(&self, other: &$enum_name) -> bool {
                    self.as_value_ref() == other.as_value_ref()
                }
            }

            impl TryFrom<$enum_name> for $args {
                type Error = ();

                fn try_from(value: $enum_name) -> Result<Self, Self::Error> {
                    match value {
                        $enum_name::$args(ty) => Ok(ty),
                        _ => Err(()),
                    }
                }
            }
        )*
    );
}

enum_value_set! { BasicValue: IntValue, FloatValue, FunctionValue, PointerValue, ArrayValue, StructValue }

impl BasicValue {
    pub fn new(value: Values) -> BasicValue {
        match value.kind() {
            LLVMTypeKind::LLVMFloatTypeKind
            | LLVMTypeKind::LLVMFP128TypeKind
            | LLVMTypeKind::LLVMDoubleTypeKind
            | LLVMTypeKind::LLVMHalfTypeKind
            | LLVMTypeKind::LLVMX86_FP80TypeKind
            | LLVMTypeKind::LLVMPPC_FP128TypeKind => BasicValue::FloatValue(FloatValue::new(value)),
            LLVMTypeKind::LLVMIntegerTypeKind => BasicValue::IntValue(IntValue::new(value)),
            LLVMTypeKind::LLVMStructTypeKind => BasicValue::StructValue(StructValue::new(value)),
            LLVMTypeKind::LLVMPointerTypeKind => BasicValue::PointerValue(PointerValue::new(value)),
            LLVMTypeKind::LLVMArrayTypeKind => BasicValue::ArrayValue(ArrayValue::new(value)),

            _ => unreachable!("The given type is not a basic type., {:?}", value.kind()),
        }
    }

    pub fn is_int_value(self) -> bool {
        matches!(self, BasicValue::IntValue(_))
    }

    pub fn is_float_value(self) -> bool {
        matches!(self, BasicValue::FloatValue(_))
    }

    pub fn is_pointer_value(self) -> bool {
        matches!(self, BasicValue::PointerValue(_))
    }

    pub fn into_int_value(self) -> IntValue {
        if let BasicValue::IntValue(v) = self {
            v
        } else {
            panic!("Found {:?} but expected the IntValue variant", self)
        }
    }

    pub fn into_float_value(self) -> FloatValue {
        if let BasicValue::FloatValue(v) = self {
            v
        } else {
            panic!("Found {:?} but expected the FloatValue variant", self)
        }
    }

    pub fn into_ptr_value(self) -> PointerValue {
        if let BasicValue::PointerValue(v) = self {
            v
        } else {
            panic!("Found {:?} but expected the PointerValue variant", self)
        }
    }
}