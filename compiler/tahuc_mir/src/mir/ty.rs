use std::fmt::Display;

use tahuc_ast::ty::*;

#[derive(Debug, Clone)]
pub enum MirConstant {
    String(String),
    Char(char),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Null(MirType),
}

impl MirConstant {
    pub fn get_type(&self) -> MirType {
        match self {
            MirConstant::String(_) => MirType::String,
            MirConstant::Char(_) => MirType::Char,
            MirConstant::Integer(_) => MirType::I32,
            MirConstant::Float(_) => MirType::F64,
            MirConstant::Bool(_) => MirType::Bool,
            MirConstant::Null(_) => MirType::Null,
        }
    }
}

impl Display for MirConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirConstant::String(s) => write!(f, "{}", s),
            MirConstant::Char(c) => write!(f, "{}", c),
            MirConstant::Integer(i) => write!(f, "{}", i),
            MirConstant::Float(float) => write!(f, "{}", float),
            MirConstant::Bool(b) => write!(f, "{}", b),
            MirConstant::Null(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirType {
    I8, I16, I32, I64, Isize,
    U8, U16, U32, U64, Usize,
    F32, F64,
    Bool,
    Char,
    String,
    Unit,
    Null,
    Pointer(Box<MirType>),
    Array {
        ty: Box<MirType>,
        size: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, MirType)>,
    },
    Named(String),
}

impl MirType {
    pub fn is_scalar(&self) -> bool {
        match self {
            MirType::I8
            | MirType::I16
            | MirType::I32
            | MirType::I64
            | MirType::Isize
            | MirType::U8
            | MirType::U16
            | MirType::U32
            | MirType::U64
            | MirType::Usize
            | MirType::F32
            | MirType::F64
            | MirType::Bool
            | MirType::Char
            | MirType::String
            | MirType::Null 
            | MirType::Pointer(_) => true,

            _ => false,
        }
    }

    pub fn is_aggregate(&self) -> bool {
        match self {
            MirType::Array { .. } => true,
            MirType::Named(_) => true,
            MirType::Struct { .. } => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            MirType::Unit => true,
            // MirType::Nullable(inner) => inner.is_void(),
            _ => false,
        }
    }

    pub fn null_value(&self) -> MirConstant {
        match self {
            MirType::String => MirConstant::String(format!("null")),
            MirType::Char => MirConstant::Char('\0'),
            MirType::I8 
            | MirType::I16
            | MirType::I32 
            | MirType::I64
            | MirType::Isize
            | MirType::U8
            | MirType::U16
            | MirType::U32
            | MirType::U64
            | MirType::Usize => MirConstant::Integer(0),
            MirType::F32 
            | MirType::F64 => MirConstant::Float(0.0),
            MirType::Bool => MirConstant::Bool(false),
            _ => {
                MirConstant::Null(MirType::Null)
            }
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            MirType::String => true,
            _ => false,
        }
    }
}

pub trait ToMirType {
    fn to_mir_ty(&self) -> MirType;
}

impl ToMirType for Type {
    fn to_mir_ty(&self) -> MirType {
        match self {
            Type::Primitive(PrimitiveType::I8) => MirType::I8,
            Type::Primitive(PrimitiveType::I16) => MirType::I16,
            Type::Primitive(PrimitiveType::I32) => MirType::I32,
            Type::Primitive(PrimitiveType::I64) => MirType::I64,
            Type::Primitive(PrimitiveType::Isize) => MirType::Isize,
            Type::Primitive(PrimitiveType::U8) => MirType::U8,
            Type::Primitive(PrimitiveType::U16) => MirType::U16,
            Type::Primitive(PrimitiveType::U32) => MirType::U32,
            Type::Primitive(PrimitiveType::U64) => MirType::U64,
            Type::Primitive(PrimitiveType::Usize) => MirType::Usize,
            Type::Primitive(PrimitiveType::F32) => MirType::F32,
            Type::Primitive(PrimitiveType::F64) => MirType::F64,
            Type::Primitive(PrimitiveType::Bool) => MirType::Bool,
            Type::Primitive(PrimitiveType::Unit) => MirType::Unit,
            Type::Primitive(PrimitiveType::Char) => MirType::Char,

            Type::String => MirType::String,
            Type::Int => MirType::I32,
            Type::Double => MirType::F64,
            Type::Array { ty, size } => MirType::Array { ty: Box::new(ty.to_mir_ty()), size: *size },
            Type::Nullable(ty) => {
                MirType::Struct {
                    name: format!("{}?", ty),
                    fields: vec![
                        (format!("is_null"), MirType::Bool),
                        (format!("value"), ty.to_mir_ty()),
                    ]
                }
            },
            Type::Pointer(ty) => MirType::Pointer(Box::new(ty.to_mir_ty())),
            Type::Named(s) => MirType::Named(s.clone()),
            Type::Null => MirType::Null,
            _ => {
                panic!("Unsupported type: {:?}", self);
            }
        }
    }
}

impl Display for MirType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirType::I8 => write!(f, "i8"),
            MirType::I16 => write!(f, "i16"),
            MirType::I32 => write!(f, "i32"),
            MirType::I64 => write!(f, "i64"),
            MirType::Isize => write!(f, "isize"),
            MirType::U8 => write!(f, "u8"),
            MirType::U16 => write!(f, "u16"),
            MirType::U32 => write!(f, "u32"),
            MirType::U64 => write!(f, "u64"),
            MirType::Usize => write!(f, "usize"),
            MirType::F32 => write!(f, "f32"),
            MirType::F64 => write!(f, "f64"),
            MirType::Unit => write!(f, "()"),
            MirType::Bool => write!(f, "i1"),
            MirType::Char => write!(f, "i8"),
            MirType::String => write!(f, "i8*"),
            MirType::Pointer(inner) => write!(f, "{}*", inner),
            MirType::Null => write!(f, "null"),
            MirType::Array { ty, size } => write!(f, "[{} x {}]", size, ty),
            MirType::Struct { name, fields } => {
                if fields.is_empty() {
                    write!(f, "struct {}", name)
                } else {
                    let fields_str = fields
                        .iter()
                        .map(|(n, t)| format!("{}: {}", n, t))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "struct {} {{ {} }}", name, fields_str)
                }
            },
            MirType::Named(name) => write!(f, "%{}", name),
        }
    }
}

pub enum MirTypeKind {
    SignedInt,
    UnsignedInt,
    Float,
    Bool,
    Other,
}

impl MirType {
    pub fn kind(&self) -> MirTypeKind {
        match self {
            MirType::I8 | MirType::I16 | MirType::I32 | MirType::I64 | MirType::Isize => MirTypeKind::SignedInt,
            MirType::U8 | MirType::U16 | MirType::U32 | MirType::U64 | MirType::Usize => MirTypeKind::UnsignedInt,
            MirType::F32 | MirType::F64 => MirTypeKind::Float,
            MirType::Bool => MirTypeKind::Bool,
            _ => MirTypeKind::Other,
        }
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            MirType::I8 | MirType::U8 => 8,
            MirType::I16 | MirType::U16 => 16,
            MirType::I32 | MirType::U32 => 32,
            MirType::I64 | MirType::U64 => 64,

            // placeholder
            MirType::Isize | MirType::Usize => 32,
            MirType::F32 => 32,
            MirType::F64 => 64,
            _ => 0,
        }
    }
}