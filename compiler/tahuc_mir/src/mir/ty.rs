use std::fmt::Display;

use tahuc_ast::Type;

#[derive(Debug, Clone)]
pub enum MirConstant {
    String(String),
    Char(char),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null(MirType),
}

impl MirConstant {
    pub fn get_type(&self) -> MirType {
        match self {
            MirConstant::String(_) => MirType::String,
            MirConstant::Char(_) => MirType::Char,
            MirConstant::Integer(_) => MirType::I32,
            MirConstant::Float(_) => MirType::F64,
            MirConstant::Boolean(_) => MirType::Boolean,
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
            MirConstant::Boolean(b) => write!(f, "{}", b),
            MirConstant::Null(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MirType {
    String,
    Char,
    I32,
    I64,
    F64,
    Boolean,
    Void,
    Null,
    Nullable(Box<MirType>),
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
            MirType::Char
            | MirType::I32
            | MirType::I64
            | MirType::F64
            | MirType::Boolean
            | MirType::Pointer(_)
            | MirType::String
            | MirType::Null => true,

            MirType::Nullable(inner) => inner.is_scalar(),
            _ => false,
        }
    }

    pub fn is_aggregate(&self) -> bool {
        match self {
            MirType::Array { .. } => true,
            MirType::Named(_) => true,
            MirType::Struct { .. } => true,
            MirType::Nullable(inner) => inner.is_aggregate(),
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            MirType::Void => true,
            // MirType::Nullable(inner) => inner.is_void(),
            _ => false,
        }
    }

    pub fn null_value(&self) -> MirConstant {
        match self {
            MirType::String => MirConstant::String(format!("null")),
            MirType::Char => MirConstant::Char('\0'),
            MirType::I32 | MirType::I64 => MirConstant::Integer(-1),
            MirType::F64 => MirConstant::Float(-1.0),
            MirType::Boolean => MirConstant::Boolean(false),
            _ => {
                MirConstant::Null(MirType::Null)
            }
        }
    }
}

pub trait ToMirType {
    fn to_mir_ty(&self) -> MirType;
}

impl ToMirType for Type {
    fn to_mir_ty(&self) -> MirType {
        match self {
            Type::String => MirType::String,
            Type::Char => MirType::Char,
            Type::Int => MirType::I32,
            Type::Double => MirType::F64,
            Type::Boolean => MirType::Boolean,
            Type::Void => MirType::Void,
            Type::Array { ty, size } => MirType::Array { ty: Box::new(ty.to_mir_ty()), size: *size },
            Type::Nullable(ty) => {
                MirType::Struct {
                    name: format!("{}?", ty),
                    fields: vec![
                        (format!("is_null"), MirType::Boolean),
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
            MirType::Void => write!(f, "void"),
            MirType::I32 => write!(f, "i32"),
            MirType::I64 => write!(f, "i64"),
            MirType::F64 => write!(f, "f64"),
            MirType::Boolean => write!(f, "i1"),
            MirType::Char => write!(f, "i8"),
            MirType::String => write!(f, "i8*"),
            MirType::Pointer(inner) => write!(f, "{}*", inner),
            MirType::Null => write!(f, "null"),
            MirType::Nullable(ty) => write!(f, "{}?", ty),
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