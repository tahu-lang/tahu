use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    I8, I16, I32, I64, Isize,
    U8, U16, U32, U64, Usize,
    F32, F64,
    Bool,
    /// Unicode char
    Char,
    /// ()
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Primitive(PrimitiveType),
    
    // High level alias
    Int,    // alias -> Primitive(i32)
    Double, // alias -> Primitive(f64)
    String,

    Null,   // literal null depends on context

    Array {
        ty: Box<Type>,
        size: usize
    },
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Nullable(Box<Type>),
    Pointer(Box<Type>),
    
    Function(Vec<Type>, Box<Type>),
    
    // for custom type
    Named(String),
    
    // placeholder
    Inferred,
    // for error
    Error,

    /// for parser only
    Dummy,
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::Isize => write!(f, "isize"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::Usize => write!(f, "usize"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::Unit => write!(f, "()"),
        }
    }
}

impl Type {
    pub fn is_inferred(&self) -> bool {
        matches!(self, Type::Inferred)
    }

    pub fn is_nullable(&self) -> bool {
        match self {
            Type::Nullable(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Type::Primitive(PrimitiveType::Bool) => true,
            _ => false,
        }
    }

    pub fn is_named(&self) -> bool {
        match self {
            Type::Named(_) => true,
            _ => false,
        }
    }

    pub fn get_ty_named(&self) -> Option<String> {
        match self {
            Type::Named(name) => Some(name.clone()),
            _ => None
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct { .. } => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "{}", ty),

            Type::Int => write!(f, "Int"),
            Type::Double => write!(f, "Double"),
            Type::String => write!(f, "String"),
            Type::Null => write!(f, "Null"),
            Type::Array { ty, size } => write!(f, "[{} x {}]", size, ty),
            Type::Struct { name, fields } => {
                write!(f, "struct {} {{", name)?;
                for (name, ty) in fields {
                    write!(f, "{}: {}, ", name, ty)?;
                }
                write!(f, "}}")
            }
            Type::Nullable(ty) => write!(f, "{}?", ty),
            Type::Pointer(type_) => write!(f, "*{}", type_),
            Type::Function(params, type_) => write!(
                f,
                "({}) => {}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                type_
            ),
            Type::Named(name) => write!(f, "{}", name),
            Type::Inferred => write!(f, "Inferred"),
            Type::Error => write!(f, "Error"),
            Type::Dummy => write!(f, "Dummy"),
        }
    }
}

impl PrimitiveType {
    pub fn is_integer(&self) -> bool {
        match self {
            PrimitiveType::I8
            | PrimitiveType::I16
            | PrimitiveType::I32
            | PrimitiveType::I64
            | PrimitiveType::Isize
            | PrimitiveType::U8
            | PrimitiveType::U16
            | PrimitiveType::U32
            | PrimitiveType::U64
            | PrimitiveType::Usize => true,
            _ => false
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            PrimitiveType::F32 | PrimitiveType::F64 => true,
            _ => false
        }
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn can_assign(&self, other: &PrimitiveType) -> bool {
        match (self, other) {
            // exact same
            (a, b) if a == b => true,

            // integer widening
            (PrimitiveType::I16, PrimitiveType::I8)
            | (PrimitiveType::I32, PrimitiveType::I8)
            | (PrimitiveType::I32, PrimitiveType::I16)
            | (PrimitiveType::I64, PrimitiveType::I8)
            | (PrimitiveType::I64, PrimitiveType::I16)
            | (PrimitiveType::I64, PrimitiveType::I32)
            | (PrimitiveType::Isize, PrimitiveType::I8)
            | (PrimitiveType::Isize, PrimitiveType::I16)
            | (PrimitiveType::Isize, PrimitiveType::I32)
            | (PrimitiveType::Isize, PrimitiveType::I64) => true,

            // unsigned widening
            (PrimitiveType::U16, PrimitiveType::U8)
            | (PrimitiveType::U32, PrimitiveType::U8)
            | (PrimitiveType::U32, PrimitiveType::U16)
            | (PrimitiveType::U64, PrimitiveType::U8)
            | (PrimitiveType::U64, PrimitiveType::U16)
            | (PrimitiveType::U64, PrimitiveType::U32)
            | (PrimitiveType::Usize, PrimitiveType::U8)
            | (PrimitiveType::Usize, PrimitiveType::U16)
            | (PrimitiveType::Usize, PrimitiveType::U32)
            | (PrimitiveType::Usize, PrimitiveType::U64) => true,

            // float widening
            (PrimitiveType::F64, PrimitiveType::F32) => true,

            _ => false,
        }
    }

    pub fn can_compare(&self, other: &PrimitiveType) -> bool {
        match (self, other) {
            (PrimitiveType::Bool, PrimitiveType::Bool)
            | (PrimitiveType::Char, PrimitiveType::Char) => true,

            // all integer type can compareable
            (a, b) if a.is_integer() && b.is_integer() => true,
            (a, b) if a.is_float() && b.is_float() => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn is_integer(&self) -> bool {
        match self {
            Type::Primitive(p) => p.is_integer(),
            Type::Int => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Primitive(p) => p.is_float(),
            Type::Double => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Primitive(p) => p.is_numeric(),
            Type::Int => true,
            Type::Double => true,
            _ => false,
        }
    }

    pub fn can_assign(&self, other: &Type) -> bool {
        match (self, other) {
            // exact same
            (a, b) if a == b => true,

            // primitive types
            (Type::Primitive(a), Type::Primitive(b)) => a.can_assign(b),

            // alias types
            (Type::Int, Type::Primitive(PrimitiveType::I32)) => true,
            (Type::Double, Type::Primitive(PrimitiveType::F64)) => true,
            (Type::Primitive(PrimitiveType::I32), Type::Int) => true,
            (Type::Primitive(PrimitiveType::F64), Type::Double) => true,

            // nullable types
            (Type::Nullable(a), Type::Nullable(b)) => a.can_assign(b),
            (Type::Nullable(a), b) => a.can_assign(b), // nullable bisa dikasih value non-null
            (a, Type::Nullable(b)) => a.can_assign(b), // non-null bisa assign ke nullable

            // pointer types
            (Type::Pointer(a), Type::Pointer(b)) => a.can_assign(b),

            // array types
            (Type::Array { ty: a_ty, size: a_size }, Type::Array { ty: b_ty, size: b_size }) =>
                a_size == b_size && a_ty.can_assign(b_ty),

            // null literal
            (_, Type::Null) => matches!(self, Type::Nullable(_) | Type::Pointer(_)),

            // string
            (Type::String, Type::String) => true,

            _ => false,
        }
    }

    pub fn can_compare(&self, other: &Type) -> bool {
        match (self, other) {
            // primitive and aliases
            (Type::Primitive(a), Type::Primitive(b)) => a.can_compare(b),
            
            // alias comparison
            (Type::Primitive(PrimitiveType::I8), Type::Int) => true,
            (Type::Int, Type::Primitive(PrimitiveType::I8)) => true,

            (a, b) if a.is_integer() && b.is_integer() => true,
            (a, b) if a.is_float() && b.is_float() => true,

            (Type::Int, Type::Primitive(PrimitiveType::I32))
            | (Type::Primitive(PrimitiveType::I32), Type::Int)
            | (Type::Double, Type::Primitive(PrimitiveType::F64))
            | (Type::Primitive(PrimitiveType::F64), Type::Double) => true,

            // strings
            (Type::String, Type::String) => true,

            // nullable can compare with inner type
            (Type::Nullable(a), b) => a.can_compare(b),
            (a, Type::Nullable(b)) => a.can_compare(b),

            _ => false,
        }
    }
}