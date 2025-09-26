use tahuc_ast::{nodes::op::{AssignmentOp, BinaryOp, UnaryOp}, ty::{PrimitiveType, Type}};
use tahuc_span::{FileId, Span};

pub type StructId = u32;
pub type FunctionId = u32;
pub type VariableId = u32;

#[derive(Debug, Clone)]
pub struct HirModule {
    pub file_id: FileId,
    pub structs: Vec<HirStruct>,
    pub functions: Vec<HirFunction>,
    pub extern_functions: Vec<HirExternFunction>,
}

#[derive(Debug, Clone)]
pub struct HirStruct {
    pub id: StructId,
    pub name: String,
    pub fields: Vec<HirField>,
    pub visibility: HirVisibility,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct HirField {
    pub id: VariableId,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    pub id: FunctionId,
    pub name: String,
    pub parameters: Vec<HirParameters>,
    pub return_type: Type,
    pub visibility: HirVisibility,
    pub body: HirBlock,
}

#[derive(Debug, Clone)]
pub enum HirVisibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct HirExternFunction {
    pub id: FunctionId,
    pub name: String,
    pub parameters: Vec<HirParameters>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct HirParameters {
    pub id: VariableId,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct HirVariable {
    pub id: VariableId,
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBlock {
    pub statements: Vec<HirStatement>,
}

#[derive(Debug, Clone)]
pub enum HirStatement {
    Expression(HirExpression),
    Variable {
        variable: HirVariable,
        initializer: Option<HirExpression>
    },
    Assignment {
        target: HirLValue,
        op: AssignmentOp,
        value: HirExpression,
    },
    Return {
        value: Option<HirExpression>,
    },
    If(HirIfStatement),
    While {
        condition: HirExpression,
        body: Box<HirBlock>,
    },
    Continue,
    Break,
}

#[derive(Debug, Clone)]
pub struct HirIfStatement {
    pub condition: HirExpression,
    pub then_branch: HirBlock,
    pub else_branch: Option<HirElseBranch>,
}

#[derive(Debug, Clone)]
pub enum HirElseBranch {
    Block(HirBlock),
    If(Box<HirIfStatement>)
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub args: Vec<Type>,
}

#[derive(Debug, Clone)]
pub enum HirExpression {
    Literal {
        value: HirLiteral,
        ty: Type,
        span: Span,
    },
    Variable {
        id: VariableId,
        ty: Type,
        span: Span,
    },
    Ternary {
        condition: Box<HirExpression>,
        then_branch: Box<HirExpression>,
        else_branch: Box<HirExpression>,
        span: Span,
    },
    Binary {
        left: Box<HirExpression>,
        op: BinaryOp,
        right: Box<HirExpression>,
        ty: Type,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        operand: Box<HirExpression>,
        ty: Type,
        span: Span,
    },
    ArrayLiteral {
        elements: Vec<HirExpression>,
        ty: Type,
        span: Span,
    },
    ArrayAccess {
        array: Box<HirExpression>,
        index: Box<HirExpression>,
        arr_ty: Type,
        ty: Type,
        span: Span,
    },
    StructLiteral {
        object: Box<HirExpression>,
        fields: Vec<(usize, HirExpression)>,
        ty: Type,
        span: Span,
    },
    StructType {
        id: StructId,
        ty: Type,
        span: Span,
    },
    FieldAccess {
        object: Box<HirExpression>,
        field_name: String,
        field_id: u32,
        base_ty: Type,
        ty: Type,
        span: Span,
    },
    MemberAccess {
        object: Box<HirExpression>,
        member: String,
        ty: Type,
        span: Span,
    },
    Call {
        callee: FunctionId,
        arguments: Vec<HirExpression>,
        signature: FunctionSignature,
        ty: Type,
        span: Span,
    },
    MethodCall {
        object: Box<HirExpression>,
        // method: 
        arguments: Vec<HirExpression>,
        ty: Type,
        span: Span,
    },
    Grouping {
        value: Box<HirExpression>,
        ty: Type,
        span: Span,
    },
    Cast {
        value: Box<HirExpression>,
        from: Type,
        ty: Type,
        span: Span,
    },
}

impl HirExpression {
    pub fn as_variable_id(&self) -> Option<VariableId> {
        match self {
            HirExpression::Variable { id, .. } => Some(*id),
            _ => None,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            HirExpression::Literal { ty, .. } => ty.clone(),
            HirExpression::Variable { ty, .. } => ty.clone(),
            HirExpression::Ternary { then_branch, .. } => then_branch.get_type(),
            HirExpression::Binary { ty, .. } => ty.clone(),
            HirExpression::Unary { ty, .. } => ty.clone(),
            HirExpression::ArrayLiteral { ty, .. } => ty.clone(),
            HirExpression::ArrayAccess { ty, .. } => ty.clone(),
            HirExpression::StructType { ty, .. } => ty.clone(),
            HirExpression::StructLiteral { ty, .. } => ty.clone(),
            HirExpression::FieldAccess { ty, .. } => ty.clone(),
            HirExpression::MemberAccess { ty, .. } => ty.clone(),
            HirExpression::MethodCall { ty, .. } => ty.clone(),
            HirExpression::Call { ty, .. } => ty.clone(),
            HirExpression::Cast { ty, .. } => ty.clone(),
            HirExpression::Grouping { ty, .. } => ty.clone(),
        }
    }

    pub fn to_l_value(&self) -> Option<HirLValue> {
        match self {
            HirExpression::Variable { id, ty, span } => {
                Some(HirLValue::Variable {
                    id: *id,
                    ty: ty.clone(),
                    span: span.clone(),
                })
            } 
            HirExpression::ArrayAccess { array, index, arr_ty, ty, span } => {
                Some(HirLValue::ArrayAccess {
                    array: array.clone(),
                    index: index.clone(),
                    arr_ty: arr_ty.clone(),
                    ty: ty.clone(),
                    span: span.clone(),
                })
            }
            HirExpression::FieldAccess { object, field_name, field_id, base_ty, ty, span } => {
                Some(HirLValue::FieldAccess {
                    object: object.clone(),
                    field_name: field_name.clone(),
                    field_id: *field_id,
                    base_ty: base_ty.clone(),
                    ty: ty.clone(),
                    span: span.clone(),
                })
            }
            _ => None,
                
        }
    }
}

#[derive(Debug, Clone)]
pub enum HirLValue {
    Variable {
        id: VariableId,
        ty: Type,
        span: Span,
    },
    ArrayAccess {
        array: Box<HirExpression>,
        index: Box<HirExpression>,
        arr_ty: Type,
        ty: Type,
        span: Span,
    },
    Deref {
        value: HirExpression,
        ty: Type,
        span: Span,
    },
    FieldAccess {
        object: Box<HirExpression>,
        field_name: String,
        field_id: u32,
        base_ty: Type,
        ty: Type,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum HirLiteral {
    String(String),
    Char(char),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Null(Type),
}

impl HirLiteral {
    pub fn get_type(&self) -> Type {
        match self {
            HirLiteral::String(_) => Type::String,
            HirLiteral::Char(_) => Type::Primitive(PrimitiveType::Char),
            HirLiteral::Integer(_) => Type::Int,
            HirLiteral::Float(_) => Type::Double,
            HirLiteral::Bool(_) => Type::Primitive(PrimitiveType::Bool),
            HirLiteral::Null(ty) => ty.clone(),
        }
    }
}