use tahuc_ast::{nodes::op::{AssignmentOp, BinaryOp, UnaryOp}, ty::{PrimitiveType, Type}};
use tahuc_span::FileId;

pub type FunctionId = u32;
pub type VariableId = u32;

#[derive(Debug, Clone)]
pub struct HirModule {
    pub file_id: FileId,
    pub functions: Vec<HirFunction>,
    pub extern_functions: Vec<HirExternFunction>,
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
    },
    Variable {
        id: VariableId,
        ty: Type,
    },
    Ternary {
        condition: Box<HirExpression>,
        then_branch: Box<HirExpression>,
        else_branch: Box<HirExpression>,
    },
    Binary {
        left: Box<HirExpression>,
        op: BinaryOp,
        right: Box<HirExpression>,
        ty: Type,
    },
    Unary {
        op: UnaryOp,
        operand: Box<HirExpression>,
        ty: Type,
    },
    ArrayLiteral {
        elements: Vec<HirExpression>,
        ty: Type,
    },
    ArrayAccess {
        array: Box<HirExpression>,
        index: Box<HirExpression>,
        ty: Type,
    },
    MemberAccess {
        object: Box<HirExpression>,
        member: String,
        ty: Type,
    },
    Call {
        callee: FunctionId,
        arguments: Vec<HirExpression>,
        signature: FunctionSignature,
        ty: Type,
    },
    MethodCall {
        object: Box<HirExpression>,
        // method: 
        arguments: Vec<HirExpression>,
        ty: Type,
    },
    Grouping {
        value: Box<HirExpression>,
        ty: Type,
    },
    Cast {
        value: Box<HirExpression>,
        from: Type,
        ty: Type,
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
            HirExpression::MemberAccess { ty, .. } => ty.clone(),
            HirExpression::MethodCall { ty, .. } => ty.clone(),
            HirExpression::Call { ty, .. } => ty.clone(),
            HirExpression::Cast { ty, .. } => ty.clone(),
            HirExpression::Grouping { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum HirLValue {
    Variable(VariableId),
    ArrayAccess {
        array: Box<HirExpression>,
        index: Box<HirExpression>,
    },
    Deref(HirExpression),
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