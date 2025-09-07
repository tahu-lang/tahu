use tahuc_ast::{nodes::op::{AssignmentOp, BinaryOp, UnaryOp}, Type};

pub type FunctionId = u32;
pub type VariableId = u32;

#[derive(Debug, Clone)]
pub struct HirModule {
    pub functions: Vec<HirFunction>,
    pub extern_functions: Vec<HirExternFunction>,
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    pub id: FunctionId,
    pub name: String,
    pub parameters: Vec<HirParameters>,
    pub return_type: Type,
    pub body: HirBlock,
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
    // If {
    //     condition: HirExpression,
    //     then_branch: Box<HirBlock>,
    //     else_branch: Option<Box<HirBlock>>,
    // },
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
    Call {
        callee: FunctionId,
        arguments: Vec<HirExpression>,
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
}

#[derive(Debug, Clone)]
pub enum HirLValue {
    Variable(VariableId),
    ArrayAccess {
        array: Box<HirExpression>,
        index: Box<HirExpression>,
    }
}

#[derive(Debug, Clone)]
pub enum HirLiteral {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
}