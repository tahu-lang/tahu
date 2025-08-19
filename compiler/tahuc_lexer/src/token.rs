use tahuc_span::{FileId, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Literal(Literal),

    // Keyword
    Fn,
    Var,
    Val,

    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,

    /// Assign
    /// =
    Assign, // =

    /// Delimiters
    /// (
    LeftParen,
    /// )
    RightParen,
    /// {
    LeftBrace,
    /// }
    RightBrace,
    /// :
    Colon,

    Identifier,

    // Line
    Newline, // \n
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

impl TokenKind {
    pub fn make_from_identifer(value: String) -> TokenKind {
        match value.as_str() {
            "fn" => TokenKind::Fn,
            "var" => TokenKind::Var,
            "val" => TokenKind::Val,
            _ => TokenKind::Identifier,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub file_id: FileId,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, file_id: FileId, lexeme: String) -> Self {
        Self {
            kind,
            span,
            file_id,
            lexeme,
        }
    }
}
