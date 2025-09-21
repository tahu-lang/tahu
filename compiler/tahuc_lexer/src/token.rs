use tahuc_span::{FileId, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Literal(Literal),
    TemplateString {
        // "Hello {name}!"
        parts: Vec<TemplatePart>,
    },

    /// Keyword
    Import,
    From,
    Interface,
    Abstract,
    Class,
    Enum,
    Self_,
    Fn,
    Var,
    Val,
    Operator,
    Const,

    // FFI
    Extern,

    // Visibility
    Pub,
    Priv,
    Prot,
    // Control Flow
    If,
    Else,
    Switch,
    Case,
    Default,
    For,
    In,
    While,
    Do,
    Break,
    Continue,
    Return,
    Match,
    // data type
    Char,
    String,
    Integer,
    Double,
    Boolean,
    Void,

    /// Type & Cast
    Is, // is  type check
    NotIs,  // !is negated type check
    As, // as  type cast

    /// Unary
    Not, // !  logical not
    BitNot, // ~  bitwise not
    Inc,    // ++ increment (prefix/postfix)
    Dec,    // -- decrement (prefix/postfix)

    /// Operators
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Rem, // %
    // Comparison
    Eq, // ==
    Lt, // <
    Le, // <=
    Ne, // !=
    Ge, // >=
    Gt, // >
    // Logical
    And, // &&
    Or,  // ||
    // Bitwise
    BitXor, // ^
    BitAnd, // &
    BitOr,  // |
    Shl,    // <<
    Shr,    // >>

    /// Assignment
    Assign, // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    RemAssign, // %=
    AndAssign, // &=
    OrAssign,  // |=
    XorAssign, // ^=
    ShlAssign, // <<=
    ShrAssign, // >>=

    /// Delimiters
    LeftParen, // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Comma,        // ,
    Dot,          // .
    Semicolon,    // ;

    /// Null-safety
    // Null, // null literal
    Question,    // ?   nullable type marker
    QuestionDot, // ?.  safe navigation operator
    At,          // @ annotation / decorator
    Arrow,       // => lambda / arrow function

    Identifier,

    /// Special
    Spread, // ...
    Range,          // ..
    RangeInclusive, // ..=

    // Line
    Newline, // \n
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    Text(String), // Literal text part
    Expression {
        // Interpolated expression
        tokens: Vec<Token>, // Pre-tokenized expression
        source: String,     // Original source for debugging
    },
    EscapedBrace(char), // {{ -> { or }} -> }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
    Integer(i64),
    Double(f64),
    Boolean(bool),
    Null,
}

impl TokenKind {
    pub fn make_from_identifer(value: String) -> TokenKind {
        match value.as_str() {
            // Core
            "import" => TokenKind::Import,
            "from" => TokenKind::From,
            "as" => TokenKind::As,
            "interface" => TokenKind::Interface,
            "abstract" => TokenKind::Abstract,
            "class" => TokenKind::Class,
            "enum" => TokenKind::Enum,
            "self" => TokenKind::Self_,

            // FFI
            "extern" => TokenKind::Extern,

            // control flow
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "while" => TokenKind::While,
            "do" => TokenKind::Do,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "match" => TokenKind::Match,

            // data type
            "string" => TokenKind::String,
            "cahr" => TokenKind::Char,
            "integer" => TokenKind::Integer,
            "double" => TokenKind::Double,
            "boolean" => TokenKind::Boolean,
            "void" => TokenKind::Void,

            // visibility
            "pub" => TokenKind::Pub,
            "priv" => TokenKind::Priv,
            "prot" => TokenKind::Prot,

            // modifier
            "const" => TokenKind::Const,
            "fn" => TokenKind::Fn,
            "var" => TokenKind::Var,
            "val" => TokenKind::Val,
            "is" => TokenKind::Is,

            // literal
            "true" => TokenKind::Literal(Literal::Boolean(true)),
            "false" => TokenKind::Literal(Literal::Boolean(false)),
            "null" => TokenKind::Literal(Literal::Null),
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
