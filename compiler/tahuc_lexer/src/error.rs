#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char),
}