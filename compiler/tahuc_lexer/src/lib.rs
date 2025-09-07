use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_span::{FileId, Position, Span};

use crate::{
    error::LexerError,
    token::{Token, TokenKind},
};

mod error;
mod number;
mod string;
pub mod token;

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Vec<char>,
    current_char: Option<char>,
    position: Position,
    file_id: FileId,
    reporter: &'a mut DiagnosticReporter,
}

#[derive(Debug, Clone)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub has_errors: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: String, file_id: FileId, reporter: &'a mut DiagnosticReporter) -> Self {
        let mut lexer = Self {
            chars: input.chars().collect(),
            current_char: None,
            position: Position::new(1, 1, 0),
            file_id,
            reporter,
        };
        lexer.advance();
        lexer
    }

    pub fn tokenize(&mut self) -> LexerResult {
        let mut tokens = Vec::new();
        let mut has_errors = false;

        loop {
            match self.next_token() {
                Ok(token) => {
                    let is_eof = matches!(token.kind, TokenKind::Eof);
                    tokens.push(token);
                    if is_eof {
                        break;
                    }
                }
                Err(error) => {
                    has_errors = true;
                    let start_pos = self.position;
                    let span = Span::point(start_pos, self.file_id);
                    let diagnostic = error.to_diagnostic(span);
                    self.reporter.report(diagnostic);

                    // Skip the problematic character to continue parsing
                    self.advance();
                }
            }
        }

        LexerResult {
            tokens: tokens,
            has_errors: has_errors,
        }
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let start_pos = self.position;

        match self.current_char {
            None => Ok(self.make_token(TokenKind::Eof, start_pos, "")),

            Some('\n') | Some('\r') => {
                self.advance();
                Ok(self.next_token()?)

                // just delete this fucking token shit
                // Ok(self.make_token(TokenKind::Newline, start_pos, "\n"))
            }

            // skip comment
            Some('/') => {
                while let Some(c) = self.current_char {
                    if c == '\n' {
                        return Ok(self.next_token()?);
                    }
                    self.advance();
                }

                self.advance();
                Ok(self.next_token()?)
            }

            // TODO: Implement comment
            Some(ch) if ch.is_ascii_digit() => self.read_number(),
            Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => self.read_identifier(),
            Some('"') => self.read_string_or_template(),
            Some('\'') => self.read_single_quote_string(),

            // Single character delimiters
            Some('(') => {
                self.advance();
                Ok(self.make_token(TokenKind::LeftParen, start_pos, "("))
            }
            Some(')') => {
                self.advance();
                Ok(self.make_token(TokenKind::RightParen, start_pos, "("))
            }
            Some('{') => {
                self.advance();
                Ok(self.make_token(TokenKind::LeftBrace, start_pos, "("))
            }
            Some('}') => {
                self.advance();
                Ok(self.make_token(TokenKind::RightBrace, start_pos, "}"))
            }
            Some('[') => {
                self.advance();
                Ok(self.make_token(TokenKind::LeftBracket, start_pos, "["))
            }
            Some(']') => {
                self.advance();
                Ok(self.make_token(TokenKind::RightBracket, start_pos, "]"))
            }

            Some(':') => {
                self.advance();
                Ok(self.make_token(TokenKind::Colon, start_pos, ":"))
            }
            Some(';') => {
                self.advance();
                Ok(self.make_token(TokenKind::Semicolon, start_pos, ";"))
            }
            Some(',') => {
                self.advance();
                Ok(self.make_token(TokenKind::Comma, start_pos, ","))
            }
            Some('?') => {
                self.advance();
                if self.current_char == Some('.') {
                    self.advance();
                    Ok(self.make_token(TokenKind::QuestionDot, start_pos, "?."))
                } else {
                    Ok(self.make_token(TokenKind::Question, start_pos, "?"))
                }
            }
            Some('@') => {
                self.advance();
                Ok(self.make_token(TokenKind::At, start_pos, "@"))
            }

            Some(ch) => self.operator(ch, start_pos),
        }
    }

    fn operator(&mut self, ch: char, start_pos: Position) -> Result<Token, LexerError> {
        match ch {
            '+' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::AddAssign, start_pos, "+="))
                } else if self.current_char == Some('+') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Inc, start_pos, "++"))
                } else {
                    Ok(self.make_token(TokenKind::Add, start_pos, "+"))
                }
            }
            '-' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::SubAssign, start_pos, "-="))
                } else if self.current_char == Some('-') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Dec, start_pos, "--"))
                } else {
                    Ok(self.make_token(TokenKind::Sub, start_pos, "-"))
                }
            }
            '*' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::MulAssign, start_pos, "*="))
                } else {
                    Ok(self.make_token(TokenKind::Mul, start_pos, "*"))
                }
            }
            '/' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::DivAssign, start_pos, "/="))
                } else {
                    Ok(self.make_token(TokenKind::Div, start_pos, "/"))
                }
            }
            '%' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::RemAssign, start_pos, "%="))
                } else {
                    Ok(self.make_token(TokenKind::Rem, start_pos, "%"))
                }
            }
            '=' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Eq, start_pos, "=="))
                } else if self.current_char == Some('>') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Arrow, start_pos, "=>"))
                } else {
                    Ok(self.make_token(TokenKind::Assign, start_pos, "="))
                }
            }
            '<' => {
                self.advance();
                if self.current_char == Some('<') {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Ok(self.make_token(TokenKind::ShlAssign, start_pos, "<<="))
                    } else {
                        Ok(self.make_token(TokenKind::Shl, start_pos, "<<"))
                    }
                } else if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Le, start_pos, "<="))
                } else {
                    Ok(self.make_token(TokenKind::Lt, start_pos, "<"))
                }
            }
            '>' => {
                self.advance();
                if self.current_char == Some('>') {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Ok(self.make_token(TokenKind::ShrAssign, start_pos, ">>="))
                    } else {
                        Ok(self.make_token(TokenKind::Shr, start_pos, ">>"))
                    }
                } else if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Ge, start_pos, ">="))
                } else {
                    Ok(self.make_token(TokenKind::Gt, start_pos, ">"))
                }
            }
            '!' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Ne, start_pos, "!="))
                } else {
                    Ok(self.make_token(TokenKind::Not, start_pos, "!"))
                }
            }
            '&' => {
                self.advance();
                if self.current_char == Some('&') {
                    self.advance();
                    Ok(self.make_token(TokenKind::And, start_pos, "&&"))
                } else {
                    Ok(self.make_token(TokenKind::BitAnd, start_pos, "&"))
                }
            }
            '|' => {
                self.advance();
                if self.current_char == Some('|') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Or, start_pos, "||"))
                } else {
                    Ok(self.make_token(TokenKind::BitOr, start_pos, "|"))
                }
            }
            '^' => {
                self.advance();
                if self.current_char == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::XorAssign, start_pos, "^="))
                } else {
                    Ok(self.make_token(TokenKind::BitXor, start_pos, "^"))
                }
            }
            '~' => {
                self.advance();
                Ok(self.make_token(TokenKind::BitNot, start_pos, "~"))
            }

            _ => self.special(ch, start_pos),
        }
    }

    fn special(&mut self, ch: char, start_pos: Position) -> Result<Token, LexerError> {
        match ch {
            '.' => {
                self.advance();
                if self.current_char == Some('.') {
                    self.advance();
                    if self.current_char == Some('.') {
                        self.advance();
                        Ok(self.make_token(TokenKind::Spread, start_pos, "..."))
                    } else if self.current_char == Some('=') {
                        Ok(self.make_token(TokenKind::RangeInclusive, start_pos, "..="))
                    } else {
                        Ok(self.make_token(TokenKind::Range, start_pos, ".."))
                    }
                } else {
                    Ok(self.make_token(TokenKind::Dot, start_pos, "."))
                }
            }

            _ => Err(LexerError::UnexpectedCharacter(ch)),
        }
    }

    fn read_identifier(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        let mut lexeme = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                lexeme.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let token = TokenKind::make_from_identifer(lexeme.clone());

        Ok(self.make_token(token, start_pos, &lexeme.to_string()))
    }

    fn make_token(&self, kind: TokenKind, start_pos: Position, lexeme: &str) -> Token {
        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        Token::new(kind, span, self.file_id, lexeme.to_string())
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() && ch != '\n' && ch != '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn advance(&mut self) {
        if let Some(ch) = self.current_char {
            if ch == '\n' {
                self.position.advance_line();
            } else {
                self.position.advance_column();
            }
        }

        self.current_char = self.chars.get(self.position.offset as usize).copied();
    }

    fn peek_next_char(&self) -> Option<char> {
        let next_index = self.position.offset as usize + 1;
        if next_index < self.chars.len() {
            Some(self.chars[next_index])
        } else {
            None
        }
    }
}
