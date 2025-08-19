use tahuc_span::{FileId, Position, Span};

use crate::{
    error::LexerError,
    token::{Token, TokenKind},
};

mod error;
pub mod token;

#[derive(Debug)]
pub struct Lexer {
    chars: Vec<char>,
    current_char: Option<char>,
    position: Position,
    file_id: FileId,
}

#[derive(Debug, Clone)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub has_errors: bool,
}

impl Lexer {
    pub fn new(input: String, file_id: FileId) -> Self {
        let mut lexer = Self {
            chars: input.chars().collect(),
            current_char: None,
            position: Position::new(1, 1, 0),
            file_id,
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
                    println!("Error: {:?}", error);
                    has_errors = true;
                    let start_pos = self.position;
                    let span = Span::point(start_pos, self.file_id);

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

    fn log_current_char(&mut self) {
        println!("Current char: {:?}", self.current_char.unwrap());
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        // self.log_current_char();
        self.skip_whitespace();

        let start_pos = self.position;

        match self.current_char {
            None => Ok(self.make_token(TokenKind::Eof, start_pos, "")),

            Some('\n') | Some('\r') => {
                self.advance();
                Ok(self.make_token(TokenKind::Newline, start_pos, "\n"))
            }

            Some(ch) if ch.is_ascii_digit() => self.read_number(),
            Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => self.read_identifier(),

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
            Some(':') => {
                self.advance();
                Ok(self.make_token(TokenKind::Colon, start_pos, ":"))
            }
            Some('=') => {
                self.advance();
                Ok(self.make_token(TokenKind::Assign, start_pos, "="))
            }

            Some(ch) => Err(LexerError::UnexpectedCharacter(ch)),
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

    fn read_number(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        let mut lexeme = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                lexeme.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);

        Ok(self.make_token_with_span(
            TokenKind::Literal(token::Literal::Integer(lexeme.parse().unwrap())),
            span,
            &lexeme,
        ))
    }

    fn make_token_with_span(&mut self, kind: TokenKind, pos: Span, lexeme: &str) -> Token {
        Token::new(kind, pos, self.file_id, lexeme.to_string())
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
