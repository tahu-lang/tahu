use tahuc_span::{Position, Span};

use crate::{error::LexerError, token::{Literal, Token, TokenKind}, Lexer};

impl<'a> Lexer<'a> {
    pub(crate) fn read_number(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        let mut number_str = String::new();
        let mut is_float = false;
        
        // Handle different number formats (binary, octal, hex)
        if self.current_char == Some('0') {
            number_str.push('0');
            self.advance();
            
            match self.current_char {
                Some('b') | Some('B') => return self.read_binary_literal(start_pos),
                Some('o') | Some('O') => return self.read_octal_literal(start_pos),
                Some('x') | Some('X') => return self.read_hex_literal(start_pos),
                _ => {} // Continue with regular number parsing
            }
        }
        
        // Read integer part
        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                number_str.push(ch);
                self.advance();
            } else if ch == '.' && !is_float {
                // Check if it's a decimal point or range operator
                if matches!(self.peek_next_char(), Some(ch) if ch.is_ascii_digit()) {
                    is_float = true;
                    number_str.push(ch);
                    self.advance();
                } else {
                    break; // It's a range operator, stop parsing number
                }
            } else {
                break;
            }
        }
        
        // Read fractional part if it's a float
        if is_float {
            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    number_str.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        
        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        
        if is_float {
            match number_str.parse::<f64>() {
                Ok(value) => Ok(Token::new(
                    TokenKind::Literal(Literal::Double(value)),
                    span,
                    self.file_id,
                    number_str,
                )),
                Err(_) => Err(LexerError::InvalidFloatFormat),
            }
        } else {
            match number_str.parse::<i64>() {
                Ok(value) => Ok(Token::new(
                    TokenKind::Literal(Literal::Integer(value)),
                    span,
                    self.file_id,
                    number_str,
                )),
                Err(_) => Err(LexerError::IntegerOverflow),
            }
        }
    }
    
    fn read_binary_literal(&mut self, start_pos: Position) -> Result<Token, LexerError> {
        self.advance(); // Skip 'b' or 'B'
        let mut binary_str = String::from("0b");
        
        while let Some(ch) = self.current_char {
            if ch == '0' || ch == '1' {
                binary_str.push(ch);
                self.advance();
            } else if ch.is_ascii_digit() {
                return Err(LexerError::InvalidBinaryLiteral);
            } else {
                break;
            }
        }
        
        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        
        // Convert binary to decimal
        let decimal_part = &binary_str[2..]; // Remove "0b" prefix
        if decimal_part.is_empty() {
            return Err(LexerError::InvalidBinaryLiteral);
        }
        
        match i64::from_str_radix(decimal_part, 2) {
            Ok(value) => Ok(Token::new(
                TokenKind::Literal(Literal::Integer(value)),
                span,
                self.file_id,
                binary_str,
            )),
            Err(_) => Err(LexerError::InvalidBinaryLiteral),
        }
    }
    
    fn read_octal_literal(&mut self, start_pos: Position) -> Result<Token, LexerError> {
        self.advance(); // Skip 'o' or 'O'
        let mut octal_str = String::from("0o");
        
        while let Some(ch) = self.current_char {
            if ('0'..='7').contains(&ch) {
                octal_str.push(ch);
                self.advance();
            } else if ch.is_ascii_digit() {
                return Err(LexerError::InvalidOctalLiteral);
            } else {
                break;
            }
        }
        
        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        
        let decimal_part = &octal_str[2..]; // Remove "0o" prefix
        if decimal_part.is_empty() {
            return Err(LexerError::InvalidOctalLiteral);
        }
        
        match i64::from_str_radix(decimal_part, 8) {
            Ok(value) => Ok(Token::new(
                TokenKind::Literal(Literal::Integer(value)),
                span,
                self.file_id,
                octal_str,
            )),
            Err(_) => Err(LexerError::InvalidOctalLiteral),
        }
    }
    
    fn read_hex_literal(&mut self, start_pos: Position) -> Result<Token, LexerError> {
        self.advance(); // Skip 'x' or 'X'
        let mut hex_str = String::from("0x");
        
        while let Some(ch) = self.current_char {
            if ch.is_ascii_hexdigit() {
                hex_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        
        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        
        let decimal_part = &hex_str[2..]; // Remove "0x" prefix
        if decimal_part.is_empty() {
            return Err(LexerError::InvalidHexLiteral);
        }
        
        match i64::from_str_radix(decimal_part, 16) {
            Ok(value) => Ok(Token::new(
                TokenKind::Literal(Literal::Integer(value)),
                span,
                self.file_id,
                hex_str,
            )),
            Err(_) => Err(LexerError::InvalidHexLiteral),
        }
    }
}