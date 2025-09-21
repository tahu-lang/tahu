use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_span::Span;

use crate::{error::LexerError, token::{Literal, TemplatePart, Token, TokenKind}, Lexer};

impl<'a> Lexer<'a> {
    pub(crate) fn read_string_or_template(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        self.advance();

        let mut content = String::new();
        let mut has_interpolation = false;

        while let Some(ch) = self.current_char {
            match ch {
                '"' => {
                    self.advance();
                    break;
                }
                '{' => {
                    has_interpolation = true;
                    content.push(ch);
                    self.advance();
                }
                '\\' => {
                    // PERBAIKAN: Proses escape sequence di sini juga
                    self.advance();
                    if let Some(escaped) = self.current_char {
                        match escaped {
                            'n' => content.push('\n'),
                            'r' => content.push('\r'),
                            't' => content.push('\t'),
                            '\\' => content.push('\\'),
                            '"' => content.push('"'),
                            '\'' => content.push('\''),
                            '0' => content.push('\0'),
                            '{' => content.push('{'), // Escape brace
                            '}' => content.push('}'), // Escape brace
                            _ => {
                                content.push('\\');
                                content.push(escaped);
                            }
                        }
                        self.advance();
                    } else {
                        content.push('\\');
                    }
                }
                _ => {
                    content.push(ch);
                    self.advance();
                }
            }
        }

        if self.current_char.is_none() {
            let end_pos = self.position;
            let span = Span::new(start_pos, end_pos, self.file_id);
            return Err(LexerError::UnterminatedString { span });
        }

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        let lexeme = content.clone();

        if has_interpolation {
            match self.parse_template_string(&content) {
                Ok(parts) => Ok(Token::new(
                    TokenKind::TemplateString { parts },
                    span,
                    self.file_id,
                    lexeme,
                )),
                Err(err) => Err(err),
            }
        } else {
            Ok(Token::new(
                TokenKind::Literal(Literal::String(content)),
                span,
                self.file_id,
                lexeme,
            ))
        }
    }

    fn parse_template_string(&mut self, content: &str) -> Result<Vec<TemplatePart>, LexerError> {
        let mut parts = Vec::new();
        let mut chars = content.chars().peekable();
        let mut current_text = String::new();
        
        while let Some(ch) = chars.next() {
            match ch {
                '{' => {
                    if chars.peek() == Some(&'{') {
                        // Escaped brace {{
                        chars.next();
                        current_text.push('{'); 
                    } else {
                        // Start of interpolation
                        if !current_text.is_empty() {
                            parts.push(TemplatePart::Text(current_text.clone()));
                            current_text.clear();
                        }
                        
                        let expr = self.extract_interpolation_expression(&mut chars)?;
                        parts.push(TemplatePart::Expression {
                            tokens: self.tokenize_template_expression(&expr)?,
                            source: expr,
                        });
                    }
                }
                '}' => {
                    if chars.peek() == Some(&'}') {
                        // Escaped brace }}
                        chars.next();
                        current_text.push('}'); 
                    } else {
                        current_text.push(ch);
                    }
                }
                _ => current_text.push(ch),
            }
        }
        
        if !current_text.is_empty() {
            parts.push(TemplatePart::Text(current_text));
        }
        
        Ok(parts)
    }

    fn process_escape_sequence(&mut self) -> Result<char, LexerError> {
        if let Some(escaped) = self.current_char {
            match escaped {
                'n' => Ok('\n'),
                'r' => Ok('\r'),
                't' => Ok('\t'),
                '\\' => Ok('\\'),
                '"' => Ok('"'),
                '\'' => Ok('\''),
                '0' => Ok('\0'),
                'u' => {
                    // Unicode escape \u{XXXX}
                    self.advance();
                    if self.current_char == Some('{') {
                        self.advance();
                        self.read_unicode_escape()
                    } else {
                        Err(LexerError::InvalidUnicodeEscape)
                    }
                }
                _ => Err(LexerError::InvalidEscapeSequence(escaped)),
            }
        } else {
            Err(LexerError::UnexpectedEof)
        }
    }
    
    pub(crate) fn read_char_literal(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        self.advance(); // skip opening '

        let ch = match self.current_char {
            Some('\\') => {
                self.advance(); // skip '\'
                let res = self.process_escape_sequence();
                match res {
                    Ok(ch) => {
                        self.advance();
                        ch
                    },
                    Err(err) => return Err(err),
                }
            }
            Some(ch) => {
                let c = ch;
                self.advance();
                c
            }
            None => {
                let span = Span::new(start_pos, self.position, self.file_id);
                return Err(LexerError::UnterminatedChar { span });
            }
        };

        // Cek apakah ada karakter tambahan sebelum penutup '
        if let Some(next_ch) = self.current_char {
            if next_ch != '\'' {
                let span = Span::new(start_pos, self.position, self.file_id);
                return Err(LexerError::TooManyCharsInCharLiteral { span });
            }
        } else {
            let span = Span::new(start_pos, self.position, self.file_id);
            return Err(LexerError::UnterminatedChar { span });
        }

        self.advance(); // skip closing '

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);

        Ok(Token::new(
            TokenKind::Literal(Literal::Char(ch)),
            span,
            self.file_id,
            ch.to_string(),
        ))
    }
    
    fn read_unicode_escape(&mut self) -> Result<char, LexerError> {
        let mut hex_digits = String::new();
        
        while hex_digits.len() < 6 {
            match self.current_char {
                Some('}') => break,
                Some(ch) if ch.is_ascii_hexdigit() => {
                    hex_digits.push(ch);
                    self.advance();
                }
                Some(_) => return Err(LexerError::InvalidUnicodeEscape),
                None => return Err(LexerError::UnexpectedEof),
            }
        }
        
        if self.current_char != Some('}') {
            return Err(LexerError::InvalidUnicodeEscape);
        }
        self.advance();
        
        if hex_digits.is_empty() {
            return Err(LexerError::InvalidUnicodeEscape);
        }
        
        let code_point = u32::from_str_radix(&hex_digits, 16)
            .map_err(|_| LexerError::InvalidUnicodeEscape)?;
        
        char::from_u32(code_point).ok_or(LexerError::InvalidUnicodeEscape)
    }
    
    fn tokenize_template_expression(&mut self, expr: &str) -> Result<Vec<Token>, LexerError> {
        let mut temp_reporter = DiagnosticReporter::new();
        let mut sub_lexer = Lexer::new(expr.trim().to_string(), self.file_id, &mut temp_reporter);
        let result = sub_lexer.tokenize();
        
        if temp_reporter.has_errors() {
            return Err(LexerError::InvalidTemplateExpression);
        }
        
        let tokens = result.tokens
            .into_iter()
            .filter(|token| !matches!(token.kind, TokenKind::Eof))
            .collect();
            
        Ok(tokens)
    }
    
    fn extract_interpolation_expression(&mut self, chars: &mut std::iter::Peekable<std::str::Chars>) -> Result<String, LexerError> {
        let mut expression = String::new();
        let mut brace_count = 1;
        
        while let Some(ch) = chars.next() {
            match ch {
                '{' => {
                    brace_count += 1;
                    expression.push(ch);
                }
                '}' => {
                    brace_count -= 1;
                    if brace_count == 0 {
                        break;
                    }
                    expression.push(ch);
                }
                _ => expression.push(ch),
            }
        }
        
        if brace_count > 0 {
            Err(LexerError::UnbalancedBraces)
        } else {
            Ok(expression)
        }
    }
}