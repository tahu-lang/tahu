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
                    content.push(ch);
                    self.advance();
                    if let Some(escaped) = self.current_char {
                        content.push(escaped);
                        self.advance();
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
            return Err(LexerError::UnterminatedString { span: span });
        }

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        let lexeme = format!("\"{}\"", content);

        if has_interpolation {
        }

        if has_interpolation {
            match self.parse_template_string(&content) {
                Ok(parts) => Ok(Token::new(
                    TokenKind::TemplateString { 
                        parts,
                    },
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
                        parts.push(TemplatePart::EscapedBrace('{'));
                    } else {
                        // Start of interpolation
                        if !current_text.is_empty() {
                            parts.push(TemplatePart::Text(current_text.clone()));
                            current_text.clear();
                        }
                        
                        let expr = self.extract_interpolation_expression(&mut chars)?;
                        parts.push(TemplatePart::Expression {
                            // tokens: Vec::new(), // TODO: Tokenize the expression
                            tokens: self.tokenize_template_expression(&expr)?,
                            source: expr,
                        });
                    }
                }
                '}' => {
                    if chars.peek() == Some(&'}') {
                        // Escaped brace }}
                        chars.next();
                        parts.push(TemplatePart::EscapedBrace('}'));
                    } else {
                        // Regular character
                        current_text.push(ch);
                    }
                }
                '\\' => {
                    if let Some(escaped) = chars.next() {
                        match escaped {
                            'n' => current_text.push('\n'),
                            'r' => current_text.push('\r'),
                            't' => current_text.push('\t'),
                            '\\' => current_text.push('\\'),
                            '"' => current_text.push('"'),
                            '\'' => current_text.push('\''),
                            '0' => current_text.push('\0'),
                            _ => return Err(LexerError::InvalidEscapeSequence(escaped)),
                        }
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
                        
    pub(crate) fn read_single_quote_string(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        self.advance(); // Skip opening quote
        
        let mut content = String::new();
        // let mut char_count = 0;
        
        while let Some(ch) = self.current_char {
            match ch {
                '\'' => {
                    self.advance();
                    break;
                }
                '\\' => {
                    self.advance();
                    if let Some(escaped) = self.current_char {
                        match escaped {
                            'n' => content.push('\n'),
                            'r' => content.push('\r'),
                            't' => content.push('\t'),
                            '\\' => content.push('\\'),
                            '\'' => content.push('\''),
                            '"' => content.push('"'),
                            '0' => content.push('\0'),
                            'u' => {
                                // Unicode escape \u{XXXX}
                                self.advance();
                                if self.current_char == Some('{') {
                                    self.advance();
                                    let unicode_char = self.read_unicode_escape()?;
                                    content.push(unicode_char);
                                } else {
                                    return Err(LexerError::InvalidUnicodeEscape);
                                }
                            }
                            _ => return Err(LexerError::InvalidEscapeSequence(escaped)),
                        }
                        self.advance();
                    }
                }
                '\n' | '\r' => {
                    let span = Span::new(start_pos, self.position, self.file_id);
                    return Err(LexerError::UnterminatedString { span });
                }
                _ => {
                    content.push(ch);
                    // char_count += 1;
                    self.advance();
                }
            }
        }
        
        if self.current_char.is_none() {
            let span = Span::new(start_pos, self.position, self.file_id);
            return Err(LexerError::UnterminatedString { span });
        }
        
        // Single quotes typically used for single characters in many languages
        // But we'll allow strings of any length
        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, self.file_id);
        let lexeme = format!("'{}'", content);
        
        Ok(Token::new(
            TokenKind::Literal(Literal::String(content)),
            span,
            self.file_id,
            lexeme,
        ))
    }
    
    fn read_unicode_escape(&mut self) -> Result<char, LexerError> {
        let mut hex_digits = String::new();
        
        // Read up to 6 hex digits
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
        self.advance(); // Skip '}'
        
        if hex_digits.is_empty() {
            return Err(LexerError::InvalidUnicodeEscape);
        }
        
        let code_point = u32::from_str_radix(&hex_digits, 16)
            .map_err(|_| LexerError::InvalidUnicodeEscape)?;
        
        char::from_u32(code_point).ok_or(LexerError::InvalidUnicodeEscape)
    }
    
    fn tokenize_template_expression(&mut self, expr: &str) -> Result<Vec<Token>, LexerError> {
        // Create a temporary diagnostic reporter for the sub-lexer
        let mut temp_reporter = DiagnosticReporter::new();
        
        // Create sub-lexer for the expression
        let mut sub_lexer = Lexer::new(expr.trim().to_string(), self.file_id, &mut temp_reporter);
        let result = sub_lexer.tokenize();
        
        // If there are errors in the expression, report them
        if temp_reporter.has_errors() {
            // We could merge the errors, but for now just return a general error
            return Err(LexerError::InvalidTemplateExpression);
        }
        
        // Filter out EOF token
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