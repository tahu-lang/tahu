use std::rc::Rc;

use tahuc_ast::ty::{PrimitiveType, Type};
use tahuc_lexer::token::TokenKind;

use crate::{error::ParserError, Parser};

impl<'a> Parser<'a> {
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParserError> {
        match &self.current_token().kind {
            TokenKind::Identifier => {
                let span = self.advance().span;
                // let type_name = self.advance().lexeme.clone();
                let type_name = self.source.span_content(span).unwrap_or("");
                let name = self.pool(type_name);
                Ok(Type::Named(name))
            }
            TokenKind::I8 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I8))
            }
            TokenKind::I16 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I16))
            }
            TokenKind::I32 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I32))
            }
            TokenKind::I64 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I64))
            }
            TokenKind::Isize => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Isize))
            }
            TokenKind::U8 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U8))
            }
            TokenKind::U16 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U16))
            }
            TokenKind::U32 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U32))
            }
            TokenKind::U64 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U64))
            }
            TokenKind::Usize => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Usize))
            }

            // floating point
            TokenKind::F32 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::F32))
            }
            TokenKind::F64 => Ok(Type::Primitive(PrimitiveType::F64)),

            TokenKind::Bool => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Bool))
            }
            TokenKind::Char => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Char))
            }
            TokenKind::Unit => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Unit))
            }

            TokenKind::String => {
                self.advance();
                Ok(Type::String)
            }
            TokenKind::Integer => {
                self.advance();
                Ok(Type::Int)
            }
            TokenKind::Double => {
                self.advance();
                Ok(Type::Double)
            }

            TokenKind::Mul => {
                self.advance();
                Ok(Type::Pointer(Rc::new(self.parse_type()?)))
            }
            TokenKind::LeftBracket => {
                self.advance();
                let ty = self.parse_type()?;
                let size = if self.check(TokenKind::Semicolon) {
                    self.advance();
                    let size = self.parse_size_array();
                    let size = self.get_or_default(size, 0);
                    self.advance();
                    size
                } else {
                    0
                };
                let right_bracket = self
                    .consume(TokenKind::RightBracket, "Expected ']' after type")
                    .cloned();
                self.report_error(right_bracket);
                Ok(Type::Array {
                    ty: Rc::new(ty),
                    size: size as usize,
                })
            }
            _ => {
                let span = self.peek().span;
                let found = self.source.span_content(span).unwrap_or("").to_string();
                Err(ParserError::Expected {
                    expected: format!("type name"),
                    found,
                    span: span,
                })
            }
        }
    }

    fn parse_size_array(&mut self) -> Result<i64, ParserError> {
        let span = self.peek().span;
        let found = self.source.span_content(span).unwrap_or("").to_string();
        match &self.peek().kind {
            TokenKind::Literal(lit) => match lit {
                tahuc_lexer::token::Literal::Integer(i) => Ok(*i),
                _ => {
                    Err(ParserError::Expected {
                        expected: format!("integer literal for array size"),
                        found: found,
                        span: span,
                    })
                }
            },
            _ => {
                Err(ParserError::Expected {
                   expected: format!("Expected integer literal for array size"),
                   found: found,
                   span: span,
               })
            }
        }
    }
}