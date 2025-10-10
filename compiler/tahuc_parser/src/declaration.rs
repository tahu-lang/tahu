use std::rc::Rc;

use tahuc_ast::{nodes::declarations::{Declaration, Import, ImportTable, Visibility}, ty::{PrimitiveType, Type}};
use tahuc_lexer::token::{Token, TokenKind};

use crate::{error::ParserError, Parser};

impl<'a> Parser<'a> {
    pub(crate) fn declaration(&mut self, visibility: Visibility) -> Result<Declaration, ParserError> {
        match self.peek().kind {
            TokenKind::Import => self.parse_import(),
            TokenKind::At => self.parse_decorator(&mut Vec::new()),
            TokenKind::Fn => {
                let func = self.function_declaration(visibility)?;
                Ok(self.builder.fn_declaration(func.span, self.file_id, func))
            }
            TokenKind::Var | TokenKind::Val => {
                let variable = self.variable_declaration(visibility)?;
                Ok(self
                    .builder
                    .variable_declaration(variable.span, self.file_id, variable))
            }
            TokenKind::Extern => self.parse_extern(),
            TokenKind::Pub => {
                self.advance();
                self.declaration(Visibility::Public)
            }
            TokenKind::Struct => self.parse_struct(visibility),
            _ => {
                let token = self.peek().clone();
                // self.synchronize();
                Err(ParserError::UnexpectedTopLevel {
                    found: token.lexeme,
                    span: token.span,
                })
            }
        }
    }

    pub(crate) fn parse_import(&mut self) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().span;
        self.advance();

        let mut is_wildcard = false;
        let mut table = Vec::new();

        if self.check(TokenKind::Mul) {
            self.advance();
            is_wildcard = true;
        } else {
            let right_brace = self.consume_owned(TokenKind::LeftBrace, "'{' for import");
            self.report_error(right_brace);

            while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
                let ident =
                    self.consume_owned(TokenKind::Identifier, "identifier for import");
                let name_id = self.get_or_default(ident, self.dummy_token());

                let alias = if self.check(TokenKind::As) {
                    self.advance();
                    let ident = self
                        .consume(TokenKind::Identifier, "identifier for import")
                        .cloned();
                    Some(self.get_or_default(ident, Token::dummy()).lexeme)
                } else {
                    None
                };

                let end_span = self.get_last_span();

                let name = self.tokens[name_id].clone();
                let span = self.make_span_fspan(name.span, end_span);

                table.push(self.builder.build_import_table(
                    span,
                    self.file_id,
                    ImportTable {
                        name: name.lexeme.clone(),
                        alias,
                    },
                ));

                if self.check(TokenKind::Comma) {
                    self.advance();
                    if self.check(TokenKind::RightBrace) {
                        break;
                    }
                } else if !self.check(TokenKind::RightBrace) {
                    let current = self.current_token().clone();
                    let found = self.source.span_content(current.span).unwrap_or("");
                    let err = ParserError::Expected {
                        expected: format!("`,` or `}}`"),
                        found: found.to_string(),
                        span: current.span,
                    };
                    self.add_error(err);
                    self.synchronize();
                }
            }
        };

        if !is_wildcard {
            let rbrace = self
                .consume(TokenKind::RightBrace, "'}' for import")
                .cloned();
            self.report_error(rbrace);
        }

        let from_kw = self
            .consume(TokenKind::From, "from keyword after import")
            .cloned();
        self.report_error(from_kw);

        // let result_path = self.consume(TokenKind::Literal(tahuc_lexer::token::Literal::String(s)), "").cloned();
        let current_token = self.peek().clone();

        let path = match &current_token.kind {
            TokenKind::Literal(tahuc_lexer::token::Literal::String(s)) => s,
            _ => {
                let current_token = self.current_token().clone();
                let found = self.source.span_content(current_token.span).unwrap_or("");
                return Err(ParserError::Expected {
                    expected: format!("path"),
                    found: found.to_string(),
                    span: current_token.span,
                });
            }
        };

        self.advance();

        let semi = self
            .consume(TokenKind::Semicolon, "';' after import path")
            .cloned();
        self.report_error(semi);

        let end_span = self.get_last_span();
        // let span = span.merge(end_span);
        let span = self.make_span_fspan(start_span, end_span);
        Ok(self.builder.build_import(
            span,
            self.file_id,
            Import {
                imports: table,
                is_wildcard,
                path: path.clone(),
                span,
            },
        ))
    }

    /// for now just simple extern
    /// extern fn name(params) return_type
    pub(crate) fn parse_extern(&mut self) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().clone();
        self.advance();

        self.consume(TokenKind::Fn, "'fn' keyword")?;

        let result_name = self
            .consume_without_current_token(TokenKind::Identifier, "`name` for extern function")
            .cloned();
        let name = self.get_or_default(result_name, Token::dummy()).clone();

        let left_paren = self
            .consume(TokenKind::LeftParen, "'(' after function name")
            .cloned();
        self.report_error(left_paren);

        let mut parameters = Vec::new();
        if !self.check(TokenKind::RightParen) {
            while !self.check(TokenKind::RightParen) && !self.is_at_end() {
                let parameter = self.parse_parameters(true)?;
                parameters.push(parameter);
                if !self.check(TokenKind::Comma) {
                    break;
                }
                self.advance();
            }
        }

        let right_paren = self
            .consume(TokenKind::RightParen, "')' after parameters")
            .cloned();
        self.report_error(right_paren);

        let candidates = vec![TokenKind::Extern, TokenKind::Fn, TokenKind::Var, TokenKind::Val];

        let return_type = if self.check(TokenKind::Semicolon) {
            Type::Primitive(PrimitiveType::Unit)
        } else {
            if candidates.contains(&self.peek().kind) {
                Type::Primitive(PrimitiveType::Unit)
            } else {
                self.parse_type()?
            }
        };

        let semi_colon = self
            .consume_without_current_token(
                TokenKind::Semicolon,
                "';' after extern function declaration",
            )
            .cloned();
        self.report_error(semi_colon);

        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span.span, end_span);

        Ok(self.builder.build_extern_function(
            span,
            self.file_id,
            name.lexeme,
            parameters,
            return_type,
        ))
    }

    fn parse_struct(&mut self, visibility: Visibility) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().span;
        self.advance();

        let name = self
            .consume_without_current_token(TokenKind::Identifier, "struct name")
            .cloned();
        let name = self.get_or_default(name, Token::dummy());

        let mut fields = Vec::new();
        let mut fields_ty = Vec::new();

        let left_brace = self
            .consume(TokenKind::LeftBrace, "'{' after struct name")
            .cloned();
        self.report_error(left_brace);

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let field_start_span = self.current_token().span;
            let field_name = self
                .consume(TokenKind::Identifier, "field name")?
                .lexeme
                .clone();
            self.consume(TokenKind::Colon, "':' after field name")?;
            let r#type = self.parse_type()?;

            if !self.check(TokenKind::RightBrace) {
                self.consume(TokenKind::Comma, "',' after field declaration")?;
            }

            let field_end_span = self.get_last_span();
            let field_span = self.make_span_fspan(field_start_span, field_end_span);

            fields_ty.push((field_name.clone(), Rc::new(r#type.clone())));
            fields.push(self.builder.build_struct_field(
                field_span,
                self.file_id,
                field_name,
                r#type,
            ));
        }

        let right_brace = self
            .consume(TokenKind::RightBrace, "'}' after struct fields")
            .cloned();
        self.report_error(right_brace);
        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span, end_span);

        let ty = Type::Struct {
            name: name.lexeme.clone(),
            fields: Rc::new(fields_ty),
        };

        Ok(self.builder.build_struct_declaration(
            span,
            self.file_id,
            visibility,
            name.lexeme,
            fields,
            ty,
        ))
    }
}