use tahuc_ast::nodes::{
    declarations::{Class, ClassBody, Declaration, Function, Visibility},
    statements::Variable,
};
use tahuc_lexer::token::TokenKind;

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn class_declaration(
        &mut self,
        visibility: Visibility,
    ) -> Result<Declaration, ParserError> {
        let start_token = self.peek().clone();
        self.advance();
        let name = self
            .consume(TokenKind::Identifier, "Expected class name")?
            .lexeme
            .clone();

        // TODO: implement extend and implement

        self.consume(TokenKind::LeftBrace, "Expected '{' before class body")?;

        let mut fields: Vec<Variable> = vec![];
        let mut methods: Vec<Function> = vec![];

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let visibility = if self.check(TokenKind::Pub) {
                self.advance();
                Visibility::Public
            } else if self.check(TokenKind::Priv) {
                self.advance();
                Visibility::Private
            } else if self.check(TokenKind::Prot) {
                self.advance();
                Visibility::Protected
            } else {
                Visibility::Private
            };

            if self.check(TokenKind::Val) {
                fields.push(self.variable_declaration(visibility.clone())?);
                if self.check(TokenKind::Semicolon) { self.advance(); }
            } else if self.check(TokenKind::Var) {
                fields.push(self.variable_declaration(visibility.clone())?);
                if self.check(TokenKind::Semicolon) { self.advance(); }
            } else if self.check(TokenKind::Fn) {
                methods.push(self.function_declaration(visibility.clone())?);
            } else {
                let found = self.peek().lexeme.clone();
                return Err(ParserError::Expected {
                    expected: "Expected variable or function declaration".to_string(),
                    found: format!("But found {found}"),
                    span: self.peek().span.clone(),
                });
            }
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after class body")?;

        let end_token = self.previous().clone();
        let span = self.make_span(start_token, end_token);

        let class = Class {
            visibility: visibility,
            name: name,
            super_class: None,
            interfaces: vec![],
            is_abstract: false,
            body: ClassBody {
                fields: fields,
                methods: methods,
            },
            span,
        };

        Ok(self.builder.class_declaration(span, self.file_id, class))
    }
}
