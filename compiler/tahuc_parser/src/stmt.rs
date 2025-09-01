use tahuc_ast::nodes::{declarations::Visibility, statements::Statement};
use tahuc_lexer::token::TokenKind;

use crate::{error::ParserError, Parser};

impl<'a> Parser<'a> {
    pub(crate) fn statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token().kind.clone() {
            TokenKind::Val | TokenKind::Var => {
                let variable = self.variable_declaration(Visibility::Private)?;
                Ok(self.builder.variable_declaration_stmt(variable.span, self.file_id, variable))
            },
            TokenKind::Return => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        let expr = self.expression()?;

        self.consume(TokenKind::Semicolon, "Expected ';' after expression.")?;

        let end_token = self.current_token().clone();
        let span = self.make_span(start_token, end_token);

        Ok(self.builder.expression_statement(span, self.file_id, expr))
    }

    fn return_statement(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        self.advance();
        let value = if !self.check(TokenKind::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

         self.consume(TokenKind::Semicolon, "Expected ';' after return value.")?;

        let end_token = self.current_token().clone();
        let span = self.make_span(start_token, end_token);

        Ok(self.builder.return_statement(span, self.file_id, value))
    }
    // fn if_statement(&mut self) {}
}