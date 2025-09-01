use tahuc_ast::{
    Type,
    nodes::{
        declarations::{Function, Visibility},
        statements::Variable,
    },
};
use tahuc_lexer::token::TokenKind;

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn function_declaration(
        &mut self,
        visibility: Visibility,
    ) -> Result<Function, ParserError> {
        let start_token = self.peek().clone();
        self.consume(TokenKind::Fn, "expected 'fn' keyword")?;
        let name = self
            .consume(TokenKind::Identifier, "expected function name")?
            .lexeme
            .clone();
        self.consume(TokenKind::LeftParen, "expected '(' after function name")?;

        let mut parameters = Vec::new();
        if !self.check(TokenKind::RightParen) {
            while !self.check(TokenKind::RightParen) && !self.is_at_end() {
                let parameter = self.parse_parameters(false)?;
                parameters.push(parameter);
                if !self.check(TokenKind::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.consume(TokenKind::RightParen, "expected ')' after parameters")?;

        // TODO: Handle return type
        // fn main() int {}
        //           ^^^ return type
        let ret_type = if !self.check(TokenKind::LeftBrace) {
            self.parse_type()?
        } else {
            Type::Void
        };

        // let ret_type = Type::Inferred;

        // for now simple parse block for function
        let body = self.parse_block()?;

        let end_token = self.previous().clone();
        let span = self.make_span(start_token, end_token);
        let function = self
            .builder
            .build_function(span, self.file_id, visibility, name, parameters, ret_type, body);
        Ok(function)
    }

    pub(crate) fn variable_declaration(
        &mut self,
        visibility: Visibility,
    ) -> Result<Variable, ParserError> {
        let start_token = self.peek().clone();
        let is_mutable = self.check(TokenKind::Var);
        let variable = self.advance();
        let msg = format!("Expected variable name after '{}'", variable.lexeme.clone());
        let name = self.consume(TokenKind::Identifier, &msg)?.lexeme.clone();

        // get colon and check the type of variable
        // 'var a: int' or 'var a'
        let type_annotation = if self.check(TokenKind::Colon) {
            self.advance();
            self.parse_type()?
        } else {
            Type::Inferred
        };

        let initializer = if self.check(TokenKind::Assign) {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

         self.consume(TokenKind::Semicolon, "Expected ';' after variable declaration")?;

        let end_token = self.peek().clone();
        let span = self.make_span(start_token, end_token);

        let variable = Variable::new(
            visibility,
            name,
            type_annotation,
            initializer,
            is_mutable,
            span,
        );

        Ok(variable)
    }
}
