use tahuc_ast::{
    nodes::{
        declarations::{Function, Visibility},
        statements::Variable,
    }, ty::{ErrorContext, PrimitiveType, Type},
};
use tahuc_lexer::token::{Token, TokenKind};

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn function_declaration(
        &mut self,
        visibility: Visibility,
    ) -> Result<Function, ParserError> {
        let start_token = self.peek().clone();
        self.consume(TokenKind::Fn, "expected 'fn' keyword")?;
        let result_name = self
            .consume(TokenKind::Identifier, "expected function name").cloned();
        let name = self.get_or_default(result_name, Token::dummy()).lexeme;

        self.consume(TokenKind::LeftParen, "expected '(' after function name")?;

        let mut parameters = Vec::new();
        if !self.check(TokenKind::RightParen) && !self.check(TokenKind::LeftBrace) && !self.is_at_end() {
            while !self.check(TokenKind::RightParen) && !self.is_at_end() {
                let parameter = self.parse_parameters(false)?;
                parameters.push(parameter);
                if !self.check(TokenKind::Comma) {
                    break;
                }
                self.advance();
            }
        }

        let res_cons = self.consume(TokenKind::RightParen, "expected ')' after parameters").cloned();
        self.report_error(res_cons);

        // TODO: Handle return type
        // fn main() int {}
        //           ^^^ return type
        let ret_type = if !self.check(TokenKind::LeftBrace) {
            let ty = self.parse_type();
            self.get_or_default(ty, Type::Dummy)
        } else {
            Type::Primitive(PrimitiveType::Unit)
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
        let result_name = self.consume(TokenKind::Identifier, &msg).cloned();
        // ?.lexeme.clone();
        let name = self.get_or_default(result_name, Token::dummy()).lexeme.clone();


        // get colon and check the type of variable
        // 'var a: int' or 'var a'
        let type_annotation = if self.check(TokenKind::Colon) {
            self.advance();
            let ty = self.parse_type();
            let ty = self.get_or_default(ty, Type::Dummy);

            // handle nullable 
            // `string?`, `int?`
            if self.check(TokenKind::Question) {
                self.advance();
                Type::Nullable(Box::new(ty))
            } else {
                ty
            }
        } else {
            Type::Inferred
        };

        let initializer = if self.check(TokenKind::Assign) {
            self.switch_expression_context();
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        let res = self.consume(TokenKind::Semicolon, "Expected ';' after variable declaration").cloned();
        self.report_error(res);
        

        let end_token = self.peek().clone();
        let span = self.make_span(start_token, end_token);

        let variable = Variable {
            visibility,
            name,
            variable_type: type_annotation,
            initializer,
            is_mutable,
            span,
        };

        Ok(variable)
    }
}
