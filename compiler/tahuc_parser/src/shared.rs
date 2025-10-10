use std::rc::Rc;

use tahuc_ast::{
    nodes::{
        declarations::{Function, Visibility},
        statements::Variable,
    }, ty::{PrimitiveType, Type},
};
use tahuc_lexer::token::{Token, TokenKind};

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn function_declaration(
        &mut self,
        visibility: Visibility,
    ) -> Result<Function, ParserError> {
        let start_span = self.current_token().span;
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

        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span, end_span);
        let function = self
            .builder
            .build_function(span, self.file_id, visibility, Vec::new(), name, parameters, ret_type, body);
        Ok(function)
    }

    pub(crate) fn variable_declaration(
        &mut self,
        visibility: Visibility,
    ) -> Result<Variable, ParserError> {
        let start_span = self.current_token().span;
        let is_mutable = self.check(TokenKind::Var);
        self.advance();
        let result_name = self.consume_owned(TokenKind::Identifier, "variable name");

        let default = self.get_or_default(result_name, self.dummy_token());
        let name = self.get_token(default).lexeme;


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
                Type::Nullable(Rc::new(ty))
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
        

        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span, end_span);

        let variable = Variable {
            visibility,
            name,
            variable_type: Rc::new(type_annotation),
            initializer,
            is_mutable,
            span,
        };

        Ok(variable)
    }
}
