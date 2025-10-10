use tahuc_ast::nodes::{ast::AstNode, declarations::{Declaration, Decorator, DecoratorValue, Visibility}};
use tahuc_lexer::token::TokenKind;

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn parse_decorator(
        &mut self,
        decorators: &mut Vec<AstNode<Decorator>>,
    ) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().span;
        self.advance();

        let dummy = self.dummy_token();
        let result_name_id = self.consume_owned(TokenKind::Identifier, "Expected decorator name");
        let name_id = self.get_or_default(result_name_id, dummy);

        let mut arguments = Vec::new();

        if self.check(TokenKind::LeftParen) {
            self.advance();
            let args = self.parse_decorator_argument();
            arguments.extend(self.get_or_default(args, Vec::new()));

            let right_paren = self.consume_owned(
                TokenKind::RightParen,
                "Expected ')' after decorator parameters",
            );
            self.report_error(right_paren);
        }

        let last_span = self.get_last_span();
        let span = self.make_span_fspan(start_span, last_span);
        let name = self.get_token(name_id);
        let str_id = name.lexeme;

        let decorator = self
            .builder
            .build_decorator(span, self.file_id, str_id, arguments);

        decorators.push(decorator);

        self.parse_declaration_with_decorator(decorators, Visibility::Private)
    }

    fn parse_decorator_argument(&mut self) -> Result<Vec<DecoratorValue>, ParserError> {
        let start_span = self.current_token().span;
        let mut arguments = Vec::new();

        while !self.check(TokenKind::RightParen) && !self.is_at_end() {
            if self.check_ahead(1, TokenKind::Assign) {
                let name_token = self.advance().clone();
                let name = name_token.lexeme;
                let name = self.builder.identifier(name_token.span, self.file_id, name);

                self.consume(TokenKind::Assign, "Expected '=' after parameter name")?;
                let value = self.expression()?;
                let span = self.make_span_fspan(name_token.span, value.span);
                arguments.push(DecoratorValue {
                    name,
                    value: Some(value),
                    span,
                });
            } else {
                let expr = self.expression()?;
                let span = self.make_span_fspan(start_span, expr.span);
                arguments.push(DecoratorValue {
                    name: expr,
                    value: None,
                    span,
                });
            }
        }

        Ok(arguments)
    }

    fn parse_declaration_with_decorator(
        &mut self,
        decorators: &mut Vec<AstNode<Decorator>>,
        visibility: Visibility,
    ) -> Result<Declaration, ParserError> {
        match self.current_token().kind {
            TokenKind::Pub => {
                self.advance();
                self.parse_declaration_with_decorator(decorators, Visibility::Public)
            }
            TokenKind::Fn => {
                let mut func = self.function_declaration(visibility)?;
                let current_decor = decorators.clone();
                func.kind.decorators = current_decor;
                decorators.clear();
                Ok(self.builder.fn_declaration(func.span, self.file_id, func))
            }
            TokenKind::Var | TokenKind::Val => {
                let variable = self.variable_declaration(visibility)?;
                Ok(self
                    .builder
                    .variable_declaration(variable.span, self.file_id, variable))
            }
            TokenKind::At => self.parse_decorator(decorators),
            _ => {
                let token = self.current_token().clone();
                self.synchronize();
                Err(ParserError::Unexpected {
                    unexcepted: token.lexeme,
                    span: token.span,
                })
            }
        }
    }
}
