use tahuc_ast::nodes::{
    ast::AstNode, expressions::{Argument, ExpressionKind, StructLiteralField, TemplatePart}, op::{BinaryOp, UnaryOp}, Expression
};
use tahuc_lexer::token::TokenKind;

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn expression(&mut self) -> Result<Expression, ParserError> {
        self.expression_ternary()
    }

    fn expression_ternary(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_binary_expression(0)?;

        // Handle ternary operator: condition ? true_expr : false_expr
        if self.check(TokenKind::Question) {
            self.advance();
            let true_expr = self.expression_ternary()?; // Right-associative
            let colon = self.consume(TokenKind::Colon, "Expected ':' after ternary true expression").cloned();
            self.report_error(colon);
            let false_expr = self.expression_ternary()?; // Right-associative
            let span = self.make_span_fspan(expr.span, false_expr.span);
            expr = self.builder.ternary(span, self.file_id, expr, true_expr, false_expr);
        }

        Ok(expr)
    }

    /// parse expression binary
    /// +, -,
    fn parse_binary_expression(&mut self, min_prec: u8) -> Result<Expression, ParserError> {
        let mut left = self.expression_unary()?;

        while !self.is_at_end() && !self.is_expression_terminator() {
            let (op, prec) = match self.current_token().kind {
                TokenKind::Or => (BinaryOp::Or, 0),
                TokenKind::And => (BinaryOp::And, 1),

                TokenKind::BitOr => (BinaryOp::BitOr, 2),
                TokenKind::BitXor => (BinaryOp::BitXor, 2),
                TokenKind::BitAnd => (BinaryOp::BitAnd, 2),

                TokenKind::Eq => (BinaryOp::Eq, 3),
                TokenKind::Ne => (BinaryOp::Ne, 3),

                TokenKind::Lt => (BinaryOp::Lt, 4),
                TokenKind::Le => (BinaryOp::Le, 4),
                TokenKind::Gt => (BinaryOp::Gt, 4),
                TokenKind::Ge => (BinaryOp::Ge, 4),

                TokenKind::Shl => (BinaryOp::Shl, 5),
                TokenKind::Shr => (BinaryOp::Shr, 5),

                TokenKind::Add => (BinaryOp::Add, 6),
                TokenKind::Sub => (BinaryOp::Sub, 6),

                TokenKind::Mul => (BinaryOp::Mul, 7),
                TokenKind::Div => (BinaryOp::Div, 7),
                TokenKind::Rem => (BinaryOp::Rem, 7),
                _ => break,
            };

            if prec < min_prec {
                break;
            }

            let left_span = left.span;
            self.advance();
            let right = self.parse_binary_expression(prec + 1)?;

            let span = self.make_span_fspan(left_span, right.span);
            left = self
                .builder
                .binary(span, self.file_id, left.into(), op, right);
        }

        Ok(left)
    }

    fn expression_unary(&mut self) -> Result<Expression, ParserError> {
        // Check for unary operators
        match self.current_token().kind {
            TokenKind::BitAnd => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?; // address-of is right-associative
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self.builder.unary(span, self.file_id, UnaryOp::AddressOf, expr))
            }
            TokenKind::Mul => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?; // dereference is right-associative
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self.builder.unary(span, self.file_id, UnaryOp::Deref, expr))
            }
            // Prefix unary operators
            TokenKind::Sub => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?; // Right-associative, recurse
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self.builder.unary(span, self.file_id, UnaryOp::Minus, expr))
            }
            TokenKind::Add => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?;
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self.builder.unary(span, self.file_id, UnaryOp::Plus, expr))
            }
            TokenKind::Not => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?;
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self.builder.unary(span, self.file_id, UnaryOp::Not, expr))
            }
            TokenKind::BitNot => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?;
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self
                    .builder
                    .unary(span, self.file_id, UnaryOp::BitNot, expr))
            }
            TokenKind::Inc => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?;
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self
                    .builder
                    .unary(span, self.file_id, UnaryOp::PreIncrement, expr))
            }
            TokenKind::Dec => {
                let op_token = self.advance().clone();
                let expr = self.expression_unary()?;
                let span = self.make_span_fspan(op_token.span, expr.span);
                Ok(self
                    .builder
                    .unary(span, self.file_id, UnaryOp::PreDecrement, expr))
            }
            _ => self.expression_postfix(),
        }
    }

    fn expression_postfix(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.expression_primary()?;

        loop {
            match self.current_token().kind {
                TokenKind::Inc => {
                    let op_token = self.advance().clone();
                    let span = self.make_span_fspan(expr.span, op_token.span);
                    expr = self
                        .builder
                        .unary(span, self.file_id, UnaryOp::PostIncrement, expr);
                }
                TokenKind::Dec => {
                    let op_token = self.advance().clone();
                    let span = self.make_span_fspan(expr.span, op_token.span);
                    expr = self
                        .builder
                        .unary(span, self.file_id, UnaryOp::PostDecrement, expr);
                }
                TokenKind::LeftBracket => {
                    self.advance();
                    let index = self.expression()?;
                    let right_bracket = self.consume(TokenKind::RightBracket, "Expected ']' after array index").cloned();
                    self.report_error(right_bracket);
                    let end_token = self.get_last_span();
                    let span = self.make_span_fspan(expr.span, end_token);
                    expr = self.builder.array_access(span, self.file_id, expr, index);
                }
                TokenKind::Dot => {
                    self.advance();
                    if !self.check(TokenKind::Identifier) {
                        let prev = self.previous().clone();
                        let token = self.current_token().clone();
                        let span = self.make_span(prev, token.clone());
                        self.synchronize();
                        return Err(ParserError::Unexpected {
                            unexcepted: format!("'{}' - expected identifier", token.lexeme),
                            span: span,
                        });
                    }
                    let field_token = self.advance().clone();
                    let field = field_token.lexeme;
                    let span = self.make_span_fspan(expr.span, field_token.span);
                    expr = self.builder.member_access(span, self.file_id, expr, field);
                }
                TokenKind::As => {
                    self.advance();
                    let type_expr = self.parse_type()?;
                    let span = self.get_last_span();
                    let span = self.make_span_fspan(expr.span, span);
                    expr = self.builder.cast(span, self.file_id, type_expr, expr);
                }
                TokenKind::LeftParen => {
                    self.advance();
                    let mut arguments = Vec::new();
                    if !self.check(TokenKind::RightParen) {
                        arguments = self.parse_arguments()?;
                    }
                    let right_parent = self.consume(TokenKind::RightParen, "Expected ')' after arguments").cloned();
                    self.report_error(right_parent);
                    let span = self.get_last_span();
                    let span = self.make_span_fspan(expr.span, span);
                    expr = self.builder.function_call(span, self.file_id, expr, arguments);
                }
                TokenKind::LeftBrace => {
                    match &expr.kind {
                        ExpressionKind::Identifier(_) => {
                            if self.context.is_statement() {
                                break;
                            }

                            let span = self.get_last_span();
                            self.advance();
                            let fields = self.parse_struct_fields()?;

                            let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' after struct fields").cloned();
                            self.report_error(right_brace);
                            let end_span = self.get_last_span();
                            let span = self.make_span_fspan(span, end_span);
                            expr = self.builder.build_struct_literal(span, self.file_id, expr, fields);
                        }
                        _ => {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<AstNode<StructLiteralField>>, ParserError> {
        let mut fields = Vec::new();
        
        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
            // Parse field name
            let start_span = self.current_token().span.clone();
            let field_name_token = self.current_token().clone();
            let ident = self.consume(TokenKind::Identifier, "Expected field name").cloned();
            self.report_error(ident);
            let field_name = field_name_token.lexeme.clone();

            let name = self.builder.identifier(
                field_name_token.span,
                self.file_id,
                field_name.clone()
            );

            let value = if self.check(TokenKind::Colon) {
                // Named field: name: "John"
                self.advance(); // consume ':'
                Some(self.expression()?)
            } else {
                None
            };

            let end_span = self.get_last_span();
            let span = self.make_span_fspan(start_span, end_span);
            fields.push(self.builder.build_struct_literal_field(span, self.file_id, name, value));
            
            
            // Handle comma separation
            if self.check(TokenKind::Comma) {
                self.advance();
                // Allow trailing comma
                if self.check(TokenKind::RightBrace) {
                    break;
                }
            } else if !self.check(TokenKind::RightBrace) {
                return Err(ParserError::Expected {
                    expected: "expected ',' or '}'".to_string(),
                    found: self.current_token().lexeme.clone(),
                    span: self.current_token().span,
                });
            }
        }
        
        Ok(fields)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Argument>, ParserError> {
        let mut arguments = Vec::new();
        let mut seen_named = false;

        if self.check(TokenKind::RightParen) {
            return Ok(arguments);
        }

        loop {
            let arg = self.parse_single_argument(&mut seen_named)?;
            arguments.push(arg);

            if !self.check(TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        Ok(arguments)
    }

    fn parse_single_argument(&mut self, seen_named: &mut bool) -> Result<Argument, ParserError> {
        // Check if this might be a named argument
        // Look ahead for pattern: IDENTIFIER = EXPRESSION
        if self.check(TokenKind::Identifier) && self.check_ahead(1, TokenKind::Assign) {
            *seen_named = true;

            let name_token = self.advance().clone();
            let name: String = match &name_token.kind {
                TokenKind::Identifier => name_token.lexeme.to_string(),
                _ => unreachable!(),
            };

            self.consume(TokenKind::Assign, "Expected '=' after parameter name")?;
            let value = self.expression()?;
            let span = self.make_span_fspan(name_token.span, value.span);

            Ok(Argument::Named { name, value, span })
        } else {
            let expr = self.expression()?;
            Ok(Argument::Positional(expr))
        }
    }

    fn expression_primary(&mut self) -> Result<Expression, ParserError> {
        match self.current_token().kind.clone() {
            TokenKind::Literal(literal) => {
                let token = self.advance().clone();
                let file_id = self.file_id;
                Ok(self.builder.literal(token.span, file_id, literal))
            }
            TokenKind::TemplateString { parts } => {
                let token = self.advance().clone();

                let mut template = Vec::new(); 
                for part in parts {
                    match part {
                        tahuc_lexer::token::TemplatePart::EscapedBrace(c) => template.push(TemplatePart::Text(c.to_string())),
                        tahuc_lexer::token::TemplatePart::Expression { tokens, .. } => {
                            let result = Parser::parse_expression(self.file_id, tokens, self.reporter);
                            match result {
                                Ok(expr) => {
                                    template.push(TemplatePart::Expression(expr));
                                }
                                Err(err) => {
                                    println!("err template string: {:?}", err.to_diagnostic());
                                }
                            }
                        }
                        tahuc_lexer::token::TemplatePart::Text(s) => template.push(TemplatePart::Text(s.to_string())),
                    }
                }

                Ok(self.builder.template_string(token.span, self.file_id, template))
            }
            TokenKind::Identifier => {
                let token = self.advance().clone();
                let file_id = self.file_id;
                Ok(self.builder.identifier(token.span, file_id, token.lexeme))
            }
            TokenKind::Self_ => {
                let token = self.advance().clone();
                let file_id = self.file_id;
                Ok(self.builder.identifier(token.span, file_id, token.lexeme))
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                let right_paren = self.consume(TokenKind::RightParen, "Expected ')' after expression").cloned();
                self.report_error(right_paren);
                Ok(self.builder.grouping(expr.span, self.file_id, expr))
            }
            TokenKind::LeftBracket => {
                let token = self.advance().clone();
                let mut expr = Vec::new();
                while !self.is_at_end() && !self.check(TokenKind::RightBracket) {
                    if self.check(TokenKind::Comma) {
                        self.advance();
                    }
                    expr.push(self.expression()?);
                }
                let right_bracket =  self.consume(TokenKind::RightBracket, "Expected ']' after expression").cloned();
                self.report_error(right_bracket);
                let end_span = self.get_last_span();
                let span = self.make_span_fspan(token.span, end_span);
                Ok(self.builder.build_array_literal(span, self.file_id, expr))
            }
            TokenKind::RightParen => {
                let token = self.current_token().clone();
                self.synchronize();
                Err(ParserError::Unexpected {
                    unexcepted: "')' - missing expression".to_string(),
                    span: token.span,
                })
            }
            TokenKind::Eof => {
                let token = self.current_token().clone();
                self.synchronize();
                Err(ParserError::Unexpected {
                    unexcepted: "end of file - missing expression".to_string(),
                    span: token.span,
                })
            }
            _ => {
                let token = self.current_token().clone();
                Err(ParserError::Unexpected {
                    unexcepted: format!("token '{}' - expected expression", token.lexeme),
                    span: token.span,
                })
            }
        }
    }
}
