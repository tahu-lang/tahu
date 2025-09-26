use tahuc_ast::nodes::{
    declarations::Visibility,
    op::AssignmentOp,
    statements::{ElseBranch, IfStatement, Statement, StatementKind, WhileStatement},
};
use tahuc_lexer::token::TokenKind;

use crate::{Parser, error::ParserError};

impl<'a> Parser<'a> {
    pub(crate) fn statement(&mut self) -> Result<Statement, ParserError> {
        self.switch_statement_context();
        match self.current_token().kind.clone() {
            TokenKind::Val | TokenKind::Var => {
                let variable = self.variable_declaration(Visibility::Private)?;
                Ok(self
                    .builder
                    .variable_declaration_stmt(variable.span, self.file_id, variable))
            }
            TokenKind::Return => self.return_statement(),
            TokenKind::If => self.if_statement(),
            TokenKind::While => self.while_statement(),
            // TokenKind::For => self.for_statement(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Break => self.parse_break(),
            _ => {
                self.log_current_token();
                Ok(self.expression_statement()?)
            }
        }
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        let lhs = self.expression()?;

        if self.is_assignment_operator() {
            let op = self.parse_assignment_operator()?;
            self.advance();
            let rhs = self.expression()?;

            let semi_colon = self.consume(TokenKind::Semicolon, "Expected ';' after assigment.").cloned();
            self.report_error(semi_colon);

            let end_token = self.current_token().clone();
            let span = self.make_span(start_token, end_token);

            Ok(self
                .builder
                .build_assigment_statement(span, self.file_id, lhs, op, rhs))
        } else {
            let semi_colon = self.consume(TokenKind::Semicolon, "Expected ';' after expression.").cloned();
            self.report_error(semi_colon);
            let end_token = self.current_token().clone();
            let span = self.make_span(start_token, end_token);
            Ok(self.builder.expression_statement(span, self.file_id, lhs))
        }
    }

    fn parse_assignment_operator(&mut self) -> Result<AssignmentOp, ParserError> {
        match self.current_token().kind.clone() {
            TokenKind::Assign => Ok(AssignmentOp::Assign),
            // Arithmetic compound assignments
            TokenKind::AddAssign => Ok(AssignmentOp::AddAssign),
            TokenKind::SubAssign => Ok(AssignmentOp::SubAssign),
            TokenKind::MulAssign => Ok(AssignmentOp::MulAssign),
            TokenKind::DivAssign => Ok(AssignmentOp::DivAssign),
            TokenKind::RemAssign => Ok(AssignmentOp::RemAssign),
            // Bitwise compound assignments
            TokenKind::AndAssign => Ok(AssignmentOp::BitAndAssign),
            TokenKind::OrAssign => Ok(AssignmentOp::BitOrAssign),
            TokenKind::XorAssign => Ok(AssignmentOp::BitXorAssign),
            // Shift compound assignments
            TokenKind::ShlAssign => Ok(AssignmentOp::ShlAssign),
            TokenKind::ShrAssign => Ok(AssignmentOp::ShrAssign),
            _ => Err(ParserError::Unexpected {
                unexcepted: "Invalid assignment operator".to_string(),
                span: self.current_token().span,
            }),
        }
    }

    fn is_assignment_operator(&mut self) -> bool {
        match self.current_token().kind.clone() {
            TokenKind::Assign
            | TokenKind::AddAssign
            | TokenKind::SubAssign
            | TokenKind::MulAssign
            | TokenKind::DivAssign
            | TokenKind::RemAssign
            | TokenKind::AndAssign
            | TokenKind::OrAssign
            | TokenKind::XorAssign
            | TokenKind::ShlAssign
            | TokenKind::ShrAssign => true,
            _ => false,
        }
    }

    fn return_statement(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        self.advance();
        let value = if !self.check(TokenKind::Semicolon) {
            self.switch_expression_context();
            Some(self.expression()?)
        } else {
            None
        };

        let semi_colon = self.consume(TokenKind::Semicolon, "Expected ';' after return value.").cloned();
        self.report_error(semi_colon);

        let end_token = self.current_token().clone();
        let span = self.make_span(start_token, end_token);

        Ok(self.builder.return_statement(span, self.file_id, value))
    }

    fn if_statement(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        self.consume(TokenKind::If, "Expected 'if' keyword.")?;

        let condition = self.expression()?;
        let then_block = self.parse_block()?;

        let else_branch = if self.check(TokenKind::Else) {
            self.advance();

            if self.check(TokenKind::If) {
                // else if
                let else_if_block = self.if_statement()?;
                if let Statement {
                    kind: StatementKind::IfStatement(if_stmt),
                    ..
                } = else_if_block
                {
                    Some(ElseBranch::If(Box::new(if_stmt)))
                } else {
                    self.synchronize();
                    return Err(ParserError::Unexpected {
                        unexcepted: "Failed to parse else if".to_string(),
                        span: self.current_token().span,
                    });
                }
            } else {
                let else_block = self.parse_block()?;
                Some(ElseBranch::Block(else_block))
            }
        } else {
            None
        };

        let end_span = if let Some(else_branch) = &else_branch {
            match else_branch {
                ElseBranch::Block(block) => block.span,
                ElseBranch::If(if_stmt) => if_stmt.span,
            }
        } else {
            then_block.span
        };

        let span = self.make_span_fspan(start_token.span, end_span);

        let if_stmt = IfStatement {
            condition,
            else_branch: else_branch,
            span,
            then_branch: then_block,
        };

        Ok(self.builder.if_statement(span, self.file_id, if_stmt))
    }

    fn while_statement(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        self.consume(TokenKind::While, "Expected 'while' keyword.")?;

        let condition = self.expression()?;
        let body = self.parse_block()?;

        let end_token = self.current_token().clone();
        let span = self.make_span(start_token, end_token);

        let while_stmt = WhileStatement {
            condition: condition,
            body: body,
            span: span,
        };

        Ok(self.builder.while_statement(span, self.file_id, while_stmt))
    }

    // fn for_statement(&mut self) {}

    fn parse_continue(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        self.advance();
        let semi_colon = self.consume(TokenKind::Semicolon, "Expected ';' after continue.").cloned();
        self.report_error(semi_colon);

        let end_token = self.current_token().clone();
        let span = self.make_span(start_token, end_token);

        Ok(self.builder.build_continue_statement(span, self.file_id))
    }

    fn parse_break(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.current_token().clone();
        self.advance();
        let semi_colon = self.consume(TokenKind::Semicolon, "Expected ';' after break.").cloned();
        self.report_error(semi_colon);

        let end_token = self.current_token().clone();
        let span = self.make_span(start_token, end_token);

        Ok(self.builder.build_break_statement(span, self.file_id))
    }
}
