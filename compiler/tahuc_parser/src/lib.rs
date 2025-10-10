use std::rc::Rc;

use tahuc_ast::{
    AstBuilder, Module,
    nodes::{
        Expression,
        ast::AstNode,
        declarations::{Declaration, Parameter, ParameterKind, Visibility},
        expressions::ExpressionKind,
        statements::Block,
    },
    ty::Type,
};
use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_lexer::{
    LexerResult,
    token::{Token, TokenKind},
};
use tahuc_span::{FileId, SourceFile, Span, StrId, StrPool};

use crate::error::ParserError;

mod declaration;
mod decorator;
mod error;
mod expr;
mod shared;
mod stmt;
mod ty;

#[derive(Debug)]
pub(crate) enum ParserContext {
    TopLevel,
    Statement,
    Expression,
}

impl ParserContext {
    // pub fn is_top_level(&self) -> bool {
    //     matches!(self, ParserContext::TopLevel)
    // }

    pub fn is_statement(&self) -> bool {
        matches!(self, ParserContext::Statement)
    }

    // pub fn is_expression(&self) -> bool {
    //     matches!(self, ParserContext::Expression)
    // }
}

#[derive(Debug)]
pub struct Parser<'a> {
    source: &'a SourceFile,
    str_pool: &'a mut StrPool,
    file_id: FileId,
    builder: AstBuilder,
    tokens: Vec<Token>,
    position: usize,
    parser_errors: Vec<ParserError>,
    pub(crate) context: ParserContext,
    pub(crate) reporter: &'a mut DiagnosticReporter,
}

#[derive(Debug, Clone)]
pub struct ParserResult {
    pub module: Module,
    pub has_errors: bool,
    pub parser_errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(
        source: &'a SourceFile,
        str_pool: &'a mut StrPool,
        lexer: LexerResult,
        reporter: &'a mut DiagnosticReporter,
    ) -> Self {
        Parser::with_builder(
            source,
            str_pool,
            source.id,
            lexer,
            AstBuilder::new(),
            reporter,
        )
    }

    pub fn with_builder(
        source: &'a SourceFile,
        str_pool: &'a mut StrPool,
        file_id: FileId,
        lexer: LexerResult,
        builder: AstBuilder,
        reporter: &'a mut DiagnosticReporter,
    ) -> Self {
        Self {
            source,
            str_pool,
            file_id,
            builder,
            tokens: lexer.tokens,
            position: 0,
            parser_errors: Vec::new(),
            reporter,
            context: ParserContext::TopLevel,
        }
    }

    pub fn parse_expression(
        source: &'a SourceFile,
        str_pool: &'a mut StrPool,
        tokens: Vec<Token>,
        reporter: &'a mut DiagnosticReporter,
    ) -> Result<Expression, ParserError> {
        let mut parser = Parser::new(
            source,
            str_pool,
            LexerResult {
                tokens,
                has_errors: false,
            },
            reporter,
        );
        parser.expression()
    }

    pub fn parse(&mut self) -> ParserResult {
        let mut programs: Vec<Declaration> = Vec::new();

        while !self.is_at_end() {
            if self.check(TokenKind::Newline) {
                self.advance();
                continue;
            }

            match self.declaration(Visibility::Private) {
                Ok(program) => programs.push(program),
                Err(error) => {
                    self.add_error(error);
                    self.synchronize();
                }
            }
        }

        ParserResult {
            module: Module {
                file: self.file_id,
                declaration: programs,
            },
            has_errors: self.parser_errors.len() > 0,
            parser_errors: self.parser_errors.clone(),
        }
    }

    fn parse_parameters(&mut self, is_extern: bool) -> Result<Parameter, ParserError> {
        let start_span = self.current_token().clone();

        let result_name = self.consume_owned(TokenKind::Identifier, "Expected parameter name");
        let dummy = self.dummy_token();
        let name_id = self.get_or_default(result_name, dummy);

        let colon = self.consume_owned(TokenKind::Colon, "Expected ':' after parameter name");
        self.report_error(colon);

        let mut r#type = self.parse_type()?;

        if self.check(TokenKind::Question) {
            self.advance();
            r#type = Type::Nullable(Rc::new(r#type));
        }

        let mut default: Option<AstNode<ExpressionKind>> = None;

        if self.check(TokenKind::Assign) && !is_extern {
            self.advance();
            let value: AstNode<ExpressionKind> = self.expression()?;
            default = Some(value);
        } else {
            if is_extern && self.check(TokenKind::Assign) {
                let unexcepted = self.str_pool.pool("cannot assign value to extern function");
                return Err(ParserError::Unexpected {
                    unexcepted: unexcepted,
                    span: self.get_last_span(),
                });
            }
        }

        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span.span, end_span);

        let name = &self.tokens[name_id];

        Ok(self.builder.build_parameter(
            span,
            self.file_id,
            ParameterKind {
                name: name.clone().lexeme,
                r#type: Rc::new(r#type),
                default: default,
                span: span,
            },
        ))
    }

    fn parse_block(&mut self) -> Result<Block, ParserError> {
        let start_span = self.current_token().span;
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;

        self.skip_newlines();

        let mut statements = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let stmt_res = self.statement();
            match stmt_res {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    let diagnostic = error.to_diagnostic();
                    self.reporter.report(diagnostic);
                    self.synchronize();
                }
            }
            self.skip_newlines();
        }

        let right_brace = self.consume_owned(TokenKind::RightBrace, "Expected '}'");
        self.report_error(right_brace);

        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span, end_span);

        Ok(Block { statements, span })
    }

    fn synchronize(&mut self) {
        self.advance(); // Always advance at least one token to prevent infinite loop

        while !self.is_at_end() {
            let prev_token = self.previous().kind.clone();

            // Stop after statement terminators
            if matches!(prev_token, TokenKind::Semicolon | TokenKind::RightBrace) {
                return;
            }

            // Stop before declaration keywords
            match self.peek().kind {
                TokenKind::Fn | TokenKind::Var | TokenKind::Pub | TokenKind::Eof => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn get_or_default<T>(&mut self, result: Result<T, ParserError>, default: T) -> T {
        match result {
            Ok(value) => value,
            Err(error) => {
                self.add_error(error);
                default
            }
        }
    }

    fn report_error<T>(&mut self, result: Result<T, ParserError>) {
        match result {
            Err(err) => self.add_error(err),
            _ => {}
        }
    }

    fn add_error(&mut self, error: ParserError) {
        let diagnostic = error.to_diagnostic();
        self.parser_errors.push(error);
        self.reporter.report(diagnostic);
    }

    //===== HELPER METHOD =====//
    #[allow(dead_code)]
    fn log_current_token(&mut self) {
        println!("Current Token: {:?}", self.peek());
    }

    fn switch_context(&mut self, new_context: ParserContext) {
        self.context = new_context;
    }

    fn switch_expression_context(&mut self) {
        self.switch_context(ParserContext::Expression);
    }

    fn switch_statement_context(&mut self) {
        self.switch_context(ParserContext::Statement);
    }

    // fn switch_top_level_context(&mut self) {
    //     self.switch_context(ParserContext::TopLevel);
    // }

    fn is_expression_terminator(&mut self) -> bool {
        matches!(
            self.current_token().kind,
            TokenKind::Semicolon | TokenKind::RightBrace | TokenKind::Newline | TokenKind::Eof
        )
    }

    fn skip_newlines(&mut self) {
        while self.current_token().kind == TokenKind::Newline {
            self.advance();
        }
    }

    fn make_span(&mut self, start_token: Token, end_token: Token) -> Span {
        Span::new(start_token.span.start, end_token.span.end, self.file_id)
    }

    fn make_span_fspan(&mut self, start_span: Span, end_span: Span) -> Span {
        Span::new(start_span.start, end_span.end, self.file_id)
    }

    fn get_last_span(&mut self) -> Span {
        self.previous().clone().span
    }

    fn peek(&mut self) -> &Token {
        // self.tokens.get(self.position).unwrap()
        self.tokens
            .get(self.position)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    fn check_ahead(&self, offset: usize, kind: TokenKind) -> bool {
        if self.position + offset < self.tokens.len() {
            if let Some(token) = self.tokens.get(self.position + offset) {
                std::mem::discriminant(&token.kind) == std::mem::discriminant(&kind)
            } else {
                false
            }
        } else {
            false
        }
    }

    fn current_token(&mut self) -> &Token {
        self.peek()
    }

    fn is_at_end(&mut self) -> bool {
        // self.peek().kind == TokenKind::Eof || self.position >= self.tokens.len()
        if self.position >= self.tokens.len() {
            true
        } else {
            self.peek().kind == TokenKind::Eof
        }
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == kind
    }

    fn consume(&mut self, kind: TokenKind, expected: &'a str) -> Result<&Token, ParserError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let span = self.get_last_span();
            // let token = self.peek().clone();
            // let span = span.merge(token.span);
            // self.synchronize();
            // let expected = self.str_pool.pool(expected);
            let found = self.source.span_content(span).unwrap_or("");
            Err(ParserError::Expected {
                expected: expected.to_string(),
                found: found.to_string(),
                span: span,
            })
        }
    }

    fn consume_without_current_token(&mut self, kind: TokenKind, expected: &'a str) -> Result<&Token, ParserError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParserError::ExpectedToken {
                expected: expected.to_string(),
                span: self.get_last_span(),
            })
        }
    }

    fn consume_owned(&mut self, kind: TokenKind, expected: &'a str) -> Result<usize, ParserError> {
        if self.check(kind) {
            Ok(self.advance_owned())
        } else {
            let span = self.get_last_span();
            // let token = self.peek().clone();
            // let span = span.merge(token.span);
            // self.synchronize();
            let found = self.source.span_content(span).unwrap_or("");
            Err(ParserError::Expected {
                expected: expected.to_string(),
                found: found.to_string(),
                span: span,
            })
        }
    }

    fn advance_owned(&mut self) -> usize {
        if !self.is_at_end() {
            self.position += 1;
        }
        self.previous_owned()
    }

    fn previous_owned(&mut self) -> usize {
        self.position - 1
    }

    fn dummy_token(&self) -> usize {
        self.position - 1
    }

    fn get_token(&mut self, position: usize) -> &Token {
        &self.tokens[position]
    }

    fn pool(&mut self, str: &str) -> StrId {
        self.str_pool.pool(str)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.position += 1;
        }
        self.previous()
    }

    fn previous(&mut self) -> &Token {
        &self.tokens[self.position - 1]
    }
}
