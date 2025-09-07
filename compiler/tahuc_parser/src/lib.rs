use tahuc_ast::{
    AstBuilder, Module, Type,
    nodes::{
        Expression,
        ast::AstNode,
        declarations::{Declaration, Parameter, ParameterKind, Visibility},
        expressions::ExpressionKind,
        statements::Block,
    },
};
use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_lexer::{
    LexerResult,
    token::{Token, TokenKind},
};
use tahuc_span::{FileId, Span};

use crate::error::ParserError;

mod declaration;
mod error;
mod expr;
mod shared;
mod stmt;

#[derive(Debug)]
pub struct Parser<'a> {
    file_id: FileId,
    lexer: LexerResult,
    builder: AstBuilder,
    tokens: Vec<Token>,
    position: usize,
    reporter: &'a mut DiagnosticReporter,
}

#[derive(Debug)]
pub struct ParserResult {
    pub module: Module,
    pub has_errors: bool,
}

impl<'a> Parser<'a> {
    pub fn new(file_id: FileId, lexer: LexerResult, reporter: &'a mut DiagnosticReporter) -> Self {
        Parser::with_builder(file_id, lexer, AstBuilder::new(), reporter)
    }

    pub fn with_builder(
        file_id: FileId,
        lexer: LexerResult,
        builder: AstBuilder,
        reporter: &'a mut DiagnosticReporter,
    ) -> Self {
        Self {
            file_id,
            lexer: lexer.clone(),
            builder,
            tokens: lexer.tokens,
            position: 0,
            reporter,
        }
    }

    pub fn parse_expression(
        file_id: FileId,
        tokens: Vec<Token>,
        reporter: &'a mut DiagnosticReporter,
    ) -> Result<Expression, ParserError> {
        let mut parser = Parser::new(
            file_id,
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
        let mut has_errors = false;

        while !self.is_at_end() {
            if self.check(TokenKind::Newline) {
                self.advance();
                continue;
            }

            match self.declaration(Visibility::Private) {
                Ok(program) => programs.push(program),
                Err(error) => {
                    let dianostic = error.to_diagnostic();
                    self.reporter.report(dianostic);
                    has_errors = true;
                    self.synchronize();
                }
            }
        }

        ParserResult {
            module: Module {
                file: self.file_id,
                declaration: programs,
            },
            has_errors: has_errors || self.lexer.has_errors,
        }
    }

    fn declaration(&mut self, visibility: Visibility) -> Result<Declaration, ParserError> {
        match self.peek().kind {
            TokenKind::Class => self.class_declaration(visibility),
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
            TokenKind::Priv => {
                self.advance();
                self.declaration(Visibility::Private)
            }
            TokenKind::Prot => {
                self.advance();
                self.declaration(Visibility::Protected)
            }
            _ => {
                let token = self.peek().clone();
                self.synchronize();
                Err(ParserError::UnexpectedTopLevel {
                    found: token.lexeme,
                    span: token.span,
                })
            }
        }
    }

    /// for now just simple extern
    /// extern fn name(params) return_type
    fn parse_extern(&mut self) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().clone();
        self.advance();

        self.consume(TokenKind::Fn, "Expected 'fn' keyword")?;

        let name = self
            .consume(TokenKind::Identifier, "Expected function name")?
            .clone();

        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;

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

        self.consume(TokenKind::RightParen, "expected ')' after parameters")?;

        let return_type = self.parse_type()?;

        if self.check(TokenKind::Semicolon) {
            self.advance();
        }

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

    fn parse_parameters(&mut self, is_extern: bool) -> Result<Parameter, ParserError> {
        let start_span = self.current_token().clone();
        let name = self
            .consume(TokenKind::Identifier, "Expected parameter name")?
            .clone();

        self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;

        let r#type = self.parse_type()?;

        let mut default: Option<AstNode<ExpressionKind>> = None;

        if self.check(TokenKind::Assign) && !is_extern {
            self.advance();
            let value: AstNode<ExpressionKind> = self.expression()?;
            default = Some(value);
        } else {
            if is_extern && self.check(TokenKind::Assign) {
                return Err(ParserError::Unexpected {
                    unexcepted: "cannot assign value to extern function".to_string(),
                    span: self.get_last_span(),
                });
            }
        }

        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span.span, end_span);

        Ok(self.builder.build_parameter(
            span,
            self.file_id,
            ParameterKind {
                name: name.clone().lexeme,
                r#type: r#type,
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
            let stmt = self.statement()?;
            statements.push(stmt);
            self.skip_newlines();
        }

        let end_token = self.consume(TokenKind::RightBrace, "Expected '}'")?;
        let span = Span::new(start_span.start, end_token.span.end, start_span.file_id);

        Ok(Block { statements, span })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let is_pointer = if self.current_token().kind == TokenKind::Mul {
            self.advance();
            true
        } else {
            false
        };
        let base_type = match &self.current_token().kind {
            TokenKind::Identifier => {
                let type_name = self.advance().lexeme.clone();
                Ok(match type_name.as_str() {
                    "string" => Type::String,
                    "int" => Type::Int,
                    "double" => Type::Double,
                    "bool" => Type::Boolean,
                    "void" => Type::Void,
                    _ => Type::Named(type_name),
                })
            }
            TokenKind::String => {
                self.advance();
                Ok(Type::String)
            }
            TokenKind::Integer => {
                self.advance();
                Ok(Type::Int)
            }
            TokenKind::Double => {
                self.advance();
                Ok(Type::Double)
            }
            TokenKind::Boolean => {
                self.advance();
                Ok(Type::Boolean)
            }
            TokenKind::Void => {
                self.advance();
                Ok(Type::Void)
            }
            TokenKind::LeftBracket => {
                self.advance();
                let ty = self.parse_type()?;
                self.consume(TokenKind::RightBracket, "Expected ')' after type")?;
                Ok(Type::Array(Box::new(ty)))
            }
            _ => {
                let token = self.current_token().clone();
                Err(ParserError::Expected {
                    expected: "Expected type name".to_string(),
                    found: token.lexeme,
                    span: token.span,
                })
            }
        };

        if is_pointer {
            Ok(Type::Pointer(Box::new(base_type?)))
        } else {
            base_type
        }
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
                TokenKind::Fn
                | TokenKind::Var
                | TokenKind::Class
                | TokenKind::Pub
                | TokenKind::Eof => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    //===== HELPER METHOD =====//
    fn log_current_token(&mut self) {
        println!("Current Token: {:?}", self.peek());
    }

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

    fn consume(&mut self, kind: TokenKind, expected: &str) -> Result<&Token, ParserError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            self.synchronize();
            Err(ParserError::Expected {
                expected: expected.to_string(),
                found: token.lexeme,
                span: token.span,
            })
        }
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
