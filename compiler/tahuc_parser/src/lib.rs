use tahuc_ast::{
    nodes::{
        ast::AstNode, declarations::{Declaration, Import, ImportTable, Parameter, ParameterKind, Visibility}, expressions::ExpressionKind, statements::Block, Expression
    }, ty::{PrimitiveType, Type}, AstBuilder, Module
};
use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_lexer::{
    LexerResult,
    token::{Token, TokenKind},
};
use tahuc_span::{FileId, Span};

use crate::error::ParserError;

mod error;
mod expr;
mod shared;
mod stmt;
mod test;

#[derive(Debug)]
pub(crate) enum ParserContext {
    TopLevel,
    Statement,
    Expression,
}

impl ParserContext {
    pub fn is_top_level(&self) -> bool {
        matches!(self, ParserContext::TopLevel)
    }

    pub fn is_statement(&self) -> bool {
        matches!(self, ParserContext::Statement)
    }

    pub fn is_expression(&self) -> bool {
        matches!(self, ParserContext::Expression)
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    file_id: FileId,
    lexer: LexerResult,
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
            parser_errors: Vec::new(),
            reporter,
            context: ParserContext::TopLevel,
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

    fn declaration(&mut self, visibility: Visibility) -> Result<Declaration, ParserError> {
        match self.peek().kind {
            TokenKind::Import => {
                self.parse_import()
            }
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
            TokenKind::Struct => self.parse_struct(visibility),
            _ => {
                let token = self.peek().clone();
                // self.synchronize();
                Err(ParserError::UnexpectedTopLevel {
                    found: token.lexeme,
                    span: token.span,
                })
            }
        }
    }

    fn parse_import(&mut self) -> Result<Declaration, ParserError> {
        self.advance();
        let span = self.get_last_span();

        let mut is_wildcard = false;
        let mut table = Vec::new();

        if self.check(TokenKind::Mul) {
            self.advance();
            is_wildcard = true;
        } else {
            let right_brace = self.consume_owned(TokenKind::LeftBrace, "Expected '{' for import");
            self.report_error(right_brace);

            while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
                let ident = self.consume_owned(TokenKind::Identifier, "Expected identifier for import");
                let name_id = self.get_or_default(ident, self.dummy_token());
    
                let alias = if self.check(TokenKind::As) {
                    self.advance();
                    let ident = self.consume(TokenKind::Identifier, "Expected identifier for import").cloned();
                    Some(self.get_or_default(ident, Token::dummy()).lexeme)
                } else {
                    None
                };

                let end_span = self.get_last_span();

                let name = self.tokens[name_id].clone();
                let span = self.make_span_fspan(name.span, end_span);

                table.push(self.builder.build_import_table(span, self.file_id, ImportTable {
                    name: name.lexeme.clone(),
                    alias,
                }));

                if self.check(TokenKind::Comma) {
                    self.advance();
                    if self.check(TokenKind::RightBrace) {
                        break;
                    }
                } else if !self.check(TokenKind::RightBrace) {
                    let current = self.current_token();
                    let err = ParserError::Expected {
                        expected: format!(""),
                        found: current.lexeme.clone(),
                        span: current.span,
                    };
                    self.add_error(err);
                    self.synchronize();
                }
            }
        };

        if !is_wildcard {
            let rbrace = self.consume(TokenKind::RightBrace, "Expected '}' for import").cloned();
            self.report_error(rbrace);
        }

        let from_kw = self.consume(TokenKind::From, "Expected from keyword after import").cloned();
        self.report_error(from_kw);

        // let result_path = self.consume(TokenKind::Literal(tahuc_lexer::token::Literal::String(s)), "").cloned();
        let current_token = self.peek().clone();

        let path = match &current_token.kind {
            TokenKind::Literal(tahuc_lexer::token::Literal::String(s)) => s,
            _ => {
                let current_token = self.current_token();
                return Err(ParserError::Expected {
                    expected: format!("expected string path"),
                    found: format!("but found `{}`", current_token.lexeme.clone()),
                    span: current_token.span,
                });
            }
        };

        self.advance();

        let semi = self.consume(TokenKind::Semicolon, "Expected ';' after import path").cloned();
        self.report_error(semi);

        let end_span = self.get_last_span();
        // let span = span.merge(end_span);
        let span = self.make_span_fspan(span, end_span);
        Ok(self.builder.build_import(span, self.file_id, Import {
            imports: table,
            is_wildcard,
            path: path.clone(),
            span,
        }))
    }

    /// for now just simple extern
    /// extern fn name(params) return_type
    fn parse_extern(&mut self) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().clone();
        self.advance();

        self.consume(TokenKind::Fn, "Expected 'fn' keyword")?;

        let result_name = self.consume(TokenKind::Identifier, "Expected function name").cloned();
        let name = self.get_or_default(result_name, Token::dummy()).clone();

        let left_paren =  self.consume(TokenKind::LeftParen, "Expected '(' after function name").cloned();
        self.report_error(left_paren);

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

        let right_paren = self.consume(TokenKind::RightParen, "expected ')' after parameters").cloned();
        self.report_error(right_paren);

        let return_type = if self.check(TokenKind::Semicolon) {
            Type::Primitive(PrimitiveType::Unit)
        } else {
            self.parse_type()?
        };

        let semi_colon = self.consume(TokenKind::Semicolon, "Expected ';' after extern function declaration").cloned();
        self.report_error(semi_colon);

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

    fn parse_struct(&mut self, visibility: Visibility) -> Result<Declaration, ParserError> {
        let start_span = self.current_token().clone();
        self.advance();

        let name = self.consume(TokenKind::Identifier, "Expected struct name").cloned();
        let name = self.get_or_default(name, Token::dummy());

        let mut fields = Vec::new();
        let mut fields_ty = Vec::new();
        // if !self.check(TokenKind::LeftBrace) {
        //     let token = self.peek().clone();
        //     self.synchronize();
        //     return Err(ParserError::Expected {
        //         expected: "Expected '{' after struct name".to_string(),
        //         found: token.lexeme,
        //         span: token.span,
        //     });
        // }

        let left_brace = self.consume(TokenKind::LeftBrace, "Expected '{' after struct name").cloned();
        self.report_error(left_brace);

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let field_start_token = self.current_token().clone();
            let field_name = self
                .consume(TokenKind::Identifier, "Expected field name")?
                .lexeme
                .clone();
            self.consume(TokenKind::Colon, "Expected ':' after field name")?;
            let r#type = self.parse_type()?;

            if !self.check(TokenKind::RightBrace) {
                self.consume(TokenKind::Comma, "Expected ',' after field declaration")?;
            }

            // self.consume(TokenKind::Semicolon, "Expected ';' after field declaration")?;

            let field_end_token = self.previous().clone();
            let field_span = self.make_span(field_start_token, field_end_token);

            fields_ty.push((field_name.clone(), r#type.clone()));
            fields.push(self.builder.build_struct_field(
                field_span,
                self.file_id,
                field_name,
                r#type,
            ));
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' after struct fields").cloned();
        self.report_error(right_brace);
        let end_span = self.get_last_span();
        let span = self.make_span_fspan(start_span.span, end_span);

        let ty = Type::Struct { name: name.lexeme.clone(), fields: fields_ty };

        Ok(self
            .builder
            .build_struct_declaration(span, self.file_id, visibility, name.lexeme, fields, ty))
            
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
            r#type = Type::Nullable(Box::new(r#type));
        }

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

        let name = &self.tokens[name_id];

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

        let end_token = self.consume(TokenKind::RightBrace, "Expected '}'")?;
        let span = Span::new(start_span.start, end_token.span.end, start_span.file_id);

        Ok(Block { statements, span })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        match &self.current_token().kind {
            TokenKind::Identifier => {
                let type_name = self.advance().lexeme.clone();
                Ok(match type_name.as_str() {
                    _ => Type::Named(type_name),
                })
            }
            TokenKind::I8 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I8))
            }
            TokenKind::I16 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I16))
            }
            TokenKind::I32 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I32))
            }
            TokenKind::I64 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::I64))
            }
            TokenKind::Isize => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Isize))
            }
            TokenKind::U8 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U8))
            }
            TokenKind::U16 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U16))
            }
            TokenKind::U32 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U32))
            }
            TokenKind::U64 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::U64))
            }
            TokenKind::Usize => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Usize))
            }

            // floating point
            TokenKind::F32 => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::F32))
            }
            TokenKind::F64 => {
                Ok(Type::Primitive(PrimitiveType::F64))
            }

            TokenKind::Bool => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Bool))
            }
            TokenKind::Char => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Char))
            }
            TokenKind::Unit => {
                self.advance();
                Ok(Type::Primitive(PrimitiveType::Unit))
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
            
            TokenKind::Mul => {
                self.advance();
                Ok(Type::Pointer(Box::new(self.parse_type()?)))
            }
            TokenKind::LeftBracket => {
                self.advance();
                let ty = self.parse_type()?;
                let size = if self.check(TokenKind::Semicolon) {
                    self.advance();
                    let size = self.parse_size_array();
                    let size = self.get_or_default(size, 0);
                    self.advance();
                    size
                } else {
                    0
                };
                let right_bracket = self.consume(TokenKind::RightBracket, "Expected ']' after type").cloned();
                self.report_error(right_bracket);
                Ok(Type::Array { ty: Box::new(ty), size: size as usize })
            }
            _ => {
                let token = self.current_token().clone();
                Err(ParserError::Expected {
                    expected: "Expected type name".to_string(),
                    found: token.lexeme,
                    span: token.span,
                })
            }
        }
    }

    fn parse_size_array(&mut self) -> Result<i64, ParserError> {
        match &self.peek().kind {
            TokenKind::Literal(lit) => {
                match lit {
                    tahuc_lexer::token::Literal::Integer(i) => Ok(*i),
                    _ => Err(ParserError::Expected {
                        expected: format!("Expected integer literal for array size"),
                        found: self.peek().lexeme.clone(),
                        span: self.peek().span,
                    })
                }
            }
            _ => Err(ParserError::Expected {
                expected: format!("Expected integer literal for array size"),
                found: self.peek().lexeme.clone(),
                span: self.peek().span,
            })
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
                | TokenKind::Pub
                | TokenKind::Eof => return,
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

    fn switch_top_level_context(&mut self) {
        self.switch_context(ParserContext::TopLevel);
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

    fn consume(&mut self, kind: TokenKind, expected: &str) -> Result<&Token, ParserError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let span = self.get_last_span();
            let token = self.peek().clone();
            let span = span.merge(token.span);
            // self.synchronize();
            Err(ParserError::Expected {
                expected: expected.to_string(),
                found: format!("found '{}'", token.lexeme),
                span: span,
            })
        }
    }

    fn consume_owned(&mut self, kind: TokenKind, expected: &str) -> Result<usize, ParserError> {
        if self.check(kind) {
            Ok(self.advance_owned())
        } else {
            let span = self.get_last_span();
            let token = self.peek().clone();
            let span = span.merge(token.span);
            // self.synchronize();
            Err(ParserError::Expected {
                expected: expected.to_string(),
                found: format!("found '{}'", token.lexeme),
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
        usize::MAX
    }

    fn get_token(&mut self, position: usize) -> &Token {
        &self.tokens[position]
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
