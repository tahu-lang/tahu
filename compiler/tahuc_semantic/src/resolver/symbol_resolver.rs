use std::collections::HashMap;

use tahuc_ast::nodes::{
    ast::NodeId, declarations::{Declaration, DeclarationKind}, expressions::{Argument, ExpressionKind}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind, Variable}, Expression
};
use tahuc_module::{config::Config, resolver::ModuleResult};
use tahuc_span::{FileId, Span, StrId};

use crate::{
    database::{
        database::Database,
        symbol::{FunctionSymbol, StructSymbol, Symbol, SymbolKind, SymbolTrait, VariableSymbol},
    },
    error::SemanticError,
};

#[derive(Debug, Clone, Copy)]
enum SearchContext {
    All,
    Function,
    Struct,
}

enum LookupStatus<T: SymbolTrait> {
    NotFound,
    Ambiguous(Vec<T>),
}

pub struct SymbolResolution<'a> {
    config: &'a Config<'a>,
    db: &'a mut Database,
    file_id: FileId,
}

impl<'a> SymbolResolution<'a> {
    pub fn new(config: &'a Config, db: &'a mut Database) -> Self {
        Self {
            config,
            db,
            file_id: FileId(0),
        }
    }

    pub fn analyze_module(&mut self, module: &HashMap<FileId, ModuleResult>) {
        for (file_id, result) in module {
            self.db.reset_scope();
            self.file_id = *file_id;

            for declaration in &result.module.declaration {
                self.resolve_declaration(declaration);
            }
        }
    }

    fn add_variable(&mut self, id: NodeId, var: &Variable) -> Result<VariableSymbol, String> {
        let var_symbol = VariableSymbol {
            file_id: self.file_id,
            id,
            name: var.name,
            declared_type: var.variable_type.clone(),
            is_mutable: var.is_mutable,
            initializer: None,
            visibility: var.visibility,
            need_inferred: var.variable_type.is_inferred(),
            inferred_type: None,
            final_type: var.variable_type.clone(),
            span: var.span,
        };

        let result = self.add_var(var_symbol.clone());

        if let Err(err) = result {
            return Err(err);
        }

        return Ok(var_symbol);
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Fn(func) => {
                if let Some(_) = self.get_top_function(func.kind.name) {
                    self.db.enter_scope();

                    for param in func.kind.parameters.iter() {
                        let ty = self.db.get_type(self.file_id, param.id).unwrap();
                        let var_symbol = VariableSymbol {
                            file_id: self.file_id,
                            id: param.id,
                            name: param.kind.name,
                            declared_type: ty.clone(),
                            initializer: None,
                            is_mutable: true,
                            visibility: tahuc_ast::nodes::declarations::Visibility::Private,
                            need_inferred: false,
                            inferred_type: None,
                            final_type: ty.clone(),
                            span: param.span,
                        };

                        self.set_symbol(param.id, var_symbol.clone().into());

                        if let Err(_) = self.add_var(var_symbol) {
                            let name = self.config.str_pool.get(param.kind.name);
                            let prev = self.lookup_variable(param.kind.name).unwrap().clone();
                            let prev_span = prev.span;
                            let prev_name = self.config.str_pool.get(prev.name);
                            self.db.report_error(SemanticError::Duplicate {
                                name: name.to_string(),
                                span: param.span,
                                previous: prev_name.to_string(),
                                previous_span: prev_span,
                            });
                        }
                    }

                    self.resolve_block(&func.kind.body);

                    self.db.exit_scope();
                } else {
                    self.db.report_error(SemanticError::UndefinedFunction {
                        name: self.config.str_pool.get(func.kind.name).to_string(),
                        span: func.span,
                    });
                }
            }
            _ => {}
        }
    }

    fn resolve_block(&mut self, block: &Block) {
        self.db.enter_scope();
        for statement in &block.statements {
            self.resolve_statement(statement);
        }
        self.db.exit_scope();
    }

    fn resolve_if_statement(&mut self, if_stmt: &IfStatement) {
        self.resolve_expression(&if_stmt.condition, SearchContext::All);
        self.resolve_block(&if_stmt.then_branch);

        if let Some(else_branch) = &if_stmt.else_branch {
            match &else_branch {
                ElseBranch::Block(block) => self.resolve_block(block),
                ElseBranch::If(else_if_stmt) => self.resolve_if_statement(else_if_stmt),
            }
        }
    }

    fn resolve_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Expression(expr) => {
                self.resolve_expression(expr, SearchContext::All);
            }
            StatementKind::Variable(var) => {
                let symbol = self.add_variable(statement.id, var);
                if let Err(_) = symbol {
                    let name = self.config.str_pool.get(var.name);
                    let prev = self.lookup_variable(var.name).unwrap();
                    let prev_span = prev.span;
                    self.db.report_error(SemanticError::Duplicate {
                        name: name.to_string(),
                        span: var.span,
                        previous: name.to_string(),
                        previous_span: prev_span,
                    });
                }
                if let Some(initializer) = &var.initializer {
                    self.resolve_expression(initializer, SearchContext::All);
                }
                if let Ok(symbol) = symbol {
                    self.set_symbol(statement.id, symbol.into());
                }
            }
            StatementKind::Assignment { left, right, .. } => {
                self.resolve_expression(&left, SearchContext::All);
                self.resolve_expression(&right, SearchContext::All);
            }
            StatementKind::Return(ret) => {
                if let Some(value) = &ret {
                    self.resolve_expression(value, SearchContext::All);
                }
            }
            StatementKind::IfStatement(if_stmt) => {
                self.resolve_if_statement(if_stmt);
            }
            StatementKind::WhileStatement(while_stmt) => {
                self.resolve_expression(&while_stmt.condition, SearchContext::All);
                self.resolve_block(&while_stmt.body);
            }
            _ => {}
        }
    }

    fn report_lookup_status<T: SymbolTrait>(&mut self, name: StrId, span: Span, status: LookupStatus<T>, search_contex: SearchContext) -> Option<Symbol> {
        match status {
            LookupStatus::NotFound => {
                self.report_lookup_status_error(name, span, search_contex);
            }
            LookupStatus::Ambiguous(symbols) => {
                let mut new_symbols = Vec::new();
                for symbol in symbols {
                    let symbol: Symbol = symbol.clone().into_symbol();
                    if symbol.file_id != self.file_id {
                        if symbol.visibility.is_public() {
                            new_symbols.push(symbol);
                        }
                    } else {
                        new_symbols.push(symbol);
                    }
                }

                if new_symbols.len() > 1 {
                    self.db.report_error(SemanticError::Ambiguous {
                        name: self.config.str_pool.get(name).to_string(),
                        span,
                        related: new_symbols
                            .iter()
                            .map(|s| {
                                (s.span, self.config.str_pool.get(s.name).to_string())
                            })
                            .collect(),
                    });
                } else if new_symbols.len() == 0 {
                    self.report_lookup_status_error(name, span, search_contex);
                } else {
                    return Some(new_symbols[0].clone());
                }
            }
        }

        None
    }

    fn report_lookup_status_error(&mut self, name: StrId, span: Span, search_contex: SearchContext) {
        let name = self.config.str_pool.get(name).to_string();
        match search_contex {
            SearchContext::Struct => {
                self.db.report_error(SemanticError::UndefinedStruct { name, span});
            }
            SearchContext::Function => {
                self.db.report_error(SemanticError::UndefinedFunction { name, span});
            }
            _ => {
                self.db.report_error(SemanticError::Undefined {
                    name,
                    span,
                });
            }
        }
    }

    fn resolve_expression(&mut self, expression: &Expression, search_contex: SearchContext) {
        match &expression.kind {
            ExpressionKind::Identifier(identifer) => {
                if let SearchContext::Struct = search_contex {
                    let status = self.lookup_struct_with_import(*identifer);
                    match status {
                        Ok(s) => {
                            self.set_symbol(expression.id, s.clone().into());
                            self.resolve_symbol(s.file_id, s.into(), search_contex);
                        }
                        Err(status) => {
                            if let Some(symbol) = self.report_lookup_status(*identifer, expression.span, status, search_contex) {
                                self.set_symbol(expression.id, symbol.clone().into());
                                self.resolve_symbol(symbol.file_id, symbol.into(), search_contex);
                            }
                        }
                    }
                } else if let SearchContext::Function = search_contex {
                    let status = self.lookup_function_with_import(*identifer);
                    match status {
                        Ok(s) => {
                            self.set_symbol(expression.id, s.clone().into());
                            self.resolve_symbol(s.file_id, s.into(), search_contex);
                        }
                        Err(status) => {
                            if let Some(symbol) = self.report_lookup_status(*identifer, expression.span, status, search_contex) {
                                self.set_symbol(expression.id, symbol.clone().into());
                                self.resolve_symbol(symbol.file_id, symbol.into(), search_contex);
                            }
                        }
                    }
                } else {
                    let symbols = self.lookup_all(*identifer);
                    if symbols.len() == 1 {
                        self.set_symbol(expression.id, symbols[0].clone().into());
                        self.resolve_symbol(symbols[0].file_id, symbols[0].clone(), search_contex);
                    } else if symbols.len() > 1 {
                        let mut new_symbols = Vec::new();
                        let mut private_symbols = Vec::new();
                        for symbol in symbols {
                            if symbol.file_id != self.file_id {
                                if symbol.visibility.is_public() {
                                    new_symbols.push(symbol);
                                } else {
                                    private_symbols.push(symbol);
                                }
                            } else {
                                new_symbols.push(symbol);
                            }
                        }

                        if private_symbols.len() > 0 {
                            let symbol = private_symbols[0].clone();
                            self.resolve_symbol(symbol.file_id, symbol, search_contex);
                            return;
                        }

                        if new_symbols.len() > 1 {
                            self.db.report_error(SemanticError::Ambiguous {
                                name: self.config.str_pool.get(*identifer).to_string(),
                                span: expression.span,
                                related: new_symbols
                                    .iter()
                                    .map(|s| (s.span, self.config.str_pool.get(s.name).to_string()))
                                    .collect(),
                            });
                        } else if new_symbols.len() == 0 {
                            self.db.report_error(SemanticError::Undefined {
                                name: self.config.str_pool.get(*identifer).to_string(),
                                span: expression.span,
                            });
                        } else {
                            self.set_symbol(expression.id, new_symbols[0].clone().into());
                            self.resolve_symbol(new_symbols[0].file_id, new_symbols[0].clone(), search_contex);
                        }
                    } else {
                        self.db.report_error(SemanticError::Undefined {
                            name: self.config.str_pool.get(*identifer).to_string(),
                            span: expression.span,
                        });
                    }
                }
            }
            ExpressionKind::Ternary { condition, then, otherwise } => {
                self.resolve_expression(condition, SearchContext::All);
                self.resolve_expression(then, SearchContext::All);
                self.resolve_expression(otherwise, SearchContext::All);
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.resolve_expression(left, SearchContext::All);
                self.resolve_expression(right, SearchContext::All);
            }
            ExpressionKind::Unary { operand, .. } => {
                self.resolve_expression(operand, SearchContext::All);
            }
            ExpressionKind::ArrayLiteral { elements } => {
                elements.iter().for_each(|element| {
                    self.resolve_expression(element, SearchContext::All);
                });
            }
            ExpressionKind::ArrayAccess { array, index } => {
                self.resolve_expression(array, SearchContext::All);
                self.resolve_expression(index, SearchContext::All);
            }
            ExpressionKind::StructLiteral { object, fields } => {
                self.resolve_expression(object, SearchContext::Struct);

                for field in fields {
                    if let Some(value) = &field.kind.value {
                        self.resolve_expression(&value, SearchContext::All);
                    } else {
                        self.resolve_expression(&field.kind.name, SearchContext::All);
                    }
                }
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.resolve_expression(object, SearchContext::All);
            }
            ExpressionKind::FunctionCall(callee) => {
                self.resolve_expression(&callee.function, SearchContext::Function);

                callee.arguments.iter().for_each(|argument| {
                    self.resolve_argument(argument);
                });
            }
            ExpressionKind::Grouping(group) => {
                self.resolve_expression(group, SearchContext::All);
            }
            ExpressionKind::Cast { expression, .. } => {
                self.resolve_expression(expression, SearchContext::All);
            }
            _ => {}
        }
    }

    fn resolve_symbol(&mut self, file_id: FileId, symbol: Symbol, search_contex: SearchContext) {
        let current_file_id = self.file_id;
        if current_file_id != file_id {
            if !symbol.visibility.is_public() {
                let name = self.config.str_pool.get(symbol.name);
                match search_contex {
                    SearchContext::Struct => {
                        self.private_struct(name, symbol.span);
                    }
                    SearchContext::Function => {
                        self.private_function(name, symbol.span);
                    }
                    SearchContext::All => {
                        match symbol.kind {
                            SymbolKind::Struct(_) => {
                                self.private_struct(name, symbol.span);
                            }
                            SymbolKind::Function(_) => {
                                self.private_function(name, symbol.span);
                            }
                            SymbolKind::Variable(_) => {
                                self.private_variable(name, symbol.span);
                            }
                        }
                    }
                }
            }
        }
    }

    fn private_struct(&mut self, name: &str, span: tahuc_span::Span) {
        self.db.report_error(SemanticError::PrivateStruct {
            name: name.to_string(),
            span,
        });
    }

    fn private_function(&mut self, name: &str, span: tahuc_span::Span) {
        self.db.report_error(SemanticError::PrivateFunction {
            name: name.to_string(),
            span,
        });
    }

    fn private_variable(&mut self, name: &str, span: tahuc_span::Span) {
        self.db.report_error(SemanticError::PrivateVariable {
            name: name.to_string(),
            span,
        });
    }

    fn resolve_argument(&mut self, argument: &Argument) {
        match &argument {
            Argument::Named { value, .. } => {
                self.resolve_expression(value, SearchContext::All);
            }
            Argument::Positional(expr) => {
                self.resolve_expression(expr, SearchContext::All);
            }
        }
    }

    fn lookup_struct_with_import(&mut self, name: StrId) -> Result<StructSymbol, LookupStatus<StructSymbol>> {
        let structs = self.db.lookup_struct_with_import(self.file_id, name, false);
        if structs.len() == 0 {
            return Err(LookupStatus::NotFound);
        } else if structs.len() > 1 {
            return Err(LookupStatus::Ambiguous(structs));
        } else {
            return Ok(structs[0].clone());
        }
    }

    fn lookup_function_with_import(&mut self, name: StrId) -> Result<FunctionSymbol, LookupStatus<FunctionSymbol>> {
        let functions = self.db.lookup_fn_with_import(self.file_id, name, false);
        if functions.len() == 0 {
            return Err(LookupStatus::NotFound);
        } else if functions.len() > 1 {
            return Err(LookupStatus::Ambiguous(functions));
        } else {
            return Ok(functions[0].clone());
        }
    }

    #[allow(dead_code)]
    fn lookup_variable_with_import(&mut self, name: StrId) -> Result<VariableSymbol, LookupStatus<VariableSymbol>> {
        let variables = self.db.lookup_var_with_import(self.file_id, name, false);
        if variables.len() == 0 {
            return Err(LookupStatus::NotFound);
        } else if variables.len() > 1 {
            return Err(LookupStatus::Ambiguous(variables));
        } else {
            return Ok(variables[0].clone());
        }
    }

    fn get_top_function(&mut self, name: StrId) -> Option<&FunctionSymbol> {
        self.db.get_fn_top_level(self.file_id, name)
    }

    fn add_var(&mut self, var: VariableSymbol) -> Result<(), String> {
        self.db.add_var(self.file_id, var.name, var)
    }

    fn lookup_variable(&mut self, name: StrId) -> Option<&VariableSymbol> {
        self.db.lookup_var(self.file_id, name)
    }

    fn lookup_all(&mut self, name: StrId) -> Vec<Symbol> {
        self.db.lookup_all(self.file_id, name)
    }

    fn set_symbol(&mut self, id: NodeId, symbol: Symbol) {
        self.db.set_symbol(self.file_id, id, symbol);
    }
}
