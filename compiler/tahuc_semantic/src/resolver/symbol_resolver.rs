use std::collections::HashMap;

use tahuc_ast::{
    nodes::{
        ast::NodeId, declarations::{Declaration, DeclarationKind}, expressions::{Argument, ExpressionKind}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind, Variable}, Expression
    }
};
use tahuc_module::resolver::ModuleResult;
use tahuc_span::FileId;

use crate::{
    database::Database,
    error::SemanticError,
    symbol::{Symbol, SymbolKind, VariableSymbol},
};

struct ImportQuery {
    pub name: String,
    pub file_id: FileId,
}

enum SearchContext {
    All,
    Function,
    Struct,
    Variable,
}

pub struct SymbolResolution<'a> {
    db: &'a mut Database,
    file_id: FileId,
    map: HashMap<String, FileId>,
    cache_import: HashMap<String, ImportQuery>,
    search_wildcard: Vec<FileId>,
}

impl<'a> SymbolResolution<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            file_id: FileId(0),
            map: HashMap::new(),
            cache_import: HashMap::new(),
            search_wildcard: Vec::new(),
        }
    }

    pub fn analyze_module(&mut self, module: &HashMap<FileId, ModuleResult>) {
        for (file_id, result) in module {
            self.map.insert(result.name.clone(), *file_id);
        }

        for (file_id, result) in module {
            self.db.reset_scope();
            self.file_id = *file_id;

            for declaration in &result.module.declaration {
                self.resolve_declaration(declaration);
            }
        }
    }

    fn add_variable(&mut self, var: &Variable) -> Result<Symbol, String> {
        let symbol = Symbol {
            name: var.name.clone(),
            span: var.span,
            kind: SymbolKind::Variable(VariableSymbol {
                name: var.name.clone(),
                declared_type: var.variable_type.clone(),
                is_mutable: var.is_mutable,
                need_inferred: var.variable_type.is_inferred(),
                inferred_type: None,
                initializer: None,
                final_type: var.variable_type.clone(),
            }),
        };

        let result = self.add_symbol(symbol.clone());

        if let Err(err) = result {
            return Err(err);
        }

        return Ok(symbol);
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
            DeclarationKind::Import(import) => {
                let import_file = self.map.get(&import.path).unwrap();
                if import.is_wildcard {
                    self.search_wildcard.push(*import_file);
                } else {
                    for field in &import.imports {
                        let name = field.kind.alias.clone().unwrap_or(field.kind.name.clone());
                        self.cache_import.insert(name, ImportQuery {
                            file_id: *import_file,
                            name: field.kind.name.clone(),
                        });
                    }
                }
            }
            DeclarationKind::Fn(func) => {
                if let Some(_) = self.lookup_symbol(func.kind.name.clone()) {
                    self.db.enter_scope();

                    for p in func.kind.parameters.iter() {
                        if let Err(_) = self.add_symbol(Symbol {
                            name: p.kind.name.clone(),
                            span: p.span,
                            kind: SymbolKind::Variable(VariableSymbol {
                                name: p.kind.name.clone(),
                                declared_type: p.kind.r#type.clone(),
                                initializer: None,
                                is_mutable: true,

                                need_inferred: false,
                                inferred_type: None,
                                final_type: p.kind.r#type.clone(),
                            }),
                        }) {
                            let prev = self.lookup_symbol(p.kind.name.clone()).unwrap();
                            let prev_span = prev.span;
                            self.db
                                .report_error(crate::error::SemanticError::Duplicate {
                                    name: p.kind.name.clone(),
                                    span: p.kind.span,
                                    previous: prev.name,
                                    previous_span: prev_span,
                                });
                        }
                    }

                    func.kind.body.statements.iter().for_each(|statement| {
                        self.resolve_statement(statement);
                    });

                    self.db.exit_scope();
                }
            }
            DeclarationKind::Variable(var) => {
                if let Some(_) = self.lookup_symbol(var.name.clone()) {
                    if let Some(initiliazer) = &var.initializer {
                        self.resolve_expression(initiliazer, SearchContext::All);
                    }
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
                self.resolve_expression(expr,SearchContext::All);
            }
            StatementKind::Variable(var) => {
                let symbol = self.add_variable(var);
                if let Err(_) = symbol {
                    let prev = self.lookup_symbol(var.name.clone()).unwrap();
                    let prev_name = prev.name;
                    let prev_span = prev.span;
                    self.db.report_error(SemanticError::Duplicate {
                        name: var.name.clone(),
                        span: var.span,
                        previous: prev_name,
                        previous_span: prev_span,
                    });
                }
                if let Some(initializer) = &var.initializer {
                    self.resolve_expression(initializer, SearchContext::All);
                }
                if let Ok(symbol) = symbol {
                    self.set_symbol(statement.id, symbol.clone());
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
                self.resolve_expression(&while_stmt.condition,SearchContext::All);
                self.resolve_block(&while_stmt.body);
            }
            _ => {}
        }
    }

    fn resolve_expression(&mut self, expression: &Expression, search_contex: SearchContext) {
        match &expression.kind {
            ExpressionKind::Identifier(identifer) => {
                let mut found = false;
                // check file scope from local scope to file scope
                // see database
                if let Some(symbol) = self.lookup_symbol_scope_file(identifer.clone()) {
                    found = true;
                    self.set_symbol(expression.id, symbol.clone());
                    self.resolve_symbol(symbol, search_contex);
                } else {
                    // check in import with field selective
                    if let Some(query) = self.cache_import.get(identifer) {
                        if let Some(symbol) = self.db.lookup_global_scope(query.file_id, query.name.clone()) {
                            found = true;
                            self.set_symbol(expression.id, symbol.clone());
                            self.resolve_symbol(symbol, search_contex);
                        }
                    } else {
                        // search in wildcard
                        let wildcards = self.search_wildcard.clone();
                        for file_id in wildcards {
                            if let Some(symbol) = self.db.lookup_global_scope(file_id, identifer.clone()) {
                                found = true;
                                self.set_symbol(expression.id, symbol.clone());
                                self.resolve_symbol(symbol, search_contex);
                                break;
                            }
                        }
                    }
                }

                if !found {
                    self.db.report_error(SemanticError::Undefined {
                        name: identifer.clone(),
                        span: expression.span,
                    });
                }
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.resolve_expression(left, SearchContext::All);
                self.resolve_expression(right, SearchContext::All);
            }
            ExpressionKind::Unary { operand, .. } => {
                self.resolve_expression(operand, SearchContext::All);
            }
            ExpressionKind::FunctionCall(callee) => {
                self.resolve_expression(&callee.function, SearchContext::Function);

                callee.arguments.iter().for_each(|argument| {
                    self.resolve_argument(argument);
                });
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.resolve_expression(object, SearchContext::All);
            }
            ExpressionKind::StructLiteral { object, fields } => {
                self.resolve_expression(object, SearchContext::Struct);

                for field in fields {
                    if let Some(value) = &field.kind.value {
                        self.resolve_expression(&value, SearchContext::All);
                    }
                }
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
            ExpressionKind::Cast { expression, .. } => {
                self.resolve_expression(expression, SearchContext::All);
            }
            _ => {}
        }
    }

    fn resolve_symbol(&mut self, symbol: Symbol, search_contex: SearchContext) {
        match search_contex {
            SearchContext::Function => {
                if let Some(func) = symbol.get_function() {
                    if self.file_id != func.file_id {
                        if !func.visibility.is_public() {
                            self.db.report_error(SemanticError::PrivateFunction {
                                name: func.name.clone(),
                                span: func.span,
                            });
                        }
                    }
                } else {
                    self.db.report_error(SemanticError::NotFunction {
                        name: symbol.name.clone(),
                        span: symbol.span,
                    });
                }
            }
            SearchContext::Struct => {
                if let Some(struct_access) = symbol.get_struct() {
                    if self.file_id != struct_access.file_id {
                        if !struct_access.visibility.is_public() {
                            self.db.report_error(SemanticError::PrivateStruct {
                                name: struct_access.name.clone(),
                                span: struct_access.span,
                            });
                        }
                    }
                } else {
                    self.db.report_error(SemanticError::NotStruct {
                        name: symbol.name.clone(),
                        span: symbol.span,
                    });
                }
            }
            SearchContext::Variable => {
                if let None = symbol.get_variable() {
                    self.db.report_error(SemanticError::NotVariable {
                        name: symbol.name.clone(),
                        span: symbol.span,
                    });
                }
            }
            SearchContext::All => {
                if let Some(func) = symbol.get_function() {
                    if self.file_id != func.file_id {
                        if !func.visibility.is_public() {
                            self.db.report_error(SemanticError::PrivateFunction {
                                name: func.name.clone(),
                                span: func.span,
                            });
                        }
                    }
                } else if let Some(struct_access) = symbol.get_struct() {
                    if self.file_id != struct_access.file_id {
                        if !struct_access.visibility.is_public() {
                            self.db.report_error(SemanticError::PrivateStruct {
                                name: struct_access.name.clone(),
                                span: struct_access.span,
                            });
                        }
                    }
                }
            }
        }
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

    fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        self.db.add_symbol(self.file_id, symbol)
    }

    fn lookup_symbol(&mut self, name: String) -> Option<Symbol> {
        self.db.lookup_symbol(self.file_id, name)
    }

    fn lookup_symbol_scope_file(&mut self, name: String) -> Option<Symbol> {
        self.db.lookup_symbol_scope_file(self.file_id, name)
    }

    fn set_symbol(&mut self, id: NodeId, symbol: Symbol) {
        self.db.set_symbol(self.file_id, id, symbol);
    }
}
