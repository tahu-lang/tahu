use tahuc_ast::{
    Module,
    nodes::{
        Expression,
        declarations::{Declaration, DeclarationKind},
        expressions::{Argument, ExpressionKind},
        statements::{Block, ElseBranch, IfStatement, Statement, StatementKind, Variable},
    },
};
use tahuc_span::FileId;

use crate::{
    database::Database,
    error::SemanticError,
    symbol::{Symbol, SymbolKind, VariableSymbol},
};

pub struct SymbolResolution<'a> {
    db: &'a mut Database,
    file_id: FileId,
}

impl<'a> SymbolResolution<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            file_id: FileId(0),
        }
    }

    pub fn analyze_module(&mut self, module: &Module) {
        self.db.reset_scope();
        self.file_id = module.file;

        for declaration in &module.declaration {
            self.resolve_declaration(declaration);
        }
    }

    fn add_variable(&mut self, var: &Variable) -> Result<(), String> {
        let symbol = Symbol {
            name: var.name.clone(),
            span: var.span,
            kind: SymbolKind::Variable(VariableSymbol {
                name: var.name.clone(),
                declared_type: var.variable_type.clone(),
                need_inferred: var.variable_type.is_inferred(),
                inferred_type: None,
                initializer: None,
                final_type: var.variable_type.clone(),
            }),
        };

        self.add_symbol(symbol)
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) {
        match &declaration.kind {
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
                        self.resolve_expression(initiliazer);
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
        self.resolve_expression(&if_stmt.condition);
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
            StatementKind::Variable(var) => {
                if let Err(_) = self.add_variable(var) {
                    let prev_name = self.lookup_symbol(var.name.clone()).unwrap().name;
                    let prev_span = self.lookup_symbol(var.name.clone()).unwrap().span;
                    self.db.report_error(SemanticError::Duplicate {
                        name: var.name.clone(),
                        span: var.span,
                        previous: prev_name,
                        previous_span: prev_span,
                    });
                }
                if let Some(initializer) = &var.initializer {
                    self.resolve_expression(initializer);
                }
            }
            StatementKind::Return(ret) => {
                if let Some(value) = &ret {
                    self.resolve_expression(value);
                }
            }
            StatementKind::Expression(expr) => {
                self.resolve_expression(expr);
            }
            StatementKind::IfStatement(if_stmt) => {
                self.resolve_if_statement(if_stmt);
            }
            StatementKind::WhileStatement(while_stmt) => {
                self.resolve_expression(&while_stmt.condition);
                self.resolve_block(&while_stmt.body);
            }
            _ => {}
        }
    }

    fn resolve_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::Identifier(identifer) => {
                if let Some(symbol) = self.lookup_symbol(identifer.clone()) {
                    if let Some(func) = symbol.get_function() {
                        if !func.visibility.is_public() {
                            if self.file_id != func.file_id {
                                self.db.report_error(SemanticError::PrivateFunction {
                                    name: func.name.clone(),
                                    span: func.span,
                                });
                            }
                        }
                    } else if let Some(_) = symbol.get_variable() {
                        // All good
                    } else {
                        self.db.report_error(SemanticError::Undefined {
                            name: identifer.clone(),
                            span: expression.span,
                        });
                    }
                } else {
                    self.db.report_error(SemanticError::Undefined {
                        name: identifer.clone(),
                        span: expression.span,
                    });
                }
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            ExpressionKind::Unary { operand, .. } => {
                self.resolve_expression(operand);
            }
            ExpressionKind::FunctionCall(callee) => {
                self.resolve_expression(&callee.function);

                callee.arguments.iter().for_each(|argument| {
                    self.resolve_argument(argument);
                });
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.resolve_expression(object);
            }
            _ => {}
        }
    }

    fn resolve_argument(&mut self, argument: &Argument) {
        match &argument {
            Argument::Named { value, .. } => {
                self.resolve_expression(value);
            }
            Argument::Positional(expr) => {
                self.resolve_expression(expr);
            }
        }
    }

    fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        self.db.add_symbol(self.file_id, symbol)
    }

    fn lookup_symbol(&mut self, name: String) -> Option<Symbol> {
        self.db.lookup_symbol(self.file_id, name)
    }
}
