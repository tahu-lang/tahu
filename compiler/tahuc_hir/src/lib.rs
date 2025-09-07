use std::collections::HashMap;

use tahuc_ast::{
    Module,
    nodes::{
        Expression,
        declarations::{DeclarationKind, Function},
        expressions::{Argument, ExpressionKind},
        statements::{Block, ElseBranch, IfStatement, Statement, StatementKind},
    },
};
use tahuc_lexer::token::Literal;
use tahuc_semantic::database::Database;

use crate::{
    hir::{
        FunctionId, HirBlock, HirElseBranch, HirExpression, HirExternFunction, HirFunction, HirIfStatement, HirLValue, HirLiteral, HirModule, HirParameters, HirStatement, HirVariable
    },
    printer::HirPrinter,
};

pub mod hir;
mod printer;

pub struct Hir {
    db: Database,
    functions: Vec<HirFunction>,
    extern_functions: Vec<HirExternFunction>,
    variables: HashMap<String, HirVariable>,
    functions_map: HashMap<String, FunctionId>,
    function_id: u32,
    variable_id: u32,
}

impl Hir {
    pub fn new(db: Database) -> Self {
        Self {
            db,
            functions: vec![],
            extern_functions: vec![],
            variables: HashMap::new(),
            functions_map: HashMap::new(),
            function_id: 0,
            variable_id: 0,
        }
    }

    fn next_id_function(&mut self) -> u32 {
        let id = self.function_id;
        self.function_id += 1;
        id
    }

    fn next_id_variable(&mut self) -> u32 {
        let id = self.variable_id;
        self.variable_id += 1;
        id
    }

    pub fn to_hir(&mut self, module: &Module) -> HirModule {
        // First pass: Register all function names with IDs
        for declaration in &module.declaration {
            match &declaration.kind {
                DeclarationKind::Fn(func) => {
                    let function_id = self.next_id_function();
                    self.functions_map.insert(func.kind.name.clone(), function_id);
                }
                DeclarationKind::Extern(extern_func) => {
                    let extern_fn_id = self.next_id_function();
                    self.functions_map.insert(extern_func.name.clone(), extern_fn_id);
                }
                _ => {}
            }
        }

        // Second pass: Process function bodies
        for declaration in &module.declaration {
            match &declaration.kind {
                DeclarationKind::Fn(func) => {
                    let fun = self.function_declaration(func);
                    self.functions.push(fun);
                }
                DeclarationKind::Extern(extern_func) => {
                    let mut hir_params: Vec<HirParameters> = Vec::new();

                    for param in &extern_func.parameters {
                        hir_params.push(HirParameters {
                            id: self.next_id_variable(),
                            name: param.kind.name.clone(),
                            ty: param.kind.r#type.clone(),
                        });
                    }
                    let extern_fn = HirExternFunction {
                        id: *self.functions_map.get(&extern_func.name).unwrap(),
                        name: extern_func.name.clone(),
                        parameters: hir_params,
                        return_type: extern_func.return_type.clone(),
                    };
                    self.extern_functions.push(extern_fn);
                }
                _ => {}
            }
        }

        HirModule {
            functions: self.functions.clone(),
            extern_functions: self.extern_functions.clone(),
        }
    }

    fn function_declaration(&mut self, func: &Function) -> HirFunction {
        let mut hir_params: Vec<HirParameters> = Vec::new();

        self.variables.clear();
        self.db.enter_scope();

        for param in &func.kind.parameters {
            let var = HirVariable {
                id: self.next_id_variable(),
                name: param.kind.name.clone(),
                ty: param.kind.r#type.clone(),
            };
            self.variables.insert(param.kind.name.clone(), var.clone());

            hir_params.push(HirParameters {
                id: var.id,
                name: param.kind.name.clone(),
                ty: param.kind.r#type.clone(),
            });
        }

        let block = self.lower_block(&func.kind.body);

        self.db.exit_scope();

        HirFunction {
            id: *self.functions_map.get(&func.kind.name).unwrap(), // Use pre-registered ID
            name: func.kind.name.clone(),
            parameters: hir_params,
            body: block,
            return_type: func.kind.return_type.clone(),
        }
    }

    fn lower_block(&mut self, block: &Block) -> HirBlock {
        self.db.enter_scope();

        let mut hir_statements: Vec<HirStatement> = Vec::new();
        for statement in &block.statements {
            let res = self.lower_statement(statement);
            match res.clone() {
                Ok(_) => {}
                Err(_) => {
                    println!("Error block {:?}", statement);
                    println!();
                }
            }
            hir_statements.push(res.unwrap());
        }

        self.db.enter_scope();

        HirBlock {
            statements: hir_statements,
        }
    }

    fn lower_statement(&mut self, statement: &Statement) -> Result<HirStatement, String> {
        match &statement.kind {
            StatementKind::Expression(expr) => {
                let hir_expr = self.lower_expression(expr)?;
                Ok(HirStatement::Expression(hir_expr))
            }
            StatementKind::Variable(var) => {
                let hir_var = HirVariable {
                    id: self.next_id_variable(),
                    name: var.name.clone(),
                    ty: self
                        .db
                        .get_type(statement.id)
                        .unwrap_or(&var.variable_type)
                        .clone(),
                };

                let initializer = if let Some(init) = &var.initializer {
                    Some(self.lower_expression(init)?)
                } else {
                    None
                };

                self.variables.insert(var.name.clone(), hir_var.clone());

                Ok(HirStatement::Variable {
                    variable: hir_var,
                    initializer: initializer,
                })
            }
            StatementKind::Assignment { left, op, right } => {
                let left = self.get_left_value(left)?;
                let right = self.lower_expression(right)?;

                Ok(HirStatement::Assignment {
                    target: left,
                    op: op.clone(),
                    value: right,
                })
            }
            StatementKind::Return(ret) => {
                let return_value = if let Some(expr) = &ret {
                    Some(self.lower_expression(expr)?)
                } else {
                    None
                };

                Ok(HirStatement::Return {
                    value: return_value,
                })
            }
            StatementKind::IfStatement(if_stmt) => {
                Ok(HirStatement::If(self.lower_if_statement(if_stmt)))
                // let condition = self.lower_expression(&if_stmt.condition).unwrap();
                // let then_branch = self.lower_block(&if_stmt.then_branch);
                // let else_branch = if let Some(else_branch) = &if_stmt.else_branch {
                //     Some(self.lower_else_branch(else_branch))
                // } else {
                //     None
                // };

                // Ok(HirStatement::If(HirIfStatement {
                //     condition: condition,
                //     then_branch: then_branch,
                //     else_branch: else_branch,
                // }))
            }
            StatementKind::WhileStatement(while_stmt) => {
                let condition = self.lower_expression(&while_stmt.condition)?;
                let body = self.lower_block(&while_stmt.body);

                Ok(HirStatement::While {
                    condition: condition,
                    body: Box::new(body),
                })
            }
            // StatementKind::ForStatement(for_stmt) => {}
            // StatementKind::ForInStatement(forin) => {}
            StatementKind::Continue => Ok(HirStatement::Continue),
            StatementKind::Break => Ok(HirStatement::Break),
            _ => Err("Not Support statement".to_string()),
        }
    }

    fn lower_if_statement(&mut self, if_stmt: &IfStatement) -> HirIfStatement {
        let condition = self.lower_expression(&if_stmt.condition).unwrap();
        let then_branch = self.lower_block(&if_stmt.then_branch);
        let else_branch = if let Some(else_branch) = &if_stmt.else_branch {
            Some(self.lower_else_branch(else_branch))
        } else {
            None
        };

        // let stmt = HirStatement::If {
        //     condition,
        //     then_branch: Box::new(then_branch),
        //     else_branch: else_branch,
        // };

        // let mut blocks: Vec<HirStatement> = Vec::new();
        // blocks.push(stmt);

        // HirBlock { statements: blocks }
        HirIfStatement {
            condition: condition,
            then_branch: then_branch,
            else_branch: else_branch,
        }
    }

    fn lower_else_branch(&mut self, else_branch: &ElseBranch) -> HirElseBranch {
        match else_branch {
            ElseBranch::If(if_stmt) => {
                HirElseBranch::If(Box::new(self.lower_if_statement(if_stmt)))
            },
            ElseBranch::Block(block) => {
                HirElseBranch::Block(self.lower_block(block))
            }
            //  self.lower_block(block),
        }
    }

    fn get_left_value(&mut self, left: &Expression) -> Result<HirLValue, String> {
        match &left.kind {
            ExpressionKind::ArrayAccess { array, index } => {
                let arr = self.lower_expression(&array)?;
                let idx = self.lower_expression(&index)?;
                Ok(HirLValue::ArrayAccess {
                    array: Box::new(arr),
                    index: Box::new(idx),
                })
            }
            ExpressionKind::Identifier(id) => {
                self
                    .variables
                    .get(id)
                    .cloned()
                    .ok_or("Variable not found".to_string())
                    .map(|var| HirLValue::Variable(var.id))
            }
            _ => Err("Not Support expression".to_string()),
        }
    }

    fn lower_expression(&mut self, expr: &Expression) -> Result<HirExpression, String> {
        match &expr.kind {
            ExpressionKind::Literal(literal) => {
                if let None = self.db.get_type(expr.id) {
                    println!("Error Literal {:?}", expr);
                }
                Ok(HirExpression::Literal {
                    value: self.ast_literal_to_hir_literal(literal),
                    ty: self.db.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::TemplateString { parts } => {
                let mut acc = HirExpression::Literal {
                    value: HirLiteral::String("".to_string()),
                    ty: self.db.get_type(expr.id).unwrap().clone(),
                };

                for part in parts {
                    let rhs = match part {
                        tahuc_ast::nodes::expressions::TemplatePart::Expression(inner) => {
                            self.lower_expression(inner)?
                        }
                        tahuc_ast::nodes::expressions::TemplatePart::Text(literal) => {
                            HirExpression::Literal {
                                value: HirLiteral::String(literal.to_string()),
                                ty: self.db.get_type(expr.id).unwrap().clone(),
                            }
                        }
                    };

                    // concat: acc = acc + rhs
                    acc = HirExpression::Binary {
                        left: Box::new(acc),
                        op: tahuc_ast::nodes::op::BinaryOp::Add,
                        right: Box::new(rhs),
                        ty: self.db.get_type(expr.id).unwrap().clone(),
                    };
                }

                Ok(acc)
            }
            ExpressionKind::Identifier(id) => {
                if let None = self.db.get_type(expr.id) {
                    println!();
                    println!("id {} not found", expr.id);
                    println!("Error not found {:?}", expr);
                }
                self.variables
                    .get(id)
                    .cloned()
                    .ok_or("Variable not found".to_string())
                    .map(|var| HirExpression::Variable { id: var.id, ty: var.ty })
            }
            ExpressionKind::Ternary {
                condition,
                then,
                otherwise,
            } => {
                let cond = self.lower_expression(&condition)?;
                let then_branch = self.lower_expression(&then)?;
                let else_branch = self.lower_expression(&otherwise)?;

                Ok(HirExpression::Ternary {
                    condition: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
            }
            ExpressionKind::Binary { left, op, right } => {
                let lhs = self.lower_expression(&left)?;
                let rhs = self.lower_expression(&right)?;

                Ok(HirExpression::Binary {
                    left: Box::new(lhs),
                    op: op.clone(),
                    right: Box::new(rhs),
                    ty: self.db.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::Unary { op, operand } => {
                let right = self.lower_expression(&operand)?;

                Ok(HirExpression::Unary {
                    op: op.clone(),
                    operand: Box::new(right),
                    ty: self.db.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::ArrayLiteral { elements } => {
                let mut arr = Vec::new();
                for element in elements {
                    arr.push(self.lower_expression(element)?);
                }

                Ok(HirExpression::ArrayLiteral {
                    elements: arr,
                    ty: self.db.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let arr = self.lower_expression(&array)?;
                let idx = self.lower_expression(&index)?;

                Ok(HirExpression::ArrayAccess {
                    array: Box::new(arr),
                    index: Box::new(idx),
                    ty: self.db.get_type(expr.id).unwrap().clone(),
                })
            }
            // ExpressionKind::MemberAccess { object, member } => {}
            ExpressionKind::FunctionCall(callee) => {
                let mut hir_args: Vec<HirExpression> = Vec::new();

                for arg in &callee.arguments {
                    match arg {
                        Argument::Positional(arg) => {
                            hir_args.push(self.lower_expression(arg)?);
                        }
                        Argument::Named { value, .. } => {
                            hir_args.push(self.lower_expression(value)?);
                        }
                    }
                }

                let fun = match &callee.function.kind {
                    ExpressionKind::Identifier(ident) => {
                        Some(self.functions_map.get(ident).cloned().unwrap())
                    }
                    _ => None,
                };

                Ok(HirExpression::Call {
                    callee: fun.unwrap(),
                    arguments: hir_args,
                    ty: self.db.get_type(callee.function.id).unwrap().clone(),
                })
            }
            // ExpressionKind::Grouping(group) => {}
            _ => Err("Not Support expression".to_string()),
        }
    }

    fn ast_literal_to_hir_literal(&mut self, literal: &Literal) -> HirLiteral {
        match literal {
            Literal::String(str) => HirLiteral::String(str.to_string()),
            Literal::Integer(int) => HirLiteral::Integer(*int),
            Literal::Double(float) => HirLiteral::Float(*float),
            Literal::Boolean(bool) => HirLiteral::Boolean(*bool),
            Literal::Null => HirLiteral::Null,
        }
    }

    pub fn print_debug(&self, hir_module: HirModule) {
        let mut printer = HirPrinter::new();
        printer.print_module(&hir_module);
    }
}
