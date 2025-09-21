use std::collections::HashMap;

use tahuc_ast::{
    nodes::{
        declarations::{DeclarationKind, ExternFn, Function}, expressions::{Argument, ExpressionKind}, op::UnaryOp, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind}, Expression
    }, Module, Type
};
use tahuc_lexer::token::Literal;
use tahuc_semantic::database::Database;
use tahuc_span::FileId;

use crate::{
    hir::*,
    printer::HirPrinter,
};

pub mod hir;
mod printer;

pub struct FnSignature {
    pub name: String,
    pub paramater: Vec<HirParameters>,
    pub param_ty: Vec<Type>,
    pub return_type: Type,
}

trait Visibility {
    fn to_hir(&self) -> HirVisibility;
}

impl Visibility for tahuc_ast::nodes::declarations::Visibility {
    fn to_hir(&self) -> HirVisibility {
        match self {
            tahuc_ast::nodes::declarations::Visibility::Public => HirVisibility::Public,
            tahuc_ast::nodes::declarations::Visibility::Private => HirVisibility::Private,
            tahuc_ast::nodes::declarations::Visibility::Protected => HirVisibility::Private,
        }
    }
}

pub struct Hir<'a> {
    db: &'a mut Database,
    file_id: FileId,
    functions: Vec<HirFunction>,
    extern_functions: Vec<HirExternFunction>,
    variables: HashMap<String, HirVariable>,
    functions_map: HashMap<FileId, HashMap<String, FunctionId>>,
    function_signature: HashMap<FunctionId, FnSignature>,
    function_id: u32,
    variable_id: u32,
    current_file: Option<FileId>,

    pendings_extern_fn: HashMap<FunctionId, String>,
}

impl<'a> Hir<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            file_id: FileId(0),
            functions: vec![],
            extern_functions: vec![],
            variables: HashMap::new(),
            functions_map: HashMap::new(),
            function_signature: HashMap::new(),
            function_id: 0,
            variable_id: 0,
            current_file: None,
            pendings_extern_fn: HashMap::new(),
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

    fn collect_declaration(&mut self, modules: &Vec<Module>) {
        for module in modules {
            for declaration in &module.declaration {
                match &declaration.kind {
                    DeclarationKind::Fn(func) => {
                        let id = self.next_id_function();
                        // self.functions_map.insert(func.kind.name.clone(), function_id);

                        self.add_function(module.file, id, func.kind.name.clone());

                        self.add_signature_func(id, func);
                    }
                    DeclarationKind::Extern(extern_func) => {
                        let id = self.next_id_function();
                        // self.functions_map.insert(extern_func.name.clone(), extern_fn_id);
                        self.add_function(module.file, id, extern_func.name.clone());

                        self.add_signature_ex_func(id, extern_func);
                    }
                    _ => {}
                }
            }
        }
    }

    pub fn to_hir(&mut self, modules: &Vec<Module>) -> Vec<HirModule> {
        // First pass: Register all function names with IDs
        self.collect_declaration(modules);

        let mut hir_modules = Vec::new();

        for module in modules {
            self.file_id = module.file;
            self.current_file = Some(module.file);
            self.clear_state();

            hir_modules.push(self.lowering_module(module));
        }

        hir_modules
    }

    fn clear_state(&mut self) {
        self.functions.clear();
        self.extern_functions.clear();
        self.pendings_extern_fn.clear();
    }

    fn lowering_module(&mut self, module: &Module) -> HirModule {
        for declaration in &module.declaration {
            match &declaration.kind {
                DeclarationKind::Fn(func) => {
                    let fun = self.function_declaration(module.file, func);
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
                        // id: *self.functions_map.get(&extern_func.name).unwrap(),
                        id: *self.functions_map.get(&module.file).unwrap().get(&extern_func.name).unwrap(),
                        name: extern_func.name.clone(),
                        parameters: hir_params,
                        return_type: extern_func.return_type.clone(),
                    };
                    self.extern_functions.push(extern_fn);
                }
                _ => {}
            }
        }

        for (id, _) in &self.pendings_extern_fn {
            println!("pending extern fn {}", id);
            let signature = self.function_signature.get(id).unwrap();
            let extern_fn = HirExternFunction {
                id: *id,
                name: signature.name.clone(),
                parameters: signature.paramater.clone(),
                return_type: signature.return_type.clone(),
            };
            self.extern_functions.push(extern_fn);
        }

        HirModule {
            file_id: module.file,
            functions: self.functions.clone(),
            extern_functions: self.extern_functions.clone(),
        }
    }

    fn function_declaration(&mut self, file_id: FileId, func: &Function) -> HirFunction {
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
            // id: *self.functions_map.get(&func.kind.name).unwrap(), // Use pre-registered ID
            id: self.functions_map.get(&file_id).unwrap().get(&func.kind.name).unwrap().clone(),
            name: func.kind.name.clone(),
            parameters: hir_params,
            body: block,
            return_type: func.kind.return_type.clone(),
            visibility: func.kind.visibility.to_hir(),
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

        self.db.exit_scope();

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
            ExpressionKind::Unary { op, operand } => {
                match op {
                    UnaryOp::Deref => {
                        let operand = self.lower_expression(&operand)?;
                        Ok(HirLValue::Deref(operand))
                    }
                    _ => {
                        Err(format!("Not Support unary expression {:?}", op))
                    }
                }
            }
            _ => {
                Err(format!("Not Support expression {:?}", left.kind))
            },
        }
    }

    fn lower_expression(&mut self, expr: &Expression) -> Result<HirExpression, String> {
        match &expr.kind {
            ExpressionKind::Literal(literal) => {
                if let None = self.get_type(expr.id) {
                    println!("Error Literal {:?}", expr);
                }
                let ty = self.get_type(expr.id).unwrap().clone();
                Ok(HirExpression::Literal {
                    value: self.ast_literal_to_hir_literal(literal, ty.clone()),
                    ty: ty.clone(),
                })
            }
            ExpressionKind::TemplateString { parts } => {
                let mut acc = HirExpression::Literal {
                    value: HirLiteral::String("".to_string()),
                    ty: self.get_type(expr.id).unwrap().clone(),
                };

                for part in parts {
                    let rhs = match part {
                        tahuc_ast::nodes::expressions::TemplatePart::Expression(inner) => {
                            self.lower_expression(inner)?
                        }
                        tahuc_ast::nodes::expressions::TemplatePart::Text(literal) => {
                            HirExpression::Literal {
                                value: HirLiteral::String(literal.to_string()),
                                ty: self.get_type(expr.id).unwrap().clone(),
                            }
                        }
                    };

                    // concat: acc = acc + rhs
                    acc = HirExpression::Binary {
                        left: Box::new(acc),
                        op: tahuc_ast::nodes::op::BinaryOp::Add,
                        right: Box::new(rhs),
                        ty: self.get_type(expr.id).unwrap().clone(),
                    };
                }

                Ok(acc)
            }
            ExpressionKind::Identifier(id) => {
                if let None = self.get_type(expr.id) {
                    println!();
                    println!("HIR: id {} not found", expr.id);
                    println!("HIR: Error not found {:?}", expr);
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
                    ty: self.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::Unary { op, operand } => {
                let right = self.lower_expression(&operand)?;

                Ok(HirExpression::Unary {
                    op: op.clone(),
                    operand: Box::new(right),
                    ty: self.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::ArrayLiteral { elements } => {
                let mut arr = Vec::new();
                for element in elements {
                    arr.push(self.lower_expression(element)?);
                }

                Ok(HirExpression::ArrayLiteral {
                    elements: arr,
                    ty: self.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let arr = self.lower_expression(&array)?;
                let idx = self.lower_expression(&index)?;

                Ok(HirExpression::ArrayAccess {
                    array: Box::new(arr),
                    index: Box::new(idx),
                    ty: self.get_type(expr.id).unwrap().clone(),
                })
            }
            ExpressionKind::MemberAccess { object, member } => {
                let obj = self.lower_expression(&object)?;
                Ok(HirExpression::MemberAccess {
                    object: Box::new(obj),
                    member: member.clone(),
                    ty: self.get_type(expr.id).unwrap().clone(),
                })
            }
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

                match &callee.function.kind {
                    ExpressionKind::Identifier(ident) => {
                        let fid = self.get_function_id(ident.to_string()).unwrap();
                        let signature = self.get_signature(fid);
                        return Ok(HirExpression::Call {
                            callee: fid,
                            arguments: hir_args,
                            signature: FunctionSignature {
                                args: signature.param_ty.clone(),
                            },
                            ty: self.get_type(callee.function.id).unwrap().clone(),
                        });
                    }
                    ExpressionKind::MemberAccess { .. } => {}
                    _ => {}
                }

                let fun = match &callee.function.kind {
                    ExpressionKind::Identifier(ident) => {
                        self.get_function_id(ident.to_string())
                    }
                    _ => None,
                };

                Ok(HirExpression::Call {
                    callee: fun.unwrap(),
                    signature: FunctionSignature {
                        args: Vec::new(),
                    },
                    arguments: hir_args,
                    ty: self.get_type(callee.function.id).unwrap().clone(),
                })
            }
            // ExpressionKind::Grouping(group) => {}
            ExpressionKind::Cast { ty, expression } => {
                let expr = self.lower_expression(&expression)?;

                Ok(HirExpression::Cast {
                    value: Box::new(expr),
                    ty: ty.clone(),
                })
            }
            _ => {

                Err(format!("Not Support expression {:?}", expr.kind))
            },
        }
    }

    fn ast_literal_to_hir_literal(&mut self, literal: &Literal, ty: Type) -> HirLiteral {
        match literal {
            Literal::String(str) => HirLiteral::String(str.to_string()),
            Literal::Char(char) => HirLiteral::Char(*char),
            Literal::Integer(int) => HirLiteral::Integer(*int),
            Literal::Double(float) => HirLiteral::Float(*float),
            Literal::Boolean(bool) => HirLiteral::Boolean(*bool),
            Literal::Null => HirLiteral::Null(ty),
        }
    }

    fn add_function(&mut self, file_id: FileId, id: u32, name: String) {
        self.functions_map.entry(file_id).or_default().insert(name, id);
    }

    fn get_function_id(&mut self, name: String) -> Option<FunctionId> {
        for file_id in self.functions_map.keys() {
            if let Some(id) = self.functions_map.get(file_id).unwrap().get(&name) {
                if let Some(current_file) = self.current_file {
                    if file_id.0 != current_file.0 {
                        self.pendings_extern_fn.insert(*id, name);
                    }
                }
                return Some(*id);
            }
        }
        None
    }

    fn add_signature_func(&mut self, id: u32, func: &Function) {
        let params: Vec<HirParameters> = func
            .kind
            .parameters
            .iter()
            .map(|p| 
                HirParameters {
                    id: 0,
                    name: p.kind.name.clone(),
                    ty: p.kind.r#type.clone(),
                }
            )
            .collect();
        self.add_signature(id, FnSignature {
            name: func.kind.name.clone(),
            paramater: params.clone(),
            param_ty: params.iter().map(|p| p.ty.clone()).collect(),
            return_type: func.kind.return_type.clone(),
        });
    }

    fn add_signature_ex_func(&mut self, id: u32, extern_fn: &ExternFn) {
        let params: Vec<HirParameters> = extern_fn
            .parameters
            .iter()
            .map(|p|
                HirParameters {
                    id: 0,
                    name: p.kind.name.clone(),
                    ty: p.kind.r#type.clone(),
                }
            )
            .collect();
        self.add_signature(id, FnSignature {
            name: extern_fn.name.clone(),
            paramater: params.clone(),
            param_ty: params.iter().map(|p| p.ty.clone()).collect(),
            return_type: extern_fn.return_type.clone(),
        });
    }

    fn add_signature(&mut self, id: u32, signature: FnSignature) {
        self.function_signature.insert(id, signature);
    }

    fn get_signature(&mut self, id: u32) -> &FnSignature {
        self.function_signature.get(&id).unwrap()
    }

    fn get_type(&mut self, id: u32) -> Option<&Type> {
        self.db.get_type(self.file_id, id)
    }

    pub fn print_debug(&self, hir_modules: &Vec<HirModule>) {
        for module in hir_modules {
            let mut printer = HirPrinter::new();
            printer.print_module(module);
        }
    }
}
