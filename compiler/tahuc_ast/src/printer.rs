use crate::nodes::{
    ast::AstNode, declarations::{Declaration, DeclarationKind, Parameter, StructField}, expressions::{Argument, Expression, ExpressionKind}, op::{AssignmentOp, BinaryOp, UnaryOp}, statements::{Block, ElseBranch, IfStatement, Statement, StatementKind, Variable}
};
use tahuc_lexer::token::Literal;

pub struct TreePrinter {
    is_last: Vec<bool>,
}

impl TreePrinter {
    pub fn new() -> Self {
        Self {
            is_last: Vec::new(),
        }
    }

    fn get_prefix(&self) -> String {
        let mut prefix = String::new();

        for (i, &is_last) in self.is_last.iter().enumerate() {
            if i == self.is_last.len() - 1 {
                if is_last {
                    prefix.push_str("└── ");
                } else {
                    prefix.push_str("├── ");
                }
            } else {
                if is_last {
                    prefix.push_str("    ");
                } else {
                    prefix.push_str("│   ");
                }
            }
        }

        prefix
    }

    fn print_node(&mut self, name: &str) {
        println!("{}{}", self.get_prefix(), name);
    }

    fn with_child<F>(&mut self, is_last: bool, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.is_last.push(is_last);
        f(self);
        self.is_last.pop();
    }

    pub fn print_module(&mut self, declarations: &[Declaration]) {
        println!("Module");

        if !declarations.is_empty() {
            self.with_child(true, |printer| {
                printer.print_node("Declarations");
                printer.print_declarations(declarations);
            });
        }
    }

    fn print_declarations(&mut self, declarations: &[Declaration]) {
        for (i, decl) in declarations.iter().enumerate() {
            let is_last = i == declarations.len() - 1;
            self.with_child(is_last, |printer| {
                printer.print_declaration(decl);
            });
        }
    }

    fn print_variable(&mut self, var: &Variable) {
        let mutability = if var.is_mutable { "mut " } else { "" };
        let type_info = var.variable_type.clone();

        self.print_node(&format!(
            "Variable: {}{} {}",
            mutability, var.name, type_info
        ));

        if let Some(init) = &var.initializer {
            self.with_child(true, |printer| {
                printer.print_node("Initializer");
                printer.with_child(true, |printer| {
                    printer.print_expression(init);
                });
            });
        }
    }

    fn print_declaration(&mut self, decl: &Declaration) {
        match &decl.kind {
            DeclarationKind::Struct(struct_decl) => {
                self.print_node(&format!("Struct: {}", struct_decl.name));
                self.with_child(true, |printer| {
                    printer.print_node("Fields");
                    printer.print_struct_fields(&struct_decl.fields);
                });
            }
            DeclarationKind::Fn(func) => {
                self.print_node(&format!("Function: {} -> {}", func.kind.name, func.kind.return_type));
                self.with_child(false, |printer| {
                    printer.print_node("Parameters");
                    printer.print_parameters(&func.kind.parameters);
                });

                self.with_child(true, |printer| {
                    printer.print_node("Body");
                    printer.print_block(&func.kind.body);
                });
            }
            DeclarationKind::Variable(var) => self.print_variable(var),
            DeclarationKind::Extern(extern_func) => {
                self.print_node(&format!("Extern Function: {} -> {}", extern_func.name, extern_func.return_type));
                self.with_child(true, |printer| {
                    printer.print_node("Parameters");
                    printer.print_parameters(&extern_func.parameters);
                });
            }
            _ => {
                self.print_node("Unknown Declaration");
            }
        }
    }

    fn print_struct_fields(&mut self, fields: &[AstNode<StructField>]) {
        for (i, field) in fields.iter().enumerate() {
            let is_last = i == fields.len() - 1;
            self.with_child(is_last, |printer| {
                printer.print_node(&format!(
                    "{} : {}",
                    field.kind.name, field.kind.r#type
                ));
            });
        }
    }

    fn print_block(&mut self, block: &Block) {
        if block.statements.is_empty() {
            self.with_child(true, |printer| {
                printer.print_node("(empty block)");
            });
            return;
        }

        for (i, stmt) in block.statements.iter().enumerate() {
            let is_last = i == block.statements.len() - 1;
            self.with_child(is_last, |printer| {
                printer.print_statement(stmt);
            });
        }
    }

    fn print_if_statement(&mut self, if_stmt: &IfStatement) {
        self.print_node("If Statement");

        self.with_child(false, |printer| {
            printer.print_node("Condition");
            printer.with_child(true, |printer| {
                printer.print_expression(&if_stmt.condition);
            });
        });

        let is_else = if_stmt.else_branch.is_none();

        self.with_child(is_else, |printer| {
            printer.print_node("Then");
            printer.print_block(&if_stmt.then_branch);
        });

        if let Some(else_branch) = &if_stmt.else_branch {
            self.with_child(true, |printer| {
                printer.print_node("Else");
                printer.with_child(true, |printer| {
                    printer.print_else_branch(else_branch);
                });
            });
        }
    }

    fn print_else_branch(&mut self, else_branch: &ElseBranch) {
        match else_branch {
            ElseBranch::Block(block) => {
                self.print_node("Block");
                self.print_block(block);
            }
            ElseBranch::If(if_stmt) => {
                self.print_if_statement(if_stmt);
            }
        }
    }

    fn print_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::Expression(expr) => {
                self.print_node("Expression Statement");
                self.with_child(true, |printer| {
                    printer.print_expression(expr);
                });
            }
            StatementKind::Variable(var) => self.print_variable(var),
            StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.print_node("Return Statement");

                    self.with_child(true, |printer| {
                        printer.print_node("Expression");
                        printer.with_child(true, |printer| {
                            printer.print_expression(expr);
                        });
                    });
                } else {
                    self.print_node("Return");
                }
            }
            StatementKind::IfStatement(if_stmt) => {
                self.print_if_statement(if_stmt);
            }
            StatementKind::Assignment { left, op, right } => {
                self.print_node("Assignment Statement");

                self.with_child(false, |printer| {
                    printer.print_node("Left");
                    printer.with_child(true, |printer| {
                        printer.print_expression(left);
                    });
                });

                self.with_child(false, |printer| {
                    printer.print_node("Operator");
                    printer.with_child(true, |printer| {
                        printer.print_node(&format!("{}", printer.format_assigment_op(op)));
                    });
                });
                    

                self.with_child(true, |printer| {
                    printer.print_node("Right");
                    printer.with_child(true, |printer| {
                        printer.print_expression(right);
                    });
                });
            }
            _ => {
                self.print_node("Unknown Statement");
            }
        }
    }

    fn print_expression(&mut self, expr: &Expression) {
        match &expr.kind {
            ExpressionKind::Literal(lit) => {
                self.print_node(&format!("Literal: {}", self.format_literal(lit)));
            }
            ExpressionKind::Identifier(name) => {
                self.print_node(&format!("Identifier: {}", name));
            }
            ExpressionKind::Binary { left, op, right } => {
                self.print_node(&format!("Binary Operation: {}", self.format_binary_op(op)));

                self.with_child(false, |printer| {
                    printer.print_node("Left");
                    printer.with_child(true, |printer| {
                        printer.print_expression(left);
                    });
                });

                self.with_child(true, |printer| {
                    printer.print_node("Right");
                    printer.with_child(true, |printer| {
                        printer.print_expression(right);
                    });
                });
            }
            ExpressionKind::Unary { op, operand } => {
                self.print_node(&format!("Unary Operation: {}", self.format_unary_op(op)));
                self.with_child(true, |printer| {
                    printer.print_node("Operand");
                    printer.with_child(true, |printer| {
                        printer.print_expression(operand);
                    });
                });
            }
            ExpressionKind::FunctionCall(call) => {
                self.print_node("Function Call");

                self.with_child(false, |printer| {
                    printer.print_node("Function");
                    printer.with_child(true, |printer| {
                        printer.print_expression(&call.function);
                    });
                });

                self.with_child(true, |printer| {
                    if !call.arguments.is_empty() {
                        printer.print_node("Arguments");
                        printer.print_arguments(&call.arguments);
                    } else {
                        printer.print_node("Arguments: (none)");
                    }
                });
            }
            ExpressionKind::Grouping(exprs) => {
                self.print_node("Grouping");
                self.with_child(true, |printer| {
                    printer.print_node("Expressions");
                    printer.print_expression(exprs);
                });
            }
            ExpressionKind::MemberAccess { object, member } => {
                self.print_node("Member Access");
                self.with_child(false, |printer| {
                    printer.print_expression(&object);
                });
                self.with_child(true, |printer| {
                    printer.print_node(&format!("Member: {}", member));
                });
            }
            ExpressionKind::ArrayLiteral { elements } => {
                self.print_node("Array Literal");
                self.with_child(true, |printer| {
                    printer.print_node("Elements");
                    for (i, expr) in elements.iter().enumerate() {
                        let is_last = i == elements.len() - 1;
                        printer.with_child(is_last, |printer| {
                            printer.print_expression(expr);
                        });
                    }
                });
            }
            ExpressionKind::ArrayAccess { array, index } => {
                self.print_node("Array Access");

                // Array object
                self.with_child(false, |printer| {
                    printer.print_expression(array);
                });

                // Index
                self.with_child(true, |printer| {
                    printer.print_node("Index");
                    printer.with_child(true, |printer| {
                        printer.print_expression(index);
                    });
                });
            }
            ExpressionKind::Cast { ty, expression } => {
                self.print_node("Cast");
                self.with_child(false, |printer| {
                    printer.print_node(&format!("Type: {}", ty));
                });
                self.with_child(true, |printer| {
                    printer.print_node("Expression");
                    printer.with_child(true, |printer| {
                        printer.print_expression(expression);
                    });
                });
            }
            ExpressionKind::StructLiteral { object, fields } => {
                self.print_node("Struct Literal");
                self.with_child(false, |printer| {
                    printer.print_node("Object");
                    printer.with_child(true, |printer| {
                        printer.print_expression(object);
                    });
                });
                self.with_child(true, |printer| {
                    printer.print_node("Fields");
                    for (i, field) in fields.iter().enumerate() {
                        let is_last = i == fields.len() - 1;
                        printer.with_child(is_last, |printer| {
                            printer.print_expression(&field.kind.name);
                            if let Some(value) = &field.kind.value {
                                printer.with_child(true, |printer| {
                                    printer.print_expression(value);
                                });
                            }
                        });
                    }
                });
            }
            _ => {
                println!("{:?}", expr.kind);
                self.print_node("Unknown Expression");
            }
        }
    }

    fn print_arguments(&mut self, args: &[Argument]) {
        for (i, arg) in args.iter().enumerate() {
            let is_last = i == args.len() - 1;
            self.with_child(is_last, |printer| match arg {
                Argument::Positional(expr) => {
                    printer.print_node("Positional Argument");
                    printer.with_child(true, |printer| {
                        printer.print_expression(expr);
                    });
                }
                Argument::Named { name, value, .. } => {
                    printer.print_node(&format!("Named Argument: {}", name));
                    printer.with_child(true, |printer| {
                        printer.print_expression(value);
                    });
                }
            });
        }
    }

    fn print_parameters(&mut self, params: &[Parameter]) {
        for (i, param) in params.iter().enumerate() {
            let is_last = i == params.len() - 1;
            let default_value = param.kind.default.as_ref();
            self.with_child(is_last, |printer| {
                printer.print_node(&format!("Parameter: {}", param.kind.name));
                if let Some(expr) = default_value {
                    printer.with_child(true, |printer| {
                        printer.print_node(&format!("Default Value"));
                        printer.with_child(true, |printer| {
                            printer.print_expression(expr);
                        });
                    });
                }
            });
        }
    }

    fn format_literal(&self, lit: &Literal) -> String {
        match lit {
            Literal::String(s) => s.to_string(),
            Literal::Char(c) => c.to_string(),
            Literal::Integer(i) => i.to_string(),
            Literal::Double(f) => f.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Null => "null".to_string(),
            // Literal::Undefined => "undefined".to_string(),
        }
    }

    fn format_assigment_op(&self, op: &AssignmentOp) -> String {
        match op {
            AssignmentOp::Assign => "=".to_string(),
            AssignmentOp::AddAssign => "+=".to_string(),
            AssignmentOp::SubAssign => "-=".to_string(),
            AssignmentOp::MulAssign => "*=".to_string(),
            AssignmentOp::DivAssign => "/=".to_string(),
            AssignmentOp::RemAssign => "%=".to_string(),
            AssignmentOp::BitAndAssign => "&=".to_string(),
            AssignmentOp::BitOrAssign => "|=".to_string(),
            AssignmentOp::BitXorAssign => "^=".to_string(),
            AssignmentOp::ShlAssign => "<<=".to_string(),
            AssignmentOp::ShrAssign => ">>=".to_string(),
        }
    }

    fn format_binary_op(&self, op: &BinaryOp) -> String {
        match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Rem => "%".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
            BinaryOp::BitXor => "^".to_string(),
            BinaryOp::BitAnd => "&".to_string(),
            BinaryOp::BitOr => "|".to_string(),
            BinaryOp::Shl => "<<".to_string(),
            BinaryOp::Shr => ">>".to_string(),
            BinaryOp::Eq => "==".to_string(),
            BinaryOp::Lt => "<".to_string(),
            BinaryOp::Le => "<=".to_string(),
            BinaryOp::Ne => "!=".to_string(),
            BinaryOp::Ge => ">=".to_string(),
            BinaryOp::Gt => ">".to_string(),
        }
    }

    fn format_unary_op(&self, op: &UnaryOp) -> String {
        match op {
            UnaryOp::Minus => "-".to_string(),
            UnaryOp::Plus => "+".to_string(),
            UnaryOp::Not => "!".to_string(),
            UnaryOp::BitNot => "~".to_string(),
            UnaryOp::PostIncrement => "++".to_string(),
            UnaryOp::PostDecrement => "--".to_string(),
            UnaryOp::PreIncrement => "++".to_string(),
            UnaryOp::PreDecrement => "--".to_string(),
            UnaryOp::Deref => "*".to_string(),
            UnaryOp::AddressOf => "&".to_string(),
        }
    }
}

// Convenience functions for easy usage
pub fn print_ast(declarations: &[Declaration]) {
    let mut printer = TreePrinter::new();
    printer.print_module(declarations);
}

pub fn print_expression_tree(expr: &Expression) {
    let mut printer = TreePrinter::new();
    println!("Expression");
    printer.with_child(true, |printer| {
        printer.print_expression(expr);
    });
}

pub fn print_statement_tree(stmt: &Statement) {
    let mut printer = TreePrinter::new();
    println!("Statement");
    printer.with_child(true, |printer| {
        printer.print_statement(stmt);
    });
}
