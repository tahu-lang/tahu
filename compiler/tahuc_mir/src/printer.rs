use tahuc_ast::nodes::op::{BinaryOp, UnaryOp};
use tahuc_hir::hir::FunctionId;

use crate::mir::{block::{MirBasicBlock, MirTerminator}, function::{MirExternFunction, MirFunction}, instruction::{MirInstruction, MirOperand}, ty::{MirConstant, MirType}, *};

pub struct MirPrinter<'a> {
    module: &'a MirModule,
    current_function: Option<&'a MirFunction>,
}

pub fn print_mir(module: &MirModule) {
    let printer = MirPrinter::new(module);
    println!("MIR printer");
    println!("{}", printer.print_module());
    println!("==================");
}

pub fn mir_to_string(module: &MirModule) -> String {
    let printer = MirPrinter::new(module);
    printer.print_module()
}

impl<'a> MirPrinter<'a> {
    pub fn new(module: &'a MirModule) -> Self {
        Self {
            module,
            current_function: None,
        }
    }

    pub fn print_module(&self) -> String {
        let mut output = String::new();

        // print global type
        for (id, ty) in &self.module.globals {
            output.push_str(&format!("declare {} @{}", ty, id));
            output.push('\n');
        }

        if !self.module.globals.is_empty() {
            output.push('\n');
        }

        // Print external functions
        for extern_func in &self.module.extern_functions {
            output.push_str(&self.print_extern_function(extern_func));
            output.push('\n');
        }

        if !self.module.extern_functions.is_empty() {
            output.push('\n');
        }

        // Print functions
        for function in &self.module.functions {
            output.push_str(&self.print_function(function));
            output.push_str("\n\n");
        }

        output
    }

    fn print_extern_function(&self, extern_func: &MirExternFunction) -> String {
        format!(
            "declare {} @{}({})",
            &extern_func.return_type,
            extern_func.name,
            extern_func
                .parameters
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(""),
        )
    }

    pub fn print_function(&self, function: &'a MirFunction) -> String {
        let mut output = String::new();

        // Function signature
        output.push_str(&format!(
            "define {} @{}({}) {{\n",
            &function.return_type,
            function.name,
            function
                .parameters
                .iter()
                .map(|param| format!(
                    "{} %{}",
                    &param.ty,
                    param.name
                ))
                .collect::<Vec<_>>()
                .join(", ")
        ));

        // Set current function for value printing
        let printer = MirPrinter {
            module: self.module,
            current_function: Some(function),
        };

        // // Print locals (optional, for debugging)
        // if !function.locals.is_empty() {
        //     output.push_str("  ; Locals:\n");
        //     for (local_id, ty) in &function.locals {
        //         if !function.parameters.iter().any(|p| p.id == *local_id) {
        //             output.push_str(&format!("  ;   %{}: {}\n", local_id, ty));
        //         }
        //     }
        // }

        // Print basic blocks in order
        for &block_id in &function.block_order {
            if let Some(block) = function.basic_blocks.get(&block_id) {
                output.push_str(&printer.print_basic_block(block));
                output.push('\n');
            }
        }

        output.push_str("}\n");
        output
    }

    fn print_basic_block(&self, block: &MirBasicBlock) -> String {
        let mut output = String::new();

        // Block label
        output.push_str(&format!("{}:\n", block.name));

        // Print predecessors (optional, for debugging)
        if !block.predecessors.is_empty() {
            output.push_str(&format!(
                "  ; Predecessors: {}\n",
                block
                    .predecessors
                    .iter()
                    .map(|pred_id| self.print_block_name(*pred_id))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        // Print instructions
        for instruction in &block.instructions {
            output.push_str(&self.print_instruction(instruction));
            output.push_str("\n");
        }

        // Print terminator
        output.push_str(&self.print_terminator(&block.terminator));
        
        output
    }

    fn print_instruction(&self, instruction: &MirInstruction) -> String {
        match instruction {
            // MirInstruction::Assign { target, source, ty } => {
            //     format!(
            //         "  %{} = {} {}",
            //         target,
            //         if matches!(*ty, MirType::Void) { "store" } else { "assign" },
            //         self.print_value(source)
            //     )
            // }
            MirInstruction::Alloca { target, ty } => {
                format!(
                    "  %{} = alloca {}",
                    target,
                    ty,
                )
            }
            MirInstruction::Load { target, ptr, ty } => {
                format!(
                    "  %{} = load {}, {}",
                    target,
                    ty,
                    self.print_value(ptr)
                )
            }
            MirInstruction::Store { ptr, value, ty } => {
                format!(
                    "  store {} {}, {}",
                    ty,
                    self.print_value(value),
                    self.print_value(ptr)
                )
            }
            MirInstruction::GetElementPtr { target, base, indices, base_ty, inner_ty } => {
                let indices_str = indices
                    .iter()
                    .map(|index| {
                        let index_ty = self.get_value_type(index);
                        format!("{} {}", &index_ty, self.print_value(index))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                
                format!("  %{} = getelementptr {}, {}* {}, {}",
                    target,
                    base_ty,
                    inner_ty, // The element type
                    self.print_value(base),
                    indices_str
                )
            }
            MirInstruction::Binary { target, left, right, op, .. } => {
                let op_str = match op {
                    BinaryOp::Add => "add",
                    BinaryOp::Sub => "sub",
                    BinaryOp::Mul => "mul",
                    BinaryOp::Div => "div",
                    BinaryOp::Rem => "mod",
                    BinaryOp::Eq => "eq",
                    BinaryOp::Ne => "ne",
                    BinaryOp::Lt => "lt",
                    BinaryOp::Le => "le",
                    BinaryOp::Gt => "gt",
                    BinaryOp::Ge => "ge",
                    BinaryOp::And => "and",
                    BinaryOp::Or => "or",
                    _ => &format!("; unsupported binary op {:?}", op),
                };

                let left_ty = self.get_value_type(left);
                // let right_ty = self.get_value_type(right);

                format!("  %{} = {} {} {}, {}", 
                    target,
                    op_str,
                    &left_ty,
                    self.print_value(left),
                    self.print_value(right)
                )
            }
            MirInstruction::Unary { target, op, value, .. } => {
                let op_str = match op {
                    UnaryOp::Minus => "neg",
                    UnaryOp::Not => "not",
                    UnaryOp::AddressOf => "address of",
                    UnaryOp::Deref => "deref",
                    _ => &format!("; unsupported unary op {:?}", op),
                };

                let value_ty = self.get_value_type(value);

                format!("  %{} = {} {} {}", 
                    target,
                    op_str,
                    &value_ty,
                    self.print_value(value)
                )
            }
            MirInstruction::Call { target, function, arguments, .. } => {
                let target_str = format!("%{} = ", target);

                let return_ty = self.get_local_type(*target).unwrap_or(&MirType::Unit);

                let args_str = arguments
                    .iter()
                    .map(|arg| {
                        let arg_ty = self.get_value_type(arg);
                        format!("{} {}", &arg_ty, self.print_value(arg))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("  {}call {} @{}({})", 
                    target_str,
                    return_ty,
                    self.get_function_name(*function),
                    args_str
                )
            }
            _ => {
                format!("  ; unsupported instruction {:?}", instruction)
            }
        }
    }

    fn print_terminator(&self, terminator: &MirTerminator) -> String {
        match terminator {
            MirTerminator::Return { value } => {
                if let Some(val) = value {
                    let val_ty = self.get_value_type(val);
                    format!("  ret {} {}", 
                        &val_ty,
                        self.print_value(val)
                    )
                } else {
                    "  ret void".to_string()
                }
            }
            MirTerminator::Jump { target } => {
                format!("  br label %{}", self.print_block_name(*target))
            }
            MirTerminator::Branch { condition, true_target, false_target } => {
                format!("  br i1 {}, label %{}, label %{}", 
                    self.print_value(condition),
                    self.print_block_name(*true_target),
                    self.print_block_name(*false_target)
                )
            }
            MirTerminator::Unreachable => {
                "  unreachable".to_string()
            }
        }
    }

    fn print_value(&self, value: &MirOperand) -> String {
        match value {
            MirOperand::Local(local_id) => format!("%{}", local_id),
            MirOperand::Constant(constant) => self.print_constant(constant),
        }
    }

    fn get_value_type(&self, value: &MirOperand) -> MirType {
        match value {
            MirOperand::Local(local_id) => self.get_local_type(*local_id).unwrap_or(&MirType::Unit).clone(),
            MirOperand::Constant(constant) => match constant {
                MirConstant::String(_) => MirType::String,
                MirConstant::Char(_) => MirType::Char,
                MirConstant::Int { ty, .. } => ty.clone(), // Default to i32 for integers
                MirConstant::Float { ty, .. } => ty.clone(),
                MirConstant::Bool(_) => MirType::Bool,
                MirConstant::Null(ty) => ty.clone(),
            },
        }
    }

    fn get_local_type(&self, local_id: LocalId) -> Option<&MirType> {
        self.current_function.and_then(|func| func.locals.get(&local_id))
    }

    fn get_function_name(&self, function_id: FunctionId) -> String {
        if let Some(func) = self.module.functions.iter().find(|f| f.id == function_id) {
            func.name.clone()
        } else if let Some(ext_func) = self.module.extern_functions.iter().find(|f| f.id == function_id) {
            ext_func.name.clone()
        } else {
            format!("unknown_function_{}", function_id)
        }
    }

    fn print_block_name(&self, block_id: BasicBlockId) -> String {
        if let Some(function) = self.current_function {
            if let Some(block) = function.basic_blocks.get(&block_id) {
                return block.name.clone();
            }
        }
        format!("block_{}", block_id)
    }

    fn print_constant(&self, constant: &MirConstant) -> String {
        match constant {
            MirConstant::String(s) => format!("\"{}\"", s.escape_default()),
            MirConstant::Char(c) => format!("'{}'", c.escape_default()),
            MirConstant::Int { value, ..} => value.to_string(),
            MirConstant::Float { value, ..} => value.to_string(),
            MirConstant::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            MirConstant::Null(ty) => format!("null {}", ty),
        }
    }
}