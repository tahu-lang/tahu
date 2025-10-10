use std::rc::Rc;

use tahuc_ast::{nodes::{ast::NodeId, declarations::Visibility}, ty::Type};
use tahuc_span::{FileId, Span, StrId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: StrId,
    pub file_id: FileId,
    pub visibility: Visibility,
    pub span: Span,
    pub kind: SymbolKind,
}

pub trait SymbolTrait: Clone {
    fn into_symbol(self) -> Symbol;
}

impl Symbol {
    pub fn get_struct(&self) -> Option<&StructSymbol> {
        match &self.kind {
            SymbolKind::Struct(struct_symbol) => {
                return Some(struct_symbol);
            }
            _ => None,
        }
    }

    pub fn get_struct_mut(&mut self) -> Option<&mut StructSymbol> {
        if let SymbolKind::Struct(v) = &mut self.kind {
            return Some(v);
        } else {
            None
        }
    }

    pub fn get_function(&self) -> Option<&FunctionSymbol> {
        match &self.kind {
            SymbolKind::Function(func) => {
                return Some(func);
            }
            _ => None,
        }
    }

    pub fn get_function_mut(&mut self) -> Option<&mut FunctionSymbol> {
        if let SymbolKind::Function(v) = &mut self.kind {
            return Some(v);
        } else {
            None
        }
    }

    pub fn get_variable(&self) -> Option<&VariableSymbol> {
        match &self.kind {
            SymbolKind::Variable(var) => {
                return Some(var);
            }
            _ => None,
        }
    }

    pub fn get_variable_mut(&mut self) -> Option<&mut VariableSymbol> {
        if let SymbolKind::Variable(v) = &mut self.kind {
            return Some(v);
        } else {
            None
        }
    }

    pub fn update_struct<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut StructSymbol),
    {
        if let SymbolKind::Struct(struct_symbol) = &mut self.kind {
            f(struct_symbol);
        }
    }

    pub fn update_function<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut FunctionSymbol),
    {
        if let SymbolKind::Function(func) = &mut self.kind {
            f(func);
        }
    }

    pub fn update_variable<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut VariableSymbol),
    {
        if let SymbolKind::Variable(var) = &mut self.kind {
            f(var);
        }
    }

    pub fn get_type(&self) -> Rc<Type> {
        match &self.kind {
            SymbolKind::Struct(struct_symbol) => {
                struct_symbol.ty.clone()
            }
            SymbolKind::Function(func) => {
                func.return_type.clone()
            }
            SymbolKind::Variable(var) => {
                var.final_type.clone()
            }
        }
    }

    pub fn is_struct(&self) -> bool {
        match &self.kind {
            SymbolKind::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match &self.kind {
            SymbolKind::Function(_) => true,
            _ => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        match &self.kind {
            SymbolKind::Variable(_) => true,
            _ => false,
        }
    }
}

impl From<Symbol> for SymbolKind {
    fn from(value: Symbol) -> Self {
        match value.kind {
            SymbolKind::Struct(struct_symbol) => {
                return SymbolKind::Struct(struct_symbol);
            }
            SymbolKind::Function(func) => {
                return SymbolKind::Function(func);
            }
            SymbolKind::Variable(var) => {
                return SymbolKind::Variable(var);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Struct(StructSymbol),
    Function(FunctionSymbol),
    Variable(VariableSymbol),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructSymbol {
    pub file_id: FileId,
    pub id: NodeId,
    pub name: StrId,
    pub fields: Vec<StructFieldSymbol>,
    pub visibility: Visibility,
    pub ty: Rc<Type>,
    pub span: Span,
}

impl StructSymbol {
    pub fn update_field<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut StructFieldSymbol),
    {
        for field in &mut self.fields {
            f(field);
        }
    }

    pub fn update_field_at(&mut self, index: usize, mut f: impl FnMut(&mut StructFieldSymbol)) {
        f(&mut self.fields[index]);
    }
}

impl From<&StructSymbol> for Symbol {
    fn from(value: &StructSymbol) -> Self {
        Symbol {
            name: value.name,
            file_id: value.file_id,
            visibility: value.visibility,
            span: value.span,
            kind: SymbolKind::Struct(value.clone()),
        }
    }
}

impl From<StructSymbol> for Symbol {
    fn from(value: StructSymbol) -> Self {
        Symbol {
            name: value.name,
            file_id: value.file_id,
            visibility: value.visibility,
            span: value.span,
            kind: SymbolKind::Struct(value),
        }
    }
}

impl SymbolTrait for StructSymbol {
    fn into_symbol(self) -> Symbol {
        self.into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructFieldSymbol {
    pub id: NodeId,
    pub name: StrId,
    pub r#type: Rc<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSymbol {
    // default information
    pub file_id: FileId,
    pub id: NodeId,
    pub name: StrId, // identifier for function
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Rc<Type>,
    pub visibility: Visibility,
    pub span: Span,
    // for now skip type inference
    // /// mark if type is Type::Inference
    // pub is_inferred: bool,
    // pub return_type_inferred: Option<Type>,
}

impl FunctionSymbol {
    pub fn update_parameter<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut ParameterInfo),
    {
        for param in &mut self.parameters {
            f(param);
        }
    }

    pub fn update_parameter_at(&mut self, index: usize, mut f: impl FnMut(&mut ParameterInfo)) {
        f(&mut self.parameters[index]);
    }
}

impl From<&FunctionSymbol> for Symbol {
    fn from(value: &FunctionSymbol) -> Self {
        Symbol {
            name: value.name,
            file_id: value.file_id,
            visibility: value.visibility,
            span: value.span,
            kind: SymbolKind::Function(value.clone()),
        }
    }
}

impl From<FunctionSymbol> for Symbol {
    fn from(value: FunctionSymbol) -> Self {
        Symbol {
            name: value.name,
            file_id: value.file_id,
            visibility: value.visibility,
            span: value.span,
            kind: SymbolKind::Function(value),
        }
    }
}

impl SymbolTrait for FunctionSymbol {
    fn into_symbol(self) -> Symbol {
        self.into()
    }
}

/// for now just static typing
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterInfo {
    pub id: NodeId,
    pub name: StrId,
    pub var_type: Rc<Type>,
    pub span: Span,
    // // type inference
    // pub is_inferred: bool,
    // pub inferred_type: Option<Type>, // update after type checking
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableSymbol {
    pub file_id: FileId,
    pub id: NodeId,
    pub name: StrId,
    pub declared_type: Rc<Type>,
    pub initializer: Option<NodeId>,
    pub visibility: Visibility,

    pub is_mutable: bool,

    /// mark if type is Type::Inference
    /// use
    /// ```no run
    /// Type.is_inferred()
    /// ```
    pub need_inferred: bool,

    /// update after type checking
    pub inferred_type: Option<Type>,

    /// final type resolved type
    pub final_type: Rc<Type>,

    pub span: Span,
}

impl From<&VariableSymbol> for Symbol {
    fn from(value: &VariableSymbol) -> Self {
        Symbol {
            name: value.name,
            file_id: value.file_id,
            visibility: value.visibility,
            span: value.span,
            kind: SymbolKind::Variable(value.clone()),
        }
    }
}

impl From<VariableSymbol> for Symbol {
    fn from(value: VariableSymbol) -> Self {
        Symbol {
            name: value.name,
            file_id: value.file_id,
            visibility: value.visibility,
            span: value.span,
            kind: SymbolKind::Variable(value),
        }
    }
}

impl SymbolTrait for VariableSymbol {
    fn into_symbol(self) -> Symbol {
        self.into()
    }
}
