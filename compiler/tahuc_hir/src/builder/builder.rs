use std::{collections::HashMap, path::PathBuf, rc::Rc};

use tahuc_ast::{nodes::{ast::NodeId, declarations::{DeclarationKind, ExternFn, Function}}, ty::Type, Module};
use tahuc_module::{config::Config, resolver::ModuleResult};
use tahuc_semantic::database::{database::Database, scope::ScopeManager, symbol::Symbol};
use tahuc_span::{FileId, StrId};

use crate::{hir::{FunctionId, HirExternFunction, HirField, HirFunction, HirModule, HirParameters, HirStruct, HirVariable, HirVisibility}, printer::HirPrinter};

pub struct FnSignature {
    pub name: StrId,
    pub paramater: Vec<HirParameters>,
    pub param_ty: Vec<Rc<Type>>,
    pub return_type: Rc<Type>,
}

trait Visibility {
    fn to_hir(&self) -> HirVisibility;
}

impl Visibility for tahuc_ast::nodes::declarations::Visibility {
    fn to_hir(&self) -> HirVisibility {
        match self {
            tahuc_ast::nodes::declarations::Visibility::Public => HirVisibility::Public,
            tahuc_ast::nodes::declarations::Visibility::Private => HirVisibility::Private,
        }
    }
}



pub struct HirBuilder<'a> {
    pub(crate) config: &'a Config<'a>,
    pub(crate) db: &'a mut Database,
    pub(crate) scope: ScopeManager,
    files: Vec<FileId>,
    pub(crate) file_id: FileId,

    // cross file
    pub(crate) structs_map: HashMap<FileId, HashMap<StrId, HirStruct>>,
    pub(crate) functions_map: HashMap<FileId, HashMap<StrId, FunctionId>>,
    // variable_map: HashMap<FileId, HashMap<String, VariableId>>,

    // scope file
    structs: Vec<HirStruct>,
    functions: Vec<HirFunction>,
    extern_functions: Vec<HirExternFunction>,

    function_signature: HashMap<FunctionId, FnSignature>,

    pub(crate) variables: HashMap<usize, HashMap<StrId, HirVariable>>,

    struct_id: u32,
    function_id: u32,
    variable_id: u32,
    current_file: Option<FileId>,

    pendings_extern_fn: HashMap<FunctionId, StrId>,
}

impl<'a> HirBuilder<'a> {
    pub fn new(config: &'a Config, db: &'a mut Database) -> Self {
        Self {
            config,
            db,
            scope: ScopeManager::new(),
            files: vec![],
            file_id: FileId(0),

            // cross file
            structs_map: HashMap::new(),
            functions_map: HashMap::new(),
            // variable_map: HashMap::new(),

            // scope file
            structs: vec![],
            functions: vec![],
            extern_functions: vec![],
            variables: HashMap::new(),
            function_signature: HashMap::new(),

            struct_id: 0,
            function_id: 0,
            variable_id: 0,
            current_file: None,
            pendings_extern_fn: HashMap::new(),
        }
    }

    fn next_id_struct(&mut self) -> u32 {
        let id = self.struct_id;
        self.struct_id += 1;
        id
    }

    fn next_id_function(&mut self) -> u32 {
        let id = self.function_id;
        self.function_id += 1;
        id
    }

    pub(crate) fn next_id_variable(&mut self) -> u32 {
        let id = self.variable_id;
        self.variable_id += 1;
        id
    }

    fn clear_state(&mut self) {
        self.structs.clear();
        self.functions.clear();
        self.extern_functions.clear();
        self.pendings_extern_fn.clear();
    }

    pub fn to_hir(&mut self, modules: &HashMap<FileId, ModuleResult>) -> HashMap<FileId, (HirModule, PathBuf)> {
        // First pass: Register all function names with IDs
        self.collect_declaration(modules);

        let mut hir_modules = HashMap::new();

        for (file_id, result) in modules {
            self.file_id = *file_id;
            self.current_file = Some(*file_id);
            self.clear_state();

            hir_modules.insert(*file_id, (self.lowering_module(&result.module), result.path.clone()));
        }

        hir_modules
    }

    fn collect_declaration(&mut self, modules: &HashMap<FileId, ModuleResult>) {
        for (file_id, result) in modules {
            self.file_id = *file_id;
            self.files.push(*file_id);
            for declaration in &result.module.declaration {
                match &declaration.kind {
                    DeclarationKind::Struct(struct_decl) => {
                        let id = self.next_id_struct();
                        let mut fields: Vec<HirField> = Vec::new();
                        for field in &struct_decl.fields {
                            let Some(ty) = self.get_type(field.id) else {
                                println!("id not found? {} {}", field.id, field.kind.name);
                                continue;
                            };
                            fields.push(HirField {
                                id: field.id,
                                name: field.kind.name,
                                ty: ty.clone(),
                            });
                        }
                        let struct_dec = HirStruct {
                            id: id,
                            name: struct_decl.name,
                            fields: fields,
                            visibility: struct_decl.visibility.to_hir(),
                            ty: self.get_type(declaration.id).unwrap().clone(),
                        };


                        self
                            .structs_map
                            .entry(*file_id)
                            .or_default()
                            .insert(struct_decl.name, struct_dec);
                        
                    }
                    DeclarationKind::Fn(func) => {
                        let id = self.next_id_function();

                        self.add_function(*file_id, id, func.kind.name);

                        self.add_signature_func(declaration.id, id, func);
                    }
                    DeclarationKind::Extern(extern_func) => {
                        let id = self.next_id_function();
                        self.add_function(*file_id, id, extern_func.name);

                        self.add_signature_ex_func(declaration.id, id, extern_func);
                    }
                    _ => {}
                }
            }
        }
    }

    fn lowering_module(&mut self, module: &Module) -> HirModule {
        for declaration in &module.declaration {
            match &declaration.kind {
                DeclarationKind::Struct(struct_decl) => {
                    if let Some(structs) = self.structs_map.get(&module.file) {
                        if let Some(hir_struct) = structs.get(&struct_decl.name) {
                            self.structs.push(hir_struct.clone());
                        }
                    }
                }
                DeclarationKind::Fn(func) => {
                    let fun = self.function_declaration(declaration.id, module.file, func);
                    self.functions.push(fun);
                }
                DeclarationKind::Extern(extern_func) => {
                    let mut hir_params: Vec<HirParameters> = Vec::new();

                    for param in &extern_func.parameters {
                        let ty = self.get_type(param.id).unwrap().clone();
                        hir_params.push(HirParameters {
                            id: self.next_id_variable(),
                            name: param.kind.name,
                            ty: ty,
                        });
                    }

                    let ty = self.get_type(declaration.id).unwrap().clone();
                    let extern_fn = HirExternFunction {
                        // id: *self.functions_map.get(&extern_func.name).unwrap(),
                        id: *self.functions_map.get(&module.file).unwrap().get(&extern_func.name).unwrap(),
                        name: extern_func.name,
                        parameters: hir_params,
                        return_type: ty,
                    };
                    self.extern_functions.push(extern_fn);
                }
                _ => {}
            }
        }

        for (id, _) in &self.pendings_extern_fn {
            let signature = self.function_signature.get(id).unwrap();
            let extern_fn = HirExternFunction {
                id: *id,
                name: signature.name,
                parameters: signature.paramater.clone(),
                return_type: signature.return_type.clone(),
            };
            self.extern_functions.push(extern_fn);
        }

        HirModule {
            file_id: module.file,
            structs: self.structs.clone(),
            functions: self.functions.clone(),
            extern_functions: self.extern_functions.clone(),
        }
    }

    fn function_declaration(&mut self, node_id: NodeId, file_id: FileId, func: &Function) -> HirFunction {
        self.variables.clear();
        self.scope.reset();

        self.scope.next();
        let mut hir_params: Vec<HirParameters> = Vec::new();

        for param in &func.kind.parameters {
            let ty = self.get_type(param.id).unwrap().clone();
            let var = HirVariable {
                id: self.next_id_variable(),
                name: param.kind.name,
                ty: ty.clone(),
                span: param.span,
            };
            self.variables
                .entry(self.scope.current())
                .or_default()
                .insert(param.kind.name, var.clone());

            hir_params.push(HirParameters {
                id: var.id,
                name: param.kind.name,
                ty: ty.clone(),
            });
        }

        let block = self.lower_block(&func.kind.body);

        self.scope.pop();

        let ty = self.get_type(node_id).unwrap().clone();
        HirFunction {
            // id: *self.functions_map.get(&func.kind.name).unwrap(), // Use pre-registered ID
            id: self.functions_map.get(&file_id).unwrap().get(&func.kind.name).unwrap().clone(),
            name: func.kind.name,
            parameters: hir_params,
            body: block,
            return_type: ty,
            visibility: func.kind.visibility.to_hir(),
        }
    }

    fn add_function(&mut self, file_id: FileId, id: u32, name: StrId) {
        self.functions_map.entry(file_id).or_default().insert(name, id);
    }

    pub(crate) fn get_function_id(&mut self, id: NodeId, name: StrId) -> Option<FunctionId> {
        let function = self
            .get_symbol(id)
            .and_then(|symbol| symbol.get_function())?;

        let file_id = function.file_id;

        let function_id = self
            .functions_map
            .get(&function.file_id)
            .and_then(|fmap| fmap.get(&function.name))
            .cloned()?;

        if let Some(current_file_id) = self.current_file {
            if file_id.0 != current_file_id.0 {
                self.pendings_extern_fn.insert(function_id, name);
            }
        }

        Some(function_id)
    }

    fn add_signature_func(&mut self, node_id: NodeId, id: u32, func: &Function) {
        let mut params = Vec::new(); 
        for param in &func.kind.parameters {
            let ty = self.get_type(param.id).unwrap().clone();
            params.push(HirParameters {
                id: 0,
                name: param.kind.name,
                ty: ty.clone(),
            });
        }

        let ty = self.get_type(node_id).unwrap().clone();
        self.add_signature(id, FnSignature {
            name: func.kind.name,
            paramater: params.clone(),
            param_ty: params.iter().map(|p| p.ty.clone()).collect(),
            return_type: ty,
        });
    }

    fn add_signature_ex_func(&mut self, node_id: NodeId, id: u32, extern_fn: &ExternFn) {
        let mut params = Vec::new(); 
        for param in &extern_fn.parameters {
            let ty = self.get_type(param.id).unwrap().clone();
            params.push(HirParameters {
                id: 0,
                name: param.kind.name,
                ty: ty.clone(),
            });
        }

        let ty = self.get_type(node_id).unwrap().clone();
        self.add_signature(id, FnSignature {
            name: extern_fn.name,
            paramater: params.clone(),
            param_ty: params.iter().map(|p| p.ty.clone()).collect(),
            return_type: ty,
        });
    }

    fn add_signature(&mut self, id: u32, signature: FnSignature) {
        self.function_signature.insert(id, signature);
    }

    pub(crate) fn get_signature(&mut self, id: u32) -> &FnSignature {
        self.function_signature.get(&id).unwrap()
    }

    pub(crate) fn get_type(&mut self, id: u32) -> Option<&Rc<Type>> {
        self.db.get_type(self.file_id, id)
    }

    fn get_symbol(&self, id: NodeId) -> Option<&Symbol> {
        self.db.get_symbol(self.file_id, id)
    }

    pub(crate) fn find_struct_field(&self, struct_name: &StrId, field_name: StrId) -> Option<&HirField> {
        // Cari struct berdasarkan nama
        if let Some(struct_map) = self.structs_map.get(&self.file_id) {
            if let Some(hir_struct) = struct_map.get(struct_name) {
                // Cari field dalam struct
                for field in &hir_struct.fields {
                    if field.name == field_name {
                        return Some(field);
                    }
                }
            }
        }
        None
    }

    pub fn print_debug(&self, config: &'a Config, hir_modules: &HashMap<FileId, (HirModule, PathBuf)>) {
        for (_, (module, _)) in hir_modules {
            let mut printer = HirPrinter::new(config);
            printer.print_module(module);
        }
    }
}