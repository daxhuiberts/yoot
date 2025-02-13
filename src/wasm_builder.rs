use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, Module, TypeSection, ValType,
};

#[derive(Debug)]
pub struct App {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    imports: ImportSection,
    exports: ExportSection,
    codes: CodeSection,
    function_count: u32,
    function_implementations: Vec<(u32, Function)>,
}

impl App {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            types: TypeSection::new(),
            functions: FunctionSection::new(),
            imports: ImportSection::new(),
            exports: ExportSection::new(),
            codes: CodeSection::new(),
            function_count: 0,
            function_implementations: Vec::new(),
        }
    }

    pub fn add_function<F: FnOnce(&mut Fun)>(
        &mut self,
        scope: HashMap<String, u32>,
        // name: &str,
        params: Vec<(String, ValType)>,
        result_ty: Option<ValType>,
        f: F,
    ) -> u32 {
        let (param_names, param_types) = params.into_iter().unzip();
        let results = result_ty.into_iter().collect::<Vec<_>>();
        let type_index = self.add_type(param_types, results);

        // println!("ADD FUNCTION DEFINITION: {name}");
        self.functions.function(type_index);
        let function_index = self.function_count;
        self.function_count += 1;

        let mut fun = Fun::new(self, scope, param_names);
        f(&mut fun);

        // println!("FUN: {fun:#?}");

        let mut f = Function::new_with_locals_types(fun.locals);
        for instruction in fun.instructions {
            f.instruction(&instruction);
        }
        f.instruction(&Instruction::End);
        self.function_implementations.push((function_index, f));

        function_index
    }

    fn add_type(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        self.types.ty().function(params, results);
        self.types.len() - 1
    }

    pub fn add_export(&mut self, name: &str, function_index: u32) {
        self.exports.export(name, ExportKind::Func, function_index);
    }

    pub fn finish(mut self) -> Vec<u8> {
        self.module.section(&self.types);
        self.module.section(&self.imports);
        self.module.section(&self.functions);
        self.module.section(&self.exports);

        self.function_implementations.sort_by_key(|x| x.0);
        self.function_implementations
            .into_iter()
            .map(|x| x.1)
            .for_each(|fi| {
                self.codes.function(&fi);
            });

        self.module.section(&self.codes);

        self.module.finish()
    }

    pub fn add_function_import(
        &mut self,
        module: &str,
        field: &str,
        params: Vec<ValType>,
        results: Vec<ValType>,
    ) -> u32 {
        let type_index = self.add_type(params, results);
        self.imports
            .import(module, field, EntityType::Function(type_index));
        let function_index = self.function_count;
        self.function_count += 1;
        function_index
    }
}

#[derive(Debug)]
pub struct Fun<'a> {
    app: &'a mut App,
    vars: HashMap<String, u32>,
    funs: HashMap<String, u32>,
    locals: Vec<ValType>,
    instructions: Vec<Instruction<'a>>,
}

impl<'a> Fun<'a> {
    fn new(app: &'a mut App, funs: HashMap<String, u32>, args: Vec<String>) -> Self {
        Self {
            app,
            vars: args
                .into_iter()
                .enumerate()
                .map(|(index, name)| (name, index as u32))
                .collect(),
            funs,
            locals: vec![],
            instructions: vec![],
        }
    }

    pub fn local_index(&self, name: &str) -> Option<u32> {
        self.vars.get(name).copied()
    }

    pub fn function_index(&self, name: &str) -> u32 {
        // println!("FUN? {name}");
        self.funs.get(name).copied().unwrap()
    }

    pub fn get_or_add_local(&mut self, name: String, ty: ValType) -> u32 {
        if let Some(index) = self.local_index(&name) {
            index
        } else {
            self.locals.push(ty);
            let index = self.vars.len() as u32;
            self.vars.insert(name, index);
            index
        }
    }

    pub fn add_function(&mut self, name: String, index: u32) {
        self.funs.insert(name, index);
    }

    pub fn instr(&mut self, instruction: Instruction<'a>) {
        self.instructions.push(instruction);
    }

    pub fn funs(&self) -> &HashMap<String, u32> {
        &self.funs
    }

    pub fn app(&mut self) -> &mut App {
        self.app
    }
}
