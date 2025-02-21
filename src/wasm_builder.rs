use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ConstExpr, DataCountSection, DataSection, EntityType, ExportKind, ExportSection,
    Function, FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

#[derive(Debug)]
pub struct App {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    memory: MemorySection,
    imports: ImportSection,
    exports: ExportSection,
    codes: CodeSection,
    data: Vec<u8>,
    function_count: u32,
    function_implementations: Vec<(u32, Function)>,
}

impl App {
    pub fn new() -> Self {
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        Self {
            module: Module::new(),
            types: TypeSection::new(),
            functions: FunctionSection::new(),
            memory,
            imports: ImportSection::new(),
            exports: ExportSection::new(),
            codes: CodeSection::new(),
            data: vec![],
            function_count: 0,
            function_implementations: Vec::new(),
        }
    }

    pub fn add_function<F: FnOnce(&mut Fun)>(
        &mut self,
        scope: HashMap<String, u32>,
        // name: &str,
        params: Vec<(String, Vec<ValType>)>,
        results: Vec<ValType>,
        f: F,
    ) -> u32 {
        let param_types: Vec<_> = params.iter().flat_map(|param| param.1.clone()).collect();
        let param_types_len = param_types.len();
        let type_index = self.add_function_type(param_types, results);

        // println!("ADD FUNCTION DEFINITION: {name}");
        self.functions.function(type_index);
        let function_index = self.function_count;
        self.function_count += 1;

        let mut fun = Fun::new(self, scope, params);
        f(&mut fun);

        // println!("FUN: {fun:#?}");

        let mut f =
            Function::new_with_locals_types((fun.locals[param_types_len..]).into_iter().cloned());
        for instruction in fun.instructions {
            f.instruction(&instruction);
        }
        f.instruction(&Instruction::End);
        self.function_implementations.push((function_index, f));

        function_index
    }

    fn add_function_type(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        self.types.ty().function(params, results);
        self.types.len() - 1
    }

    pub fn add_export(&mut self, name: &str, kind: ExportKind, function_index: u32) {
        self.exports.export(name, kind, function_index);
    }

    pub fn add_data(&mut self, data: &[u8]) -> u32 {
        let len = self.data.len() as u32;
        self.data.extend_from_slice(data);
        len
    }

    pub fn finish(mut self) -> Vec<u8> {
        self.module.section(&self.types);
        self.module.section(&self.imports);
        self.module.section(&self.functions);

        self.add_export("memory", ExportKind::Memory, 0);
        self.module.section(&self.memory);

        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(self.data.len() as i32),
        );
        self.module.section(&globals);
        self.module.section(&self.exports);

        self.module.section(&DataCountSection { count: 1 });

        self.function_implementations.sort_by_key(|x| x.0);
        self.function_implementations
            .into_iter()
            .map(|x| x.1)
            .for_each(|fi| {
                self.codes.function(&fi);
            });

        self.module.section(&self.codes);

        let mut datas = DataSection::new();
        datas.active(0, &ConstExpr::i32_const(0), self.data);
        self.module.section(&datas);

        self.module.finish()
    }

    pub fn add_function_import(
        &mut self,
        module: &str,
        field: &str,
        params: Vec<ValType>,
        results: Vec<ValType>,
    ) -> u32 {
        let type_index = self.add_function_type(params, results);
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
    funs: HashMap<String, u32>,
    vars: HashMap<String, u32>,
    locals: Vec<ValType>,
    instructions: Vec<Instruction<'a>>,
}

impl<'a> Fun<'a> {
    fn new(
        app: &'a mut App,
        funs: HashMap<String, u32>,
        params: Vec<(String, Vec<ValType>)>,
    ) -> Self {
        let (vars, locals): (_, Vec<_>) = params
            .into_iter()
            .scan(0, |offset, (param_name, param_types)| {
                let var = (param_name, *offset as u32);
                *offset += param_types.len();
                Some((var, param_types))
            })
            .unzip();
        let locals = locals.into_iter().flat_map(|x| x).collect();
        Self {
            app,
            funs,
            vars,
            locals,
            instructions: vec![],
        }
    }

    pub fn var_index(&self, name: &str) -> Option<u32> {
        self.vars.get(name).copied()
    }

    pub fn function_index(&self, name: &str) -> u32 {
        self.funs.get(name).copied().unwrap()
    }

    pub fn get_or_add_var(&mut self, name: String, tys: Vec<ValType>) -> u32 {
        if let Some(index) = self.var_index(&name) {
            index
        } else {
            let mut ty_iter = tys.into_iter();
            let ty = ty_iter.next().unwrap();
            let index = self.add_temp_local(ty);
            self.vars.insert(name, index);
            for ty in ty_iter {
                self.add_temp_local(ty);
            }
            index
        }
    }

    pub fn add_temp_local(&mut self, ty: ValType) -> u32 {
        let index = self.locals.len() as u32;
        self.locals.push(ty);
        index
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
