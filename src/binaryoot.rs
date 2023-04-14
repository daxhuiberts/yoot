#![allow(dead_code)]

/// Binaryoot is a 'safe' abstraction on top op binaryen-sys.
use binaryen::ffi::*;

pub struct Module {
    inner: BinaryenModuleRef,
}

impl Module {
    pub fn new() -> Self {
        Module {
            inner: unsafe { BinaryenModuleCreate() },
        }
    }

    pub fn local_get(&self, i: u32, ty: Type) -> Expression {
        Expression {
            inner: unsafe { BinaryenLocalGet(self.inner, i, ty.to_binaryen()) },
        }
    }

    pub fn binary(&self, op: Op, left: Expression, right: Expression) -> Expression {
        Expression {
            inner: unsafe { BinaryenBinary(self.inner, op.to_binaryen(), left.inner, right.inner) },
        }
    }

    pub fn add_function(&self, name: &str, ty: &FnType, expr: Expression) -> Function {
        let name = std::ffi::CString::new(name).unwrap();
        Function {
            inner: unsafe {
                BinaryenAddFunction(
                    self.inner,
                    name.as_ptr(),
                    ty.binaryen_params,
                    ty.binaryen_results,
                    std::ptr::null() as *const usize as *mut usize,
                    0,
                    expr.inner,
                )
            },
        }
    }

    pub fn print(&self) {
        unsafe {
            BinaryenModulePrint(self.inner);
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            BinaryenModuleDispose(self.inner);
        }
    }
}

pub struct Expression {
    inner: BinaryenExpressionRef,
}

pub struct Function {
    inner: BinaryenFunctionRef,
}

pub struct FnType {
    raw_params: Box<[BinaryenType]>,
    binaryen_params: BinaryenType,
    binaryen_results: BinaryenType,
}

impl FnType {
    pub fn new(params: &[Type], results: Type) -> Self {
        // BinaryenTypeCreate requires a pointer to a persistent in memory array of types and
        // returns a reference to that for use as a type itself. Store both the persistent array in
        // memory as well as the reference.
        unsafe {
            let raw_params: Box<_> = params.iter().map(Type::to_binaryen).collect();
            let binaryen_params = BinaryenTypeCreate(
                &*raw_params as *const _ as *mut BinaryenType,
                raw_params.len() as u32,
            );

            let binaryen_results = results.to_binaryen();

            Self {
                raw_params,
                binaryen_params,
                binaryen_results,
            }
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Int32,
}

impl Type {
    fn to_binaryen(&self) -> BinaryenType {
        match self {
            Self::Int32 => unsafe { BinaryenTypeInt32() },
        }
    }
}

#[derive(Debug)]
pub enum Op {
    AddInt32,
}

impl Op {
    fn to_binaryen(&self) -> BinaryenOp {
        match self {
            Self::AddInt32 => unsafe { BinaryenAddInt32() },
        }
    }
}
