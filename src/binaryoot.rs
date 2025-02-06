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

    pub fn drop_(&self, expression: Expression) -> Expression {
        Expression {
            inner: unsafe { BinaryenDrop(self.inner, expression.inner) },
        }
    }

    pub fn const_(&self, literal: Literal) -> Expression {
        Expression {
            inner: unsafe { BinaryenConst(self.inner, literal.to_binaryen()) },
        }
    }

    pub fn block(&self, expressions: Vec<Expression>) -> Expression {
        let expressions = expressions.into_iter().map(|e| e.inner).collect::<Vec<_>>();
        let (ptr, len) = to_raw_parts(expressions);

        Expression {
            inner: unsafe {
                BinaryenBlock(
                    self.inner,
                    std::ptr::null(),
                    ptr,
                    len as u32,
                    BinaryenTypeAuto(),
                )
            },
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

    pub fn add_function_export(&self, function: &Function, extern_name: &str) {
        unsafe {
            let intern_name = BinaryenFunctionGetName(function.inner);
            let extern_name = std::ffi::CString::new(extern_name).unwrap();
            BinaryenAddFunctionExport(self.inner, intern_name, extern_name.as_ptr() as *const i8);
        }
    }

    pub fn print(&self) {
        unsafe {
            BinaryenModulePrint(self.inner);
        }
    }

    pub fn compile(&self) -> Vec<u8> {
        unsafe {
            let result = BinaryenModuleAllocateAndWrite(self.inner, std::ptr::null());
            Vec::from_raw_parts(
                result.binary as *mut u8,
                result.binaryBytes,
                result.binaryBytes,
            )
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
    None,
    Int32,
    Int64,
}

impl Type {
    fn to_binaryen(&self) -> BinaryenType {
        match self {
            Self::None => unsafe { BinaryenTypeNone() },
            Self::Int32 => unsafe { BinaryenTypeInt32() },
            Self::Int64 => unsafe { BinaryenTypeInt64() },
        }
    }
}

#[derive(Debug)]
pub enum Op {
    AddInt32,
    AddInt64,
}

impl Op {
    fn to_binaryen(&self) -> BinaryenOp {
        match self {
            Self::AddInt32 => unsafe { BinaryenAddInt32() },
            Self::AddInt64 => unsafe { BinaryenAddInt64() },
        }
    }
}

pub enum Literal {
    Nil,
    Bool(bool),
    Num(i64),
}

impl Literal {
    fn to_binaryen(&self) -> BinaryenLiteral {
        match self {
            Literal::Nil => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Num(num) => unsafe { BinaryenLiteralInt64(*num) },
        }
    }
}

fn to_raw_parts<T>(mut vec: Vec<T>) -> (*mut T, usize) {
    vec.shrink_to_fit();
    assert!(vec.len() == vec.capacity());
    let ptr = vec.as_mut_ptr();
    let len = vec.len();
    std::mem::forget(vec); // prevent deallocation in Rust
                           // The array is still there but no Rust object
                           // feels responsible. We only have ptr/len now
                           // to reach it.
    (ptr, len)
}
