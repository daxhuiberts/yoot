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

    pub fn add_function_import(
        &self,
        internal_name: &str,
        external_name: (&str, &str),
        ty: &FnType,
    ) {
        let internal_name = std::ffi::CString::new(internal_name).unwrap();
        let external_module_name = std::ffi::CString::new(external_name.0).unwrap();
        let external_base_name = std::ffi::CString::new(external_name.1).unwrap();

        unsafe {
            BinaryenAddFunctionImport(
                self.inner,
                internal_name.as_ptr(),
                external_module_name.as_ptr(),
                external_base_name.as_ptr(),
                ty.binaryen_params,
                ty.binaryen_results,
            )
        }
    }

    pub fn local_get(&self, i: usize, ty: Type) -> Expression {
        Expression {
            inner: unsafe { BinaryenLocalGet(self.inner, i as u32, ty.to_binaryen()) },
        }
    }

    pub fn local_set(&self, i: usize, expression: Expression) -> Expression {
        Expression {
            inner: unsafe { BinaryenLocalSet(self.inner, i as u32, expression.inner) },
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

    pub fn call(&self, name: &str, args: Vec<Expression>, ret_ty: Type) -> Expression {
        let name = std::ffi::CString::new(name).unwrap();
        let (args_ptr, args_len) = to_raw_parts(args.into_iter().map(|arg| arg.inner).collect());
        Expression {
            inner: unsafe {
                BinaryenCall(
                    self.inner,
                    name.as_ptr() as *const i8,
                    args_ptr,
                    args_len as u32,
                    ret_ty.to_binaryen(),
                )
            },
        }
    }

    pub fn if_(
        &self,
        condition: Expression,
        then: Expression,
        else_: Option<Expression>,
    ) -> Expression {
        let else_ = if let Some(else_) = else_ {
            else_.inner
        } else {
            std::ptr::null_mut()
        };
        Expression {
            inner: unsafe { BinaryenIf(self.inner, condition.inner, then.inner, else_) },
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

    pub fn break_(&self, name: &str, condition: Expression, do_: Expression) -> Expression {
        let name = std::ffi::CString::new(name).unwrap();

        Expression {
            inner: unsafe {
                BinaryenBreak(
                    self.inner,
                    name.as_ptr() as *const i8,
                    condition.inner,
                    do_.inner,
                )
            },
        }
    }

    pub fn loop_(&self, in_: &str, body: Expression) -> Expression {
        let in_ = std::ffi::CString::new(in_).unwrap();

        Expression {
            inner: unsafe { BinaryenLoop(self.inner, in_.as_ptr() as *const i8, body.inner) },
        }
    }

    pub fn unary(&self, op: Op, expr: Expression) -> Expression {
        Expression {
            inner: unsafe { BinaryenUnary(self.inner, op.to_binaryen(), expr.inner) },
        }
    }

    pub fn binary(&self, op: Op, left: Expression, right: Expression) -> Expression {
        Expression {
            inner: unsafe { BinaryenBinary(self.inner, op.to_binaryen(), left.inner, right.inner) },
        }
    }

    pub fn nop(&self) -> Expression {
        Expression {
            inner: unsafe { BinaryenNop(self.inner) },
        }
    }

    pub fn add_function(
        &self,
        name: &str,
        ty: &FnType,
        vars: Vec<Type>,
        expr: Expression,
    ) -> Function {
        let name = std::ffi::CString::new(name).unwrap();
        let (vars_ptr, vars_len) =
            to_raw_parts(vars.into_iter().map(|ty| ty.to_binaryen()).collect());

        Function {
            inner: unsafe {
                BinaryenAddFunction(
                    self.inner,
                    name.as_ptr(),
                    ty.binaryen_params,
                    ty.binaryen_results,
                    vars_ptr as *mut usize,
                    vars_len as u32,
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
    EqInt32,
    EqInt64,
    EqZInt32,
    GtSInt64,
    LeSInt64,
    OrInt32,
    SubInt64,
}

impl Op {
    fn to_binaryen(&self) -> BinaryenOp {
        match self {
            Self::AddInt32 => unsafe { BinaryenAddInt32() },
            Self::AddInt64 => unsafe { BinaryenAddInt64() },
            Self::EqInt32 => unsafe { BinaryenEqInt32() },
            Self::EqInt64 => unsafe { BinaryenEqInt64() },
            Self::EqZInt32 => unsafe { BinaryenEqZInt32() },
            Self::GtSInt64 => unsafe { BinaryenGtSInt64() },
            Self::LeSInt64 => unsafe { BinaryenLeSInt64() },
            Self::OrInt32 => unsafe { BinaryenOrInt32() },
            Self::SubInt64 => unsafe { BinaryenSubInt64() },
        }
    }
}

pub enum Literal {
    Int32(i32),
    Int64(i64),
}

impl Literal {
    fn to_binaryen(&self) -> BinaryenLiteral {
        match self {
            Literal::Int32(num) => unsafe { BinaryenLiteralInt32(*num) },
            Literal::Int64(num) => unsafe { BinaryenLiteralInt64(*num) },
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
