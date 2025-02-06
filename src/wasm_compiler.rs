#![allow(unused)]

use crate::ast::ExprKind;
/// Uses the Binaryoot abstraction to build a wasm program.
use crate::binaryoot::{Op::*, *};
use crate::typed_ast::*;
use crate::util::{IterExt, Result};

pub struct WasmModule(Module);

pub fn to_wasm_module(program: &TypedProgram) -> Result<WasmModule> {
    let mut module = Module::new();

    let (expression, ty) = compile_decls(&mut module, program.decls());

    let function = module.add_function("main", &FnType::new(&[], ty), expression);
    module.add_function_export(&function, "main");

    Ok(WasmModule(module))
}

pub fn print(module: &WasmModule) {
    module.0.print();
}

pub fn compile(module: &WasmModule) -> Result<Vec<u8>> {
    Ok(module.0.compile())
}

fn compile_decls(module: &mut Module, decls: &[TypedDecl]) -> (Expression, Type) {
    let mut expressions = vec![];

    let len = decls.len();

    let final_type: Type = decls
        .iter()
        .enumerate()
        .try_fold_with_context(
            Type::None,
            &mut expressions,
            |expressions: &mut &mut Vec<_>, (index, decl): (usize, &TypedDecl)| {
                let mut expression = compile_decl(module, decl);
                let ty = from_ty(&decl.ty());
                if index < len - 1 {
                    match ty {
                        Type::None => (),
                        _ => {
                            expression = module.drop_(expression);
                        }
                    };
                }
                expressions.push(expression);
                Ok::<Type, ()>(ty)
            },
        )
        .unwrap();

    (module.block(expressions), final_type)
}

fn compile_decl(module: &mut Module, decl: &TypedDecl) -> Expression {
    match decl {
        TypedDecl::Stm { expr, ty } => compile_expr(module, expr),
        TypedDecl::Ass { name, expr, ty } => todo!("implement ass"),
        TypedDecl::Fun {
            name,
            args,
            body,
            ty,
        } => todo!("implement fun"),
    }
}

fn compile_expr(module: &mut Module, expr: &TypedExpr) -> Expression {
    match &expr.kind {
        ExprKind::Lit { lit } => match lit {
            crate::ast::LitKind::Nil => todo!(),
            crate::ast::LitKind::Bool(_) => todo!(),
            crate::ast::LitKind::Num(num) => module.const_(Literal::Num(*num)),
        },
        ExprKind::Ident { name } => todo!("implement ident"),
        ExprKind::UnOp { kind, expr } => todo!("implement unop"),
        ExprKind::BinOp { kind, left, right } => todo!("implement binop"),
        ExprKind::If { cond, then, else_ } => todo!("implement if"),
        ExprKind::While { cond, do_ } => todo!("implement while"),
        ExprKind::Call { name, args } => todo!("implement call"),
        ExprKind::Print { expr } => todo!("implement print"),
        ExprKind::Block { decls } => todo!("implement decls"),
    }
}

fn from_ty(ty: &TySimple) -> Type {
    match ty {
        TySimple::Nil => Type::None,
        TySimple::Bool => todo!(),
        TySimple::Num => Type::Int64,
    }
}
