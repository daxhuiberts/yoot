#![allow(unused)]

use std::collections::HashMap;

use crate::typed_ast::*;
use crate::util::Result;
use crate::wasm_builder::*;

use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, ExportKind, ExportSection, Function, FunctionSection,
    GlobalType, ImportSection,
    Instruction::{self, *},
    MemArg, Module, TypeSection, ValType,
};

pub fn compile(program: &TypedProgram) -> Result<Vec<u8>> {
    let mut app = App::new();
    let mut scope = HashMap::new();

    let global_index = app.add_global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(0),
    );
    assert_eq!(global_index, 0);

    let function_index = app.add_function_import("foo", "print_i64", vec![ValType::I64], vec![]);
    scope.insert("print_i64".into(), function_index);
    let function_index = app.add_function_import("foo", "print_string", vec![ValType::I32], vec![]);
    scope.insert("print_string".into(), function_index);

    let function_index = app.add_function(
        HashMap::new(),
        vec![("size".into(), ValType::I32)],
        Some(ValType::I32),
        |fun| {
            let size = fun.local_index("size").unwrap();
            fun.instr(GlobalGet(0));
            fun.instr(GlobalGet(0));
            fun.instr(LocalGet(size));
            fun.instr(I32Add);
            fun.instr(GlobalSet(0));
        },
    );
    scope.insert("allocate".into(), function_index);

    let function_index = app.add_function(scope, vec![], ty_to_val_ty(&program.ty), |fun| {
        compile_decls(fun, &program.decls);
    });

    app.add_export("main", ExportKind::Func, function_index);

    let wasm_bytes = app.finish();

    Ok(wasm_bytes)
}

pub fn print(src: &[u8]) {
    println!("{}", wasmprinter::print_bytes(src).unwrap());
}

// fn compile_function()

fn compile_decls(fun: &mut Fun, decls: &[TypedDecl]) {
    // fn compile_decls(module: &mut Module, scope: &mut Scope, decls: &[TypedDecl]) -> Expression {
    let len = decls.len();

    let mut expressions = decls
        .iter()
        .enumerate()
        .map(|(index, decl)| {
            compile_decl(fun, decl);
            // let ty = from_ty(&decl.ty());
            // println!("{index} < {len} - 1 = {}", index < len - 1);
            // println!("{decl:?}");
            // println!("TY {:?}", decl.ty());

            if index < len - 1
                && matches!(decl, TyDecl::Stm { .. })
                && !matches!(decl.ty(), TySimple::Nil)
            {
                println!("DROP?!?!?");
                fun.instr(Drop);
                // expression = module.drop_(expression);
            }
            // Some(expression)
        })
        .collect::<Vec<_>>();

    // match expressions.len() {
    //     0 => module.nop(),
    //     1 => expressions.remove(0),
    //     _ => module.block(expressions),
    // }
}

fn compile_decl(fun: &mut Fun, decl: &TypedDecl) -> Option<()> {
    // fn compile_decl(module: &mut Module, scope: &mut Scope, decl: &TypedDecl) -> Option<Expression> {
    match decl {
        TypedDecl::Stm { expr, ty } => Some(compile_expr(fun, expr)),
        TypedDecl::Ass { name, expr, ty } => {
            compile_decls(fun, expr);
            let index = fun.get_or_add_local(name.clone(), ty_to_val_ty(ty).unwrap());
            fun.instr(LocalSet(index));
            Some(())
        }
        TypedDecl::Fun {
            name,
            args,
            body,
            ty,
        } => {
            // let args: Vec<&str> = args.iter().map(<_>::as_ref).collect();
            // compile_function(module, name, ty, &args, &scope.funs, body);
            // scope.funs.insert(name.clone(), ty.clone());
            // None
            // let scope = HashMap::new();
            let scope = fun.funs().clone();
            let args = args
                .iter()
                .zip(&ty.args)
                .map(|(name, ty)| (name.clone(), ty_to_val_ty(ty).unwrap()))
                .collect::<Vec<_>>();
            let function_index =
                fun.app()
                    .add_function(scope, args, ty_to_val_ty(&ty.ret), |fun| {
                        compile_decls(fun, body);
                    });
            fun.add_function(name.clone(), function_index);
            Some(())
            // todo!()
        }
    }
}

fn compile_expr(fun: &mut Fun, expr: &TypedExpr) {
    match &expr.kind {
        crate::ast::ExprKind::Lit { lit } => match lit {
            crate::ast::LitKind::Nil => {
                // todo!()
            }
            crate::ast::LitKind::Bool(bool) => fun.instr(I32Const(*bool as i32)),
            crate::ast::LitKind::Num(num) => fun.instr(I64Const(*num)),
            crate::ast::LitKind::String(val) => {
                let data_index = fun.app().add_data(val.clone().into_bytes());
                let function_index = fun.function_index("allocate");
                let new_address = fun.get_or_add_local("local".into(), ValType::I32);

                // allocate memory
                fun.instr(I32Const((val.len() + 4) as i32));
                fun.instr(Call(function_index));
                fun.instr(LocalSet(new_address));

                // store length
                fun.instr(LocalGet(new_address));
                fun.instr(I32Const(val.len() as i32));
                fun.instr(I32Store(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));

                // fun.instr(LocalGet(new_address));
                // let function_index = fun.function_index("print");
                // fun.instr(Call(function_index));

                // copy string literal from data segment
                fun.instr(LocalGet(new_address));
                fun.instr(I32Const(4));
                fun.instr(I32Add);
                fun.instr(I32Const(0));
                fun.instr(I32Const(val.len() as i32));
                fun.instr(MemoryInit { mem: 0, data_index });

                // return string address
                fun.instr(LocalGet(new_address));
            }
        },
        crate::ast::ExprKind::Ident { name } => {
            let index = fun.local_index(name).unwrap();
            fun.instr(LocalGet(index));
            // todo!()
        }
        crate::ast::ExprKind::UnOp {
            kind: crate::ast::UnOpKind::Neg,
            expr,
        } => {
            fun.instr(I64Const(0));
            compile_expr(fun, expr);
            fun.instr(I64Sub);
        }
        crate::ast::ExprKind::UnOp {
            kind: crate::ast::UnOpKind::Not,
            expr,
        } => {
            compile_expr(fun, expr);
            fun.instr(I32Eqz);
        }
        crate::ast::ExprKind::BinOp { kind, left, right } => {
            compile_expr(fun, left);
            compile_expr(fun, right);
            match kind {
                crate::ast::BinOpKind::Add => fun.instr(I64Add),
                crate::ast::BinOpKind::Sub => fun.instr(I64Sub),
                crate::ast::BinOpKind::Mul => todo!(),
                crate::ast::BinOpKind::Div => todo!(),
                crate::ast::BinOpKind::Eq => {
                    let op = match left.ty {
                        TySimple::Bool => I32Eq,
                        TySimple::Num => I64Eq,
                        _ => panic!("{:?} should not exist here", expr.ty),
                    };
                    fun.instr(op)
                }
                crate::ast::BinOpKind::Neq => todo!(),
                crate::ast::BinOpKind::Gt => fun.instr(I64GtS),
                crate::ast::BinOpKind::Gte => todo!(),
                crate::ast::BinOpKind::Lt => todo!(),
                crate::ast::BinOpKind::Lte => fun.instr(I64LeS),
                crate::ast::BinOpKind::And => todo!(),
                crate::ast::BinOpKind::Or => fun.instr(I32Or),
            }
        }
        crate::ast::ExprKind::If { cond, then, else_ } => {
            // let cond = compile_expr(module, scope, cond);
            // let do_ = compile_expr(module, scope, do_);

            // module.loop_("foo", module.break_("foo", cond, do_))
            //

            compile_expr(fun, cond);
            fun.instr(If(ty_to_block_ty(&expr.ty)));
            compile_expr(fun, then);
            if let Some(else_) = else_ {
                fun.instr(Else);
                compile_expr(fun, else_);
            }
            fun.instr(End);
        }
        crate::ast::ExprKind::While { cond, do_ } => {
            fun.instr(Loop(BlockType::Empty));
            compile_expr(fun, do_);
            compile_expr(fun, cond);
            fun.instr(BrIf(0));
            fun.instr(End);
        }
        crate::ast::ExprKind::Call { name, args } => {
            let function_index = fun.function_index(name);
            args.iter().for_each(|arg| compile_expr(fun, arg));
            fun.instr(Call(function_index));
        }
        crate::ast::ExprKind::Print { expr } => {
            compile_expr(fun, expr);
            let print_function = match expr.ty {
                TySimple::Nil => todo!(),
                TySimple::Bool => todo!(),
                TySimple::Num => "print_i64",
                TySimple::String => "print_string",
            };
            let function_index = fun.function_index(print_function);
            fun.instr(Call(function_index));
        }
        crate::ast::ExprKind::Block { decls } => {
            compile_decls(fun, decls);
        }
    }
}

fn ty_to_val_ty(ty: &TySimple) -> Option<ValType> {
    match ty {
        TySimple::Nil => None,
        TySimple::Bool => Some(ValType::I32),
        TySimple::Num => Some(ValType::I64),
        TySimple::String => todo!(),
    }
}

fn ty_to_block_ty(ty: &TySimple) -> BlockType {
    match ty_to_val_ty(ty) {
        Some(ty) => BlockType::Result(ty),
        None => BlockType::Empty,
    }
}
