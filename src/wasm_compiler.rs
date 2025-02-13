#![allow(unused)]

use std::collections::HashMap;

use crate::ast::ExprKind;
/// Uses the Binaryoot abstraction to build a wasm program.
use crate::binaryoot::*;
use crate::typed_ast::*;
use crate::util::{IterExt, Result};

pub struct WasmModule(Module);

#[derive(Debug)]
struct Scope {
    args: Vec<(String, TySimple)>,
    vars: Vec<(String, TySimple)>,
    funs: HashMap<String, TyFunction>,
}

impl Scope {
    fn new(args: &[&str], arg_tys: &[TySimple], funs: &HashMap<String, TyFunction>) -> Self {
        Self {
            args: args
                .iter()
                .zip(arg_tys)
                .map(|(a, t)| (String::from(*a), t.clone()))
                .collect(),
            vars: vec![],
            funs: funs.clone(),
        }
    }

    fn get_local_index(&self, name: &str) -> Option<usize> {
        self.args
            .iter()
            .chain(&self.vars)
            .position(|var| var.0 == *name)
    }

    fn get_or_add_var(&mut self, name: String, ty: TySimple) -> usize {
        if let Some(index) = self.get_local_index(&name) {
            index
        } else {
            self.vars.push((name, ty));
            self.args.len() + self.vars.len() - 1
        }
    }

    fn get_fun_ty(&self, name: &str) -> &TyFunction {
        self.funs.get(name).unwrap()
    }
}

pub fn to_wasm_module(program: &TypedProgram) -> Result<WasmModule> {
    let mut module = Module::new();

    let mut env = HashMap::new();
    define_import_function(
        &module,
        &mut env,
        "print_i64",
        TyFunction {
            args: vec![TySimple::Num],
            ret: TySimple::Nil,
        },
    );

    let function = compile_function(
        &mut module,
        "main",
        &TyFunction {
            args: vec![],
            ret: program.ty.clone(),
        },
        &[],
        &env,
        &program.decls,
    );
    module.add_function_export(&function, "main");

    Ok(WasmModule(module))
}

fn define_import_function(
    module: &Module,
    env: &mut HashMap<String, TyFunction>,
    name: &str,
    ty: TyFunction,
) {
    module.add_function_import(name, ("foo", name), &ty_function_to_fn_type(&ty));
    env.insert(name.to_owned(), ty);
}

pub fn print(module: &WasmModule) {
    module.0.print();
}

pub fn compile(module: &WasmModule) -> Result<Vec<u8>> {
    Ok(module.0.compile())
}

fn compile_decls(module: &mut Module, scope: &mut Scope, decls: &[TypedDecl]) -> Expression {
    let len = decls.len();

    let mut expressions = decls
        .iter()
        .enumerate()
        .filter_map(|(index, decl)| {
            let mut expression = compile_decl(module, scope, decl)?;
            let ty = from_ty(&decl.ty());
            if index < len - 1 && matches!(decl, TyDecl::Stm { .. }) && !matches!(ty, Type::None) {
                println!("DROP?!?!?");
                expression = module.drop_(expression);
            }
            Some(expression)
        })
        .collect::<Vec<_>>();

    match expressions.len() {
        0 => module.nop(),
        1 => expressions.remove(0),
        _ => module.block(expressions),
    }
}

fn compile_decl(module: &mut Module, scope: &mut Scope, decl: &TypedDecl) -> Option<Expression> {
    match decl {
        TypedDecl::Stm { expr, ty } => Some(compile_expr(module, scope, expr)),
        TypedDecl::Ass { name, expr, ty } => {
            let expression = compile_decls(module, scope, expr);
            let index = scope.get_or_add_var(name.clone(), ty.clone());
            // println!("SCOPE AFTER VARS PUSH: {scope:?}");
            Some(module.local_set(index, expression))
        }
        TypedDecl::Fun {
            name,
            args,
            body,
            ty,
        } => {
            let args: Vec<&str> = args.iter().map(<_>::as_ref).collect();
            compile_function(module, name, ty, &args, &scope.funs, body);
            scope.funs.insert(name.clone(), ty.clone());
            None
        }
    }
}

fn compile_expr(module: &mut Module, scope: &mut Scope, expr: &TypedExpr) -> Expression {
    match &expr.kind {
        ExprKind::Lit { lit } => match lit {
            crate::ast::LitKind::Nil => {
                // todo!()
                module.nop()
            }
            crate::ast::LitKind::Bool(bool) => module.const_(Literal::Int32(*bool as i32)),
            crate::ast::LitKind::Num(num) => module.const_(Literal::Int64(*num)),
        },
        ExprKind::Ident { name } => {
            let index = scope.get_local_index(name).expect("var should exist");
            module.local_get(index, from_ty(&expr.ty))
        }
        crate::ast::ExprKind::UnOp {
            kind: crate::ast::UnOpKind::Neg,
            expr,
        } => {
            let zero = module.const_(Literal::Int64(0));
            let expr = compile_expr(module, scope, expr);
            module.binary(Op::SubInt64, zero, expr)
        }
        crate::ast::ExprKind::UnOp {
            kind: crate::ast::UnOpKind::Not,
            expr,
        } => {
            let expr = compile_expr(module, scope, expr);
            module.unary(Op::EqZInt32, expr)
        }
        ExprKind::BinOp { kind, left, right } => {
            let left_expr = compile_expr(module, scope, left);
            let right_expr = compile_expr(module, scope, right);

            match kind {
                crate::ast::BinOpKind::Add => module.binary(Op::AddInt64, left_expr, right_expr),
                crate::ast::BinOpKind::Sub => module.binary(Op::SubInt64, left_expr, right_expr),
                crate::ast::BinOpKind::Mul => todo!("implement binop Mul"),
                crate::ast::BinOpKind::Div => todo!("implement binop Div"),
                crate::ast::BinOpKind::Eq => {
                    let op = match left.ty {
                        TySimple::Bool => Op::EqInt32,
                        TySimple::Num => Op::EqInt64,
                        _ => panic!("{:?} should not exist here", expr.ty),
                    };
                    module.binary(op, left_expr, right_expr)
                }
                crate::ast::BinOpKind::Neq => todo!("implement binop Neq"),
                crate::ast::BinOpKind::Gt => module.binary(Op::GtSInt64, left_expr, right_expr),
                crate::ast::BinOpKind::Gte => todo!("implement binop Gte"),
                crate::ast::BinOpKind::Lt => todo!("implement binop Lt"),
                crate::ast::BinOpKind::Lte => module.binary(Op::LeSInt64, left_expr, right_expr),
                crate::ast::BinOpKind::And => todo!("implement binop And"),
                crate::ast::BinOpKind::Or => module.binary(Op::OrInt32, left_expr, right_expr),
            }
        }
        ExprKind::If { cond, then, else_ } => {
            let cond = compile_expr(module, scope, cond);
            let then = compile_expr(module, scope, then);
            let else_ = else_
                .as_ref()
                .map(|else_| compile_expr(module, scope, else_));
            module.if_(cond, then, else_)
        }
        ExprKind::While { cond, do_ } => {
            let cond = compile_expr(module, scope, cond);
            let do_ = compile_expr(module, scope, do_);

            module.loop_("foo", module.break_("foo", cond, do_))
        }
        ExprKind::Call { name, args } => {
            let args = args
                .iter()
                .map(|arg| compile_expr(module, scope, arg))
                .collect();
            let ret_ty = from_ty(&scope.get_fun_ty(name).ret);
            module.call(name, args, ret_ty)
        }
        ExprKind::Print { expr } => {
            // todo!("implement print")
            let fun = match expr.ty {
                TySimple::Nil => todo!(),
                TySimple::Bool => todo!(),
                TySimple::Num => "print_i64",
            };
            let ret_ty = from_ty(&scope.get_fun_ty(fun).ret);
            let expr = compile_expr(module, scope, expr);
            module.call(fun, vec![expr], ret_ty)
            // module.nop()
        }
        ExprKind::Block { decls } => compile_decls(module, scope, decls),
    }
}

fn compile_function(
    module: &mut Module,
    name: &str,
    ty: &TyFunction,
    args: &[&str],
    funs: &HashMap<String, TyFunction>,
    decls: &[TypedDecl],
) -> Function {
    let mut scope = Scope::new(args, &ty.args, funs);
    let expression = compile_decls(module, &mut scope, decls);
    let vars = scope
        .vars
        .iter()
        .map(|(_, ty)| from_ty(&ty))
        .collect::<Vec<_>>();
    // println!("VARS: #{vars:?}");
    let ty = ty_function_to_fn_type(ty);
    module.add_function(name, &ty, vars, expression)
}

fn get_ty_from_decls(decls: &[TypedDecl]) -> TySimple {
    decls.last().unwrap().ty()
}

fn ty_function_to_fn_type(ty: &TyFunction) -> FnType {
    FnType::new(
        &ty.args.iter().map(from_ty).collect::<Vec<_>>(),
        from_ty(&ty.ret),
    )
}

fn from_ty(ty: &TySimple) -> Type {
    match ty {
        TySimple::Nil => Type::None,
        TySimple::Bool => Type::Int32,
        TySimple::Num => Type::Int64,
    }
}
