use std::collections::HashMap;

use super::ast::*;
use super::typed_ast::*;
use super::util::*;

pub fn check(program: &Program, print_maybe: bool) -> Result<TypedProgram> {
    let maybe_decls = check_decls(program.decls(), &mut HashMap::new())?;

    if print_maybe {
        println!("MAYBE TYPED: {maybe_decls:#?}");
    }

    let decls = maybe_decls
        .into_iter()
        .map(maybe_typed_decl_to_typed_decl)
        .collect::<Result<Vec<_>>>()?;

    let ty = decls.last().unwrap().ty();
    Ok(TypedProgram { decls, ty })
}

pub fn check_decls(
    decls: &[Decl],
    env: &mut HashMap<String, MaybeTy>,
) -> Result<Vec<MaybeTypedDecl>> {
    decls
        .iter()
        .map_with_context(env, |env, decl| match &decl {
            Decl::Stm { expr } => {
                let expr = check_expr(expr, env)?;
                let ty = expr.ty.clone();

                Ok(MaybeTypedDecl::Stm { expr, ty })
            }

            Decl::Ass {
                name: (name, expected_type),
                expr,
            } => {
                let expected_type = MaybeTySimple::parse(&name, &expected_type)?;
                let decls = check_decls(expr, &mut env.clone())?;
                let ty = match decls.last().unwrap().ty() {
                    MaybeTy::Simple(ty) => ty,
                    MaybeTy::Function(_) => MaybeTySimple::Expected(TySimple::Nil),
                };

                match_type(&ty, &expected_type)
                    .map_err(|_| format!("expected {expected_type:?}, got {ty:?}"))?;

                env.insert(name.clone(), MaybeTy::Simple(ty.clone()));

                Ok(MaybeTypedDecl::Ass {
                    name: name.clone(),
                    expr: decls,
                    ty,
                })
            }

            Decl::Fun {
                name,
                args,
                ret,
                body,
            } => {
                let args = args
                    .iter()
                    .cloned()
                    .map(|(arg, ty)| {
                        let ty = MaybeTySimple::parse(&arg, &ty)?;
                        Ok((arg, ty))
                    })
                    .collect::<Result<Vec<_>>>()?;
                let (args, types): (Vec<_>, Vec<_>) = args.into_iter().unzip();

                let ty_ret = MaybeTySimple::parse(&name, &ret)?;
                let ty_function = MaybeTyFunction {
                    args: types.clone(),
                    ret: ty_ret.clone(),
                };

                let mut scoped_env = HashMap::new();
                scoped_env.insert(name.clone(), MaybeTy::Function(ty_function.clone())); // allows recursion
                scoped_env.extend(
                    args.iter()
                        .cloned()
                        .zip(types.iter().cloned().map(MaybeTy::Simple))
                        .collect::<HashMap<_, _>>(),
                );

                let decls = check_decls(body, &mut scoped_env)?;
                let provided_ty_ret = match decls.last().unwrap().ty() {
                    MaybeTy::Simple(ty) => ty,
                    MaybeTy::Function(_) => MaybeTySimple::Expected(TySimple::Nil),
                };

                match_type(&provided_ty_ret, &ty_ret).map_err(|_| {
                    format!("Expected {ty_ret:?} return value, got {provided_ty_ret:?}")
                })?;

                env.insert(name.clone(), MaybeTy::Function(ty_function.clone()));

                Ok(MaybeTypedDecl::Fun {
                    name: name.clone(),
                    args,
                    body: decls,
                    ty: ty_function,
                })
            }
        })
        .collect()
}

fn check_expr(expr: &Expr, env: &mut HashMap<String, MaybeTy>) -> Result<MaybeTypedExpr> {
    match &expr.kind {
        ExprKind::Lit { lit: LitKind::Nil } => Ok(MaybeTypedExpr {
            kind: ExprKind::Lit { lit: LitKind::Nil },
            ty: MaybeTySimple::Expected(TySimple::Nil),
        }),

        ExprKind::Lit {
            lit: LitKind::Bool(val),
        } => Ok(MaybeTypedExpr {
            kind: ExprKind::Lit {
                lit: LitKind::Bool(*val),
            },
            ty: MaybeTySimple::Expected(TySimple::Bool),
        }),

        ExprKind::Lit {
            lit: LitKind::Num(val),
        } => Ok(MaybeTypedExpr {
            kind: ExprKind::Lit {
                lit: LitKind::Num(*val),
            },
            ty: MaybeTySimple::Expected(TySimple::Num),
        }),

        ExprKind::Ident { name } => match env.get(name) {
            Some(MaybeTy::Simple(ty)) => Ok(MaybeTypedExpr {
                kind: ExprKind::Ident { name: name.clone() },
                ty: ty.clone(),
            }),
            Some(MaybeTy::Function(..)) => Err(format!("{name} is not a value, but a function")),
            None => Err(format!("{name} not in env")),
        },

        ExprKind::UnOp { kind, expr } => {
            let expr = check_expr(expr, env)?;
            let ty = expr.ty.clone();
            match kind {
                UnOpKind::Not => {
                    match_type(&ty, &MaybeTySimple::Expected(TySimple::Bool))
                        .map_err(|_| format!("can't not {ty:?}"))?;
                    Ok(MaybeTypedExpr {
                        kind: ExprKind::UnOp {
                            kind: kind.clone(),
                            expr: Box::new(expr),
                        },
                        ty: MaybeTySimple::Expected(TySimple::Bool),
                    })
                }
                UnOpKind::Neg => {
                    match_type(&ty, &MaybeTySimple::Expected(TySimple::Num))
                        .map_err(|_| format!("can't neg {ty:?}"))?;
                    Ok(MaybeTypedExpr {
                        kind: ExprKind::UnOp {
                            kind: kind.clone(),
                            expr: Box::new(expr),
                        },
                        ty: MaybeTySimple::Expected(TySimple::Num),
                    })
                }
            }
        }

        ExprKind::BinOp { kind, left, right } => {
            let left = check_expr(left, env)?;
            let right = check_expr(right, env)?;

            let (left_ty, right_ty) = (left.ty.clone(), right.ty.clone());
            match_type(&left_ty, &right_ty).map_err(|_| {
                format!("expected same type on both sides: left: {left_ty:?}; right: {right_ty:?}")
            })?;

            let ty = match kind {
                BinOpKind::Eq | BinOpKind::Neq => MaybeTySimple::Expected(TySimple::Bool),
                BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
                    match_type(&left_ty, &MaybeTySimple::Expected(TySimple::Num))
                        .map_err(|_| format!("expected type to be Num: got {left_ty:?}"))?;
                    MaybeTySimple::Expected(TySimple::Num)
                }
                BinOpKind::Gt | BinOpKind::Gte | BinOpKind::Lt | BinOpKind::Lte => {
                    match_type(&left_ty, &MaybeTySimple::Expected(TySimple::Num))
                        .map_err(|_| format!("expected type to be Num: got {left_ty:?}"))?;
                    MaybeTySimple::Expected(TySimple::Bool)
                }
                BinOpKind::And | BinOpKind::Or => {
                    match_type(&left_ty, &MaybeTySimple::Expected(TySimple::Bool))
                        .map_err(|_| format!("expected type to be Bool: got {left_ty:?}"))?;
                    MaybeTySimple::Expected(TySimple::Bool)
                }
            };

            Ok(MaybeTypedExpr {
                kind: ExprKind::BinOp {
                    kind: kind.clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty,
            })
        }

        ExprKind::If { cond, then, else_ } => {
            let cond = check_expr(cond, env)?;
            let then = check_expr(then, env)?;
            let (cond_ty, then_ty) = (cond.ty.clone(), then.ty.clone());
            let else_ = else_
                .as_ref()
                .map(|else_| check_expr(else_, env))
                .transpose()?;

            match_type(&cond_ty, &MaybeTySimple::Expected(TySimple::Bool))
                .map_err(|_| "condition should be bool".to_string())?;

            if let Some(else_) = else_ {
                match_type(&then_ty, &else_.ty).map_err(|_| {
                    format!("expect then and else branch to be of same type: {then_ty:?} and {else_ty:?}", else_ty=else_.ty)
                })?;
                Ok(MaybeTypedExpr {
                    kind: ExprKind::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Some(Box::new(else_)),
                    },
                    ty: then_ty,
                })
            } else {
                match_type(&then_ty, &MaybeTySimple::Expected(TySimple::Nil)).map_err(|_| {
                    format!("expect then branch to be of type nil: got {then_ty:?}")
                })?;
                Ok(MaybeTypedExpr {
                    kind: ExprKind::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: None,
                    },
                    ty: MaybeTySimple::Expected(TySimple::Nil),
                })
            }
        }

        ExprKind::While { cond, do_ } => {
            let cond = check_expr(cond, env)?;
            let do_ = check_expr(do_, env)?;

            match_type(&cond.ty, &MaybeTySimple::Expected(TySimple::Bool))
                .map_err(|_| "condition should be bool".to_string())?;

            Ok(MaybeTypedExpr {
                kind: ExprKind::While {
                    cond: Box::new(cond),
                    do_: Box::new(do_),
                },
                ty: MaybeTySimple::Expected(TySimple::Nil),
            })
        }

        ExprKind::Call { name, args } => {
            let (function_args_ty, function_ret_ty) = match env.get(name).cloned() {
                Some(MaybeTy::Function(MaybeTyFunction { args, ret })) => Ok((args, ret)),
                Some(MaybeTy::Simple(..)) => Err(format!("{name} is not a function, but a value")),
                None => Err(format!("{name} not in env")),
            }?;

            if function_args_ty.len() != args.len() {
                return Err(format!(
                    "expected {} arguments for {name}, got {}",
                    function_args_ty.len(),
                    args.len()
                ));
            }

            let args = args
                .iter()
                .zip(function_args_ty)
                .enumerate()
                .map(|(index, (arg, expected_ty))| {
                    let expr = check_expr(arg, env)?;
                    let provided_ty = expr.ty.clone();
                    match_type(&provided_ty, &expected_ty).map_err(|_| {
                        format!(
                            "Expected {expected_ty:?} for argument {index}, got {provided_ty:?}"
                        )
                    })?;
                    Ok(expr)
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(MaybeTypedExpr {
                kind: ExprKind::Call {
                    name: name.clone(),
                    args,
                },
                ty: function_ret_ty,
            })
        }

        ExprKind::Print { expr } => Ok(MaybeTypedExpr {
            kind: ExprKind::Print {
                expr: Box::new(check_expr(expr, env)?),
            },
            ty: MaybeTySimple::Expected(TySimple::Nil),
        }),

        ExprKind::Block { decls } => {
            let decls = check_decls(decls, env)?;
            let ty = match decls.last().unwrap().ty() {
                MaybeTy::Simple(ty) => ty,
                MaybeTy::Function(_) => MaybeTySimple::Expected(TySimple::Nil),
            };
            Ok(MaybeTypedExpr {
                kind: ExprKind::Block { decls },
                ty,
            })
        }
    }
}

fn maybe_typed_expr_to_typed_expr(maybe: MaybeTypedExpr) -> Result<TypedExpr> {
    let ty = maybe.ty.get_type()?;

    match maybe.kind {
        ExprKind::Lit { lit } => Ok(TypedExpr {
            kind: ExprKind::Lit { lit },
            ty,
        }),
        ExprKind::Ident { name } => Ok(TypedExpr {
            kind: ExprKind::Ident { name },
            ty,
        }),
        ExprKind::UnOp { kind, expr } => Ok(TypedExpr {
            kind: ExprKind::UnOp {
                kind,
                expr: Box::new(maybe_typed_expr_to_typed_expr(*expr)?),
            },
            ty,
        }),
        ExprKind::BinOp { kind, left, right } => Ok(TypedExpr {
            kind: ExprKind::BinOp {
                kind,
                left: Box::new(maybe_typed_expr_to_typed_expr(*left)?),
                right: Box::new(maybe_typed_expr_to_typed_expr(*right)?),
            },
            ty,
        }),
        ExprKind::If { cond, then, else_ } => Ok(TypedExpr {
            kind: ExprKind::If {
                cond: Box::new(maybe_typed_expr_to_typed_expr(*cond)?),
                then: Box::new(maybe_typed_expr_to_typed_expr(*then)?),
                else_: else_
                    .map(|else_| maybe_typed_expr_to_typed_expr(*else_))
                    .transpose()?
                    .map(Box::new),
            },
            ty,
        }),
        ExprKind::While { cond, do_ } => Ok(TypedExpr {
            kind: ExprKind::While {
                cond: Box::new(maybe_typed_expr_to_typed_expr(*cond)?),
                do_: Box::new(maybe_typed_expr_to_typed_expr(*do_)?),
            },
            ty,
        }),
        ExprKind::Call { name, args } => Ok(TypedExpr {
            kind: ExprKind::Call {
                name,
                args: args
                    .into_iter()
                    .map(maybe_typed_expr_to_typed_expr)
                    .collect::<Result<_>>()?,
            },
            ty,
        }),

        ExprKind::Print { expr } => Ok(TypedExpr {
            kind: ExprKind::Print {
                expr: Box::new(maybe_typed_expr_to_typed_expr(*expr)?),
            },
            ty,
        }),

        ExprKind::Block { decls } => Ok(TypedExpr {
            kind: ExprKind::Block {
                decls: decls
                    .into_iter()
                    .map(maybe_typed_decl_to_typed_decl)
                    .collect::<Result<_>>()?,
            },
            ty,
        }),
    }
}

fn maybe_typed_decl_to_typed_decl(maybe: MaybeTypedDecl) -> Result<TypedDecl> {
    match maybe {
        MaybeTypedDecl::Stm { expr, ty } => {
            let ty = ty.get_type()?;

            Ok(TypedDecl::Stm {
                expr: maybe_typed_expr_to_typed_expr(expr)?,
                ty,
            })
        }
        MaybeTypedDecl::Ass { name, expr, ty } => {
            let ty = ty.get_type()?;

            Ok(TypedDecl::Ass {
                name,
                expr: expr
                    .into_iter()
                    .map(maybe_typed_decl_to_typed_decl)
                    .collect::<Result<Vec<_>>>()?,
                ty,
            })
        }
        MaybeTypedDecl::Fun {
            name,
            args,
            body,
            ty,
        } => {
            let ty = TyFunction {
                args: ty
                    .args
                    .into_iter()
                    .map(|ty| ty.get_type())
                    .collect::<Result<Vec<_>>>()?,
                ret: ty.ret.get_type()?,
            };

            Ok(TypedDecl::Fun {
                name,
                args,
                body: body
                    .into_iter()
                    .map(maybe_typed_decl_to_typed_decl)
                    .collect::<Result<Vec<_>>>()?,
                ty,
            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::macros::*;
    use crate::typed_ast::macros::*;
    use crate::util::macros::eenv;

    fn check_expr(expr: &Expr, env: &mut HashMap<String, MaybeTy>) -> Result<TypedExpr> {
        maybe_typed_expr_to_typed_expr(super::check_expr(expr, env)?)
    }

    fn check_decls(decls: &[Decl], env: &mut HashMap<String, MaybeTy>) -> Result<Vec<TypedDecl>> {
        let maybe_decls = super::check_decls(decls, env)?;

        maybe_decls
            .into_iter()
            .map(maybe_typed_decl_to_typed_decl)
            .collect::<Result<Vec<_>>>()
    }

    #[test]
    fn test_check_expr() {
        assert_eq!(check_expr(&nil!(), &mut eenv!()), Ok(tnil!()));

        assert_eq!(
            check_expr(
                &ident!(a),
                &mut eenv!(a => MaybeTy::Simple(MaybeTySimple::Expected(TySimple::Bool)))
            ),
            Ok(tident!(a, Bool))
        );

        assert_eq!(
            check_expr(&add!(num!(1), num!(2)), &mut eenv!()),
            Ok(tadd!(tnum!(1), tnum!(2), Num))
        );

        assert_eq!(
            check_expr(&add!(num!(1), nil!()), &mut eenv!()),
            Err("expected same type on both sides: left: Num; right: Nil".to_string())
        );
    }

    #[test]
    fn test_check_expr_if() {
        assert_eq!(
            check_expr(&if_!(bool!(true), nil!()), &mut eenv!()),
            Ok(tif!(tbool!(true), tnil!(); Nil))
        );

        assert_eq!(
            check_expr(&if_!(bool!(true), num!(1), num!(2)), &mut eenv!()),
            Ok(tif!(tbool!(true), tnum!(1), tnum!(2); Num))
        );

        assert_eq!(
            check_expr(&if_!(bool!(true), num!(1)), &mut eenv!()),
            Err("expect then branch to be of type nil: got Num".into())
        );
    }

    #[test]
    fn test_check_expr_call() {
        assert_eq!(
            check_expr(
                &call!(foo(num!(1))),
                &mut eenv!(foo => MaybeTy::Function(MaybeTyFunction { args: vec![MaybeTySimple::Expected(TySimple::Num)], ret: MaybeTySimple::Expected(TySimple::Bool) }))
            ),
            Ok(tcall!(foo(tnum!(1)); Bool))
        );

        assert_eq!(
            check_expr(
                &call!(foo(num!(1))),
                &mut eenv!(foo => MaybeTy::Function(MaybeTyFunction { args: vec![MaybeTySimple::Expected(TySimple::Bool)], ret: MaybeTySimple::Expected(TySimple::Bool) }))
            ),
            Err("Expected Bool for argument 0, got Num".into())
        );

        assert_eq!(
            check_expr(
                &call!(foo(num!(1))),
                &mut eenv!(foo => MaybeTy::Function(MaybeTyFunction { args: vec![], ret: MaybeTySimple::Expected(TySimple::Bool) }))
            ),
            Err("expected 0 arguments for foo, got 1".into())
        );
    }

    #[test]
    fn test_check_decls_ass() {
        assert_eq!(
            check_decls(&vec![ass!(foo = num!(1))], &mut eenv!()),
            Ok(vec![tass!(foo: Num = tnum!(1))])
        );

        assert_eq!(
            check_decls(&vec![ass!(foo: Num = num!(1))], &mut eenv!()),
            Ok(vec![tass!(foo: Num = tnum!(1))])
        );

        assert_eq!(
            check_decls(&vec![ass!(foo: Nil = num!(1))], &mut eenv!()),
            Err("expected Nil, got Num".into())
        );
    }

    #[test]
    fn test_check_decls_fun() {
        assert_eq!(
            check_decls(
                &vec![fun!(inc(val:Num):Num => add!(ident!(val), num!(1)))],
                &mut eenv!()
            ),
            Ok(vec![tfun!(
                inc(val:Num):Num => tadd!(tident!(val, Num), tnum!(1), Num)
            )])
        );

        assert_eq!(
            check_decls(
                &vec![fun!(inc(val:Bool):Num => add!(ident!(val), num!(1)))],
                &mut eenv!()
            ),
            Err("expected same type on both sides: left: Bool; right: Num".into())
        );

        assert_eq!(
            check_decls(
                &vec![fun!(inc(val:Num):Bool => add!(ident!(val), num!(1)))],
                &mut eenv!()
            ),
            Err("Expected Bool return value, got Num".into())
        );
    }

    #[test]
    fn test_check_decls_fun_call() {
        assert_eq!(
            check_decls(
                &vec![fun!(inc(val) => add!(ident!(val), num!(1)))],
                &mut eenv!()
            ),
            Ok(vec![tfun!(
                inc(val:Num):Num => tadd!(tident!(val, Num), tnum!(1), Num)
            )])
        );
    }
}
