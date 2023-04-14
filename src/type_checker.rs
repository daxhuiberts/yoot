use std::collections::HashMap;
use std::str::FromStr;

use super::ast::*;
use super::typed_ast::*;
use super::util::*;

pub fn check(program: &Program) -> Result<TypedProgram> {
    Ok(TypedProgram::new(check_decls(program.decls())?))
}

pub fn check_decls(decls: &[Decl]) -> Result<Vec<TypedDecl>> {
    decls
        .iter()
        .map_with_context(HashMap::new(), |env, decl| match &decl {
            Decl::Stm { expr } => {
                let expr = check_expr(expr, &env)?;
                let ty = expr.ty.clone();

                Ok(TypedDecl::Stm { expr, ty })
            }

            Decl::Ass {
                name: (name, expected_type),
                expr,
            } => {
                let expected_type = expected_type
                    .clone()
                    .map(|exp_ty| TySimple::from_str(&exp_ty))
                    .transpose()?;
                let expr = check_expr(expr, &env)?;
                let ty = expr.ty.clone();
                if expected_type.clone().map_or(false, |ex| ex != ty) {
                    return Err(format!("expected {expected_type:?}, got {ty:?}"));
                }

                env.insert(name.clone(), Ty::Simple(ty.clone()));

                Ok(TypedDecl::Ass {
                    name: name.clone(),
                    expr,
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
                        let ty = ty.ok_or(String::from("missing type"))?;
                        let ty = TySimple::from_str(&ty)?;
                        Ok((arg, ty))
                    })
                    .collect::<Result<Vec<_>>>()?;
                let (args, types): (Vec<_>, Vec<_>) = args.into_iter().unzip();

                let ty_ret = ret.as_ref().ok_or(String::from("missing type"))?;
                let ty_ret = TySimple::from_str(&ty_ret)?;
                let ty_function = TyFunction {
                    args: types.clone(),
                    ret: ty_ret.clone(),
                };

                let mut scoped_env = env.clone();
                scoped_env.insert(name.clone(), Ty::Function(ty_function.clone()));
                scoped_env.extend(
                    args.iter()
                        .cloned()
                        .zip(types.iter().cloned().map(Ty::Simple))
                        .collect::<HashMap<_, _>>(),
                );
                let expr = check_expr(body, &scoped_env)?;

                let provided_ty_ret = expr.ty.clone();
                if provided_ty_ret != ty_ret {
                    return Err(format!(
                        "Expected {ty_ret:?} return value, got {provided_ty_ret:?}"
                    ));
                }

                env.insert(name.clone(), Ty::Function(ty_function.clone()));

                Ok(TypedDecl::Fun {
                    name: name.clone(),
                    args,
                    body: expr,
                    ty: ty_function,
                })
            }
        })
        .collect()
}

fn check_expr(expr: &Expr, env: &HashMap<String, Ty>) -> Result<TypedExpr> {
    match &expr.kind {
        ExprKind::Lit { lit: LitKind::Nil } => Ok(TypedExpr {
            kind: ExprKind::Lit { lit: LitKind::Nil },
            ty: TySimple::Nil,
        }),

        ExprKind::Lit {
            lit: LitKind::Bool(val),
        } => Ok(TypedExpr {
            kind: ExprKind::Lit {
                lit: LitKind::Bool(*val),
            },
            ty: TySimple::Bool,
        }),

        ExprKind::Lit {
            lit: LitKind::Num(val),
        } => Ok(TypedExpr {
            kind: ExprKind::Lit {
                lit: LitKind::Num(*val),
            },
            ty: TySimple::Num,
        }),

        ExprKind::Lit {
            lit: LitKind::Str(_),
        } => Err("str not supported".into()),

        ExprKind::Ident { name } => match env.get(name) {
            Some(Ty::Simple(ty)) => Ok(TypedExpr {
                kind: ExprKind::Ident { name: name.clone() },
                ty: ty.clone(),
            }),
            Some(Ty::Function(..)) => Err(format!("{name} is not a value, but a function")),
            None => Err(format!("{name} not in env")),
        },

        ExprKind::UnOp { kind, expr } => {
            let expr = check_expr(expr, env)?;
            let ty = expr.ty.clone();
            match kind {
                UnOpKind::Not => match ty {
                    TySimple::Bool => Ok(TypedExpr {
                        kind: ExprKind::UnOp {
                            kind: kind.clone(),
                            expr: Box::new(expr),
                        },
                        ty: TySimple::Bool,
                    }),
                    _ => Err(format!("can't not {ty:?}")),
                },
                UnOpKind::Neg => match ty {
                    TySimple::Num => Ok(TypedExpr {
                        kind: ExprKind::UnOp {
                            kind: kind.clone(),
                            expr: Box::new(expr),
                        },
                        ty: TySimple::Num,
                    }),
                    _ => Err(format!("can't neg {ty:?}")),
                },
            }
        }

        ExprKind::BinOp { kind, left, right } => {
            let left = check_expr(left, env)?;
            let right = check_expr(right, env)?;

            let (left_ty, right_ty) = (left.ty.clone(), right.ty.clone());
            if left_ty != right_ty {
                return Err(format!(
                    "expected same type on both sides: left: {left_ty:?}; right: {right_ty:?}"
                ));
            }

            let ty = match kind {
                BinOpKind::Eq | BinOpKind::Neq => left_ty,
                BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
                    if left_ty != TySimple::Num {
                        return Err(format!("expected type to be Num: got {left_ty:?}"));
                    } else {
                        TySimple::Num
                    }
                }
                BinOpKind::Gt | BinOpKind::Gte | BinOpKind::Lt | BinOpKind::Lte => {
                    if left_ty != TySimple::Num {
                        return Err(format!("expected type to be Num: got {left_ty:?}"));
                    } else {
                        TySimple::Bool
                    }
                }
                BinOpKind::And | BinOpKind::Or => {
                    if left_ty != TySimple::Bool {
                        return Err(format!("expected type to be Bool: got {left_ty:?}"));
                    } else {
                        TySimple::Bool
                    }
                }
            };

            Ok(TypedExpr {
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

            if cond_ty != TySimple::Bool {
                return Err("condition should be bool".to_string());
            }

            let else_ty = else_.as_ref().map(|e| e.ty.clone());
            match (then_ty, else_ty, else_) {
                (TySimple::Nil, None, _) => Ok(TypedExpr {
                    kind: ExprKind::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: None,
                    },
                    ty: TySimple::Nil,
                }),
                (then_ty, None, _) => Err(format!(
                    "expect then branch to be of type nil: got {then_ty:?}"
                )),
                (then_ty, Some(else_ty), Some(else_)) if then_ty == else_ty => Ok(TypedExpr {
                    kind: ExprKind::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Some(Box::new(else_)),
                    },
                    ty: then_ty,
                }),
                (then_ty, Some(else_ty), _) => Err(format!(
                    "expect then and else branch to be of same type: {then_ty:?} and {else_ty:?}"
                )),
            }
        }

        ExprKind::Call { name, args } => {
            let (function_args_ty, function_ret_ty) = match env.get(name) {
                Some(Ty::Function(TyFunction { args, ret })) => Ok((args, ret)),
                Some(Ty::Simple(..)) => Err(format!("{name} is not a function, but a value")),
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
                    if provided_ty != *expected_ty {
                        Err(format!(
                            "Expected {expected_ty:?} for argument {index}, got {provided_ty:?}"
                        ))
                    } else {
                        Ok(expr)
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(TypedExpr {
                kind: ExprKind::Call {
                    name: name.clone(),
                    args,
                },
                ty: function_ret_ty.clone(),
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

    #[test]
    fn test_check_expr() {
        assert_eq!(check_expr(&nil!(), &eenv!()), Ok(tnil!()));

        assert_eq!(
            check_expr(&ident!(a), &eenv!(a => Ty::Simple(TySimple::Bool))),
            Ok(tident!(a, Bool))
        );

        assert_eq!(
            check_expr(&add!(num!(1), num!(2)), &eenv!()),
            Ok(tadd!(tnum!(1), tnum!(2), Num))
        );

        assert_eq!(
            check_expr(&add!(num!(1), nil!()), &eenv!()),
            Err("expected same type on both sides: left: Num; right: Nil".to_string())
        );
    }

    #[test]
    fn test_check_expr_call() {
        assert_eq!(
            check_expr(
                &call!(foo, num!(1)),
                &eenv!(foo => Ty::Function(TyFunction { args: vec![TySimple::Num], ret: TySimple::Bool }))
            ),
            Ok(tcall!(foo, tnum!(1); Bool))
        );

        assert_eq!(
            check_expr(
                &call!(foo, num!(1)),
                &eenv!(foo => Ty::Function(TyFunction { args: vec![TySimple::Bool], ret: TySimple::Bool }))
            ),
            Err("Expected Bool for argument 0, got Num".into())
        );

        assert_eq!(
            check_expr(
                &call!(foo, num!(1)),
                &eenv!(foo => Ty::Function(TyFunction { args: vec![], ret: TySimple::Bool }))
            ),
            Err("expected 0 arguments for foo, got 1".into())
        );
    }

    #[test]
    fn test_check_decls_fun() {
        assert_eq!(
            check_decls(&vec![fun!(inc(val:Num):Num => add!(ident!(val), num!(1)))]),
            Ok(vec![tfun!(
                inc: (Num): Num,
                [val],
                tadd!(tident!(val, Num), tnum!(1), Num)
            )])
        );

        assert_eq!(
            check_decls(&vec![fun!(inc(val:Bool):Num => add!(ident!(val), num!(1)))]),
            Err("expected same type on both sides: left: Bool; right: Num".into())
        );

        assert_eq!(
            check_decls(&vec![fun!(inc(val:Num):Bool => add!(ident!(val), num!(1)))]),
            Err("Expected Bool return value, got Num".into())
        );
    }
}
