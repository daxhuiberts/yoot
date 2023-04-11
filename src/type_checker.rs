use std::collections::HashMap;
use std::str::FromStr;

use super::ast::*;
use super::typed_ast::*;
use super::util::*;

pub fn type_check(program: &Program) -> Result<TypedProgram> {
    Ok(TypedProgram::new(check_decls(program.decls())?))
}

pub fn check_decls(decls: &[Decl]) -> Result<Vec<TypedDecl>> {
    let mut env = HashMap::new();
    let mut typed_decls = vec![];

    for decl in decls {
        match decl {
            Decl::Stm { expr } => {
                let (expr, ty) = check_expr(expr, &env)?;
                typed_decls.push(TypedDecl::Stm { expr, ty });
            }
            Decl::Ass {
                name: (name, expected_type),
                expr,
            } => {
                let expected_type = expected_type
                    .clone()
                    .map(|exp_ty| TySimple::from_str(&exp_ty))
                    .transpose()?;
                let (expr, ty) = check_expr(expr, &env)?;
                if expected_type.clone().map_or(false, |ex| ex != ty) {
                    return Err(format!("expected {expected_type:?}, got {ty:?}"));
                }
                env.insert(name.clone(), Ty::Simple(ty.clone()));
                typed_decls.push(TypedDecl::Ass {
                    name: name.clone(),
                    expr,
                    ty,
                });
            }
            Decl::Fun { name, args, body } => {
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
                let local_env = args
                    .iter()
                    .cloned()
                    .zip(types.iter().cloned().map(Ty::Simple))
                    .collect::<HashMap<_, _>>();
                let (expr, ty) = check_expr(body, &local_env)?;
                let ty_function = TyFunction {
                    args: types,
                    ret: ty,
                };

                env.insert(name.clone(), Ty::Function(ty_function.clone()));
                typed_decls.push(TypedDecl::Fun {
                    name: name.clone(),
                    args: args,
                    body: expr,
                    ty: ty_function,
                })
            }
        }
    }

    Ok(typed_decls)
}

fn check_expr(expr: &Expr, env: &HashMap<String, Ty>) -> Result<(TypedExpr, TySimple)> {
    match expr {
        Expr::Lit(Lit::Nil) => Ok((TypedExpr::Lit(Lit::Nil), TySimple::Nil)),
        Expr::Lit(Lit::Bool(val)) => Ok((TypedExpr::Lit(Lit::Bool(*val)), TySimple::Bool)),
        Expr::Lit(Lit::Num(val)) => Ok((TypedExpr::Lit(Lit::Num(*val)), TySimple::Num)),
        Expr::Lit(Lit::Str(_)) => Err("str not supported".into()),
        Expr::Ident(name) => match env.get(name) {
            Some(Ty::Simple(ty)) => Ok((
                TypedExpr::Ident {
                    name: name.clone(),
                    ty: ty.clone(),
                },
                ty.clone(),
            )),
            Some(Ty::Function(..)) => Err(format!("{name} is not a value, but a function")),
            None => Err(format!("{name} not in env")),
        },
        Expr::UnOp(kind, expr) => {
            let (expr, ty) = check_expr(expr, env)?;
            match kind {
                UnOpKind::Not => match ty {
                    TySimple::Bool => Ok((
                        TypedExpr::UnOp {
                            kind: kind.clone(),
                            expr: Box::new(expr),
                            ty: TySimple::Bool,
                        },
                        TySimple::Bool,
                    )),
                    _ => Err(format!("can't not {ty:?}")),
                },
                UnOpKind::Neg => match ty {
                    TySimple::Num => Ok((
                        TypedExpr::UnOp {
                            kind: kind.clone(),
                            expr: Box::new(expr),
                            ty: TySimple::Num,
                        },
                        TySimple::Num,
                    )),
                    _ => Err(format!("can't neg {ty:?}")),
                },
            }
        }
        Expr::BinOp(kind, left, right) => {
            let (left, left_ty) = check_expr(left, env)?;
            let (right, right_ty) = check_expr(right, env)?;

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

            Ok((
                TypedExpr::BinOp {
                    kind: kind.clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                    ty: ty.clone(),
                },
                ty,
            ))
        }
        Expr::If(cond, then, else_) => {
            let (cond, cond_ty) = check_expr(cond, env)?;
            let (then, then_ty) = check_expr(then, env)?;
            let else_ = else_
                .as_ref()
                .map(|else_| check_expr(else_, env))
                .transpose()?;

            if cond_ty != TySimple::Bool {
                return Err("condition should be bool".to_string());
            }

            match (then_ty, else_) {
                (TySimple::Nil, None) => Ok((
                    TypedExpr::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: None,
                        ty: TySimple::Nil,
                    },
                    TySimple::Nil,
                )),
                (then_ty, None) => Err(format!(
                    "expect then branch to be of type nil: got {then_ty:?}"
                )),
                (then_ty, Some((else_, else_ty))) if then_ty == else_ty => Ok((
                    TypedExpr::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Some(Box::new(else_)),
                        ty: then_ty.clone(),
                    },
                    then_ty,
                )),
                (then_ty, Some((_, else_ty))) => Err(format!(
                    "expect then and else branch to be of same type: {then_ty:?} and {else_ty:?}"
                )),
            }
        }
        Expr::Call(name, args) => {
            let (function_args_ty, function_ret_ty) = match env.get(name) {
                Some(Ty::Function(TyFunction { args, ret })) => Ok((args, ret)),
                Some(Ty::Simple(..)) => Err(format!("{name} is not a function, but a value")),
                None => Err(format!("{name} not in env")),
            }?;
            let args = args
                .iter()
                .map(|arg| check_expr(arg, env))
                .collect::<Result<Vec<_>>>()?;
            if function_args_ty.len() != args.len() {
                return Err(format!(
                    "expected {} arguments for {name}, got {}",
                    function_args_ty.len(),
                    args.len()
                ));
            }
            let args = args
                .into_iter()
                .zip(function_args_ty)
                .enumerate()
                .map(|(index, ((expr, provided_ty), expected_ty))| {
                    if provided_ty != *expected_ty {
                        Err(format!(
                            "Expected {expected_ty:?} for argument {index}, got {provided_ty:?}"
                        ))
                    } else {
                        Ok(expr)
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            Ok((
                TypedExpr::Call {
                    name: name.clone(),
                    args,
                    ty: function_ret_ty.clone(),
                },
                function_ret_ty.clone(),
            ))
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
        assert_eq!(check_expr(&nil!(), &eenv!()), Ok((tnil!(), TySimple::Nil)));

        assert_eq!(
            check_expr(&ident!(a), &eenv!(a => Ty::Simple(TySimple::Bool))),
            Ok((tident!(a, Bool), TySimple::Bool))
        );

        assert_eq!(
            check_expr(&add!(num!(1), num!(2)), &eenv!()),
            Ok((tadd!(tnum!(1), tnum!(2), Num), TySimple::Num))
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
            Ok((tcall!(foo, tnum!(1); Bool), TySimple::Bool))
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
            check_decls(&vec![fun!(inc, [val: Num], add!(ident!(val), num!(1)))]),
            Ok(vec![tfun!(
                inc: (Num): Num,
                [val],
                tadd!(tident!(val, Num), tnum!(1), Num)
            )])
        );

        assert_eq!(
            check_decls(&vec![fun!(inc, [val: Bool], add!(ident!(val), num!(1)))]),
            Err("expected same type on both sides: left: Bool; right: Num".into())
        );
    }
}
