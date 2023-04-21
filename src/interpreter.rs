use super::ast::*;
use super::util::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Num(i64),
}

#[derive(Clone, Debug)]
struct Var {
    name: String,
    kind: Kind,
}

#[derive(Clone, Debug)]
enum Kind {
    Val { value: Value },
    Fun { args: Vec<String>, body: Expr },
}

pub fn execute(program: &Program) -> Result<Value> {
    program
        .decls()
        .iter()
        .try_fold_with_context(Value::Nil, vec![], |vars, decl| match decl {
            Decl::Ass { name, expr } => {
                let value = eval(expr, vars)?;
                vars.push(Var {
                    name: name.0.clone(),
                    kind: Kind::Val { value },
                });
                Ok(Value::Nil)
            }
            Decl::Fun {
                name,
                args,
                ret: _ret,
                body,
            } => {
                if body.len() != 1 { return Err("Expect only 1 body decl".to_string()) }
                let Decl::Stm { expr: body } = &body[0] else { return Err("Expect stm decl".to_string()) };

                vars.push(Var {
                    name: name.clone(),
                    kind: Kind::Fun {
                        args: args.iter().map(|arg| arg.0.clone()).collect(),
                        body: body.clone(),
                    },
                });
                Ok(Value::Nil)
            }
            Decl::Stm { expr } => eval(expr, vars),
        })
}

fn eval(expr: &Expr, vars: &[Var]) -> Result<Value> {
    match &expr.kind {
        ExprKind::Lit { lit: LitKind::Nil } => Ok(Value::Nil),

        ExprKind::Lit {
            lit: LitKind::Bool(val),
        } => Ok(Value::Bool(*val)),

        ExprKind::Lit {
            lit: LitKind::Num(val),
        } => Ok(Value::Num(*val)),

        ExprKind::Ident { name } => {
            if let Some(Var { kind, .. }) = vars.iter().rev().find(|var| var.name == *name) {
                if let Kind::Val { value } = kind {
                    Ok(value.clone())
                } else {
                    Err(format!("Variable `{}` is a function, not a value", name))
                }
            } else {
                Err(format!("Cannot find variable `{}` in scope", name))
            }
        }

        ExprKind::UnOp {
            kind: UnOpKind::Neg,
            expr,
        } => {
            let val = eval(expr, vars)?;
            if let Value::Num(val) = val {
                Ok(Value::Num(-val))
            } else {
                Err(format!("can't negate {val:?}"))
            }
        }

        ExprKind::UnOp {
            kind: UnOpKind::Not,
            expr,
        } => {
            let val = eval(expr, vars)?;
            if let Value::Bool(val) = val {
                Ok(Value::Bool(!val))
            } else {
                Err(format!("can't not {val:?}"))
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Add,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left + right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} + {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Sub,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left - right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} - {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Mul,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left * right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} * {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Div,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left / right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} / {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Eq,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
                (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left == right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left == right)),
                _ => Err(format!(
                    "expect same type on both sides: {left_val:?} == {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Neq,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
                (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left != right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left != right)),
                _ => Err(format!(
                    "expect same type on both sides: {left_val:?} != {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Lt,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left < right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} < {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Lte,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left <= right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} <= {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Gt,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left > right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} > {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Gte,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left >= right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} >= {right_val:?}"
                )),
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::And,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            if let Value::Bool(left_val) = left_val {
                if left_val {
                    let right_val = eval(right, vars)?;
                    if let Value::Bool(right_val) = right_val {
                        Ok(Value::Bool(right_val))
                    } else {
                        Err(format!(
                            "expect bool on right side with && operator: {right_val:?}"
                        ))
                    }
                } else {
                    Ok(Value::Bool(false))
                }
            } else {
                Err(format!(
                    "expect bool on left side with && operator: {left_val:?}"
                ))
            }
        }

        ExprKind::BinOp {
            kind: BinOpKind::Or,
            left,
            right,
        } => {
            let left_val = eval(left, vars)?;
            if let Value::Bool(left_val) = left_val {
                if left_val {
                    Ok(Value::Bool(true))
                } else {
                    let right_val = eval(right, vars)?;
                    if let Value::Bool(right_val) = right_val {
                        Ok(Value::Bool(right_val))
                    } else {
                        Err(format!(
                            "expect bool on right side with || operator: {right_val:?}"
                        ))
                    }
                }
            } else {
                Err(format!(
                    "expect bool on left side with || operator: {left_val:?}"
                ))
            }
        }

        ExprKind::If { cond, then, else_ } => {
            let cond_val = eval(cond, vars)?;

            if let Value::Bool(cond_val) = cond_val {
                if cond_val {
                    let then_val = eval(then, vars)?;
                    Ok(then_val)
                } else if let Some(else_) = else_ {
                    let else_val = eval(else_, vars)?;
                    Ok(else_val)
                } else {
                    Ok(Value::Nil)
                }
            } else {
                Err(format!(
                    "expect bool as condition for if statement: {cond_val:?}"
                ))
            }
        }

        ExprKind::Call { name, args } => {
            let var = vars.iter().rev().find(|var| var.name == *name);

            if let Some(Var { kind, .. }) = var {
                if let Kind::Fun {
                    args: arg_names,
                    body,
                } = kind
                {
                    if arg_names.len() == args.len() {
                        let mut args = args
                            .iter()
                            .zip(arg_names.iter())
                            .map(|(arg, name)| {
                                Ok(Var {
                                    name: name.clone(),
                                    kind: Kind::Val {
                                        value: eval(arg, vars)?,
                                    },
                                })
                            })
                            .collect::<Result<_>>()?;
                        let mut cloned_vars = vars.to_owned();
                        cloned_vars.append(&mut args);
                        eval(body, &cloned_vars)
                    } else {
                        Err(format!(
                            "Wrong number of arguments for function `{}`: expected {}, found {}",
                            name,
                            arg_names.len(),
                            args.len()
                        ))
                    }
                } else {
                    Err(format!("Variable `{}` is a value, not a function", name))
                }
            } else {
                Err(format!("Cannot find variable `{}` in scope", name))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::macros::*;

    #[test]
    fn test_eval() {
        assert_eq!(
            eval(&add!(num!(1), bool!(true)), &Vec::new()),
            Err("expect num on both sides: Num(1) + Bool(true)".into())
        );

        assert_eq!(
            eval(&not!(bool!(true)), &Vec::new()),
            Ok(Value::Bool(false))
        );

        assert_eq!(eval(&neg!(num!(3)), &Vec::new()), Ok(Value::Num(-3)));

        assert_eq!(
            eval(
                &or!(and!(bool!(true), bool!(false)), bool!(true)),
                &Vec::new()
            ),
            Ok(Value::Bool(true))
        );

        assert_eq!(
            eval(
                &sub!(add!(num!(1), mul!(num!(2), num!(3))), num!(4)),
                &Vec::new()
            ),
            Ok(Value::Num(3))
        );

        assert_eq!(
            eval(
                &mul!(add!(num!(1), num!(2)), sub!(num!(3), num!(4))),
                &Vec::new()
            ),
            Ok(Value::Num(-3))
        );

        assert_eq!(
            eval(
                &or!(
                    eq!(lte!(add!(num!(1), num!(2)), neg!(num!(4))), bool!(false)),
                    not!(bool!(true))
                ),
                &Vec::new()
            ),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_execute_if_statement() {
        assert_eq!(
            eval(&if_!(bool!(true), num!(1), num!(2)), &Vec::new()),
            Ok(Value::Num(1))
        );

        assert_eq!(
            eval(&if_!(bool!(false), num!(1), num!(2)), &Vec::new()),
            Ok(Value::Num(2))
        );

        assert_eq!(
            eval(&if_!(bool!(true), num!(1)), &Vec::new()),
            Ok(Value::Num(1))
        );

        assert_eq!(
            eval(&if_!(bool!(false), num!(1)), &Vec::new()),
            Ok(Value::Nil)
        );

        assert_eq!(
            eval(&if_!(nil!(), num!(1)), &Vec::new()),
            Err("expect bool as condition for if statement: Nil".into())
        );
    }

    #[test]
    fn test_execute_assignments() {
        let program = Program::new(vec![
            ass!(foo = num!(1)),
            ass!(bar = num!(2)),
            stm!(eq!(add!(ident!(foo), ident!(bar)), num!(3))),
        ]);

        assert_eq!(execute(&program), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_execute_function_call() {
        let program = Program::new(vec![
            fun!(add(a, b) => add!(ident!(a), ident!(b))),
            stm!(call!(add(num!(1), num!(2)))),
        ]);

        assert_eq!(execute(&program), Ok(Value::Num(3)));
    }
}
