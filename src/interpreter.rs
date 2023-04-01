use super::ast::*;

type Result<T> = std::result::Result<T, String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
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

pub struct Interpreter<'a> {
    program: &'a Program,
    vars: Vec<Var>,
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            vars: Vec::new(),
        }
    }

    pub fn execute(&mut self) -> Result<Value> {
        let mut last = Value::Nil;

        for decl in self.program.decls() {
            last = match decl {
                Decl::Ass { name, expr } => {
                    let value = eval(expr, &self.vars)?;
                    self.vars.push(Var {
                        name: name.clone(),
                        kind: Kind::Val { value },
                    });
                    Value::Nil
                }
                Decl::Fun { name, args, body } => {
                    self.vars.push(Var {
                        name: name.clone(),
                        kind: Kind::Fun {
                            args: args.clone(),
                            body: body.clone(),
                        },
                    });
                    Value::Nil
                }
                Decl::Stm { expr } => eval(expr, &self.vars)?,
            }
        }

        Ok(last)
    }
}

fn eval(expr: &Expr, vars: &[Var]) -> Result<Value> {
    match expr {
        Expr::Lit(Lit::Nil) => Ok(Value::Nil),
        Expr::Lit(Lit::Bool(val)) => Ok(Value::Bool(*val)),
        Expr::Lit(Lit::Num(val)) => Ok(Value::Num(*val)),
        Expr::Lit(Lit::Str(val)) => Ok(Value::Str(val.clone())),

        Expr::Ident(name) => {
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

        Expr::UnOp(UnOp::Neg, expr) => {
            let val = eval(expr, vars)?;
            if let Value::Num(val) = val {
                Ok(Value::Num(-val))
            } else {
                Err(format!("can't negate {val:?}"))
            }
        }

        Expr::UnOp(UnOp::Not, expr) => {
            let val = eval(expr, vars)?;
            if let Value::Bool(val) = val {
                Ok(Value::Bool(!val))
            } else {
                Err(format!("can't not {val:?}"))
            }
        }

        Expr::BinOp(BinOp::Add, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left + right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} + {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Sub, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left - right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} - {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Mul, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left * right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} * {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Div, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left / right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} / {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Eq, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
                (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left == right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left == right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left == right)),
                _ => Err(format!(
                    "expect same type on both sides: {left_val:?} == {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Neq, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
                (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left != right)),
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left != right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left != right)),
                _ => Err(format!(
                    "expect same type on both sides: {left_val:?} != {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Lt, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left < right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left < right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} < {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Lte, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left <= right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left <= right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} <= {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Gt, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left > right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left > right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} > {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::Gte, left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left >= right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left >= right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} >= {right_val:?}"
                )),
            }
        }

        Expr::BinOp(BinOp::And, left, right) => {
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

        Expr::BinOp(BinOp::Or, left, right) => {
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

        Expr::If(cond, then, else_) => {
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

        Expr::Call(name, args) => {
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
            eval(&add!(num!(1.0), bool!(true)), &Vec::new()),
            Err("expect num on both sides: Num(1.0) + Bool(true)".into())
        );

        assert_eq!(
            eval(&not!(bool!(true)), &Vec::new()),
            Ok(Value::Bool(false))
        );

        assert_eq!(eval(&neg!(num!(3.0)), &Vec::new()), Ok(Value::Num(-3.0)));

        assert_eq!(
            eval(
                &or!(and!(bool!(true), bool!(false)), bool!(true)),
                &Vec::new()
            ),
            Ok(Value::Bool(true))
        );

        assert_eq!(
            eval(
                &sub!(add!(num!(1.0), mul!(num!(2.0), num!(3.0))), num!(4.0)),
                &Vec::new()
            ),
            Ok(Value::Num(3.0))
        );

        assert_eq!(
            eval(
                &mul!(add!(num!(1.0), num!(2.0)), sub!(num!(3.0), num!(4.0))),
                &Vec::new()
            ),
            Ok(Value::Num(-3.0))
        );

        assert_eq!(
            eval(
                &or!(
                    eq!(
                        lte!(add!(num!(1.0), num!(2.0)), neg!(num!(4.0))),
                        bool!(false)
                    ),
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
            eval(&iff!(bool!(true), num!(1.0), num!(2.0)), &Vec::new()),
            Ok(Value::Num(1.0))
        );

        assert_eq!(
            eval(&iff!(bool!(false), num!(1.0), num!(2.0)), &Vec::new()),
            Ok(Value::Num(2.0))
        );

        assert_eq!(
            eval(&iff!(bool!(true), num!(1.0)), &Vec::new()),
            Ok(Value::Num(1.0))
        );

        assert_eq!(
            eval(&iff!(bool!(false), num!(1.0)), &Vec::new()),
            Ok(Value::Nil)
        );

        assert_eq!(
            eval(&iff!(nil!(), num!(1.0)), &Vec::new()),
            Err("expect bool as condition for if statement: Nil".into())
        );
    }

    #[test]
    fn test_execute_assignments() {
        let decls = vec![
            ass!(foo, num!(1.0)),
            ass!(bar, num!(2.0)),
            stm!(eq!(add!(ident!(foo), ident!(bar)), num!(3.0))),
        ];

        let program = Program::new(decls);
        let mut interpreter = Interpreter::new(&program);

        assert_eq!(interpreter.execute(), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_execute_function_call() {
        let decls = vec![
            fun!(add, [a, b], add!(ident!(a), ident!(b))),
            stm!(call!(add, num!(1.0), num!(2.0))),
        ];

        let program = Program::new(decls);
        let mut interpreter = Interpreter::new(&program);

        assert_eq!(interpreter.execute(), Ok(Value::Num(3.0)));
    }
}
