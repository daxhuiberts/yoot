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
        Expr::Nil => Ok(Value::Nil),
        Expr::Bool(val) => Ok(Value::Bool(*val)),
        Expr::Num(val) => Ok(Value::Num(*val)),
        Expr::Str(val) => Ok(Value::Str(val.clone())),

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

        Expr::Neg(expr) => {
            let val = eval(expr, vars)?;
            if let Value::Num(val) = val {
                Ok(Value::Num(-val))
            } else {
                Err(format!("can't negate {val:?}"))
            }
        }

        Expr::Not(expr) => {
            let val = eval(expr, vars)?;
            if let Value::Bool(val) = val {
                Ok(Value::Bool(!val))
            } else {
                Err(format!("can't not {val:?}"))
            }
        }

        Expr::Add(left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left + right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} + {right_val:?}"
                )),
            }
        }

        Expr::Sub(left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left - right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} - {right_val:?}"
                )),
            }
        }

        Expr::Mul(left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left * right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} * {right_val:?}"
                )),
            }
        }

        Expr::Div(left, right) => {
            let left_val = eval(left, vars)?;
            let right_val = eval(right, vars)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left / right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} / {right_val:?}"
                )),
            }
        }

        Expr::Eq(left, right) => {
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

        Expr::Neq(left, right) => {
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

        Expr::Lt(left, right) => {
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

        Expr::Lte(left, right) => {
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

        Expr::Gt(left, right) => {
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

        Expr::Gte(left, right) => {
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

        Expr::And(left, right) => {
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

        Expr::Or(left, right) => {
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

    #[test]
    fn test_eval() {
        assert_eq!(
            eval(
                &Expr::Add(Box::new(Expr::Num(1.0)), Box::new(Expr::Bool(true)),),
                &Vec::new()
            ),
            Err("expect num on both sides: Num(1.0) + Bool(true)".into())
        );

        assert_eq!(
            eval(&Expr::Not(Box::new(Expr::Bool(true))), &Vec::new()),
            Ok(Value::Bool(false))
        );

        assert_eq!(
            eval(&Expr::Neg(Box::new(Expr::Num(3.0))), &Vec::new()),
            Ok(Value::Num(-3.0))
        );

        assert_eq!(
            eval(
                &Expr::Or(
                    Box::new(Expr::And(
                        Box::new(Expr::Bool(true)),
                        Box::new(Expr::Bool(false)),
                    )),
                    Box::new(Expr::Bool(true)),
                ),
                &Vec::new()
            ),
            Ok(Value::Bool(true))
        );

        assert_eq!(
            eval(
                &Expr::Sub(
                    Box::new(Expr::Add(
                        Box::new(Expr::Num(1.0)),
                        Box::new(Expr::Mul(
                            Box::new(Expr::Num(2.0)),
                            Box::new(Expr::Num(3.0))
                        ))
                    )),
                    Box::new(Expr::Num(4.0))
                ),
                &Vec::new()
            ),
            Ok(Value::Num(3.0))
        );

        assert_eq!(
            eval(
                &Expr::Mul(
                    Box::new(Expr::Add(
                        Box::new(Expr::Num(1.0)),
                        Box::new(Expr::Num(2.0))
                    )),
                    Box::new(Expr::Sub(
                        Box::new(Expr::Num(3.0)),
                        Box::new(Expr::Num(4.0))
                    ))
                ),
                &Vec::new()
            ),
            Ok(Value::Num(-3.0))
        );

        assert_eq!(
            eval(
                &Expr::Or(
                    Box::new(Expr::Eq(
                        Box::new(Expr::Lte(
                            Box::new(Expr::Add(
                                Box::new(Expr::Num(1.0)),
                                Box::new(Expr::Num(2.0)),
                            )),
                            Box::new(Expr::Neg(Box::new(Expr::Num(4.0)))),
                        )),
                        Box::new(Expr::Bool(false)),
                    )),
                    Box::new(Expr::Not(Box::new(Expr::Bool(true)))),
                ),
                &Vec::new()
            ),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_execute_if_statement() {
        assert_eq!(
            eval(
                &Expr::If(
                    Box::new(Expr::Bool(true)),
                    Box::new(Expr::Num(1.0)),
                    Some(Box::new(Expr::Num(2.0))),
                ),
                &Vec::new()
            ),
            Ok(Value::Num(1.0))
        );

        assert_eq!(
            eval(
                &Expr::If(
                    Box::new(Expr::Bool(false)),
                    Box::new(Expr::Num(1.0)),
                    Some(Box::new(Expr::Num(2.0))),
                ),
                &Vec::new()
            ),
            Ok(Value::Num(2.0))
        );

        assert_eq!(
            eval(
                &Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Num(1.0)), None,),
                &Vec::new()
            ),
            Ok(Value::Num(1.0))
        );

        assert_eq!(
            eval(
                &Expr::If(Box::new(Expr::Bool(false)), Box::new(Expr::Num(1.0)), None,),
                &Vec::new()
            ),
            Ok(Value::Nil)
        );

        assert_eq!(
            eval(
                &Expr::If(Box::new(Expr::Nil), Box::new(Expr::Num(1.0)), None,),
                &Vec::new()
            ),
            Err("expect bool as condition for if statement: Nil".into())
        );
    }

    #[test]
    fn test_execute_assignments() {
        let decls = vec![
            Decl::Ass {
                name: "foo".into(),
                expr: Expr::Num(1.0),
            },
            Decl::Ass {
                name: "bar".into(),
                expr: Expr::Num(2.0),
            },
            Decl::Stm {
                expr: Expr::Eq(
                    Box::new(Expr::Add(
                        Box::new(Expr::Ident("foo".into())),
                        Box::new(Expr::Ident("bar".into())),
                    )),
                    Box::new(Expr::Num(3.0)),
                ),
            },
        ];

        let program = Program::new(decls);
        let mut interpreter = Interpreter::new(&program);

        assert_eq!(interpreter.execute(), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_execute_function_call() {
        let decls = vec![
            Decl::Fun {
                name: "add".into(),
                args: vec!["a".into(), "b".into()],
                body: Expr::Add(
                    Box::new(Expr::Ident("a".into())),
                    Box::new(Expr::Ident("b".into())),
                ),
            },
            Decl::Stm {
                expr: Expr::Call("add".into(), vec![Expr::Num(1.0), Expr::Num(2.0)]),
            },
        ];

        let program = Program::new(decls);
        let mut interpreter = Interpreter::new(&program);

        assert_eq!(interpreter.execute(), Ok(Value::Num(3.0)));
    }
}
