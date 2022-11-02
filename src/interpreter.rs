use super::ast::Expr;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
}

pub fn eval(expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Nil => Ok(Value::Nil),
        Expr::Bool(val) => Ok(Value::Bool(*val)),
        Expr::Num(val) => Ok(Value::Num(*val)),
        Expr::Str(val) => Ok(Value::Str(val.clone())),
        Expr::Ident(_) => Err("no support for identifiers yet".into()),

        Expr::Neg(expr) => {
            let val = eval(expr)?;
            if let Value::Num(val) = val {
                Ok(Value::Num(-val))
            } else {
                Err(format!("can't negate {val:?}"))
            }
        }

        Expr::Not(expr) => {
            let val = eval(expr)?;
            if let Value::Bool(val) = val {
                Ok(Value::Bool(!val))
            } else {
                Err(format!("can't not {val:?}"))
            }
        }

        Expr::Add(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left + right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} + {right_val:?}"
                )),
            }
        }

        Expr::Sub(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left - right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} - {right_val:?}"
                )),
            }
        }

        Expr::Mul(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left * right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} * {right_val:?}"
                )),
            }
        }

        Expr::Div(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Num(left / right)),
                _ => Err(format!(
                    "expect num on both sides: {left_val:?} / {right_val:?}"
                )),
            }
        }

        Expr::Eq(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
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
            let left_val = eval(left)?;
            let right_val = eval(right)?;
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
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left < right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left < right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} < {right_val:?}"
                )),
            }
        }

        Expr::Lte(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left <= right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left <= right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} <= {right_val:?}"
                )),
            }
        }

        Expr::Gt(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left > right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left > right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} > {right_val:?}"
                )),
            }
        }

        Expr::Gte(left, right) => {
            let left_val = eval(left)?;
            let right_val = eval(right)?;
            match (&left_val, &right_val) {
                (Value::Num(left), Value::Num(right)) => Ok(Value::Bool(left >= right)),
                (Value::Str(left), Value::Str(right)) => Ok(Value::Bool(left >= right)),
                _ => Err(format!(
                    "expect num or str on both sides: {left_val:?} >= {right_val:?}"
                )),
            }
        }

        Expr::And(left, right) => {
            let left_val = eval(left)?;
            if let Value::Bool(left_val) = left_val {
                if left_val {
                    let right_val = eval(right)?;
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
            let left_val = eval(left)?;
            if let Value::Bool(left_val) = left_val {
                if left_val {
                    Ok(Value::Bool(true))
                } else {
                    let right_val = eval(right)?;
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
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eval() {
        assert_eq!(
            eval(&Expr::Add(
                Box::new(Expr::Num(1.0)),
                Box::new(Expr::Bool(true)),
            )),
            Err("expect num on both sides: Num(1.0) + Bool(true)".into())
        );

        assert_eq!(
            eval(&Expr::Not(Box::new(Expr::Bool(true)))),
            Ok(Value::Bool(false))
        );

        assert_eq!(
            eval(&Expr::Neg(Box::new(Expr::Num(3.0)))),
            Ok(Value::Num(-3.0))
        );

        assert_eq!(
            eval(&Expr::Or(
                Box::new(Expr::And(
                    Box::new(Expr::Bool(true)),
                    Box::new(Expr::Bool(false)),
                )),
                Box::new(Expr::Bool(true)),
            )),
            Ok(Value::Bool(true))
        );

        assert_eq!(
            eval(&Expr::Sub(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(1.0)),
                    Box::new(Expr::Mul(
                        Box::new(Expr::Num(2.0)),
                        Box::new(Expr::Num(3.0))
                    ))
                )),
                Box::new(Expr::Num(4.0))
            )),
            Ok(Value::Num(3.0))
        );

        assert_eq!(
            eval(&Expr::Mul(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(1.0)),
                    Box::new(Expr::Num(2.0))
                )),
                Box::new(Expr::Sub(
                    Box::new(Expr::Num(3.0)),
                    Box::new(Expr::Num(4.0))
                ))
            )),
            Ok(Value::Num(-3.0))
        );

        assert_eq!(
            eval(&Expr::Or(
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
            )),
            Ok(Value::Bool(true))
        );
    }
}
