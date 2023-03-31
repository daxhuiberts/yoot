#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // literals
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
    Ident(String),

    // unary operators
    Neg(Box<Expr>),
    Not(Box<Expr>),

    // binary operators
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Gte(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),

    Call(String, Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Stm {
        expr: Expr,
    },
    Ass {
        name: String,
        expr: Expr,
    },
    Fun {
        name: String,
        args: Vec<String>,
        body: Expr,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program(Vec<Decl>);

impl Program {
    pub fn new(decls: Vec<Decl>) -> Program {
        Program(decls)
    }

    pub fn decls(&self) -> &[Decl] {
        &self.0
    }
}

#[cfg(test)]
pub mod macros {
    macro_rules! pubmacro {
        ($name:ident, $($tt:tt)*) => {
            macro_rules! $name { $($tt)* }

            pub(crate) use $name;
        }
    }

    pubmacro! { nil,
        () => {
            Expr::Nil
        };
    }

    pubmacro! { bool,
        ($value:literal) => {
            Expr::Bool($value)
        };
    }

    pubmacro! { num,
        ($value:literal) => {
            Expr::Num($value)
        };
    }

    pubmacro! { str,
        ($value:literal) => {
            Expr::Str($value.to_string())
        };
    }

    pubmacro! { ident,
        ($value:ident) => {
            Expr::Ident(stringify!($value).to_string())
        };
    }

    macro_rules! unop {
        ($unop:ident, $macro_name:ident) => {
            pubmacro! { $macro_name,
                ($expr:expr) => {
                    Expr::$unop(Box::new($expr))
                };
            }
        };
    }

    unop!(Not, not);
    unop!(Neg, neg);

    macro_rules! binop {
        ($binop:ident, $macro_name:ident) => {
            pubmacro! { $macro_name,
                ($left:expr, $right:expr) => {
                    Expr::$binop(Box::new($left), Box::new($right))
                };
            }
        };
    }

    binop!(Add, add);
    binop!(Sub, sub);
    binop!(Mul, mul);
    // binop!(Div, div);
    binop!(Eq, eq);
    // binop!(Neq, neq);
    // binop!(Gt, gt);
    // binop!(Gte, gte);
    // binop!(Lt, lt);
    binop!(Lte, lte);
    binop!(And, and);
    binop!(Or, or);

    pubmacro! { iff,
        ($expr:expr, $then:expr) => {
            Expr::If(Box::new($expr), Box::new($then), None)
        };
        ($expr:expr, $then:expr, $else:expr) => {
            Expr::If(Box::new($expr), Box::new($then), Some(Box::new($else)))
        };
    }

    pubmacro! { call,
        ($name:ident, $($args:expr),*) => {
            Expr::Call(stringify!($name).to_string(), vec![$($args),*])
        }
    }

    pubmacro! { stm,
        ($expr:expr) => {
            Decl::Stm { expr: $expr }
        };
    }

    pubmacro! { ass,
        ($name:ident, $expr:expr) => {
            Decl::Ass {
                name: stringify!($name).to_string(),
                expr: $expr,
            }
        };
    }

    pubmacro! { fun,
        ($name:ident, [ $($args:ident),* ], $body:expr) => {
            Decl::Fun {
                name: stringify!($name).to_string(),
                args: vec![$(stringify!($args).to_string()),*],
                body: $body,
            }
        }
    }
}
