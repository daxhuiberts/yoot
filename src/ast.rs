#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Ident(String),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
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
            Expr::Lit(Lit::Nil)
        };
    }

    pubmacro! { bool,
        ($value:literal) => {
            Expr::Lit(Lit::Bool($value))
        };
    }

    pubmacro! { num,
        ($value:literal) => {
            Expr::Lit(Lit::Num($value))
        };
    }

    pubmacro! { str,
        ($value:literal) => {
            Expr::Lit(Lit::Str($value.to_string()))
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
                    Expr::UnOp(UnOp::$unop, Box::new($expr))
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
                    Expr::BinOp(BinOp::$binop, Box::new($left), Box::new($right))
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
