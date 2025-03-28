#[derive(Clone, Debug, PartialEq)]
pub enum LitKind {
    Nil,
    Bool(bool),
    Num(i64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOpKind {
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
pub enum ExprKind<E, D> {
    Lit {
        lit: LitKind,
    },
    Ident {
        name: String,
    },
    UnOp {
        kind: UnOpKind,
        expr: Box<E>,
    },
    BinOp {
        kind: BinOpKind,
        left: Box<E>,
        right: Box<E>,
    },
    If {
        cond: Box<E>,
        then: Box<E>,
        else_: Option<Box<E>>,
    },
    While {
        cond: Box<E>,
        do_: Box<E>,
    },
    Call {
        name: String,
        args: Vec<E>,
    },
    Print {
        expr: Box<E>,
    },
    Block {
        decls: Vec<D>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind<Self, Decl>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Stm {
        expr: Expr,
    },
    Ass {
        name: (String, Option<String>),
        expr: Vec<Decl>,
    },
    Fun {
        name: String,
        args: Vec<(String, Option<String>)>,
        ret: Option<String>,
        body: Vec<Decl>,
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
    use crate::util::macros::pubmacro;

    pubmacro! { nil,
        () => {
            Expr {
                kind: ExprKind::Lit {
                    lit: LitKind::Nil,
                }
            }
        };
    }

    pubmacro! { bool,
        ($value:literal) => {
            Expr {
                kind: ExprKind::Lit {
                    lit: LitKind::Bool($value),
                }
            }
        };
    }

    pubmacro! { num,
        ($value:literal) => {
            Expr {
                kind: ExprKind::Lit {
                    lit: LitKind::Num($value),
                }
            }
        };
    }

    pubmacro! { string,
        ($value:literal) => {
            Expr {
                kind: ExprKind::Lit {
                    lit: LitKind::String($value.to_owned()),
                }
            }
        };
    }

    pubmacro! { ident,
        ($value:ident) => {
            Expr {
                kind: ExprKind::Ident {
                    name: stringify!($value).to_string(),
                }
            }
        };
    }

    macro_rules! unop {
        ($unop:ident, $macro_name:ident) => {
            pubmacro! { $macro_name,
                ($expr:expr) => {
                    Expr {
                        kind: ExprKind::UnOp {
                            kind: UnOpKind::$unop,
                            expr: Box::new($expr),
                        }
                    }
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
                    Expr {
                        kind: ExprKind::BinOp {
                            kind: BinOpKind::$binop,
                            left: Box::new($left),
                            right: Box::new($right),
                        }
                    }
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

    pubmacro! { if_,
        ($expr:expr, $then:expr) => {
            Expr {
                kind: ExprKind::If {
                    cond: Box::new($expr),
                    then: Box::new($then),
                    else_: None,
                }
            }
        };
        ($expr:expr, $then:expr, $else:expr) => {
            Expr {
                kind: ExprKind::If {
                    cond: Box::new($expr),
                    then: Box::new($then),
                    else_: Some(Box::new($else)),
                }
            }
        };
    }

    pubmacro! { call,
        ($name:ident ( $($args:expr),* )) => {
            Expr {
                kind: ExprKind::Call {
                    name: stringify!($name).to_string(),
                    args: vec![$($args),*],
                }
            }
        }
    }

    pubmacro! { print_,
        ($expr:expr) => {
            Expr {
                kind: ExprKind::Print {
                    expr: Box::new($expr),
                }
            }
        }
    }

    pubmacro! { block,
        ($($expr:expr),+) => {
            Expr {
                kind: ExprKind::Block {
                    decls: vec![$($expr),+]
                }
            }
        }
    }

    pubmacro! { stm,
        ($expr:expr) => {
            Decl::Stm {
                expr: $expr,
            }
        };
    }

    pubmacro! { ass,
        ($name:ident $(: $type:ident)? = $($expr:expr),+) => {
            Decl::Ass {
                name: typed_arg!($name $(: $type)?),
                expr: vec![$(stm!($expr)),+]
            }
        };
    }

    pubmacro! { fun,
        ($name:ident ( $($args:ident $(: $types:ident)?),* ) $(: $ret:ident)? => $body:expr) => {
            Decl::Fun {
                name: stringify!($name).to_string(),
                args: vec![$(typed_arg!($args $(: $types)?)),*],
                ret: typ!($(: $ret)?),
                body: vec![stm!($body)],
            }
        };
    }

    pubmacro! { typed_arg,
        ($name:ident $(: $typ:ident)?) => {
            (
                stringify!($name).to_string(),
                typ!($(: $typ)?),
            )
        };
    }

    pubmacro! { typ,
        () => {
            None
        };
        (: $ret:ident) => {
            Some(stringify!($ret).to_string())
        };
    }

    pubmacro! { nilbody,
        () => {
            vec![stm!(nil!())]
        }
    }

    #[cfg(test)]
    mod test {
        use super::super::*;
        use super::*;

        #[test]
        fn test_if() {
            assert_eq!(
                if_!(bool!(true), num!(1)),
                Expr {
                    kind: ExprKind::If {
                        cond: Box::new(bool!(true)),
                        then: Box::new(num!(1)),
                        else_: None
                    }
                }
            );

            assert_eq!(
                if_!(bool!(true), num!(1), num!(2)),
                Expr {
                    kind: ExprKind::If {
                        cond: Box::new(bool!(true)),
                        then: Box::new(num!(1)),
                        else_: Some(Box::new(num!(2)))
                    }
                }
            );
        }

        #[test]
        fn test_call() {
            assert_eq!(
                call!(foo()),
                Expr {
                    kind: ExprKind::Call {
                        name: "foo".to_string(),
                        args: vec![],
                    }
                }
            );

            assert_eq!(
                call!(foo(nil!())),
                Expr {
                    kind: ExprKind::Call {
                        name: "foo".to_string(),
                        args: vec![nil!()],
                    }
                }
            );

            assert_eq!(
                call!(foo(nil!(), nil!())),
                Expr {
                    kind: ExprKind::Call {
                        name: "foo".to_string(),
                        args: vec![nil!(), nil!()],
                    }
                }
            );
        }

        #[test]
        fn test_assignment() {
            assert_eq!(
                ass!(foo = nil!()),
                Decl::Ass {
                    name: ("foo".to_string(), None),
                    expr: nilbody!(),
                }
            );

            assert_eq!(
                ass!(foo: Num = nil!()),
                Decl::Ass {
                    name: ("foo".to_string(), Some("Num".to_string())),
                    expr: nilbody!(),
                }
            );
        }

        #[test]
        fn test_function() {
            assert_eq!(
                fun!(foo() => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![],
                    ret: None,
                    body: nilbody!(),
                }
            );

            assert_eq!(
                fun!(foo(a) => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![("a".to_string(), None)],
                    ret: None,
                    body: nilbody!(),
                }
            );

            assert_eq!(
                fun!(foo(a, b) => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![("a".to_string(), None), ("b".to_string(), None)],
                    ret: None,
                    body: nilbody!(),
                }
            );

            assert_eq!(
                fun!(foo(a: Num) => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![("a".to_string(), Some("Num".to_string()))],
                    ret: None,
                    body: nilbody!(),
                }
            );

            assert_eq!(
                fun!(foo(a:Num, b:Num) => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![
                        ("a".to_string(), Some("Num".to_string())),
                        ("b".to_string(), Some("Num".to_string()))
                    ],
                    ret: None,
                    body: nilbody!(),
                }
            );

            assert_eq!(
                fun!(foo():Num => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![],
                    ret: Some("Num".to_string()),
                    body: nilbody!(),
                }
            );

            assert_eq!(
                fun!(foo(a:Num, b:Num):Num => nil!()),
                Decl::Fun {
                    name: "foo".to_string(),
                    args: vec![
                        ("a".to_string(), Some("Num".to_string())),
                        ("b".to_string(), Some("Num".to_string()))
                    ],
                    ret: Some("Num".to_string()),
                    body: nilbody!(),
                }
            );
        }
    }
}
