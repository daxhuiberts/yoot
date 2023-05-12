use crate::ast::ExprKind;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
pub enum TySimple {
    Nil,
    Bool,
    Num,
}

impl FromStr for TySimple {
    type Err = String;

    fn from_str(input: &str) -> std::result::Result<Self, String> {
        match input {
            "Nil" => Ok(TySimple::Nil),
            "Bool" => Ok(TySimple::Bool),
            "Num" => Ok(TySimple::Num),
            _ => Err(format!("unknown type '{input}'")),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyFunction {
    pub args: Vec<TySimple>,
    pub ret: TySimple,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Simple(TySimple),
    Function(TyFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub kind: ExprKind<Self>,
    pub ty: TySimple,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedDecl {
    Stm {
        expr: TypedExpr,
        ty: TySimple,
    },
    Ass {
        name: String,
        expr: Vec<TypedDecl>,
        ty: TySimple,
    },
    Fun {
        name: String,
        args: Vec<String>,
        body: Vec<TypedDecl>,
        ty: TyFunction,
    },
}

impl TypedDecl {
    pub fn ty(&self) -> Ty {
        match self {
            TypedDecl::Stm { ty, .. } => Ty::Simple(ty.clone()),
            TypedDecl::Ass { ty, .. } => Ty::Simple(ty.clone()),
            TypedDecl::Fun { ty, .. } => Ty::Function(ty.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedProgram(Vec<TypedDecl>);

impl TypedProgram {
    pub fn new(decls: Vec<TypedDecl>) -> Self {
        Self(decls)
    }
}

#[cfg(test)]
pub mod macros {
    use crate::util::macros::pubmacro;

    pubmacro! { tnil,
        () => {
            TypedExpr {
                kind: ExprKind::Lit {
                    lit: LitKind::Nil,
                },
                ty: TySimple::Nil,
            }
        };
    }

    pubmacro! { tbool,
        ($value:literal) => {
            TypedExpr {
                kind: ExprKind::Lit {
                    lit: LitKind::Bool($value),
                },
                ty: TySimple::Bool,
            }
        };
    }

    pubmacro! { tnum,
        ($value:literal) => {
            TypedExpr {
                kind: ExprKind::Lit {
                    lit: LitKind::Num($value),
                },
                ty: TySimple::Num,
            }
        };
    }

    pubmacro! { tident,
        ($value:ident, $ty:ident) => {
            TypedExpr {
                kind: ExprKind::Ident {
                    name: stringify!($value).to_string(),
                },
                ty: TySimple::$ty,
            }
        };
    }

    // macro_rules! unop {
    //     ($unop:ident, $macro_name:ident) => {
    //         pubmacro! { $macro_name,
    //             ($expr:expr) => {
    //                 Expr::UnOp(UnOpKind::$unop, Box::new($expr))
    //             };
    //         }
    //     };
    // }

    // unop!(Not, not);
    // unop!(Neg, neg);

    macro_rules! binop {
        ($binop:ident, $macro_name:ident) => {
            pubmacro! { $macro_name,
                ($left:expr, $right:expr, $ty:ident) => {
                    TypedExpr {
                        kind: ExprKind::BinOp {
                            kind: BinOpKind::$binop,
                            left: Box::new($left),
                            right: Box::new($right),
                        },
                        ty: TySimple::$ty,
                    }
                };
            }
        };
    }

    binop!(Add, tadd);
    // binop!(Sub, sub);
    // binop!(Mul, mul);
    // binop!(Div, div);
    // binop!(Eq, eq);
    // binop!(Neq, neq);
    // binop!(Gt, gt);
    // binop!(Gte, gte);
    // binop!(Lt, lt);
    // binop!(Lte, lte);
    // binop!(And, and);
    // binop!(Or, or);

    pubmacro! { tif,
        ($expr:expr, $then:expr; $ty:ident) => {
            TypedExpr {
                kind: ExprKind::If {
                    cond: Box::new($expr),
                    then: Box::new($then),
                    else_: None,
                },
                ty: TySimple::$ty,
            }
        };
        ($expr:expr, $then:expr, $else:expr; $ty:ident) => {
            TypedExpr {
                kind: ExprKind::If {
                    cond: Box::new($expr),
                    then: Box::new($then),
                    else_: Some(Box::new($else)),
                },
                ty: TySimple::$ty,
            }
        };
    }

    pubmacro! { tcall,
        ($name:ident($($args:expr),*) ; $ty:ident) => {
            TypedExpr {
                kind: ExprKind::Call {
                    name: stringify!($name).to_string(),
                    args: vec![$($args),*],
                },
                ty: TySimple::$ty,
            }
        }
    }

    pubmacro! { tstm,
        ($expr:expr, $ty:ident) => {
            TypedDecl::Stm { expr: $expr, ty: TySimple::$ty }
        };
    }

    pubmacro! { tass,
        ($name:ident:$ret:ident = $expr:expr) => {
            TypedDecl::Ass {
                name: stringify!($name).to_string(),
                expr: vec![tstm!($expr, $ret)],
                ty: TySimple::$ret
            }
        };
    }

    pubmacro! { tfun,
        ($name:ident ( $($args:ident : $types:ident),* ) : $ret:ident => $body:expr) => {
            TypedDecl::Fun {
                name: stringify!($name).to_string(),
                args: vec![$(stringify!($args).to_string()),*],
                body: vec![tstm!($body, $ret)],
                ty: TyFunction {
                    args: vec![$(TySimple::$types),*],
                    ret: TySimple::$ret,
                }
            }
        };
    }
}
