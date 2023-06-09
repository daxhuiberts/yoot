use crate::ast::ExprKind;
use std::str::FromStr;

// Generic stuff

// pub trait TyInterface {
//     type Simple;
//     type Function: Clone + std::fmt::Debug + PartialEq;
// }

#[derive(Clone, Debug, PartialEq)]
pub struct TyExpr<TS, TF> {
    pub kind: ExprKind<Self, TyDecl<TS, TF>>,
    pub ty: TS,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TyDecl<TS, TF> {
    Stm {
        expr: TyExpr<TS, TF>,
        ty: TS,
    },
    Ass {
        name: String,
        expr: Vec<Self>,
        ty: TS,
    },
    Fun {
        name: String,
        args: Vec<String>,
        body: Vec<Self>,
        ty: TF,
    },
}

// Type data

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

// #[derive(Clone, Debug, PartialEq)]
// pub enum Ty {
//     Simple(TySimple),
//     Function(TyFunction),
// }

// Typed stuff

pub type TypedExpr = TyExpr<TySimple, TyFunction>;
pub type TypedDecl = TyDecl<TySimple, TyFunction>;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedProgram(Vec<TypedDecl>);

impl TypedProgram {
    pub fn new(decls: Vec<TypedDecl>) -> Self {
        Self(decls)
    }
}

// Maybe type stuff

#[derive(Clone, PartialEq)]
pub enum MaybeTySimple {
    Unknown,
    Expected(TySimple),
    Possibly(Vec<TySimple>),
    Inferred(TySimple),
}

impl std::fmt::Debug for MaybeTySimple {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        match self {
            MaybeTySimple::Expected(ty) => ty.fmt(formatter)?,
            _ => panic!("cant format non expected types"),
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MaybeTyFunction {
    pub args: Vec<MaybeTySimple>,
    pub ret: MaybeTySimple,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MaybeTy {
    Simple(MaybeTySimple),
    Function(MaybeTyFunction),
}

pub type MaybeTypedExpr = TyExpr<MaybeTySimple, MaybeTyFunction>;
pub type MaybeTypedDecl = TyDecl<MaybeTySimple, MaybeTyFunction>;

impl MaybeTypedDecl {
    pub fn ty(&self) -> MaybeTy {
        match self {
            MaybeTypedDecl::Stm { ty, .. } => MaybeTy::Simple(ty.clone()),
            MaybeTypedDecl::Ass { ty, .. } => MaybeTy::Simple(ty.clone()),
            MaybeTypedDecl::Fun { ty, .. } => MaybeTy::Function(ty.clone()),
        }
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
