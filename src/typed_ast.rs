use crate::ast::{BinOpKind, Lit, UnOpKind};
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
pub enum TySimple {
    Nil,
    Bool,
    Num,
}

impl FromStr for TySimple {
    type Err = String;

    fn from_str(input: &str) -> std::result::Result<Self, <Self as FromStr>::Err> {
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
pub enum TypedExpr {
    Lit {
        lit: Lit,
        ty: TySimple,
    },
    Ident {
        name: String,
        ty: TySimple,
    },
    UnOp {
        kind: UnOpKind,
        expr: Box<TypedExpr>,
        ty: TySimple,
    },
    BinOp {
        kind: BinOpKind,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
        ty: TySimple,
    },
    If {
        cond: Box<TypedExpr>,
        then: Box<TypedExpr>,
        else_: Option<Box<TypedExpr>>,
        ty: TySimple,
    },
    Call {
        name: String,
        args: Vec<TypedExpr>,
        ty: TySimple,
    },
}

impl TypedExpr {
    pub fn get_ty(&self) -> &TySimple {
        match self {
            TypedExpr::Lit { ty, .. } => ty,
            TypedExpr::Ident { ty, .. } => ty,
            TypedExpr::UnOp { ty, .. } => ty,
            TypedExpr::BinOp { ty, .. } => ty,
            TypedExpr::If { ty, .. } => ty,
            TypedExpr::Call { ty, .. } => ty,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedDecl {
    Stm {
        expr: TypedExpr,
        ty: TySimple,
    },
    Ass {
        name: String,
        expr: TypedExpr,
        ty: TySimple,
    },
    Fun {
        name: String,
        args: Vec<String>,
        body: TypedExpr,
        ty: TyFunction,
    },
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
            TypedExpr::Lit { lit: Lit::Nil, ty: TySimple::Nil }
        };
    }

    // pubmacro! { bool,
    //     ($value:literal) => {
    //         Expr::Lit(Lit::Bool($value))
    //     };
    // }

    pubmacro! { tnum,
        ($value:literal) => {
            TypedExpr::Lit { lit: Lit::Num($value), ty: TySimple::Num }
        };
    }

    // pubmacro! { str,
    //     ($value:literal) => {
    //         Expr::Lit(Lit::Str($value.to_string()))
    //     };
    // }

    pubmacro! { tident,
        ($value:ident, $ty:ident) => {
            TypedExpr::Ident { name: stringify!($value).to_string(), ty: TySimple::$ty }
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
                    TypedExpr::BinOp {
                        kind: BinOpKind::$binop,
                        left: Box::new($left),
                        right: Box::new($right),
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

    // pubmacro! { iff,
    //     ($expr:expr, $then:expr) => {
    //         Expr::If(Box::new($expr), Box::new($then), None)
    //     };
    //     ($expr:expr, $then:expr, $else:expr) => {
    //         Expr::If(Box::new($expr), Box::new($then), Some(Box::new($else)))
    //     };
    // }

    pubmacro! { tcall,
        ($name:ident, $($args:expr),* ; $ty:ident) => {
            TypedExpr::Call { name: stringify!($name).to_string(), args: vec![$($args),*], ty: TySimple::$ty }
        }
    }

    // pubmacro! { stm,
    //     ($expr:expr) => {
    //         Decl::Stm { expr: $expr }
    //     };
    // }

    // pubmacro! { ass,
    //     ($name:ident, $expr:expr) => {
    //         Decl::Ass {
    //             name: stringify!($name).to_string(),
    //             expr: $expr,
    //         }
    //     };
    // }

    pubmacro! { tfun,
        ($name:ident : ($($ty_args:ident),*):$ret:ident, [ $($args:ident),* ], $body:expr) => {
            TypedDecl::Fun {
                name: stringify!($name).to_string(),
                args: vec![$(stringify!($args).to_string()),*],
                body: $body,
                ty: TyFunction {
                    args: vec![$(TySimple::$ty_args),*],
                    ret: TySimple::$ret
                }
            }
        }
    }
}
