use super::util::*;
use crate::ast::ExprKind;
use std::str::FromStr;
use std::{cell::RefCell, rc::Rc};

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

impl TypedDecl {
    pub fn ty(&self) -> TySimple {
        match self {
            TypedDecl::Stm { ty, .. } => ty.clone(),
            TypedDecl::Ass { ty, .. } => ty.clone(),
            TypedDecl::Fun { .. } => TySimple::Nil,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedProgram(Vec<TypedDecl>);

impl TypedProgram {
    pub fn new(decls: Vec<TypedDecl>) -> Self {
        Self(decls)
    }

    pub fn decls(&self) -> &[TypedDecl] {
        &self.0
    }
}

// Maybe type stuff

#[derive(Clone, PartialEq)]
pub enum MaybeTySimple {
    Unknown {
        variable_name: String,
        target: Rc<RefCell<Option<Self>>>,
    },
    Expected(TySimple),
    Possibly(Vec<TySimple>),
    Inferred(TySimple),
}

impl MaybeTySimple {
    pub fn parse(var: &String, ty: &Option<String>) -> Result<Self> {
        if let Some(ty) = ty {
            Ok(MaybeTySimple::Expected(TySimple::from_str(&ty)?))
        } else {
            Ok(MaybeTySimple::Unknown {
                variable_name: var.clone(),
                target: Rc::new(RefCell::new(None)),
            })
        }
    }

    pub fn get_type(&self) -> Result<TySimple> {
        match self {
            MaybeTySimple::Unknown {
                target,
                variable_name: var,
            } => {
                if let Some(sub) = &*target.borrow() {
                    sub.get_type()
                } else {
                    Err(format!("var {var} not resolved"))
                }
            }
            MaybeTySimple::Expected(ty) => Ok(ty.clone()),
            _ => Err(format!("unepexted type {self:?}")),
        }
    }
}

pub fn match_type(left: &MaybeTySimple, right: &MaybeTySimple) -> Result<()> {
    // println!("MATCH: {left:?} <-> {right:?}");

    fn occurs_in(var: &MaybeTySimple, var2: &String, other: &MaybeTySimple) -> bool {
        // println!("occurs_in({var:?}, {var2:?}, {other:?})");

        match other {
            MaybeTySimple::Expected(_) => false,
            MaybeTySimple::Unknown {
                target,
                variable_name: other_var,
            } => {
                if let Some(sub) = &*target.borrow() {
                    occurs_in(var, var2, sub)
                } else {
                    var2 == other_var
                }
            }
            _ => panic!("SHOULD NOT HAPPEN"),
        }
    }

    fn handle_var(
        var: &MaybeTySimple,
        var2: &String,
        sub: &RefCell<Option<MaybeTySimple>>,
        other: &MaybeTySimple,
    ) -> Result<()> {
        // println!("handle_var({var:?}, {var2:?}, {sub:?}, {other:?})");

        if let Some(sub) = &*sub.borrow() {
            return match_type(&sub, other);
        }

        if occurs_in(var, var2, other) {
            // Err(format!("infinite type: {var:?} = {other:?}"))
            // not occurs_in, but var points to itself, which is equal
            Ok(())
        } else {
            *sub.borrow_mut() = Some(other.clone());
            Ok(())
        }
    }

    match (left, right) {
        (
            MaybeTySimple::Unknown {
                target,
                variable_name: var,
            },
            _,
        ) => handle_var(left, var, target, right),
        (
            _,
            MaybeTySimple::Unknown {
                target,
                variable_name: var,
            },
        ) => handle_var(right, var, target, left),
        (MaybeTySimple::Expected(left), MaybeTySimple::Expected(right)) if left == right => Ok(()),
        (MaybeTySimple::Expected(left), MaybeTySimple::Expected(right)) => Err(format!(
            "expected same type on both sides: left: {left:?}; right: {right:?}"
        )),
        (_, _) => Err(format!(
            "unexpected types: left: {left:?}; right: {right:?}"
        )),
    }
}

impl std::fmt::Debug for MaybeTySimple {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        match self {
            MaybeTySimple::Expected(ty) => ty.fmt(formatter)?,
            MaybeTySimple::Unknown {
                variable_name,
                target,
            } => {
                formatter.write_str("$")?;
                formatter.write_str(&variable_name)?;
                if let Some(target) = &*target.borrow() {
                    formatter.write_str("->")?;
                    target.fmt(formatter)?;
                }
            }
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
