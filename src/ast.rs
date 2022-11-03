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
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Stm { expr: Expr },
    Ass { name: String, expr: Expr },
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
