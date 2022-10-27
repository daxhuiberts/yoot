use super::ast::Expr;

use chumsky::prelude::*;

fn expr() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let boolean = just("true")
            .to(true)
            .or(just("false").to(false))
            .map(Expr::Bool);

        let number = text::int(10).from_str().unwrapped().map(Expr::Num);

        let primary = boolean
            .or(number)
            .or(expr.delimited_by(just('('), just(')')));

        let unary = just('-')
            .to(Expr::Neg as fn(_) -> _)
            .or(just('!').to(Expr::Not as fn(_) -> _))
            .then(primary.clone())
            .map(|(unary, primary)| unary(Box::new(primary)))
            .or(primary);

        let op = |c| just(c).delimited_by(just(' '), just(' '));

        let factor = unary
            .clone()
            .then(
                op("*")
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op("/").to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let term = factor
            .clone()
            .then(
                op("+")
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op("-").to(Expr::Sub as fn(_, _) -> _))
                    .then(factor)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let comp = term
            .clone()
            .then(
                op(">=")
                    .to(Expr::Gte as fn(_, _) -> _)
                    .or(op(">").to(Expr::Gt as fn(_, _) -> _))
                    .or(op("<").to(Expr::Lt as fn(_, _) -> _))
                    .or(op("<=").to(Expr::Lte as fn(_, _) -> _))
                    .or(op("==").to(Expr::Eq as fn(_, _) -> _))
                    .or(op("!=").to(Expr::Neq as fn(_, _) -> _))
                    .then(term)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let and = comp
            .clone()
            .then(
                op("&&")
                    .to(Expr::And as fn(_, _) -> _)
                    .then(comp)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        and.clone()
            .then(op("||").to(Expr::Or as fn(_, _) -> _).then(and).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
    })
}

pub fn parser() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> {
    expr().then_ignore(just("\n").or_not()).then_ignore(end())
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_ok {
        ($expression:literal, $expected:expr) => {
            assert_eq!(parser().parse($expression), Ok($expected));
        };
    }

    macro_rules! assert_err {
        ($expression:literal) => {
            assert!(parser().parse($expression).is_err());
        };
    }

    #[test]
    fn test_parser() {
        assert_err!("");
        assert_err!(" ");
        assert_err!("a");
        assert_err!("a1");
        assert_err!("1a");
        assert_err!("01");

        assert_ok!("true", Expr::Bool(true));
        assert_ok!("false", Expr::Bool(false));

        assert_ok!("1", Expr::Num(1.0));
        assert_ok!("10", Expr::Num(10.0));

        assert_ok!("!true", Expr::Not(Box::new(Expr::Bool(true))));
        assert_ok!("-1", Expr::Neg(Box::new(Expr::Num(1.0))));

        assert_err!("1+2");

        assert_ok!(
            "1 + 2",
            Expr::Add(Box::new(Expr::Num(1.0)), Box::new(Expr::Num(2.0)))
        );

        assert_ok!(
            "2 - 1",
            Expr::Sub(Box::new(Expr::Num(2.0)), Box::new(Expr::Num(1.0)))
        );

        assert_err!("()");

        assert_ok!("(true)", Expr::Bool(true));
        assert_ok!("(1)", Expr::Num(1.0));

        assert_ok!(
            "true && false || true",
            Expr::Or(
                Box::new(Expr::And(
                    Box::new(Expr::Bool(true)),
                    Box::new(Expr::Bool(false)),
                )),
                Box::new(Expr::Bool(true)),
            )
        );

        assert_ok!(
            "true || false && true",
            Expr::Or(
                Box::new(Expr::Bool(true)),
                Box::new(Expr::And(
                    Box::new(Expr::Bool(false)),
                    Box::new(Expr::Bool(true)),
                )),
            )
        );

        assert_ok!(
            "1 + 2 * 3 - 4",
            Expr::Sub(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(1.0)),
                    Box::new(Expr::Mul(
                        Box::new(Expr::Num(2.0)),
                        Box::new(Expr::Num(3.0)),
                    )),
                )),
                Box::new(Expr::Num(4.0)),
            )
        );

        assert_ok!(
            "(1 + 2) * (3 - 4)",
            Expr::Mul(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(1.0)),
                    Box::new(Expr::Num(2.0)),
                )),
                Box::new(Expr::Sub(
                    Box::new(Expr::Num(3.0)),
                    Box::new(Expr::Num(4.0)),
                )),
            )
        );

        assert_ok!(
            "1 + 2 <= -4 == false || !true",
            Expr::Or(
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
            )
        );
    }
}
