use super::ast::Expr;

use chumsky::prelude::*;

fn expr() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let number = text::int(10).from_str().unwrapped().map(Expr::Num);

        let primary = number.or(expr.delimited_by(just('('), just(')')));

        let op = |c| just(c).delimited_by(just(' '), just(' '));

        let factor = primary
            .clone()
            .then(
                op("*")
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op("/").to(Expr::Div as fn(_, _) -> _))
                    .then(primary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        factor
            .clone()
            .then(
                op("+")
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op("-").to(Expr::Sub as fn(_, _) -> _))
                    .then(factor)
                    .repeated(),
            )
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

        assert_ok!("1", Expr::Num(1.0));
        assert_ok!("10", Expr::Num(10.0));

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
        assert_ok!("(1)", Expr::Num(1.0));

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
    }
}
