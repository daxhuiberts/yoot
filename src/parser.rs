use super::ast::*;

use chumsky::prelude::*;

fn expression() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> + Clone {
    recursive(|expr| {
        let nil = just("nil").map(|_| Expr::Nil);

        let boolean = just("true")
            .to(true)
            .or(just("false").to(false))
            .map(Expr::Bool);

        let number = text::int(10).from_str().unwrapped().map(Expr::Num);

        let string = just("\"")
            .ignore_then(none_of("\"").repeated())
            .then_ignore(just("\""))
            .collect::<String>()
            .map(Expr::Str);

        let call = text::ident()
            .then(
                expr.clone()
                    .separated_by(just(", "))
                    .delimited_by(just('('), just(')')),
            )
            .map(|(name, args)| Expr::Call(name, args));

        let identifier = text::ident().map(Expr::Ident);

        let subexpression = expr.delimited_by(just('('), just(')'));

        let primary = nil
            .or(boolean)
            .or(number)
            .or(string)
            .or(call)
            .or(identifier)
            .or(subexpression);

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

fn declaration() -> impl chumsky::Parser<char, Vec<Decl>, Error = Simple<char>> {
    let expression = expression();

    let assignment = text::ident()
        .then_ignore(just(" = "))
        .then(expression.clone())
        .map(|(name, expr)| Decl::Ass { name, expr });

    let function = text::ident()
        .then_ignore(just(" "))
        .then(text::ident().separated_by(just(" ")))
        .then_ignore(just(" = "))
        .then(expression.clone())
        .map(|((name, args), body)| Decl::Fun { name, args, body });

    let statement = expression.map(|expr| Decl::Stm { expr });

    assignment
        .or(function)
        .or(statement)
        .or_not()
        .separated_by(just("\n"))
        .allow_trailing()
        .map(|decls| decls.into_iter().flatten().collect())
}

pub fn parser() -> impl chumsky::Parser<char, Program, Error = Simple<char>> {
    declaration().then_ignore(end()).map(Program::new)
}

#[cfg(test)]
mod test {
    use super::*;

    fn expression() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> {
        super::expression().then_ignore(end())
    }

    fn declaration() -> impl chumsky::Parser<char, Vec<Decl>, Error = Simple<char>> {
        super::declaration().then_ignore(end())
    }

    macro_rules! assert_ok {
        ($parser:ident, $program:literal, $expected:expr) => {
            assert_eq!($parser().parse($program), Ok($expected));
        };
    }

    macro_rules! assert_err {
        ($parser:ident, $program:literal) => {
            assert!($parser().parse($program).is_err());
        };
    }

    #[test]
    fn test_expression() {
        assert_err!(expression, "");
        assert_err!(expression, " ");
        assert_err!(expression, "1a");
        assert_err!(expression, "01");

        assert_ok!(expression, "nil", Expr::Nil);

        assert_ok!(expression, "true", Expr::Bool(true));
        assert_ok!(expression, "false", Expr::Bool(false));

        assert_ok!(expression, "a", Expr::Ident("a".to_string()));
        assert_ok!(expression, "a1", Expr::Ident("a1".to_string()));
        assert_ok!(expression, "foo", Expr::Ident("foo".to_string()));
        assert_ok!(expression, "bar", Expr::Ident("bar".to_string()));

        assert_ok!(expression, "1", Expr::Num(1.0));
        assert_ok!(expression, "10", Expr::Num(10.0));

        assert_ok!(expression, "\"hello\"", Expr::Str("hello".to_string()));
        assert_ok!(expression, "\"world\"", Expr::Str("world".to_string()));
        assert_ok!(
            expression,
            "\"hello world\"",
            Expr::Str("hello world".to_string())
        );

        assert_ok!(expression, "!true", Expr::Not(Box::new(Expr::Bool(true))));
        assert_ok!(expression, "-1", Expr::Neg(Box::new(Expr::Num(1.0))));

        assert_err!(expression, "1+2");

        assert_ok!(
            expression,
            "1 + 2",
            Expr::Add(Box::new(Expr::Num(1.0)), Box::new(Expr::Num(2.0)))
        );

        assert_ok!(
            expression,
            "2 - 1",
            Expr::Sub(Box::new(Expr::Num(2.0)), Box::new(Expr::Num(1.0)))
        );

        assert_err!(expression, "()");

        assert_ok!(expression, "(true)", Expr::Bool(true));
        assert_ok!(expression, "(1)", Expr::Num(1.0));

        assert_ok!(
            expression,
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
            expression,
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
            expression,
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
            expression,
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
            expression,
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

    #[test]
    fn test_declaration() {
        assert_ok!(
            declaration,
            "foo = 1\nbar = 2\nfoo + bar == 3",
            vec![
                Decl::Ass {
                    name: "foo".into(),
                    expr: Expr::Num(1.0)
                },
                Decl::Ass {
                    name: "bar".into(),
                    expr: Expr::Num(2.0)
                },
                Decl::Stm {
                    expr: Expr::Eq(
                        Box::new(Expr::Add(
                            Box::new(Expr::Ident("foo".into())),
                            Box::new(Expr::Ident("bar".into())),
                        )),
                        Box::new(Expr::Num(3.0)),
                    )
                }
            ]
        );

        assert_ok!(
            declaration,
            "add a b = a + b\nadd(1, 2)",
            vec![
                Decl::Fun {
                    name: "add".into(),
                    args: vec!["a".into(), "b".into()],
                    body: Expr::Add(
                        Box::new(Expr::Ident("a".into())),
                        Box::new(Expr::Ident("b".into())),
                    )
                },
                Decl::Stm {
                    expr: Expr::Call("add".into(), vec![Expr::Num(1.0), Expr::Num(2.0)],)
                }
            ]
        );
    }
}
