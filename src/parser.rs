use super::ast::*;

use chumsky::prelude::*;

fn expression() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> + Clone {
    recursive(|expr| {
        let nil = just("nil").map(|_| Lit::Nil);

        let boolean = just("true")
            .to(true)
            .or(just("false").to(false))
            .map(Lit::Bool);

        let number = text::int(10).from_str().unwrapped().map(Lit::Num);

        let string = just("\"")
            .ignore_then(none_of("\"").repeated())
            .then_ignore(just("\""))
            .collect::<String>()
            .map(Lit::Str);

        let literal = nil.or(boolean).or(number).or(string).map(Expr::Lit);

        let if_ = just("if")
            .ignore_then(
                expr.clone()
                    .then_ignore(just(", "))
                    .then(expr.clone())
                    .then(just(", ").ignore_then(expr.clone()).or_not())
                    .delimited_by(just('('), just(')')),
            )
            .map(|((cond, then), else_)| {
                Expr::If(Box::new(cond), Box::new(then), else_.map(Box::new))
            });

        let call = text::ident()
            .then(
                expr.clone()
                    .separated_by(just(", "))
                    .delimited_by(just('('), just(')')),
            )
            .map(|(name, args)| Expr::Call(name, args));

        let identifier = text::ident().map(Expr::Ident);

        let subexpression = expr.delimited_by(just('('), just(')'));

        let primary = literal
            .or(if_)
            .or(call)
            .or(identifier)
            .or(subexpression)
            .boxed();

        let unary = just('-')
            .to(UnOpKind::Neg)
            .or(just('!').to(UnOpKind::Not))
            .then(primary.clone())
            .map(|(unary, primary)| Expr::UnOp(unary, Box::new(primary)))
            .or(primary);

        let op = |c| just(c).delimited_by(just(' '), just(' '));

        let factor = unary
            .clone()
            .then(
                op("*")
                    .to(BinOpKind::Mul)
                    .or(op("/").to(BinOpKind::Div))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        let term = factor
            .clone()
            .then(
                op("+")
                    .to(BinOpKind::Add)
                    .or(op("-").to(BinOpKind::Sub))
                    .then(factor)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        let comp = term
            .clone()
            .then(
                op(">=")
                    .to(BinOpKind::Gte)
                    .or(op(">").to(BinOpKind::Gt))
                    .or(op("<").to(BinOpKind::Lt))
                    .or(op("<=").to(BinOpKind::Lte))
                    .or(op("==").to(BinOpKind::Eq))
                    .or(op("!=").to(BinOpKind::Neq))
                    .then(term)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        let and = comp
            .clone()
            .then(op("&&").to(BinOpKind::And).then(comp).repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        and.clone()
            .then(op("||").to(BinOpKind::Or).then(and).repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)))
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
    use crate::ast::macros::*;

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

        assert_ok!(expression, "nil", nil!());

        assert_ok!(expression, "true", bool!(true));
        assert_ok!(expression, "false", bool!(false));

        assert_ok!(expression, "a", ident!(a));
        assert_ok!(expression, "a1", ident!(a1));
        assert_ok!(expression, "foo", ident!(foo));
        assert_ok!(expression, "bar", ident!(bar));

        assert_ok!(expression, "1", num!(1));
        assert_ok!(expression, "10", num!(10));

        assert_ok!(expression, "\"hello\"", str!("hello"));
        assert_ok!(expression, "\"world\"", str!("world"));
        assert_ok!(expression, "\"hello world\"", str!("hello world"));

        assert_ok!(expression, "!true", not!(bool!(true)));
        assert_ok!(expression, "-1", neg!(num!(1)));

        assert_err!(expression, "1+2");

        assert_ok!(expression, "1 + 2", add!(num!(1), num!(2)));

        assert_ok!(expression, "2 - 1", sub!(num!(2), num!(1)));

        assert_err!(expression, "()");

        assert_ok!(expression, "(true)", bool!(true));
        assert_ok!(expression, "(1)", num!(1));

        assert_ok!(
            expression,
            "true && false || true",
            or!(and!(bool!(true), bool!(false)), bool!(true))
        );

        assert_ok!(
            expression,
            "true || false && true",
            or!(bool!(true), and!(bool!(false), bool!(true)))
        );

        assert_ok!(
            expression,
            "1 + 2 * 3 - 4",
            sub!(add!(num!(1), mul!(num!(2), num!(3))), num!(4))
        );

        assert_ok!(
            expression,
            "(1 + 2) * (3 - 4)",
            mul!(add!(num!(1), num!(2)), sub!(num!(3), num!(4)))
        );

        assert_ok!(
            expression,
            "1 + 2 <= -4 == false || !true",
            or!(
                eq!(lte!(add!(num!(1), num!(2)), neg!(num!(4))), bool!(false)),
                not!(bool!(true))
            )
        );
    }

    #[test]
    fn test_if_statement() {
        assert_ok!(expression, "if(true, 1)", iff!(bool!(true), num!(1)));

        assert_ok!(
            expression,
            "if(true, 1, 2)",
            iff!(bool!(true), num!(1), num!(2))
        );
    }

    #[test]
    fn test_declaration() {
        assert_ok!(
            declaration,
            "foo = 1\nbar = 2\nfoo + bar == 3",
            vec![
                ass!(foo, num!(1)),
                ass!(bar, num!(2)),
                stm!(eq!(add!(ident!(foo), ident!(bar)), num!(3))),
            ]
        );

        assert_ok!(
            declaration,
            "add a b = a + b\nadd(1, 2)",
            vec![
                fun!(add, [a, b], add!(ident!(a), ident!(b))),
                stm!(call!(add, num!(1), num!(2))),
            ]
        );
    }
}
