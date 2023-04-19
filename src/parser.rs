use super::ast::*;
use crate::util::Result;

use chumsky::prelude::*;

fn literal() -> impl chumsky::Parser<char, LitKind, Error = Simple<char>> + Clone {
    let nil = just("nil").map(|_| LitKind::Nil);

    let boolean = just("true")
        .to(true)
        .or(just("false").to(false))
        .map(LitKind::Bool);

    let number = text::int(10).from_str().unwrapped().map(LitKind::Num);

    let string = just("\"")
        .ignore_then(none_of("\"").repeated())
        .then_ignore(just("\""))
        .collect::<String>()
        .map(LitKind::Str);

    choice((nil, boolean, number, string))
}

fn expression() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> + Clone {
    recursive(|expr| {
        let literal = literal().map(|lit| Expr {
            kind: ExprKind::Lit { lit },
        });

        let if_ = just("if")
            .ignore_then(
                expr.clone()
                    .then_ignore(just(", "))
                    .then(expr.clone())
                    .then(just(", ").ignore_then(expr.clone()).or_not())
                    .delimited_by(just('('), just(')')),
            )
            .map(|((cond, then), else_)| Expr {
                kind: ExprKind::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: else_.map(Box::new),
                },
            });

        let call = text::ident()
            .then(
                expr.clone()
                    .separated_by(just(", "))
                    .delimited_by(just('('), just(')')),
            )
            .map(|(name, args)| Expr {
                kind: ExprKind::Call { name, args },
            });

        let identifier = text::ident().map(|ident| Expr {
            kind: ExprKind::Ident { name: ident },
        });

        let subexpression = expr.delimited_by(just('('), just(')'));

        let primary = choice((literal, if_, call, identifier, subexpression)).boxed();

        let unary = just('-')
            .to(UnOpKind::Neg)
            .or(just('!').to(UnOpKind::Not))
            .then(primary.clone())
            .map(|(kind, expr)| Expr {
                kind: ExprKind::UnOp {
                    kind,
                    expr: Box::new(expr),
                },
            })
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
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });

        let term = factor
            .clone()
            .then(
                op("+")
                    .to(BinOpKind::Add)
                    .or(op("-").to(BinOpKind::Sub))
                    .then(factor)
                    .repeated(),
            )
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });

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
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });

        let and = comp
            .clone()
            .then(op("&&").to(BinOpKind::And).then(comp).repeated())
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });

        and.clone()
            .then(op("||").to(BinOpKind::Or).then(and).repeated())
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            })
    })
}

fn declaration() -> impl chumsky::Parser<char, Vec<Decl>, Error = Simple<char>> {
    let expression = expression();

    let assignment = text::ident()
        .then(just(":").ignore_then(text::ident()).or_not())
        .then_ignore(just(" = "))
        .then(expression.clone())
        .map(|(name, expr)| Decl::Ass { name, expr });

    let function = text::ident()
        .then_ignore(just(" "))
        .then(
            text::ident()
                .then(just(":").ignore_then(text::ident()).or_not())
                .separated_by(just(" ")),
        )
        .then(just(" -> ").ignore_then(text::ident()).or_not())
        .then_ignore(just(" = "))
        .then(expression.clone())
        .map(|(((name, args), ret), body)| Decl::Fun {
            name,
            args,
            ret,
            body,
        });

    let statement = expression.map(|expr| Decl::Stm { expr });

    choice((assignment, function, statement))
        .separated_by(just("; ").ignored().or(just("\n").repeated().ignored()))
        .map(|decls| decls.into_iter().collect())
        .then_ignore(just("\n").or_not())
}

pub fn parser() -> impl chumsky::Parser<char, Program, Error = Simple<char>> {
    declaration().then_ignore(end()).map(Program::new)
}

pub fn parse(input: &str) -> Result<Program> {
    parser().parse(input).map_err(|err| format!("{err:?}"))
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
    fn test_declaration_expression() {
        assert_ok!(declaration, "nil", vec![stm!(nil!())]);

        assert_ok!(declaration, "a + b", vec![stm!(add!(ident!(a), ident!(b)))]);
    }

    #[test]
    fn test_declaration_assignment() {
        assert_ok!(declaration, "foo = 1", vec![ass!(foo = num!(1))]);
        assert_ok!(declaration, "foo:Num = 1", vec![ass!(foo: Num = num!(1))]);
    }

    #[test]
    fn test_declaration_function() {
        assert_ok!(
            declaration,
            "add a b = a + b",
            vec![fun!(add(a, b) => add!(ident!(a), ident!(b)))]
        );

        assert_ok!(
            declaration,
            "add a:Num b:Num = a + b",
            vec![fun!(add(a:Num, b:Num) => add!(ident!(a), ident!(b)))]
        );

        assert_ok!(
            declaration,
            "add a:Num b:Num -> Num = a + b",
            vec![fun!(add(a:Num, b:Num):Num => add!(ident!(a), ident!(b)))]
        );
    }

    #[test]
    fn test_declaration_multiple() {
        assert_ok!(
            declaration,
            "foo = 1\ninc a = a + 1\ninc(foo)",
            vec![
                ass!(foo = num!(1)),
                fun!(inc(a) => add!(ident!(a), num!(1))),
                stm!(call!(inc(ident!(foo)))),
            ]
        );
    }

    #[test]
    fn test_trailing_newline() {
        // allow multiple newlines between statements,
        // but only one trailing newline at the end.

        assert_ok!(declaration, "true", vec![stm!(bool!(true))]);
        assert_ok!(declaration, "true\n", vec![stm!(bool!(true))]);

        assert_ok!(
            declaration,
            "true\nfalse",
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_ok!(
            declaration,
            "true\nfalse\n",
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_ok!(
            declaration,
            "true\n\n\nfalse\n",
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_err!(declaration, "true\n\n");
        assert_err!(declaration, "true\nfalse\n\n");
    }

    #[test]
    fn test_declaration_separator_semicolon() {
        assert_ok!(
            declaration,
            "true; false",
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_ok!(
            declaration,
            "true; false\n",
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_err!(declaration, "true;false");
        assert_err!(declaration, "true; ; false");
        assert_err!(declaration, "true;");
        assert_err!(declaration, "true; ");
        assert_err!(declaration, "true;\n");
        assert_err!(declaration, "true; \n");
    }
}
