use crate::ast::*;
use crate::tokenizer::{Keyword, Token};
use crate::util::Result;
use chumsky::prelude::*;

fn literal() -> impl chumsky::Parser<Token, LitKind, Error = Simple<Token>> + Clone {
    select! {
        Token::Keyword(Keyword::Nil) => LitKind::Nil,
        Token::Keyword(Keyword::True) => LitKind::Bool(true),
        Token::Keyword(Keyword::False) => LitKind::Bool(false),
        Token::Num(num) => LitKind::Num(num),
    }
}

fn ident() -> impl chumsky::Parser<Token, String, Error = Simple<Token>> + Clone {
    select! {
        Token::Ident(name) => name
    }
}

fn punct(punct: &str) -> impl chumsky::Parser<Token, (), Error = Simple<Token>> + Clone {
    just(Token::Punct(punct.to_string())).ignored()
}

fn expression() -> impl chumsky::Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let literal = literal().map(|lit| Expr {
            kind: ExprKind::Lit { lit },
        });

        let if_ = just(Token::Keyword(Keyword::If))
            .ignore_then(
                expr.clone()
                    .then_ignore(punct(","))
                    .then(expr.clone())
                    .then(punct(",").ignore_then(expr.clone()).or_not())
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|((cond, then), else_)| Expr {
                kind: ExprKind::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: else_.map(Box::new),
                },
            });

        let call = ident()
            .then(
                expr.clone()
                    .separated_by(punct(","))
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|(name, args)| Expr {
                kind: ExprKind::Call { name, args },
            });

        let identifier = ident().map(|name| Expr {
            kind: ExprKind::Ident { name },
        });

        let subexpression = expr.delimited_by(just(Token::OpenParen), just(Token::CloseParen));

        let primary = choice((literal, if_, call, identifier, subexpression)).boxed();

        let unary = punct("-")
            .to(UnOpKind::Neg)
            .or(punct("!").to(UnOpKind::Not))
            .then(primary.clone())
            .map(|(kind, expr)| Expr {
                kind: ExprKind::UnOp {
                    kind,
                    expr: Box::new(expr),
                },
            })
            .or(primary);

        let factor = unary
            .clone()
            .then(
                punct("*")
                    .to(BinOpKind::Mul)
                    .or(punct("/").to(BinOpKind::Div))
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
                punct("+")
                    .to(BinOpKind::Add)
                    .or(punct("-").to(BinOpKind::Sub))
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
                punct(">=")
                    .to(BinOpKind::Gte)
                    .or(punct(">").to(BinOpKind::Gt))
                    .or(punct("<").to(BinOpKind::Lt))
                    .or(punct("<=").to(BinOpKind::Lte))
                    .or(punct("==").to(BinOpKind::Eq))
                    .or(punct("!=").to(BinOpKind::Neq))
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
            .then(punct("&&").to(BinOpKind::And).then(comp).repeated())
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });

        and.clone()
            .then(punct("||").to(BinOpKind::Or).then(and).repeated())
            .foldl(|left, (kind, right)| Expr {
                kind: ExprKind::BinOp {
                    kind,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            })
    })
}

fn declaration() -> impl chumsky::Parser<Token, Vec<Decl>, Error = Simple<Token>> {
    let expression = expression();

    let assignment = ident()
        .then(punct(":").ignore_then(ident()).or_not())
        .then_ignore(punct("="))
        .then(expression.clone())
        .map(|(name, expr)| Decl::Ass {
            name,
            expr: vec![Decl::Stm { expr }],
        });

    let function = ident()
        .then(
            ident()
                .then(punct(":").ignore_then(ident()).or_not())
                .repeated(), // .separated_by(op(","))
                             // .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
        )
        .then(punct("->").ignore_then(ident()).or_not())
        .then_ignore(punct("="))
        .then(expression.clone())
        .map(|(((name, args), ret), body)| Decl::Fun {
            name,
            args,
            ret,
            body: vec![Decl::Stm { expr: body }],
        });

    let statement = expression.map(|expr| Decl::Stm { expr });

    choice((assignment, function, statement))
        .separated_by(choice((
            punct(";").ignored(),
            just(Token::Newline).repeated().at_least(1).ignored(),
        )))
        .map(|decls| decls.into_iter().collect())
        .then_ignore(just(Token::Newline).or_not())
}

pub fn parser() -> impl chumsky::Parser<Token, Program, Error = Simple<Token>> {
    declaration().then_ignore(end()).map(Program::new)
}

pub fn parse(tokens: Vec<Token>) -> Result<Program> {
    parser().parse(tokens).map_err(|err| format!("{err:?}"))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::macros::*;
    use crate::tokenizer::tokenize;
    use crate::util::macros::*;

    fn parse_expr(input: &str) -> std::result::Result<Expr, Vec<chumsky::error::Simple<Token>>> {
        expression().then_ignore(end()).parse(tokenize(input))
    }

    fn parse_decl(
        input: &str,
    ) -> std::result::Result<Vec<Decl>, Vec<chumsky::error::Simple<Token>>> {
        declaration().then_ignore(end()).parse(tokenize(input))
    }

    #[test]
    fn test_expression() {
        assert_err!(parse_expr(""));
        assert_err!(parse_expr(" "));
        assert_err!(parse_expr("1a"));
        assert_err!(parse_expr("01"));

        assert_ok!(parse_expr("nil"), nil!());

        assert_ok!(parse_expr("true"), bool!(true));
        assert_ok!(parse_expr("false"), bool!(false));

        assert_ok!(parse_expr("a"), ident!(a));
        assert_ok!(parse_expr("a1"), ident!(a1));
        assert_ok!(parse_expr("foo"), ident!(foo));
        assert_ok!(parse_expr("bar"), ident!(bar));

        assert_ok!(parse_expr("1"), num!(1));
        assert_ok!(parse_expr("10"), num!(10));

        assert_ok!(parse_expr("!true"), not!(bool!(true)));
        assert_ok!(parse_expr("-1"), neg!(num!(1)));

        // // No special whitespace handling in indent_parser
        // assert_err!(parse_expr("1+2"));

        assert_ok!(parse_expr("1 + 2"), add!(num!(1), num!(2)));
        assert_ok!(parse_expr("2 - 1"), sub!(num!(2), num!(1)));

        assert_err!(parse_expr("()"));

        assert_ok!(parse_expr("(true)"), bool!(true));
        assert_ok!(parse_expr("(1)"), num!(1));

        assert_ok!(
            parse_expr("true && false || true"),
            or!(and!(bool!(true), bool!(false)), bool!(true))
        );

        assert_ok!(
            parse_expr("true || false && true"),
            or!(bool!(true), and!(bool!(false), bool!(true)))
        );

        assert_ok!(
            parse_expr("1 + 2 * 3 - 4"),
            sub!(add!(num!(1), mul!(num!(2), num!(3))), num!(4))
        );

        assert_ok!(
            parse_expr("(1 + 2) * (3 - 4)"),
            mul!(add!(num!(1), num!(2)), sub!(num!(3), num!(4)))
        );

        assert_ok!(
            parse_expr("1 + 2 <= -4 == false || !true"),
            or!(
                eq!(lte!(add!(num!(1), num!(2)), neg!(num!(4))), bool!(false)),
                not!(bool!(true))
            )
        );
    }

    #[test]
    fn test_if_statement() {
        assert_ok!(parse_expr("if(true, 1)"), if_!(bool!(true), num!(1)));

        assert_ok!(
            parse_expr("if(true, 1, 2)"),
            if_!(bool!(true), num!(1), num!(2))
        );
    }

    #[test]
    fn test_call_statement() {
        assert_ok!(parse_expr("foo()"), call!(foo()));
        assert_ok!(parse_expr("foo(bar)"), call!(foo(ident!(bar))));
        assert_ok!(
            parse_expr("foo(bar, baz)"),
            call!(foo(ident!(bar), ident!(baz)))
        );
        assert_ok!(
            parse_expr("foo(bar, baz, quux)"),
            call!(foo(ident!(bar), ident!(baz), ident!(quux)))
        );

        assert_ok!(parse_expr("foo(1 + 2)"), call!(foo(add!(num!(1), num!(2)))));
        assert_ok!(
            parse_expr("foo(1 + 2, 3 + 4)"),
            call!(foo(add!(num!(1), num!(2)), add!(num!(3), num!(4))))
        );
    }

    #[test]
    fn test_declaration_expression() {
        assert_ok!(parse_decl("nil"), vec![stm!(nil!())]);

        assert_ok!(parse_decl("a + b"), vec![stm!(add!(ident!(a), ident!(b)))]);
    }

    #[test]
    fn test_declaration_assignment() {
        assert_ok!(parse_decl("foo = 1"), vec![ass!(foo = num!(1))]);
        assert_ok!(parse_decl("foo:Num = 1"), vec![ass!(foo: Num = num!(1))]);
    }

    #[test]
    fn test_declaration_function() {
        assert_ok!(
            parse_decl("add a b = a + b"),
            vec![fun!(add(a, b) => add!(ident!(a), ident!(b)))]
        );

        assert_ok!(
            parse_decl("add a:Num b:Num = a + b"),
            vec![fun!(add(a:Num, b:Num) => add!(ident!(a), ident!(b)))]
        );

        assert_ok!(
            parse_decl("add a:Num b:Num -> Num = a + b"),
            vec![fun!(add(a:Num, b:Num):Num => add!(ident!(a), ident!(b)))]
        );
    }

    #[test]
    fn test_declaration_multiple() {
        assert_ok!(
            parse_decl("foo = 1\ninc a = a + 1\ninc(foo)"),
            vec![
                ass!(foo = num!(1)),
                fun!(inc(a) => add!(ident!(a), num!(1))),
                stm!(call!(inc(ident!(foo)))),
            ]
        );
    }

    #[test]
    fn test_newline_handling() {
        // allow multiple newlines between statements,
        // but only one trailing newline at the end.

        assert_ok!(parse_decl("true"), vec![stm!(bool!(true))]);
        assert_ok!(parse_decl("true\n"), vec![stm!(bool!(true))]);

        assert_ok!(
            parse_decl("true\nfalse"),
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_ok!(
            parse_decl("true\nfalse\n"),
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_ok!(
            parse_decl("true\n\n\nfalse\n"),
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_err!(parse_decl("true\n\n"));
        assert_err!(parse_decl("true\nfalse\n\n"));
    }

    #[test]
    fn test_declaration_separator_semicolon() {
        assert_ok!(
            parse_decl("true; false"),
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        assert_ok!(
            parse_decl("true; false\n"),
            vec![stm!(bool!(true)), stm!(bool!(false))]
        );

        // // No special whitespace handling in indent_parser
        // assert_err!(parse_decl("true;false"));

        assert_err!(parse_decl("true; ; false"));
        assert_err!(parse_decl("true;"));
        assert_err!(parse_decl("true; "));
        assert_err!(parse_decl("true;\n"));
        assert_err!(parse_decl("true; \n"));
    }
}
