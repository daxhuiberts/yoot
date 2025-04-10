use crate::ast::*;
use crate::indent_stream::indent_stream;
use crate::util::*;
use chumsky::prelude::*;

type Item = char;
const NEWLINE: char = '\n';
const OPEN_PAREN: char = '(';
const CLOSE_PAREN: char = ')';
const OPEN_BLOCK: char = crate::indent_stream::OPEN_BLOCK;
const CLOSE_BLOCK: char = crate::indent_stream::CLOSE_BLOCK;

fn ident() -> impl chumsky::Parser<Item, String, Error = Simple<Item>> + Clone {
    text::ident()
}

fn punct(punct: &str) -> impl chumsky::Parser<Item, (), Error = Simple<Item>> + Clone + '_ {
    just(punct).ignored()
}

fn literal() -> impl chumsky::Parser<Item, LitKind, Error = Simple<Item>> + Clone {
    let nil = just("nil").map(|_| LitKind::Nil);

    let boolean = just("true")
        .to(true)
        .or(just("false").to(false))
        .map(LitKind::Bool);

    let number = text::int(10).from_str().unwrapped().map(LitKind::Num);

    let string = just("\"")
        .ignore_then(just("\"").not().repeated())
        .then_ignore(just("\""))
        .map(String::from_iter)
        .map(LitKind::String);

    choice((nil, boolean, number, string))
}

fn sub_expression() -> impl chumsky::Parser<Item, Expr, Error = Simple<Item>> + Clone {
    let binop_mapper = |left, (kind, right): (_, _)| Expr {
        kind: ExprKind::BinOp {
            kind,
            left: Box::new(left),
            right: Box::new(right),
        },
    };

    recursive(|expr| {
        let literal = literal().map(|lit| Expr {
            kind: ExprKind::Lit { lit },
        });

        let call = ident()
            .then(
                expr.clone()
                    .separated_by(punct(", "))
                    .delimited_by(just(OPEN_PAREN), just(CLOSE_PAREN)),
            )
            .map(|(name, args)| Expr {
                kind: ExprKind::Call { name, args },
            });

        let identifier = ident().map(|name| Expr {
            kind: ExprKind::Ident { name },
        });

        let subexpression = expr.delimited_by(just(OPEN_PAREN), just(CLOSE_PAREN));

        let primary = choice((literal, call, identifier, subexpression)).boxed();

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
                punct(" * ")
                    .to(BinOpKind::Mul)
                    .or(punct(" / ").to(BinOpKind::Div))
                    .then(unary)
                    .repeated(),
            )
            .foldl(binop_mapper);

        let term = factor
            .clone()
            .then(
                punct(" + ")
                    .to(BinOpKind::Add)
                    .or(punct(" - ").to(BinOpKind::Sub))
                    .then(factor)
                    .repeated(),
            )
            .foldl(binop_mapper);

        let comp = term
            .clone()
            .then(
                punct(" >= ")
                    .to(BinOpKind::Gte)
                    .or(punct(" > ").to(BinOpKind::Gt))
                    .or(punct(" < ").to(BinOpKind::Lt))
                    .or(punct(" <= ").to(BinOpKind::Lte))
                    .or(punct(" == ").to(BinOpKind::Eq))
                    .or(punct(" != ").to(BinOpKind::Neq))
                    .then(term)
                    .repeated(),
            )
            .foldl(binop_mapper);

        let and = comp
            .clone()
            .then(punct(" && ").to(BinOpKind::And).then(comp).repeated())
            .foldl(binop_mapper);

        and.clone()
            .then(punct(" || ").to(BinOpKind::Or).then(and).repeated())
            .foldl(binop_mapper)
    })
}

fn top_level_expression(
    decls_parser: impl chumsky::Parser<Item, Vec<Decl>, Error = Simple<Item>> + Clone,
) -> impl chumsky::Parser<Item, Expr, Error = Simple<Item>> + Clone {
    {
        let inline_args =
            punct(" ").ignore_then(sub_expression().separated_by(punct(", ")).at_least(1));

        let blocks = decls_parser
            .delimited_by(just(OPEN_BLOCK), just(CLOSE_BLOCK))
            .map(|decls| Expr {
                kind: ExprKind::Block { decls },
            })
            .repeated()
            .at_least(1);

        ident()
            .then(choice((
                inline_args
                    .clone()
                    .then_ignore(punct(",").or_not())
                    .chain(blocks.clone())
                    .labelled("inline and block args"),
                inline_args.labelled("only inline args"),
                blocks.labelled("only blocks args"),
            )))
            .map(|(name, args)| Expr {
                kind: ExprKind::Call { name, args },
            })
            .or(sub_expression())
    }
}

fn declaration() -> impl chumsky::Parser<Item, Vec<Decl>, Error = Simple<Item>> + Clone {
    recursive(|declaration| {
        let expression = top_level_expression(declaration.clone());

        let definition = declaration
            .delimited_by(just(OPEN_BLOCK), just(CLOSE_BLOCK))
            .or(punct(" ").ignore_then(expression.clone().map(|expr| vec![Decl::Stm { expr }])));

        let assignment = ident()
            .then(punct(":").ignore_then(ident()).or_not())
            .then_ignore(punct(" ="))
            .then(definition.clone())
            .map(|(name, decls)| Decl::Ass { name, expr: decls });

        let function = ident()
            .then_ignore(punct(" "))
            .then(
                ident()
                    .then(punct(":").ignore_then(ident()).or_not())
                    .separated_by(punct(" ")),
            )
            .then(punct(" -> ").ignore_then(ident()).or_not())
            .then_ignore(punct(" ="))
            .then(definition)
            .map(|(((name, args), ret), decls)| Decl::Fun {
                name,
                args,
                ret,
                body: decls,
            });

        let statement = expression.map(|expr| Decl::Stm { expr });

        choice((assignment, function, statement))
            .map(Some)
            .or(punct("# ").then(none_of(NEWLINE).repeated()).map(|_| None))
            .separated_by(choice((
                punct("; ").ignored(),
                just(NEWLINE).repeated().at_least(1).ignored(),
            )))
            .flatten()
    })
    .then_ignore(just(NEWLINE).or_not())
}

fn transform_expr(expr: Expr) -> Expr {
    match expr.kind {
        ExprKind::Call { name, args } => {
            let args: Vec<_> = args.into_iter().map(transform_expr).collect();

            match name.as_str() {
                "if" if (args.len() == 2 || args.len() == 3) => Expr {
                    kind: ExprKind::If {
                        cond: Box::new(args[0].clone()),
                        then: Box::new(args[1].clone()),
                        else_: args.get(2).map(|else_| Box::new(else_.clone())),
                    },
                },
                "print" if args.len() == 1 => Expr {
                    kind: ExprKind::Print {
                        expr: Box::new(args[0].clone()),
                    },
                },
                "while" if args.len() == 2 => Expr {
                    kind: ExprKind::While {
                        cond: Box::new(args[0].clone()),
                        do_: Box::new(args[1].clone()),
                    },
                },
                _ => Expr {
                    kind: ExprKind::Call { name, args },
                },
            }
        }
        ExprKind::UnOp { kind, expr } => Expr {
            kind: ExprKind::UnOp {
                kind,
                expr: Box::new(transform_expr(*expr)),
            },
        },
        ExprKind::BinOp { kind, left, right } => Expr {
            kind: ExprKind::BinOp {
                kind,
                left: Box::new(transform_expr(*left)),
                right: Box::new(transform_expr(*right)),
            },
        },
        ExprKind::Lit { .. } | ExprKind::Ident { .. } => expr,
        ExprKind::If { .. } | ExprKind::Print { .. } | ExprKind::While { .. } => {
            // panic!("should not exist here")
            expr
        }
        ExprKind::Block { decls } => Expr {
            kind: ExprKind::Block {
                decls: transform_decls(decls),
            },
        },
    }
}

fn transform_decls(decls: Vec<Decl>) -> Vec<Decl> {
    decls
        .into_iter()
        .map(|decl| match decl {
            Decl::Ass { name, expr } => Decl::Ass {
                name,
                expr: transform_decls(expr),
            },
            Decl::Fun {
                name,
                args,
                ret,
                body,
            } => Decl::Fun {
                name,
                args,
                ret,
                body: transform_decls(body),
            },
            Decl::Stm { expr } => Decl::Stm {
                expr: transform_expr(expr),
            },
        })
        .tuple_merger(|a, b| match (a, b) {
            (
                Decl::Stm {
                    expr:
                        Expr {
                            kind:
                                ExprKind::If {
                                    cond,
                                    then,
                                    else_: None,
                                },
                        },
                },
                Decl::Stm {
                    expr:
                        Expr {
                            kind:
                                ExprKind::Call {
                                    name: name_b,
                                    args: args_b,
                                },
                        },
                },
            ) if name_b == "else" && args_b.len() == 1 => Some(Decl::Stm {
                expr: Expr {
                    kind: ExprKind::If {
                        cond: cond.clone(),
                        then: then.clone(),
                        else_: Some(Box::new(args_b[0].clone())),
                    },
                },
            }),
            _ => None,
        })
        .collect()
}

fn parse_inner<T>(
    input: &str,
    parser: impl chumsky::Parser<Item, T, Error = Simple<Item>>,
    transformer: impl Fn(T) -> T,
) -> std::result::Result<T, Vec<chumsky::error::Simple<Item>>> {
    parser
        .then_ignore(end())
        .parse(indent_stream(input).collect::<String>())
        .map(transformer)
}

pub fn parse(input: &str) -> Result<Program> {
    parse_inner(input, declaration(), transform_decls)
        .map(Program::new)
        .map_err(|err| format!("{err:?}"))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::macros::*;
    use crate::util::macros::*;

    fn parse_expr(input: &str) -> std::result::Result<Expr, Vec<chumsky::error::Simple<Item>>> {
        parse_inner(input, top_level_expression(declaration()), transform_expr)
    }

    fn parse_decl(
        input: &str,
    ) -> std::result::Result<Vec<Decl>, Vec<chumsky::error::Simple<Item>>> {
        parse_inner(input, declaration(), transform_decls)
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

        assert_ok!(parse_expr("\"\""), string!(""));
        assert_ok!(parse_expr("\"foo\""), string!("foo"));

        assert_ok!(parse_expr("!true"), not!(bool!(true)));
        assert_ok!(parse_expr("-1"), neg!(num!(1)));

        assert_err!(parse_expr("1+2"));

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

        assert_ok!(parse_expr("if true, 1"), if_!(bool!(true), num!(1)));

        assert_ok!(
            parse_expr("if(true, 1, 2)"),
            if_!(bool!(true), num!(1), num!(2))
        );

        assert_ok!(
            parse_expr("if true, 1, 2"),
            if_!(bool!(true), num!(1), num!(2))
        );

        assert_ok!(
            parse_expr("foo if(true, 1, 2)"),
            call!(foo(if_!(bool!(true), num!(1), num!(2))))
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

        // Yay!!! This now works!
        assert_ok!(parse_expr("foo(1) + 2"), add!(call!(foo(num!(1))), num!(2)));
    }

    #[test]
    fn test_print_statement() {
        assert_ok!(parse_expr("print nil"), print_!(nil!()));
        assert_ok!(parse_expr("print true"), print_!(bool!(true)));
        assert_ok!(parse_expr("print 1"), print_!(num!(1)));
    }

    #[test]
    fn test_block_expression() {
        assert_ok!(parse_expr("foo"), ident!(foo));
        assert_ok!(parse_expr("foo a"), call!(foo(ident!(a))));
        assert_ok!(parse_expr("foo a, b"), call!(foo(ident!(a), ident!(b))));

        assert_ok!(
            parse_expr("foo\n  bar"),
            call!(foo(block!(stm!(ident!(bar)))))
        );
        assert_ok!(
            parse_expr("foo\n  bar\n    baz"),
            call!(foo(block!(stm!(call!(bar(block!(stm!(ident!(baz)))))))))
        );

        assert_ok!(
            parse_expr("foo a\n  bar"),
            call!(foo(ident!(a), block!(stm!(ident!(bar)))))
        );
        assert_ok!(
            parse_expr("foo a,\n  bar"),
            call!(foo(ident!(a), block!(stm!(ident!(bar)))))
        );
        assert_ok!(
            parse_expr("foo a, b\n  bar"),
            call!(foo(ident!(a), ident!(b), block!(stm!(ident!(bar)))))
        );

        // // NOT ANYMORE!!!
        // // Yoot allows for multiple blocks by first using a deep indent followed by a shallow indent.
        // assert_ok!(
        //     parse_expr("foo\n    bar\n  baz"),
        //     call!(foo(ident!(bar), ident!(baz)))
        // );
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

        assert_err!(parse_decl("true;false"));

        assert_err!(parse_decl("true; ; false"));
        assert_err!(parse_decl("true;"));
        assert_err!(parse_decl("true; "));
        assert_err!(parse_decl("true;\n"));
        assert_err!(parse_decl("true; \n"));
    }

    #[test]
    fn test_transform_decls() {
        assert_eq!(transform_decls(vec![]), vec![]);
        assert_eq!(transform_decls(vec![stm!(nil!())]), vec![stm!(nil!())]);

        assert_eq!(
            transform_decls(vec![
                stm!(if_!(bool!(true), num!(1))),
                stm!(call!(else(num!(2))))
            ]),
            vec![stm!(if_!(bool!(true), num!(1), num!(2)))]
        );

        assert_eq!(
            transform_decls(vec![
                stm!(if_!(bool!(true), num!(1), num!(2))),
                stm!(call!(else(num!(2))))
            ]),
            vec![
                stm!(if_!(bool!(true), num!(1), num!(2))),
                stm!(call!(else(num!(2))))
            ]
        );
    }
}
