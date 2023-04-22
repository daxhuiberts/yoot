#![allow(dead_code)]

/// Trying to explore a block parser.
use chumsky::prelude::*;

use crate::util::Result;

#[derive(Debug, PartialEq, Eq)]
enum Expr {
    Bool(bool),
    Num(i64),
    Ident(String),
    Call(String, Vec<Expr>),
}

fn parse(input: &str) -> Result<Expr> {
    let mut iter = input.lines();
    let line = iter.next().unwrap_or("");
    block_parser((line, Box::new(iter)))
}

fn block_parser<'a>(
    (line, rest): (&'a str, Box<dyn Iterator<Item = &'a str> + 'a>),
) -> Result<Expr> {
    let mut sub_block = rest.map_while(|l| {
        if l.starts_with("  ") {
            Some(&l[2..])
        } else {
            None
        }
    });

    let sub_expr = if let Some(next_line) = sub_block.next() {
        Some(block_parser((next_line, Box::new(sub_block)))?)
    } else {
        None
    };

    let expr = expression_parser()
        .parse(line)
        .map_err(|err| format!("{err:?}"))?;

    match (expr, sub_expr) {
        (Expr::Ident(name), Some(subexpr)) => Ok(Expr::Call(name, vec![subexpr])),
        (expr, Some(subexpr)) => Err(format!(
            "didn't expect subexpression {subexpr:?} for {expr:?}"
        )),
        (expr, None) => Ok(expr),
    }
}

fn expression_parser() -> impl chumsky::Parser<char, Expr, Error = Simple<char>> + Clone {
    let boolean = just("true")
        .to(true)
        .or(just("false").to(false))
        .map(Expr::Bool);

    let number = text::int(10).from_str().unwrapped().map(Expr::Num);

    let identifier = text::ident().map(Expr::Ident);

    choice((boolean, number, identifier))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("foo\n  bar\n"),
            Ok(Expr::Call(
                "foo".to_string(),
                vec![Expr::Ident("bar".to_string())]
            ))
        );
    }
}
