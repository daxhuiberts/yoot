#![allow(dead_code)]

/// Tokenizer is a module that parses a rust style syntax into a token list.
/// It uses chumsky for tokenization, although that might not be the most performant.
/// The idea is to create a two step parsing setup (for a traditional syntax)
/// in which the tokenizer handles whitespace padding.
///
/// Entrypoint is `Tokenizer::new()` which creates Iterator object which generates Tokens.
/// It uses the `tokenize_parser()` for parsing the tokens.
use std::ops::Range;

use chumsky::prelude::*;

use crate::util::Result;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Fn,
    Ident(String),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    String(String),
}

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(T, Range<usize>);

pub struct Tokenizer {
    input: String,
    parser: Box<dyn chumsky::Parser<char, Spanned<Token>, Error = Simple<char>>>,
    offset: usize,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Tokenizer {
            input: input.into(),
            parser: Box::new(tokenize_parser()),
            offset: 0,
        }
    }

    fn next_token(&mut self) -> Result<Spanned<Token>> {
        let parse_input: &str = &self.input[self.offset..];
        // println!("{parse_input}");
        let result = self.parser.parse(parse_input);
        let Spanned(token, range) = result.map_err(|err| format!("{err:?}"))?;
        // println!("token: {token:?}; range: {range:?}");
        let new_range = (range.start + self.offset)..(range.end + self.offset);
        self.offset = new_range.end;
        Ok(Spanned(token, new_range))
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().map(|Spanned(token, _)| token).ok()
    }
}

fn tokenize_parser() -> impl chumsky::Parser<char, Spanned<Token>, Error = Simple<char>> + Clone {
    let fn_ = text::keyword("fn").to(Token::Fn);
    let ident = text::ident().map(Token::Ident);
    let open_paren = just('(').to(Token::OpenParen);
    let close_paren = just(')').to(Token::CloseParen);
    let open_brace = just('{').to(Token::OpenBrace);
    let close_brace = just('}').to(Token::CloseBrace);
    let colon = just(':').to(Token::Colon);

    let string = just("\"")
        .ignore_then(none_of("\"").repeated())
        .then_ignore(just("\""))
        .collect::<String>()
        .map(Token::String);

    let parser = fn_
        .or(ident)
        .or(open_paren)
        .or(close_paren)
        .or(open_brace)
        .or(close_brace)
        .or(colon)
        .or(string);

    parser.map_with_span(Spanned).padded()
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &'static str = r#"fn say(name: String) { print("Hello {name}") }"#;

    #[test]
    fn test_tokenize() {
        use Token::*;

        let tokenizer = Tokenizer::new(INPUT);

        let result = tokenizer.collect::<Vec<_>>();

        let expected = vec![
            Fn,
            Ident("say".into()),
            OpenParen,
            Ident("name".into()),
            Colon,
            Ident("String".into()),
            CloseParen,
            OpenBrace,
            Ident("print".into()),
            OpenParen,
            String("Hello {name}".into()),
            CloseParen,
            CloseBrace,
        ];

        assert_eq!(result, expected);
    }

    #[test]
    fn test_tokenize_parser() {
        let parser = tokenize_parser();

        let output = parser.parse(INPUT);
        assert_eq!(output, Ok(Spanned(Token::Fn, 0..2)));

        let output = parser.parse(&INPUT[2..]);
        assert_eq!(output, Ok(Spanned(Token::Ident("say".into()), 1..4)));
    }
}
