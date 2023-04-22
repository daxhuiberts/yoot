#![allow(dead_code)]

/// Explorations in Chumsky's semantic_indentation parser.
use chumsky::prelude::*;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Ident(String),
    Sub(Vec<Token>),
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let tt = text::ident().map(Token::Ident);

    text::semantic_indentation(tt, |tts, _| {
        println!("{tts:?}");
        Token::Sub(tts)
    })
    .then_ignore(end())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer() {
        assert_eq!(
            lexer().parse("a\n  b\n    c\n"),
            Ok(vec![
                Token::Ident("a".to_string()),
                Token::Sub(vec![
                    Token::Ident("b".to_string()),
                    Token::Sub(vec![Token::Ident("c".to_string())])
                ])
            ])
        );
    }
}
