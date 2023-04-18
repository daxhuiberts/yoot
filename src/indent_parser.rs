#![allow(dead_code)]

use chumsky::prelude::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    OpenParen,
    CloseParen,
}

#[derive(Debug, PartialEq, Eq)]
enum TokenTree {
    Token(Token),
    Tree(Vec<TokenTree>),
}

fn lexer() -> impl Parser<char, Vec<TokenTree>, Error = Simple<char>> {
    let tt = text::ident().map(Token::Ident).map(TokenTree::Token);

    text::semantic_indentation(tt, |tts, _| TokenTree::Tree(tts)).then_ignore(end())
}

fn flatten(input: Vec<TokenTree>) -> impl Iterator<Item = Token> {
    let mut current = input.into_iter();
    let mut stack = Vec::new();

    std::iter::from_fn(move || match current.next() {
        Some(TokenTree::Token(token)) => Some(token),
        Some(TokenTree::Tree(subtree)) => {
            let mut subtree = subtree.into_iter();
            std::mem::swap(&mut current, &mut subtree);
            stack.push(subtree);
            Some(Token::OpenParen)
        }
        None => match stack.pop() {
            Some(mut prev) => {
                std::mem::swap(&mut current, &mut prev);
                Some(Token::CloseParen)
            }
            None => None,
        },
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer() {
        assert_eq!(
            lexer().parse("a\n  b\n    c\n"),
            Ok(vec![
                TokenTree::Token(Token::Ident("a".to_string())),
                TokenTree::Tree(vec![
                    TokenTree::Token(Token::Ident("b".to_string())),
                    TokenTree::Tree(vec![TokenTree::Token(Token::Ident("c".to_string()))])
                ])
            ])
        );
    }

    #[test]
    fn test_flatter() {
        assert_eq!(
            flatten(vec![
                TokenTree::Token(Token::Ident("a".to_string())),
                TokenTree::Tree(vec![
                    TokenTree::Token(Token::Ident("b".to_string())),
                    TokenTree::Tree(vec![TokenTree::Token(Token::Ident("c".to_string()))])
                ])
            ])
            .collect::<Vec<_>>(),
            vec![
                Token::Ident("a".to_string()),
                Token::OpenParen,
                Token::Ident("b".to_string()),
                Token::OpenParen,
                Token::Ident("c".to_string()),
                Token::CloseParen,
                Token::CloseParen
            ],
        );
    }
}
