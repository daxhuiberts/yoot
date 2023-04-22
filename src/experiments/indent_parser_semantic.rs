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

pub fn semantic_indentation2<'a, C, Tok, T, F, E: chumsky::Error<C> + 'a>(
    token: T,
    make_group: F,
) -> impl Parser<C, Vec<Tok>, Error = E> + Clone + 'a
where
    C: chumsky::text::Character + std::fmt::Debug + 'a,
    Tok: std::fmt::Debug + 'a,
    T: Parser<C, Tok, Error = E> + Clone + 'a,
    F: Fn(Vec<Tok>, E::Span) -> Tok + Clone + 'a,
    <E as chumsky::Error<C>>::Span: std::fmt::Debug,
{
    let line_ws = filter(|c: &C| c.is_inline_whitespace());

    let line = token.padded_by(line_ws.ignored().repeated()).repeated();

    let lines = line_ws
        .repeated()
        .then(line.map_with_span(|line, span| (line, span)))
        .separated_by(chumsky::text::newline())
        .padded();

    lines.map(move |lines| {
        fn collapse<C, Tok, F, S>(
            mut tree: Vec<(Vec<C>, Vec<Tok>, Option<S>)>,
            make_group: &F,
        ) -> Option<Tok>
        where
            F: Fn(Vec<Tok>, S) -> Tok,
            C: std::fmt::Debug,
            Tok: std::fmt::Debug,
            S: std::fmt::Debug,
        {
            println!("TREE: {tree:?}");
            while let Some((_, tts, line_span)) = tree.pop() {
                let tt = make_group(tts, line_span?);
                println!("GROUPED: {tt:?}");
                if let Some(last) = tree.last_mut() {
                    last.1.push(tt);
                } else {
                    return Some(tt);
                }
            }
            None
        }

        let mut nesting = vec![(Vec::new(), Vec::new(), None)];
        for (indent, (mut line, line_span)) in lines {
            let mut indent = indent.as_slice();
            println!("\nLINE: {line:?}");
            println!("INDENT: {indent:?}");
            let mut i = 0;
            while let Some(tail) = nesting
                .get(i)
                .and_then(|(n, _, _)| indent.strip_prefix(n.as_slice()))
            {
                indent = tail;
                i += 1;
            }
            if let Some(tail) = collapse(nesting.split_off(i), &make_group) {
                println!("APPEND TREE CLOSED: {tail:?}");
                nesting.last_mut().unwrap().1.push(tail);
            }
            if !indent.is_empty() {
                println!("ADD INDENT: {indent:?}");
                nesting.push((indent.to_vec(), line, Some(line_span)));
            } else {
                println!("APPEND LINE: {line:?}");
                nesting.last_mut().unwrap().1.append(&mut line);
            }

            println!("NESTING: {nesting:?}");
        }

        nesting.remove(0).1
    })
}

fn tree_lexer() -> impl Parser<char, Vec<TokenTree>, Error = Simple<char>> {
    let tt = text::ident().map(Token::Ident).map(TokenTree::Token);

    semantic_indentation2(tt, |tts, _| TokenTree::Tree(tts)).then_ignore(end())
}

fn flatter(input: Vec<TokenTree>) -> impl Iterator<Item = Token> {
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

pub fn lex(input: &str) -> Vec<Token> {
    flatter(tree_lexer().parse(input).unwrap()).collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tree_lexer() {
        // tree_lexer().parse("a b\n  c d\n\n    e f\ng h\n  i\n");

        assert_eq!(
            tree_lexer().parse("a\n  b\n    c\n"),
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
            flatter(vec![
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
