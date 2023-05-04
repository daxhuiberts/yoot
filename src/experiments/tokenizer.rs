use chumsky::prelude::*;
use chumsky::text::keyword;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Keyword {
    Nil,
    True,
    False,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Keyword(Keyword),
    Ident(String),
    Punct(String),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenBlock,
    CloseBlock,
    Newline,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TokenTree {
    Token(Token),
    Tree(Vec<TokenTree>),
}

fn number() -> impl Parser<char, i64, Error = Simple<char>> + Clone {
    // let negative = just('-').or_not();
    // let number = text::int(10).from_str::<i64>().unwrapped();
    // negative
    //     .then(number)
    //     .map(|(neg, num)| if neg.is_some() { -num } else { num })
    text::int(10).from_str().unwrapped()
}

fn token_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let keywords = choice((
        keyword("nil").to(Keyword::Nil),
        keyword("true").to(Keyword::True),
        keyword("false").to(Keyword::False),
    ))
    .map(Token::Keyword);

    let number = number().map(Token::Num);
    let ident = text::ident().map(Token::Ident);
    let op = one_of(r"!@#$%^&*-+=;:,./<>?\|")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Punct);

    let container = select! {
        '(' => Token::OpenParen,
        ')' => Token::CloseParen,
        '{' => Token::OpenBrace,
        '}' => Token::CloseBrace,
        '[' => Token::OpenBracket,
        ']' => Token::CloseBracket,
    };

    choice((keywords, number, ident, op, container))
}

fn tree_lexer() -> impl Parser<char, Vec<TokenTree>, Error = Simple<char>> {
    let tt = token_lexer().map(TokenTree::Token);
    semantic_indentation(tt, TokenTree::Token(Token::Newline), |tts, _| {
        TokenTree::Tree(tts)
    })
    .then_ignore(end())
    .map(|output| {
        // remove first newline which should not be there.
        output[1..].to_vec()
    })
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
            Some(Token::OpenBlock)
        }
        None => match stack.pop() {
            Some(mut prev) => {
                std::mem::swap(&mut current, &mut prev);
                Some(Token::CloseBlock)
            }
            None => None,
        },
    })
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let lexer = tree_lexer();
    let token_tree = lexer.parse(input).unwrap();
    flatten(token_tree).collect::<Vec<_>>()
}

pub fn semantic_indentation<'a, C, Tok, T, F, E: chumsky::Error<C> + 'a>(
    token: T,
    newline_token: Tok,
    make_group: F,
) -> impl Parser<C, Vec<Tok>, Error = E> + Clone + 'a
where
    C: chumsky::text::Character + 'a,
    Tok: Clone + 'a,
    T: Parser<C, Tok, Error = E> + Clone + 'a,
    F: Fn(Vec<Tok>, E::Span) -> Tok + Clone + 'a,
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
        {
            while let Some((_, tts, line_span)) = tree.pop() {
                let tt = make_group(tts, line_span?);
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
            let mut i = 0;
            while let Some(tail) = nesting
                .get(i)
                .and_then(|(n, _, _)| indent.strip_prefix(n.as_slice()))
            {
                indent = tail;
                i += 1;
            }
            if let Some(tail) = collapse(nesting.split_off(i), &make_group) {
                nesting.last_mut().unwrap().1.push(tail);
            }
            if !indent.is_empty() {
                nesting.push((indent.to_vec(), line, Some(line_span)));
            } else {
                nesting.last_mut().unwrap().1.push(newline_token.clone());
                nesting.last_mut().unwrap().1.append(&mut line);
            }
        }

        if let Some(tail) = collapse(nesting.split_off(1), &make_group) {
            nesting.last_mut().unwrap().1.push(tail);
        }

        nesting.remove(0).1
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_token_lexer() {
        let lexer = token_lexer().then_ignore(end());

        // assert_eq!(lexer.parse("-1"), Ok(Token::Num(-1)));
        assert_eq!(lexer.parse("0"), Ok(Token::Num(0)));
        assert_eq!(lexer.parse("1"), Ok(Token::Num(1)));
        assert_eq!(lexer.parse("10"), Ok(Token::Num(10)));

        assert!(lexer.parse("-1").is_err());
        assert!(lexer.parse("01").is_err());

        assert_eq!(lexer.parse("nil"), Ok(Token::Keyword(Keyword::Nil)));
        assert_eq!(lexer.parse("true"), Ok(Token::Keyword(Keyword::True)));
        assert_eq!(lexer.parse("false"), Ok(Token::Keyword(Keyword::False)));

        assert_eq!(lexer.parse("foo"), Ok(Token::Ident("foo".to_string())));
        assert_eq!(lexer.parse("nill"), Ok(Token::Ident("nill".to_string())));

        assert_eq!(lexer.parse("!"), Ok(Token::Punct("!".to_string())));
        assert_eq!(lexer.parse("="), Ok(Token::Punct("=".to_string())));
        assert_eq!(lexer.parse("=="), Ok(Token::Punct("==".to_string())));

        assert_eq!(lexer.parse("("), Ok(Token::OpenParen));
        assert_eq!(lexer.parse(")"), Ok(Token::CloseParen));
        assert_eq!(lexer.parse("{"), Ok(Token::OpenBrace));
        assert_eq!(lexer.parse("}"), Ok(Token::CloseBrace));
        assert_eq!(lexer.parse("["), Ok(Token::OpenBracket));
        assert_eq!(lexer.parse("]"), Ok(Token::CloseBracket));
    }

    #[test]
    fn test_tree_lexer_empty_line() {
        assert_eq!(tree_lexer().parse(""), Ok(vec![]));
    }

    #[test]
    fn test_tree_lexer_basic_single_line() {
        assert_eq!(
            tree_lexer().parse("foo bar"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Token(Token::Ident("bar".to_string())),
            ])
        );
    }

    #[test]
    fn test_tree_lexer_basic_single_line_with_newline() {
        assert_eq!(
            tree_lexer().parse("foo bar\n"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Token(Token::Ident("bar".to_string())),
                TokenTree::Token(Token::Newline),
            ])
        );
    }

    #[test]
    fn test_tree_lexer_basic_multi_line() {
        assert_eq!(
            tree_lexer().parse("foo\nbar"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Token(Token::Newline),
                TokenTree::Token(Token::Ident("bar".to_string())),
            ])
        );
    }

    #[test]
    fn test_tree_lexer_basic_multi_line_with_empty_lines() {
        assert_eq!(
            tree_lexer().parse("foo\n\nbar"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Token(Token::Newline),
                TokenTree::Token(Token::Newline),
                TokenTree::Token(Token::Ident("bar".to_string())),
            ])
        );
    }

    #[test]
    fn test_tree_lexer_indent2() {
        assert_eq!(
            tree_lexer().parse("a\n  b"),
            Ok(vec![
                TokenTree::Token(Token::Ident("a".to_string())),
                TokenTree::Tree(vec![TokenTree::Token(Token::Ident("b".to_string())),])
            ])
        );
    }

    #[test]
    fn test_tree_lexer_indent_with_next_line() {
        assert_eq!(
            tree_lexer().parse("foo\n  bar\nbaz"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Tree(vec![TokenTree::Token(Token::Ident("bar".to_string())),]),
                TokenTree::Token(Token::Newline),
                TokenTree::Token(Token::Ident("baz".to_string())),
            ])
        );
    }

    #[test]
    fn test_tree_lexer_multi_indent() {
        assert_eq!(
            tree_lexer().parse("a\n  b\n    c\n"),
            Ok(vec![
                TokenTree::Token(Token::Ident("a".to_string())),
                TokenTree::Tree(vec![
                    TokenTree::Token(Token::Ident("b".to_string())),
                    TokenTree::Tree(vec![TokenTree::Token(Token::Ident("c".to_string())),])
                ]),
                TokenTree::Token(Token::Newline)
            ])
        );
    }

    #[test]
    fn test_tree_lexer_double_block() {
        assert_eq!(
            tree_lexer().parse("foo\n    indent_two\n  indent_one\n"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Tree(vec![TokenTree::Token(Token::Ident(
                    "indent_two".to_string()
                ))]),
                TokenTree::Tree(vec![TokenTree::Token(Token::Ident(
                    "indent_one".to_string()
                ))]),
                TokenTree::Token(Token::Newline)
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
                    TokenTree::Tree(vec![TokenTree::Token(Token::Ident("c".to_string())),])
                ]),
                TokenTree::Token(Token::Newline)
            ])
            .collect::<Vec<_>>(),
            vec![
                Token::Ident("a".to_string()),
                Token::OpenBlock,
                Token::Ident("b".to_string()),
                Token::OpenBlock,
                Token::Ident("c".to_string()),
                Token::CloseBlock,
                Token::CloseBlock,
                Token::Newline,
            ],
        );
    }

    #[test]
    fn test_lext() {
        assert_eq!(
            tokenize("a\n  b\n   c\n"),
            vec![
                Token::Ident("a".to_string()),
                Token::OpenBlock,
                Token::Ident("b".to_string()),
                Token::OpenBlock,
                Token::Ident("c".to_string()),
                Token::CloseBlock,
                Token::CloseBlock,
                Token::Newline,
            ],
        );
    }
}
