use chumsky::prelude::*;
use chumsky::text::keyword;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Keyword {
    Nil,
    True,
    False,
    If,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Keyword(Keyword),
    Ident(String),
    Op(String),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenBlock,
    CloseBlock,
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
        keyword("if").to(Keyword::If),
    ))
    .map(Token::Keyword);

    let number = number().map(Token::Num);
    let ident = text::ident().map(Token::Ident);
    let op = one_of(r"!@#$%^&*-+=;:,./<>?\|")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Op);

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

pub fn lex(input: &str) -> Vec<Token> {
    let lexer = tree_lexer();
    let token_tree = lexer.parse(input).unwrap();
    flatten(token_tree).collect::<Vec<_>>()
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

        assert_eq!(lexer.parse("("), Ok(Token::OpenParen));
        assert_eq!(lexer.parse(")"), Ok(Token::CloseParen));
        assert_eq!(lexer.parse("{"), Ok(Token::OpenBrace));
        assert_eq!(lexer.parse("}"), Ok(Token::CloseBrace));
        assert_eq!(lexer.parse("["), Ok(Token::OpenBracket));
        assert_eq!(lexer.parse("]"), Ok(Token::CloseBracket));
    }

    #[test]
    fn test_tree_lexer_line() {
        assert_eq!(
            tree_lexer().parse("foo bar"),
            Ok(vec![
                TokenTree::Token(Token::Ident("foo".to_string())),
                TokenTree::Token(Token::Ident("bar".to_string())),
            ])
        );
    }

    #[test]
    fn test_tree_lexer_indent() {
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
                Token::OpenBlock,
                Token::Ident("b".to_string()),
                Token::OpenBlock,
                Token::Ident("c".to_string()),
                Token::CloseBlock,
                Token::CloseBlock
            ],
        );
    }

    // #[test]
    // fn test_simple_newline() {
    //     println!("{:?}", flatten(tree_lexer().parse("foo\nbar").unwrap()).collect::<Vec<_>>());
    //     panic!();
    // }
}
