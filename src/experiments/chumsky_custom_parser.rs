#![allow(dead_code)]

/// Explorations in Chumsky's stream and custom parser.
/// The internals of Chumsky are difficult to digest.
/// Lots of type parameters and stuff.
///
/// Aparently custom is unusable in the current release
/// (see https://github.com/zesterer/chumsky/issues/160)
/// In the zero-copy branch it should work though.
use chumsky::prelude::*;

type Line = &'static str;

fn line_parser() -> impl Parser<Line, Line, Error = Simple<Line>> {
    any()
}

#[cfg(test)]
mod test {
    use super::*;

    use chumsky::{
        error::{Cheap, Located},
        primitive::custom,
        stream::Stream,
        Error,
    };

    #[test]
    fn test_line_parser() {
        let input: &[&str] = &["foo", "bar"];
        assert_eq!(line_parser().parse(input), Ok("foo"));
    }

    #[test]
    fn test_stream() {
        let input: &[&str] = &["foo", "bar"];
        let mut stream = Stream::from(input);
        let tokens_iter = stream.fetch_tokens();
        let tokens = tokens_iter.collect::<Vec<_>>();

        assert_eq!(tokens, vec![("foo", 0..1), ("bar", 1..2)]);

        let new_iter = tokens.into_iter();
        let mut new_stream = Stream::from_iter(0..new_iter.len(), new_iter);
        let new_tokens_iter = new_stream.fetch_tokens();
        let new_tokens = new_tokens_iter.collect::<Vec<_>>();

        assert_eq!(new_tokens, vec![("foo", 0..1), ("bar", 1..2)]);
    }

    #[test]
    fn test_custom() {
        assert_eq!(custom(custom_fn).parse("foo"), Ok(()));
    }

    fn custom_fn(
        _s: &mut Stream<'_, char, <Cheap<char> as Error<char>>::Span>,
    ) -> (
        Vec<Located<char, Cheap<char>>>,
        Result<((), Option<Located<char, Cheap<char>>>), Located<char, Cheap<char>>>,
    ) {
        (vec![], Ok(((), None)))
    }
}
