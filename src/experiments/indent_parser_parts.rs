#![allow(dead_code)]

/// Implementing chumsky like just/ident/keyword parsers.

trait Parser<T: ?Sized> {
    fn parse_inner<'a>(&self, input: &'a str) -> Option<&'a T>;

    fn parse<'a>(&self, input: &'a str) -> Option<&'a T> {
        self.parse_inner(input)
    }
}

struct Just(&'static str);

impl Parser<str> for Just {
    fn parse_inner<'a>(&self, input: &'a str) -> Option<&'a str> {
        let to_match = &input[0..self.0.len()];
        if to_match == self.0 {
            Some(to_match)
        } else {
            None
        }
    }
}

fn just(text: &'static str) -> impl Parser<str> {
    Just(text)
}

struct Ident;

impl Parser<str> for Ident {
    fn parse_inner<'a>(&self, input: &'a str) -> Option<&'a str> {
        let count = input
            .chars()
            .take_while(|c| c.is_ascii_alphabetic() || c == &'_')
            .count();
        if count > 0 {
            Some(&input[0..count])
        } else {
            None
        }
    }
}

fn ident() -> impl Parser<str> {
    Ident
}

struct Keyword(&'static str);

impl Parser<str> for Keyword {
    fn parse_inner<'a>(&self, input: &'a str) -> Option<&'a str> {
        let Some(ident) = Ident.parse_inner(input) else { return None };
        if ident == self.0 {
            Some(ident)
        } else {
            None
        }
    }
}

fn keyword(value: &'static str) -> impl Parser<str> {
    Keyword(value)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_just() {
        assert_eq!(just("foo").parse("foo"), Some("foo"));
        assert_eq!(just("foo").parse("foobar"), Some("foo"));
        assert_eq!(just("foo").parse("bar"), None);
        assert_eq!(just("!").parse("!"), Some("!"));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident().parse("foo"), Some("foo"));
        assert_eq!(ident().parse("foobar"), Some("foobar"));
        assert_eq!(ident().parse("bar"), Some("bar"));
        assert_eq!(ident().parse("!"), None);
    }

    #[test]
    fn test_keyword() {
        assert_eq!(keyword("foo").parse("foo"), Some("foo"));
        assert_eq!(keyword("foo").parse("foobar"), None);
        assert_eq!(keyword("foo").parse("bar"), None);
    }
}
