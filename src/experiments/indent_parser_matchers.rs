#![allow(dead_code)]

struct Parser {
    source: &'static str,
    current_slice: &'static str,
}

impl Parser {
    fn new(input: &'static str) -> Self {
        Self {
            source: input,
            current_slice: input,
        }
    }

    fn get_current(&self) -> &str {
        self.current_slice
    }

    fn advance(&mut self, len: usize) -> &str {
        let (split, rest) = self.current_slice.split_at(len);
        self.current_slice = rest;
        split
    }

    fn peek_char(&self) -> Option<char> {
        self.get_current().chars().next()
    }

    fn pop_char(&mut self) -> Option<char> {
        let char = self.get_current().chars().next()?;
        self.advance(char.len_utf8());
        Some(char)
    }

    fn try_match(&mut self, value: &str) -> Option<&str> {
        if self.get_current().starts_with(value) {
            Some(self.advance(value.len()))
        } else {
            None
        }
    }
}

fn parse(input: &'static str) {
    let mut parser = Parser::new(input);
    parser.try_match("foo");
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse("foo"), ());
    }
}
