#![allow(dead_code)]

use std::fmt::Debug;
use std::str::Lines;
// use std::iter::FilterMap;

#[derive(Clone, Debug)]
struct BlockIterator {
    lines: Lines<'static>,
    indent: usize,
}

impl BlockIterator {
    fn new(input: &'static str) -> Self {
        Self {
            lines: input.lines(),
            indent: 0,
        }
    }
}

impl Iterator for BlockIterator {
    type Item = (&'static str, BlockIterator);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next_line = self.lines.next()?;
            if next_line.starts_with(&" ".repeat(self.indent + 2)) {
                continue;
            }
            if next_line.starts_with(&" ".repeat(self.indent)) {
                return Some((
                    &next_line[self.indent..],
                    BlockIterator {
                        lines: self.lines.clone(),
                        indent: self.indent + 2,
                    },
                ));
            } else {
                return None;
            }
        }
    }
}

fn process(input: &'static str) -> String {
    fn inner(iter: &mut BlockIterator) -> String {
        iter.map(|(line, mut sub)| {
            let subs = inner(&mut sub);
            if subs.is_empty() {
                format!("{}", line)
            } else {
                format!("{}({})", line, subs)
            }
        })
        .collect::<Vec<_>>()
        .join(",")
    }

    inner(&mut BlockIterator::new(input))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_process() {
        assert_eq!(process("foo"), "foo");
        assert_eq!(process("foo\nbar"), "foo,bar");
        assert_eq!(process("foo\n  bar"), "foo(bar)");
        assert_eq!(process("foo\n  bar\n    baz"), "foo(bar(baz))");
        assert_eq!(
            process("foo\n  bar\n    baz\n  quux\nyoot"),
            "foo(bar(baz),quux),yoot"
        );
    }
}
