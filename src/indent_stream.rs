// use crate::util::*;

use std::collections::VecDeque;

pub const OPEN_BLOCK: char = '«';
pub const CLOSE_BLOCK: char = '»';

/// This method takes a str and produces another str with indents
/// replaced with OPEN_BLOCK and dedents replaced with CLOSE_BLOCK.
pub fn indent_stream(input: &str) -> impl Iterator<Item = char> + '_ {
    let mut chars = input.chars().peekable();
    let mut current_indent = 0;
    let mut queued = VecDeque::new();

    std::iter::from_fn(move || {
        if let Some(char) = queued.pop_front() {
            Some(char)
        } else if chars.next_if_eq(&'\n').is_some() {
            let new_indent = loop {
                let mut indent = 0;
                while chars.next_if_eq(&' ').is_some() {
                    indent += 1;
                }
                if chars.next_if_eq(&'\n').is_some() {
                    // if empty line, queue newline.
                    queued.push_back('\n')
                } else {
                    break indent;
                }
            };

            if new_indent % 2 == 1 {
                panic!("odd indent not allowed");
            } else if new_indent > current_indent + 2 {
                panic!("too deep indented");
            } else if new_indent == current_indent + 2 {
                // new indent, skip newline and start new block.
                current_indent = new_indent;
                queued.push_back(OPEN_BLOCK);
            } else if new_indent == current_indent {
                // same indent, queue newline.
                queued.push_back('\n');
            } else if new_indent < current_indent {
                // end indent, close blocks and queue newline.
                for _ in 0..((current_indent - new_indent) / 2) {
                    queued.push_front(CLOSE_BLOCK);
                }
                queued.push_back('\n');
                current_indent = new_indent;
            }

            queued.pop_front()
        } else {
            chars.next().or_else(|| {
                if current_indent > 0 {
                    current_indent -= 2;
                    Some(CLOSE_BLOCK)
                } else {
                    None
                }
            })
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_indent_stream() {
        fn check(input: &str) -> String {
            indent_stream(input).collect::<String>()
        }

        assert_eq!(check(""), "");
        assert_eq!(check("\n"), "\n");
        assert_eq!(check("\n\n"), "\n\n");
        assert_eq!(check("foo"), "foo");
        assert_eq!(check("foo\n"), "foo\n");
        assert_eq!(check("foo\n\n"), "foo\n\n");
        assert_eq!(check("foo\nbar"), "foo\nbar");
        assert_eq!(check("foo\nbar\n"), "foo\nbar\n");
        assert_eq!(check("foo\nbar\n\n"), "foo\nbar\n\n");
        assert_eq!(check("foo\n\nbar"), "foo\n\nbar");
        assert_eq!(check("foo\n\nbar\n"), "foo\n\nbar\n");
        assert_eq!(check("foo\n\nbar\n\n"), "foo\n\nbar\n\n");

        assert_eq!(check("foo\n  bar"), "foo«bar»");
        assert_eq!(check("foo\n  bar\n"), "foo«bar»\n");
        assert_eq!(check("foo\n  bar\n\n"), "foo«bar»\n\n");
        assert_eq!(check("foo\n\n  bar"), "foo\n«bar»");
        assert_eq!(check("foo\n\n  bar\n"), "foo\n«bar»\n");
        assert_eq!(check("foo\n\n  bar\n\n"), "foo\n«bar»\n\n");

        assert_eq!(check("foo\n  bar\n    baz"), "foo«bar«baz»»");
        assert_eq!(check("foo\n  bar\n    baz\n"), "foo«bar«baz»»\n");
        assert_eq!(check("foo\n  bar\n    baz\n  quux"), "foo«bar«baz»\nquux»");
        assert_eq!(
            check("foo\n  bar\n    baz\n  quux\n"),
            "foo«bar«baz»\nquux»\n"
        );

        assert_eq!(check("foo\n  bar\n  baz"), "foo«bar\nbaz»");
        assert_eq!(check("foo\n  bar\n\n  baz"), "foo«bar\n\nbaz»");

        assert_eq!(check("foo\n  bar\nbaz"), "foo«bar»\nbaz");
        assert_eq!(check("foo\n  bar\n\nbaz"), "foo«bar»\n\nbaz");
    }
}
