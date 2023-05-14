use std::iter::Scan;

pub type Result<T> = std::result::Result<T, String>;

type MapWithContext<I, St, B, F> =
    Scan<I, (F, St), fn(&mut (F, St), <I as Iterator>::Item) -> Option<B>>;

pub trait IterExt: Iterator {
    fn map_with_context<St, B, F>(self, init: St, f: F) -> MapWithContext<Self, St, B, F>
    where
        Self: Sized,
        F: FnMut(&mut St, Self::Item) -> B,
    {
        fn map_with_context_closure<St, F, I, B>(f_st: &mut (F, St), item: I) -> Option<B>
        where
            F: FnMut(&mut St, I) -> B,
        {
            Some((f_st.0)(&mut f_st.1, item))
        }

        self.scan((f, init), map_with_context_closure)
    }

    fn try_fold_with_context<B, E, F, C>(
        &mut self,
        init: B,
        mut context: C,
        mut f: F,
    ) -> std::result::Result<B, E>
    where
        Self: Sized,
        F: FnMut(&mut C, Self::Item) -> std::result::Result<B, E>,
    {
        self.try_fold(init, move |_value, item| {
            let value = f(&mut context, item)?;
            Ok(value)
        })
    }

    fn tuple_merger<F>(self, f: F) -> TupleMerger<Self, Self::Item, F>
    where
        Self: Sized,
        F: Fn(&Self::Item, &Self::Item) -> Option<Self::Item>,
    {
        TupleMerger {
            iterator: self,
            prev: None,
            f,
        }
    }
}

pub struct TupleMerger<I: Iterator<Item = T>, T, F: Fn(&T, &T) -> Option<T>> {
    iterator: I,
    prev: Option<T>,
    f: F,
}

impl<I: Iterator<Item = T>, T, F: Fn(&T, &T) -> Option<T>> Iterator for TupleMerger<I, T, F> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let prev = self.prev.take().or_else(|| self.iterator.next())?;
        let Some(next) = self.iterator.next() else { return Some(prev) };

        if let Some(merged) = (self.f)(&prev, &next) {
            Some(merged)
        } else {
            self.prev = Some(next);
            Some(prev)
        }
    }
}

impl<T> IterExt for T where T: Iterator {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tuple_merger() {
        pub fn tuple_merger<T, F>(input: Vec<T>, f: F) -> Vec<T>
        where
            F: Fn(&T, &T) -> Option<T>,
        {
            input.into_iter().tuple_merger(f).collect()
        }

        assert_eq!(tuple_merger::<i32, _>(vec![], |_, _| None), vec![]);
        assert_eq!(tuple_merger::<i32, _>(vec![1], |_, _| None), vec![1]);
        assert_eq!(tuple_merger::<i32, _>(vec![1, 2], |_, _| None), vec![1, 2]);
        assert_eq!(
            tuple_merger::<i32, _>(vec![1, 2, 3], |_, _| None),
            vec![1, 2, 3]
        );
        assert_eq!(
            tuple_merger::<i32, _>(vec![1, 2, 3, 4], |_, _| None),
            vec![1, 2, 3, 4]
        );

        assert_eq!(tuple_merger::<i32, _>(vec![], |_, _| Some(9)), vec![]);
        assert_eq!(tuple_merger::<i32, _>(vec![1], |_, _| Some(9)), vec![1]);
        assert_eq!(tuple_merger::<i32, _>(vec![1, 2], |_, _| Some(9)), vec![9]);
        assert_eq!(
            tuple_merger::<i32, _>(vec![1, 2, 3], |_, _| Some(9)),
            vec![9, 3]
        );
        assert_eq!(
            tuple_merger::<i32, _>(vec![1, 2, 3, 4], |_, _| Some(9)),
            vec![9, 9]
        );
    }
}

#[cfg(test)]
pub mod macros {
    macro_rules! pubmacro {
        ($name:ident, $($tt:tt)*) => {
            macro_rules! $name { $($tt)* }

            pub(crate) use $name;
        }
    }

    pub(crate) use pubmacro;

    pubmacro! { assert_ok,
        ($test:expr, $expected:expr) => {
            assert_eq!($test, Ok($expected));
        }
    }

    pubmacro! { assert_err,
        ($test:expr) => {
            assert!($test.is_err());
        }
    }

    pubmacro! { map,
        () => {
            std::collections::HashMap::new()
        };
        ( $( $key:expr => $val:expr ),+ ) => {
            {
                let mut temp_map = std::collections::HashMap::new();
                $(
                    temp_map.insert($key, $val);
                )*
                temp_map
            }
        };
    }

    pubmacro! { eenv,
        ( $( $key:ident => $val:expr ),* ) => {
            crate::util::macros::map!($(stringify!($key).to_string() => $val),*)
        };
    }
}
