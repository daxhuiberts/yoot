use std::iter::Scan;

pub type Result<T> = std::result::Result<T, String>;

pub trait IterExt: Iterator {
    fn map_with_context<St, B, F>(
        self,
        init: St,
        f: F,
    ) -> Scan<Self, (F, St), fn(&mut (F, St), Self::Item) -> Option<B>>
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
        context: C,
        mut f: F,
    ) -> std::result::Result<B, E>
    where
        Self: Sized,
        F: FnMut(&mut C, Self::Item) -> std::result::Result<B, E>,
    {
        self.try_fold((init, context), |(_value, mut context), item| {
            let value = f(&mut context, item)?;
            Ok((value, context))
        })
        .map(|(value, _)| value)
    }
}

impl<T> IterExt for T where T: Iterator {}

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
