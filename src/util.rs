pub type Result<T> = std::result::Result<T, String>;

#[cfg(test)]
pub mod macros {
    macro_rules! pubmacro {
        ($name:ident, $($tt:tt)*) => {
            macro_rules! $name { $($tt)* }

            pub(crate) use $name;
        }
    }

    pub(crate) use pubmacro;

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
