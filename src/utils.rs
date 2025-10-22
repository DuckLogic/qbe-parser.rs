use std::cell::Cell;
use std::fmt::Display;

macro_rules! delegate_enum_getters {
    (enum $target:ident {
        $($variant:ident),+ $(,)?
    } get {}) => {};
    (enum $target:ident {
        $($variant:ident),+ $(,)?
    } get { $v:vis fn $field:ident(&self) -> $field_type:ty; $($rem:tt)* }) => {
        impl $target {
            $v fn $field(&self) -> $field_type {
                match self {
                    $(Self::$variant(inner) => inner.$field(),)*
                }
            }
        }
        // remaining elements
        delegate_enum_getters!(enum $target { $($variant),* } get { $($rem)* });
    };
}
macro_rules! impl_enum_display {
    (enum $target:ident {
        $($variant:ident),+ $(,)?
    }) => {
        impl std::fmt::Display for $target {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(Self::$variant(inner) => std::fmt::Display::fmt(inner, f),)*
                }
            }
        }
    };
}

pub trait IterExt: Iterator {
    /// Emulate [`Itertools::format`]
    ///
    /// [`Itertools::format`]: https://docs.rs/itertools/0.14.0/itertools/trait.Itertools.html#method.format
    #[inline]
    fn format(self, sep: &str) -> Format<'_, Self>
    where
        Self: Sized,
        Self::Item: Display,
    {
        Format {
            iter: Cell::new(Some(self)),
            sep,
        }
    }
}
impl<I: Iterator> IterExt for I {}

pub struct Format<'a, I: Iterator> {
    iter: Cell<Option<I>>,
    sep: &'a str,
}
impl<I: Iterator> Display for Format<'_, I>
where
    I::Item: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let iter = self.iter.take().expect("already Displayed once");
        for (index, item) in iter.enumerate() {
            if index > 0 {
                f.write_str(self.sep)?;
            }
            Display::fmt(&item, f)?;
        }
        Ok(())
    }
}

pub(crate) use {delegate_enum_getters, impl_enum_display};
