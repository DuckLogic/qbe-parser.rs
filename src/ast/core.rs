//! The core AST types.

pub use crate::ast::Span;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::hash::Hash;

#[derive(Copy, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Spanned").field(&self.value).finish()
    }
}
impl<T: PartialOrd> PartialOrd for Spanned<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl<T: PartialEq> PartialEq for Spanned<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Eq> Eq for Spanned<T> {}
impl<T> From<T> for Spanned<T> {
    fn from(value: T) -> Self {
        Spanned {
            value,
            span: Span::MISSING,
        }
    }
}
impl<T> From<(T, Span)> for Spanned<T> {
    fn from(value: (T, Span)) -> Self {
        Spanned {
            value: value.0,
            span: value.1,
        }
    }
}

/// Implements a wrapper around a [`String`] like [`Ident`] or [`StringLiteral`].
///
/// Does not implement [`Display`].
macro_rules! impl_string_wrapper {
    ($target:ident) => {
        impl $target {
            #[inline]
            pub fn new(text: impl Into<String>, span: Span) -> $target {
                $target {
                    text: text.into(),
                    span,
                }
            }
            #[inline]
            pub fn text(&self) -> &'_ str {
                &self.text
            }
            #[inline]
            pub fn span(&self) -> Span {
                self.span
            }
        }
        impl From<String> for $target {
            fn from(value: String) -> Self {
                $target::new(value, Span::MISSING)
            }
        }
        impl From<(String, Span)> for $target {
            #[inline]
            fn from(value: (String, Span)) -> Self {
                $target::new(value.0, value.1)
            }
        }
        impl<T: Into<String>> From<Spanned<T>> for $target {
            fn from(value: Spanned<T>) -> Self {
                $target::new(value.value, value.span)
            }
        }
        impl Debug for $target {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($target))
                    .field(&self.text)
                    .finish()
            }
        }
        impl AsRef<str> for $target {
            #[inline]
            fn as_ref(&self) -> &str {
                &self.text
            }
        }
        impl Borrow<str> for $target {
            #[inline]
            fn borrow(&self) -> &str {
                &self.text
            }
        }
        impl equivalent::Equivalent<String> for $target {
            fn equivalent(&self, other: &String) -> bool {
                self.text.equivalent(other)
            }
        }
        impl equivalent::Comparable<String> for $target {
            fn compare(&self, key: &String) -> Ordering {
                self.text.cmp(key)
            }
        }
    };
}
/// An identifier in the source code.
#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Ident {
    text: String,
    span: Span,
}
impl_string_wrapper!(Ident);
impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.text)
    }
}

/// A quoted string literal.
#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct StringLiteral {
    text: String,
    span: Span,
}
impl_string_wrapper!(StringLiteral);
impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        for c in self.text.chars().flat_map(char::escape_default) {
            f.write_char(c)?;
        }
        f.write_char('"')
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
pub struct NumericLiteral<T: Number> {
    pub value: T,
    pub span: Span,
}

/// A type that can be used in a [`NumericLiteral`].
pub trait Number: Debug + Display + num_traits::Num + Clone {}
macro_rules! impl_numtype {
    ($($target:ty),*) => {
        $(impl Number for $target {})*
    };
}
impl_numtype!(
    u32,
    u64,
    usize,
    i32,
    i64,
    isize,
    f64,
    ordered_float::OrderedFloat<f64>
);
