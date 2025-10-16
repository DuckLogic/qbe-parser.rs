//! The core AST types.

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::hash::Hash;
use std::sync::OnceLock;
use unicode_ident::{is_xid_continue, is_xid_start};

pub use crate::ast::Span;

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

/// Implements an opaque wrapper around a [`String`] like [`Ident`] or [`StringLiteral`].
///
/// Does not implement [`Display`].
macro_rules! opaque_string_wrapper {
    ($target:ident) => {
        impl $target {
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
        impl_string_like!($target);
    };
}
/// An identifier in the source code.
#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Ident {
    text: String,
    span: Span,
}
impl Ident {
    #[inline]
    #[track_caller]
    pub fn new(text: impl Into<String>, span: Span) -> Self {
        let text = text.into();
        let invalid_char = |c: char| panic!("Invalid char {c:?} at {span:?}");
        let mut chars = text.chars();
        let first = chars
            .next()
            .unwrap_or_else(|| panic!("Identifier is empty at {span:?}"));
        if !is_xid_start(first) {
            invalid_char(first)
        }
        for other in chars {
            if !is_xid_continue(other) {
                invalid_char(other)
            }
        }
        if let Ok(byte_len) = span.byte_len() {
            assert_eq!(
                byte_len,
                text.len() as u64,
                "Length of span {span} doesn't match {text:?}"
            )
        }
        Ident { text, span }
    }
}
opaque_string_wrapper!(Ident);
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
impl StringLiteral {
    #[inline]
    pub fn new(text: impl Into<String>, span: Span) -> Self {
        StringLiteral {
            text: text.into(),
            span,
        }
    }
}
opaque_string_wrapper!(StringLiteral);
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
    ($($target:ty),+ $(,)?) => {
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
    ordered_float::OrderedFloat<f64>,
    u128,
    i128,
);

macro_rules! prefixed_ident_type {
    ($target:ident, $prefix:literal) => {
        #[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
        pub struct $target {
            ident: Ident,
            span: Span,
        }
        impl $target {
            pub const PREFIX: char = $prefix;
            /// The label used for describing this value.
            pub(crate) fn label() -> &'static str {
                static LABEL: OnceLock<Box<str>> = OnceLock::new();
                &*LABEL.get_or_init(|| {
                    let snake_name: &str = paste3::paste!(stringify!());
                    snake_name.replace('_', " ").into_boxed_str()
                })
            }
            pub fn without_span(text: &str) -> Self {
                Self {
                    ident: Ident::new(text, Span::MISSING),
                    span: Span::MISSING,
                }
            }
            #[inline]
            pub fn text(&self) -> &'_ str {
                self.ident.text()
            }
            #[inline]
            pub fn ident(&self) -> &'_ Ident {
                &self.ident
            }
            #[inline]
            pub fn span(&self) -> Span {
                self.span
            }
            #[inline]
            #[track_caller]
            pub fn new(ident: Ident, span: Span) -> Self {
                let res = Self { ident, span };
                assert_eq!(res.ident.span().is_missing(), span.is_missing());
                if !span.is_missing() {
                    assert_eq!(
                        res.ident.span().byte_range().unwrap(),
                        span.slice_byte_indexes(1..).byte_range().unwrap(),
                        "Span for {ident:?} doesn't correspond to {res:?}",
                        ident = res.ident
                    );
                }
                res
            }
        }
        impl_ident_like!($target);
        impl Display for $target {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.write_char(Self::PREFIX)?;
                f.write_str(self.text())
            }
        }
    };
}
prefixed_ident_type!(TypeName, ':');
prefixed_ident_type!(GlobalName, '$');
prefixed_ident_type!(TemporaryName, '%');
prefixed_ident_type!(BlockName, '@');
