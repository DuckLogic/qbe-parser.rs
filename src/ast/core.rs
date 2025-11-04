//! The core AST types.

use chumsky::Parser;
use ordered_float::OrderedFloat;
use std::borrow::Borrow;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::OnceLock;
use unicode_ident::{is_xid_continue, is_xid_start};

pub use crate::ast::Span;
use crate::lexer::{Token, TokenParser};
use crate::parse::{Parse, impl_fromstr_via_parse};

// NOTE: Derive macros for Ord, Eq, Hash ignore Span, only consider value
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
impl<T> Spanned<T> {
    #[inline]
    pub fn map<U>(this: Self, func: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: func(this.value),
            span: this.span,
        }
    }
}
impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Spanned")
            .field(&self.value)
            .field(&self.span)
            .finish()
    }
}
impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}
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
impl<T> Deref for Spanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
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
        impl From<&str> for $target {
            fn from(value: &str) -> Self {
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
    /// Get a string representation of this [`Ident`],
    /// equivalent to the [`Display`] impl.
    ///
    /// Not present on prefixed idents like [`TemporaryName`],
    /// as those also have a prefix.
    #[inline]
    pub fn as_str(&self) -> &'_ str {
        &self.text
    }
}
opaque_string_wrapper!(Ident);
impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.text)
    }
}
impl Parse for Ident {
    const DESC: &'static str = "identifier";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        chumsky::select!(Token::Ident(ref name) => name.clone()).labelled(Self::DESC)
    }
}

/// A quoted string literal.
#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct StringLiteral {
    text: String,
    span: Span,
}
impl StringLiteral {
    pub fn unspanned(text: impl Into<String>) -> Self {
        StringLiteral::new(text, Span::MISSING)
    }
    #[inline]
    pub fn new(text: impl Into<String>, span: Span) -> Self {
        StringLiteral {
            text: text.into(),
            span,
        }
    }
}
opaque_string_wrapper!(StringLiteral);
impl Parse for StringLiteral {
    const DESC: &'static str = "string literal";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        chumsky::select!(Token::StringLiteral(str) => str).labelled(Self::DESC)
    }
}
impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        for c in self.text.chars().flat_map(char::escape_default) {
            f.write_char(c)?;
        }
        f.write_char('"')
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
pub struct NumericLiteral<T: Number> {
    pub value: T,
    pub span: Span,
}
impl<T: Number> NumericLiteral<T> {
    #[inline]
    pub fn unspanned(value: T) -> Self {
        NumericLiteral {
            value,
            span: Span::MISSING,
        }
    }
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }
    #[inline]
    pub fn map_value<U: Number>(self, func: impl FnOnce(T) -> U) -> NumericLiteral<U> {
        NumericLiteral {
            value: func(self.value),
            span: self.span,
        }
    }
}
impl<T: Number> Display for NumericLiteral<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}
impl<T: Number> From<T> for NumericLiteral<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::unspanned(value)
    }
}
impl From<f64> for NumericLiteral<OrderedFloat<f64>> {
    #[inline]
    fn from(value: f64) -> Self {
        Self::unspanned(value.into())
    }
}
impl From<f32> for NumericLiteral<OrderedFloat<f32>> {
    #[inline]
    fn from(value: f32) -> Self {
        Self::unspanned(value.into())
    }
}

/// A prefix for a [`FloatLiteral`],
/// which determines the size of the float.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum FloatPrefix {
    SinglePrecision,
    DoublePrecision,
}
impl FloatPrefix {
    pub fn text(&self) -> &'static str {
        match self {
            FloatPrefix::SinglePrecision => "s_",
            FloatPrefix::DoublePrecision => "d_",
        }
    }
}
impl Display for FloatPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.text())
    }
}
type FloatValue = NumericLiteral<OrderedFloat<f64>>;
#[derive(Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
pub struct FloatLiteral {
    pub span: Span,
    pub prefix: Spanned<FloatPrefix>,
    pub value: FloatValue,
}
impl FloatLiteral {
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }
    /// Create a single-precision float literal without any span information.
    pub fn single_unspanned(value: impl Into<FloatValue>) -> Self {
        FloatLiteral {
            value: value.into(),
            span: Span::MISSING,
            prefix: Spanned::from(FloatPrefix::SinglePrecision),
        }
    }
    /// Create a double-precision float literal without any span information.
    pub fn double_unspanned(value: impl Into<FloatValue>) -> Self {
        FloatLiteral {
            value: value.into(),
            span: Span::MISSING,
            prefix: Spanned::from(FloatPrefix::DoublePrecision),
        }
    }
}
impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.prefix, self.value)
    }
}
impl Parse for FloatLiteral {
    const DESC: &'static str = "float literal";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        chumsky::select!(Token::Float(literal) => literal).labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(FloatLiteral);

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
    ordered_float::OrderedFloat<f32>,
    ordered_float::OrderedFloat<f64>,
    ordered_float::NotNan<f64>,
    u128,
    i128,
);

macro_rules! prefixed_ident_type {
    ($target:ident, PREFIX = $prefix:literal, DESC = $desc:literal) => {
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
            pub fn unspanned(text: &str) -> Self {
                Self::without_span(text)
            }
            // TODO: Deprecate in favor of unspanned
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
        impl Parse for $target {
            const DESC: &'static str = $desc;
            fn parser<'a>() -> impl TokenParser<'a, Self> {
                use chumsky::Parser;
                chumsky::select!(Token::$target(val) => val).labelled(Self::DESC)
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
prefixed_ident_type!(TypeName, PREFIX = ':', DESC = "type name");
prefixed_ident_type!(GlobalName, PREFIX = '$', DESC = "global name");
prefixed_ident_type!(TemporaryName, PREFIX = '%', DESC = "temporary name");
prefixed_ident_type!(BlockName, PREFIX = '@', DESC = "block name");
