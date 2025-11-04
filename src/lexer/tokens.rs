use std::fmt::{Display, Formatter};
use std::str::FromStr;
use std::sync::OnceLock;

use chumsky::prelude::*;

use crate::ast::{
    BlockName, FloatLiteral, GlobalName, Ident, NumericLiteral, Span, Spanned, StringLiteral,
    TemporaryName, TypeName,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Token {
    //
    // identifiers
    //
    Ident(Ident),
    TypeName(TypeName),
    GlobalName(GlobalName),
    TemporaryName(TemporaryName),
    BlockName(BlockName),
    //
    // literals
    //
    StringLiteral(StringLiteral),
    Number(NumericLiteral<u64>),
    Integer(NumericLiteral<i128>),
    Float(FloatLiteral),
    //
    // other
    //
    Keyword(Keyword),
    ShortTypeSpec(ShortTypeSpec),
    Operator(Operator),
    // delimiters
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
}
impl Token {
    #[inline]
    pub(crate) fn number<'a>() -> impl TokenParser<'a, NumericLiteral<u64>> {
        select!(Token::Number(num) => num).labelled("number (unsigned)")
    }
}
macro_rules! token_impls {
    (
        complex { $($complex_variant:ident),+ $(,)? },
        wraps_enum { $($wrap_variant:ident),+ $(,)? },
        simple { $($simple_variant:ident  => $delim:literal),+ $(,)? } $(,)?
    ) => {
        impl Token {
            pub fn span(&self) -> Option<Span> {
                #[deny(unreachable_patterns)]
                match self {
                    $(Self::$complex_variant(inner) => Some(inner.span()),)*
                    $(Self::$wrap_variant(_) => None,)*
                    $(Self::$simple_variant => None,)*
                }
            }
        }
        impl Display for Token {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$complex_variant(inner) => Display::fmt(inner, f),)*
                    $(Self::$wrap_variant(inner) => Display::fmt(inner, f),)+
                    $(Self::$simple_variant => Display::fmt(&$delim, f),)+
                }
            }
        }
    }
}
token_impls! {
    complex {
        Ident,
        TypeName,
        GlobalName,
        TemporaryName,
        BlockName,
        StringLiteral,
        Number,
        Integer,
        Float,
    },
    wraps_enum {
        Keyword,
        Operator,
        ShortTypeSpec,
    },
    simple {
        OpenBrace => '{',
        CloseBrace => '}',
        OpenParen => '(',
        CloseParen => ')',
    }
}
macro_rules! token_wrapper_from {
    ($($variant:ident),+ $(,)?) => {
        $(impl From<$variant> for Token {
            #[inline]
            fn from(value: $variant) -> Self {
                Token::$variant(value)
            }
        }
        impl From<$variant> for Spanned<Token> {
            #[inline]
            fn from(value: $variant) -> Self {
                let span = value.span();
                Spanned {
                    value: Token::$variant(value),
                    span,
                }
            }
        })*
    };
}
token_wrapper_from! {
    Ident,
    TypeName,
    GlobalName,
    TemporaryName,
    BlockName,
    StringLiteral,
}
impl From<FloatLiteral> for Token {
    fn from(value: FloatLiteral) -> Self {
        Token::Float(value)
    }
}
impl From<FloatLiteral> for Spanned<Token> {
    fn from(value: FloatLiteral) -> Self {
        Spanned {
            span: value.span(),
            value: Token::Float(value),
        }
    }
}
#[allow(unused)]
const _TOKEN_USED: () = {
    let _ = Token::span;
};

macro_rules! define_keyword_enum {
    (enum $target:ident {
        $($kw:ident),+ $(,)?
    }) => {
        paste3::paste! {
            define_string_enum!(enum $target {
                $($kw([<$kw:snake>])),*
            });
        }
    };
}
macro_rules! define_string_enum {
    (enum $target:ident {
        $($kw:ident ( $($inner:tt)* )),+ $(,)?
    }) => {
        paste3::paste! {
            macro_rules! [<$target:snake>] {
                $(($($inner)*) => ($crate::lexer::$target::$kw);)*
            }
        }
        define_string_enum!(@nomacro enum $target {
            $($kw => stringify!($($inner)*)),*
        });
    };
    (@nomacro enum $target:ident {
        // TODO: The `=>` should be niehter `+`, nor `?`, but exactly once
        $($kw:ident => $text:expr),+ $(,)?
    }) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub enum $target {
            $($kw),*
        }
        impl $target {
            pub const ALL: [Self; Self::COUNT] = [$(Self::$kw),*];
            pub const COUNT: usize = define_string_enum!(@count $($kw),*);
            #[inline]
            pub fn text(self) -> &'static str {
                match self {
                    $(Self::$kw => $text),*
                }
            }
            #[inline]
            pub fn to_token(self) -> Token {
                Token::$target(self)
            }
            /// Parses this token, returning the [`Span`] of the value.
            ///
            /// Equivalent to calling [`just`] with [`Self::to_token`].
            /// This gives superior error messages to using [`select!`].
            #[inline]
            pub(crate) fn parser<'a>(self) -> impl TokenParser<'a, Span> {
                just(self.to_token()).to_span()
            }
            pub(super) fn text_parser<'a>() -> impl StringParser<'a, $target> {
                // TODO: Would be nice to cache the result directly,
                // but that module is currently unstable
                const COUNT: usize = define_string_enum!(@count $($kw),*);
                static SORTED_TOKENS: OnceLock<[$target; COUNT]> = OnceLock::new();
                let sorted_tokens: [$target; COUNT] = *SORTED_TOKENS.get_or_init(|| {
                    let mut tokens: [$target; Self::COUNT] = Self::ALL.clone();
                    tokens.sort_by_key(|tk| {
                        let text = tk.text();
                        // long tokens must always come before short tokens,
                        // then sort alphabetically
                        (text.len(), text)
                    });
                    tokens
                });
                let parsers = sorted_tokens.map(|token| just(token.text()).to(token));
                choice(parsers)
            }
        }
        impl From<$target> for Token {
            #[inline]
            fn from(value: $target) -> Self {
                value.to_token()
            }
        }
        impl From<$target> for Spanned<Token> {
            #[inline]
            fn from(value: $target) -> Self {
                Spanned {
                    value: value.to_token(),
                    span: Span::MISSING,
                }
            }
        }
        impl FromStr for $target {
            type Err = paste3::paste!([<Invalid $target Error>]);
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($text => Ok(Self::$kw),)*
                    _ => Err(paste3::paste!([<Invalid $target Error>])),
                }
            }
        }
        impl Display for $target {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.write_str(self.text())
            }
        }
    };
    (@text $kw:ident) => (paste3::paste!(stringify!([<$kw:lower>])));
    (@text $kw:ident => $text:expr) => ($text);
    (@text $kw:ident( $($inner:tt)* )) => (stringify!($($inner)*));
    (@count) => (0);
    (@count $kw:ident) => (1);
    (@count $first:ident , $($kw:ident),*) => (1 + define_string_enum!(@count $($kw),*));
}
define_keyword_enum!(
    enum Keyword {
        Align,
        Data,
        Export,
        Function,
        Section,
        Thread,
        Type,
    }
);
define_string_enum!(
    enum ShortTypeSpec {
        // base types (BASETY)
        Word(w),
        Long(l),
        Single(s),
        Double(d),
        // extended types (EXTTY)
        Byte(b),
        Half(h),
        // Sub-word types (SUBWTY)
        SignedByte(sb),
        UnsignedByte(ub),
        SignedHalf(sh),
        UnsignedHalf(uh),
    }
);
#[allow(unused)]
const _SHORT_TYPE_SPEC_USED: () = {
    let _ = ShortTypeSpec::parser;
    let _ = short_type_spec!(w);
};
define_string_enum!(enum Operator {
    SingleEquals(=),
    Colon(:),
    Comma(,),
    Plus(+),
    ZeroInitMarker(z),
    Ellipsis(...),
});
#[derive(thiserror::Error, Debug, Copy, Clone)]
#[error("Keyword is not valid")]
pub struct InvalidKeywordError;

#[derive(thiserror::Error, Debug, Copy, Clone)]
#[error("Short type spec is not valid")]
pub struct InvalidShortTypeSpecError;

#[derive(thiserror::Error, Debug, Copy, Clone)]
#[error("Operator is not valid")]
pub struct InvalidOperatorError;

// re-export macros (this works?)
use crate::lexer::{StringParser, TokenParser};
pub(crate) use keyword;
pub(crate) use operator;
#[allow(unused_imports)]
pub(crate) use short_type_spec;

#[cfg(test)]
mod test {
    #[test]
    fn token_macros() {
        assert_eq!(operator!(=).text(), "=");
        assert_eq!(keyword!(align).text(), "align");
    }
}
