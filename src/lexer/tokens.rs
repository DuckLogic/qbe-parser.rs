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
    //
    // magic
    //
    /// Represents a series of one or more newlines.
    ///
    /// The QBE IR uses newlines to separate instructions,
    /// and restricts the usage of newlines in other locations.
    Newline,
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
        simple { $($simple_variant:ident  => $value:literal),+ $(,)? },
        magic { $($magic_variant:ident => $magic_text:literal as $desc:literal),+ $(,)? } $(,)?
    )
    => {
        impl Token {
            pub fn span(&self) -> Option<Span> {
                #[deny(unreachable_patterns)]
                match self {
                    $(Self::$complex_variant(inner) => Some(inner.span()),)*
                    $(Self::$wrap_variant(_) => None,)*
                    $(Self::$simple_variant => None,)*
                    $(Self::$magic_variant => None,)*
                }
            }
            /// If the token is "magic" and has special behavior.
            #[inline]
            pub fn is_magic(&self) -> bool {
                match self {
                    $(Self::$magic_variant => true,)*
                    _ => false,
                }
            }
            /// The text of the token.
            ///
            /// This will differ from the [Display] impl only if the token is [magic](Self::is_magic).
            #[inline]
            pub fn text(&self) -> impl Display + '_ {
                struct Text<'a>(&'a Token);
                impl Display for Text<'_> {
                    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                        match self.0 {
                            $(Token::$magic_variant => f.write_str($magic_text),)+
                            _ => {
                                assert!(!self.0.is_magic());
                                write!(f, "{}", self.0)
                            }
                        }
                    }
                }
                Text(self)
            }
        }
        /// Display a human-readable description of the token.
        ///
        /// This is either the text of the token or `<desc>` if the token is magic.
        /// Use [`Token::text`] if you want the actual text of the token.
        impl Display for Token {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$complex_variant(inner) => Display::fmt(inner, f),)*
                    $(Self::$wrap_variant(inner) => Display::fmt(inner, f),)+
                    $(Self::$simple_variant => Display::fmt(&$value, f),)+
                    $(Self::$magic_variant => f.write_str($desc),)+
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
    },
    magic {
        Newline => "\n" as "<newline>",
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
    ($(#[$emeta:meta])* enum $target:ident {
        $($kw:ident ( $($inner:tt)* )),+ $(,)?
    }) => {
        paste3::paste! {
            macro_rules! [<$target:snake>] {
                $(($($inner)*) => ($crate::lexer::$target::$kw);)*
            }
        }
        define_string_enum!(@nomacro $(#[$emeta])* enum $target {
            $($kw => stringify!($($inner)*)),*
        });
    };
    (@nomacro $(#[$emeta:meta])* enum $target:ident {
        // TODO: The `=>` should be neither `+`, nor `?`, but exactly once
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
define_string_enum!(
    /// Defines operators, the third main class of tokens besides [`Keyword`] and [`ShortTypeSpec`].
    ///
    /// # Symbols
    /// Most operators are "symbols", which have two special behaviors when lexing.
    ///
    /// First, if exactly one of two consecutive tokens is a symbol,
    /// then the spacing between the tokens can be omitted.
    /// This means that `type=`, `=w`, `foo,` will lex as `type =`, `= w`, `foo ,`.
    /// This behavior is by design and is described in the [spacing] section of the QBE reference.
    ///
    /// The only non-symbol that this rule clearly creates is ['z'](Operator::ZeroInitMarker),
    /// as we don't want `zero` to parse as `z ero`.
    ///
    /// The second property is that symbols have higher lexer priority than all other tokens.
    /// This is an implementation detail that would require a fair deal of work to resolve.
    /// It means that `:` cannot be a symbol, as it could be confused with a type name.
    ///
    /// [spacing]: https://c9x.me/compile/doc/il.html#Spacing
    enum Operator {
        SingleEquals(=),
        Colon(:),
        Comma(,),
        // The plus operator is not considered a symbol as it could be confused with numbers.
        // This limitation would be straightforward to remove by special-casing digits.
        Plus(+),
        // TODO: Should this be an `Ident`?
        ZeroInitMarker(z),
        Ellipsis(...),
    }
);
impl Operator {
    /// Determine if this operator is a symbol for the purposes of lexing.
    #[inline]
    pub fn is_symbol(&self) -> bool {
        !matches!(
            self,
            Operator::ZeroInitMarker | Operator::Colon | Operator::Plus
        )
    }
}
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
    use crate::lexer::Token;

    #[test]
    fn token_macros() {
        assert_eq!(operator!(=).text(), "=");
        assert_eq!(keyword!(align).text(), "align");
    }

    #[test]
    fn display() {
        assert_eq!(operator!(=).to_token().to_string(), "=");
        assert_eq!(operator!(,).to_token().to_string(), ",");
        assert_eq!(short_type_spec!(w).to_token().to_string(), "w");
        assert_eq!(Token::Newline.to_string(), "<newline>");
    }
}
