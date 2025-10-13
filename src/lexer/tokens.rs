use std::fmt::{Display, Formatter};
use std::str::FromStr;
use std::sync::OnceLock;

use chumsky::prelude::*;
use chumsky::text::whitespace;
use ordered_float::OrderedFloat;

use crate::ast::{
    BlockName, GlobalName, Ident, NumericLiteral, Span, Spanned, StringLiteral, TemporaryName,
    TypeName,
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
    Float(NumericLiteral<OrderedFloat<f64>>),
    //
    // other
    //
    Keyword(Keyword, Span),
    ShortTypeSpec(ShortTypeSpec, Span),
    Operator(Operator, Span),
}

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
                $(($($inner)*) => ($target::$kw);)*
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
            pub(crate) fn parser<'a>() -> impl StringParser<'a, $target> {
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
        impl From<Spanned<$target>> for Token {
            fn from(value: Spanned<$target>) -> Self {
                Token::$target(value.value, value.span)
            }
        }
        impl From<$target> for Token {
            fn from(value: $target) -> Self {
                Token::$target(value, Span::MISSING)
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
        Type,
    }
);
define_string_enum!(
    enum ShortTypeSpec {
        // base types (BASETY)
        Word(w),
        Long(l),
        Short(s),
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
define_string_enum!(enum Operator {
    SingleEquals(=),
    Colon(:),
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
use crate::lexer::StringParser;
pub(crate) use keyword;
pub(crate) use operator;
pub(crate) use short_type_spec;

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn token_macros() {
        assert_eq!(operator!(=).text(), "=");
        assert_eq!(keyword!(align).text(), "align");
    }
}
