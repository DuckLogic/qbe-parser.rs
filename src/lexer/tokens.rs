use crate::ast::{
    BlockName, GlobalName, Ident, NumericLiteral, Span, StringLiteral, TemporaryName, TypeName,
};
use ordered_float::OrderedFloat;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

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
            #[inline]
            pub fn text(self) -> &'static str {
                match self {
                    $(Self::$kw => $text),*
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
