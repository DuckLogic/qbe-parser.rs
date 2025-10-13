use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

use crate::ast::{NumericLiteral, Span};
use crate::lexer::ShortTypeSpec;
use crate::lexer::tokens::InvalidShortTypeSpecError;

/// Specifies the alignment of a type.
#[derive(Clone)]
pub struct AlignSpec {
    pub span: Span,
    pub value: NumericLiteral<u64>,
}

macro_rules! simple_type {
    (
        $(#[$tp_attr:meta])*
        $v:vis enum $target:ident {
            $($variant:ident),+ $(,)?
        }
    ) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        $(#[$tp_attr])*
        $v enum $target {
            $($variant),*
        }
        impl $target {
            pub fn to_short_spec(&self) -> ShortTypeSpec {
                match self {
                    $(Self::$variant => ShortTypeSpec::$variant,)*
                }
            }
            fn type_desc() -> &'static str {
                let snake = paste3::paste!(stringify!([<$target:snake>]));
                snake.strip_suffix("_type").expect(snake)
            }
        }
        impl Display for $target {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                Display::fmt(&self.to_short_spec(), f)
            }
        }
        paste3::paste! {
            #[derive(Debug, thiserror::Error, Copy, Clone)]
            #[error("Expected a {} type, but got \"{spec}\"", $target::type_desc())]
            pub struct [<Invalid $target Error>] {
                spec: ShortTypeSpec
            }
            impl TryFrom<ShortTypeSpec> for $target {
                type Error = [<Invalid $target:camel Error>];
                fn try_from(spec: ShortTypeSpec) -> Result<Self, Self::Error> {
                    match spec {
                        $(ShortTypeSpec::$variant => Ok(Self::$variant),)*
                        _ => return Err(Self::Error { spec })
                    }
                }
            }
            #[derive(Debug, thiserror::Error, Clone)]
            #[error("Expected a {} type, but got {text:?}", $target::type_desc())]
            pub struct [<$target ParseError>] {
                text: String,
            }
            impl From<[<Invalid $target Error>]> for [<$target ParseError>] {
                #[cold]
                fn from(cause: [<Invalid $target Error>]) -> Self {
                    Self { text: cause.spec.text().into() }
                }
            }
            impl FromStr for $target {
                type Err = [<$target ParseError>];
                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    Ok(ShortTypeSpec::from_str(s)
                        .map_err(#[cold] |InvalidShortTypeSpecError| Self::Err { text: s.into() })?
                        .try_into()?)
                }
            }
        }
        #[cfg(test)]
        paste3::paste! {
            mod [<test_ $target:snake>] {
                #[test]
                fn desc() {
                    super::$target::type_desc();
                }
            }
        }
    };
}

simple_type!(
    pub enum BasicType {
        Word,
        Long,
        Single,
        Double,
    }
);
impl TryFrom<ExtendedType> for BasicType {
    type Error = ();

    fn try_from(value: ExtendedType) -> Result<Self, Self::Error> {
        todo!()
    }
}
simple_type!(
    pub enum ExtendedType {
        // basic
        Word,
        Long,
        Single,
        Double,
        // extended
        Half,
        Byte,
    }
);
