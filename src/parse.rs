use chumsky::Parser;
use std::error::Error;
use std::fmt::{Display, Formatter};

use chumsky::input::MapExtra;

use crate::ast::Spanned;
use crate::lexer::{LexError, Token, TokenStream};

pub(crate) use crate::lexer::{RichParseError, TokenParser};

#[derive(Debug)]
pub struct ParseError {
    pub(crate) desc: &'static str,
    pub(crate) reason: ParseErrorReason,
}
impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.reason.source()
    }
}
impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to parse {}: {}", self.desc, self.reason)
    }
}
#[derive(thiserror::Error, Debug)]
pub(crate) enum ParseErrorReason {
    #[error("{0}")]
    Parse(RichParseError<'static, Token>),
    #[error(transparent)]
    Lex(#[from] LexError),
}

pub(crate) fn spanned<'a, T>(
    value: T,
    extra: &mut MapExtra<'a, '_, TokenStream<'a>, crate::lexer::ParserExtra<'a, Token>>,
) -> Spanned<T> {
    Spanned {
        value,
        span: extra.span(),
    }
}

pub(crate) trait Parse: Sized {
    const DESC: &'static str;
    fn parser<'a>() -> impl TokenParser<'a, Self>;
}
pub(crate) fn parse_str<T: Parse>(text: &str) -> Result<T, ParseError> {
    let tokens = crate::lexer::tokenize(text).map_err(|cause| ParseError {
        desc: T::DESC,
        reason: ParseErrorReason::Lex(cause),
    })?;
    T::parser()
        .parse(tokens.as_stream())
        .into_result()
        .map_err(|causes| {
            let first = causes
                .into_iter()
                .next()
                .expect("empty error list")
                .into_owned();
            ParseError {
                desc: T::DESC,
                reason: ParseErrorReason::Parse(first),
            }
        })
}
macro_rules! impl_fromstr_via_parse {
    ($($target:path),+ $(,)?) => {
        $(impl std::str::FromStr for $target {
            type Err = crate::parse::ParseError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                crate::parse::parse_str::<Self>(s)
            }
        })*
    };
}
#[cfg(test)]
macro_rules! test_parse_print {
    ($parse_name:ident, $print_name:ident for $target:ty { $($text:literal => $item:expr),+ $(,)? }) => {
        #[test]
        fn $parse_name() {
            $({
                let text: &str = indoc::indoc!($text);
                assert_eq!(
                    crate::parse::parse_str::<$target>(text)
                        .unwrap_or_else(|e| panic!("{e} (text = `{text}`)")),
                    $item,
                )
            })*
        }
        #[test]
        fn $print_name() {
            $({
                let item: $target = $item;
                assert_eq!(
                    indoc::indoc!($text),
                    ToString::to_string(&item),
                );
            })*
        }
    };
}

pub(crate) use impl_fromstr_via_parse;
#[cfg(test)]
pub(crate) use test_parse_print;
