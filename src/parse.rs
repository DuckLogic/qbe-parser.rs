use chumsky::Parser;
use std::error::Error;
use std::fmt::{Display, Formatter};

use chumsky::error::Rich;
use chumsky::input::MapExtra;

use crate::ast::Spanned;
use crate::lexer::{LexError, Token, TokenParser};

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
    Parse(Rich<'static, Token>),
    #[error(transparent)]
    Lex(#[from] LexError),
}

pub(crate) fn spanned<'a, T>(
    value: T,
    extra: &mut MapExtra<'a, '_, &'a [Token], crate::lexer::ParserExtra<'a, Token>>,
) -> Spanned<T> {
    Spanned {
        value,
        span: extra.span().into(),
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
    T::parser().parse(&tokens).into_result().map_err(|causes| {
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
    ($($target:ident),+ $(,)?) => {
        $(impl std::str::FromStr for $target {
            type Err = crate::parse::ParseError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                crate::parse::parse_str::<Self>(s)
            }
        })*
    };
}

pub(crate) use impl_fromstr_via_parse;
