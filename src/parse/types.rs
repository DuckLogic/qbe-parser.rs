use crate::ast::types::AlignSpec;
use crate::lexer::{Keyword, Token, TokenParser, keyword};
use chumsky::prelude::*;
use std::str::FromStr;

impl AlignSpec {
    pub(crate) fn parser<'a>() -> impl TokenParser<'a, AlignSpec> {
        keyword!(align)
            .parser()
            .ignore_then(select!(Token::Number(number) => number))
            .map_with(|value, extra| AlignSpec {
                value,
                span: extra.span().into(),
            })
    }
}
