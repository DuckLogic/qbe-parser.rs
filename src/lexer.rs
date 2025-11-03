pub mod tokens;

use crate::ast::{
    BlockName, FloatLiteral, FloatPrefix, GlobalName, Ident, NumericLiteral, Span, Spanned,
    StringLiteral, TemporaryName, TypeName,
};
use chumsky::input::{MapExtra, MappedSpan};
use chumsky::prelude::*;
use ordered_float::OrderedFloat;
pub(crate) use tokens::{Keyword, Operator, ShortTypeSpec, Token};
pub(crate) use tokens::{keyword, operator};

pub(crate) type RichParseError<'a, T = Token> = Rich<'a, T, Span>;
pub(crate) type ParserExtra<'a, T = Token> = extra::Err<RichParseError<'a, T>>;
macro_rules! parser_trait_alias {
    ($v:vis trait $name:ident<$l:lifetime, $output:ident>: $($bound:tt)*) => {
        $v trait $name<$l, $output>: $($bound)* {}
        impl<$l, $output, T> $name<$l, $output> for T
            where T: $($bound)* {}
    };
}
type SpanMapFunc = fn(SimpleSpan) -> Span;
fn span_mapper(src: SimpleSpan) -> Span {
    src.into()
}
pub(crate) type StringStream<'a> = MappedSpan<Span, &'a str, SpanMapFunc>;
pub(crate) type TokenStream<'a> = MappedSpan<Span, &'a [Token], SpanMapFunc>;
parser_trait_alias!(pub(crate) trait TokenParser<'a, O>: Parser<'a, TokenStream<'a>, O, ParserExtra<'a, Token>>);
parser_trait_alias!(pub(crate) trait StringParser<'a, O>: Parser<'a, StringStream<'a>, O, ParserExtra<'a, char>>);

fn token<'a>() -> impl StringParser<'a, Token> {
    macro_rules! prefixed_idents {
        ($($target:ident),+ $(,)?) => ({
            choice((
                $(prefixed_idents!(@specific $target),)*
            ))
        });
        (@specific $target:ident) => ({
            just($target::PREFIX)
                .ignore_then(ident())
                .map_with(spanned)
                .labelled($target::label())
                .map(|x| Token::$target($target::new(x.value, x.span)))
        });
    }
    macro_rules! delimiter {
        ($txt:literal => $variant:ident) => {
            just($txt)
                .map_with(spanned)
                .map(|val| Token::$variant(val.span))
        };
    }
    let prefixed_idents = prefixed_idents!(TypeName, GlobalName, TemporaryName, BlockName,);
    // A single token
    choice((
        Keyword::text_parser()
            .map_with(spanned)
            .map(Token::from)
            .labelled("keyword"),
        // must come before ident and type spec or `d_` might be recognized incorrectly
        float_literal().map(Token::Float),
        ShortTypeSpec::text_parser()
            .map_with(spanned)
            .map(Token::from)
            .labelled("type specifier"),
        prefixed_idents,
        Operator::text_parser().map_with(spanned).map(Token::from),
        // must come after Operator since `z` is an operator
        ident().map(Token::Ident),
        string_literal().map(Token::StringLiteral),
        delimiter!("{" => OpenBrace),
        delimiter!("}" => CloseBrace),
        one_of("+-")
            .or_not()
            .ignore_then(text::int(10))
            .to_slice()
            .try_map(|text: &str, span| {
                let value = text
                    .parse::<i128>()
                    .map_err(|e| Rich::custom(span, format!("Failed to parse integer, {e}")))?;
                let first = text.chars().next().unwrap();
                if !matches!(first, '+' | '-')
                    && let Ok(value) = u64::try_from(value)
                {
                    Ok(Token::Number(NumericLiteral { value, span }))
                } else {
                    Ok(Token::Integer(NumericLiteral { value, span }))
                }
            })
            .labelled("integer"),
    ))
    .labelled("token")
    .boxed()
}
fn float_literal<'a>() -> impl StringParser<'a, FloatLiteral> {
    let prefix = choice((
        just("d_").to(FloatPrefix::DoublePrecision),
        just("s_").to(FloatPrefix::SinglePrecision),
    ))
    .map_with(spanned);
    prefix
        .then(floating_point_value())
        .map_with(|(prefix, value), extra| FloatLiteral {
            span: extra.span(),
            value,
            prefix,
        })
        .labelled("floating point literal")
}
fn floating_point_value<'a>() -> impl StringParser<'a, NumericLiteral<OrderedFloat<f64>>> {
    one_of("+-")
        .or_not()
        .then(text::int(10).or_not())
        .then(just("."))
        .then(text::int(10))
        .to_slice()
        .try_map(|text: &str, span| {
            let value = text.parse::<f64>().map_err(|e| {
                Rich::custom(span, format!("Failed to parse floating-point number, {e}"))
            })?;
            Ok(NumericLiteral {
                value: OrderedFloat(value),
                span,
            })
        })
        .labelled("floating-point number")
}
pub(crate) fn tokenizer<'a>() -> impl StringParser<'a, Vec<Token>> {
    // Unlike text::newline, this only accepts ASCII newline operators
    // TODO: Contribute this to chumsky?
    let newline = one_of("\r\n")
        .ignored()
        .or(just("\r\n").ignored())
        .labelled("newline");
    let comment = just('#')
        .then(newline.not().repeated())
        .then(newline)
        .labelled("comment");
    // Loosely based on the tokenizer in the nano_rust example
    // https://github.com/zesterer/chumsky/blob/0.11/examples/nano_rust.rs#L95-L102
    token()
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead.
        // This strategy was copied from the nano_rust example:
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[derive(Debug, thiserror::Error)]
#[error("Failed to tokenize input")]
pub struct LexError(RichParseError<'static, char>);
impl LexError {
    pub(crate) fn from_rich_list(value: Vec<RichParseError<'_, char>>) -> Self {
        Self::from_rich(value.into_iter().next().expect("empty error list"))
    }
    pub(crate) fn from_rich(rich: RichParseError<'_, char>) -> Self {
        LexError(rich.into_owned())
    }
}

#[doc(hidden)]
pub(crate) trait IntoStream<'a> {
    type Stream: Input<'a, Span = Span>;
    #[allow(
        clippy::wrong_self_convention,
        reason = "may interfere with inherent methods"
    )]
    fn into_stream(this: Self) -> Self::Stream;
}
impl<'a> IntoStream<'a> for &'a str {
    type Stream = StringStream<'a>;
    #[inline]
    fn into_stream(this: Self) -> Self::Stream {
        this.map_span(span_mapper)
    }
}
impl<'a> IntoStream<'a> for &'a [Token] {
    type Stream = TokenStream<'a>;
    #[inline]
    fn into_stream(this: Self) -> Self::Stream {
        this.map_span(span_mapper)
    }
}
impl<'a> IntoStream<'a> for &'a Vec<Token> {
    type Stream = TokenStream<'a>;
    #[inline]
    fn into_stream(this: Self) -> Self::Stream {
        stream(this.as_slice())
    }
}
pub(crate) fn stream<'a, S: IntoStream<'a>>(input: S) -> S::Stream {
    S::into_stream(input)
}
/// Tokenize the specified input.
pub fn tokenize(text: &str) -> Result<Vec<Token>, LexError> {
    tokenizer()
        .parse(stream(text))
        .into_result()
        .map_err(LexError::from_rich_list)
}

fn spanned<'a, T>(
    value: T,
    extra: &mut MapExtra<'a, '_, StringStream<'a>, ParserExtra<'a, char>>,
) -> Spanned<T> {
    Spanned {
        value,
        span: extra.span(),
    }
}
fn ident<'a>() -> impl StringParser<'a, Ident> {
    text::ident::<StringStream<'a>, _>()
        .map_with(spanned)
        .map(Ident::from)
        .labelled("identifier")
}
/// Parses an escape character generated by [`char::escape_default`].
fn string_escape_char<'a>() -> impl StringParser<'a, char> {
    let unicode_escape_value = any()
        .filter(char::is_ascii_hexdigit)
        .repeated()
        .at_least(1)
        .at_most(6)
        .to_slice()
        .map(|x| u32::from_str_radix(x, 16).expect("cannot fail"))
        .delimited_by(just("{"), just("}"));
    let unicode_escape = just("\\u")
        .ignore_then(unicode_escape_value)
        .try_map(|value, span| {
            char::from_u32(value)
                .ok_or_else(|| Rich::custom(span, "Unicode escape references invalid codepoint"))
        })
        .labelled("unicode escape");
    let basic_escape = just('\\').ignore_then(any()).try_map(|x, span| {
        Ok(match x {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '0' => '\0',
            '\\' | '\'' | '"' => x, // these escape chars correspond to their own values
            _ => return Err(Rich::custom(span, "Unknown escape character")),
        })
    });
    unicode_escape.or(basic_escape).labelled("escape char")
}
fn string_literal<'a>() -> impl StringParser<'a, StringLiteral> {
    // needs to be able to reverse
    let string_part = none_of(['\\', '"']).or(string_escape_char());
    string_part
        .repeated()
        .collect::<String>()
        .delimited_by(just("\""), just("\""))
        .map_with(spanned)
        .map(StringLiteral::from)
        .labelled("string literal")
}

#[cfg(test)]
mod test {
    use super::*;

    fn tokens(val: impl IntoIterator<Item: Into<Token>>) -> Vec<Token> {
        val.into_iter().map(Into::into).collect()
    }

    #[test]
    fn operators() {
        assert_eq!(token().parse(stream("=")).unwrap(), operator!(=).into());
        assert_eq!(
            tokenize("= : + z").unwrap(),
            tokens([operator!(=), operator!(:), operator!(+), operator!(z)])
        )
    }

    #[test]
    fn float_literals_basic() {
        fn float(val: f64, prefix: FloatPrefix) -> FloatLiteral {
            FloatLiteral {
                span: Span::MISSING,
                prefix: Spanned::from(prefix),
                value: NumericLiteral {
                    value: OrderedFloat(val),
                    span: Span::MISSING,
                },
            }
        }
        fn double(f: f64) -> FloatLiteral {
            float(f, FloatPrefix::DoublePrecision)
        }
        fn single(f: f64) -> FloatLiteral {
            float(f, FloatPrefix::SinglePrecision)
        }
        assert_eq!(
            tokenize("d_5.8 s_1.0").unwrap(),
            tokens([double(5.8), single(1.0)])
        )
    }

    #[test]
    fn prefixed_idents() {
        assert_eq!(
            tokenize(":foo $bar %baz @foo").unwrap(),
            vec![
                TypeName::without_span("foo").into(),
                GlobalName::without_span("bar").into(),
                TemporaryName::without_span("baz").into(),
                BlockName::without_span("foo").into(),
            ]
        )
    }
}
