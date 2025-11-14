mod stream;
pub mod tokens;

use crate::ast::{
    BlockName, FloatLiteral, FloatPrefix, GlobalName, Ident, Location, NumericLiteral, Span,
    Spanned, StringLiteral, TemporaryName, TypeName,
};
use chumsky::combinator::Repeated;
use chumsky::error::LabelError;
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
use crate::lexer::stream::TokenVec;
pub(crate) use stream::TokenStream;

parser_trait_alias!(pub(crate) trait TokenParser<'a, O>: Parser<'a, TokenStream<'a>, O, ParserExtra<'a, Token>>);
parser_trait_alias!(pub(crate) trait StringParser<'a, O>: Parser<'a, StringStream<'a>, O, ParserExtra<'a, char>>);

fn require_spacing<'a>() -> impl StringParser<'a, ()> {
    text::whitespace()
        .at_least(1)
        .ignored()
        .or(end())
        .rewind()
        .labelled("spacing")
}
fn symbol<'a>() -> impl StringParser<'a, Operator> {
    Operator::text_parser()
        .filter(Operator::is_symbol)
        .labelled("symbol")
}
fn delimiter<'a>() -> impl StringParser<'a, Token> {
    macro_rules! delimiters {
        ($($txt:literal => $variant:ident),+ $(,)?) => {
            choice((
                $(just($txt).to(Token::$variant),)*
            ))
        };
    }
    delimiters!(
        "{" => OpenBrace,
        "}" => CloseBrace,
        "(" => OpenParen,
        ")" => CloseParen,
    )
}
/// Require either spacing or an operator.
///
/// This allows shorthand like `=w` and `field_def,`.
/// It is still forbidden to have two operators joined together,
/// but that is handled separately.
fn require_space_like<'a>() -> impl StringParser<'a, ()> {
    require_spacing()
        .or(symbol().ignored().or(delimiter().ignored()).rewind())
        .labelled("spacing")
}
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
    let prefixed_idents = prefixed_idents!(TypeName, GlobalName, TemporaryName, BlockName,);
    // A basic (not magic) token
    let basic_token = choice((
        Keyword::text_parser().map(Token::from).labelled("keyword"),
        // must come before ident and type spec or `d_` might be recognized incorrectly
        float_literal().map(Token::Float),
        ShortTypeSpec::text_parser()
            .map(Token::from)
            .labelled("type specifier"),
        prefixed_idents,
        Operator::text_parser()
            .filter(|op| !op.is_symbol())
            .map(Token::from),
        // must come after Operator since `z` is an operator
        ident().map(Token::Ident),
        string_literal().map(Token::StringLiteral),
        delimiter(),
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
    ));
    let forbidding_following_symbol = symbol().ignored().or(text::digits(10)); // may start a number
    let symbol = symbol()
        .then_ignore(forbidding_following_symbol.not().rewind())
        .map(Token::from);
    let newline_token = ascii_newline().repeated().at_least(1).to(Token::Newline);
    symbol
        .or(basic_token.then_ignore(require_space_like()))
        .or(newline_token)
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
/// Parse a single ASCII newline.
///
/// Unlike text::newline, this only accepts ASCII newline operators
/// that would be recognized by [`str::lines`] ("\r\n" and "\n").
///
/// TODO: Contribute this to chumsky?
fn ascii_newline<'a>() -> impl StringParser<'a, &'a str> {
    just("\n").or(just("\r\n")).labelled("newline")
}
type SingleInlineWhitespace<'a> = Boxed<'a, 'a, StringStream<'a>, char, ParserExtra<'a, char>>;
type InlineWhitespace<'a> =
    Repeated<SingleInlineWhitespace<'a>, char, StringStream<'a>, ParserExtra<'a, char>>;
/// Parses a single character of [`inline_whitespace`],
/// giving an error for other types of whitespace.
///
/// Separated from [`inline_whitespace`] mainly for testing.
fn single_inline_whitespace<'a>() -> SingleInlineWhitespace<'a> {
    const ALLOWED_WHITESPACE: [char; 2] = ['\t', ' '];
    let allowed_whitespace = one_of(ALLOWED_WHITESPACE);
    let forbidden_whitespace = any()
        .filter(|c: &char| c.is_whitespace() && !ALLOWED_WHITESPACE.contains(c))
        .and_is(ascii_newline().not())
        .validate(|c, extra, emitter| {
            emitter.emit(LabelError::<StringStream<'a>, _>::expected_found(
                ALLOWED_WHITESPACE,
                Some(c.into()),
                extra.span(),
            ));
            ' ' // treat as a space
        });
    allowed_whitespace.or(forbidden_whitespace).boxed()
}
/// Skips all inline whitespace (tabs and spaces),
/// giving an error for any other type of whitespace.
pub fn inline_whitespace<'a>() -> InlineWhitespace<'a> {
    single_inline_whitespace().repeated()
}

pub(crate) fn tokenizer<'a>() -> impl StringParser<'a, Vec<Spanned<Token>>> {
    let comment = just('#')
        .then(ascii_newline().not().repeated())
        .then(ascii_newline())
        .labelled("comment");
    // Loosely based on the tokenizer in the nano_rust example
    // https://github.com/zesterer/chumsky/blob/0.11/examples/nano_rust.rs#L95-L102
    token()
        .map_with(|token, extra| {
            let span = extra.span();
            if let Some(existing_span) = token.span() {
                assert_eq!(existing_span.eq(), span.eq(), "Inconsistent Token span");
            }
            Spanned { value: token, span }
        })
        .padded_by(comment.repeated())
        .padded_by(inline_whitespace())
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

/// Create a [`StringStream`] from a string.
///
/// Use [`TokenVec::as_stream`] for tokens.
#[inline]
pub(crate) fn stream(input: &str) -> StringStream<'_> {
    input.map_span(span_mapper)
}
/// Tokenize the specified input.
pub fn tokenize(text: &str) -> Result<TokenVec, LexError> {
    tokenizer()
        .parse(stream(text))
        .into_result()
        .map(|tokens| TokenVec::from_raw_parts(tokens, Location::from_byte(text.len())))
        .map_err(LexError::from_rich_list)
}
#[inline]
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
    use chumsky::error::{RichPattern, RichReason};

    fn tokens(val: impl IntoIterator<Item: Into<Token>>) -> Vec<Token> {
        val.into_iter().map(Into::into).collect()
    }

    const VALID_NEWLINES: [&str; 2] = ["\n", "\r\n"];

    #[test]
    fn ascii_newline_parser() {
        for valid in VALID_NEWLINES {
            assert_eq!(
                ascii_newline().parse(stream(valid)).into_result(),
                Ok(valid)
            );
        }
        assert_eq!(
            ascii_newline()
                .or_not()
                .then_ignore(any())
                .parse(stream("\r"))
                .unwrap(),
            None,
            "should not parse `\r`"
        );
    }

    #[test]
    fn ignored_whitespace_parser() {
        #[track_caller]
        fn require_success(text: impl Into<String>) {
            let text = text.into();
            assert_eq!(
                inline_whitespace().to_slice().parse(stream(&text)).unwrap(),
                text
            );
        }
        require_success(" \t ");
        require_success(' ');
        require_success("   ");
        require_success("   \t");
        for newline in VALID_NEWLINES {
            assert_eq!(
                inline_whitespace().parse(stream(newline)).into_output(),
                None
            );
        }
    }

    #[test]
    fn unsupported_whitespace_errors() {
        #[track_caller]
        fn require_failure_matching<'a, O>(
            parser: impl StringParser<'a, O>,
            text: &'a str,
            matches: impl Fn(&RichReason<char>) -> bool,
        ) -> ParseResult<&'a str, RichParseError<'a, char>> {
            let res = parser.to_slice().parse(stream(text));
            assert!(
                res.has_errors(),
                "Parsed without expected errors: res = {:?}, text = {text:?}",
                res.output()
            );
            assert!(
                res.errors().any(|e| matches(e.reason())),
                "Actual parse errors don't match the expected result: {:?}",
                res.clone().into_errors()
            );
            res
        }
        #[track_caller]
        fn check_whitespace_failure<'a, O>(
            parser: impl StringParser<'a, O>,
            text: &'a str,
            failing_chars: impl AsRef<[char]>,
        ) {
            let failing_chars = failing_chars.as_ref();
            let should_be_expected: Vec<RichPattern<char>> = vec!['\t'.into(), ' '.into()];
            let res = require_failure_matching(parser, text, |x| {
                matches!(x, RichReason::ExpectedFound {
                    found: Some(fc),
                    expected,
                } if failing_chars.contains(fc) && *expected == should_be_expected)
            });
            assert_eq!(
                res.into_output(),
                Some(text),
                "Parse should succeed even with invalid whitespace"
            )
        }
        let iws = inline_whitespace(); // primary parser being tested
        let iws = &iws;
        // use of \r is invalid without being preceded by \n
        check_whitespace_failure(iws, "\r", ['\r']);
        check_whitespace_failure(iws, "\r", ['\r']);
        check_whitespace_failure(iws, " \r", ['\r']);
        const UNSUPPORTED_WHITESPACE: &[char] = &[
            '\x0b',   // vertical tab
            '\x0C',   // form feed
            '\u{A0}', // non-breaking space (latin1)
        ];
        for &invalid in UNSUPPORTED_WHITESPACE {
            assert!(invalid.is_whitespace(), "{invalid:?}");
            for s in [String::from(invalid), format!(" \t{invalid} \t")] {
                check_whitespace_failure(inline_whitespace(), &s, [invalid]);
            }
            // if `\r` occurs in a newline, it should be parsed without issue
            check_whitespace_failure(
                single_inline_whitespace()
                    .ignored()
                    .or(ascii_newline().ignored())
                    .repeated(),
                &format!(" \r\n{invalid}\r\n\n"),
                [invalid],
            );
        }
    }

    #[test]
    fn newline_token() {
        #[track_caller]
        fn check(text: &str) {
            assert_eq!(token().parse(stream(text)).unwrap(), Token::Newline);
        }
        check("\n");
        check("\r\n");
        // ensure multiple newlines are collapsed to a single token
        check("\n\n\n");
        check("\r\n\n\r\n\n");
    }

    /// Test the newline token interspersed with other tokens.
    #[test]
    fn newline_token_interspersed() {
        assert_eq!(
            tokenize("\n\n+ z \ntype\r\n").unwrap(),
            tokens([
                Token::Newline,
                operator!(+).into(),
                operator!(z).into(),
                Token::Newline,
                keyword!(type).into(),
                Token::Newline,
            ])
        )
    }

    #[test]
    fn operators() {
        assert_eq!(token().parse(stream("=")).unwrap(), operator!(=).into());
        assert_eq!(
            tokenize("= : + z").unwrap(),
            tokens([operator!(=), operator!(:), operator!(+), operator!(z)])
        );
    }

    /// Ensures that tokens cannot be clumped together without spaces between them,
    /// unless exactly one of them is an operator.
    ///
    /// This is required by the [spacing](https://c9x.me/compile/doc/il.html#Spacing) section
    /// of the QBE IR specification.
    /// The exception involving operators makes things like `=w` and `ident,` valid syntax.
    #[test]
    fn clumped_tokens() {
        fn require_failure(text: &str) {
            let res = tokenize(text);
            assert!(res.is_err(), "{res:?}");
        }
        require_failure("===");
        require_failure("+-");
        assert_eq!(
            tokenize("=type").unwrap(),
            tokens([operator!(=).to_token(), keyword!(type).into(),])
        );
        assert_eq!(
            tokenize("type...").unwrap(),
            tokens([keyword!(type).into(), operator!(...).to_token(),])
        );
        assert_eq!(
            tokenize("type()").unwrap(),
            tokens([keyword!(type).into(), Token::OpenParen, Token::CloseParen])
        );
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
                TypeName::unspanned("foo").into(),
                GlobalName::unspanned("bar").into(),
                TemporaryName::unspanned("baz").into(),
                BlockName::unspanned("foo").into(),
            ]
        )
    }
}
