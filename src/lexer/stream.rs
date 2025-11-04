use crate::ast::span::MissingLocationError;
use crate::ast::{Location, Span, Spanned};
use crate::lexer::Token;
use chumsky::input::{BorrowInput, ExactSizeInput, SliceInput, ValueInput};
use chumsky::prelude::*;
use std::ops::{Range, RangeFrom};

/// An index into the [`TokenStream`].
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct TokenIndex(usize);

/// An owned vector of [tokens](`Token`), produced by [`crate::lexer::tokenize`].
///
/// Prefer using a [`TokenStream`], which implements [`Input`] and is much faster to `Clone`].
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TokenVec {
    /// The main list of tokens
    ///
    /// A struct of arrays might be faster,
    /// but would require changing the `tokenize` function
    tokens: Vec<Spanned<Token>>,
    eof: Location,
}
impl TokenVec {
    #[inline]
    pub fn as_stream(&self) -> TokenStream<'_> {
        TokenStream {
            tokens: &self.tokens,
            eof: self.eof,
        }
    }
    pub(super) fn from_raw_parts(tokens: Vec<Spanned<Token>>, eof: Location) -> Self {
        if let Some(last) = tokens.last() {
            let last_location = last.span.end();
            assert!(!last_location.is_missing());
            assert!(last_location <= eof);
        }
        TokenVec { tokens, eof }
    }
}
impl PartialEq<Vec<Token>> for TokenVec {
    fn eq(&self, other: &Vec<Token>) -> bool {
        self.tokens.len() == other.len() && self.tokens.iter().map(|tk| &tk.value).eq(other.iter())
    }
}
#[derive(Clone)]
pub struct TokenStream<'a> {
    tokens: &'a [Spanned<Token>],
    /// The end of the file.
    ///
    /// Used to allow [`Self::span_for`] with one-past-end inexes.
    ///
    /// TODO: This is conflated with the end of the final token.
    eof: Location,
}
impl<'a> TokenStream<'a> {
    #[inline]
    fn span_for(&self, idx: TokenIndex) -> Span {
        if idx.0 == self.tokens.len() {
            self.eof.to_span()
        } else {
            self.tokens[idx.0].span
        }
    }
    /// Implements all next methods.
    ///
    /// Equivalent to [`BorrowInput::next_ref`], but not `unsafe`.
    #[inline]
    fn do_next(cache: &mut Self, cursor: &mut TokenIndex) -> Option<&'a Token> {
        if let Some(tok) = cache.tokens.get(cursor.0) {
            cursor.0 += 1;
            Some(&tok.value)
        } else {
            None
        }
    }
}
impl<'a> Input<'a> for TokenStream<'a> {
    type Span = Span;
    type Token = Token;
    type MaybeToken = &'a Token;
    type Cursor = TokenIndex;
    type Cache = Self;

    #[inline]
    fn begin(self) -> (Self::Cursor, Self::Cache) {
        (TokenIndex(0), self)
    }

    #[inline]
    fn cursor_location(cursor: &Self::Cursor) -> usize {
        cursor.0
    }

    #[inline]
    unsafe fn next_maybe(cache: &mut Self, cursor: &mut TokenIndex) -> Option<Self::MaybeToken> {
        Self::do_next(cache, cursor)
    }

    #[inline]
    unsafe fn span(state: &mut Self, range: Range<&TokenIndex>) -> Self::Span {
        let state = &*state;
        assert!(range.start <= range.end);
        join_spans(state.span_for(*range.start), state.span_for(*range.end))
    }
}
impl<'a> ValueInput<'a> for TokenStream<'a> {
    #[inline]
    unsafe fn next(cache: &mut Self, cursor: &mut Self::Cursor) -> Option<Token> {
        Self::do_next(cache, cursor).cloned()
    }
}
impl<'a> BorrowInput<'a> for TokenStream<'a> {
    #[inline]
    unsafe fn next_ref(cache: &mut Self, cursor: &mut Self::Cursor) -> Option<&'a Token> {
        Self::do_next(cache, cursor)
    }
}
impl<'a> ExactSizeInput<'a> for TokenStream<'a> {
    unsafe fn span_from(state: &mut Self::Cache, range: RangeFrom<&Self::Cursor>) -> Self::Span {
        join_spans(state.span_for(*range.start), state.eof)
    }
}
impl<'a> SliceInput<'a> for TokenStream<'a> {
    type Slice = TokenStream<'a>;

    #[inline]
    fn full_slice(cache: &mut Self::Cache) -> Self::Slice {
        (*cache).clone()
    }

    #[inline]
    unsafe fn slice(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Slice {
        TokenStream {
            tokens: &cache.tokens[range.start.0..range.end.0],
            eof: cache.eof,
        }
    }

    #[inline]
    unsafe fn slice_from(cache: &mut Self::Cache, range: RangeFrom<&Self::Cursor>) -> Self::Slice {
        TokenStream {
            tokens: &cache.tokens[range.start.0..],
            eof: cache.eof,
        }
    }
}
#[inline]
fn join_spans(first: impl Into<Span>, second: impl Into<Span>) -> Span {
    #[inline]
    fn do_join_spans(first: Span, second: Span) -> Result<Span, MissingLocationError> {
        let start = first.start().byte_offset()?;
        let end = second.end().byte_offset()?;
        assert!(start <= end);
        Ok(Span::from_byte_range(start..end))
    }
    do_join_spans(first.into(), second.into()).unwrap_or(Span::MISSING)
}
