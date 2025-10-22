//! Defines the [`SimpleInstructionArgs`] type.

use crate::ast::functions::Value;
use arrayvec::ArrayVec;
use std::iter::{DoubleEndedIterator, FusedIterator};
use std::ops::Deref;

const MAX_SIMPLE_ARGS: usize = 3;

/// Holds the arguments for a simple instruction,
/// limited to [`Self::LIMIT`] entries.
///
/// Unfortunately, this needs to be boxed because otherwise it will
/// activate the `clippy::large_enum_variant` warning.
/// This defeats the advantage of using an [`ArrayVec`] internally.
/// Using a [`Vec`] would probably be simpler.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Default)]
pub struct SimpleInstructionArgs(Box<ArrayVec<Value, MAX_SIMPLE_ARGS>>);
impl SimpleInstructionArgs {
    /// The maximum number of arguments permitted for a simple instruction.
    pub const LIMIT: usize = MAX_SIMPLE_ARGS;
    #[inline]
    pub fn new() -> SimpleInstructionArgs {
        SimpleInstructionArgs(Box::new(ArrayVec::new()))
    }
    /// Push a value onto the list of arguments, panicking if it would exceed the [`Self::LIMIT`].
    #[track_caller]
    pub fn push(&mut self, value: Value) {
        self.try_push(value).unwrap();
    }
    /// Push a value onto the list of arguments, returning an error if it would exceed the [`Self::LIMIT`].
    pub fn try_push(&mut self, value: Value) -> Result<(), TooManySimpleInsnArgsError> {
        self.0
            .try_push(value)
            .map_err(|_| TooManySimpleInsnArgsError { _priv: () })
    }
    pub fn try_from_iter(
        iter: impl IntoIterator<Item = Value>,
    ) -> Result<Self, TooManySimpleInsnArgsError> {
        let mut res = SimpleInstructionArgs::new();
        for value in iter.into_iter() {
            res.try_push(value)?;
        }
        Ok(res)
    }
}
impl Deref for SimpleInstructionArgs {
    type Target = [Value];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl FromIterator<Value> for SimpleInstructionArgs {
    #[track_caller]
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self::try_from_iter(iter).unwrap_or_else(|err| panic!("{err}"))
    }
}
impl<'a> IntoIterator for &'a SimpleInstructionArgs {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl TryFrom<&[Value]> for SimpleInstructionArgs {
    type Error = TooManySimpleInsnArgsError;

    fn try_from(value: &[Value]) -> Result<Self, Self::Error> {
        match ArrayVec::try_from(value) {
            Ok(res) => Ok(SimpleInstructionArgs(Box::new(res))),
            Err(_) => Err(TooManySimpleInsnArgsError { _priv: () }),
        }
    }
}
impl From<[Value; 0]> for SimpleInstructionArgs {
    fn from(value: [Value; 0]) -> Self {
        let [] = value;
        SimpleInstructionArgs::new()
    }
}
impl From<[Value; 1]> for SimpleInstructionArgs {
    fn from(value: [Value; 1]) -> Self {
        SimpleInstructionArgs(Box::new(ArrayVec::from_iter(value)))
    }
}
impl From<[Value; 2]> for SimpleInstructionArgs {
    fn from(value: [Value; 2]) -> Self {
        SimpleInstructionArgs(Box::new(ArrayVec::from_iter(value)))
    }
}
impl From<[Value; 3]> for SimpleInstructionArgs {
    fn from(value: [Value; 3]) -> Self {
        SimpleInstructionArgs(Box::new(ArrayVec::from(value)))
    }
}
impl IntoIterator for SimpleInstructionArgs {
    type Item = Value;
    type IntoIter = IntoIter;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.0.into_iter())
    }
}
#[derive(Debug, Clone, thiserror::Error)]
#[error(
    "Number of simple instruction arguments exceeds limit {limit}",
    limit = SimpleInstructionArgs::LIMIT
)]
pub struct TooManySimpleInsnArgsError {
    _priv: (),
}

/// An iterator over the values of a [`SimpleInstructionArgs`].
///
/// Ideally, this would be an existential type.
#[derive(Clone, Debug)]
pub struct IntoIter(arrayvec::IntoIter<Value, MAX_SIMPLE_ARGS>);
impl Iterator for IntoIter {
    type Item = Value;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
impl ExactSizeIterator for IntoIter {}
impl FusedIterator for IntoIter {}
impl DoubleEndedIterator for IntoIter {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}
