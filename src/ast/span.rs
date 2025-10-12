use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Range;

/// An error that occurs when a [`Location`] or [`Span`] is missing.
///
/// See [`Location::MISSING`] and [`Span::MISSING`] for more details.
#[derive(Copy, Clone, Debug, thiserror::Error)]
#[error("Missing location information")]
pub struct MissingLocationError;

/// Either references a location in the original source file,
/// or is [`Location::missing`].
///
/// A location has a total order based on the byte offset,
/// with [`Location::MISSING`] coming after every valid location.
#[derive(Copy, Clone)]
pub struct Location {
    // Should really be `Option<NonMax>`.
    // Once NonMax becomes stable, this will become public
    byte_offset: u64,
}
impl Location {
    /// Indicates that the location information is missing,
    /// and doesn't correspond to a known location in the source file.
    ///
    /// For the purposes of a comparison using [`Ord`],
    /// a missing value comes after all present values.
    pub const MISSING: Location = Location {
        byte_offset: u64::MAX,
    };

    /// Create a [`Location`] referencing a byte offset in the source file.
    ///
    /// # Panics
    /// If the byte offset is too large, this will panic.
    #[inline]
    pub fn from_byte<T: sealed::PrimUInt>(byte_offset: T) -> Location {
        let byte_offset = num_traits::cast::<_, u64>(byte_offset)
            .filter(|&x| x != u64::MAX)
            .expect("byte offset overflow");
        Location { byte_offset }
    }

    /// Determine if the location is [`Self::MISSING`].
    #[inline]
    pub const fn is_missing(self) -> bool {
        self.byte_offset == Self::MISSING.byte_offset
    }

    /// Return the byte offset of this location,
    /// or a [`MissingLocationError`] if missing.
    #[inline]
    pub const fn byte_offset(self) -> Result<u64, MissingLocationError> {
        if !self.is_missing() {
            Ok(self.byte_offset)
        } else {
            Err(MissingLocationError)
        }
    }

    /// A span which points only to this location.
    ///
    /// Returns [`Span::MISSING`] if this location is missing.
    #[inline]
    pub fn to_span(self) -> Span {
        Span {
            start: self,
            end: self,
        }
    }
}
impl PartialEq for Location {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.byte_offset == other.byte_offset
    }
}
impl Eq for Location {}
impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Location {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        const {
            assert!(
                Location::MISSING.byte_offset == u64::MAX,
                "Integer ordering must match semantic ordering"
            );
        }
        self.byte_offset.cmp(&other.byte_offset)
    }
}
impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.byte_offset() {
            Ok(offset) => write!(f, "{offset}"),
            Err(MissingLocationError) => f.write_str("MISSING"),
        }
    }
}
impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_missing() {
            f.write_str("Location::MISSING")
        } else {
            write!(f, "Location({self})")
        }
    }
}
impl Default for Location {
    fn default() -> Self {
        Location::MISSING
    }
}

/// References a range of bytes in the original source file.
///
/// Can be ["missing"](Span::missing),
/// in which case doesn't correspond to any actual portion of the source file.
/// This is the [`Default`] value.
///
/// For the purposes of comparisons, all spans are considered equal to each other.
/// This means that calling [`PartialEq::eq`] just returns `true`,
/// calling [`Has:h::hash`] does nothing
/// and calling [`Ord::cmp`] always returns [`Ordering::Equal`].
/// This means that adding a [`Span`] field to a type
/// can never impact the derived implementations of these traits.
#[derive(Copy, Clone)]
pub struct Span {
    // this is private in the hopes it can someday become a
    // `pub byte_offsets: Option<std::range::Range<NonMaxU64>>`
    // See `Location` for a similar  hope
    start: Location,
    end: Location,
}
impl Span {
    /// Indicates that the span information is missing,
    /// and doesn't correspond to a known portion of the source file.
    pub const MISSING: Span = Span {
        start: Location::MISSING,
        end: Location::MISSING,
    };

    /// Create a span with the specified start and end locations.
    ///
    /// If either location is [missing], this will return [`Span::MISSING`].
    ///
    /// # Panics
    /// If the start location comes after the end location, this function will panic.
    /// If either location is [missing], this function is guaranteed to succeed.
    ///
    /// [missing]: Location::MISSING
    #[inline]
    #[track_caller]
    pub fn new(start: Location, end: Location) -> Span {
        // don't use assert! because missing locations will return false
        if start > end {
            panic!("start > end: {start} > {end}");
        }
        Span { start, end }
    }

    /// Create a span from the specified range of byte indexes.
    ///
    /// # Panics
    /// This will panic if the start location comes after the end location,
    /// just like [`Span::new`].
    /// This will also panic if either offset overflows a [`Location`],
    /// just like [`Location::from_byte`].
    #[inline]
    #[track_caller]
    pub fn from_byte_range<T: sealed::PrimUInt>(range: Range<T>) -> Self {
        assert!(
            range.start <= range.end,
            "start > end: {} > {}",
            range.start,
            range.end
        );
        Span {
            start: Location::from_byte(range.start),
            end: Location::from_byte(range.end),
        }
    }

    /// Return a wrapper type that implements by-value equality
    /// rather than having all values be equal.
    #[inline]
    pub fn eq(self) -> OrderedSpan {
        OrderedSpan(self)
    }

    /// Return a wrapper type that implements by-value ordering
    /// rather than having all values be equal.
    #[inline]
    pub fn ord(self) -> OrderedSpan {
        OrderedSpan(self)
    }

    /// Check if the span is [`Self::MISSING`].
    #[inline]
    pub const fn is_missing(&self) -> bool {
        self.start.is_missing() || self.end.is_missing()
    }

    /// Return the range of bytes in the original file,
    /// or a [`MissingLocationError`] if missing.
    #[inline]
    pub fn byte_range(&self) -> Result<Range<u64>, MissingLocationError> {
        Ok(self.start.byte_offset()?..self.end.byte_offset()?)
    }
}
impl From<Location> for Span {
    fn from(value: Location) -> Self {
        value.to_span()
    }
}
impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.byte_range() {
            Ok(Range { start, end }) => write!(f, "{start}..{end}"),
            Err(MissingLocationError) => f.write_str("MISSING"),
        }
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_missing() {
            f.write_str("Span::MISSING")
        } else {
            write!(f, "Span({self})")
        }
    }
}
impl Default for Span {
    fn default() -> Self {
        Span::MISSING
    }
}
/// Unconditionally returns `true`,
/// so that adding a Span to a struct will not affect `derive(Eq)`
impl PartialEq for Span {
    fn eq(&self, _other: &Span) -> bool {
        // all spans are equal
        true
    }
}
impl Eq for Span {}
/// Hashes the span.
///
/// This is guaranteed to do nothing,
/// so adding it to a struct will not affect the result of `derive(Hash)`.
impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // does not do anything, per type guarantees
        let _ = state;
    }
}
impl PartialOrd for Span {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Span {
    #[inline]
    fn cmp(&self, _other: &Self) -> Ordering {
        // all spans are equal
        Ordering::Equal
    }
}
impl From<OrderedSpan> for Span {
    #[inline]
    fn from(value: OrderedSpan) -> Self {
        value.0
    }
}
/// A wrapper around [`Span`] that properly implements [`Eq`] and [`Ord`]
/// installed of always being [`Ordering::Equal`].
///
/// A [`Span::MISSING`] is considered greater than all other spans.
#[derive(Debug, Copy, Clone)]
pub struct OrderedSpan(pub Span);
impl OrderedSpan {
    /// The [`OrderedSpan`] corresponding to [`Span::MISSING`].
    pub const MISSING: Self = OrderedSpan(Span::MISSING);
}
impl From<Span> for OrderedSpan {
    #[inline]
    fn from(span: Span) -> Self {
        OrderedSpan(span)
    }
}
impl Hash for OrderedSpan {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.byte_range() {
            Ok(o) => (o.start, o.end).hash(state),
            Err(MissingLocationError) => state.write_u8(0),
        }
    }
}
impl PartialEq for OrderedSpan {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.byte_range() == other.byte_range()
    }
}
impl PartialEq<Span> for OrderedSpan {
    fn eq(&self, other: &Span) -> bool {
        self.byte_range() == other.byte_range()
    }
}
impl Eq for OrderedSpan {}
impl PartialOrd for OrderedSpan {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialOrd<Span> for OrderedSpan {
    fn partial_cmp(&self, other: &Span) -> Option<Ordering> {
        Some(self.cmp(&other.ord()))
    }
}
impl Ord for OrderedSpan {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.0.byte_range(), other.0.byte_range()) {
            (Ok(this_range), Ok(other_range)) => this_range
                .start
                .cmp(&other_range.start)
                .then_with(|| this_range.end.cmp(&other_range.end)),
            (Err(_), Ok(_)) => Ordering::Greater,
            (Ok(_), Err(_)) => Ordering::Less,
            (Err(_), Err(_)) => Ordering::Equal,
        }
    }
}
impl Deref for OrderedSpan {
    type Target = Span;
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

mod sealed {
    use std::fmt::{Debug, Display};

    /// An internal trait for unsigned primitive integers.
    ///
    /// Used for [`Location::from_byte`] and [`Span::from_byte_range`].
    ///
    /// Unlike [`num_traits::PrimInt`], this is is restricted to primitive integers only.
    pub trait PrimUInt: Display + Debug + num_traits::PrimInt + num_traits::Unsigned {}
    macro_rules! impl_prim_uints {
        ($($target:ident),+ $(,)?) => {
            $(impl PrimUInt for $target {})*
        };
    }
    impl_prim_uints!(u8, u16, u32, u64, usize, u128);
}
