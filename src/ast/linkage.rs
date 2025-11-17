use crate::ast::{AstString, Span, StringLiteral};
use crate::lexer::{TokenParser, keyword};
use crate::parse::{Parse, impl_fromstr_via_parse, maybe_newline};
use crate::utils::IterExt;
use arrayvec::ArrayVec;
use chumsky::prelude::*;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LinkageSection {
    pub span: Span,
    pub name: StringLiteral,
    pub flags: Option<StringLiteral>,
}
impl Display for LinkageSection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "section {}", self.name)?;
        if let Some(ref flags) = self.flags {
            write!(f, " {flags}")?;
        }
        Ok(())
    }
}
impl Parse for LinkageSection {
    const DESC: &'static str = "linkage section";

    fn parser<'a>() -> impl TokenParser<'a, Self> {
        keyword!(section)
            .parser()
            .ignore_then(StringLiteral::parser())
            .then(StringLiteral::parser().or_not())
            .map_with(|(name, flags), extra| LinkageSection {
                name,
                flags,
                span: extra.span(),
            })
    }
}
#[derive(Clone, Debug, Eq, PartialEq, Hash, Default)]
#[non_exhaustive]
pub struct Linkage {
    span: Span,
    // with 3 entries, linear search is fast
    // We want to preserve insertion order,
    // and this avoids needing an IndexMap
    specifiers: ArrayVec<LinkageSpecifier, 3>,
}
impl Parse for Linkage {
    const DESC: &'static str = "linkage";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        LinkageSpecifier::parser()
            .then_ignore(maybe_newline())
            .repeated()
            .collect::<Vec<LinkageSpecifier>>()
            .try_map(|specifiers, span| {
                Linkage::from_specifiers(span, specifiers).map_err(|e| Rich::custom(span, e))
            })
            .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(Linkage);
macro_rules! linkage_extract_item {
    ($this:expr => $variant:ident) => {{
        match $this.get(LinkageSpecifierKind::$variant) {
            Some(LinkageSpecifier::$variant(value)) => Some(value),
            Some(other) => unreachable!("{:?}", other.kind()),
            None => None,
        }
    }};
}
impl Linkage {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn is_empty(&self) -> bool {
        self.specifiers.is_empty()
    }
    pub fn is_export(&self) -> bool {
        self.has_specifier(LinkageSpecifierKind::Export)
    }
    pub fn is_thread(&self) -> bool {
        self.has_specifier(LinkageSpecifierKind::Thread)
    }
    pub fn export(&self) -> Option<&'_ ExportLinkage> {
        linkage_extract_item!(self => Export)
    }
    pub fn thread(&self) -> Option<&'_ ThreadLinkage> {
        linkage_extract_item!(self => Thread)
    }
    pub fn section(&self) -> Option<&'_ LinkageSection> {
        linkage_extract_item!(self => Section)
    }
    pub fn from_specifiers(
        span: Span,
        specifiers: impl IntoIterator<Item = LinkageSpecifier>,
    ) -> Result<Self, DuplicateSpecifierError> {
        let mut result = Linkage {
            span,
            specifiers: ArrayVec::new(),
        };
        for spec in specifiers {
            let kind = spec.kind();
            if result.has_specifier(kind) {
                return Err(DuplicateSpecifierError { kind });
            } else {
                result.specifiers.push(spec);
            }
        }
        Ok(result)
    }
    #[inline]
    fn get(&self, kind: LinkageSpecifierKind) -> Option<&LinkageSpecifier> {
        // Emulate filter + Itertools::exactly_one
        let mut res = None;
        for entry in &self.specifiers {
            if entry.kind() == kind {
                assert!(res.is_none(), "Internal Error: Duplicate {kind:?} entries");
                res = Some(entry);
            }
        }
        res
    }
    #[inline]
    pub fn has_specifier(&self, kind: LinkageSpecifierKind) -> bool {
        self.get(kind).is_some()
    }
    #[inline]
    pub fn specifier_kinds(&self) -> impl Iterator<Item = LinkageSpecifierKind> + '_ {
        self.specifiers.iter().map(LinkageSpecifier::kind)
    }
    #[inline]
    pub fn specifiers(&self) -> impl Iterator<Item = &'_ LinkageSpecifier> + '_ {
        self.specifiers.iter()
    }
    pub fn builder() -> LinkageBuilder {
        LinkageBuilder::default()
    }
}
impl Display for Linkage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.specifiers.iter().format(" "))
    }
}
impl From<Linkage> for LinkageBuilder {
    fn from(linkage: Linkage) -> Self {
        LinkageBuilder { linkage }
    }
}
#[derive(Default)]
pub struct LinkageBuilder {
    linkage: Linkage,
}
impl LinkageBuilder {
    /// Add a specifier to the linkage, panicking if already specified.
    ///
    /// # Panics
    /// If the specifier conflicts with an already existing specifier, this will panic.
    /// If this is not desired,
    /// use [`Self::with_specifier_replacing`] or [`Self::try_with_specifier`].
    #[track_caller]
    pub fn with_specifier(&mut self, specifier: impl Into<LinkageSpecifier>) -> &mut Self {
        let specifier = specifier.into();
        if let Some(existing) = self.linkage.get(specifier.kind()) {
            panic!("Specifier `{specifier}` conflicts with existing specifier `{existing}`")
        } else {
            self.linkage.specifiers.push(specifier);
            self
        }
    }
    /// Add a specifier to the linkage.
    /// If a matching specifier already exists, it will replace it.
    ///
    /// Thin wrapper around [`Self::replace_specifier`], which discards the old specifier.
    pub fn with_specifier_replacing(
        &mut self,
        specifier: impl Into<LinkageSpecifier>,
    ) -> &mut Self {
        self.replace_specifier(specifier);
        self
    }
    /// Marks the linkage as [`thread`](ThreadLinkage) if not already marked as such.
    ///
    /// Does nothing if that linkage has already been specified.
    pub fn with_thread(&mut self) -> &mut Self {
        self.with_specifier_replacing(ThreadLinkage {
            span: Span::MISSING,
        })
    }

    /// Marks the linkage as [`export`](ExportLinkage) if not already marked as such.
    ///
    /// Does nothing if that linkage has already been specified.
    pub fn with_export(&mut self) -> &mut Self {
        self.with_specifier_replacing(ExportLinkage {
            span: Span::MISSING,
        })
    }
    /// Add a [`LinkageSection`] with just a name (no flags).
    ///
    /// # Panics
    /// Will panic if a section has already been specified.
    #[track_caller]
    pub fn with_simple_section(&mut self, name: impl Into<AstString>) -> &mut Self {
        self.with_specifier(LinkageSection {
            span: Span::MISSING,
            name: StringLiteral::unspanned(name),
            flags: None,
        })
    }
    /// Add a [`LinkageSection`] with both a name and flags.
    ///
    /// # Panics
    /// If a section has already been specified, this will panic
    #[track_caller]
    pub fn with_section_and_flags(
        &mut self,
        name: impl Into<AstString>,
        flags: impl Into<AstString>,
    ) -> &mut Self {
        self.with_specifier(LinkageSection {
            span: Span::MISSING,
            name: StringLiteral::unspanned(name),
            flags: Some(StringLiteral::unspanned(flags)),
        })
    }
    /// Try to add a specifier to the linkage,
    /// returning an error if it conflicts with an existing specifier.
    pub fn try_with_specifier(
        &mut self,
        specifier: impl Into<LinkageSpecifier>,
    ) -> Result<&mut Self, DuplicateSpecifierError> {
        let specifier = specifier.into();
        if self.linkage.has_specifier(specifier.kind()) {
            Err(DuplicateSpecifierError {
                kind: specifier.kind(),
            })
        } else {
            self.linkage.specifiers.push(specifier);
            Ok(self)
        }
    }
    /// Add a specifier to the linkage,
    /// overriding any conflicting specifier.
    ///
    /// Returns the old specifier if present
    pub fn replace_specifier(
        &mut self,
        specifier: impl Into<LinkageSpecifier>,
    ) -> Option<LinkageSpecifier> {
        let specifier = specifier.into();
        let index = self
            .linkage
            .specifiers
            .iter()
            .position(|item| item.kind() == specifier.kind());
        match index {
            Some(index) => Some(std::mem::replace(
                &mut self.linkage.specifiers[index],
                specifier,
            )),
            None => {
                self.linkage.specifiers.push(specifier);
                None
            }
        }
    }
    #[inline]
    pub fn with_span(&mut self, span: Span) -> &mut Self {
        self.linkage.span = span;
        self
    }
    pub fn build(&mut self) -> Linkage {
        self.linkage.clone()
    }
}

#[derive(thiserror::Error, Debug, Clone, Eq, PartialEq)]
#[error("Linkage contains duplicate `{kind}` specifiers")]
pub struct DuplicateSpecifierError {
    kind: LinkageSpecifierKind,
}
macro_rules! declare_specifiers {
    (enum LinkageSpecifier {
        $($variant:ident($inner:ty)),+ $(,)?
    }) => {
        /// The kind of [`LinkageSpecifier`].
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
        #[non_exhaustive]
        #[repr(usize)]
        pub enum LinkageSpecifierKind {
            $($variant,)*
        }
        impl LinkageSpecifierKind {
            /// The number of different kinds.
            pub const COUNT: usize = declare_specifiers!(@count $($variant),*);
            #[inline]
            pub fn as_str(self) -> &'static str {
                match self {
                    $(LinkageSpecifierKind::$variant => paste3::paste!(stringify!([<$variant:lower>])),)*
                }
            }
            #[inline]
            pub fn index(self) -> usize {
                self as usize
            }
            #[inline]
            pub fn from_index(idx: usize) -> Option<LinkageSpecifierKind> {
                if idx < Self::COUNT {
                    // SAFETY: Performed the appropriate bounds check
                    Some(unsafe { std::mem::transmute::<usize, Self>(idx) })
                } else {
                    None
                }
            }
        }
        impl Display for LinkageSpecifierKind {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.write_str(self.as_str())
            }
        }
        #[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
        #[non_exhaustive]
        pub enum LinkageSpecifier {
            $($variant($inner),)*
        }
        impl LinkageSpecifier {
            #[inline]
            pub fn kind(&self) -> LinkageSpecifierKind {
                match self {
                    $(Self::$variant(_) => LinkageSpecifierKind::$variant,)*
                }
            }
            #[inline]
            pub fn span(&self) -> Span {
                match self {
                    $(Self::$variant(inner) => inner.span,)*
                }
            }
        }
        impl Display for LinkageSpecifier {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$variant(inner) => write!(f, "{inner}"),)*
                }
            }
        }
        impl Parse for LinkageSpecifier {
            const DESC: &'static str = "linkage specifier";
            fn parser<'a>() -> impl TokenParser<'a, Self> {
                choice((
                    $(<$inner as Parse>::parser().map(LinkageSpecifier::$variant)),*
                )).labelled(Self::DESC)
            }
        }
        $(impl From<$inner> for LinkageSpecifier {
            #[inline]
            fn from(v: $inner) -> Self {
                Self::$variant(v)
            }
        })*
    };
    (@count) => (0);
    (@count $first:ident $(, $item:ident)* $(,)?) => {
        1 + declare_specifiers!(@count $($item),*)
    }
}
declare_specifiers!(
    enum LinkageSpecifier {
        Export(ExportLinkage),
        Thread(ThreadLinkage),
        Section(LinkageSection),
    }
);

/// Specifies `export` [linkage](Linkage).
#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ExportLinkage {
    pub span: Span,
}
impl Display for ExportLinkage {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("export")
    }
}
impl Parse for ExportLinkage {
    const DESC: &'static str = "export linkage spec";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        keyword!(export).parser().map(|span| ExportLinkage { span })
    }
}
/// Specifies `thread` [linkage](Linkage).
#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ThreadLinkage {
    pub span: Span,
}
impl Display for ThreadLinkage {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("thread")
    }
}
impl Parse for ThreadLinkage {
    const DESC: &'static str = "thread linkage spec";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        keyword!(thread).parser().map(|span| ThreadLinkage { span })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use similar_asserts::assert_eq;

    fn export() -> LinkageSpecifier {
        ExportLinkage {
            span: Span::MISSING,
        }
        .into()
    }

    fn thread() -> LinkageSpecifier {
        ThreadLinkage {
            span: Span::MISSING,
        }
        .into()
    }

    fn builder() -> LinkageBuilder {
        LinkageBuilder::default()
    }

    fn linkage<const N: usize>(sections: [LinkageSpecifier; N]) -> Linkage {
        assert!(sections.len() <= LinkageSpecifierKind::COUNT);
        Linkage::from_specifiers(Span::MISSING, sections).unwrap()
    }

    #[test]
    fn parse_linkage() {
        assert_eq!("".parse::<Linkage>().unwrap(), linkage([]));
        assert_eq!("export".parse::<Linkage>().unwrap(), linkage([export()]),);
        assert_eq!(
            "export thread".parse::<Linkage>().unwrap(),
            linkage([export(), thread()]),
        );
        assert_eq!(
            "thread\nexport".parse::<Linkage>().unwrap(),
            linkage([thread(), export()]),
        );
        assert_eq!(
            "export thread section \"foo\"".parse::<Linkage>().unwrap(),
            builder()
                .with_export()
                .with_thread()
                .with_simple_section("foo")
                .build()
        );
        assert_eq!(
            "export thread section \"foo\" \"flags\""
                .parse::<Linkage>()
                .unwrap(),
            builder()
                .with_export()
                .with_thread()
                .with_section_and_flags("foo", "flags")
                .build(),
        );
    }

    #[test]
    fn print_linkage() {
        assert_eq!("", Linkage::default().to_string());
        assert_eq!("export", linkage([export()]).to_string());
        assert_eq!("export thread", linkage([export(), thread()]).to_string());
        assert_eq!("thread export", linkage([thread(), export()]).to_string());
        assert_eq!(
            "export thread section \"foo\"",
            builder()
                .with_export()
                .with_thread()
                .with_simple_section("foo")
                .build()
                .to_string()
        );
        assert_eq!(
            "export thread section \"foo\" \"flags\"",
            builder()
                .with_export()
                .with_thread()
                .with_section_and_flags("foo", "flags")
                .build()
                .to_string()
        );
    }
}
