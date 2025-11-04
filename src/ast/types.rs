use std::fmt::{self, Debug, Display, Formatter, Write};
use std::str::FromStr;

use crate::ast::{NumericLiteral, Span, Spanned, TypeName};
use crate::lexer::ShortTypeSpec;
use crate::lexer::tokens::InvalidShortTypeSpecError;
use crate::print::IndentedPrinter;

mod parse;
#[cfg(test)]
mod test;

/// Specifies the alignment of a type.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct AlignSpec {
    pub span: Span,
    pub value: NumericLiteral<u64>,
}
impl AlignSpec {
    pub fn unspanned(value: u64) -> Self {
        AlignSpec {
            span: Span::MISSING,
            value: NumericLiteral::unspanned(value),
        }
    }
}
impl Display for AlignSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "align {}", self.value)
    }
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
            #[inline] // should reduce to just addition
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
                #[inline] // should reduce to a bounds check + addition
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
    pub enum BaseType {
        Word,
        Long,
        Single,
        Double,
    }
);
impl BaseType {
    pub fn to_extended_type(&self) -> ExtendedType {
        ExtendedType::from(*self)
    }
}
impl TryFrom<ExtendedType> for BaseType {
    type Error = InvalidBaseTypeError;

    #[inline] // should reduce to just a bounds check
    fn try_from(value: ExtendedType) -> Result<Self, Self::Error> {
        value.to_short_spec().try_into()
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
impl From<BaseType> for ExtendedType {
    #[inline] // should reduce to just addition
    fn from(value: BaseType) -> Self {
        value.to_short_spec().try_into().unwrap()
    }
}
simple_type!(
    pub enum SubWordType {
        SignedByte,
        UnsignedByte,
        SignedHalf,
        UnsignedHalf,
    }
);
impl SubWordType {
    /// Erase the sign information, then return the corresponding [`ExtendedType`].
    #[inline]
    pub fn erase_sign_info(&self) -> ExtendedType {
        match self {
            SubWordType::SignedByte | SubWordType::UnsignedByte => ExtendedType::Byte,
            SubWordType::SignedHalf | SubWordType::UnsignedHalf => ExtendedType::Half,
        }
    }
}

macro_rules! maybe_named_type {
    (
        $(#[$ty_attr:meta])*
        $v:vis enum $target:ident {
            Named(TypeName),
            $($simple_variant:ident($simple_type:ty, Span)),+ $(,)?
        }
    ) => {
        $(#[$ty_attr])*
        #[derive(Debug, Clone)]
        $v enum $target {
            Named(TypeName),
            $($simple_variant($simple_type, Span)),*
        }
        impl $target {
            pub fn span(&self) -> Span {
                match *self {
                    $($target::$simple_variant(_, span) => span,)*
                    $target::Named(ref name) => name.span(),
                }
            }
        }
        impl Display for $target {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match self {
                    $target::Named(name) => Display::fmt(name, f),
                    $($target::$simple_variant(simple, _) => Display::fmt(simple, f),)*
                }
            }
        }
        $(impl From<$simple_type> for $target {
            fn from(value: $simple_type) -> Self {
                $target::$simple_variant(value, Span::MISSING)
            }
        }
        impl From<Spanned<$simple_type>> for $target {
            fn from(value: Spanned<$simple_type>) -> Self {
                $target::$simple_variant(value.value, value.span)
            }
        })*
        impl From<TypeName> for $target {
            #[inline]
            fn from(value: TypeName) -> Self {
                $target::Named(value)
            }
        }
    };
}

maybe_named_type! {
    /// Refers to an ABI type, which is either a [`BaseType`], a [`SubWordType`], or a [`TypeName`].
    #[derive(PartialEq, Eq, Hash)]
    pub enum AbiType {
        Named(TypeName),
        Base(BaseType, Span),
        SubWord(SubWordType, Span),
    }
}
maybe_named_type! {
    /// Refers to the type of a field.
    #[derive(PartialEq, Eq, Hash)]
    pub enum FieldType  {
        Named(TypeName),
        Extended(ExtendedType, Span),
    }
}
impl From<BaseType> for FieldType {
    fn from(value: BaseType) -> Self {
        value.to_extended_type().into()
    }
}
impl From<Spanned<BaseType>> for FieldType {
    fn from(value: Spanned<BaseType>) -> Self {
        Spanned::map(value, ExtendedType::from).into()
    }
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct TypeDef {
    pub name: TypeName,
    pub align: Option<AlignSpec>,
    pub span: Span,
    pub body: TypeDefBody,
}
impl TypeDef {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        write!(out, "type {} = ", self.name)?;
        if let Some(ref align) = self.align {
            write!(out, "{align} ")?;
        }
        self.body.print(out)
    }
    pub fn validate(&self) -> Result<(), Vec<InvalidTypeDefReason>> {
        let mut errors = Vec::new();
        match self.body {
            TypeDefBody::Struct(_) => {}
            TypeDefBody::Union(ref body) => {
                if body.variants.is_empty() {
                    errors.push(InvalidTypeDefReason::UnionTypeEmpty);
                }
            }
            TypeDefBody::Opaque(_) => {
                if self.align.is_none() {
                    errors.push(InvalidTypeDefReason::OpaqueMissingAlignment);
                }
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.print(&mut IndentedPrinter::new(f))
    }
}
/// An error that occurs calling [`InvalidTypeDefReason`].
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidTypeDefReason {
    #[error("Opaque type missing alignment")]
    OpaqueMissingAlignment,
    #[error("Union type has no variants")]
    UnionTypeEmpty,
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
#[non_exhaustive]
pub enum TypeDefBody {
    Struct(StructBody),
    Union(UnionBody),
    Opaque(OpaqueBody),
}
impl TypeDefBody {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        match self {
            TypeDefBody::Struct(body) => body.print(out),
            TypeDefBody::Union(body) => body.print(out),
            TypeDefBody::Opaque(body) => body.print(out),
        }
    }
}
impl Display for TypeDefBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.print(&mut IndentedPrinter::new(f))
    }
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct OpaqueBody {
    pub span: Span,
    pub size: NumericLiteral<u64>,
}
impl OpaqueBody {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        // This one is easy to print (no indentation/newlines)
        write!(out, "{self}")
    }
}
impl Display for OpaqueBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ {} }}", self.size)
    }
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct UnionBody {
    pub span: Span,
    pub variants: Vec<StructBody>,
}
impl UnionBody {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        writeln!(out, "{{")?;
        out.indented(|out| {
            for variant in &self.variants {
                out.maybe_writeln()?;
                variant.print(out)?;
            }
            out.maybe_writeln()
        })?;
        write!(out, "}}")
    }
}
impl Display for UnionBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.print(&mut IndentedPrinter::new(f))
    }
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructBody {
    pub span: Span,
    pub fields: Vec<FieldDef>,
}
impl StructBody {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        writeln!(out, "{{")?;
        out.indented(|out| {
            out.print_separated_with(",", &self.fields, |field, out| {
                out.maybe_writeln()?;
                write!(out, "{field}")
            })
        })?;
        write!(out, "\n}}")
    }
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct FieldDef {
    pub span: Span,
    pub ty: FieldType,
    /// The number of times this field should be repeated.
    pub repeated: Option<NumericLiteral<u64>>,
}
impl Display for FieldDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)?;
        if let Some(ref repeated) = self.repeated {
            write!(f, " {repeated}")?;
        }
        Ok(())
    }
}
