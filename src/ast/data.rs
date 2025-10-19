use crate::ast::linkage::Linkage;
use crate::ast::types::{AlignSpec, ExtendedType};
use crate::ast::{FloatLiteral, GlobalName, Number, NumericLiteral, Span, StringLiteral};
use crate::lexer::{Token, TokenParser, keyword, operator};
use crate::parse::{Parse, impl_fromstr_via_parse};
use std::fmt::{self, Display, Formatter, Write};

use crate::print::{IndentedPrinter, impl_display_via_print};
use chumsky::prelude::*;

#[derive(Debug, Eq, PartialEq, Hash, Clone, typed_builder::TypedBuilder)]
#[non_exhaustive]
pub struct DataDef {
    #[builder(default)]
    pub span: Span,
    #[builder(default)]
    pub linkage: Linkage,
    pub name: GlobalName,
    #[builder(default, setter(strip_option))]
    pub align: Option<AlignSpec>,
    pub fields: Vec<DataField>,
}
impl DataDef {
    fn print(&self, out: &mut IndentedPrinter) -> fmt::Result {
        if self.linkage != Linkage::default() {
            write!(out, "{} ", self.linkage)?;
        }
        write!(out, "data {} = ", self.name)?;
        if let Some(ref align) = self.align {
            write!(out, "{align} ")?;
        }
        out.write_char('{')?;
        out.indented(|out| {
            out.maybe_writeln()?;
            out.print_separated_with(",\n", &self.fields, |field, out| write!(out, "{field}"))
        })?;
        out.maybe_writeln()?;
        out.write_char('}')
    }
}
impl_display_via_print!(DataDef);

impl Parse for DataDef {
    const DESC: &'static str = "data definition";
    #[allow(clippy::unused_unit, reason = "part of select macro")]
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        let body = DataField::parser()
            .separated_by(operator!(,).parser())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(select!(Token::OpenBrace(_)), select!(Token::CloseBrace(_)));
        Linkage::parser()
            .then_ignore(keyword!(data).parser())
            .then(select!(Token::GlobalName(name) => name))
            .then_ignore(operator!(=).parser())
            .then(AlignSpec::parser().or_not())
            .then(body)
            .map_with(|(((linkage, name), align), body), extra| DataDef {
                span: extra.span().into(),
                linkage,
                name,
                align,
                fields: body,
            })
            .labelled(Self::DESC)
    }
}

/// A field in a [`DataDef`]
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
#[non_exhaustive]
pub enum DataField {
    Regular {
        span: Span,
        ty: ExtendedType,
        items: Vec<DataItem>,
    },
    ZeroInitialize {
        span: Span,
        count: NumericLiteral<u64>,
    },
}
impl Parse for DataField {
    const DESC: &'static str = "data field";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        let regular_field = ExtendedType::parser()
            .then(
                DataItem::parser()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .map_with(|(ty, items), extra| DataField::Regular {
                ty,
                span: extra.span().into(),
                items,
            })
            .labelled("regular field");
        let zero_init = operator!(z)
            .parser()
            .ignore_then(select!(Token::Number(count) => count))
            .map_with(|count, extra| DataField::ZeroInitialize {
                span: extra.span().into(),
                count,
            })
            .labelled("zero initialized field");
        zero_init.or(regular_field).labelled(Self::DESC)
    }
}
impl Display for DataField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DataField::Regular { span: _, ty, items } => {
                write!(f, "{ty}")?;
                for val in items {
                    write!(f, " {val}")?;
                }
                Ok(())
            }
            DataField::ZeroInitialize { span: _, count } => {
                write!(f, "z {count}")
            }
        }
    }
}
/// Represents the offset in a [`DataItem::SymbolRefWithOffset`].
///
/// This is an opaque type in case negative offsets are later added.
/// For now, the IR definition restricts it to nonnegative numbers.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
pub struct SymbolOffset(NumericLiteral<u64>);
impl SymbolOffset {
    pub fn unspanned<T: Number>(value: T) -> Self
    where
        Self: From<NumericLiteral<T>>,
    {
        NumericLiteral::unspanned(value).into()
    }
}
impl From<NumericLiteral<u64>> for SymbolOffset {
    fn from(value: NumericLiteral<u64>) -> Self {
        SymbolOffset(value)
    }
}
impl Parse for SymbolOffset {
    const DESC: &'static str = "symbol offset";

    fn parser<'a>() -> impl TokenParser<'a, Self> {
        select!(Token::Number(value) => SymbolOffset(value))
    }
}
impl Display for SymbolOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
#[non_exhaustive]
pub enum DataItem {
    /// A reference to a symbol with an explicit offset.
    ///
    /// If the offset is not present,
    /// this will instead be parsed as a [`Constant::SymbolRef`].
    SymbolRefWithOffset {
        span: Span,
        name: GlobalName,
        offset: SymbolOffset,
    },
    String(StringLiteral),
    Constant(Constant),
}
impl Parse for DataItem {
    const DESC: &'static str = "data item";

    fn parser<'a>() -> impl TokenParser<'a, Self> {
        let offset_symbol_ref = select!(Token::GlobalName(name) => name)
            .then_ignore(operator!(+).parser())
            .then(SymbolOffset::parser())
            .map_with(|(name, offset), extra| DataItem::SymbolRefWithOffset {
                span: extra.span().into(),
                name,
                offset,
            })
            .labelled("symbol and offset");
        choice((
            offset_symbol_ref,
            select!(Token::StringLiteral(literal) => DataItem::String(literal)),
            Constant::parser().map(DataItem::Constant),
        ))
        .labelled(Self::DESC)
    }
}
impl Display for DataItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataItem::SymbolRefWithOffset {
                span: _,
                name,
                offset,
            } => {
                write!(f, "{name} + {offset}")
            }
            DataItem::String(lit) => write!(f, "{lit}"),
            DataItem::Constant(value) => write!(f, "{value}"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
#[non_exhaustive]
pub enum Constant {
    Integer(NumericLiteral<i128>),
    Float(FloatLiteral),
    SymbolRef(GlobalName),
}
impl Parse for Constant {
    const DESC: &'static str = "constant";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        select! {
            Token::GlobalName(name) => Constant::SymbolRef(name),
            Token::Float(literal) => Constant::Float(literal),
            Token::Number(value) => Constant::Integer(value.map_value(i128::from)),
            Token::Integer(value) => Constant::Integer(value),
        }
        .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(Constant);
impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::Float(lit) => write!(f, "{lit}"),
            Constant::SymbolRef(symbol) => write!(f, "{symbol}"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::test_parse_print;
    use similar_asserts::assert_eq;

    fn integer(x: i128) -> Constant {
        Constant::Integer(NumericLiteral::unspanned(x))
    }

    macro_rules! test_constants {
        ($($text:literal => $value:expr),+ $(,)?) => {
            test_parse_print! {
                parse_constant, print_constant for Constant {
                    $($text => $value),*
                }
            }
            test_parse_print! {
                parse_constant_data_item, print_constant_data_item for DataItem {
                    $($text => DataItem::Constant($value)),*
                }
            }
        };
    }
    test_constants! {
        "128" => integer(128),
        "-128" => integer(-128),
        "d_8.7" => Constant::Float(FloatLiteral::double_unspanned(8.7)),
        "s_8.7" => Constant::Float(FloatLiteral::single_unspanned(8.7)),
        "$foo" => Constant::SymbolRef(GlobalName::unspanned("foo")),
    }
    test_parse_print! {
        // NOTE: tests for DataItem::Constant already handled above
        parse_data_item, print_data_item for DataItem {
            "$foo + 3" => DataItem::SymbolRefWithOffset {
                span: Span::MISSING,
                name: GlobalName::unspanned("foo"),
                offset: SymbolOffset::unspanned(3),
            },
            // must parse as regular constant, not as SymbolWithOffset
            "$foo" => DataItem::Constant(Constant::SymbolRef(GlobalName::unspanned("foo"))),
            "\"foo\"" => DataItem::String(StringLiteral::unspanned("foo")),
        }
    }
    test_parse_print! {
        parse_data_field, print_data_field for DataField {
            "w 12 $foo \"bar\"" => DataField::Regular {
                ty: ExtendedType::Word,
                span: Span::MISSING,
                items: vec![
                    DataItem::Constant(integer(12)),
                    DataItem::Constant(Constant::SymbolRef(GlobalName::unspanned("foo"))),
                    DataItem::String(StringLiteral::unspanned("bar")),
                ]
            },
            "z 80" => DataField::ZeroInitialize {
                span: Span::MISSING,
                count: NumericLiteral::unspanned(80),
            }
        }
    }
    test_parse_print! {
        parse_data_def, print_data_def for DataDef {
            "export data $example = align 16 {
                w 12 $foo,
                z 80,
                l \"bar\"
            }" => DataDef {
                linkage: Linkage::builder().with_export().build(),
                span: Span::MISSING,
                name: GlobalName::unspanned("example"),
                align: Some(AlignSpec::unspanned(16)),
                fields: vec![
                   DataField::Regular {
                        ty: ExtendedType::Word,
                        span: Span::MISSING,
                        items: vec![
                            DataItem::Constant(integer(12)),
                            DataItem::Constant(Constant::SymbolRef(GlobalName::unspanned("foo"))),
                        ]
                    },
                    DataField::ZeroInitialize {
                        span: Span::MISSING,
                        count: NumericLiteral::unspanned(80),
                    },
                    DataField::Regular {
                        ty: ExtendedType::Long,
                        span: Span::MISSING,
                        items: vec![DataItem::String(StringLiteral::unspanned("bar"))]
                    }
                ]
            }
        }
    }
}
