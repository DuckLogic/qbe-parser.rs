#![allow(clippy::unused_unit, reason = "part of macros")]
use crate::ast::types::*;
use crate::lexer::{Token, TokenParser, keyword, operator};
use crate::parse::{Parse, impl_fromstr_via_parse, spanned};

use chumsky::prelude::*;

impl Parse for AlignSpec {
    const DESC: &'static str = "alignment spec";
    fn parser<'a>() -> impl TokenParser<'a, AlignSpec> {
        keyword!(align)
            .parser()
            .ignore_then(select!(Token::Number(number) => number))
            .map_with(|value, extra| AlignSpec {
                value,
                span: extra.span().into(),
            })
            .labelled(Self::DESC)
    }
}

impl Parse for TypeDef {
    const DESC: &'static str = "type definition";

    fn parser<'a>() -> impl TokenParser<'a, TypeDef> {
        keyword!(type)
            .parser()
            .ignore_then(select!(Token::TypeName(name) => name))
            .then_ignore(operator!(=).parser())
            .then(AlignSpec::parser().or_not())
            .then(TypeDefBody::parser())
            .map_with(|((name, align), body), extra| TypeDef {
                span: extra.span().into(),
                body,
                align,
                name,
            })
            .validate(|td, extra, emitter| {
                if let Err(errors) = td.validate() {
                    for e in errors {
                        emitter.emit(Rich::custom(extra.span(), e));
                    }
                }
                td
            })
            .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(TypeDef);

impl Parse for TypeDefBody {
    const DESC: &'static str = "type body";

    fn parser<'a>() -> impl TokenParser<'a, TypeDefBody> {
        choice((
            OpaqueBody::parser().map(TypeDefBody::Opaque),
            StructBody::parser().map(TypeDefBody::Struct),
            UnionBody::parser().map(TypeDefBody::Union),
        ))
        .labelled("typedef body")
    }
}
impl Parse for OpaqueBody {
    const DESC: &'static str = "opaque type body";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        select!(Token::Number(n) => n)
            .delimited_by(select!(Token::OpenBrace(_)), select!(Token::CloseBrace(_)))
            .map_with(spanned)
            .map(|Spanned { value: size, span }| OpaqueBody { span, size })
            .labelled(Self::DESC)
    }
}
impl Parse for StructBody {
    const DESC: &'static str = "struct body";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        FieldDef::parser()
            .separated_by(operator!(,).parser())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(select!(Token::OpenBrace(_)), select!(Token::CloseBrace(_)))
            .map_with(|fields, extra| StructBody {
                span: extra.span().into(),
                fields,
            })
            .labelled(Self::DESC)
    }
}
impl Parse for UnionBody {
    const DESC: &'static str = "union body";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        StructBody::parser()
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(select!(Token::OpenBrace(_)), select!(Token::CloseBrace(_)))
            .map_with(|variants, extra| UnionBody {
                span: extra.span().into(),
                variants,
            })
            .labelled("struct body")
    }
}
impl Parse for FieldDef {
    const DESC: &'static str = "field definition";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        FieldType::parser()
            .then(select!(Token::Number(n) => n).or_not())
            .map_with(|(ty, repeated), extra| FieldDef {
                span: extra.span().into(),
                ty,
                repeated,
            })
            .labelled(Self::DESC)
    }
}
impl Parse for FieldType {
    const DESC: &'static str = "field type";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        select!(Token::TypeName(name) => FieldType::Named(name))
            .or(ExtendedType::parser()
                .map_with(|tp, extra| FieldType::Extended(tp, extra.span().into())))
            .labelled("field type")
    }
}
impl_fromstr_via_parse!(FieldType);

macro_rules! simple_type_parser {
    ($($target:ident { DESC = $desc:literal }),+ $(,)?) => {
        $(impl Parse for $target {
            const DESC: &'static str = $desc;
            fn parser<'a>() -> impl TokenParser<'a, Self> {
                select!(Token::ShortTypeSpec(spec, _) => spec)
                    .try_map(|spec, span| spec.try_into().map_err(|reason| {
                        Rich::custom(span, reason)
                    }))
                    .labelled(Self::DESC)
            }
        })*
    };
}
simple_type_parser!(
    BaseType { DESC = "base type" },
    ExtendedType { DESC = "extended type" },
);

#[cfg(test)]
mod test {}
