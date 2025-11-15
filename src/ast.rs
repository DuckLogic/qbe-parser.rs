#[macro_use]
mod macros;
mod core;
pub mod data;
pub mod functions;
pub mod linkage;
pub mod span;
pub mod types;

use crate::ast::data::DataDef;
use crate::ast::functions::FunctionDef;
use crate::ast::types::TypeDef;
use crate::lexer::TokenParser;
use crate::parse::{Parse, impl_fromstr_via_parse};
use crate::utils::{IterExt, delegate_enum_getters, impl_enum_display};
use chumsky::prelude::*;
use std::fmt::Display;

pub use core::*;
pub use span::{Location, Span};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum Item {
    FunctionDef(FunctionDef),
    DataDef(DataDef),
    TypeDef(TypeDef),
}
delegate_enum_getters! {
    enum Item {
        FunctionDef,
        DataDef,
        TypeDef,
    } get {
        pub fn span(&self) -> Span;
    }
}
impl Parse for Item {
    const DESC: &'static str = "top-level item";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice((
            FunctionDef::parser().map(Item::FunctionDef),
            DataDef::parser().map(Item::DataDef),
            TypeDef::parser().map(Item::TypeDef),
        ))
    }
}
impl_fromstr_via_parse!(Item);
impl_enum_display!(
    enum Item {
        FunctionDef,
        DataDef,
        TypeDef,
    }
);

/// A file of QBE IR, containing a series of [items](Item).
pub struct File {
    pub span: Span,
    pub items: Vec<Item>,
}
impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.items.iter().format("\n\n"))
    }
}
impl_fromstr_via_parse!(File);
impl Parse for File {
    const DESC: &'static str = "QBE IR file";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        Item::parser()
            .repeated()
            .collect()
            .map_with(|items, extra| File {
                span: extra.span(),
                items,
            })
    }
}
