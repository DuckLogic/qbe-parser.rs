use crate::ast::data::Constant;
use crate::ast::functions::{
    CallArgument, CallInstruction, EnvironmentParamDef, FunctionBlock, FunctionBody, FunctionDef,
    InsnDestInfo, JumpInstruction, JumpInstructionKind, ParamDef, PhiArg, PhiInstruction,
    RegularCallArgument, RegularInstruction, RegularParamDef, SimpleInstruction, ThreadLocalRef,
    Value, VariadicParamDef,
};
use crate::ast::linkage::Linkage;
use crate::ast::types::{AbiType, BaseType};
use crate::ast::{BlockName, GlobalName, Ident, TemporaryName};
use crate::lexer::{Operator, Token, TokenParser, keyword, operator};
use crate::parse::{Parse, impl_fromstr_via_parse, maybe_newline};
use chumsky::prelude::*;

impl Parse for ThreadLocalRef {
    const DESC: &'static str = "thread-local ref";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        keyword!(thread)
            .parser()
            .ignore_then(GlobalName::parser())
            .map_with(|name, extra| ThreadLocalRef {
                name,
                span: extra.span(),
            })
    }
}
impl_fromstr_via_parse!(ThreadLocalRef);
impl Parse for Value {
    const DESC: &'static str = "value";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice((
            TemporaryName::parser().map(Value::Temporary),
            ThreadLocalRef::parser().map(Value::ThreadLocalRef),
            Constant::parser().map(Value::Constant),
        ))
        .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(Value);
impl JumpInstructionKind {
    /// Parse an exact match for only this specific kind of jump instruction,
    /// rejecting all others.
    fn parser_exact<'a>(&self) -> impl TokenParser<'a, JumpInstructionKind> {
        let expected = *self;
        <JumpInstructionKind as Parse>::parser().filter(move |kind| *kind == expected)
    }
}
impl Parse for JumpInstructionKind {
    const DESC: &'static str = "jump instruction name";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice([
            keyword!(jmp).parser().to(JumpInstructionKind::Jump),
            keyword!(jnz).parser().to(JumpInstructionKind::JumpNonZero),
            keyword!(ret).parser().to(JumpInstructionKind::Return),
            keyword!(hlt).parser().to(JumpInstructionKind::Halt),
        ])
        .labelled(Self::DESC)
    }
}
impl Parse for JumpInstruction {
    const DESC: &'static str = "jump instruction";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice((
            JumpInstructionKind::Jump
                .parser_exact()
                .ignore_then(BlockName::parser())
                .map_with(|target, extra| JumpInstruction::Jump {
                    target,
                    span: extra.span(),
                }),
            JumpInstructionKind::JumpNonZero
                .parser_exact()
                .ignore_then(Value::parser())
                .then_ignore(Operator::Comma.parser())
                .then(BlockName::parser())
                .then_ignore(Operator::Comma.parser())
                .then(BlockName::parser())
                .map_with(
                    |((op, target), fallthrough), extra| JumpInstruction::JumpNonZero {
                        op,
                        target,
                        fallthrough,
                        span: extra.span(),
                    },
                ),
            JumpInstructionKind::Return
                .parser_exact()
                .ignore_then(Value::parser().or_not())
                .map_with(|value, extra| JumpInstruction::Return {
                    span: extra.span(),
                    value,
                }),
            JumpInstructionKind::Halt
                .parser_exact()
                .map_with(|_kind, extra| JumpInstruction::Halt { span: extra.span() }),
        ))
        .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(JumpInstruction);

impl Parse for RegularParamDef {
    const DESC: &'static str = "regular parameter";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        AbiType::parser()
            .then(TemporaryName::parser())
            .map_with(|(ty, name), extra| RegularParamDef {
                ty,
                name,
                span: extra.span(),
            })
            .labelled(Self::DESC)
    }
}
impl Parse for EnvironmentParamDef {
    const DESC: &'static str = "environment parameter";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        keyword!(env)
            .parser()
            .ignore_then(TemporaryName::parser())
            .map_with(|name, extra| EnvironmentParamDef {
                name,
                span: extra.span(),
            })
            .labelled(Self::DESC)
    }
}
impl Parse for VariadicParamDef {
    const DESC: &'static str = "variadic parameter";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        operator!(...)
            .parser()
            .to_span()
            .map(|span| VariadicParamDef { span })
            .labelled(Self::DESC)
    }
}
impl Parse for ParamDef {
    const DESC: &'static str = "parameter definition";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice((
            RegularParamDef::parser().map(ParamDef::Regular),
            EnvironmentParamDef::parser().map(ParamDef::Environment),
            VariadicParamDef::parser().map(ParamDef::Variadic),
        ))
        .labelled(Self::DESC)
    }
}

impl Parse for FunctionDef {
    const DESC: &'static str = "function definition";

    fn parser<'a>() -> impl TokenParser<'a, Self> {
        let params = ParamDef::parser()
            .separated_by(operator!(,).parser())
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .labelled("function parameters");
        Linkage::parser()
            .then_ignore(keyword!(function).parser())
            .then(AbiType::parser().or_not().labelled("return type"))
            .then(GlobalName::parser())
            .then(params)
            .then_ignore(maybe_newline())
            .then(FunctionBody::parser())
            .labelled(Self::DESC)
            .map_with(
                |((((linkage, return_type), name), params), body), extra| FunctionDef {
                    span: extra.span(),
                    linkage,
                    name,
                    params,
                    body,
                    return_type,
                },
            )
            .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(FunctionDef);
impl Parse for FunctionBody {
    const DESC: &'static str = "function body";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        just(Token::OpenBrace)
            .then_ignore(just(Token::Newline))
            .ignore_then(
                FunctionBlock::parser()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::CloseBrace))
            .map_with(|blocks, extra| FunctionBody {
                blocks,
                span: extra.span(),
            })
    }
}

impl Parse for FunctionBlock {
    const DESC: &'static str = "block";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        BlockName::parser()
            .labelled("block label")
            .then_ignore(just(Token::Newline))
            .then(
                PhiInstruction::parser()
                    .then_ignore(just(Token::Newline))
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                RegularInstruction::parser()
                    .then_ignore(just(Token::Newline))
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                JumpInstruction::parser()
                    .then_ignore(just(Token::Newline))
                    .or_not(),
            )
            .map_with(
                |(((label, phis), instructions), terminator), extra| FunctionBlock {
                    span: extra.span(),
                    label,
                    instructions,
                    phis,
                    terminator,
                },
            )
    }
}
impl_fromstr_via_parse!(FunctionBlock);

impl Parse for PhiInstruction {
    const DESC: &'static str = "phi instruction";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        InsnDestInfo::parser()
            .then_ignore(keyword!(phi).parser())
            .then(
                PhiArg::parser()
                    .separated_by(operator!(,).parser())
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .map_with(|(dest_info, args), extra| PhiInstruction {
                span: extra.span(),
                dest_info,
                args,
            })
            .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(PhiInstruction);
impl Parse for PhiArg {
    const DESC: &'static str = "phi argument";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        BlockName::parser()
            .then(Value::parser())
            .map_with(|(block, value), extra| PhiArg {
                value,
                span: extra.span(),
                block,
            })
            .labelled(Self::DESC)
    }
}
impl Parse for RegularInstruction {
    const DESC: &'static str = "regular instruction";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice((
            SimpleInstruction::parser().map(RegularInstruction::Simple),
            CallInstruction::parser().map(RegularInstruction::Call),
        ))
        .labelled(Self::DESC)
    }
}
impl_fromstr_via_parse!(RegularInstruction);
impl Parse for SimpleInstruction {
    const DESC: &'static str = "simple instruction";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        let values = Value::parser()
            .separated_by(operator!(,).parser())
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>();
        InsnDestInfo::parser()
            .or_not()
            .then(Ident::parser())
            .then(values)
            .map_with(|((dest, name), args), extra| SimpleInstruction {
                span: extra.span(),
                dest_info: dest,
                name,
                args,
            })
    }
}
impl_fromstr_via_parse!(SimpleInstruction);
impl Parse for CallInstruction {
    const DESC: &'static str = "call instruction";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        let args = CallArgument::parser()
            .separated_by(operator!(,).parser())
            .collect::<Vec<_>>()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen));
        InsnDestInfo::parser()
            .or_not()
            .then(keyword!(call).parser().to_span())
            .then(Value::parser())
            .then(args)
            .map_with(
                |(((dest_info, call_kw_span), target), args), extra| CallInstruction {
                    span: extra.span(),
                    call_kw_span,
                    target,
                    args,
                    dest_info,
                },
            )
    }
}
impl Parse for CallArgument {
    const DESC: &'static str = "call argument";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        choice((
            RegularCallArgument::parser().map(CallArgument::Regular),
            keyword!(env)
                .parser()
                .ignore_then(Value::parser())
                .map(CallArgument::Environment),
            operator!(...)
                .parser()
                .to_span()
                .map(CallArgument::VariadicMarker),
        ))
    }
}
impl Parse for RegularCallArgument {
    const DESC: &'static str = "regular call argument";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        AbiType::parser()
            .then(Value::parser())
            .map_with(|(ty, value), extra| RegularCallArgument {
                ty,
                value,
                span: extra.span(),
            })
    }
}
impl Parse for InsnDestInfo {
    const DESC: &'static str = "instruction destination";
    fn parser<'a>() -> impl TokenParser<'a, Self> {
        TemporaryName::parser()
            .then_ignore(operator!(=).parser())
            .then(BaseType::parser())
            .map_with(|(dest, ty), extra| InsnDestInfo {
                dest,
                ty,
                span: extra.span(),
            })
    }
}
