use crate::ast::data::Constant;
use crate::ast::functions::{
    CallArgument, CallInstruction, FunctionBlock, FunctionBody, FunctionDef, InsnDestInfo,
    JumpInstruction, ParamDef, PhiArg, PhiInstruction, RegularCallArgument, RegularInstruction,
    RegularParamDef, SimpleInstruction, Value, VariadicParamDef,
};
use crate::ast::linkage::Linkage;
use crate::ast::types::BaseType;
use crate::ast::{BlockName, GlobalName, Ident, Span, TemporaryName};
use indoc::indoc;
use similar_asserts::assert_eq;

fn loop_func() -> FunctionDef {
    FunctionDef {
        span: Span::MISSING,
        name: GlobalName::unspanned("loop"),
        params: Vec::new(),
        linkage: Linkage::default(),
        body: FunctionBody {
            span: Span::MISSING,
            blocks: vec![
                FunctionBlock {
                    label: BlockName::unspanned("start"),
                    terminator: None,
                    instructions: Vec::new(),
                    phis: Vec::new(),
                    span: Span::MISSING,
                },
                FunctionBlock {
                    span: Span::MISSING,
                    label: BlockName::unspanned("loop"),
                    phis: vec![PhiInstruction {
                        span: Span::MISSING,
                        dest_info: InsnDestInfo {
                            span: Span::MISSING,
                            dest: TemporaryName::unspanned("x"),
                            ty: BaseType::Word,
                        },
                        args: vec![
                            PhiArg {
                                span: Span::MISSING,
                                block: BlockName::unspanned("start"),
                                value: Value::from(100),
                            },
                            PhiArg {
                                span: Span::MISSING,
                                block: BlockName::unspanned("loop"),
                                value: TemporaryName::unspanned("x1").into(),
                            },
                        ],
                    }],
                    instructions: vec![RegularInstruction::Simple(SimpleInstruction {
                        span: Span::MISSING,
                        dest_info: Some(InsnDestInfo {
                            span: Span::MISSING,
                            dest: TemporaryName::unspanned("x1"),
                            ty: BaseType::Word,
                        }),
                        args: vec![TemporaryName::unspanned("x").into(), Value::from(1)],
                        name: Ident::unspanned("sub"),
                    })],
                    terminator: Some(JumpInstruction::JumpNonZero {
                        span: Span::MISSING,
                        target: BlockName::unspanned("loop"),
                        fallthrough: BlockName::unspanned("end"),
                        op: TemporaryName::unspanned("x1").into(),
                    }),
                },
                FunctionBlock {
                    span: Span::MISSING,
                    label: BlockName::unspanned("end"),
                    instructions: Vec::new(),
                    phis: Vec::new(),
                    terminator: Some(JumpInstruction::Return {
                        value: None,
                        span: Span::MISSING,
                    }),
                },
            ],
        },
        return_type: None,
    }
}

const LOOP_IR: &str = indoc!(
    "
    function $loop() {
    @start
    @loop
        %x =w phi @start 100, @loop %x1
        %x1 =w sub %x, 1
        jnz %x1, @loop, @end
    @end
        ret
    }"
);
#[test]
fn parse_loop() {
    assert_eq!(LOOP_IR.parse::<FunctionDef>().unwrap(), loop_func(),);
}
#[test]
fn print_loop() {
    assert_eq!(loop_func().to_string(), LOOP_IR,);
}

fn variadic_add3() -> FunctionDef {
    FunctionDef {
        span: Span::MISSING,
        name: GlobalName::unspanned("add3"),
        return_type: Some(BaseType::Single.into()),
        linkage: Linkage::default(),
        params: vec![
            ParamDef::Regular(RegularParamDef {
                ty: BaseType::Single.into(),
                name: TemporaryName::unspanned("a"),
                span: Span::MISSING,
            }),
            ParamDef::Variadic(VariadicParamDef {
                span: Span::MISSING,
            }),
        ],
        body: FunctionBody {
            span: Span::MISSING,
            blocks: vec![FunctionBlock {
                span: Span::MISSING,
                label: BlockName::unspanned("start"),
                phis: Vec::new(),
                instructions: vec![
                    SimpleInstruction {
                        span: Span::MISSING,
                        name: Ident::unspanned("alloc8"),
                        dest_info: Some(InsnDestInfo {
                            span: Span::MISSING,
                            ty: BaseType::Long,
                            dest: TemporaryName::unspanned("ap"),
                        }),
                        args: vec![32.into()],
                    }
                    .into(),
                    SimpleInstruction {
                        span: Span::MISSING,
                        dest_info: None,
                        name: Ident::unspanned("vastart"),
                        args: vec![TemporaryName::unspanned("ap").into()],
                    }
                    .into(),
                    CallInstruction {
                        span: Span::MISSING,
                        dest_info: Some(InsnDestInfo {
                            span: Span::MISSING,
                            ty: BaseType::Single,
                            dest: TemporaryName::unspanned("r"),
                        }),
                        target: Value::Constant(Constant::SymbolRef(GlobalName::unspanned("vadd"))),
                        call_kw_span: Span::MISSING,
                        args: vec![
                            CallArgument::Regular(RegularCallArgument {
                                ty: BaseType::Single.into(),
                                value: TemporaryName::unspanned("a").into(),
                                span: Span::MISSING,
                            }),
                            CallArgument::Regular(RegularCallArgument {
                                ty: BaseType::Long.into(),
                                value: TemporaryName::unspanned("ap").into(),
                                span: Span::MISSING,
                            }),
                        ],
                    }
                    .into(),
                ],
                terminator: Some(JumpInstruction::Return {
                    value: Some(TemporaryName::unspanned("r").into()),
                    span: Span::MISSING,
                }),
            }],
        },
    }
}
const VARIADIC_ADD3_IR: &str = indoc!(
    "
    function s $add3(s %a, ...) {
    @start
        %ap =l alloc8 32
        vastart %ap
        %r =s call $vadd(s %a, l %ap)
        ret %r
    }"
);
#[test]
fn parse_variadic_add3() {
    assert_eq!(
        VARIADIC_ADD3_IR.parse::<FunctionDef>().unwrap(),
        variadic_add3(),
    );
}
#[test]
fn print_variadic_add3() {
    assert_eq!(VARIADIC_ADD3_IR.to_string(), VARIADIC_ADD3_IR,);
}
