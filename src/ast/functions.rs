pub use self::args::SimpleInstructionArgs;
use crate::ast::data::Constant;
use crate::ast::linkage::Linkage;
use crate::ast::types::{AbiType, BaseType};
use crate::ast::{BlockName, FloatLiteral, GlobalName, Ident, Span, Spanned, TemporaryName};
use crate::lexer::Keyword;
use crate::print::{IndentedPrinter, impl_display_via_print};
use crate::utils::{IterExt, delegate_enum_getters, impl_enum_display};
use std::fmt;
use std::fmt::{Display, Formatter, Write};
use std::str::FromStr;

mod args;
mod parse;
#[cfg(test)]
mod test;

#[derive(Clone, Debug, Eq, PartialEq, Hash, typed_builder::TypedBuilder)]
#[non_exhaustive]
pub struct FunctionDef {
    #[builder(default)]
    pub span: Span,
    #[builder(default)]
    pub linkage: Linkage,
    #[builder(default, setter(strip_option, into))]
    pub return_type: Option<AbiType>,
    pub name: GlobalName,
    #[builder(default, mutators(
        fn add_param(&mut self, param: impl Into<ParamDef>) {
            self.params.push(param.into());
        }
    ))]
    pub params: Vec<ParamDef>,
    pub body: FunctionBody,
}
impl FunctionDef {
    // dummy method for enum getter
    pub(crate) fn span(&self) -> Span {
        self.span
    }
    pub fn validate(&self) -> Result<(), Vec<InvalidFunctionReason>> {
        let mut res = Vec::new();
        for (index, param) in self.params.iter().enumerate() {
            match param {
                ParamDef::Regular(_) => {}
                ParamDef::Environment(_) => {
                    if index > 0 {
                        res.push(InvalidFunctionReason::EnvironmentParamMustComeFirst {
                            span: param.span(),
                        })
                    }
                }
                ParamDef::Variadic(_) => {
                    if index < self.params.len() - 1 {
                        res.push(InvalidFunctionReason::VariadicParamMustComeLast {
                            span: param.span(),
                        })
                    }
                }
            }
        }
        if res.is_empty() { Ok(()) } else { Err(res) }
    }
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        if !self.linkage.is_empty() {
            write!(out, "{} ", self.linkage)?;
        }
        out.write_str("function ")?;
        if let Some(ref return_type) = self.return_type {
            write!(out, "{return_type} ")?;
        }
        write!(out, "{}({}) ", self.name, self.params.iter().format(", "))?;
        self.body.print(out)?;
        Ok(())
    }
}
impl_display_via_print!(FunctionDef);

/// An error that occurs calling [`FunctionDef::validate`].
#[derive(Debug, Clone, Eq, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidFunctionReason {
    #[error("Variadic parameter must come last")]
    VariadicParamMustComeLast { span: Span },
    #[error("Environment parameter must come first")]
    EnvironmentParamMustComeFirst { span: Span },
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ParamDef {
    Regular(RegularParamDef),
    Environment(EnvironmentParamDef),
    Variadic(VariadicParamDef),
}
impl ParamDef {
    pub fn span(&self) -> Span {
        match self {
            ParamDef::Regular(param) => param.span,
            ParamDef::Environment(param) => param.span,
            ParamDef::Variadic(param) => param.span,
        }
    }
    pub fn name(&self) -> Result<&'_ TemporaryName, UnnamedParamError> {
        Ok(match self {
            ParamDef::Regular(param) => &param.name,
            ParamDef::Environment(param) => &param.name,
            ParamDef::Variadic(param) => {
                return Err(UnnamedParamError::Variadic { span: param.span });
            }
        })
    }
}
impl Display for ParamDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParamDef::Regular(param) => write!(f, "{param}"),
            ParamDef::Environment(param) => write!(f, "{param}"),
            ParamDef::Variadic(param) => write!(f, "{param}"),
        }
    }
}
#[derive(thiserror::Error, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum UnnamedParamError {
    #[error("Variadic parameter has no name")]
    Variadic { span: Span },
}
impl UnnamedParamError {
    pub fn span(&self) -> Span {
        match self {
            UnnamedParamError::Variadic { span } => *span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RegularParamDef {
    pub span: Span,
    pub name: TemporaryName,
    pub ty: AbiType,
}
impl Display for RegularParamDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct EnvironmentParamDef {
    pub span: Span,
    pub name: TemporaryName,
}
impl Display for EnvironmentParamDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "env {}", self.name)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct VariadicParamDef {
    pub span: Span,
}
impl Display for VariadicParamDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("...")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionBody {
    pub span: Span,
    pub blocks: Vec<FunctionBlock>,
}
impl FunctionBody {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        out.write_str("{\n")?;
        for block in &self.blocks {
            block.print(out)?;
        }
        out.maybe_writeln()?;
        out.write_char('}')
    }
}
impl_display_via_print!(FunctionBody);
#[derive(Clone, Debug, Eq, PartialEq, Hash, typed_builder::TypedBuilder)]
#[non_exhaustive]
pub struct FunctionBlock {
    #[builder(default)]
    pub span: Span,
    pub label: BlockName,
    #[builder(default)]
    pub phis: Vec<PhiInstruction>,
    #[builder(default)]
    pub instructions: Vec<RegularInstruction>,
    pub terminator: Option<JumpInstruction>,
}
impl FunctionBlock {
    fn print(&self, out: &mut IndentedPrinter<'_>) -> fmt::Result {
        writeln!(out, "{}", self.label)?;
        out.indented(|out| {
            for phi in &self.phis {
                writeln!(out, "{phi}")?;
            }
            for insn in &self.instructions {
                writeln!(out, "{insn}")?;
            }
            if let Some(ref term) = self.terminator {
                writeln!(out, "{term}")?;
            }
            Ok(())
        })
    }
}
impl_display_via_print!(FunctionBlock);
#[derive(Clone, Debug, Eq, PartialEq, Hash, typed_builder::TypedBuilder)]
pub struct PhiInstruction {
    pub span: Span,
    pub dest_info: InsnDestInfo,
    pub args: Vec<PhiArg>,
}
impl PhiInstruction {
    #[inline]
    pub fn dest(&self) -> &'_ TemporaryName {
        &self.dest_info.dest
    }
    #[inline]
    pub fn dest_type(&self) -> &'_ BaseType {
        &self.dest_info.ty
    }
}
impl Display for PhiInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} phi ", self.dest_info)?;
        write!(f, "{}", self.args.iter().format(", "))?;
        Ok(())
    }
}
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PhiArg {
    pub span: Span,
    pub block: BlockName,
    pub value: Value,
}
impl Display for PhiArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.block, self.value)
    }
}

/// The destination where the result of an instruction is stored.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InsnDestInfo {
    pub span: Span,
    pub dest: TemporaryName,
    pub ty: BaseType,
}
impl Display for InsnDestInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ={}", self.dest, self.ty)
    }
}
/// An instruction that is not a [`JumpInstruction`].
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum RegularInstruction {
    Simple(SimpleInstruction),
    Call(CallInstruction),
}
delegate_enum_getters! {
    enum RegularInstruction {
        Simple,
        Call
    } get {
        pub fn dest_info(&self) -> Option<&'_ InsnDestInfo>;
        pub fn dest(&self) -> Option<&'_ TemporaryName>;
        pub fn dest_type(&self) -> Option<&'_ BaseType>;
        pub fn name(&self) -> Ident;
        pub fn span(&self) -> Span;
    }
}
impl From<SimpleInstruction> for RegularInstruction {
    fn from(value: SimpleInstruction) -> Self {
        RegularInstruction::Simple(value)
    }
}
impl From<CallInstruction> for RegularInstruction {
    fn from(value: CallInstruction) -> Self {
        RegularInstruction::Call(value)
    }
}
impl_enum_display!(
    enum RegularInstruction {
        Simple,
        Call,
    }
);
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SimpleInstruction {
    pub span: Span,
    pub dest_info: Option<InsnDestInfo>,
    pub args: SimpleInstructionArgs,
    pub name: Ident,
}
impl SimpleInstruction {
    fn name(&self) -> Ident {
        self.name.clone()
    }
}
macro_rules! regular_insn_common {
    ($target:ident) => {
        impl $target {
            // this is an internal method, only needed for the macro
            fn dest_info(&self) -> Option<&'_ InsnDestInfo> {
                self.dest_info.as_ref()
            }
            #[inline]
            fn span(&self) -> Span {
                self.span
            }
            pub fn dest(&self) -> Option<&'_ TemporaryName> {
                self.dest_info.as_ref().map(|info| &info.dest)
            }
            pub fn dest_type(&self) -> Option<&'_ BaseType> {
                self.dest_info.as_ref().map(|info| &info.ty)
            }
        }
    };
}
regular_insn_common!(SimpleInstruction);
impl Display for SimpleInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ref info) = self.dest_info {
            write!(f, "{info} ")?;
        }
        write!(f, "{}", self.name)?;
        if !self.args.is_empty() {
            f.write_char(' ')?;
        }
        write!(f, "{}", self.args.iter().format(", "))?;
        Ok(())
    }
}
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CallInstruction {
    pub span: Span,
    /// Span of the "call" keyword, used for [`Self::name`].
    pub call_kw_span: Span,
    pub dest_info: Option<InsnDestInfo>,
    pub target: Value,
    pub args: Vec<CallArgument>,
}
impl Display for CallInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ref info) = self.dest_info {
            write!(f, "{info} ")?;
        }
        write!(f, "call {}({})", self.target, self.args.iter().format(", "))
    }
}
impl CallInstruction {
    pub fn name(&self) -> Ident {
        Spanned {
            span: self.call_kw_span,
            value: Keyword::Call,
        }
        .into()
    }
}
regular_insn_common!(CallInstruction);

/// An argument to a [`CallInstruction`].
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum CallArgument {
    Value(Value),
    Environment(Value),
    VariadicMarker(Span),
}
impl Display for CallArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CallArgument::Value(value) => Display::fmt(value, f),
            CallArgument::Environment(value) => write!(f, "env {value}"),
            CallArgument::VariadicMarker(_) => f.write_str("..."),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ThreadLocalRef {
    pub span: Span,
    pub name: GlobalName,
}
impl Display for ThreadLocalRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "thread {}", self.name)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum Value {
    Constant(Constant),
    ThreadLocalRef(ThreadLocalRef),
    Temporary(TemporaryName),
}
impl_enum_display!(
    enum Value {
        Constant,
        ThreadLocalRef,
        Temporary,
    }
);
macro_rules! impl_from_constant {
    ($($target:ty),+) => {
        $(impl From<$target> for Value {
            fn from(value: $target) -> Self {
                Value::Constant(value.into())
            }
        })*
    };
}
impl_from_constant!(Constant, i128, i64, i32, u64, FloatLiteral);
impl From<TemporaryName> for Value {
    fn from(name: TemporaryName) -> Self {
        Value::Temporary(name)
    }
}

macro_rules! insn_kind_names {
    ($target:ident {
        const KIND_DESC = $kind_desc:literal;
        $($variant:ident => $name:literal),+ $(,)?
    }) => {
        impl $target {
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $name,)*
                }
            }
            pub fn from_name(name: &str) -> Option<Self> {
                match name {
                    $($name => Some(Self::$variant),)*
                    _ => None,
                }
            }
        }
        impl FromStr for $target {
            type Err = UnknownInstructionNameError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Self::from_name(s).ok_or_else(|| UnknownInstructionNameError {
                    kind_desc: Some($kind_desc),
                    name: s.into()
                })
            }
        }
    };
}
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum JumpInstructionKind {
    Jump,
    JumpNonZero,
    Return,
    Halt,
}
insn_kind_names!(JumpInstructionKind {
    const KIND_DESC = "jump";
    Jump => "jmp",
    JumpNonZero => "jnz",
    Return => "ret",
    Halt => "hlt",
});
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum JumpInstruction {
    Jump {
        span: Span,
        target: BlockName,
    },
    JumpNonZero {
        span: Span,
        op: Value,
        target: BlockName,
        fallthrough: BlockName,
    },
    Return {
        span: Span,
        value: Option<Value>,
    },
    Halt {
        span: Span,
    },
}
impl JumpInstruction {
    #[inline]
    pub fn dest_info(&self) -> Option<&InsnDestInfo> {
        None
    }
    pub fn span(&self) -> Span {
        match *self {
            JumpInstruction::Jump { span, .. }
            | JumpInstruction::JumpNonZero { span, .. }
            | JumpInstruction::Return { span, .. }
            | JumpInstruction::Halt { span, .. } => span,
        }
    }
    pub fn kind(self) -> JumpInstructionKind {
        match self {
            JumpInstruction::Jump { .. } => JumpInstructionKind::Jump,
            JumpInstruction::JumpNonZero { .. } => JumpInstructionKind::JumpNonZero,
            JumpInstruction::Return { .. } => JumpInstructionKind::Return,
            JumpInstruction::Halt { .. } => JumpInstructionKind::Halt,
        }
    }
}
impl Display for JumpInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpInstruction::Jump { span: _, target } => {
                write!(f, "jmp {}", target)
            }
            JumpInstruction::JumpNonZero {
                span: _,
                op,
                fallthrough,
                target,
            } => {
                write!(f, "jnz {op}, {target}, {fallthrough}")
            }
            JumpInstruction::Return { span: _, value } => {
                f.write_str("ret")?;
                if let Some(value) = value {
                    write!(f, " {value}")?;
                }
                Ok(())
            }
            JumpInstruction::Halt { span: _ } => f.write_str("hlt"),
        }
    }
}

#[derive(thiserror::Error, Debug, Clone, Eq, PartialEq)]
pub struct UnknownInstructionNameError {
    kind_desc: Option<&'static str>,
    name: String,
}
impl UnknownInstructionNameError {
    pub fn name(&self) -> &'_ str {
        &self.name
    }
}
impl Display for UnknownInstructionNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Unknown")?;
        if let Some(kind) = self.kind_desc {
            write!(f, " {}", kind)?;
        }
        write!(f, " instruction name: {:?}", self.name)
    }
}
