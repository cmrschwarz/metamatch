use proc_macro::{Delimiter, Span, TokenTree};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

pub struct MetaError {
    pub span: Span,
    pub message: String,
}

#[derive(Debug)]
pub struct BindingParameter {
    pub span: Span,
    pub name: Rc<str>,
    pub super_bound: bool,
}

pub struct BuiltinFn {
    pub param_count: usize,
    #[allow(clippy::type_complexity)]
    pub builtin: Box<
        dyn Fn(
            &mut Context,
            Span,
            &[Rc<MetaValue>],
        ) -> Result<Rc<MetaValue>, ()>,
    >,
}

#[derive(Clone, Debug)]
pub enum MetaValue {
    Token(TokenTree),
    Tokens(Vec<TokenTree>),
    Int {
        value: i64,
        // For literals that come straight from source code this
        // is set and will be used for the span in case occurs in output.
        // For 'generated' literals like from a computed expression or
        // an `enumerate` this is not set, and we use the span
        // of the final expression that caused the output instead.
        // Because this language is fuzzy about the types of things it can
        // happen that the user really intended a *token* but ended up
        // with an integer. In order to mimic the token semantics more closely
        // and prevent users from having to use `raw!` everwhere this
        // seems like a worthwile tradeoff.
        span: Option<Span>,
    },
    Float {
        value: f64,
        span: Option<Span>,
    },
    Bool {
        value: bool,
        span: Option<Span>, // same reasoning as `Int`
    },
    String {
        value: Rc<str>,
        span: Option<Span>, // same reasoning as `Int`
    },
    Fn(Rc<Function>),
    BuiltinFn(Rc<BuiltinFn>),
    List(Vec<Rc<MetaValue>>),
    Tuple(Vec<Rc<MetaValue>>),
}

#[derive(Debug)]
pub enum Pattern {
    Ident(BindingParameter),
    Tuple { span: Span, elems: Vec<Pattern> },
    List { span: Span, elems: Vec<Pattern> },
}

#[derive(Debug)]
pub struct Function {
    pub span: Span,
    pub name: Rc<str>,
    pub params: Vec<BindingParameter>,
    pub body: Vec<Rc<MetaExpr>>,
}

#[derive(Debug)]
pub struct ExpandPattern {
    pub span: Span,
    pub for_pattern: Pattern,
    pub for_expr: Rc<MetaExpr>,
    pub match_arm_patterns: Vec<Rc<MetaExpr>>,
    pub match_arm_guard: Vec<Rc<MetaExpr>>,
    pub match_arm_body: Vec<Rc<MetaExpr>>,
}

#[derive(Debug)]
pub enum MetaExpr {
    Literal {
        span: Span,
        value: Rc<MetaValue>,
    },
    Ident {
        span: Span,
        name: Rc<str>,
    },
    LetBinding {
        span: Span,
        pattern: Pattern,
        expr: Option<Rc<MetaExpr>>,
    },
    FnCall {
        span: Span,
        name: Rc<str>,
        args: Vec<Rc<MetaExpr>>,
    },
    FnDecl(Rc<Function>),
    RawOutputGroup {
        span: Span,
        delimiter: Delimiter,
        contents: Vec<Rc<MetaExpr>>,
    },
    IfExpr {
        span: Span,
        condition: Rc<MetaExpr>,
        body: Vec<Rc<MetaExpr>>,
        else_expr: Option<Rc<MetaExpr>>,
    },
    ForExpansion {
        span: Span,
        pattern: Pattern,
        variants_expr: Rc<MetaExpr>,
        body: Vec<Rc<MetaExpr>>,
    },
    // boxed cause large
    ExpandPattern(Box<ExpandPattern>),
    Scope {
        span: Span,
        body: Vec<Rc<MetaExpr>>,
    },
    List {
        span: Span,
        exprs: Vec<Rc<MetaExpr>>,
    },
    Tuple {
        span: Span,
        exprs: Vec<Rc<MetaExpr>>,
    },
    OpUnary {
        kind: UnaryOpKind,
        span: Span,
        operand: Rc<MetaExpr>,
    },
    OpBinary {
        kind: BinaryOpKind,
        span: Span,
        lhs: Rc<MetaExpr>,
        rhs: Rc<MetaExpr>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
    Plus,
    Minus,
    Not,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equals,
    RangeExclusive,
    RangeInclusive,
    Assign,
}

#[derive(Debug)]
pub struct Binding {
    #[allow(unused)] // Todo: im sure we will need this at some point?
    pub span: Span,
    // super bound bindings are available in quoted contexts
    pub super_bound: bool,
    pub value: Rc<MetaValue>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Raw,
    Quoted,
    Unquoted,
    Evaluation,
    Metamatch,
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub bindings: HashMap<Rc<str>, Binding>,
}

pub struct Context {
    pub empty_token_list: Rc<MetaValue>,
    pub empty_token_list_expr: Rc<MetaExpr>,
    pub scopes: Vec<Scope>,
    pub errors: Vec<MetaError>,
}

#[derive(PartialEq, Eq)]
pub enum TrailingBlockKind {
    For,
    If,
    Else,
    Let,
    Quote,
    Unquote,
    Raw,
    Fn,
}

impl Default for MetaValue {
    fn default() -> Self {
        Self::Tokens(Vec::new())
    }
}

impl Debug for BuiltinFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFn")
            .field("param_count", &self.param_count)
            .field("builtin", &"..")
            .finish()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Int,
    Float,
    Bool,
    String,

    Token,
    Tokens,

    Fn,

    List,
    Tuple,
}

impl MetaValue {
    pub fn kind(&self) -> Kind {
        match self {
            MetaValue::Token(_) => Kind::Token,
            MetaValue::Tokens(_) => Kind::Tokens,
            MetaValue::Int { .. } => Kind::Int,
            MetaValue::Float { .. } => Kind::Float,
            MetaValue::String { .. } => Kind::String,
            MetaValue::Bool { .. } => Kind::Bool,
            MetaValue::Fn(_) => Kind::Fn,
            MetaValue::BuiltinFn(_) => Kind::Fn,
            MetaValue::List(_) => Kind::List,
            MetaValue::Tuple(_) => Kind::Tuple,
        }
    }
}

impl Kind {
    fn to_str(self) -> &'static str {
        match self {
            Kind::Int => "int",
            Kind::Float => "float",
            Kind::Bool => "bool",
            Kind::String => "string",
            Kind::Token => "token",
            Kind::Tokens => "tokens",
            Kind::Fn => "fn",
            Kind::List => "list",
            Kind::Tuple => "tuple",
        }
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl UnaryOpKind {
    fn to_str(self) -> &'static str {
        match self {
            UnaryOpKind::Plus => "unary plus",
            UnaryOpKind::Minus => "unary minus",
            UnaryOpKind::Not => "unary not",
        }
    }
}

impl BinaryOpKind {
    pub fn to_str(self) -> &'static str {
        match self {
            BinaryOpKind::Add => "plus",
            BinaryOpKind::Sub => "minus",
            BinaryOpKind::Mul => "multiply",
            BinaryOpKind::Div => "divide",
            BinaryOpKind::Rem => "remainder",
            BinaryOpKind::Equals => "equals",
            BinaryOpKind::RangeExclusive => "exclusive range",
            BinaryOpKind::RangeInclusive => "inclusive range",
            BinaryOpKind::Assign => "assign",
        }
    }
    pub fn symbol(self) -> &'static str {
        match self {
            BinaryOpKind::Add => "+",
            BinaryOpKind::Sub => "-",
            BinaryOpKind::Mul => "*",
            BinaryOpKind::Div => "/",
            BinaryOpKind::Rem => "%",
            BinaryOpKind::Equals => "==",
            BinaryOpKind::RangeExclusive => "..",
            BinaryOpKind::RangeInclusive => "..=",
            BinaryOpKind::Assign => "=",
        }
    }
}

impl MetaExpr {
    pub fn span(&self) -> Span {
        *match self {
            MetaExpr::Literal { span, .. } => span,
            MetaExpr::Ident { span, .. } => span,
            MetaExpr::LetBinding { span, .. } => span,
            MetaExpr::FnCall { span, .. } => span,
            MetaExpr::ExpandPattern(ep) => &ep.span,
            MetaExpr::FnDecl(function) => &function.span,
            MetaExpr::RawOutputGroup { span, .. } => span,
            MetaExpr::IfExpr { span, .. } => span,
            MetaExpr::ForExpansion { span, .. } => span,
            MetaExpr::Scope { span, .. } => span,
            MetaExpr::List { span, .. } => span,
            MetaExpr::Tuple { span, .. } => span,
            MetaExpr::OpUnary { span, .. } => span,
            MetaExpr::OpBinary { span, .. } => span,
        }
    }
}

impl TrailingBlockKind {
    pub fn to_str(&self) -> &'static str {
        match self {
            TrailingBlockKind::For => "for",
            TrailingBlockKind::If => "if",
            TrailingBlockKind::Else => "else",
            TrailingBlockKind::Let => "let",
            TrailingBlockKind::Fn => "fn",
            TrailingBlockKind::Unquote => "unquote",
            TrailingBlockKind::Quote => "quote",
            TrailingBlockKind::Raw => "raw",
        }
    }
}
