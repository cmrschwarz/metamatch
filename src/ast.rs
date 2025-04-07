use proc_macro::{Delimiter, Span, TokenTree};
use std::{
    cell::RefCell,
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
    pub mutable: bool,
    pub super_bound: bool,
}

pub struct BuiltinFn {
    pub name: Rc<str>,
    pub param_count: Option<usize>,
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
    Char {
        value: char,
        span: Option<Span>, // same reasoning as `Int`
    },
    String {
        value: Rc<str>,
        span: Option<Span>, // same reasoning as `Int`
    },
    Fn(Rc<Function>),
    Lambda(Rc<Lambda>),
    BuiltinFn(Rc<BuiltinFn>),
    List(RefCell<Vec<Rc<MetaValue>>>),
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
    pub params: Vec<Pattern>,
    pub body: Vec<Rc<MetaExpr>>,
}

#[derive(Debug)]
pub struct Lambda {
    pub span: Span,
    pub params: Vec<Pattern>,
    pub body: Rc<MetaExpr>,
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
    Call {
        span: Span,
        lhs: Rc<MetaExpr>,
        args: Vec<Rc<MetaExpr>>,
    },
    FnDecl(Rc<Function>),
    Lambda(Rc<Lambda>),
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
    // foo[..]
    ListAccess {
        span: Span,
        list: Rc<MetaExpr>,
        index: Rc<MetaExpr>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
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

    BinaryAnd,
    BinaryOr,
    BinaryXor,
    ShiftLeft,
    ShiftRight,

    LogicalAnd,
    LogicalOr,

    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    Assign,

    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,

    BinaryAndAssign,
    BinaryOrAssign,
    BinaryXorAssign,

    ShiftLeftAssign,
    ShiftRightAssign,

    RangeExclusive,
    RangeInclusive,
}

#[derive(Debug)]
pub struct Binding {
    #[allow(unused)] // Todo: im sure we will need this at some point?
    pub span: Span,
    // super bound bindings are available in quoted contexts
    pub super_bound: bool,
    pub mutable: bool,
    pub value: Rc<MetaValue>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Builtin,
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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
    Char,

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
            MetaValue::Char { .. } => Kind::Char,
            MetaValue::Bool { .. } => Kind::Bool,
            MetaValue::Fn(_) => Kind::Fn,
            MetaValue::Lambda(_) => Kind::Fn,
            MetaValue::BuiltinFn(_) => Kind::Fn,
            MetaValue::List(_) => Kind::List,
            MetaValue::Tuple(_) => Kind::Tuple,
        }
    }
    pub fn get_span(&self) -> Option<Span> {
        match self {
            MetaValue::Token(token_tree) => Some(token_tree.span()),
            MetaValue::Tokens(token_trees) => {
                token_trees.first().map(|tt| tt.span())
            }
            MetaValue::Int { span, .. } => *span,
            MetaValue::Float { span, .. } => *span,
            MetaValue::Bool { span, .. } => *span,
            MetaValue::Char { span, .. } => *span,
            MetaValue::String { span, .. } => *span,
            MetaValue::Fn(function) => Some(function.span),
            MetaValue::Lambda(lambda) => Some(lambda.span),
            MetaValue::BuiltinFn(_) => None,
            MetaValue::List(_) => None,
            MetaValue::Tuple(_) => None,
        }
    }

    pub fn span(&self) -> Span {
        self.get_span().unwrap_or_else(Span::call_site)
    }
}

impl Kind {
    fn to_str(self) -> &'static str {
        match self {
            Kind::Int => "int",
            Kind::Float => "float",
            Kind::Bool => "bool",
            Kind::String => "string",
            Kind::Char => "char",
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
    pub fn to_str(self) -> &'static str {
        match self {
            UnaryOpKind::Minus => "unary minus",
            UnaryOpKind::Not => "unary not",
        }
    }

    pub fn symbol(self) -> &'static str {
        match self {
            UnaryOpKind::Minus => "-",
            UnaryOpKind::Not => "!",
        }
    }

    pub fn precedence(self) -> u8 {
        match self {
            UnaryOpKind::Minus => 12,
            UnaryOpKind::Not => 12,
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
            BinaryOpKind::Equal => "equals",
            BinaryOpKind::RangeExclusive => "exclusive range",
            BinaryOpKind::RangeInclusive => "inclusive range",
            BinaryOpKind::Assign => "assign",
            BinaryOpKind::NotEqual => "not equal",
            BinaryOpKind::BinaryAnd => "binary and",
            BinaryOpKind::BinaryOr => "binary or",
            BinaryOpKind::BinaryXor => "binary xor",
            BinaryOpKind::LogicalAnd => "logical and",
            BinaryOpKind::LogicalOr => "logical or",
            BinaryOpKind::LessThan => "less than",
            BinaryOpKind::LessThanOrEqual => "less than or equal",
            BinaryOpKind::GreaterThan => "greater than",
            BinaryOpKind::GreaterThanOrEqual => "greater than or equal",
            BinaryOpKind::ShiftLeft => "shift left",
            BinaryOpKind::ShiftRight => "shift right",
            BinaryOpKind::AddAssign => "add assign",
            BinaryOpKind::SubAssign => "sub assign",
            BinaryOpKind::MulAssign => "mul assign",
            BinaryOpKind::DivAssign => "div assign",
            BinaryOpKind::RemAssign => "rem assign",
            BinaryOpKind::BinaryAndAssign => "binary and assign",
            BinaryOpKind::BinaryOrAssign => "binary or assign",
            BinaryOpKind::BinaryXorAssign => "binary xor assign",
            BinaryOpKind::ShiftLeftAssign => "shift left assign",
            BinaryOpKind::ShiftRightAssign => "shift right assign",
        }
    }
    pub fn symbol(self) -> &'static str {
        match self {
            BinaryOpKind::Add => "+",
            BinaryOpKind::Sub => "-",
            BinaryOpKind::Mul => "*",
            BinaryOpKind::Div => "/",
            BinaryOpKind::Rem => "%",
            BinaryOpKind::Equal => "==",
            BinaryOpKind::RangeExclusive => "..",
            BinaryOpKind::RangeInclusive => "..=",
            BinaryOpKind::Assign => "=",
            BinaryOpKind::NotEqual => "!=",
            BinaryOpKind::BinaryAnd => "&",
            BinaryOpKind::BinaryOr => "|",
            BinaryOpKind::BinaryXor => "^",
            BinaryOpKind::LogicalAnd => "&&",
            BinaryOpKind::LogicalOr => "||",
            BinaryOpKind::LessThan => "<",
            BinaryOpKind::LessThanOrEqual => "<=",
            BinaryOpKind::GreaterThan => ">",
            BinaryOpKind::GreaterThanOrEqual => ">=",
            BinaryOpKind::ShiftLeft => "<<",
            BinaryOpKind::ShiftRight => ">>",
            BinaryOpKind::AddAssign => "+=",
            BinaryOpKind::SubAssign => "-=",
            BinaryOpKind::MulAssign => "*=",
            BinaryOpKind::DivAssign => "/=",
            BinaryOpKind::RemAssign => "%=",
            BinaryOpKind::BinaryAndAssign => "&=",
            BinaryOpKind::BinaryOrAssign => "|=",
            BinaryOpKind::BinaryXorAssign => "^=",
            BinaryOpKind::ShiftLeftAssign => "<<=",
            BinaryOpKind::ShiftRightAssign => ">>=",
        }
    }
    pub fn precedence(self) -> u8 {
        match self {
            BinaryOpKind::Mul | BinaryOpKind::Div | BinaryOpKind::Rem => 11,
            BinaryOpKind::Add | BinaryOpKind::Sub => 10,
            BinaryOpKind::ShiftLeft | BinaryOpKind::ShiftRight => 9,
            BinaryOpKind::BinaryAnd => 8,
            BinaryOpKind::BinaryXor => 7,
            BinaryOpKind::BinaryOr => 6,
            BinaryOpKind::Equal
            | BinaryOpKind::NotEqual
            | BinaryOpKind::LessThan
            | BinaryOpKind::GreaterThan
            | BinaryOpKind::LessThanOrEqual
            | BinaryOpKind::GreaterThanOrEqual => 5,
            BinaryOpKind::LogicalAnd => 4,
            BinaryOpKind::LogicalOr => 3,
            BinaryOpKind::RangeExclusive | BinaryOpKind::RangeInclusive => 2,
            BinaryOpKind::Assign
            | BinaryOpKind::AddAssign
            | BinaryOpKind::SubAssign
            | BinaryOpKind::MulAssign
            | BinaryOpKind::DivAssign
            | BinaryOpKind::RemAssign
            | BinaryOpKind::BinaryAndAssign
            | BinaryOpKind::BinaryOrAssign
            | BinaryOpKind::BinaryXorAssign
            | BinaryOpKind::ShiftLeftAssign
            | BinaryOpKind::ShiftRightAssign => 1,
        }
    }

    pub fn is_right_associative(self) -> bool {
        match self {
            BinaryOpKind::Add
            | BinaryOpKind::Sub
            | BinaryOpKind::Mul
            | BinaryOpKind::Div
            | BinaryOpKind::Rem
            | BinaryOpKind::Equal
            | BinaryOpKind::RangeExclusive
            | BinaryOpKind::RangeInclusive
            | BinaryOpKind::NotEqual
            | BinaryOpKind::BinaryAnd
            | BinaryOpKind::BinaryOr
            | BinaryOpKind::BinaryXor
            | BinaryOpKind::LogicalAnd
            | BinaryOpKind::LogicalOr
            | BinaryOpKind::LessThan
            | BinaryOpKind::LessThanOrEqual
            | BinaryOpKind::GreaterThan
            | BinaryOpKind::GreaterThanOrEqual
            | BinaryOpKind::ShiftLeft
            | BinaryOpKind::ShiftRight => false,

            BinaryOpKind::Assign
            | BinaryOpKind::AddAssign
            | BinaryOpKind::SubAssign
            | BinaryOpKind::MulAssign
            | BinaryOpKind::DivAssign
            | BinaryOpKind::RemAssign
            | BinaryOpKind::BinaryAndAssign
            | BinaryOpKind::BinaryOrAssign
            | BinaryOpKind::BinaryXorAssign
            | BinaryOpKind::ShiftLeftAssign
            | BinaryOpKind::ShiftRightAssign => true,
        }
    }

    pub fn non_assigning_version(self) -> Option<BinaryOpKind> {
        match self {
            BinaryOpKind::Add
            | BinaryOpKind::Sub
            | BinaryOpKind::Mul
            | BinaryOpKind::Div
            | BinaryOpKind::Rem
            | BinaryOpKind::Equal
            | BinaryOpKind::NotEqual
            | BinaryOpKind::RangeExclusive
            | BinaryOpKind::RangeInclusive
            | BinaryOpKind::BinaryAnd
            | BinaryOpKind::BinaryOr
            | BinaryOpKind::BinaryXor
            | BinaryOpKind::LogicalAnd
            | BinaryOpKind::LogicalOr
            | BinaryOpKind::LessThan
            | BinaryOpKind::LessThanOrEqual
            | BinaryOpKind::GreaterThan
            | BinaryOpKind::GreaterThanOrEqual
            | BinaryOpKind::ShiftLeft
            | BinaryOpKind::ShiftRight => None,
            BinaryOpKind::Assign => None,
            BinaryOpKind::AddAssign => Some(BinaryOpKind::Add),
            BinaryOpKind::SubAssign => Some(BinaryOpKind::Sub),
            BinaryOpKind::MulAssign => Some(BinaryOpKind::Mul),
            BinaryOpKind::DivAssign => Some(BinaryOpKind::Div),
            BinaryOpKind::RemAssign => Some(BinaryOpKind::Rem),
            BinaryOpKind::BinaryAndAssign => Some(BinaryOpKind::BinaryAnd),
            BinaryOpKind::BinaryOrAssign => Some(BinaryOpKind::BinaryOr),
            BinaryOpKind::BinaryXorAssign => Some(BinaryOpKind::BinaryXor),
            BinaryOpKind::ShiftLeftAssign => Some(BinaryOpKind::ShiftLeft),
            BinaryOpKind::ShiftRightAssign => Some(BinaryOpKind::ShiftRight),
        }
    }
}

impl MetaExpr {
    pub fn span(&self) -> Span {
        *match self {
            MetaExpr::Literal { span, .. } => span,
            MetaExpr::Ident { span, .. } => span,
            MetaExpr::LetBinding { span, .. } => span,
            MetaExpr::Call { span, .. } => span,
            MetaExpr::ExpandPattern(ep) => &ep.span,
            MetaExpr::FnDecl(function) => &function.span,
            MetaExpr::Lambda(lambda) => &lambda.span,
            MetaExpr::RawOutputGroup { span, .. } => span,
            MetaExpr::IfExpr { span, .. } => span,
            MetaExpr::ForExpansion { span, .. } => span,
            MetaExpr::Scope { span, .. } => span,
            MetaExpr::List { span, .. } => span,
            MetaExpr::Tuple { span, .. } => span,
            MetaExpr::OpUnary { span, .. } => span,
            MetaExpr::OpBinary { span, .. } => span,
            MetaExpr::ListAccess { span, .. } => span,
        }
    }
    pub fn kind_str(&self) -> &'static str {
        match self {
            MetaExpr::Literal { .. } => "literal",
            MetaExpr::Ident { .. } => "identifier",
            MetaExpr::LetBinding { .. } => "let binding",
            MetaExpr::Call { .. } => "function call",
            MetaExpr::FnDecl { .. } => "function declaration",
            MetaExpr::Lambda { .. } => "lambda",
            MetaExpr::RawOutputGroup { .. } => "token tree",
            MetaExpr::IfExpr { .. } => "if expression",
            MetaExpr::ForExpansion { .. } => "for loop",
            MetaExpr::ExpandPattern { .. } => "expand pattern",
            MetaExpr::Scope { .. } => "scope",
            MetaExpr::List { .. } => "list",
            MetaExpr::Tuple { .. } => "tuple",
            MetaExpr::OpUnary { .. } => "unary operator",
            MetaExpr::OpBinary { .. } => "binary operator",
            MetaExpr::ListAccess { .. } => "property access",
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
