use proc_macro::{Delimiter, Ident, Span, TokenTree};
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

#[derive(Debug, Clone)]
pub struct ExprBlock {
    pub span: Span,
    pub stmts: Vec<Rc<MetaExpr>>,
    pub trailing_semi: bool,
}

#[derive(Debug, Clone)]
pub struct BindingParameter {
    pub ident: MetaIdent,
    pub mutable: bool,
    pub super_bound: bool,
}

pub enum EvalError {
    PatternMissmatch,
    Error,
    Return {
        span: Span,
        value: Option<Rc<MetaValue>>,
    },
    Break {
        span: Span,
        value: Option<Rc<MetaValue>>,
    },
    Continue,
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
        ) -> Result<Rc<MetaValue>, EvalError>,
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
    Range {
        start: Option<i64>,
        end: Option<i64>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(BindingParameter),
    Tuple { span: Span, elems: Vec<Pattern> },
    List { span: Span, elems: Vec<Pattern> },
}

#[derive(Debug)]
pub struct Function {
    pub ident: MetaIdent,
    pub visibility: Visibility,
    pub params: Vec<Pattern>,
    pub body: ExprBlock,
}

#[derive(Debug)]
pub struct Lambda {
    pub span: Span,
    pub params: Vec<Pattern>,
    pub body: Rc<MetaExpr>,
}

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Extern,
    PubExtern,
    Regular,
}

#[derive(Clone, Debug)]
pub struct UsePath {
    pub leading_double_colon: bool,
    pub segments: Vec<MetaIdent>,
}

#[derive(Clone, Debug)]
pub struct UseReplacement {
    pub ident: MetaIdent,
    pub target_path: UsePath,
    pub binding: Rc<str>,
}

#[derive(Debug)]
pub enum UseTree {
    /// `some::path`
    Path {
        span: Span,
        path: UsePath,
        replacement: Rc<UseReplacement>,
    },
    /// `some::path::{item1, item2}`
    Group {
        span: Span,
        path: UsePath,
        items: Vec<UseTree>,
    },
    /// `some::path as other_name`
    Rename {
        span: Span,
        path: UsePath,
        alias: Rc<str>,
        replacement: Rc<UseReplacement>,
    },
}

#[derive(Debug)]
pub struct UseDecl {
    pub span: Span,
    pub tree: UseTree,
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

/// Part of a format string: either literal text or an expression to be
/// evaluated
#[derive(Debug)]
pub enum FormatPart {
    /// Literal string segment
    Literal(String),
    /// Expression to be formatted as string
    Expr(Rc<MetaExpr>),
}

#[derive(Debug, Clone)]
pub struct MetaIdent {
    pub name: Rc<str>,
    pub raw: bool,
    pub span: Span,
}

#[derive(Debug)]
pub enum MetaExpr {
    Break {
        span: Span,
        expr: Option<Rc<MetaExpr>>,
    },
    Return {
        span: Span,
        expr: Option<Rc<MetaExpr>>,
    },
    Continue {
        span: Span,
    },
    Literal {
        span: Span,
        value: Rc<MetaValue>,
        from_raw_block: bool,
    },
    Ident(MetaIdent),
    LetBinding {
        visibility: Visibility,
        span: Span,
        pattern: Pattern,
        expr: Option<Rc<MetaExpr>>,
    },
    UseDecl(Rc<UseDecl>),
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
        body: ExprBlock,
        else_expr: Option<Rc<MetaExpr>>,
    },
    For {
        span: Span,
        pattern: Pattern,
        variants_expr: Rc<MetaExpr>,
        body: ExprBlock,
    },
    Loop {
        span: Span,
        body: ExprBlock,
    },
    While {
        condition: Rc<MetaExpr>,
        span: Span,
        body: ExprBlock,
    },
    WhileLet {
        pattern: Pattern,
        expr: Rc<MetaExpr>,
        span: Span,
        body: ExprBlock,
    },
    Parenthesized {
        span: Span,
        expr: Rc<MetaExpr>,
    },
    // boxed cause large
    ExpandPattern(Box<ExpandPattern>),
    Block(ExprBlock),
    Group {
        span: Span,
        delimiter: Delimiter,
        body: ExprBlock,
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
    // start..end, start..=end, start.., ..end, ..
    Range {
        span: Span,
        start: Option<Rc<MetaExpr>>,
        end: Option<Rc<MetaExpr>>,
        inclusive: bool,
    },
    /// format!("..{expr}..")
    FormatString {
        span: Span,
        parts: Vec<FormatPart>,
    },
    /// assert!(condition) or assert!(condition, "msg {expr}")
    Assert {
        span: Span,
        condition: Rc<MetaExpr>,
        /// If present, format string parts for the message
        message: Option<Vec<FormatPart>>,
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
}

#[derive(Debug)]
pub struct Binding {
    pub ident: MetaIdent,
    // super bound bindings are available in quoted contexts
    pub super_bound: bool,
    pub mutable: bool,

    pub value: Rc<MetaValue>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Builtin,
    Raw,
    Group,
    Template,
    Quote,
    Eval,
    Metamatch,
    Lambda,
    // not `eval`, but used during execution of the program
    Evaluation,
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub bindings: HashMap<Rc<str>, Binding>,
}

pub enum ExternDecl {
    Fn {
        decl: Rc<Function>,
        quoted: Vec<TokenTree>,
    },
    Let {
        span: Span,
        visibility: Visibility,
        bindings: Pattern,
        values: Rc<MetaValue>,
    },
}

pub struct Context {
    pub empty_token_list: Rc<MetaValue>,
    pub empty_token_list_expr: Rc<MetaExpr>,
    pub scopes: Vec<Scope>,
    pub errors: Vec<MetaError>,
    pub extern_decls: Vec<ExternDecl>,
    pub extern_uses: Vec<Rc<UseReplacement>>,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TrailingBlockKind {
    For,
    While,
    Loop,
    If,
    Else,
    Let,
    Template,
    Eval,
    Raw,
    Fn,
    Call,
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
    ExternSymbol,

    List,
    Tuple,
    Range,
}

impl MetaIdent {
    pub fn to_ident(&self) -> Ident {
        if self.raw {
            Ident::new_raw(&self.name, self.span)
        } else {
            Ident::new(&self.name, self.span)
        }
    }
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
            MetaValue::Range { .. } => Kind::Range,
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
            MetaValue::Fn(function) => Some(function.ident.span),
            MetaValue::Lambda(lambda) => Some(lambda.span),
            MetaValue::BuiltinFn(_) => None,
            MetaValue::List(_) => None,
            MetaValue::Tuple(_) => None,
            MetaValue::Range { .. } => None,
        }
    }

    pub fn span(&self) -> Span {
        self.get_span().unwrap_or_else(Span::call_site)
    }
}

impl Kind {
    fn to_str(self) -> &'static str {
        match self {
            Kind::ExternSymbol => "extern_symbol",
            Kind::Int => "int",
            Kind::Float => "float",
            Kind::Bool => "bool",
            Kind::String => "str",
            Kind::Char => "char",
            Kind::Token => "token",
            Kind::Tokens => "tokens",
            Kind::Fn => "fn",
            Kind::List => "list",
            Kind::Tuple => "tuple",
            Kind::Range => "range",
        }
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl Default for ExprBlock {
    fn default() -> Self {
        Self {
            span: Span::call_site(),
            stmts: Default::default(),
            trailing_semi: Default::default(),
        }
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
            // we use prec 0 to differentiate between top level statements
            // and nested ones inside parentheses
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
            MetaExpr::Break { span, .. } => span,
            MetaExpr::Return { span, .. } => span,
            MetaExpr::Continue { span, .. } => span,
            MetaExpr::Literal { span, .. } => span,
            MetaExpr::Ident(ident) => &ident.span,
            MetaExpr::LetBinding { span, .. } => span,
            MetaExpr::UseDecl(use_decl) => &use_decl.span,
            MetaExpr::Call { span, .. } => span,
            MetaExpr::ExpandPattern(ep) => &ep.span,
            MetaExpr::FnDecl(function) => &function.ident.span,
            MetaExpr::Lambda(lambda) => &lambda.span,
            MetaExpr::RawOutputGroup { span, .. } => span,
            MetaExpr::IfExpr { span, .. } => span,
            MetaExpr::For { span, .. } => span,
            MetaExpr::Loop { span, .. } => span,
            MetaExpr::Block(block) => &block.span,
            MetaExpr::List { span, .. } => span,
            MetaExpr::Tuple { span, .. } => span,
            MetaExpr::OpUnary { span, .. } => span,
            MetaExpr::OpBinary { span, .. } => span,
            MetaExpr::ListAccess { span, .. } => span,
            MetaExpr::Parenthesized { span, .. } => span,
            MetaExpr::While { span, .. } => span,
            MetaExpr::WhileLet { span, .. } => span,
            MetaExpr::Group { span, .. } => span,
            MetaExpr::Range { span, .. } => span,
            MetaExpr::FormatString { span, .. } => span,
            MetaExpr::Assert { span, .. } => span,
        }
    }
    pub fn kind_str(&self) -> &'static str {
        match self {
            MetaExpr::Break { .. } => "break",
            MetaExpr::Return { .. } => "return",
            MetaExpr::Continue { .. } => "continue",
            MetaExpr::Literal { .. } => "literal",
            MetaExpr::Ident { .. } => "identifier",
            MetaExpr::LetBinding { .. } => "let binding",
            MetaExpr::UseDecl { .. } => "use declaration",
            MetaExpr::Call { .. } => "function call",
            MetaExpr::FnDecl { .. } => "function declaration",
            MetaExpr::Lambda { .. } => "lambda",
            MetaExpr::RawOutputGroup { .. } => "token tree",
            MetaExpr::Group { .. } => "group expression",
            MetaExpr::IfExpr { .. } => "if expression",
            MetaExpr::For { .. } => "for loop",
            MetaExpr::Loop { .. } => "loop",
            MetaExpr::While { .. } => "while loop",
            MetaExpr::WhileLet { .. } => "while loop",
            MetaExpr::ExpandPattern { .. } => "expand pattern",
            MetaExpr::Block { .. } => "scope",
            MetaExpr::List { .. } => "list",
            MetaExpr::Tuple { .. } => "tuple",
            MetaExpr::OpUnary { .. } => "unary operator",
            MetaExpr::OpBinary { .. } => "binary operator",
            MetaExpr::ListAccess { .. } => "property access",
            MetaExpr::Parenthesized { .. } => "parentheses",
            MetaExpr::Range { .. } => "range",
            MetaExpr::FormatString { .. } => "format string",
            MetaExpr::Assert { .. } => "assert",
        }
    }
    pub fn may_drop_semicolon(&self) -> bool {
        match self {
            MetaExpr::FnDecl(..)
            | MetaExpr::For { .. }
            | MetaExpr::Loop { .. }
            | MetaExpr::While { .. }
            | MetaExpr::WhileLet { .. }
            | MetaExpr::IfExpr { .. }
            | MetaExpr::RawOutputGroup { .. }
            | MetaExpr::Group { .. }
            | MetaExpr::Break { .. }
            | MetaExpr::Continue { .. }
            | MetaExpr::Return { .. } => true,

            MetaExpr::Literal { from_raw_block, .. } => *from_raw_block,

            MetaExpr::Ident { .. }
            | MetaExpr::Call { .. }
            | MetaExpr::Lambda(..)
            | MetaExpr::ExpandPattern(..)
            | MetaExpr::Block { .. }
            | MetaExpr::List { .. }
            | MetaExpr::Tuple { .. }
            | MetaExpr::OpUnary { .. }
            | MetaExpr::OpBinary { .. }
            | MetaExpr::ListAccess { .. }
            | MetaExpr::Parenthesized { .. }
            | MetaExpr::LetBinding { .. }
            | MetaExpr::UseDecl { .. }
            | MetaExpr::Range { .. }
            | MetaExpr::FormatString { .. }
            | MetaExpr::Assert { .. } => false,
        }
    }

    /// Returns true if this expression terminates expression parsing
    /// (i.e., binary operators should not be parsed after it).
    /// This is different from may_drop_semicolon - literals from raw blocks
    /// can still be followed by binary operators.
    pub fn terminates_expression_parsing(&self) -> bool {
        match self {
            MetaExpr::FnDecl(..)
            | MetaExpr::For { .. }
            | MetaExpr::Loop { .. }
            | MetaExpr::While { .. }
            | MetaExpr::WhileLet { .. }
            | MetaExpr::IfExpr { .. }
            | MetaExpr::RawOutputGroup { .. }
            | MetaExpr::Group { .. }
            | MetaExpr::Break { .. }
            | MetaExpr::Continue { .. }
            | MetaExpr::Return { .. } => true,

            // Unlike may_drop_semicolon, literals from raw blocks
            // do NOT terminate expression parsing - they can be followed
            // by binary operators like `raw!(5) + x`
            MetaExpr::Literal { .. }
            | MetaExpr::Ident { .. }
            | MetaExpr::Call { .. }
            | MetaExpr::Lambda(..)
            | MetaExpr::ExpandPattern(..)
            | MetaExpr::Block { .. }
            | MetaExpr::List { .. }
            | MetaExpr::Tuple { .. }
            | MetaExpr::OpUnary { .. }
            | MetaExpr::OpBinary { .. }
            | MetaExpr::ListAccess { .. }
            | MetaExpr::Parenthesized { .. }
            | MetaExpr::LetBinding { .. }
            | MetaExpr::UseDecl { .. }
            | MetaExpr::Range { .. }
            | MetaExpr::FormatString { .. }
            | MetaExpr::Assert { .. } => false,
        }
    }
}

impl TrailingBlockKind {
    pub fn to_str(self) -> &'static str {
        match self {
            TrailingBlockKind::For => "for",
            TrailingBlockKind::While => "while",
            TrailingBlockKind::Loop => "loop",
            TrailingBlockKind::If => "if",
            TrailingBlockKind::Else => "else",
            TrailingBlockKind::Let => "let",
            TrailingBlockKind::Fn => "fn",
            TrailingBlockKind::Eval => "eval",
            TrailingBlockKind::Template => "quote",
            TrailingBlockKind::Raw => "raw",
            TrailingBlockKind::Call => "",
        }
    }
}

impl Visibility {
    pub fn is_extern(&self) -> bool {
        matches!(self, Visibility::Extern | Visibility::PubExtern)
    }

    pub fn is_pub(&self) -> bool {
        matches!(self, Visibility::PubExtern)
    }
}
