//! Abstract Syntax Tree (AST) for the Min language.
//!
//! The AST represents the semantic structure of a Min program,
//! stripped of whitespace, comments, and other trivia.

use min_diagnostics::Span;

/// A complete Min source file
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub items: Vec<Item>,
    pub span: Span,
}

/// A top-level item in a source file
#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    TypeAlias(TypeAlias),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Function(f) => f.span,
            Item::TypeAlias(t) => t.span,
        }
    }
}

/// A function definition
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub attributes: Vec<Attribute>,
    pub doc: Option<DocString>,
    pub params: Vec<Param>,
    pub return_type: TypeExpr,
    pub body: Block,
    pub span: Span,
}

/// A type alias definition: `type Name = Type`
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: Ident,
    pub ty: TypeExpr,
    pub span: Span,
}

/// A function parameter
#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
    pub ty: TypeExpr,
    pub default: Option<Expr>,
    pub attributes: Vec<Attribute>,
    pub doc: Option<DocString>,
    pub span: Span,
}

/// An attribute: `@name` or `@name: value`
#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Ident,
    pub value: Option<AttrValue>,
    pub span: Span,
}

/// An attribute value
#[derive(Debug, Clone)]
pub enum AttrValue {
    Ident(Ident),
    String(String, Span),
    Int(i64, Span),
    Bool(bool, Span),
}

impl AttrValue {
    pub fn span(&self) -> Span {
        match self {
            AttrValue::Ident(id) => id.span,
            AttrValue::String(_, span) => *span,
            AttrValue::Int(_, span) => *span,
            AttrValue::Bool(_, span) => *span,
        }
    }
}

/// A documentation string
#[derive(Debug, Clone)]
pub struct DocString {
    pub content: String,
    pub span: Span,
}

/// A type expression
#[derive(Debug, Clone)]
pub enum TypeExpr {
    /// Primitive or named type: `int`, `float`, `bool`, `string`, `Point`
    Named(Ident),
    /// Generic type: `List<T>`, `Option<T>`
    Generic {
        name: Ident,
        args: Vec<TypeExpr>,
        span: Span,
    },
    /// Struct type: `{ x: int, y: int }`
    Struct {
        fields: Vec<StructField>,
        span: Span,
    },
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Named(id) => id.span,
            TypeExpr::Generic { span, .. } => *span,
            TypeExpr::Struct { span, .. } => *span,
        }
    }
}

/// A field in a struct type
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Ident,
    pub ty: TypeExpr,
    pub span: Span,
}

/// An expression
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal: `42`
    IntLit(i64, Span),
    /// Float literal: `3.14`
    FloatLit(f64, Span),
    /// String literal: `"hello"`
    StringLit(String, Span),
    /// Interpolated string: `"hello {name}"`
    StringInterp(StringInterp),
    /// Boolean literal: `true`, `false`
    BoolLit(bool, Span),
    /// Variable reference: `x`
    Ident(Ident),
    /// Field access: `point.x`
    FieldAccess(Box<FieldAccess>),
    /// Struct expression: `{ x: 1, y: 2 }`
    Struct(StructExpr),
    /// Function call: `add(a: 1, b: 2)`
    Call(CallExpr),
    /// If expression: `if cond { ... } else { ... }`
    If(Box<IfExpr>),
    /// Block expression: `{ let x = 1; x + 2 }`
    Block(Block),
    /// Binary expression: `a + b`
    Binary(Box<BinaryExpr>),
    /// Unary expression: `-x`, `not flag`
    Unary(Box<UnaryExpr>),
    /// Return expression: `return value`
    Return(Box<ReturnExpr>),
    /// Error placeholder for error recovery
    Error(Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::IntLit(_, span) => *span,
            Expr::FloatLit(_, span) => *span,
            Expr::StringLit(_, span) => *span,
            Expr::StringInterp(s) => s.span,
            Expr::BoolLit(_, span) => *span,
            Expr::Ident(id) => id.span,
            Expr::FieldAccess(fa) => fa.span,
            Expr::Struct(s) => s.span,
            Expr::Call(c) => c.span,
            Expr::If(i) => i.span,
            Expr::Block(b) => b.span,
            Expr::Binary(b) => b.span,
            Expr::Unary(u) => u.span,
            Expr::Return(r) => r.span,
            Expr::Error(span) => *span,
        }
    }
}

/// Interpolated string parts
#[derive(Debug, Clone)]
pub struct StringInterp {
    pub parts: Vec<StringPart>,
    pub span: Span,
}

/// A part of an interpolated string
#[derive(Debug, Clone)]
pub enum StringPart {
    /// Literal text portion
    Lit(String, Span),
    /// Interpolated expression: `{expr}`
    Expr(Expr, Span),
}

/// Field access expression: `expr.field`
#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub expr: Expr,
    pub field: Ident,
    pub span: Span,
}

/// Struct expression: `{ x: 1, y: 2 }`
#[derive(Debug, Clone)]
pub struct StructExpr {
    pub fields: Vec<FieldInit>,
    pub span: Span,
}

/// Field initialization: `name: expr`
#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

/// Function call: `func(arg1: val1, arg2: val2)`
#[derive(Debug, Clone)]
pub struct CallExpr {
    pub function: Ident,
    pub args: Vec<NamedArg>,
    pub span: Span,
}

/// Named argument: `name: expr`
#[derive(Debug, Clone)]
pub struct NamedArg {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

/// If expression
#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_block: Block,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

/// The else branch of an if expression
#[derive(Debug, Clone)]
pub enum ElseBranch {
    /// `else { ... }`
    Block(Block),
    /// `else if ... { ... }`
    ElseIf(Box<IfExpr>),
}

/// A block: `{ stmt*; expr? }`
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail_expr: Option<Box<Expr>>,
    pub span: Span,
}

/// A statement
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Let binding: `let x: Type = expr;`
    Let(LetStmt),
    /// Expression statement: `expr;`
    Expr(Expr, Span),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(l) => l.span,
            Stmt::Expr(_, span) => *span,
        }
    }
}

/// Let statement: `let name: type = expr;`
#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: Ident,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
    pub span: Span,
}

/// Binary expression: `left op right`
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: BinOp,
    pub right: Expr,
    pub span: Span,
}

/// Binary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    Mod,      // %
    Eq,       // ==
    Neq,      // !=
    Lt,       // <
    Gt,       // >
    Lte,      // <=
    Gte,      // >=
    And,      // and
    Or,       // or
}

impl BinOp {
    /// Returns the precedence level (higher = tighter binding)
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::Eq | BinOp::Neq => 3,
            BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => 4,
            BinOp::Add | BinOp::Sub => 5,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 6,
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lte => write!(f, "<="),
            BinOp::Gte => write!(f, ">="),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

/// Unary expression: `op expr`
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Expr,
    pub span: Span,
}

/// Unary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg, // -
    Not, // not
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "not"),
        }
    }
}

/// Return expression: `return expr`
#[derive(Debug, Clone)]
pub struct ReturnExpr {
    pub value: Option<Expr>,
    pub span: Span,
}

/// An identifier with its source span
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
