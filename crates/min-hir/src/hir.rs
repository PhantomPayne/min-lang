//! High-level Intermediate Representation (HIR) for the Min language.
//!
//! The HIR is a desugared representation of the AST with:
//! - All names resolved to definition IDs
//! - All type expressions resolved to concrete types
//! - Arena-based expression storage per function body

use min_diagnostics::Span;
use min_syntax::BinOp;
use min_types::Type;

/// Unique identifier for a definition (function, type alias, parameter, local variable).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

/// A resolved Min module (one per source file).
#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
    pub type_aliases: Vec<TypeAlias>,
}

/// A resolved function definition.
#[derive(Debug)]
pub struct Function {
    pub def_id: DefId,
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Body,
    pub span: Span,
}

/// A resolved function parameter.
#[derive(Debug)]
pub struct Param {
    pub def_id: DefId,
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// A resolved type alias.
#[derive(Debug)]
pub struct TypeAlias {
    pub def_id: DefId,
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Arena-based storage for expressions within a function body.
#[derive(Debug)]
pub struct Body {
    exprs: Vec<ExprData>,
    spans: Vec<Span>,
    /// The root expression of the body (the function's block).
    pub root: ExprId,
}

impl Body {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            spans: Vec::new(),
            root: ExprId(0),
        }
    }

    /// Allocate a new expression in the arena, returning its ID.
    pub fn alloc(&mut self, expr: ExprData, span: Span) -> ExprId {
        let id = ExprId(self.exprs.len() as u32);
        self.exprs.push(expr);
        self.spans.push(span);
        id
    }

    /// Get the expression data for an ID.
    pub fn expr(&self, id: ExprId) -> &ExprData {
        &self.exprs[id.0 as usize]
    }

    /// Get the source span for an expression.
    pub fn span(&self, id: ExprId) -> Span {
        self.spans[id.0 as usize]
    }

    /// Total number of expressions in the body.
    pub fn len(&self) -> usize {
        self.exprs.len()
    }
}

/// An index into the body's expression arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

/// HIR expression data.
#[derive(Debug)]
pub enum ExprData {
    /// Integer literal.
    IntLit(i64),
    /// Float literal.
    FloatLit(f64),
    /// String literal.
    StringLit(String),
    /// Boolean literal.
    BoolLit(bool),
    /// String interpolation (list of literal/expression parts).
    StringInterp(Vec<StringInterpPart>),
    /// Reference to a local variable or parameter.
    Var(DefId),
    /// Field access: `expr.field`.
    FieldAccess {
        expr: ExprId,
        field: String,
    },
    /// Struct literal: `{ x: 1, y: 2 }`.
    StructLit {
        fields: Vec<(String, ExprId)>,
    },
    /// Function call: `func(a: 1, b: 2)`.
    Call {
        func: DefId,
        args: Vec<(String, ExprId)>,
    },
    /// If expression with optional else.
    If {
        cond: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    },
    /// Block expression with statements and optional tail.
    Block {
        stmts: Vec<Stmt>,
        tail: Option<ExprId>,
    },
    /// Binary operation.
    Binary {
        op: BinOp,
        left: ExprId,
        right: ExprId,
    },
    /// Unary operation.
    Unary {
        op: UnaryOp,
        expr: ExprId,
    },
    /// Return expression.
    Return {
        value: Option<ExprId>,
    },
    /// Error placeholder for unresolved/invalid expressions.
    Missing,
}

/// A part of a string interpolation.
#[derive(Debug)]
pub enum StringInterpPart {
    /// Literal text segment.
    Lit(String),
    /// Interpolated expression.
    Expr(ExprId),
}

/// Unary operator (mirrors AST but kept separate for HIR independence).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Arithmetic negation: `-x`
    Neg,
    /// Logical negation: `not x`
    Not,
}

/// A statement within a block.
#[derive(Debug)]
pub enum Stmt {
    /// Let binding: `let x: T = expr;`
    Let {
        def_id: DefId,
        name: String,
        ty: Option<Type>,
        value: ExprId,
        span: Span,
    },
    /// Expression statement: `expr;`
    Expr {
        expr: ExprId,
        span: Span,
    },
}
