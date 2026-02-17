//! Salsa-based incremental compilation database for the Min language.
//!
//! This crate integrates the lexer, parser, HIR lowering, and type checker
//! into an incremental computation framework using Salsa. When source text
//! changes, only the affected queries are recomputed.

use std::sync::Arc;

use min_diagnostics::Diagnostic;
use min_hir::check::TypeCheckResult;
use min_hir::hir::Module;
use min_hir::lower::LoweringContext;
use min_syntax::SourceFile;

// ── Salsa Input ─────────────────────────────────────────────────────────────

/// A source program input. Holds the source text for a single file.
#[salsa::input]
pub struct SourceProgram {
    #[returns(ref)]
    pub text: String,
}

// ── Intermediate Result Types ───────────────────────────────────────────────
//
// These types wrap compilation results in `Arc` and use pointer-based equality.
// When the source text hasn't changed, Salsa returns the cached result (same Arc).
// When it has changed, a new Arc is created and downstream queries re-execute.

/// The result of parsing a source file.
#[derive(Clone, Debug)]
pub struct ParseResult {
    pub ast: Arc<SourceFile>,
    pub diagnostics: Arc<Vec<Diagnostic>>,
}

impl PartialEq for ParseResult {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.ast, &other.ast) && Arc::ptr_eq(&self.diagnostics, &other.diagnostics)
    }
}
impl Eq for ParseResult {}

/// The result of lowering an AST to HIR.
#[derive(Clone, Debug)]
pub struct LowerResult {
    pub module: Arc<Module>,
    pub ctx: Arc<LoweringContext>,
    pub diagnostics: Arc<Vec<Diagnostic>>,
}

impl PartialEq for LowerResult {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.module, &other.module)
            && Arc::ptr_eq(&self.ctx, &other.ctx)
            && Arc::ptr_eq(&self.diagnostics, &other.diagnostics)
    }
}
impl Eq for LowerResult {}

/// The result of type-checking a module.
#[derive(Clone, Debug)]
pub struct CheckResult {
    pub result: Arc<TypeCheckResult>,
}

impl PartialEq for CheckResult {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.result, &other.result)
    }
}
impl Eq for CheckResult {}

/// All diagnostics collected from every compilation phase.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct AllDiagnostics {
    pub diagnostics: Vec<Diagnostic>,
}

// ── Tracked Query Functions ─────────────────────────────────────────────────

/// Parse source text into an AST. Cached and only re-executed when the
/// source text changes.
#[salsa::tracked]
pub fn parse_file(db: &dyn salsa::Database, source: SourceProgram) -> ParseResult {
    let text = source.text(db);
    let (ast, diagnostics) = min_syntax::parser::parse(text);
    ParseResult {
        ast: Arc::new(ast),
        diagnostics: Arc::new(diagnostics),
    }
}

/// Lower a parsed AST to HIR with name resolution. Depends on `parse_file`.
#[salsa::tracked]
pub fn lower_file(db: &dyn salsa::Database, source: SourceProgram) -> LowerResult {
    let parsed = parse_file(db, source);
    let mut ctx = LoweringContext::new();
    let module = ctx.lower_source_file(&parsed.ast);
    let diagnostics = std::mem::take(&mut ctx.diagnostics);
    LowerResult {
        module: Arc::new(module),
        ctx: Arc::new(ctx),
        diagnostics: Arc::new(diagnostics),
    }
}

/// Type-check a lowered module. Depends on `lower_file`.
#[salsa::tracked]
pub fn type_check_file(db: &dyn salsa::Database, source: SourceProgram) -> CheckResult {
    let lowered = lower_file(db, source);
    let result = min_hir::check::type_check(&lowered.module, &lowered.ctx);
    CheckResult {
        result: Arc::new(result),
    }
}

/// Collect all diagnostics from parsing, lowering, and type-checking.
/// This is the primary query for getting compilation errors.
#[salsa::tracked]
pub fn file_diagnostics(db: &dyn salsa::Database, source: SourceProgram) -> AllDiagnostics {
    let parsed = parse_file(db, source);
    let lowered = lower_file(db, source);
    let checked = type_check_file(db, source);

    let mut all = Vec::new();
    all.extend(parsed.diagnostics.iter().cloned());
    all.extend(lowered.diagnostics.iter().cloned());
    all.extend(checked.result.diagnostics.iter().cloned());

    AllDiagnostics { diagnostics: all }
}

// ── Database Implementation ─────────────────────────────────────────────────

/// The Min compiler database. Holds all Salsa storage for incremental
/// compilation queries.
#[salsa::db]
#[derive(Default, Clone)]
pub struct MinDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for MinDatabase {}

// ── Convenience API ─────────────────────────────────────────────────────────

impl MinDatabase {
    /// Create a new empty database.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new source program with the given text.
    pub fn set_source(&self, text: impl Into<String>) -> SourceProgram {
        SourceProgram::new(self, text.into())
    }

    /// Parse the source and return the AST with diagnostics.
    pub fn parse(&self, source: SourceProgram) -> ParseResult {
        parse_file(self, source)
    }

    /// Lower the source to HIR.
    pub fn lower(&self, source: SourceProgram) -> LowerResult {
        lower_file(self, source)
    }

    /// Type-check the source.
    pub fn check(&self, source: SourceProgram) -> CheckResult {
        type_check_file(self, source)
    }

    /// Get all diagnostics for the source.
    pub fn diagnostics(&self, source: SourceProgram) -> Vec<Diagnostic> {
        file_diagnostics(self, source).diagnostics
    }

    /// Full compilation pipeline: parse, lower, type-check, and return
    /// the module along with all diagnostics.
    pub fn compile(&self, source: SourceProgram) -> CompileResult {
        let parsed = self.parse(source);
        let lowered = self.lower(source);
        let checked = self.check(source);
        let diagnostics = self.diagnostics(source);

        CompileResult {
            ast: parsed.ast,
            module: lowered.module,
            ctx: lowered.ctx,
            type_info: checked.result,
            diagnostics,
        }
    }
}

/// The complete result of compiling a source file.
#[derive(Debug)]
pub struct CompileResult {
    pub ast: Arc<SourceFile>,
    pub module: Arc<Module>,
    pub ctx: Arc<LoweringContext>,
    pub type_info: Arc<TypeCheckResult>,
    pub diagnostics: Vec<Diagnostic>,
}

impl CompileResult {
    /// Returns true if there are no errors.
    pub fn is_ok(&self) -> bool {
        self.diagnostics
            .iter()
            .all(|d| d.severity != min_diagnostics::Severity::Error)
    }

    /// Returns only the error diagnostics.
    pub fn errors(&self) -> Vec<&Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == min_diagnostics::Severity::Error)
            .collect()
    }
}

#[cfg(test)]
mod tests;
