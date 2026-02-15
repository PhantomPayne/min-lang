# Min Compiler Architecture

## Overview

The Min compiler is built on a query-based architecture using Salsa for incremental computation. The same query system is shared between the compiler and Language Server Protocol (LSP) implementation.

**Key principles:**
- Query-based incremental computation (Salsa)
- Error-tolerant parsing
- Shared IR between compiler and LSP
- Extensible design for future language forks

## Architecture Diagram

```
Source Code (.min files)
    ↓
┌─────────────────────────────────────────┐
│ Parser (error-tolerant)                 │
│ - Lexer                                 │
│ - Parser (resilient to errors)         │
└─────────────────────────────────────────┘
    ↓
CST (Concrete Syntax Tree)
    - Preserves all tokens, whitespace, comments
    - Used by: LSP (formatting, syntax highlighting)
    ↓
┌─────────────────────────────────────────┐
│ CST → AST Lowering                      │
│ - Strip whitespace/comments             │
│ - Build structural representation       │
└─────────────────────────────────────────┘
    ↓
AST (Abstract Syntax Tree)
    - Structure only, no types yet
    - Used by: Basic analysis
    ↓
┌─────────────────────────────────────────┐
│ Desugaring (future)                     │
│ - Pattern matching → match expressions  │
│ - For loops → while loops               │
│ - Macros (if added)                     │
└─────────────────────────────────────────┘
    ↓
HIR (High-level IR)
    - Still has complex expressions
    - Ready for type checking
    ↓
┌─────────────────────────────────────────┐
│ Type Inference & Checking               │
│ - Infer local variable types            │
│ - Check structural type equality        │
│ - Validate function signatures          │
└─────────────────────────────────────────┘
    ↓
Typed HIR
    - Every expression has a type
    - Used by: Custom lint rules, IDE features
    ↓
┌─────────────────────────────────────────┐
│ HIR → MIR Lowering                      │
│ - Desugar complex expressions           │
│ - Explicit control flow                 │
│ - Simple operations only                │
└─────────────────────────────────────────┘
    ↓
MIR (Mid-level IR)
    - Simple, desugared form
    - Ready for optimizations
    ↓
┌─────────────────────────────────────────┐
│ Optimizations                           │
│ - Constant folding                      │
│ - Dead code elimination                 │
│ - Inlining (future)                     │
└─────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────┐
│ WIT Generation                          │
│ - Extract exported types/functions      │
│ - Generate WIT interface definitions    │
│ - Canonical type naming                 │
└─────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────┐
│ WASM Component Generation               │
│ - Lower MIR to WASM                     │
│ - Generate component wrapper            │
│ - Link with stdlib                      │
└─────────────────────────────────────────┘
    ↓
WASM Component (.wasm)
```

## IR Layer Details

### CST (Concrete Syntax Tree)

**Purpose:** Preserve all source information for LSP

**Contents:**
- All tokens (keywords, identifiers, literals, operators)
- Whitespace and newlines
- Comments
- Exact source spans

**Used by:**
- Syntax highlighting
- Formatting (preserving user style)
- Comment preservation for hover tooltips
- Error recovery and diagnostics

**Example structure:**

```rust
enum CstNode {
    SourceFile {
        items: Vec<CstNode>,
        span: Span,
    },
    Function {
        fn_token: Token,
        name: Token,
        colon: Token,
        attributes: Vec<CstNode>,
        docstring: Option<Token>,
        params: Vec<CstNode>,
        arrow: Token,
        return_type: CstNode,
        body: CstNode,
        span: Span,
    },
    // ... other nodes
}
```

### AST (Abstract Syntax Tree)

**Purpose:** Structural representation without source artifacts

**Contents:**
- Program structure (functions, types, expressions)
- No whitespace or comments
- Logical structure only

**Used by:**
- Type checking preparation
- Initial semantic analysis
- Name resolution

**Example structure:**

```rust
struct SourceFile {
    items: Vec<Item>,
}

enum Item {
    Function(Function),
    TypeAlias(TypeAlias),
}

struct Function {
    name: Ident,
    attributes: Vec<Attribute>,
    doc: Option<String>,
    params: Vec<Param>,
    return_type: Type,
    body: Block,
}

struct Param {
    name: Ident,
    ty: Type,
    default: Option<Expr>,
    doc: Option<String>,
    attributes: Vec<Attribute>,
}

enum Expr {
    Literal(Literal),
    Ident(Ident),
    Struct(StructExpr),
    Call(CallExpr),
    If(IfExpr),
    Block(Block),
    Binary(BinaryExpr),
    // ...
}
```

### HIR (High-level IR)

**Purpose:** Pre-type-checked representation

**Differences from AST:**
- Fully desugared
- Name resolution complete
- String interpolation expanded

**Example transformations:**

```min
// Before (AST)
"Hello, {name}"

// After (HIR)
concat(concat("Hello, ", name), "")
```

### Typed HIR

**Purpose:** Fully type-checked representation

**Contents:**
- All HIR nodes
- Type annotations on every expression
- Resolved type aliases
- Type equality cache

**Used by:**
- Custom lint rules
- IDE semantic highlighting
- Refactoring tools
- Documentation generation

**Example structure:**

```rust
struct TypedExpr {
    expr: ExprKind,
    ty: Type,
    span: Span,
}

struct TypedFunction {
    hir: Function,
    param_types: Vec<Type>,
    return_type: Type,
    body: TypedBlock,
}
```

### MIR (Mid-level IR)

**Purpose:** Simple, optimizable representation

**Characteristics:**
- All expressions are simple
- Explicit temporaries
- Control flow is explicit
- No complex expressions

**Example lowering:**

```min
// HIR
let x = if condition { a + b } else { c * d }

// MIR (pseudo-code)
let temp1;
if condition {
    temp1 = add(a, b)
} else {
    temp1 = mul(c, d)
}
let x = temp1
```

## Salsa Query System

All IR layers are computed incrementally using Salsa queries.

### Core Queries

```rust
#[salsa::query_group(CompilerDatabase)]
trait CompilerQueries {
    // Input
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;
    
    // Parsing
    fn parse(&self, file: FileId) -> Arc<Cst>;
    fn cst_to_ast(&self, file: FileId) -> Arc<Ast>;
    
    // Name resolution
    fn resolve_names(&self, file: FileId) -> Arc<NameResolution>;
    
    // Type checking
    fn infer_types(&self, file: FileId) -> Arc<TypedHir>;
    fn type_of_expr(&self, file: FileId, expr: ExprId) -> Type;
    
    // Lowering
    fn lower_to_mir(&self, file: FileId) -> Arc<Mir>;
    
    // Code generation
    fn generate_wit(&self, file: FileId) -> Arc<WitInterface>;
    fn compile_to_wasm(&self, file: FileId) -> Arc<WasmModule>;
    
    // Diagnostics
    fn diagnostics(&self, file: FileId) -> Arc<Vec<Diagnostic>>;
}
```

### Query Dependencies

```
source_text (input)
    ↓
parse
    ↓
cst_to_ast
    ↓
resolve_names
    ↓
infer_types → type_of_expr (fine-grained)
    ↓
lower_to_mir
    ↓
generate_wit + compile_to_wasm
```

When source code changes, only affected queries recompute.

## Error Handling

### Error-Tolerant Parsing

The parser should recover from errors and continue parsing:

**Strategies:**
1. **Missing tokens:** Insert synthetic tokens and continue
2. **Unexpected tokens:** Skip until synchronization point
3. **Incomplete constructs:** Create partial nodes with errors

**Example:**

```min
fn broken:
  name: string
  // Missing return type and body

fn works:
  x: int
-> int {
  return x
}
```

Parser should:
- Emit error for `broken` (missing return type/body)
- Create partial `Function` node for `broken`
- Continue parsing `works` successfully

### Diagnostic Collection

Errors are collected at each stage:

```rust
struct Diagnostic {
    severity: Severity,
    message: String,
    span: Span,
    labels: Vec<Label>,
    notes: Vec<String>,
    fixes: Vec<Fix>,
}

enum Severity {
    Error,
    Warning,
    Info,
}

struct Fix {
    message: String,
    edits: Vec<TextEdit>,
}
```

All diagnostics are accumulated and reported through Salsa queries.

## Type Checking

### Type Inference Algorithm

**Constraints:**
- Function parameters and returns: explicit types required
- Local variables: inferred from usage

**Algorithm:**

1. **Collect constraints:**
   - From explicit type annotations
   - From operations (e.g., `+` requires numeric types)
   - From function calls

2. **Unify constraints:**
   - Propagate type information
   - Detect conflicts

3. **Solve:**
   - Assign concrete types to all expressions
   - Report errors for unsolvable constraints

### Structural Type Checking

Types are checked structurally:

```rust
fn types_equal(db: &dyn CompilerQueries, t1: Type, t2: Type) -> bool {
    match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::Struct(fields1), Type::Struct(fields2)) => {
            fields1.len() == fields2.len() &&
            fields1.iter().zip(fields2.iter()).all(|(f1, f2)| {
                f1.name == f2.name && types_equal(db, f1.ty, f2.ty)
            })
        }
        // ...
    }
}
```

### Type Alias Resolution

Type aliases are resolved to their underlying structural types:

```rust
fn resolve_type_alias(db: &dyn CompilerQueries, alias: TypeAlias) -> Type {
    // Recursively expand aliases
    match alias.definition {
        Type::Alias(inner_alias) => resolve_type_alias(db, inner_alias),
        ty => ty,
    }
}
```

For WIT generation, track which alias name to use for each structure.

## WIT Generation

### Type Canonicalization

1. **Collect all structural types** used in exports
2. **Deduplicate** by structure
3. **Assign names:**
   - If type alias exists: use alias name
   - If multiple aliases for same structure: choose deterministically (e.g., first alphabetically)
   - If no alias: generate name (e.g., `type_0`, `type_1`)

### WIT Output Format

```wit
package min:generated;

record point {
  x: s32,
  y: s32,
}

world min-program {
  export add: func(a: s32, b: s32) -> s32;
  export make-point: func() -> point;
}
```

## WASM Code Generation

### Compilation Strategy

1. **Lower MIR to WASM IR**
   - Map Min types to WASM types
   - Generate WASM instructions

2. **Generate component wrapper**
   - Use WIT to create component interface
   - Wire exports/imports

3. **Link stdlib**
   - Tree-shake unused stdlib functions
   - Inline or import as needed

### Type Mapping

| Min Type | WASM Type | WIT Type |
|----------|-----------|----------|
| `int` | `i32` | `s32` |
| `float` | `f32` | `f32` |
| `bool` | `i32` | `bool` |
| `string` | Complex | `string` |
| Struct | Complex | `record` |

Strings and structs use the Component Model's canonical ABI for representation.

## Extensibility

### Adding IR Passes

To add a new analysis or transformation:

1. Add a Salsa query
2. Define input/output types
3. Implement transformation
4. Wire into pipeline

Example:

```rust
#[salsa::query_group(CustomAnalysisDatabase)]
trait CustomAnalysis: CompilerQueries {
    fn custom_lint_pass(&self, file: FileId) -> Arc<Vec<Lint>>;
}

fn custom_lint_pass(db: &dyn CustomAnalysis, file: FileId) -> Arc<Vec<Lint>> {
    let typed_hir = db.infer_types(file);
    // Analyze typed_hir and generate lints
    Arc::new(lints)
}
```

### Adding Language Features

To add features in a fork:

1. Extend AST with new node types
2. Extend HIR lowering
3. Extend type checking rules
4. Extend MIR lowering
5. Update WASM generation

The layered IR makes it clear where each change belongs.

## Performance Considerations

- **Incremental computation:** Salsa ensures only changed queries recompute
- **Fine-grained queries:** `type_of_expr` is per-expression, not per-file
- **Parallel compilation:** Salsa supports parallel query evaluation
- **Memoization:** All query results are cached

## Testing Strategy

Each IR layer should have:

1. **Unit tests** for transformations
2. **Snapshot tests** for IR output
3. **Integration tests** for full pipeline
4. **Error tests** for diagnostics

Example test structure:

```
tests/
  parser/
    success/
      simple_function.min
      simple_function.cst.snap
    errors/
      missing_return_type.min
      missing_return_type.errors.snap
  type_checker/
    success/
      struct_equality.min
      struct_equality.typed_hir.snap
    errors/
      type_mismatch.min
      type_mismatch.errors.snap
  codegen/
    simple_add.min
    simple_add.wit.snap
    simple_add.wasm.snap
```

## Implementation Priority

1. **Phase 1: Parser & AST**
   - Lexer
   - Error-tolerant parser
   - CST construction
   - CST → AST lowering
   - Basic diagnostics

2. **Phase 2: Type System**
   - HIR construction
   - Name resolution
   - Type inference
   - Structural type checking
   - Type error reporting

3. **Phase 3: Salsa Integration**
   - Define query interface
   - Implement incremental computation
   - Cache management

4. **Phase 4: Code Generation**
   - MIR lowering
   - WIT generation
   - Basic WASM generation

5. **Phase 5: LSP**
   - LSP server setup
   - Reuse Salsa queries
   - IDE features

6. **Phase 6: Stdlib & Polish**
   - Stdlib design
   - Optimizations
   - Better error messages
