# Min Language Implementation Roadmap

## Project Structure

```
min-lang/
├── Cargo.toml                 # Rust workspace
├── README.md
├── docs/
│   ├── LANGUAGE_SPEC.md       # Language specification
│   ├── COMPILER_ARCHITECTURE.md
│   ├── LSP_DESIGN.md
│   └── EXAMPLES.md
│
├── crates/
│   ├── min-syntax/            # Lexer, Parser, CST
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── lexer.rs       # Tokenization
│   │   │   ├── token.rs       # Token types
│   │   │   ├── parser.rs      # Parser implementation
│   │   │   ├── cst.rs         # CST node types
│   │   │   ├── ast.rs         # AST node types
│   │   │   └── lower.rs       # CST → AST lowering
│   │   └── tests/
│   │
│   ├── min-hir/               # High-level IR
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── hir.rs         # HIR node types
│   │   │   ├── lower.rs       # AST → HIR lowering
│   │   │   └── display.rs     # Pretty printing
│   │   └── tests/
│   │
│   ├── min-types/             # Type system
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── ty.rs          # Type representation
│   │   │   ├── infer.rs       # Type inference
│   │   │   ├── unify.rs       # Type unification
│   │   │   ├── check.rs       # Type checking
│   │   │   └── display.rs     # Type printing
│   │   └── tests/
│   │
│   ├── min-mir/               # Mid-level IR
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── mir.rs         # MIR node types
│   │   │   ├── lower.rs       # HIR → MIR lowering
│   │   │   └── optimize.rs    # MIR optimizations
│   │   └── tests/
│   │
│   ├── min-codegen/           # Code generation
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── wit.rs         # WIT generation
│   │   │   ├── wasm.rs        # WASM generation
│   │   │   └── component.rs   # Component model wrapper
│   │   └── tests/
│   │
│   ├── min-db/                # Salsa database
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── queries.rs     # Query definitions
│   │   │   ├── database.rs    # Database implementation
│   │   │   └── input.rs       # Input handling
│   │   └── tests/
│   │
│   ├── min-diagnostics/       # Error reporting
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── diagnostic.rs  # Diagnostic types
│   │   │   ├── emit.rs        # Error formatting
│   │   │   └── fixes.rs       # Quick fixes
│   │   └── tests/
│   │
│   ├── min-lsp/               # Language Server
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── main.rs        # LSP server entry point
│   │   │   ├── server.rs      # LSP implementation
│   │   │   ├── handlers/      # LSP request handlers
│   │   │   │   ├── mod.rs
│   │   │   │   ├── completion.rs
│   │   │   │   ├── hover.rs
│   │   │   │   ├── goto_def.rs
│   │   │   │   ├── formatting.rs
│   │   │   │   └── diagnostics.rs
│   │   │   └── util.rs        # Helper utilities
│   │   └── tests/
│   │
│   ├── min-cli/               # Command-line interface
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── main.rs        # CLI entry point
│   │   │   ├── build.rs       # Build command
│   │   │   ├── check.rs       # Check command
│   │   │   └── format.rs      # Format command
│   │   └── tests/
│   │
│   └── min-test-utils/        # Test utilities
│       ├── Cargo.toml
│       └── src/
│           ├── lib.rs
│           ├── fixtures.rs    # Test fixtures
│           └── assertions.rs  # Test assertions
│
├── editors/
│   └── vscode/                # VS Code extension
│       ├── package.json
│       ├── src/
│       │   └── extension.ts
│       └── syntaxes/
│           └── min.tmLanguage.json
│
└── tests/
    ├── end-to-end/            # Full pipeline tests
    ├── snapshots/             # Snapshot tests
    └── fixtures/              # Test .min files
```

## Implementation Phases

### Phase 1: Foundation (Weeks 1-2)

**Goal:** Parse valid Min programs and build CST/AST

**Tasks:**

1. **Set up project structure**
   - Create Cargo workspace
   - Set up all crate scaffolding
   - Add dependencies (salsa, rowan for CST, etc.)

2. **Implement Lexer** (`min-syntax/lexer.rs`)
   - Tokenize source code
   - Handle keywords, identifiers, literals, operators
   - Track spans for error reporting
   - Handle comments

3. **Implement Parser** (`min-syntax/parser.rs`)
   - Parse tokens into CST
   - Error recovery for partial programs
   - Build rowan-based CST with full fidelity

4. **Implement CST → AST lowering** (`min-syntax/lower.rs`)
   - Strip whitespace and comments
   - Build AST from CST
   - Preserve spans for diagnostics

5. **Testing**
   - Unit tests for lexer (token streams)
   - Unit tests for parser (CST structure)
   - Snapshot tests for AST output

**Deliverables:**
- Can parse all example programs
- CST preserves all source information
- AST represents program structure
- Error recovery works for basic cases

### Phase 2: Type System (Weeks 3-4)

**Goal:** Type check Min programs and report type errors

**Tasks:**

1. **Implement Type Representation** (`min-types/ty.rs`)
   - Type enum (Int, Float, Bool, String, Struct, etc.)
   - Structural equality checks
   - Type display/formatting

2. **Implement Name Resolution** (`min-hir/lower.rs`)
   - Resolve variable references
   - Resolve type names
   - Build scopes
   - Report undefined references

3. **Implement Type Inference** (`min-types/infer.rs`)
   - Infer types for local variables
   - Propagate constraints
   - Unification algorithm

4. **Implement Type Checking** (`min-types/check.rs`)
   - Check function signatures
   - Check struct field access
   - Check binary operations
   - Report type errors

5. **Testing**
   - Type inference tests
   - Type error tests
   - Structural equality tests

**Deliverables:**
- All well-typed programs type check successfully
- Type errors are reported with helpful messages
- Structural type equality works

### Phase 3: Salsa Integration (Week 5)

**Goal:** Make compilation incremental

**Tasks:**

1. **Define Salsa Database** (`min-db/database.rs`)
   - Define query traits
   - Implement database struct
   - Set up input queries

2. **Implement Queries** (`min-db/queries.rs`)
   - `parse()` query
   - `cst_to_ast()` query
   - `infer_types()` query
   - `diagnostics()` query

3. **Wire Everything Together**
   - Connect parser to Salsa
   - Connect type checker to Salsa
   - Implement incremental updates

4. **Testing**
   - Test incremental updates
   - Test query invalidation
   - Benchmark performance

**Deliverables:**
- Compiler uses Salsa for all queries
- Changes only recompute affected queries
- Query results are cached

### Phase 4: Code Generation (Weeks 6-7)

**Goal:** Generate WASM components from Min programs

**Tasks:**

1. **Implement MIR** (`min-mir/`)
   - Define MIR types
   - Lower HIR → MIR
   - Simple optimizations (constant folding, DCE)

2. **Implement WIT Generation** (`min-codegen/wit.rs`)
   - Type canonicalization
   - WIT type mapping
   - Generate WIT interfaces

3. **Implement WASM Generation** (`min-codegen/wasm.rs`)
   - Lower MIR to WASM instructions
   - Generate component wrapper
   - Link stdlib (basic for now)

4. **Testing**
   - Test WIT output
   - Test WASM output
   - Test running compiled programs

**Deliverables:**
- Can compile Min to WASM components
- WIT output is valid
- Compiled programs can run

### Phase 5: LSP Implementation (Weeks 8-9)

**Goal:** Full IDE support via LSP

**Tasks:**

1. **Set Up LSP Server** (`min-lsp/server.rs`)
   - Implement LSP protocol
   - Wire to Salsa database
   - Handle lifecycle

2. **Implement Core Features**
   - Diagnostics (errors/warnings)
   - Go to definition
   - Hover information
   - Code completion
   - Document symbols

3. **Implement Advanced Features**
   - Rename symbol
   - Find references
   - Formatting
   - Inlay hints
   - Semantic tokens

4. **VS Code Extension** (`editors/vscode/`)
   - Basic extension setup
   - Syntax highlighting
   - Connect to LSP server

5. **Testing**
   - LSP feature tests
   - Integration tests
   - Manual testing in VS Code

**Deliverables:**
- Working LSP server
- VS Code extension
- All core LSP features implemented

### Phase 6: Standard Library & Polish (Weeks 10-11)

**Goal:** Make the language actually usable

**Tasks:**

1. **Design Stdlib Interface**
   - Define core operations as WIT imports
   - Implement tree-shaking
   - Document stdlib

2. **Improve Error Messages**
   - Better diagnostics
   - Suggestions and fixes
   - Error recovery improvements

3. **Optimize Compilation**
   - Faster parsing
   - Better MIR optimizations
   - Smaller WASM output

4. **Documentation**
   - User guide
   - API documentation
   - Examples and tutorials

5. **Testing**
   - End-to-end tests
   - Performance benchmarks
   - Stress tests

**Deliverables:**
- Usable standard library
- Great error messages
- Comprehensive documentation
- Production-ready compiler

## Dependencies

### Core Dependencies

```toml
[workspace.dependencies]
# Parsing and CST
rowan = "0.15"           # CST library
logos = "0.13"           # Lexer generator (alternative: handwritten)
text-size = "1.1"        # Text ranges

# Incremental computation
salsa = "0.17"           # Query system

# Code generation
wasm-encoder = "0.38"    # WASM generation
wit-component = "0.18"   # WIT support

# LSP
tower-lsp = "0.20"       # LSP framework
lsp-types = "0.95"       # LSP types

# Utilities
anyhow = "1.0"           # Error handling
thiserror = "1.0"        # Error types
tracing = "0.1"          # Logging
serde = "1.0"            # Serialization

# CLI
clap = "4.4"             # CLI parsing
ariadne = "0.4"          # Error reporting

# Testing
expect-test = "1.4"      # Snapshot testing
insta = "1.34"           # Snapshot testing (alternative)
```

## Milestones

- **Milestone 1 (Week 2):** Can parse all example programs
- **Milestone 2 (Week 4):** Type checking works
- **Milestone 3 (Week 5):** Incremental compilation works
- **Milestone 4 (Week 7):** Can compile to WASM
- **Milestone 5 (Week 9):** LSP provides full IDE support
- **Milestone 6 (Week 11):** Production-ready v0.1 release

## Success Criteria

The implementation is successful when:

1. ✅ All example programs compile and run
2. ✅ Type errors are helpful and accurate
3. ✅ LSP works in VS Code with all features
4. ✅ Compilation is fast (< 100ms for small programs)
5. ✅ Generated WASM is correct and reasonably sized
6. ✅ Error messages are clear and actionable
7. ✅ Documentation is complete and helpful
8. ✅ Language can be forked easily for new paradigms

## Next Steps for Claude Code

To start implementation:

1. **Read all design documents** to understand the full picture
2. **Set up project structure** following the roadmap
3. **Start with Phase 1**: Implement lexer and parser
4. **Write tests first** for each component (TDD approach)
5. **Use snapshot testing** extensively for CST/AST/IR
6. **Follow incremental approach**: Get each phase working before moving to next

Good luck! The foundation is designed to be clean and extensible.
