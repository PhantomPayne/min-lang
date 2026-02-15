# Claude Code Implementation Guide for Min Language

This file provides detailed guidance for implementing the Min programming language. Follow these instructions carefully to ensure a high-quality, well-tested implementation.

## 🎯 Project Overview

You are building **Min**, a minimal programming language designed as a foundation for maximum tooling. The language compiles to WebAssembly via the WIT component model and features:

- Expression-based syntax with explicit, readable code
- Structural type system
- Salsa-based incremental compilation
- Full LSP support with excellent IDE integration
- Error-tolerant parsing

## 📖 Essential Reading

Before starting, thoroughly read these documents in order:

1. **README.md** - Understand the project goals and philosophy
2. **LANGUAGE_SPEC.md** - Master the complete language syntax and semantics
3. **COMPILER_ARCHITECTURE.md** - Understand the compilation pipeline
4. **TECHNICAL_SPECS.md** - Study the detailed implementation specifications
5. **IMPLEMENTATION_ROADMAP.md** - Follow the phased implementation plan
6. **EXAMPLES.md** - Review example programs to validate understanding
7. **LSP_DESIGN.md** - Understand LSP requirements (for Phase 5)

## 🏗️ Implementation Philosophy

### Core Principles

1. **Test-Driven Development**
   - Write tests BEFORE implementation
   - Aim for >90% test coverage
   - Use snapshot testing extensively for IR outputs
   - Test error cases as thoroughly as success cases

2. **Incremental Progress**
   - Complete each phase fully before moving to the next
   - Make each commit atomic and well-described
   - Ensure CI passes on every commit
   - Don't skip ahead - the phases build on each other

3. **Error Recovery**
   - Parser must be error-tolerant (critical for LSP)
   - Always produce partial results when possible
   - Provide helpful, actionable error messages
   - Include quick fixes where applicable

4. **Documentation**
   - Document all public APIs with doc comments
   - Include examples in documentation
   - Keep README updated with progress
   - Document design decisions in code comments

5. **Clean Code**
   - Follow Rust idioms and best practices
   - Use meaningful variable names
   - Keep functions small and focused
   - Avoid premature optimization

## 📋 Phase-by-Phase Implementation

### Phase 1: Foundation (Lexer & Parser)

**Goal:** Parse all example programs successfully

#### Step 1.1: Project Setup

```bash
# Create workspace structure
cargo init --lib crates/min-syntax
cargo init --lib crates/min-hir
cargo init --lib crates/min-types
cargo init --lib crates/min-mir
cargo init --lib crates/min-codegen
cargo init --lib crates/min-db
cargo init --lib crates/min-diagnostics
cargo init --lib crates/min-lsp
cargo init --bin crates/min-cli
cargo init --lib crates/min-test-utils
```

Add dependencies to workspace `Cargo.toml` as specified in IMPLEMENTATION_ROADMAP.md.

#### Step 1.2: Implement Lexer

**File:** `crates/min-syntax/src/lexer.rs`

**Requirements:**
- Tokenize all token types from TECHNICAL_SPECS.md
- Track accurate spans for every token
- Handle string interpolation correctly
- Emit error tokens for invalid input but continue
- Preserve all whitespace and comments in token stream

**Test Strategy:**
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn lex_keywords() {
        let tokens = lex("fn let return if else type");
        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(tokens[1].kind, TokenKind::Let);
        // ... etc
    }
    
    #[test]
    fn lex_string_interpolation() {
        let tokens = lex(r#""Hello {name}!""#);
        assert_eq!(tokens[0].kind, TokenKind::StringStart);
        assert_eq!(tokens[1].kind, TokenKind::Ident);
        assert_eq!(tokens[2].kind, TokenKind::StringEnd);
    }
    
    #[test]
    fn lex_error_recovery() {
        let tokens = lex("fn test @ 123");
        // Should emit tokens even with invalid @ character
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Error));
    }
}
```

**Validation:** Run `cargo test` - all lexer tests should pass.

#### Step 1.3: Implement CST Parser

**File:** `crates/min-syntax/src/parser.rs`

**Requirements:**
- Use `rowan` for CST construction (lossless syntax tree)
- Implement grammar from TECHNICAL_SPECS.md
- Error recovery at synchronization points
- Preserve all trivia (whitespace, comments)
- Provide helpful error messages

**Key Techniques:**

```rust
// Error recovery example
fn parse_function(&mut self) -> CompletedMarker {
    let m = self.start_node(SyntaxKind::Function);
    
    self.expect(TokenKind::Fn);
    
    if !self.at(TokenKind::Ident) {
        self.error("expected function name");
        // Try to recover by skipping to next known token
        self.skip_until(|k| matches!(k, TokenKind::Colon | TokenKind::Fn));
    } else {
        self.bump(); // consume ident
    }
    
    // Continue parsing...
    m.complete(self)
}
```

**Test Strategy:**
```rust
#[test]
fn parse_simple_function() {
    let source = r#"
        fn add:
          a: int
          b: int
        -> int {
          return a + b
        }
    "#;
    
    let cst = parse(source);
    expect_test::expect![[r#"
        SourceFile@0..XX
          Function@0..XX
            FnKw@0..2 "fn"
            Whitespace@2..3
            Ident@3..6 "add"
            Colon@6..7
            ...
    "#]].assert_eq(&format!("{:#?}", cst));
}

#[test]
fn parse_with_errors() {
    let source = "fn broken: name: string";
    let cst = parse(source);
    
    // Should produce partial CST despite errors
    assert!(!cst.errors().is_empty());
    assert!(cst.root().kind() == SyntaxKind::SourceFile);
}
```

#### Step 1.4: Implement CST → AST Lowering

**File:** `crates/min-syntax/src/lower.rs`

**Requirements:**
- Convert CST to clean AST
- Strip trivia (but preserve for diagnostics)
- Collect errors during lowering
- Build symbol table for names

**Test Strategy:**
- Snapshot tests comparing CST and AST
- Verify all examples from EXAMPLES.md parse correctly
- Test that errors are preserved

**Validation Checklist:**
- [ ] All 13 examples in EXAMPLES.md parse successfully
- [ ] Error recovery works (partial AST for broken programs)
- [ ] Spans are accurate for diagnostics
- [ ] All lexer tests pass
- [ ] All parser tests pass
- [ ] Test coverage >90%

### Phase 2: Type System

**Goal:** Type-check all valid programs and report helpful errors

#### Step 2.1: Type Representation

**File:** `crates/min-types/src/ty.rs`

Implement types exactly as specified in TECHNICAL_SPECS.md:
- Primitive types (Int, Float, Bool, String)
- Struct types with structural equality
- Generic types (basic support)
- Type variables for inference
- Error type for recovery

**Critical:**
```rust
impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        // MUST be structural equality, NOT nominal
        match (self, other) {
            (Type::Struct(fields1), Type::Struct(fields2)) => {
                fields1.len() == fields2.len() &&
                fields1.iter().zip(fields2).all(|(f1, f2)| {
                    f1.name == f2.name && f1.ty == f2.ty
                })
            }
            // ... other cases
        }
    }
}
```

#### Step 2.2: Name Resolution

**File:** `crates/min-hir/src/lower.rs`

Build scopes and resolve all identifiers:
- Function names
- Type names  
- Variable references
- Parameter names

Track definitions and uses for later LSP features.

#### Step 2.3: Type Inference

**File:** `crates/min-types/src/infer.rs`

Implement constraint-based type inference:

1. **Collect constraints** from expressions
2. **Unify** type variables using algorithm from TECHNICAL_SPECS.md
3. **Solve** to get concrete types
4. **Report** helpful errors for failures

**Test extensively:**
```rust
#[test]
fn infer_local_variable() {
    let source = r#"
        fn test() -> int {
          let x = 5;
          return x
        }
    "#;
    
    let typed = type_check(source);
    assert_eq!(typed.type_of_local("x"), Type::Int);
}

#[test]
fn infer_from_usage() {
    let source = r#"
        fn test() -> int {
          let x = 5;
          let y = x + 3;
          return y
        }
    "#;
    
    let typed = type_check(source);
    assert_eq!(typed.type_of_local("y"), Type::Int);
}
```

#### Step 2.4: Type Checking

**File:** `crates/min-types/src/check.rs`

Check all expressions have valid types:
- Binary operations (both operands same type)
- Function calls (arguments match parameters)
- If expressions (both branches same type)
- Return statements (match function return type)
- Struct field access (field exists)

**Error Messages Must Be Helpful:**

```rust
// Bad error:
"type mismatch"

// Good error:
"expected int, found string
  ┌─ example.min:3:10
  │
3 │   let x = "hello" + 5;
  │           -------   - expected string, found int
  │           │
  │           this has type string
  │
  = note: cannot add int to string
  = help: convert the int to string with to_string()"
```

**Validation Checklist:**
- [ ] All well-typed examples type-check
- [ ] Type errors are reported correctly
- [ ] Structural equality works (Point == Vec2)
- [ ] Error messages are helpful
- [ ] Test coverage >95%

### Phase 3: Salsa Integration

**Goal:** Make compilation incremental and fast

#### Implementation

**File:** `crates/min-db/src/database.rs`

Define all queries as specified in COMPILER_ARCHITECTURE.md:

```rust
#[salsa::query_group(CompilerDatabase)]
pub trait CompilerQueries: salsa::Database {
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;
    
    fn parse(&self, file: FileId) -> Arc<Cst>;
    fn lower_to_ast(&self, file: FileId) -> Arc<Ast>;
    fn infer_types(&self, file: FileId) -> Arc<TypedHir>;
    fn diagnostics(&self, file: FileId) -> Arc<Vec<Diagnostic>>;
}
```

**Critical:** Every stage must be a Salsa query for incrementality.

**Testing:**
```rust
#[test]
fn incremental_recompilation() {
    let mut db = Database::default();
    let file = FileId(0);
    
    // Initial compilation
    db.set_source_text(file, Arc::new("fn test() -> int { 5 }".to_string()));
    let diags1 = db.diagnostics(file);
    
    // Change doesn't affect types
    db.set_source_text(file, Arc::new("fn test() -> int { 6 }".to_string()));
    let diags2 = db.diagnostics(file);
    
    // Should be fast - most queries cached
}
```

**Validation Checklist:**
- [ ] All compilation stages are Salsa queries
- [ ] Changes only recompute affected queries
- [ ] Incremental tests pass
- [ ] Compilation is fast (<100ms for examples)

### Phase 4: Code Generation

**Goal:** Compile Min to runnable WASM components

#### Step 4.1: MIR Lowering

**File:** `crates/min-mir/src/lower.rs`

Lower typed HIR to MIR following TECHNICAL_SPECS.md:
- Convert to basic blocks
- Simple statements only
- Explicit control flow

#### Step 4.2: WIT Generation

**File:** `crates/min-codegen/src/wit.rs`

Generate WIT interface definitions:
- Canonicalize types (same structure = same WIT type)
- Use type alias names where available
- Generate deterministic names
- Create valid WIT output

**Test WIT output:**
```rust
#[test]
fn generate_wit_for_simple_function() {
    let source = r#"
        fn add(a: int, b: int) -> int {
          return a + b
        }
    "#;
    
    let wit = generate_wit(source);
    expect_test::expect![[r#"
        package min:program;
        
        world min-program {
          export add: func(a: s32, b: s32) -> s32;
        }
    "#]].assert_eq(&wit);
}
```

#### Step 4.3: WASM Generation

**File:** `crates/min-codegen/src/wasm.rs`

Generate WASM component:
- Lower MIR to WASM instructions
- Wire to WIT interface
- Link stdlib (minimal for now)

**Validation:**
```rust
#[test]
fn run_compiled_program() {
    let source = r#"
        fn add(a: int, b: int) -> int {
          return a + b
        }
    "#;
    
    let wasm = compile(source);
    let result = execute_wasm(&wasm, "add", &[5, 3]);
    assert_eq!(result, 8);
}
```

**Validation Checklist:**
- [ ] All examples compile to WASM
- [ ] Generated WIT is valid
- [ ] Compiled programs run correctly
- [ ] Test coverage >85%

### Phase 5: LSP Implementation

**Goal:** Full IDE support in VS Code

#### Step 5.1: LSP Server Setup

**File:** `crates/min-lsp/src/server.rs`

Set up tower-lsp server:
- Initialize with capabilities
- Handle lifecycle events
- Wire to Salsa database

#### Step 5.2: Core Features

Implement in this order (each in its own file under `handlers/`):

1. **Diagnostics** (`diagnostics.rs`)
   - Show errors/warnings in real-time
   - Clear on fixes

2. **Hover** (`hover.rs`)
   - Show type information
   - Show documentation
   - Format nicely

3. **Go to Definition** (`goto_def.rs`)
   - Jump to function definitions
   - Jump to type definitions
   - Jump to variable bindings

4. **Completion** (`completion.rs`)
   - Complete variable names
   - Complete function names
   - Complete type names
   - Complete struct fields
   - Complete parameter names

5. **Formatting** (`formatting.rs`)
   - Consistent indentation
   - Preserve user's vertical layout
   - Format on save

**Each feature needs thorough tests:**

```rust
#[test]
fn test_hover_on_variable() {
    let source = r#"
        fn test() -> int {
          let x = 5;
          return x
        }
    "#;
    //             ^ cursor here
    
    let hover = get_hover(source, Position { line: 2, character: 9 });
    assert!(hover.contents.contains("x: int"));
}
```

#### Step 5.3: VS Code Extension

**File:** `editors/vscode/src/extension.ts`

Basic TypeScript extension:
- Start LSP server
- Connect to server
- Register language configuration
- Provide syntax highlighting

**Validation Checklist:**
- [ ] All LSP features work in VS Code
- [ ] Diagnostics update on typing
- [ ] Completion is fast (<50ms)
- [ ] Go to definition works
- [ ] Hover shows type info
- [ ] Formatting preserves style
- [ ] Extension activates correctly

### Phase 6: Polish & Documentation

**Goal:** Production-ready v0.1 release

#### Tasks

1. **Improve Error Messages**
   - Review all error messages
   - Add suggestions
   - Add quick fixes
   - Test readability

2. **Standard Library**
   - Define stdlib interface (WIT imports)
   - Implement tree-shaking
   - Document all stdlib functions

3. **Optimization**
   - Benchmark compilation speed
   - Optimize hot paths
   - Reduce WASM size

4. **Documentation**
   - Write user guide
   - API documentation (rustdoc)
   - Tutorial with examples
   - Contributing guide

5. **CI/CD**
   - GitHub Actions for tests
   - Automated releases
   - Binary distribution

**Validation Checklist:**
- [ ] All examples compile and run
- [ ] Error messages are excellent
- [ ] Compilation is fast
- [ ] Documentation is complete
- [ ] CI passes on all commits
- [ ] Ready for external users

## 🧪 Testing Guidelines

### Test Categories

1. **Unit Tests**
   - Every public function
   - Use `#[cfg(test)]` modules
   - Test edge cases

2. **Snapshot Tests**
   - Use `expect_test` or `insta`
   - Test CST, AST, HIR, MIR output
   - Test error messages

3. **Integration Tests**
   - Full pipeline tests
   - Compile and run programs
   - Test LSP features

4. **Example Tests**
   - Every example in EXAMPLES.md must compile
   - Test both compilation and execution

### Test Organization

```
crates/min-syntax/
  src/
    lexer.rs
    parser.rs
    lower.rs
  tests/
    lexer_tests.rs
    parser_tests.rs
    snapshots/
      function_simple.snap
      function_with_errors.snap
```

### Writing Good Tests

```rust
// Good: Descriptive name, tests one thing
#[test]
fn parse_function_with_multiple_parameters() {
    let source = r#"
        fn add:
          a: int
          b: int
          c: int
        -> int {
          return a + b + c
        }
    "#;
    
    let ast = parse(source);
    assert_eq!(ast.functions()[0].params.len(), 3);
}

// Bad: Unclear what's being tested
#[test]
fn test1() {
    let ast = parse("fn add: a: int b: int -> int { return a + b }");
    assert!(ast.functions().len() > 0);
}
```

## 🚨 Common Pitfalls to Avoid

### 1. Skipping Error Recovery
**Don't:** Skip error recovery in the parser
**Do:** Implement proper synchronization points and partial AST construction

### 2. Nominal Instead of Structural
**Don't:** Check type equality by comparing type names
**Do:** Compare struct field names and types recursively

### 3. Breaking Incrementality
**Don't:** Cache results manually or skip Salsa queries
**Do:** Always use Salsa queries for all computation

### 4. Ignoring Spans
**Don't:** Lose source location information
**Do:** Track spans through all IR layers for diagnostics

### 5. Poor Error Messages
**Don't:** "Syntax error at line 5"
**Do:** "Expected '->' after parameter list"

### 6. Testing Only Success Cases
**Don't:** Only test valid programs
**Do:** Test error cases equally thoroughly

## 📊 Progress Tracking

Keep the README.md updated with progress:

```markdown
## Implementation Status

- [x] Phase 1: Foundation
  - [x] Lexer
  - [x] Parser
  - [x] CST → AST lowering
- [ ] Phase 2: Type System
  - [x] Type representation
  - [x] Name resolution
  - [ ] Type inference (in progress)
  - [ ] Type checking
- [ ] Phase 3: Salsa Integration
- [ ] Phase 4: Code Generation
- [ ] Phase 5: LSP
- [ ] Phase 6: Polish
```

## 💡 Best Practices

### Code Style

```rust
// Good: Clear, documented, well-structured
/// Parses a function definition.
/// 
/// # Errors
/// Returns parse errors if the function is malformed.
fn parse_function(&mut self) -> Result<Function, ParseError> {
    let name = self.parse_ident()?;
    let params = self.parse_params()?;
    let return_type = self.parse_return_type()?;
    let body = self.parse_block()?;
    
    Ok(Function {
        name,
        params,
        return_type,
        body,
    })
}

// Bad: Unclear, no docs, doing too much
fn parse(&mut self) -> Result<Ast, Error> {
    let mut items = vec![];
    while !self.at_eof() {
        if self.at(Fn) {
            self.bump();
            let name = self.text();
            self.bump();
            // ... 50 more lines
        }
    }
    Ok(Ast { items })
}
```

### Commit Messages

```bash
# Good
git commit -m "feat(parser): implement error recovery for functions

- Add synchronization points at fn and type keywords
- Create partial AST nodes when recovery occurs
- Add tests for various error scenarios

Closes #42"

# Bad
git commit -m "fix stuff"
```

## 🎓 Learning Resources

If you need to learn about specific topics:

- **Salsa:** https://salsa-rs.github.io/salsa/
- **Rowan:** https://github.com/rust-analyzer/rowan
- **Type Inference:** Algorithm W, Hindley-Milner
- **LSP:** https://microsoft.github.io/language-server-protocol/
- **WASM Components:** https://component-model.bytecodealliance.org/
- **Parser Error Recovery:** "Resilient Parsing" by Rust-Analyzer

## ✅ Definition of Done

A phase is complete when:

1. ✅ All code is written and reviewed
2. ✅ All tests pass (including new tests for the phase)
3. ✅ Test coverage meets target (>90% for parser, >95% for type checker)
4. ✅ Documentation is updated
5. ✅ All examples still work
6. ✅ CI passes
7. ✅ README progress is updated
8. ✅ No TODO comments left in code
9. ✅ Performance targets are met
10. ✅ Code is formatted (`cargo fmt`)

## 🚀 Getting Started

1. Read all documentation thoroughly
2. Set up the project structure
3. Start with Phase 1, Step 1.1
4. Write tests first for each feature
5. Implement incrementally
6. Commit frequently
7. Keep CI green
8. Update documentation as you go

Remember: **Quality over speed**. A well-tested, well-documented lexer is better than a rushed complete compiler.

## 📞 Getting Help

If you encounter challenges:

1. Re-read the relevant design documents
2. Check TECHNICAL_SPECS.md for detailed algorithms
3. Look at the examples in EXAMPLES.md
4. Review the test strategy for the component
5. Ask for clarification on specific design decisions

## 🎯 Success Criteria

The implementation is successful when:

1. ✅ All 13 examples in EXAMPLES.md compile and run correctly
2. ✅ Type checking provides helpful, accurate errors
3. ✅ LSP works smoothly in VS Code with all features
4. ✅ Compilation is fast (<100ms for small programs)
5. ✅ Generated WASM is correct and efficient
6. ✅ Error messages are clear and actionable
7. ✅ Documentation is comprehensive and helpful
8. ✅ Test coverage exceeds targets
9. ✅ CI passes on all commits
10. ✅ Code is clean and maintainable

Good luck building Min! This is a solid foundation for an excellent language implementation.
