# Min Language

A minimal, foundational programming language with maximum tooling support. Min compiles to WebAssembly via the WIT (WebAssembly Interface Types) component model.

## Overview

Min is designed as a **foundation to build upon**, not as a complete language. It provides:

- **Minimal core concepts** - Only essential features in the base language
- **Maximum tooling** - Professional LSP, incremental compilation, excellent error messages
- **Easy to fork** - Clean architecture makes it simple to extend into different paradigms
- **Expression-based** - Everything is an expression, explicit and readable
- **Structural typing** - Types are checked by structure, not name
- **WebAssembly first** - Compiles to WASM components via WIT

## Quick Example

```min
type Point = { x: int, y: int }

fn distance:
  """Calculates distance from origin."""
  @pure
  p: Point
-> float {
  let x_sq = p.x * p.x;
  let y_sq = p.y * p.y;
  sqrt(x_sq + y_sq)
}

fn main:
  """Entry point."""
-> int {
  let point = { x: 3, y: 4 };
  let dist = distance(p: point);
  return 5  // Expected distance
}
```

## Features

### Language Features

- ✅ **Pure functions** - No effects, side effects added in extensions
- ✅ **Structural typing** - Types match by structure, not name
- ✅ **Type inference** - Local variables inferred, function signatures explicit
- ✅ **Named parameters** - All function calls use named params
- ✅ **Expression-based** - If/blocks are expressions, last expression returns
- ✅ **String interpolation** - `"Hello, {name}!"`
- ✅ **Attributes** - Structured metadata for functions and parameters
- ✅ **Docstrings** - Markdown documentation built into syntax

### Tooling Features

- ✅ **Incremental compilation** - Salsa-based query system
- ✅ **Error-tolerant parsing** - Continue parsing after errors
- ✅ **Full LSP support** - All IDE features (completion, hover, goto def, etc.)
- ✅ **Excellent diagnostics** - Clear, actionable error messages
- ✅ **Fast compilation** - Minimal IR layers, efficient code generation
- ✅ **VS Code extension** - Syntax highlighting and LSP integration

### Type System

- **Primitives:** `int`, `float`, `bool`, `string`
- **Structs:** `{ x: int, y: int }`
- **Type aliases:** `type Point = { x: int, y: int }`
- **Generics:** `List<T>`, `Option<T>` (basic support)
- **Recursive types:** Self-referential types for lists, trees, etc.

## Installation

```bash
# Install from source (Rust 1.70+ required)
cargo install --path crates/min-cli

# Or download binary from releases
# (Coming soon)
```

## Usage

### Compile a Program

```bash
# Compile to WASM component
min build example.min -o example.wasm

# Check for errors without compiling
min check example.min

# Format code
min format example.min
```

### VS Code Extension

1. Install the Min extension from the marketplace
2. Open a `.min` file
3. Enjoy full IDE support!

### LSP Server

```bash
# Start LSP server (for editors)
min-lsp

# TCP mode for debugging
min-lsp --tcp 9257
```

## Documentation

- [Language Specification](docs/LANGUAGE_SPEC.md) - Complete language reference
- [Compiler Architecture](docs/COMPILER_ARCHITECTURE.md) - How the compiler works
- [LSP Design](docs/LSP_DESIGN.md) - Language Server Protocol implementation
- [Examples](docs/EXAMPLES.md) - Example programs demonstrating features
- [Implementation Roadmap](docs/IMPLEMENTATION_ROADMAP.md) - Development phases

## Project Structure

```
min-lang/
├── crates/
│   ├── min-syntax/      # Lexer, Parser, CST, AST
│   ├── min-hir/         # High-level IR
│   ├── min-types/       # Type system
│   ├── min-mir/         # Mid-level IR
│   ├── min-codegen/     # WASM/WIT generation
│   ├── min-db/          # Salsa query database
│   ├── min-diagnostics/ # Error reporting
│   ├── min-lsp/         # Language Server
│   └── min-cli/         # Command-line interface
├── editors/
│   └── vscode/          # VS Code extension
└── docs/                # Documentation
```

## Philosophy

Min is designed around these principles:

1. **Minimal core, maximal tooling** - Few language features, excellent developer experience
2. **Explicit over implicit** - Readable code is more important than concise code
3. **Structural over nominal** - Types are what they contain, not what they're called
4. **Expressions over statements** - Everything evaluates to a value
5. **Error-tolerant** - The compiler should help, not obstruct
6. **Extensible** - Easy to fork into different paradigms

## Forking Min

Min is designed to be forked! Some potential directions:

### ML-Style Fork

Add:
- Pattern matching
- Algebraic data types
- First-class functions/lambdas
- Immutable-first collections

### Imperative Fork

Add:
- Mutable variables (`mut`)
- While/for loops
- Statements vs expressions
- Imperative control flow

### Templating Fork

Add:
- Text generation syntax
- Markup literals
- Template composition
- Output blocks

### Effects Fork

Add:
- I/O operations
- Import/export system
- Async/await
- Effect handlers

The minimal core ensures these extensions don't conflict.

## Compiler Architecture

```
Source Code
    ↓
CST (Concrete Syntax Tree)
    ↓
AST (Abstract Syntax Tree)
    ↓
HIR (High-level IR)
    ↓
Typed HIR (with type annotations)
    ↓
MIR (Mid-level IR)
    ↓
WIT + WASM Component
```

All stages use **Salsa** for incremental compilation, ensuring fast rebuilds when editing code.

## Contributing

Contributions are welcome! This is an early-stage project.

**Areas where help is needed:**
- Standard library design
- Optimization passes
- Better error messages
- Editor extensions (Vim, Emacs, etc.)
- Documentation improvements
- Test coverage

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines (coming soon).

## Development

### Prerequisites

- Rust 1.70+
- Node.js 18+ (for VS Code extension)

### Build from Source

```bash
# Clone the repo
git clone https://github.com/yourusername/min-lang.git
cd min-lang

# Build all crates
cargo build --release

# Run tests
cargo test

# Build VS Code extension
cd editors/vscode
npm install
npm run build
```

### Running Tests

```bash
# Unit tests
cargo test

# Integration tests
cargo test --test integration

# Snapshot tests
cargo insta test

# LSP tests
cd crates/min-lsp
cargo test
```

## Roadmap

See [IMPLEMENTATION_ROADMAP.md](docs/IMPLEMENTATION_ROADMAP.md) for detailed phases.

**Current Status:** 🚧 Phase 1 - Foundation (In Progress)

- [x] Language specification
- [x] Compiler architecture design
- [x] LSP design
- [ ] Lexer implementation
- [ ] Parser implementation
- [ ] Type system implementation
- [ ] Code generation
- [ ] LSP server
- [ ] VS Code extension

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

Min draws inspiration from:

- **Rust** - Expression-based syntax, structural patterns
- **OCaml/ReasonML** - ML-family syntax, type inference
- **TypeScript** - Structural typing, developer experience
- **Gleam** - Simplicity, excellent tooling
- **Zig** - Explicit design, compile-time guarantees
- **Elixir** - Documentation-first syntax

Special thanks to the Rust, Salsa, and WebAssembly communities.

## FAQ

### Why create another language?

Min isn't trying to be a complete language. It's a **minimal foundation** designed for maximum tooling quality and easy extensibility. Think of it as a starter kit for language designers.

### Why WebAssembly?

WebAssembly (via the Component Model) provides:
- Portable, sandboxed execution
- Rich type system (via WIT)
- Language interoperability
- Growing ecosystem

### Why structural typing?

Structural typing is:
- Simpler to implement
- More flexible
- Better for prototyping
- Easier to understand

It's also closer to how WIT works internally.

### Can I use this in production?

Not yet! This is an early-stage research project. Wait for v1.0.

### How can I help?

- Try the language and report bugs
- Contribute code or documentation
- Share ideas for language extensions
- Build editor extensions
- Spread the word!

## Contact

- GitHub: [github.com/yourusername/min-lang](https://github.com/yourusername/min-lang)
- Issues: [github.com/yourusername/min-lang/issues](https://github.com/yourusername/min-lang/issues)
- Discussions: [github.com/yourusername/min-lang/discussions](https://github.com/yourusername/min-lang/discussions)

---

**Min Language** - Minimal foundation, maximum tooling.
