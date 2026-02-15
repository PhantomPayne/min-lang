# Technical Specifications

This document provides detailed technical specifications for implementing Min language components.

## Lexer Specification

### Token Types

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Keywords
    Fn,           // fn
    Let,          // let
    Return,       // return
    If,           // if
    Else,         // else
    Type,         // type
    And,          // and
    Or,           // or
    Not,          // not
    
    // Literals
    IntLit,       // 123
    FloatLit,     // 3.14
    StringLit,    // "hello"
    BoolLit,      // true, false
    
    // Identifiers
    Ident,        // foo, bar_baz
    
    // Operators
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Eq,           // ==
    Neq,          // !=
    Lt,           // <
    Gt,           // >
    Lte,          // <=
    Gte,          // >=
    Assign,       // =
    
    // Delimiters
    LParen,       // (
    RParen,       // )
    LBrace,       // {
    RBrace,       // }
    Comma,        // ,
    Colon,        // :
    Semicolon,    // ;
    Arrow,        // ->
    Dot,          // .
    At,           // @
    
    // String interpolation
    StringStart,  // "hello {
    StringMiddle, // } world {
    StringEnd,    // } end"
    
    // Trivia
    Whitespace,
    Comment,      // // comment
    DocString,    // """..."""
    
    // Special
    Eof,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}
```

### Lexer Rules

**Keywords:** Matched exactly
```
fn, let, return, if, else, type, and, or, not, true, false
```

**Identifiers:** `[a-zA-Z_][a-zA-Z0-9_]*` (excluding keywords)

**Integer literals:** `[0-9]+`

**Float literals:** `[0-9]+\.[0-9]+`

**String literals:**
- Simple: `"[^"]*"`
- With interpolation: `"text {expr} more {expr}"`
  - Tokenized as: `StringStart`, expression tokens, `StringMiddle`, expression tokens, `StringEnd`

**Comments:** `//[^\n]*`

**Docstrings:** `"""[^"""]*"""`

**Operators:** Two-character operators checked first (`==`, `!=`, `<=`, `>=`, `->`), then single-char

### Error Recovery

- Invalid characters: Emit `Error` token, skip character, continue
- Unterminated strings: Emit `Error`, consume to end of line
- Unterminated docstrings: Emit `Error`, consume to EOF

## Parser Specification

### Grammar (Informal BNF)

```
SourceFile := Item*

Item := Function | TypeAlias

Function := 
    'fn' Ident ':'
    Attribute*
    DocString?
    Param*
    '->' Type
    Block

Param :=
    Ident ':' Type ('=' Expr)?
    Attribute*
    DocString?

TypeAlias := 'type' Ident '=' Type

Type := 
    | 'int' | 'float' | 'bool' | 'string'
    | Ident
    | Ident '<' Type (',' Type)* '>'
    | '{' StructField (',' StructField)* '}'

StructField := Ident ':' Type

Expr :=
    | Literal
    | Ident
    | StructExpr
    | CallExpr
    | IfExpr
    | Block
    | BinaryExpr
    | UnaryExpr

Literal := IntLit | FloatLit | StringLit | BoolLit

StructExpr := '{' FieldInit (',' FieldInit)* '}'

FieldInit := Ident ':' Expr

CallExpr := Ident '(' NamedArg (',' NamedArg)* ')'

NamedArg := Ident ':' Expr

IfExpr := 'if' Expr Block ('else' (IfExpr | Block))?

Block := '{' Stmt* Expr? '}'

Stmt := 
    | LetStmt
    | ExprStmt

LetStmt := 'let' Ident (':' Type)? '=' Expr ';'

ExprStmt := Expr ';'

BinaryExpr := Expr BinOp Expr

BinOp := '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '>' | '<=' | '>=' | 'and' | 'or'

UnaryExpr := UnOp Expr

UnOp := '-' | 'not'

Attribute := '@' Ident (':' AttrValue)?

AttrValue := Ident | StringLit | IntLit | BoolLit

DocString := '"""' text '"""'
```

### Operator Precedence

From lowest to highest:

1. `or`
2. `and`
3. `==`, `!=`
4. `<`, `>`, `<=`, `>=`
5. `+`, `-`
6. `*`, `/`, `%`
7. `not`, `-` (unary)
8. `.` (field access), `()` (call)

### Error Recovery Strategies

**Missing tokens:**
- Insert synthetic token
- Add error to diagnostics
- Continue parsing

**Unexpected tokens:**
- Skip tokens until synchronization point
- Synchronization points: `fn`, `type`, `let`, `}`

**Incomplete constructs:**
- Create partial AST node with error marker
- Continue parsing siblings

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
1. Parse `fn broken:` successfully
2. Parse `name: string` as parameter
3. Expect `->` or more params, find `fn` instead
4. Report error: "Expected return type or parameter"
5. Create partial `Function` node for `broken`
6. Synchronize at `fn works`
7. Continue parsing `works` normally

## Type System Specification

### Type Representation

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Struct(Vec<Field>),
    Generic {
        name: String,
        params: Vec<Type>,
    },
    Var(TypeVar),  // For inference
    Error,         // For error recovery
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);
```

### Type Inference Algorithm

**Constraint-based inference using unification:**

1. **Constraint Collection:**
   ```rust
   struct Constraint {
       expected: Type,
       actual: Type,
       span: Span,
   }
   ```

2. **Constraint Generation:**
   - For each expression, generate constraints
   - Example: `x + y` generates:
     - `typeof(x) = Int`
     - `typeof(y) = Int`
     - `typeof(x + y) = Int`

3. **Unification:**
   ```rust
   fn unify(ty1: Type, ty2: Type) -> Result<Substitution, TypeError> {
       match (ty1, ty2) {
           (Type::Int, Type::Int) => Ok(Substitution::empty()),
           (Type::Var(v), ty) | (ty, Type::Var(v)) => {
               if ty.contains_var(v) {
                   Err(TypeError::OccursCheck)
               } else {
                   Ok(Substitution::singleton(v, ty))
               }
           }
           (Type::Struct(fields1), Type::Struct(fields2)) => {
               if fields1.len() != fields2.len() {
                   return Err(TypeError::FieldCountMismatch);
               }
               
               let mut subst = Substitution::empty();
               for (f1, f2) in fields1.iter().zip(fields2.iter()) {
                   if f1.name != f2.name {
                       return Err(TypeError::FieldNameMismatch);
                   }
                   let new_subst = unify(
                       subst.apply(&f1.ty),
                       subst.apply(&f2.ty)
                   )?;
                   subst = subst.compose(new_subst);
               }
               Ok(subst)
           }
           _ => Err(TypeError::TypeMismatch(ty1, ty2)),
       }
   }
   ```

4. **Solving:**
   - Unify all constraints
   - Apply substitutions
   - Assign concrete types

### Type Equality (Structural)

Two types are equal if:

- Both are the same primitive (`Int`, `Float`, `Bool`, `String`)
- Both are structs with same field names and types (in order)
- Both are the same generic with same type parameters

**Not based on:**
- Type alias names
- Variable names
- Source location

## MIR Specification

### MIR Node Types

```rust
#[derive(Debug, Clone)]
pub struct MirFunction {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign {
        target: Place,
        rvalue: Rvalue,
    },
}

#[derive(Debug, Clone)]
pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOp, Operand, Operand),
    UnaryOp(UnOp, Operand),
    StructInit(Vec<(String, Operand)>),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Place(Place),
    Constant(Constant),
}

#[derive(Debug, Clone)]
pub enum Place {
    Local(usize),
    Field { base: Box<Place>, field: String },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return(Operand),
    Goto(usize),
    If {
        cond: Operand,
        then_block: usize,
        else_block: usize,
    },
}
```

### HIR → MIR Lowering

**Example:**

```min
// HIR
fn compute(x: int) -> int {
  let doubled = x * 2;
  let result = doubled + 1;
  result
}

// MIR (pseudo-code)
fn compute:
  bb0:
    _1 = x * 2              // doubled
    _2 = _1 + 1             // result
    return _2
```

**Complex example with if:**

```min
// HIR
let result = if x > 5 { x * 2 } else { x + 1 }

// MIR
bb0:
    _1 = x > 5
    if _1 goto bb1 else bb2

bb1:
    _2 = x * 2
    goto bb3

bb2:
    _2 = x + 1
    goto bb3

bb3:
    result = _2
    // continue...
```

## WIT Generation Specification

### Type Mapping

| Min Type | WIT Type |
|----------|----------|
| `int` | `s32` |
| `float` | `f32` |
| `bool` | `bool` |
| `string` | `string` |
| `{ x: int, y: int }` | `record { x: s32, y: s32 }` |

### Canonicalization Algorithm

```rust
fn canonicalize_types(program: &TypedHir) -> HashMap<Type, String> {
    let mut canonical_names = HashMap::new();
    let mut type_aliases = HashMap::new();
    
    // Step 1: Collect type aliases
    for alias in program.type_aliases() {
        let structure = resolve_to_structure(alias.ty);
        type_aliases.entry(structure)
            .or_insert_with(Vec::new)
            .push(alias.name.clone());
    }
    
    // Step 2: Assign names
    for (structure, aliases) in type_aliases {
        let name = if aliases.is_empty() {
            generate_name(&structure)
        } else {
            // Pick first alphabetically for determinism
            aliases.into_iter().min().unwrap()
        };
        
        canonical_names.insert(structure, name);
    }
    
    canonical_names
}

fn generate_name(ty: &Type) -> String {
    // Generate based on structure hash
    format!("type_{}", hash(ty))
}
```

### WIT Output Format

```wit
package min:program;

// Type definitions
record point {
  x: s32,
  y: s32,
}

// World (entry point)
world min-program {
  // Exported functions
  export add: func(a: s32, b: s32) -> s32;
  export make-point: func(x: s32, y: s32) -> point;
  export distance: func(p: point) -> f32;
}
```

## Diagnostic Format Specification

### Diagnostic Structure

```rust
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub primary_label: Label,
    pub secondary_labels: Vec<Label>,
    pub notes: Vec<String>,
    pub fixes: Vec<Fix>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct Fix {
    pub message: String,
    pub edits: Vec<TextEdit>,
}

#[derive(Debug, Clone)]
pub struct TextEdit {
    pub span: Span,
    pub new_text: String,
}
```

### Output Format

Using `ariadne` for nice error messages:

```
error[E0001]: type mismatch
  ┌─ example.min:5:10
  │
5 │   let x = "hello" + 5;
  │           -------   - expected string, found int
  │           │
  │           this is string
  │
  = note: cannot add int to string
  = help: convert the int to string first
```

## LSP Protocol Mapping

### Span → LSP Range

```rust
fn span_to_lsp_range(span: Span, source: &str) -> lsp_types::Range {
    let mut line = 0;
    let mut col = 0;
    let mut pos = 0;
    
    let mut start_pos = None;
    let mut end_pos = None;
    
    for ch in source.chars() {
        if pos == span.start as usize {
            start_pos = Some(lsp_types::Position { line, character: col });
        }
        if pos == span.end as usize {
            end_pos = Some(lsp_types::Position { line, character: col });
            break;
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        
        pos += ch.len_utf8();
    }
    
    lsp_types::Range {
        start: start_pos.unwrap(),
        end: end_pos.unwrap(),
    }
}
```

### Completion Items

```rust
fn create_completion_item(
    name: &str,
    kind: CompletionItemKind,
    ty: &Type,
    docs: Option<&str>,
) -> lsp_types::CompletionItem {
    lsp_types::CompletionItem {
        label: name.to_string(),
        kind: Some(kind),
        detail: Some(format!("{}", ty)),
        documentation: docs.map(|d| {
            lsp_types::Documentation::MarkupContent(
                lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: d.to_string(),
                }
            )
        }),
        ..Default::default()
    }
}
```

## Performance Targets

### Compilation Speed

- **Small files (< 100 LOC):** < 50ms
- **Medium files (< 1000 LOC):** < 200ms
- **Large files (< 10000 LOC):** < 2s

### LSP Response Time

- **Completion:** < 50ms
- **Hover:** < 20ms
- **Go to definition:** < 20ms
- **Diagnostics:** < 100ms (on change)

### Memory Usage

- **Compiler:** < 50MB for typical program
- **LSP server:** < 100MB with multiple files open

## Testing Requirements

### Test Coverage

- **Parser:** > 90% coverage
- **Type checker:** > 95% coverage
- **Code generation:** > 85% coverage
- **LSP handlers:** > 80% coverage

### Test Categories

1. **Unit tests:** Individual function/method tests
2. **Snapshot tests:** CST/AST/IR output verification
3. **Integration tests:** Full pipeline tests
4. **Error tests:** Diagnostic output verification
5. **LSP tests:** Feature-specific tests
6. **Performance tests:** Compilation speed benchmarks

### Example Test Structure

```rust
#[test]
fn test_parse_function() {
    let source = r#"
        fn add:
          a: int
          b: int
        -> int {
          return a + b
        }
    "#;
    
    let tokens = lex(source);
    let cst = parse(tokens);
    
    assert_no_errors(&cst);
    expect_test::expect![[r#"
        Function {
            name: "add",
            params: [
                Param { name: "a", ty: Int },
                Param { name: "b", ty: Int },
            ],
            return_type: Int,
            body: Block { ... },
        }
    "#]].assert_debug_eq(&cst);
}
```

This completes the technical specifications. These should provide enough detail for implementation!
