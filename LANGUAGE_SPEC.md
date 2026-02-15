# Min Language Specification v0.1

## Overview

Min is a minimal, foundational programming language designed for maximum tooling and extensibility. It compiles to WebAssembly via the WIT (WebAssembly Interface Types) component model.

**Design Goals:**
- Minimal core concepts, maximum tooling quality
- Easy to fork into different paradigms (ML-style, imperative, templating)
- Expression-based with explicit, readable syntax
- Structural type system
- Professional LSP with full IDE support
- Error-tolerant parsing for excellent developer experience

**File Extension:** `.min`

## Syntax

### Comments

```min
// Single-line comment

// Multi-line comments use multiple single-line comments
// (no /* */ style for simplicity)
```

### Variables

```min
// Type inference
let x = 5

// Explicit type annotation
let y: int = 10

// All variables are immutable by default
// (mutability may be added in future forks)
```

### Primitive Types

- `int` - 32-bit signed integer (maps to WIT `s32`)
- `float` - 32-bit floating point (maps to WIT `f32`)
- `bool` - boolean (`true` or `false`)
- `string` - UTF-8 string

### Struct Types

Structs are structural records with named fields:

```min
// Anonymous struct
let point = { x: 10, y: 20 }

// Type is inferred as { x: int, y: int }

// Access fields
let x_val = point.x
```

### Type Aliases

Type aliases create names for structural types:

```min
type Point = { x: int, y: int }
type Vec2 = { x: int, y: int }

// Point and Vec2 are structurally identical
// They can be used interchangeably within .min code
let p: Point = { x: 1, y: 2 }
let v: Vec2 = p  // OK - same structure

// Type aliases are used for:
// 1. Documentation
// 2. WIT export naming
// 3. Error messages
```

### Generic Types

Explicit angle bracket syntax:

```min
type List<T> = { items: /* ... */ }
type Option<T> = /* ... */
type Result<T, E> = /* ... */
```

(Generic implementation details deferred to future design)

### Recursive Types

Types can reference themselves:

```min
type LinkedList<T> = {
  value: T,
  next: Option<LinkedList<T>>
}

type Tree<T> = {
  value: T,
  left: Option<Tree<T>>,
  right: Option<Tree<T>>
}
```

### Functions

Functions use vertical layout to support metadata and documentation:

```min
fn greet:
  @pure
  @since: v1.0
  """
  Greets a person with a customized message.
  
  ## Examples
  ```
  greet(name: "Alice", age: 30)
  # => "Hello, Alice"
  ```
  """
  
  name: string
    @validate: non-empty
    """The person's name."""
  
  age: int
    """The person's age."""
  
  title: string = "Mr."
    """Optional honorific title."""
    
-> string {
  return "Hello, {title} {name}"
}
```

**Function syntax breakdown:**

- `fn name:` - Function declaration with colon
- Attributes: `@key: value` - Structured metadata
- Docstrings: `"""..."""` - Markdown-formatted documentation
- Parameters: One per line with type annotations
- Parameter docs: Can be inline `# comment` or multi-line `"""..."""`
- Default values: `param: type = default`
- Return type: `-> type`
- Body: Block with braces

**Function calls:**

All function calls use named parameters:

```min
greet(name: "Alice", age: 30)
greet(name: "Bob", age: 25, title: "Dr.")

// Zero-parameter functions
get_time()
```

### Conditionals

If expressions with implicit returns:

```min
// Expression-based
let result = if x > 5 {
  "big"
} else {
  "small"
}

// Multi-statement blocks
let result = if condition {
  log("Condition is true");
  let value = compute();
  value * 2  // Last expression is returned (no semicolon)
} else {
  0
}

// Single-line
let result = if x > 5 { "big" } else { "small" }

// Can use explicit return
let result = if x > 5 {
  return "big";
} else {
  return "small";
}
```

**Semicolon rule:**
- Statements end with `;`
- Last expression in a block (no `;`) is the block's value
- `return` can be used explicitly for early returns or clarity

### Operators

**Arithmetic:** `+`, `-`, `*`, `/`, `%`

**Comparison:** `==`, `!=`, `<`, `>`, `<=`, `>=`

**Logical:** `and`, `or`, `not` (words, not symbols)

```min
if x > 5 and y < 10 {
  // ...
}

let result = not condition
```

**String Interpolation:**

```min
let name = "Alice"
let age = 30
let message = "Hello, {name}! You are {age} years old."
```

No separate string concatenation operator - use interpolation.

### Blocks

Blocks create scope and can be expressions:

```min
let result = {
  let temp = compute();
  let doubled = temp * 2;
  doubled + 1  // returned
}
```

## Type System

### Structural Typing

Min uses structural typing internally:

- Two types are equivalent if they have the same structure
- Type aliases don't create new nominal types
- `{ x: int, y: int }` is the same type everywhere, regardless of names

```min
type Point = { x: int, y: int }
type Vec2 = { x: int, y: int }

fn distance(p: Point) -> float {
  let v: Vec2 = p  // OK - same structure
  return sqrt(v.x * v.x + v.y * v.y)
}
```

### Type Inference

- **Local variables:** Full inference
- **Function parameters:** Explicit types required
- **Function return types:** Explicit types required

```min
fn compute(x: int, y: int) -> int {
  let temp = x * 2;  // temp's type inferred as int
  let result = temp + y;  // result inferred as int
  result
}
```

### WIT Mapping

When generating WIT for WebAssembly Component Model:

1. **Structural deduplication:** Same structure = same WIT type
2. **Type alias names:** If a type alias exists, use that name in WIT
3. **Canonical naming:** For anonymous types, generate canonical names

**Example:**

```min
// Min code
type Point = { x: int, y: int }

fn make_point() -> Point {
  return { x: 10, y: 20 }
}

fn distance(p: Point) -> float {
  return sqrt(p.x * p.x + p.y * p.y)
}
```

**Generated WIT:**

```wit
record point {
  x: s32,
  y: s32
}

make-point: func() -> point
distance: func(p: point) -> f32
```

If multiple type aliases have the same structure, one is chosen (deterministically) for WIT export.

## Standard Library

The base language has **no effects** (I/O, imports, etc.) - it's pure functions only.

Basic operations like arithmetic, comparisons, and string operations need stdlib support:

**Approach:** Stdlib is WIT-imported primitives (implementation details TBD)

Examples of what stdlib will provide:
- `add(a: int, b: int) -> int`
- `concat(a: string, b: string) -> string`
- `sqrt(x: float) -> float`
- `to_string(x: int) -> string`

The stdlib should be:
- Tree-shakeable (only import what's used)
- Easy to extend without compiler changes
- Implemented as intrinsics or WIT imports

## Module System

**Version 0.1:** Single-file programs only

Module system deferred to future versions. This keeps the initial implementation simple.

## Effects and I/O

**Version 0.1:** No effects, imports, or I/O

Pure functions only. This makes the initial implementation focused and testable.

Effects will be added in future forks/versions with explicit effect systems.

## Attributes

Attributes provide structured metadata for tooling:

```min
@pure              // Boolean attribute
@since: v1.0       // String attribute
@complexity: O(n)  // Custom value
@deprecated: "Use compute_v2 instead"
```

**Built-in attributes (examples):**
- `@pure` - Function has no side effects
- `@deprecated: message` - Mark as deprecated
- `@since: version` - When added
- `@validate: rule` - Validation rule for parameters
- `@sensitive: bool` - Sensitive data flag

**Custom attributes:** Users can define their own for tooling/linting

Attributes are:
- Type-checked by the LSP
- Available to the compiler for optimizations
- Accessible to lint rules and analysis tools

## Syntax Summary

```min
// Variables
let x = 5
let y: int = 10

// Type aliases
type Point = { x: int, y: int }

// Structs
let p = { x: 1, y: 2 }
let x = p.x

// Functions
fn add:
  """Adds two numbers."""
  a: int
  b: int
-> int {
  return a + b
}

// Function calls
let result = add(a: 5, b: 3)

// Conditionals
let value = if condition {
  compute();
  42
} else {
  0
}

// Operators
let result = x + y * 2
let check = x > 5 and y < 10
let neg = not flag

// String interpolation
let msg = "Value: {result}"

// Blocks
let x = {
  let temp = 5;
  temp * 2
}
```

## Future Extensions

The language is designed to be easily forkable into different paradigms:

**Potential forks:**
- ML-style: Add pattern matching, algebraic data types, lambdas
- Imperative: Add mutable variables, loops, statements
- Templating: Add text generation, markup syntax
- Effects: Add I/O, async, import system

The minimal core ensures these extensions don't conflict with existing features.
