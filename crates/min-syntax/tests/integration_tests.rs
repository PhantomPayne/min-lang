use min_syntax::parser::parse;
use min_syntax::ast::*;

/// Helper: parse and assert no errors.
fn parse_ok(src: &str) -> SourceFile {
    let (ast, errors) = parse(src);
    if !errors.is_empty() {
        for e in &errors {
            eprintln!("  error: {}", e.message);
        }
        panic!("expected no errors, got {}", errors.len());
    }
    ast
}

/// Helper: count functions in the AST.
fn count_functions(ast: &SourceFile) -> usize {
    ast.items.iter().filter(|i| matches!(i, Item::Function(_))).count()
}

/// Helper: count type aliases in the AST.
fn count_type_aliases(ast: &SourceFile) -> usize {
    ast.items.iter().filter(|i| matches!(i, Item::TypeAlias(_))).count()
}

/// Helper: get function by name.
fn get_function<'a>(ast: &'a SourceFile, name: &str) -> &'a Function {
    ast.items.iter().find_map(|i| match i {
        Item::Function(f) if f.name.name == name => Some(f),
        _ => None,
    }).unwrap_or_else(|| panic!("function '{name}' not found"))
}

// ═══════════════════════════════════════════════════════════════════════
// Example 1: Simple Arithmetic
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example1_simple_arithmetic() {
    let src = r#"
fn add:
  """Adds two integers."""
  a: int
  b: int
-> int {
  return a + b
}

fn multiply:
  """Multiplies two integers."""
  x: int
  y: int
-> int {
  return x * y
}

fn main:
  """Main entry point."""
-> int {
  let sum = add(a: 5, b: 3);
  let product = multiply(x: sum, y: 2);
  return product
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 3);

    let add = get_function(&ast, "add");
    assert_eq!(add.params.len(), 2);
    assert!(add.doc.is_some());
    assert_eq!(add.doc.as_ref().unwrap().content, "Adds two integers.");

    let main = get_function(&ast, "main");
    assert_eq!(main.params.len(), 0);
    assert_eq!(main.body.stmts.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 2: Structs and Type Aliases
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example2_structs_and_type_aliases() {
    let src = r#"
type Point = {
  x: int,
  y: int
}

type Rectangle = {
  top_left: Point,
  width: int,
  height: int
}

fn make_point:
  """Creates a point at the given coordinates."""
  x: int
  y: int
-> Point {
  return { x: x, y: y }
}

fn area:
  """Calculates the area of a rectangle."""
  rect: Rectangle
-> int {
  return rect.width * rect.height
}

fn main:
  """Demonstrates struct usage."""
-> int {
  let origin = make_point(x: 0, y: 0);
  let rect = {
    top_left: origin,
    width: 10,
    height: 20
  };
  return area(rect: rect)
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_type_aliases(&ast), 2);
    assert_eq!(count_functions(&ast), 3);

    let area = get_function(&ast, "area");
    // Body contains: return rect.width * rect.height
    let tail = area.body.tail_expr.as_ref().unwrap();
    assert!(matches!(tail.as_ref(), Expr::Return(_)));
}

// ═══════════════════════════════════════════════════════════════════════
// Example 3: Conditionals
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example3_conditionals() {
    let src = r#"
fn abs:
  """Returns the absolute value of a number."""
  x: int
-> int {
  return if x < 0 { -x } else { x }
}

fn sign:
  """Returns the sign of a number."""
  x: int
-> string {
  if x > 0 {
    return "positive"
  } else {
    if x < 0 {
      return "negative"
    } else {
      return "zero"
    }
  }
}

fn clamp:
  """Clamps a value between min and max."""
  value: int
  min: int
  max: int
-> int {
  if value < min {
    return min
  } else {
    if value > max {
      return max
    } else {
      return value
    }
  }
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 3);

    let abs = get_function(&ast, "abs");
    assert_eq!(abs.params.len(), 1);

    let clamp = get_function(&ast, "clamp");
    assert_eq!(clamp.params.len(), 3);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 4: String Interpolation
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example4_string_interpolation() {
    let src = r#"
fn greet:
  """Greets a person with a formatted message."""
  name: string
  age: int
-> string {
  return "Hello, {name}! You are {age} years old."
}

fn format_point:
  """Formats a point as a string."""
  x: int
  y: int
-> string {
  return "Point({x}, {y})"
}

fn describe_rectangle:
  """Describes a rectangle."""
  width: int
  height: int
-> string {
  let area = width * height;
  return "Rectangle {width}x{height} with area {area}"
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 3);

    let greet = get_function(&ast, "greet");
    assert_eq!(greet.params.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 5: Blocks and Scoping
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example5_blocks_and_scoping() {
    let src = r#"
fn compute:
  """Demonstrates block expressions and scoping."""
  x: int
-> int {
  let doubled = {
    let temp = x * 2;
    temp + 1
  };

  let result = {
    let a = doubled;
    let b = x;
    a + b
  };

  return result
}

fn complex_calculation:
  """Shows nested blocks."""
  input: int
-> int {
  let step1 = {
    let a = input * 2;
    let b = a + 10;
    b
  };

  let step2 = {
    let c = step1 - 5;
    c * 3
  };

  return step1 + step2
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 2);

    let compute = get_function(&ast, "compute");
    // Should have 2 let statements and a tail return
    assert_eq!(compute.body.stmts.len(), 2);
    assert!(compute.body.tail_expr.is_some());
}

// ═══════════════════════════════════════════════════════════════════════
// Example 6: Attributes and Documentation
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example6_attributes_and_documentation() {
    let src = r#"
fn legacy_add:
  @deprecated: "Use add_v2 instead"
  """
  Legacy addition function.
  """
  a: int
  b: int
-> int {
  return a + b
}

fn add_v2:
  @pure
  """
  Improved addition function.
  """
  a: int
  b: int
-> int {
  return a + b
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 2);

    let legacy = get_function(&ast, "legacy_add");
    assert!(!legacy.attributes.is_empty());

    let v2 = get_function(&ast, "add_v2");
    assert!(!v2.attributes.is_empty());
    assert_eq!(v2.attributes[0].name.name, "pure");
}

// ═══════════════════════════════════════════════════════════════════════
// Example 7: Structural Typing
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example7_structural_typing() {
    let src = r#"
type Point2D = { x: int, y: int }
type Vector2D = { x: int, y: int }
type Coordinate = { x: int, y: int }

fn add_vectors:
  """Adds two vectors."""
  a: Vector2D
  b: Vector2D
-> Vector2D {
  return {
    x: a.x + b.x,
    y: a.y + b.y
  }
}

fn main:
  """Demonstrates structural typing."""
-> int {
  let point: Point2D = { x: 3, y: 4 };
  let vector: Vector2D = { x: 1, y: 2 };
  let result = add_vectors(a: point, b: vector);
  return result.x + result.y
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_type_aliases(&ast), 3);
    assert_eq!(count_functions(&ast), 2);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 8: Default Parameters
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example8_default_parameters() {
    let src = r#"
fn greet_person:
  """Greets a person with optional title and formality."""
  name: string
  title: string = "Mr."
  formal: bool = true
-> string {
  let greeting = if formal { "Good day" } else { "Hey" };
  return "{greeting}, {title} {name}!"
}

fn create_user:
  """Creates a user with optional fields."""
  name: string
  email: string
  age: int = 0
  verified: bool = false
-> string {
  return "User: {name} ({email}), age {age}, verified: {verified}"
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 2);

    let greet = get_function(&ast, "greet_person");
    assert_eq!(greet.params.len(), 3);
    assert!(greet.params[0].default.is_none());
    assert!(greet.params[1].default.is_some());
    assert!(greet.params[2].default.is_some());

    let create = get_function(&ast, "create_user");
    assert_eq!(create.params.len(), 4);
    assert!(create.params[2].default.is_some());
    assert!(create.params[3].default.is_some());
}

// ═══════════════════════════════════════════════════════════════════════
// Example 9: Nested Structs
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example9_nested_structs() {
    let src = r#"
type Address = {
  street: string,
  city: string,
  zip: int
}

type Person = {
  name: string,
  age: int,
  address: Address
}

fn format_address:
  """Formats an address as a string."""
  addr: Address
-> string {
  return "{addr.street}, {addr.city} {addr.zip}"
}

fn describe_person:
  """Describes a person with their address."""
  person: Person
-> string {
  let addr_str = format_address(addr: person.address);
  return "{person.name}, age {person.age}, lives at {addr_str}"
}

fn main:
  """Creates and describes a person."""
-> string {
  let person = {
    name: "Alice",
    age: 30,
    address: {
      street: "123 Main St",
      city: "Springfield",
      zip: 12345
    }
  };

  return describe_person(person: person)
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_type_aliases(&ast), 2);
    assert_eq!(count_functions(&ast), 3);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 10: Complex Computation
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example10_complex_computation() {
    let src = r#"
type Circle = {
  radius: float
}

type Rectangle = {
  width: float,
  height: float
}

fn circle_area:
  """Calculates the area of a circle."""
  @pure
  circle: Circle
-> float {
  let pi = 3.14159;
  return pi * circle.radius * circle.radius
}

fn rectangle_area:
  """Calculates the area of a rectangle."""
  @pure
  rect: Rectangle
-> float {
  return rect.width * rect.height
}

fn max:
  """Returns the maximum of two floats."""
  @pure
  a: float
  b: float
-> float {
  return if a > b { a } else { b }
}

fn larger_area:
  """Returns the larger area."""
  circle_radius: float
  rect_width: float
  rect_height: float
-> float {
  let circle = { radius: circle_radius };
  let rect = { width: rect_width, height: rect_height };

  let c_area = circle_area(circle: circle);
  let r_area = rectangle_area(rect: rect);

  return max(a: c_area, b: r_area)
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_type_aliases(&ast), 2);
    assert_eq!(count_functions(&ast), 4);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 11: Early Returns
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example11_early_returns() {
    let src = r#"
fn divide:
  """Divides two numbers, returns 0 if divisor is zero."""
  numerator: int
  denominator: int
-> int {
  if denominator == 0 {
    return 0;
  }

  return numerator / denominator
}

fn validate_and_process:
  """Validates input and processes it."""
  value: int
-> int {
  if value < 0 {
    return 0;
  }

  if value > 100 {
    return 100;
  }

  let processed = value * 2;
  let adjusted = processed + 10;
  return adjusted
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 2);

    let divide = get_function(&ast, "divide");
    assert_eq!(divide.params.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════
// Example 13: Semicolon Usage
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn example13_semicolons() {
    let src = r#"
fn demonstrate_semicolons:
  """Shows explicit semicolon usage."""
  x: int
-> int {
  let a = x * 2;
  let b = a + 10;
  let c = b - 5;
  c * 3
}

fn with_explicit_return:
  """Shows explicit return."""
  x: int
-> int {
  let doubled = x * 2;
  return doubled + 1
}

fn mixed_style:
  """Mixes implicit and explicit returns."""
  x: int
-> int {
  if x < 0 {
    return 0;
  }

  let processed = x * 2;
  processed + 1
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 3);

    let demo = get_function(&ast, "demonstrate_semicolons");
    assert_eq!(demo.body.stmts.len(), 3); // 3 let statements
    assert!(demo.body.tail_expr.is_some()); // c * 3 is tail expr
}

// ═══════════════════════════════════════════════════════════════════════
// Additional edge case tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn deeply_nested_if_else() {
    let src = r#"
fn classify:
  x: int
-> string {
  if x > 100 {
    "huge"
  } else {
    if x > 50 {
      "big"
    } else {
      if x > 10 {
        "medium"
      } else {
        "small"
      }
    }
  }
}
"#;
    let ast = parse_ok(src);
    assert_eq!(count_functions(&ast), 1);
}

#[test]
fn chained_field_access() {
    let src = r#"
fn get_zip:
  person: Person
-> int {
  person.address.zip
}
"#;
    let ast = parse_ok(src);
    let f = get_function(&ast, "get_zip");
    let tail = f.body.tail_expr.as_ref().unwrap();
    // Should be field access chain: (person.address).zip
    match tail.as_ref() {
        Expr::FieldAccess(fa) => {
            assert_eq!(fa.field.name, "zip");
            match &fa.expr {
                Expr::FieldAccess(inner) => {
                    assert_eq!(inner.field.name, "address");
                }
                _ => panic!("expected inner field access"),
            }
        }
        _ => panic!("expected field access"),
    }
}

#[test]
fn complex_expression_precedence() {
    // a + b * c - d / e should parse as (a + (b * c)) - (d / e)
    let src = r#"
fn f: -> int {
  a + b * c - d / e
}
"#;
    let ast = parse_ok(src);
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Binary(bin) => {
            assert_eq!(bin.op, BinOp::Sub);
        }
        _ => panic!("expected binary sub"),
    }
}

#[test]
fn empty_function_body_with_literal() {
    let src = "fn zero: -> int { 0 }";
    let ast = parse_ok(src);
    let f = get_function(&ast, "zero");
    assert_eq!(f.body.stmts.len(), 0);
    assert!(f.body.tail_expr.is_some());
    match f.body.tail_expr.as_ref().unwrap().as_ref() {
        Expr::IntLit(0, _) => {}
        other => panic!("expected IntLit(0), got {other:?}"),
    }
}

#[test]
fn boolean_expression_precedence() {
    // not a and b or c should parse as ((not a) and b) or c
    let src = r#"
fn f:
  a: bool
  b: bool
  c: bool
-> bool {
  not a and b or c
}
"#;
    let ast = parse_ok(src);
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Binary(bin) => {
            assert_eq!(bin.op, BinOp::Or);
            match &bin.left {
                Expr::Binary(inner) => {
                    assert_eq!(inner.op, BinOp::And);
                    match &inner.left {
                        Expr::Unary(u) => {
                            assert_eq!(u.op, UnaryOp::Not);
                        }
                        _ => panic!("expected unary not"),
                    }
                }
                _ => panic!("expected binary and"),
            }
        }
        _ => panic!("expected binary or"),
    }
}

#[test]
fn multiple_type_aliases_on_same_structure() {
    let src = r#"
type Point2D = { x: int, y: int }
type Vector2D = { x: int, y: int }
type Coord = { x: int, y: int }
"#;
    let ast = parse_ok(src);
    assert_eq!(count_type_aliases(&ast), 3);
}

#[test]
fn parenthesized_expression() {
    let src = r#"
fn f: -> int { (a + b) * c }
"#;
    let ast = parse_ok(src);
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Binary(bin) => {
            assert_eq!(bin.op, BinOp::Mul);
            // Left side should be a + b (parenthesized)
            match &bin.left {
                Expr::Binary(inner) => {
                    assert_eq!(inner.op, BinOp::Add);
                }
                _ => panic!("expected binary add in parens"),
            }
        }
        _ => panic!("expected binary mul"),
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Lexer-level integration tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn lex_full_program() {
    use min_syntax::Lexer;

    let src = r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}
"#;
    let (tokens, errors) = Lexer::new(src).tokenize();
    assert!(errors.is_empty());
    // Should have many tokens including Eof
    assert!(tokens.len() > 10);
    assert_eq!(tokens.last().unwrap().kind, min_syntax::TokenKind::Eof);
}

// ═══════════════════════════════════════════════════════════════════════
// Error recovery tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_from_missing_function_body() {
    let (ast, errors) = parse(
        "fn broken:\n  x: int\n\nfn works: -> int { 42 }",
    );
    assert!(!errors.is_empty());
    // Should still find the working function
    let funcs: Vec<_> = ast.items.iter().filter_map(|i| match i {
        Item::Function(f) => Some(f),
        _ => None,
    }).collect();
    assert!(funcs.iter().any(|f| f.name.name == "works"));
}

#[test]
fn recover_from_unexpected_token() {
    let (ast, errors) = parse(
        "??? fn valid: -> int { 1 }",
    );
    assert!(!errors.is_empty());
    // Should still parse the valid function
    let funcs: Vec<_> = ast.items.iter().filter_map(|i| match i {
        Item::Function(f) => Some(f),
        _ => None,
    }).collect();
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}
