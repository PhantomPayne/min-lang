//! Integration tests for the type system: lowering + type checking.

use min_diagnostics::Diagnostic;
use min_hir::{type_check, LoweringContext, TypeCheckResult};
use min_syntax::Lexer;
use min_syntax::parser::Parser;

/// Helper: parse source, lower to HIR, and type-check.
/// Returns (TypeCheckResult, lowering_diagnostics, parse_diagnostics).
fn check(source: &str) -> (TypeCheckResult, Vec<Diagnostic>, Vec<Diagnostic>) {
    let lexer = Lexer::new(source);
    let (tokens, _lex_diags) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (ast, parse_diags) = parser.parse();

    let mut lower_ctx = LoweringContext::new();
    let module = lower_ctx.lower_source_file(&ast);
    let lower_diags = lower_ctx.diagnostics.clone();

    let result = type_check(&module, &lower_ctx);
    (result, lower_diags, parse_diags)
}

/// Helper: assert that type checking produces no errors at all.
fn assert_no_errors(source: &str) {
    let (result, lower_diags, parse_diags) = check(source);
    let all_errors: Vec<String> = parse_diags
        .iter()
        .chain(lower_diags.iter())
        .chain(result.diagnostics.iter())
        .map(|d| d.message.clone())
        .collect();
    assert!(
        all_errors.is_empty(),
        "expected no errors, got: {:#?}",
        all_errors
    );
}

/// Helper: assert that type checking produces at least one error containing
/// the given substring.
fn assert_has_error(source: &str, expected_substring: &str) {
    let (result, lower_diags, _parse_diags) = check(source);
    let all_errors: Vec<String> = lower_diags
        .iter()
        .chain(result.diagnostics.iter())
        .map(|d| d.message.clone())
        .collect();
    assert!(
        all_errors.iter().any(|e| e.contains(expected_substring)),
        "expected an error containing '{}', got: {:#?}",
        expected_substring,
        all_errors
    );
}

/// Helper: assert that type checking produces exactly the given number of errors.
fn assert_error_count(source: &str, expected: usize) {
    let (result, lower_diags, _) = check(source);
    let count = lower_diags.len() + result.diagnostics.len();
    assert_eq!(
        count, expected,
        "expected {} errors, got {}: {:#?}",
        expected,
        count,
        lower_diags
            .iter()
            .chain(result.diagnostics.iter())
            .map(|d| d.message.clone())
            .collect::<Vec<_>>()
    );
}

// ──────────────────────────────────────────────────────────────────────
// Well-typed programs: should produce zero errors
// ──────────────────────────────────────────────────────────────────────

#[test]
fn simple_int_function() {
    assert_no_errors(
        r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}
"#,
    );
}

#[test]
fn simple_float_function() {
    assert_no_errors(
        r#"
fn multiply:
  x: float
  y: float
-> float {
  return x * y
}
"#,
    );
}

#[test]
fn bool_function() {
    assert_no_errors(
        r#"
fn is_positive:
  x: int
-> bool {
  return x > 0
}
"#,
    );
}

#[test]
fn string_function() {
    assert_no_errors(
        r#"
fn greet:
  name: string
-> string {
  return "hello"
}
"#,
    );
}

#[test]
fn let_binding_type_inference() {
    assert_no_errors(
        r#"
fn compute:
  x: int
-> int {
  let doubled = x * 2;
  let result = doubled + 1;
  return result
}
"#,
    );
}

#[test]
fn let_binding_explicit_type() {
    assert_no_errors(
        r#"
fn test:
  x: int
-> int {
  let y: int = x + 1;
  return y
}
"#,
    );
}

#[test]
fn if_else_expression() {
    assert_no_errors(
        r#"
fn abs:
  x: int
-> int {
  return if x < 0 { 0 - x } else { x }
}
"#,
    );
}

#[test]
fn nested_if_else() {
    assert_no_errors(
        r#"
fn sign:
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
"#,
    );
}

#[test]
fn block_expression() {
    assert_no_errors(
        r#"
fn compute:
  x: int
-> int {
  let result = {
    let temp = x * 2;
    temp + 1
  };
  return result
}
"#,
    );
}

#[test]
fn function_call() {
    assert_no_errors(
        r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}

fn main:
-> int {
  let sum = add(a: 5, b: 3);
  return sum
}
"#,
    );
}

#[test]
fn function_call_chain() {
    assert_no_errors(
        r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}

fn multiply:
  x: int
  y: int
-> int {
  return x * y
}

fn main:
-> int {
  let sum = add(a: 5, b: 3);
  let product = multiply(x: sum, y: 2);
  return product
}
"#,
    );
}

#[test]
fn struct_literal_and_field_access() {
    assert_no_errors(
        r#"
fn main:
-> int {
  let point = { x: 3, y: 4 };
  return point.x + point.y
}
"#,
    );
}

#[test]
fn type_alias_and_struct_param() {
    assert_no_errors(
        r#"
type Point = { x: int, y: int }

fn get_x:
  p: Point
-> int {
  return p.x
}

fn main:
-> int {
  let p = { x: 10, y: 20 };
  return get_x(p: p)
}
"#,
    );
}

#[test]
fn structural_typing_different_aliases() {
    // Point and Vec2 have the same structure; should be interchangeable.
    assert_no_errors(
        r#"
type Point = { x: int, y: int }
type Vec2 = { x: int, y: int }

fn add_vectors:
  a: Vec2
  b: Vec2
-> Vec2 {
  return { x: a.x + b.x, y: a.y + b.y }
}

fn main:
-> int {
  let point: Point = { x: 3, y: 4 };
  let vector: Vec2 = { x: 1, y: 2 };
  let result = add_vectors(a: point, b: vector);
  return result.x + result.y
}
"#,
    );
}

#[test]
fn nested_struct_types() {
    assert_no_errors(
        r#"
type Address = { street: string, city: string, zip: int }
type Person = { name: string, age: int, address: Address }

fn get_zip:
  person: Person
-> int {
  return person.address.zip
}
"#,
    );
}

#[test]
fn string_interpolation() {
    assert_no_errors(
        r#"
fn greet:
  name: string
  age: int
-> string {
  return "Hello, {name}! You are {age} years old."
}
"#,
    );
}

#[test]
fn logical_operators() {
    assert_no_errors(
        r#"
fn check:
  x: int
  y: int
-> bool {
  return x > 0 and y > 0
}
"#,
    );
}

#[test]
fn unary_negation() {
    assert_no_errors(
        r#"
fn negate:
  x: int
-> int {
  return 0 - x
}
"#,
    );
}

#[test]
fn unary_not() {
    assert_no_errors(
        r#"
fn invert:
  flag: bool
-> bool {
  return not flag
}
"#,
    );
}

#[test]
fn early_return() {
    assert_no_errors(
        r#"
fn divide:
  numerator: int
  denominator: int
-> int {
  if denominator == 0 {
    return 0;
  }
  return numerator / denominator
}
"#,
    );
}

#[test]
fn complex_computation() {
    assert_no_errors(
        r#"
type Circle = { radius: float }

fn circle_area:
  circle: Circle
-> float {
  let pi = 3.14159;
  return pi * circle.radius * circle.radius
}
"#,
    );
}

#[test]
fn struct_return() {
    assert_no_errors(
        r#"
type Point = { x: int, y: int }

fn make_point:
  x: int
  y: int
-> Point {
  return { x: x, y: y }
}
"#,
    );
}

#[test]
fn example1_simple_arithmetic() {
    assert_no_errors(
        r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}

fn multiply:
  x: int
  y: int
-> int {
  return x * y
}

fn main:
-> int {
  let sum = add(a: 5, b: 3);
  let product = multiply(x: sum, y: 2);
  return product
}
"#,
    );
}

#[test]
fn example2_structs_and_type_aliases() {
    assert_no_errors(
        r#"
type Point = { x: int, y: int }

type Rectangle = {
  top_left: Point,
  width: int,
  height: int
}

fn make_point:
  x: int
  y: int
-> Point {
  return { x: x, y: y }
}

fn area:
  rect: Rectangle
-> int {
  return rect.width * rect.height
}

fn main:
-> int {
  let origin = make_point(x: 0, y: 0);
  let rect = {
    top_left: origin,
    width: 10,
    height: 20
  };
  return area(rect: rect)
}
"#,
    );
}

#[test]
fn example3_conditionals() {
    assert_no_errors(
        r#"
fn abs:
  x: int
-> int {
  return if x < 0 { 0 - x } else { x }
}

fn clamp:
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
"#,
    );
}

#[test]
fn example5_blocks_and_scoping() {
    assert_no_errors(
        r#"
fn compute:
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
"#,
    );
}

#[test]
fn example10_complex_computation() {
    assert_no_errors(
        r#"
type Circle = { radius: float }

type Rectangle = {
  width: float,
  height: float
}

fn circle_area:
  circle: Circle
-> float {
  let pi = 3.14159;
  return pi * circle.radius * circle.radius
}

fn rectangle_area:
  rect: Rectangle
-> float {
  return rect.width * rect.height
}

fn max:
  a: float
  b: float
-> float {
  return if a > b { a } else { b }
}

fn larger_area:
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
"#,
    );
}

#[test]
fn example11_early_returns() {
    assert_no_errors(
        r#"
fn divide:
  numerator: int
  denominator: int
-> int {
  if denominator == 0 {
    return 0;
  }
  return numerator / denominator
}

fn validate_and_process:
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
"#,
    );
}

#[test]
fn example13_semicolons() {
    assert_no_errors(
        r#"
fn demonstrate_semicolons:
  x: int
-> int {
  let a = x * 2;
  let b = a + 10;
  let c = b - 5;
  c * 3
}

fn with_explicit_return:
  x: int
-> int {
  let doubled = x * 2;
  return doubled + 1
}

fn mixed_style:
  x: int
-> int {
  if x < 0 {
    return 0;
  }
  let processed = x * 2;
  processed + 1
}
"#,
    );
}

// ──────────────────────────────────────────────────────────────────────
// Type error tests: should produce specific errors
// ──────────────────────────────────────────────────────────────────────

#[test]
fn error_type_mismatch_return() {
    assert_has_error(
        r#"
fn bad:
-> int {
  return "hello"
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_type_mismatch_binary_op() {
    assert_has_error(
        r#"
fn bad:
  x: int
-> int {
  return x + "hello"
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_type_mismatch_binary_different_numerics() {
    assert_has_error(
        r#"
fn bad:
  x: int
  y: float
-> int {
  return x + y
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_arithmetic_on_bool() {
    assert_has_error(
        r#"
fn bad:
  x: bool
-> int {
  return x + 1
}
"#,
        "cannot apply",
    );
}

#[test]
fn error_arithmetic_on_string() {
    assert_has_error(
        r#"
fn bad:
  x: string
-> int {
  return x * 2
}
"#,
        "cannot apply",
    );
}

#[test]
fn error_not_on_int() {
    assert_has_error(
        r#"
fn bad:
  x: int
-> bool {
  return not x
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_and_on_int() {
    assert_has_error(
        r#"
fn bad:
  x: int
  y: int
-> bool {
  return x and y
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_undefined_variable() {
    assert_has_error(
        r#"
fn bad:
-> int {
  return undefined_var
}
"#,
        "undefined variable",
    );
}

#[test]
fn error_undefined_function() {
    assert_has_error(
        r#"
fn bad:
-> int {
  return nonexistent(x: 1)
}
"#,
        "undefined function",
    );
}

#[test]
fn error_no_field_on_type() {
    assert_has_error(
        r#"
fn bad:
-> int {
  let p = { x: 1, y: 2 };
  return p.z
}
"#,
        "no field 'z'",
    );
}

#[test]
fn error_field_access_on_primitive() {
    assert_has_error(
        r#"
fn bad:
  x: int
-> int {
  return x.field
}
"#,
        "no field 'field'",
    );
}

#[test]
fn error_wrong_argument_type() {
    assert_has_error(
        r#"
fn takes_int:
  x: int
-> int {
  return x
}

fn main:
-> int {
  return takes_int(x: "hello")
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_missing_argument() {
    assert_has_error(
        r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}

fn main:
-> int {
  return add(a: 1)
}
"#,
        "missing argument",
    );
}

#[test]
fn error_extra_argument() {
    assert_has_error(
        r#"
fn one_arg:
  x: int
-> int {
  return x
}

fn main:
-> int {
  return one_arg(x: 1, y: 2)
}
"#,
        "expected 1 arguments",
    );
}

#[test]
fn error_if_condition_not_bool() {
    assert_has_error(
        r#"
fn bad:
  x: int
-> int {
  return if x { 1 } else { 2 }
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_if_branch_type_mismatch() {
    assert_has_error(
        r#"
fn bad:
  x: bool
-> int {
  return if x { 1 } else { "hello" }
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_let_type_annotation_mismatch() {
    assert_has_error(
        r#"
fn bad:
-> int {
  let x: string = 42;
  return 0
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_struct_field_count_mismatch() {
    assert_has_error(
        r#"
type Point = { x: int, y: int }

fn bad:
-> int {
  let p: Point = { x: 1 };
  return p.x
}
"#,
        "struct with 2 fields",
    );
}

#[test]
fn error_struct_field_type_mismatch() {
    assert_has_error(
        r#"
type Point = { x: int, y: int }

fn bad:
-> int {
  let p: Point = { x: 1, y: "hello" };
  return p.x
}
"#,
        "type mismatch",
    );
}

#[test]
fn error_return_from_void_style() {
    // Returning a string when int is expected
    assert_has_error(
        r#"
fn bad:
  x: int
-> int {
  if x > 0 {
    return "oops";
  }
  return x
}
"#,
        "type mismatch",
    );
}

// ──────────────────────────────────────────────────────────────────────
// Error count tests
// ──────────────────────────────────────────────────────────────────────

#[test]
fn zero_errors_for_valid_program() {
    assert_error_count(
        r#"
fn add:
  a: int
  b: int
-> int {
  return a + b
}
"#,
        0,
    );
}

#[test]
fn one_error_for_simple_mismatch() {
    assert_error_count(
        r#"
fn bad:
-> int {
  return "hello"
}
"#,
        1,
    );
}

// ──────────────────────────────────────────────────────────────────────
// Type alias resolution
// ──────────────────────────────────────────────────────────────────────

#[test]
fn type_alias_resolves_to_struct() {
    assert_no_errors(
        r#"
type Point = { x: int, y: int }

fn distance_squared:
  p: Point
-> int {
  return p.x * p.x + p.y * p.y
}
"#,
    );
}

#[test]
fn nested_type_alias() {
    assert_no_errors(
        r#"
type Point = { x: int, y: int }
type Line = { start: Point, end: Point }

fn get_start_x:
  line: Line
-> int {
  return line.start.x
}
"#,
    );
}

#[test]
fn multiple_aliases_same_structure() {
    // Three aliases for the same structure should all be interchangeable
    assert_no_errors(
        r#"
type Point2D = { x: int, y: int }
type Vector2D = { x: int, y: int }
type Coordinate = { x: int, y: int }

fn add_points:
  a: Point2D
  b: Vector2D
-> Coordinate {
  return { x: a.x + b.x, y: a.y + b.y }
}
"#,
    );
}

// ──────────────────────────────────────────────────────────────────────
// String concatenation via +
// ──────────────────────────────────────────────────────────────────────

#[test]
fn string_concatenation() {
    assert_no_errors(
        r#"
fn concat_strings:
  a: string
  b: string
-> string {
  return a + b
}
"#,
    );
}

// ──────────────────────────────────────────────────────────────────────
// Comparison operators
// ──────────────────────────────────────────────────────────────────────

#[test]
fn comparison_returns_bool() {
    assert_no_errors(
        r#"
fn less_than:
  a: int
  b: int
-> bool {
  return a < b
}

fn equal:
  a: int
  b: int
-> bool {
  return a == b
}

fn not_equal:
  a: int
  b: int
-> bool {
  return a != b
}
"#,
    );
}

#[test]
fn error_comparing_different_types() {
    assert_has_error(
        r#"
fn bad:
  x: int
  y: string
-> bool {
  return x == y
}
"#,
        "type mismatch",
    );
}

// ──────────────────────────────────────────────────────────────────────
// Tail expression type checking
// ──────────────────────────────────────────────────────────────────────

#[test]
fn tail_expression_matches_return_type() {
    assert_no_errors(
        r#"
fn compute:
  x: int
-> int {
  x * 2
}
"#,
    );
}

#[test]
fn error_tail_expression_wrong_type() {
    assert_has_error(
        r#"
fn compute:
  x: int
-> int {
  "wrong type"
}
"#,
        "type mismatch",
    );
}
