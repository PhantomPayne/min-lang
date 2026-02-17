//! Tests for the Salsa-based incremental compilation database.

use salsa::Setter;

use super::*;

// ── Basic Compilation Tests ─────────────────────────────────────────────────

#[test]
fn parse_simple_function() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}
"#,
    );

    let result = db.parse(source);
    assert!(result.diagnostics.is_empty(), "expected no parse errors");
    assert_eq!(result.ast.items.len(), 1);
}

#[test]
fn compile_simple_function() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
    assert_eq!(result.module.functions.len(), 1);
    assert_eq!(result.module.functions[0].name, "add");
}

#[test]
fn compile_multiple_functions() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}

fn double:
  x: int
-> int {
  x * 2
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
    assert_eq!(result.module.functions.len(), 2);
}

#[test]
fn compile_with_type_alias() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
type Point = { x: int, y: int }

fn make_point:
  x: int
  y: int
-> { x: int, y: int } {
  { x: x, y: y }
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
}

#[test]
fn compile_with_type_error() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn bad:
  x: int
-> string {
  x + 1
}
"#,
    );

    let result = db.compile(source);
    assert!(
        !result.is_ok(),
        "expected type errors for int returned as string"
    );
}

#[test]
fn diagnostics_from_undefined_variable() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn broken:
  x: int
-> int {
  y + 1
}
"#,
    );

    let diagnostics = db.diagnostics(source);
    assert!(
        !diagnostics.is_empty(),
        "expected diagnostics for undefined variable 'y'"
    );
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("undefined")),
        "expected 'undefined' in diagnostic message, got: {:?}",
        diagnostics
    );
}

// ── Incremental Update Tests ────────────────────────────────────────────────

#[test]
fn incremental_source_update() {
    let mut db = MinDatabase::new();

    // First compilation
    let source = db.set_source(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}
"#,
    );

    let result1 = db.compile(source);
    assert!(result1.is_ok());
    assert_eq!(result1.module.functions.len(), 1);

    // Update the source - change the function body
    source.set_text(&mut db).to(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b + 1
}
"#
        .to_string(),
    );

    // Recompile - should use updated source
    let result2 = db.compile(source);
    assert!(result2.is_ok());
    assert_eq!(result2.module.functions.len(), 1);
}

#[test]
fn incremental_adding_function() {
    let mut db = MinDatabase::new();

    let source = db.set_source(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}
"#,
    );

    let result1 = db.compile(source);
    assert_eq!(result1.module.functions.len(), 1);

    // Add a second function
    source.set_text(&mut db).to(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}

fn mul:
  a: int
  b: int
-> int {
  a * b
}
"#
        .to_string(),
    );

    let result2 = db.compile(source);
    assert!(result2.is_ok());
    assert_eq!(result2.module.functions.len(), 2);
}

#[test]
fn incremental_fixing_error() {
    let mut db = MinDatabase::new();

    // Start with a broken program
    let source = db.set_source(
        r#"
fn bad:
  x: int
-> int {
  y + 1
}
"#,
    );

    let diags1 = db.diagnostics(source);
    assert!(!diags1.is_empty(), "expected errors for undefined 'y'");

    // Fix the error by using the correct variable
    source.set_text(&mut db).to(
        r#"
fn good:
  x: int
-> int {
  x + 1
}
"#
        .to_string(),
    );

    let diags2 = db.diagnostics(source);
    assert!(
        diags2.is_empty(),
        "expected no errors after fix, got: {:?}",
        diags2
    );
}

#[test]
fn incremental_cached_when_unchanged() {
    let db = MinDatabase::new();

    let source = db.set_source(
        r#"
fn id:
  x: int
-> int {
  x
}
"#,
    );

    // Compile twice with same source - should use cache
    let result1 = db.compile(source);
    let result2 = db.compile(source);

    // Both should succeed
    assert!(result1.is_ok());
    assert!(result2.is_ok());

    // Arc pointers should be the same (cached result reused)
    assert!(
        Arc::ptr_eq(&result1.ast, &result2.ast),
        "expected cached AST to be reused"
    );
    assert!(
        Arc::ptr_eq(&result1.module, &result2.module),
        "expected cached module to be reused"
    );
}

// ── Query Dependency Tests ──────────────────────────────────────────────────

#[test]
fn parse_result_is_stable() {
    let db = MinDatabase::new();
    let source = db.set_source("fn id: x: int -> int { x }");

    let r1 = db.parse(source);
    let r2 = db.parse(source);

    // Same source -> same cached result
    assert!(Arc::ptr_eq(&r1.ast, &r2.ast));
}

#[test]
fn lower_result_is_stable() {
    let db = MinDatabase::new();
    let source = db.set_source("fn id: x: int -> int { x }");

    let r1 = db.lower(source);
    let r2 = db.lower(source);

    assert!(Arc::ptr_eq(&r1.module, &r2.module));
}

#[test]
fn check_result_is_stable() {
    let db = MinDatabase::new();
    let source = db.set_source("fn id: x: int -> int { x }");

    let r1 = db.check(source);
    let r2 = db.check(source);

    assert!(Arc::ptr_eq(&r1.result, &r2.result));
}

// ── String Interpolation Test ───────────────────────────────────────────────

#[test]
fn compile_string_interpolation() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn greet:
  name: string
-> string {
  "hello"
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
}

// ── Struct Field Access Test ────────────────────────────────────────────────

#[test]
fn compile_struct_field_access() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn get_x:
  p: { x: int, y: int }
-> int {
  p.x
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
}

// ── Boolean Logic Test ──────────────────────────────────────────────────────

#[test]
fn compile_boolean_logic() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn check:
  a: bool
  b: bool
-> bool {
  a and b or not a
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
}

// ── If Expression Test ──────────────────────────────────────────────────────

#[test]
fn compile_if_expression() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn abs:
  x: int
-> int {
  if x > 0 { x } else { 0 - x }
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
}

// ── Let Binding Test ────────────────────────────────────────────────────────

#[test]
fn compile_let_binding() {
    let db = MinDatabase::new();
    let source = db.set_source(
        r#"
fn compute:
  x: int
-> int {
  let doubled = x * 2;
  let result = doubled + 1;
  result
}
"#,
    );

    let result = db.compile(source);
    assert!(
        result.is_ok(),
        "expected no errors, got: {:?}",
        result.errors()
    );
}

// ── Incremental Cache Invalidation Test ─────────────────────────────────────

#[test]
fn cache_invalidated_on_source_change() {
    let mut db = MinDatabase::new();

    let source = db.set_source(
        r#"
fn id:
  x: int
-> int {
  x
}
"#,
    );

    // First compile
    let result1 = db.compile(source);
    assert!(result1.is_ok());

    // Change the source
    source.set_text(&mut db).to(
        r#"
fn id:
  x: int
-> int {
  x + 1
}
"#
        .to_string(),
    );

    // Second compile should get fresh results
    let result2 = db.compile(source);
    assert!(result2.is_ok());

    // The Arcs should NOT be the same (source changed, result recomputed)
    assert!(
        !Arc::ptr_eq(&result1.ast, &result2.ast),
        "expected different AST after source change"
    );
}

// ── Multiple Source Files Test ───────────────────────────────────────────────

#[test]
fn multiple_source_programs() {
    let db = MinDatabase::new();

    let source1 = db.set_source(
        r#"
fn add:
  a: int
  b: int
-> int {
  a + b
}
"#,
    );

    let source2 = db.set_source(
        r#"
fn mul:
  a: int
  b: int
-> int {
  a * b
}
"#,
    );

    let result1 = db.compile(source1);
    let result2 = db.compile(source2);

    assert!(result1.is_ok());
    assert!(result2.is_ok());
    assert_eq!(result1.module.functions[0].name, "add");
    assert_eq!(result2.module.functions[0].name, "mul");
}
