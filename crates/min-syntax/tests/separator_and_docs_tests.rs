//! Tests for separator consistency and docstrings on types.
//!
//! Verifies that:
//! - Struct type fields and fn params work the same way
//! - Commas are optional everywhere (newline separation works)
//! - Trailing commas are supported everywhere
//! - Docstrings and attributes work on type aliases and struct fields

use min_syntax::parser::parse;
use min_syntax::ast::*;

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

fn get_function<'a>(ast: &'a SourceFile, name: &str) -> &'a Function {
    ast.items.iter().find_map(|i| match i {
        Item::Function(f) if f.name.name == name => Some(f),
        _ => None,
    }).unwrap_or_else(|| panic!("function '{name}' not found"))
}

fn get_type_alias<'a>(ast: &'a SourceFile, name: &str) -> &'a TypeAlias {
    ast.items.iter().find_map(|i| match i {
        Item::TypeAlias(t) if t.name.name == name => Some(t),
        _ => None,
    }).unwrap_or_else(|| panic!("type alias '{name}' not found"))
}

fn get_struct_fields(ty: &TypeExpr) -> &[StructField] {
    match ty {
        TypeExpr::Struct { fields, .. } => fields,
        _ => panic!("expected struct type, got {ty:?}"),
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Trailing comma support
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn trailing_comma_in_struct_type() {
    let ast = parse_ok("type Point = { x: int, y: int, }");
    let ta = get_type_alias(&ast, "Point");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 2);
}

#[test]
fn trailing_comma_in_struct_expr() {
    let ast = parse_ok("fn f: -> int { { x: 1, y: 2, } }");
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Struct(s) => assert_eq!(s.fields.len(), 2),
        other => panic!("expected struct expr, got {other:?}"),
    }
}

#[test]
fn trailing_comma_in_call_args() {
    let ast = parse_ok("fn f: -> int { add(a: 1, b: 2,) }");
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Call(c) => assert_eq!(c.args.len(), 2),
        other => panic!("expected call expr, got {other:?}"),
    }
}

#[test]
fn trailing_comma_in_generic_type() {
    let ast = parse_ok("type Pair = Map<string, int,>");
    let ta = get_type_alias(&ast, "Pair");
    match &ta.ty {
        TypeExpr::Generic { args, .. } => assert_eq!(args.len(), 2),
        other => panic!("expected generic type, got {other:?}"),
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Newline-separated struct fields (no commas)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn struct_type_newline_separated() {
    let ast = parse_ok(
        "type Point = {\n  x: int\n  y: int\n}",
    );
    let ta = get_type_alias(&ast, "Point");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name.name, "x");
    assert_eq!(fields[1].name.name, "y");
}

#[test]
fn struct_type_mixed_separators() {
    // Some fields with commas, some without
    let ast = parse_ok(
        "type Rect = {\n  x: int,\n  y: int\n  width: int,\n  height: int\n}",
    );
    let ta = get_type_alias(&ast, "Rect");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 4);
}

#[test]
fn struct_type_single_field_no_comma() {
    let ast = parse_ok("type Wrapper = { value: int }");
    let ta = get_type_alias(&ast, "Wrapper");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 1);
}

// ═══════════════════════════════════════════════════════════════════════
// Docstrings on type aliases
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn docstring_on_type_alias_before_keyword() {
    let ast = parse_ok(
        "\"\"\"A 2D point.\"\"\"\ntype Point = { x: int, y: int }",
    );
    let ta = get_type_alias(&ast, "Point");
    assert!(ta.doc.is_some());
    assert_eq!(ta.doc.as_ref().unwrap().content, "A 2D point.");
}

#[test]
fn docstring_on_type_alias_after_name() {
    let ast = parse_ok(
        "type Point\n\"\"\"A 2D point.\"\"\"\n= { x: int, y: int }",
    );
    let ta = get_type_alias(&ast, "Point");
    assert!(ta.doc.is_some());
    assert_eq!(ta.doc.as_ref().unwrap().content, "A 2D point.");
}

#[test]
fn attribute_on_type_alias() {
    let ast = parse_ok(
        "@deprecated\ntype OldPoint = { x: int, y: int }",
    );
    let ta = get_type_alias(&ast, "OldPoint");
    assert_eq!(ta.attributes.len(), 1);
    assert_eq!(ta.attributes[0].name.name, "deprecated");
}

#[test]
fn attrs_and_doc_on_type_alias() {
    let ast = parse_ok(
        "@deprecated: \"use NewPoint\"\n@since: v1\n\"\"\"A legacy point type.\"\"\"\ntype OldPoint = { x: int, y: int }",
    );
    let ta = get_type_alias(&ast, "OldPoint");
    assert_eq!(ta.attributes.len(), 2);
    assert!(ta.doc.is_some());
    assert_eq!(ta.doc.as_ref().unwrap().content, "A legacy point type.");
}

// ═══════════════════════════════════════════════════════════════════════
// Docstrings and attributes on struct fields
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn docstring_on_struct_field() {
    let ast = parse_ok(
        "type Point = {\n  x: int\n    \"\"\"The x coordinate.\"\"\"\n  y: int\n    \"\"\"The y coordinate.\"\"\"\n}",
    );
    let ta = get_type_alias(&ast, "Point");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 2);
    assert!(fields[0].doc.is_some());
    assert_eq!(fields[0].doc.as_ref().unwrap().content, "The x coordinate.");
    assert!(fields[1].doc.is_some());
    assert_eq!(fields[1].doc.as_ref().unwrap().content, "The y coordinate.");
}

#[test]
fn attribute_on_struct_field() {
    let ast = parse_ok(r#"
type Config = {
  name: string
    @required
  port: int
    @default: 8080
}
"#);
    let ta = get_type_alias(&ast, "Config");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].attributes.len(), 1);
    assert_eq!(fields[0].attributes[0].name.name, "required");
    assert_eq!(fields[1].attributes.len(), 1);
    assert_eq!(fields[1].attributes[0].name.name, "default");
}

#[test]
fn attrs_and_docs_on_struct_fields() {
    let ast = parse_ok(r#"
type User = {
  name: string
    @required
    """The user's display name."""

  email: string
    @validate: email
    """The user's email address."""

  age: int
    @min: 0
    """Age in years."""
}
"#);
    let ta = get_type_alias(&ast, "User");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 3);

    assert_eq!(fields[0].name.name, "name");
    assert_eq!(fields[0].attributes.len(), 1);
    assert!(fields[0].doc.is_some());

    assert_eq!(fields[1].name.name, "email");
    assert_eq!(fields[1].attributes.len(), 1);
    assert!(fields[1].doc.is_some());

    assert_eq!(fields[2].name.name, "age");
    assert_eq!(fields[2].attributes.len(), 1);
    assert!(fields[2].doc.is_some());
}

// ═══════════════════════════════════════════════════════════════════════
// Consistency: struct fields and fn params behave identically
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn fn_params_and_struct_fields_same_style() {
    // Both use the same name: Type pattern, both support newline separation,
    // both support attrs and docs
    let ast = parse_ok(r#"
type Config = {
  host: string
    @required
    """The hostname."""
  port: int
    @default: 5432
    """The port number."""
}

fn connect:
  """Connects to a server."""
  host: string
    @required
    """The hostname."""
  port: int
    @default: 5432
    """The port number."""
-> bool {
  true
}
"#);

    let config = get_type_alias(&ast, "Config");
    let fields = get_struct_fields(&config.ty);
    let connect = get_function(&ast, "connect");

    // Same number of entries
    assert_eq!(fields.len(), connect.params.len());

    // Same names
    assert_eq!(fields[0].name.name, connect.params[0].name.name);
    assert_eq!(fields[1].name.name, connect.params[1].name.name);

    // Same attrs
    assert_eq!(fields[0].attributes.len(), connect.params[0].attributes.len());
    assert_eq!(fields[1].attributes.len(), connect.params[1].attributes.len());

    // Both have docs
    assert!(fields[0].doc.is_some());
    assert!(connect.params[0].doc.is_some());
    assert!(fields[1].doc.is_some());
    assert!(connect.params[1].doc.is_some());
}

// ═══════════════════════════════════════════════════════════════════════
// Struct expressions: newline separation
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn struct_expr_newline_separated() {
    let ast = parse_ok(
        "fn f: -> int {\n  {\n    x: 1\n    y: 2\n  }\n}",
    );
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Struct(s) => assert_eq!(s.fields.len(), 2),
        other => panic!("expected struct expr, got {other:?}"),
    }
}

#[test]
fn struct_expr_trailing_comma() {
    let ast = parse_ok("fn f: -> int { { x: 1, y: 2, } }");
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Struct(s) => assert_eq!(s.fields.len(), 2),
        other => panic!("expected struct expr, got {other:?}"),
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Call args: newline separation
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn call_args_newline_separated() {
    let ast = parse_ok(
        "fn f: -> int {\n  add(\n    a: 1\n    b: 2\n  )\n}",
    );
    let f = get_function(&ast, "f");
    let tail = f.body.tail_expr.as_ref().unwrap();
    match tail.as_ref() {
        Expr::Call(c) => assert_eq!(c.args.len(), 2),
        other => panic!("expected call expr, got {other:?}"),
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Edge cases: empty struct
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn empty_struct_type() {
    let ast = parse_ok("type Empty = { }");
    let ta = get_type_alias(&ast, "Empty");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 0);
}

#[test]
fn empty_struct_type_no_space() {
    let ast = parse_ok("type Empty = {}");
    let ta = get_type_alias(&ast, "Empty");
    let fields = get_struct_fields(&ta.ty);
    assert_eq!(fields.len(), 0);
}

// ═══════════════════════════════════════════════════════════════════════
// Realistic full program with new features
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn full_program_with_documented_types() {
    let ast = parse_ok(r#"
@since: v2
"""Represents a database connection configuration."""
type DbConfig = {
  host: string
    @required
    """Hostname of the database server."""
  port: int
    @default: 5432
    """Port number (default: 5432)."""
  database: string
    @required
    """Name of the database."""
  max_connections: int
    """Maximum number of concurrent connections."""
}

"""Creates a default database configuration."""
fn default_config: -> DbConfig {
  {
    host: "localhost"
    port: 5432
    database: "myapp"
    max_connections: 10
  }
}
"#);

    let config = get_type_alias(&ast, "DbConfig");
    assert!(config.doc.is_some());
    assert_eq!(config.attributes.len(), 1);

    let fields = get_struct_fields(&config.ty);
    assert_eq!(fields.len(), 4);
    assert!(fields[0].doc.is_some());
    assert_eq!(fields[0].attributes.len(), 1);

    let f = get_function(&ast, "default_config");
    assert!(f.doc.is_some());
}
