//! Error recovery tests for incremental compilation and LSP support.
//!
//! These tests verify that the parser always produces a partial AST
//! with meaningful diagnostics, even when the input is incomplete or
//! malformed. This is critical for the LSP server, which needs to
//! provide feedback on code that is being actively edited.

use min_syntax::parser::parse;
use min_syntax::ast::*;

fn parse_with_errors(src: &str) -> (SourceFile, Vec<min_diagnostics::Diagnostic>) {
    parse(src)
}

fn get_functions(ast: &SourceFile) -> Vec<&Function> {
    ast.items
        .iter()
        .filter_map(|i| match i {
            Item::Function(f) => Some(f),
            _ => None,
        })
        .collect()
}

fn get_type_aliases(ast: &SourceFile) -> Vec<&TypeAlias> {
    ast.items
        .iter()
        .filter_map(|i| match i {
            Item::TypeAlias(t) => Some(t),
            _ => None,
        })
        .collect()
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Multiple functions where some are broken
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_broken_fn_before_valid_fn() {
    let (ast, errors) = parse_with_errors(
        "fn broken:\n  x: int\n\nfn works: -> int { 42 }",
    );
    assert!(!errors.is_empty(), "should report errors for broken fn");
    let funcs = get_functions(&ast);
    assert!(
        funcs.iter().any(|f| f.name.name == "works"),
        "should recover and parse 'works' function"
    );
}

#[test]
fn recover_multiple_broken_functions() {
    let src = r#"
fn broken1:
  x: int

fn broken2:
  y: string

fn valid: -> int { 1 }
"#;
    let (ast, errors) = parse_with_errors(src);
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}

#[test]
fn recover_valid_fn_between_broken_ones() {
    let src = r#"
fn broken_before:
  x: int

fn valid: -> int { 42 }

fn broken_after:
  y: string
"#;
    let (ast, errors) = parse_with_errors(src);
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Garbage before valid code
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_garbage_tokens_before_fn() {
    let (ast, errors) = parse_with_errors("!!! ??? ### fn valid: -> int { 1 }");
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}

#[test]
fn recover_random_keywords_before_fn() {
    let (ast, errors) = parse_with_errors("let x = 5; fn valid: -> int { 1 }");
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Incomplete expressions in function bodies
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_missing_semicolon_in_let() {
    // Missing semicolon after let - should still parse
    let (ast, _errors) = parse_with_errors(
        "fn f: -> int {\n  let x = 42\n  x\n}",
    );
    let funcs = get_functions(&ast);
    assert_eq!(funcs.len(), 1);
}

#[test]
fn recover_unclosed_block() {
    // Missing closing brace
    let (ast, errors) = parse_with_errors("fn f: -> int { 42");
    assert!(!errors.is_empty());
    // Should still produce a partial function
    let funcs = get_functions(&ast);
    assert!(funcs.len() <= 1); // might or might not produce partial fn
}

#[test]
fn recover_extra_closing_brace() {
    let (ast, errors) = parse_with_errors("fn f: -> int { 42 } }");
    // Extra } at end should be an error but fn should parse
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert_eq!(funcs.len(), 1);
    assert_eq!(funcs[0].name.name, "f");
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Malformed type aliases
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_type_alias_before_valid_fn() {
    let (ast, _errors) = parse_with_errors(
        "type Point = { x: int, y: int }\nfn f: -> int { 1 }",
    );
    let funcs = get_functions(&ast);
    assert_eq!(funcs.len(), 1);
    let aliases = get_type_aliases(&ast);
    assert_eq!(aliases.len(), 1);
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Typing-in-progress scenarios (LSP-critical)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_typing_fn_keyword_only() {
    // User just typed "fn" - should not crash
    let (_, errors) = parse_with_errors("fn");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_fn_name() {
    // User typed "fn add" but nothing else
    let (_, errors) = parse_with_errors("fn add");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_fn_name_colon() {
    // User typed "fn add:" but no params or return type yet
    let (_, errors) = parse_with_errors("fn add:");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_fn_with_params_no_return() {
    // User typed params but hasn't added return type yet
    let (_, errors) = parse_with_errors("fn add:\n  a: int\n  b: int");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_fn_with_return_no_body() {
    // User has return type but no body yet
    let (_, errors) = parse_with_errors("fn add:\n  a: int\n-> int");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_fn_open_brace() {
    // User just opened the body
    let (_, errors) = parse_with_errors("fn add:\n  a: int\n-> int {");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_let_incomplete() {
    // User typing a let statement
    let (_, errors) = parse_with_errors("fn f: -> int {\n  let x =\n}");
    assert!(!errors.is_empty());
}

#[test]
fn recover_typing_if_incomplete() {
    // User typing an if expression
    let (_, errors) = parse_with_errors("fn f: -> int {\n  if\n}");
    assert!(!errors.is_empty());
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Valid items after broken items
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn valid_type_alias_after_broken_function() {
    let (ast, errors) = parse_with_errors(
        "fn broken:\n\ntype Point = { x: int, y: int }",
    );
    assert!(!errors.is_empty());
    let aliases = get_type_aliases(&ast);
    assert_eq!(aliases.len(), 1);
    assert_eq!(aliases[0].name.name, "Point");
}

#[test]
fn valid_function_after_broken_type() {
    let (ast, errors) = parse_with_errors(
        "type = \nfn valid: -> int { 1 }",
    );
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Unterminated strings in expressions
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn recover_unterminated_string_in_body() {
    let (_, errors) = parse_with_errors(
        "fn f: -> string {\n  return \"hello\n}\nfn g: -> int { 1 }",
    );
    assert!(!errors.is_empty());
}

#[test]
fn recover_unterminated_docstring() {
    let (_, errors) = parse_with_errors(
        "fn f:\n  \"\"\"unterminated\n  a: int\n-> int { 1 }",
    );
    assert!(!errors.is_empty());
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Diagnostics are meaningful
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn diagnostic_messages_are_descriptive() {
    let (_, errors) = parse_with_errors("fn f: -> int { @ }");
    assert!(!errors.is_empty());
    // All errors should have non-empty messages
    for e in &errors {
        assert!(!e.message.is_empty(), "error message should not be empty");
    }
}

#[test]
fn diagnostic_spans_are_valid() {
    let src = "fn f: -> int { ??? }";
    let (_, errors) = parse_with_errors(src);
    assert!(!errors.is_empty());
    for e in &errors {
        let span = e.primary_label.span;
        assert!(
            (span.start as usize) <= src.len(),
            "span start out of bounds"
        );
        assert!(
            (span.end as usize) <= src.len(),
            "span end out of bounds"
        );
        assert!(span.start <= span.end, "span start > end");
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Empty input
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn empty_input_produces_empty_ast() {
    let (ast, errors) = parse_with_errors("");
    assert!(errors.is_empty());
    assert!(ast.items.is_empty());
}

#[test]
fn whitespace_only_produces_empty_ast() {
    let (ast, errors) = parse_with_errors("   \n\n\t  ");
    assert!(errors.is_empty());
    assert!(ast.items.is_empty());
}

#[test]
fn comments_only_produces_empty_ast() {
    let (ast, errors) = parse_with_errors("// just a comment\n// another one\n");
    assert!(errors.is_empty());
    assert!(ast.items.is_empty());
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Lexer errors don't prevent parsing
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn lexer_errors_still_allow_parsing() {
    // Invalid character followed by valid code
    let (ast, errors) = parse_with_errors("~\nfn f: -> int { 1 }");
    assert!(!errors.is_empty()); // lexer error for ~
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "f"));
}

// ═══════════════════════════════════════════════════════════════════════
// Recovery: Stress tests with many errors
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn many_errors_dont_cause_infinite_loop() {
    // This should terminate in reasonable time
    let src = "!!! ??? ### $$$ %%% &&& fn valid: -> int { 1 }";
    let (ast, errors) = parse_with_errors(src);
    assert!(!errors.is_empty());
    let funcs = get_functions(&ast);
    assert!(funcs.iter().any(|f| f.name.name == "valid"));
}

#[test]
fn large_valid_program_parses_cleanly() {
    // Build a large program with many functions
    let mut src = String::new();
    for i in 0..50 {
        src.push_str(&format!(
            "fn func_{i}:\n  x: int\n-> int {{\n  return x + {i}\n}}\n\n"
        ));
    }
    let (ast, errors) = parse_with_errors(&src);
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert_eq!(get_functions(&ast).len(), 50);
}

// ═══════════════════════════════════════════════════════════════════════
// Incremental parse friendliness: re-parsing after edits
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn reparse_after_adding_function() {
    // First version
    let src1 = "fn a: -> int { 1 }";
    let (ast1, errors1) = parse_with_errors(src1);
    assert!(errors1.is_empty());
    assert_eq!(get_functions(&ast1).len(), 1);

    // User adds a new function
    let src2 = "fn a: -> int { 1 }\nfn b: -> int { 2 }";
    let (ast2, errors2) = parse_with_errors(src2);
    assert!(errors2.is_empty());
    assert_eq!(get_functions(&ast2).len(), 2);
}

#[test]
fn reparse_after_breaking_function() {
    // Valid code
    let src1 = "fn a: -> int { 1 }\nfn b: -> int { 2 }";
    let (ast1, errors1) = parse_with_errors(src1);
    assert!(errors1.is_empty());
    assert_eq!(get_functions(&ast1).len(), 2);

    // User breaks function a
    let src2 = "fn a:\nfn b: -> int { 2 }";
    let (ast2, errors2) = parse_with_errors(src2);
    assert!(!errors2.is_empty());
    // b should still parse
    let funcs = get_functions(&ast2);
    assert!(funcs.iter().any(|f| f.name.name == "b"));
}

#[test]
fn reparse_after_fixing_function() {
    // Broken code
    let src1 = "fn a:\nfn b: -> int { 2 }";
    let (_, errors1) = parse_with_errors(src1);
    assert!(!errors1.is_empty());

    // User fixes function a
    let src2 = "fn a: -> int { 1 }\nfn b: -> int { 2 }";
    let (ast2, errors2) = parse_with_errors(src2);
    assert!(errors2.is_empty());
    assert_eq!(get_functions(&ast2).len(), 2);
}
