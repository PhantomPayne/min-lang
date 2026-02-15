# Min Language Server Protocol (LSP) Implementation

## Overview

The Min LSP provides full IDE support by sharing the same Salsa query infrastructure as the compiler. This ensures the LSP and compiler stay in perfect sync.

**Supported IDEs:**
- VS Code (primary target)
- Any LSP-compatible editor (Vim, Emacs, Sublime, etc.)

## Architecture

```
┌─────────────────────────────────────────┐
│         IDE / Editor                    │
│  (VS Code, Vim, Emacs, etc.)           │
└─────────────────────────────────────────┘
              ↕ LSP Protocol (JSON-RPC)
┌─────────────────────────────────────────┐
│      Min Language Server                │
│                                          │
│  ┌────────────────────────────────────┐ │
│  │   LSP Request Handlers             │ │
│  │   - textDocument/completion        │ │
│  │   - textDocument/hover             │ │
│  │   - textDocument/definition        │ │
│  │   - textDocument/formatting        │ │
│  │   - etc.                           │ │
│  └────────────────────────────────────┘ │
│              ↓                           │
│  ┌────────────────────────────────────┐ │
│  │   Salsa Query Database             │ │
│  │   (Shared with Compiler)           │ │
│  │                                     │ │
│  │   - parse()                        │ │
│  │   - infer_types()                  │ │
│  │   - resolve_names()                │ │
│  │   - diagnostics()                  │ │
│  │   - etc.                           │ │
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

## LSP Features

### 1. Diagnostics (Errors & Warnings)

**Trigger:** On file open, edit, or save

**Implementation:**

```rust
fn publish_diagnostics(db: &RootDatabase, uri: Url) -> Vec<lsp_types::Diagnostic> {
    let file_id = uri_to_file_id(db, &uri);
    let diagnostics = db.diagnostics(file_id);
    
    diagnostics.iter().map(|d| lsp_types::Diagnostic {
        range: span_to_lsp_range(d.span),
        severity: Some(severity_to_lsp(d.severity)),
        message: d.message.clone(),
        related_information: d.labels.iter().map(|l| {
            lsp_types::DiagnosticRelatedInformation {
                location: span_to_lsp_location(l.span),
                message: l.message.clone(),
            }
        }).collect(),
        ..Default::default()
    }).collect()
}
```

**Examples:**

- Syntax errors (from parser)
- Type errors (from type checker)
- Unused variables (from linter)
- Missing return types
- Undefined references

### 2. Go to Definition

**Trigger:** User clicks "Go to Definition" or uses keyboard shortcut

**Implementation:**

```rust
fn goto_definition(
    db: &RootDatabase,
    file_id: FileId,
    position: Position,
) -> Option<Location> {
    // 1. Find identifier at position
    let cst = db.parse(file_id);
    let token = find_token_at_position(&cst, position)?;
    
    // 2. Resolve to definition
    let name_res = db.resolve_names(file_id);
    let def_location = name_res.resolve(&token)?;
    
    // 3. Return location
    Some(def_location_to_lsp(def_location))
}
```

**Works for:**
- Function names
- Variable references
- Type aliases
- Struct fields
- Parameters

### 3. Hover Information

**Trigger:** User hovers over an identifier

**Implementation:**

```rust
fn hover(
    db: &RootDatabase,
    file_id: FileId,
    position: Position,
) -> Option<Hover> {
    let token = find_token_at_position(db, file_id, position)?;
    
    match token.kind {
        TokenKind::Ident => {
            let ty = db.type_of_expr(file_id, token.expr_id)?;
            let docs = db.documentation(file_id, token.def_id)?;
            
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "```min\n{}: {}\n```\n\n{}",
                        token.text,
                        ty,
                        docs
                    ),
                }),
                range: Some(span_to_lsp_range(token.span)),
            })
        }
        // Handle other kinds...
        _ => None,
    }
}
```

**Shows:**
- Type information
- Function signatures
- Documentation (from docstrings)
- Parameter info
- Attribute values

### 4. Code Completion

**Trigger:** User types or invokes completion

**Implementation:**

```rust
fn completion(
    db: &RootDatabase,
    file_id: FileId,
    position: Position,
) -> Vec<CompletionItem> {
    let context = analyze_completion_context(db, file_id, position);
    
    match context {
        CompletionContext::TypePosition => {
            // Complete with type names
            complete_types(db, file_id)
        }
        CompletionContext::ExprPosition => {
            // Complete with variables, functions
            complete_values(db, file_id)
        }
        CompletionContext::StructField { struct_ty } => {
            // Complete with field names
            complete_struct_fields(db, struct_ty)
        }
        CompletionContext::AttributeKey => {
            // Complete with known attributes
            complete_attributes()
        }
        CompletionContext::ParameterName { fn_ty } => {
            // Complete with parameter names
            complete_params(db, fn_ty)
        }
    }
}

fn complete_values(db: &RootDatabase, file_id: FileId) -> Vec<CompletionItem> {
    let scope = db.scope_at(file_id, position);
    
    scope.visible_items().map(|item| CompletionItem {
        label: item.name.clone(),
        kind: Some(item_kind_to_lsp(item.kind)),
        detail: Some(format!("{}", item.ty)),
        documentation: item.docs.map(|d| Documentation::MarkupContent(
            MarkupContent {
                kind: MarkupKind::Markdown,
                value: d,
            }
        )),
        ..Default::default()
    }).collect()
}
```

**Completes:**
- Variable names in scope
- Function names
- Type names
- Struct field names
- Attribute keys
- Parameter names in function calls

### 5. Formatting

**Trigger:** User saves file or invokes format command

**Implementation:**

```rust
fn format_document(
    db: &RootDatabase,
    file_id: FileId,
) -> Vec<TextEdit> {
    let cst = db.parse(file_id);
    let formatted = format_cst(&cst);
    
    vec![TextEdit {
        range: Range {
            start: Position { line: 0, character: 0 },
            end: get_end_position(db, file_id),
        },
        new_text: formatted,
    }]
}

fn format_cst(cst: &Cst) -> String {
    let mut formatter = Formatter::new();
    formatter.visit(cst);
    formatter.finish()
}
```

**Formatting rules:**
- Consistent indentation (2 or 4 spaces)
- Vertical function layout preserved
- Align attributes
- Format docstrings
- Preserve user's semicolon choices (for now)

### 6. Code Actions (Quick Fixes)

**Trigger:** User clicks lightbulb or uses keyboard shortcut

**Examples:**

```rust
fn code_actions(
    db: &RootDatabase,
    file_id: FileId,
    range: Range,
) -> Vec<CodeAction> {
    let diagnostics = db.diagnostics(file_id);
    let mut actions = Vec::new();
    
    for diag in diagnostics {
        if diag.span.overlaps(range) && !diag.fixes.is_empty() {
            for fix in &diag.fixes {
                actions.push(CodeAction {
                    title: fix.message.clone(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(hashmap! {
                            uri.clone() => fix.edits.clone(),
                        }),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
            }
        }
    }
    
    actions
}
```

**Quick fixes:**
- Add missing return type
- Add missing type annotation
- Fix import errors (future)
- Convert between types
- Add missing parameters
- Auto-implement missing functions

### 7. Rename Symbol

**Trigger:** User invokes rename

**Implementation:**

```rust
fn rename(
    db: &RootDatabase,
    file_id: FileId,
    position: Position,
    new_name: String,
) -> Option<WorkspaceEdit> {
    // 1. Find symbol at position
    let symbol = find_symbol_at_position(db, file_id, position)?;
    
    // 2. Find all references
    let refs = db.find_references(file_id, symbol.def_id);
    
    // 3. Create edits for each reference
    let edits: HashMap<Url, Vec<TextEdit>> = refs
        .iter()
        .group_by(|r| r.file_id)
        .into_iter()
        .map(|(fid, refs)| {
            let uri = file_id_to_uri(db, fid);
            let edits = refs.map(|r| TextEdit {
                range: span_to_lsp_range(r.span),
                new_text: new_name.clone(),
            }).collect();
            (uri, edits)
        })
        .collect();
    
    Some(WorkspaceEdit {
        changes: Some(edits),
        ..Default::default()
    })
}
```

### 8. Find References

**Trigger:** User invokes "Find References"

**Implementation:**

```rust
fn find_references(
    db: &RootDatabase,
    file_id: FileId,
    position: Position,
    include_declaration: bool,
) -> Vec<Location> {
    let symbol = find_symbol_at_position(db, file_id, position)?;
    let refs = db.find_references(file_id, symbol.def_id);
    
    refs.iter()
        .filter(|r| include_declaration || !r.is_definition)
        .map(|r| span_to_lsp_location(r.span))
        .collect()
}
```

### 9. Document Symbols (Outline)

**Trigger:** User opens outline view

**Implementation:**

```rust
fn document_symbols(
    db: &RootDatabase,
    file_id: FileId,
) -> Vec<DocumentSymbol> {
    let ast = db.cst_to_ast(file_id);
    
    ast.items.iter().map(|item| match item {
        Item::Function(f) => DocumentSymbol {
            name: f.name.to_string(),
            kind: SymbolKind::FUNCTION,
            range: span_to_lsp_range(f.span),
            selection_range: span_to_lsp_range(f.name_span),
            children: Some(
                f.params.iter().map(|p| DocumentSymbol {
                    name: p.name.to_string(),
                    kind: SymbolKind::VARIABLE,
                    range: span_to_lsp_range(p.span),
                    selection_range: span_to_lsp_range(p.name_span),
                    ..Default::default()
                }).collect()
            ),
            ..Default::default()
        },
        Item::TypeAlias(t) => DocumentSymbol {
            name: t.name.to_string(),
            kind: SymbolKind::STRUCT,
            range: span_to_lsp_range(t.span),
            selection_range: span_to_lsp_range(t.name_span),
            ..Default::default()
        },
    }).collect()
}
```

### 10. Syntax Highlighting (Semantic Tokens)

**Trigger:** On file open or edit

**Implementation:**

```rust
fn semantic_tokens(
    db: &RootDatabase,
    file_id: FileId,
) -> Vec<SemanticToken> {
    let typed_hir = db.infer_types(file_id);
    let mut tokens = Vec::new();
    
    for node in typed_hir.all_nodes() {
        let token_type = match node {
            Node::FunctionName(_) => SemanticTokenType::FUNCTION,
            Node::TypeName(_) => SemanticTokenType::TYPE,
            Node::Variable(v) if v.is_parameter => SemanticTokenType::PARAMETER,
            Node::Variable(_) => SemanticTokenType::VARIABLE,
            Node::StringLiteral(_) => SemanticTokenType::STRING,
            Node::NumberLiteral(_) => SemanticTokenType::NUMBER,
            Node::Keyword(_) => SemanticTokenType::KEYWORD,
            _ => continue,
        };
        
        tokens.push(SemanticToken {
            delta_line: /* ... */,
            delta_start: /* ... */,
            length: /* ... */,
            token_type: token_type as u32,
            token_modifiers_bitset: 0,
        });
    }
    
    tokens
}
```

**Token types:**
- Keywords (`fn`, `let`, `if`, `return`)
- Functions
- Types
- Variables
- Parameters
- Literals (strings, numbers, booleans)
- Comments
- Attributes

### 11. Inlay Hints

**Trigger:** Automatically shown in editor

**Shows:**
- Inferred types for variables
- Parameter names in function calls
- Return types (if helpful)

```rust
fn inlay_hints(
    db: &RootDatabase,
    file_id: FileId,
    range: Range,
) -> Vec<InlayHint> {
    let typed_hir = db.infer_types(file_id);
    let mut hints = Vec::new();
    
    for binding in typed_hir.let_bindings_in_range(range) {
        if binding.has_explicit_type {
            continue;
        }
        
        hints.push(InlayHint {
            position: position_after_binding(binding),
            label: InlayHintLabel::String(format!(": {}", binding.ty)),
            kind: Some(InlayHintKind::TYPE),
            ..Default::default()
        });
    }
    
    for call in typed_hir.function_calls_in_range(range) {
        for (arg, param) in call.args.iter().zip(&call.fn_ty.params) {
            hints.push(InlayHint {
                position: arg.span.start,
                label: InlayHintLabel::String(format!("{}:", param.name)),
                kind: Some(InlayHintKind::PARAMETER),
                ..Default::default()
            });
        }
    }
    
    hints
}
```

## Salsa Integration

### Database Setup

```rust
#[salsa::database(
    CompilerDatabase,
    LspDatabase,
)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::query_group(LspDatabase)]
trait LspQueries: CompilerQueries {
    // LSP-specific queries
    fn scope_at(&self, file_id: FileId, pos: Position) -> Arc<Scope>;
    fn find_references(&self, file_id: FileId, def_id: DefId) -> Arc<Vec<Reference>>;
    fn documentation(&self, file_id: FileId, def_id: DefId) -> Option<Arc<String>>;
}
```

### Change Tracking

When files change:

```rust
fn did_change_text_document(
    db: &mut RootDatabase,
    uri: Url,
    text: String,
) {
    let file_id = uri_to_file_id(db, &uri);
    
    // Update input query - Salsa invalidates dependent queries
    db.set_source_text(file_id, Arc::new(text));
    
    // Trigger diagnostics update
    let diagnostics = db.diagnostics(file_id);
    publish_diagnostics(uri, diagnostics);
}
```

## Performance Optimizations

### 1. Incremental Parsing

Only re-parse changed portions of the file:

```rust
fn incremental_parse(
    db: &RootDatabase,
    file_id: FileId,
    old_cst: &Cst,
    changes: &[TextEdit],
) -> Cst {
    // Identify affected regions
    let affected_range = compute_affected_range(old_cst, changes);
    
    // Reuse unaffected nodes
    reparse_region(old_cst, affected_range)
}
```

### 2. Lazy Type Checking

Only type-check visible portions:

```rust
fn type_check_visible(
    db: &RootDatabase,
    file_id: FileId,
    viewport: Range,
) {
    // Only check functions in viewport
    let ast = db.cst_to_ast(file_id);
    for func in ast.functions_in_range(viewport) {
        db.infer_types_for_function(file_id, func.id);
    }
}
```

### 3. Caching

All Salsa queries are automatically memoized.

## Error Reporting

Errors should be helpful and actionable:

**Good error:**
```
error: missing return type
  ┌─ example.min:3:1
  │
3 │ fn compute:
  │ ^^^^^^^^^^ function missing return type
  │
  = help: add a return type after the parameter list
  = example: `-> int`
```

**Bad error:**
```
error: parse error at line 3
```

## LSP Server Implementation

### Server Lifecycle

```rust
struct MinLanguageServer {
    db: Arc<Mutex<RootDatabase>>,
    client: Client,
}

impl LanguageServer for MinLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions::default()),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                // ... other capabilities
                ..Default::default()
            },
            ..Default::default()
        })
    }
    
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut db = self.db.lock().unwrap();
        // Update database with changes
        // Publish diagnostics
    }
    
    // ... implement other handlers
}
```

### Running the Server

```bash
# Stdio mode (for editors)
min-lsp

# TCP mode (for debugging)
min-lsp --tcp 9257
```

## Testing

### LSP Testing Strategy

1. **Unit tests** for each LSP feature
2. **Snapshot tests** for completion/hover results
3. **Integration tests** with real editor scenarios

```rust
#[test]
fn test_completion_in_function_body() {
    let source = r#"
        fn test:
          x: int
        -> int {
          let y = 5;
          <|>  // cursor position
        }
    "#;
    
    let completions = get_completions(source);
    assert_completions!(completions, [
        "x" => "int",
        "y" => "int",
    ]);
}
```

## VS Code Extension

A companion VS Code extension:

```json
{
  "name": "min-lang",
  "displayName": "Min Language Support",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.70.0"
  },
  "contributes": {
    "languages": [{
      "id": "min",
      "extensions": [".min"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "min",
      "scopeName": "source.min",
      "path": "./syntaxes/min.tmLanguage.json"
    }]
  }
}
```

Extension starts the LSP server and connects to it.

## Future Enhancements

- **Refactoring:** Extract function, inline variable
- **Code lens:** Show type annotations, run tests
- **Call hierarchy:** Show callers/callees
- **Type hierarchy:** Show type relationships
- **Debugging support:** DAP integration
