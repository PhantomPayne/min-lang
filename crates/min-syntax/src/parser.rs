use crate::ast::*;
use crate::token::{Token, TokenKind};
use min_diagnostics::{Diagnostic, Span};

/// Parser for the Min language.
///
/// Uses recursive descent for declarations/statements and Pratt parsing
/// for expressions with operator precedence.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<Diagnostic>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (SourceFile, Vec<Diagnostic>) {
        let items = self.parse_source_file();
        let span = if items.is_empty() {
            Span::new(0, 0)
        } else {
            items.first().unwrap().span().merge(items.last().unwrap().span())
        };
        let file = SourceFile { items, span };
        (file, self.errors)
    }

    // ── Token helpers ────────────────────────────────────────────────────

    fn current(&self) -> &Token {
        &self.tokens[self.pos.min(self.tokens.len() - 1)]
    }

    fn current_kind(&self) -> TokenKind {
        self.current().kind
    }

    fn peek_kind(&self) -> TokenKind {
        self.peek_nth(1)
    }

    fn peek_nth(&self, n: usize) -> TokenKind {
        let mut ahead = 0;
        let mut i = self.pos;
        while ahead < n {
            i += 1;
            if i >= self.tokens.len() {
                return TokenKind::Eof;
            }
            if !self.tokens[i].kind.is_trivia() {
                ahead += 1;
            }
        }
        if i < self.tokens.len() {
            self.tokens[i].kind
        } else {
            TokenKind::Eof
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current_kind() == kind
    }

    fn at_eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    /// Advance past the current token and return it.
    fn bump(&mut self) -> Token {
        let tok = self.tokens[self.pos.min(self.tokens.len() - 1)].clone();
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        tok
    }

    /// Skip trivia tokens (whitespace, newlines, comments).
    fn skip_trivia(&mut self) {
        while self.current_kind().is_trivia() || self.current_kind() == TokenKind::Newline {
            self.pos += 1;
            if self.pos >= self.tokens.len() {
                self.pos = self.tokens.len() - 1;
                break;
            }
        }
    }

    /// Expect and consume a specific token kind (skipping trivia first).
    /// Returns the token if found, or emits an error and returns None.
    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        self.skip_trivia();
        if self.at(kind) {
            Some(self.bump())
        } else {
            let span = self.current().span;
            self.errors.push(Diagnostic::error(
                format!("expected {kind}, found {}", self.current_kind()),
                span,
            ));
            None
        }
    }

    /// Consume a token of the given kind if present, returning it.
    fn eat(&mut self, kind: TokenKind) -> Option<Token> {
        self.skip_trivia();
        if self.at(kind) {
            Some(self.bump())
        } else {
            None
        }
    }

    /// Advance to a synchronization point after a parse error.
    fn synchronize(&mut self) {
        loop {
            match self.current_kind() {
                TokenKind::Fn | TokenKind::Type | TokenKind::Eof => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    // ── Top-level parsing ────────────────────────────────────────────────

    fn parse_source_file(&mut self) -> Vec<Item> {
        let mut items = Vec::new();
        loop {
            self.skip_trivia();
            if self.at_eof() {
                break;
            }
            match self.current_kind() {
                TokenKind::Fn => {
                    if let Some(func) = self.parse_function() {
                        items.push(Item::Function(func));
                    }
                }
                TokenKind::Type => {
                    if let Some(alias) = self.parse_type_alias() {
                        items.push(Item::TypeAlias(alias));
                    }
                }
                TokenKind::DocString | TokenKind::At => {
                    // Attributes or docstrings before a function or type alias.
                    // Peek past attrs/docs to see if next keyword is fn or type.
                    let saved = self.pos;
                    loop {
                        self.skip_trivia();
                        if self.at(TokenKind::At) {
                            self.parse_attribute();
                        } else if self.at(TokenKind::DocString) {
                            self.parse_docstring();
                        } else {
                            break;
                        }
                    }
                    let next_kind = self.current_kind();
                    self.pos = saved; // restore position

                    match next_kind {
                        TokenKind::Type => {
                            if let Some(alias) = self.parse_type_alias() {
                                items.push(Item::TypeAlias(alias));
                            }
                        }
                        _ => {
                            // Default: treat as function (fn or error)
                            if let Some(func) = self.parse_function() {
                                items.push(Item::Function(func));
                            }
                        }
                    }
                }
                _ => {
                    let span = self.current().span;
                    self.errors.push(Diagnostic::error(
                        format!("expected `fn` or `type`, found {}", self.current_kind()),
                        span,
                    ));
                    self.synchronize();
                }
            }
        }
        items
    }

    // ── Function ─────────────────────────────────────────────────────────

    fn parse_function(&mut self) -> Option<Function> {
        self.skip_trivia();
        let start_span = self.current().span;

        // Collect leading attributes and docstring
        let mut attributes = Vec::new();
        let mut doc = None;

        loop {
            self.skip_trivia();
            if self.at(TokenKind::At) {
                attributes.push(self.parse_attribute());
            } else if self.at(TokenKind::DocString) {
                doc = Some(self.parse_docstring());
            } else {
                break;
            }
        }

        // fn keyword
        self.expect(TokenKind::Fn)?;
        self.skip_trivia();

        // function name
        let name_tok = self.expect(TokenKind::Ident)?;
        let name = Ident::new(&name_tok.text, name_tok.span);

        // colon after name
        self.expect(TokenKind::Colon)?;

        // Parse params, attributes, docstrings before ->
        let mut params = Vec::new();
        loop {
            self.skip_trivia();
            match self.current_kind() {
                TokenKind::Arrow | TokenKind::LBrace | TokenKind::Eof => break,
                TokenKind::Fn | TokenKind::Type => break, // error recovery: next item
                TokenKind::At => {
                    let attr = self.parse_attribute();
                    // If a param was already parsed, this attribute belongs
                    // to it (Min convention: attrs follow their param).
                    // If no params yet, it's a function-level attribute.
                    if !params.is_empty() {
                        // Check: is the NEXT thing another param (ident : type)?
                        // If so, this attr precedes the new param.
                        self.skip_trivia();
                        if self.at(TokenKind::Ident) && self.peek_kind() == TokenKind::Colon {
                            if let Some(param) = self.parse_param_with_attrs(vec![attr], None) {
                                params.push(param);
                            }
                        } else {
                            params.last_mut().unwrap().attributes.push(attr);
                        }
                    } else {
                        attributes.push(attr);
                    }
                }
                TokenKind::DocString => {
                    let ds = self.parse_docstring();
                    // If no params yet, this is the function docstring.
                    // If params exist, this docstring belongs to the
                    // previously parsed param (Min convention: param docs
                    // follow their param).
                    if params.is_empty() && doc.is_none() {
                        doc = Some(ds);
                    } else if !params.is_empty() {
                        // Attach to previous param
                        params.last_mut().unwrap().doc = Some(ds);
                    } else {
                        doc = Some(ds);
                    }
                }
                TokenKind::Ident => {
                    // Check if this is a param (ident followed by colon)
                    if self.peek_kind() == TokenKind::Colon {
                        if let Some(param) = self.parse_param() {
                            params.push(param);
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        // Return type
        let return_type = if self.eat(TokenKind::Arrow).is_some() {
            self.parse_type()
        } else {
            // Error: missing return type
            let span = self.current().span;
            self.errors.push(Diagnostic::error(
                "expected -> and return type",
                span,
            ));
            TypeExpr::Named(Ident::new("unknown", span))
        };

        // Body
        let body = self.parse_block()?;

        let end_span = body.span;
        Some(Function {
            name,
            attributes,
            doc,
            params,
            return_type,
            body,
            span: start_span.merge(end_span),
        })
    }

    fn parse_param(&mut self) -> Option<Param> {
        self.parse_param_with_attrs(Vec::new(), None)
    }

    fn parse_param_with_attrs(
        &mut self,
        attributes: Vec<Attribute>,
        doc: Option<DocString>,
    ) -> Option<Param> {
        self.skip_trivia();
        let start = self.current().span;

        let name_tok = self.expect(TokenKind::Ident)?;
        let name = Ident::new(&name_tok.text, name_tok.span);

        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type();

        // Optional default value
        let default = if self.eat(TokenKind::Eq).is_some() {
            Some(self.parse_expr())
        } else {
            None
        };

        let end = default
            .as_ref()
            .map(|e| e.span())
            .unwrap_or(ty.span());

        Some(Param {
            name,
            ty,
            default,
            attributes,
            doc,
            span: start.merge(end),
        })
    }

    // ── Type alias ───────────────────────────────────────────────────────

    fn parse_type_alias(&mut self) -> Option<TypeAlias> {
        self.skip_trivia();
        let start = self.current().span;

        // Collect leading attributes and docstring (before `type` keyword)
        let mut attributes = Vec::new();
        let mut doc = None;

        loop {
            self.skip_trivia();
            if self.at(TokenKind::At) {
                attributes.push(self.parse_attribute());
            } else if self.at(TokenKind::DocString) {
                doc = Some(self.parse_docstring());
            } else {
                break;
            }
        }

        self.expect(TokenKind::Type)?;
        self.skip_trivia();

        let name_tok = self.expect(TokenKind::Ident)?;
        let name = Ident::new(&name_tok.text, name_tok.span);

        // Docstring after name but before `=`
        self.skip_trivia();
        if self.at(TokenKind::DocString) && doc.is_none() {
            doc = Some(self.parse_docstring());
        }

        self.expect(TokenKind::Eq)?;
        let ty = self.parse_type();
        let end = ty.span();

        Some(TypeAlias {
            name,
            attributes,
            doc,
            ty,
            span: start.merge(end),
        })
    }

    // ── Types ────────────────────────────────────────────────────────────

    fn parse_type(&mut self) -> TypeExpr {
        self.skip_trivia();
        let start = self.current().span;

        match self.current_kind() {
            TokenKind::LBrace => self.parse_struct_type(),
            TokenKind::Ident => {
                let name_tok = self.bump();
                let name = Ident::new(&name_tok.text, name_tok.span);

                // Check for generic parameters: Name<T, U>
                self.skip_trivia();
                if self.at(TokenKind::Lt) {
                    self.bump(); // consume <
                    let mut args = Vec::new();
                    // Handle empty generic args and trailing commas
                    loop {
                        self.skip_trivia();
                        if self.at(TokenKind::Gt) || self.at_eof() {
                            break;
                        }
                        args.push(self.parse_type());
                        self.eat(TokenKind::Comma); // optional comma
                    }
                    let end_tok = self.expect(TokenKind::Gt);
                    let end = end_tok.map(|t| t.span).unwrap_or(args.last().unwrap().span());
                    TypeExpr::Generic {
                        name,
                        args,
                        span: start.merge(end),
                    }
                } else {
                    TypeExpr::Named(name)
                }
            }
            _ => {
                let span = self.current().span;
                self.errors.push(Diagnostic::error(
                    format!("expected type, found {}", self.current_kind()),
                    span,
                ));
                TypeExpr::Named(Ident::new("error", span))
            }
        }
    }

    fn parse_struct_type(&mut self) -> TypeExpr {
        let start = self.current().span;
        self.expect(TokenKind::LBrace);
        let mut fields: Vec<StructField> = Vec::new();

        // Parse fields like fn params: newline OR comma separated,
        // with optional trailing commas, attrs, and docstrings.
        loop {
            self.skip_trivia();
            match self.current_kind() {
                TokenKind::RBrace | TokenKind::Eof => break,
                TokenKind::At => {
                    let attr = self.parse_attribute();
                    if !fields.is_empty() {
                        // Attr after a field always belongs to the previous field
                        // (same convention as fn params: attrs follow their field)
                        fields.last_mut().unwrap().attributes.push(attr);
                    } else {
                        // Attr before first field: attach to next field
                        let mut attrs = vec![attr];
                        // Collect any additional attrs
                        loop {
                            self.skip_trivia();
                            if self.at(TokenKind::At) {
                                attrs.push(self.parse_attribute());
                            } else {
                                break;
                            }
                        }
                        self.skip_trivia();
                        if let Some(field) = self.parse_struct_field_with_attrs(attrs, None) {
                            fields.push(field);
                        }
                    }
                }
                TokenKind::DocString => {
                    let ds = self.parse_docstring();
                    if !fields.is_empty() {
                        // Docstring after a field belongs to it
                        fields.last_mut().unwrap().doc = Some(ds);
                    } else {
                        // Docstring before first field: attach to next field
                        self.skip_trivia();
                        if self.at(TokenKind::Ident) && self.peek_kind() == TokenKind::Colon {
                            if let Some(field) = self.parse_struct_field_with_attrs(Vec::new(), Some(ds)) {
                                fields.push(field);
                            }
                        }
                        // else: stray docstring, just drop it
                    }
                }
                TokenKind::Ident => {
                    if self.peek_kind() == TokenKind::Colon {
                        if let Some(field) = self.parse_struct_field() {
                            fields.push(field);
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
            // Commas are optional separators - eat one if present
            self.eat(TokenKind::Comma);
        }

        let end = self.expect(TokenKind::RBrace)
            .map(|t| t.span)
            .unwrap_or(start);

        TypeExpr::Struct {
            fields,
            span: start.merge(end),
        }
    }

    fn parse_struct_field(&mut self) -> Option<StructField> {
        self.parse_struct_field_with_attrs(Vec::new(), None)
    }

    fn parse_struct_field_with_attrs(
        &mut self,
        attributes: Vec<Attribute>,
        doc: Option<DocString>,
    ) -> Option<StructField> {
        self.skip_trivia();
        let start = self.current().span;
        let name_tok = self.expect(TokenKind::Ident)?;
        let name = Ident::new(&name_tok.text, name_tok.span);
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type();
        let end = ty.span();
        Some(StructField {
            name,
            ty,
            attributes,
            doc,
            span: start.merge(end),
        })
    }

    // ── Attributes ───────────────────────────────────────────────────────

    fn parse_attribute(&mut self) -> Attribute {
        let start = self.current().span;
        self.bump(); // consume @
        self.skip_trivia();

        let name_tok = self.expect(TokenKind::Ident)
            .unwrap_or_else(|| Token::new(TokenKind::Ident, "unknown", start));
        let name = Ident::new(&name_tok.text, name_tok.span);

        let value = if self.eat(TokenKind::Colon).is_some() {
            self.skip_trivia();
            Some(self.parse_attr_value())
        } else {
            None
        };

        let end = value
            .as_ref()
            .map(|v| v.span())
            .unwrap_or(name.span);

        Attribute {
            name,
            value,
            span: start.merge(end),
        }
    }

    fn parse_attr_value(&mut self) -> AttrValue {
        self.skip_trivia();
        match self.current_kind() {
            TokenKind::StringLit => {
                let tok = self.bump();
                // Strip quotes from string content
                let content = tok.text[1..tok.text.len() - 1].to_string();
                AttrValue::String(content, tok.span)
            }
            TokenKind::IntLit => {
                let tok = self.bump();
                let val: i64 = tok.text.parse().unwrap_or(0);
                AttrValue::Int(val, tok.span)
            }
            TokenKind::True => {
                let tok = self.bump();
                AttrValue::Bool(true, tok.span)
            }
            TokenKind::False => {
                let tok = self.bump();
                AttrValue::Bool(false, tok.span)
            }
            TokenKind::Ident => {
                let tok = self.bump();
                AttrValue::Ident(Ident::new(&tok.text, tok.span))
            }
            _ => {
                let span = self.current().span;
                self.errors.push(Diagnostic::error(
                    format!("expected attribute value, found {}", self.current_kind()),
                    span,
                ));
                AttrValue::Ident(Ident::new("error", span))
            }
        }
    }

    fn parse_docstring(&mut self) -> DocString {
        let tok = self.bump();
        // Strip the triple quotes
        let content = if tok.text.len() >= 6 {
            tok.text[3..tok.text.len() - 3].to_string()
        } else {
            tok.text.clone()
        };
        DocString {
            content: content.trim().to_string(),
            span: tok.span,
        }
    }

    // ── Blocks ───────────────────────────────────────────────────────────

    fn parse_block(&mut self) -> Option<Block> {
        self.skip_trivia();
        let start = self.current().span;
        self.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        let mut tail_expr = None;

        loop {
            self.skip_trivia();
            match self.current_kind() {
                TokenKind::RBrace | TokenKind::Eof => break,
                TokenKind::Let => {
                    if let Some(stmt) = self.parse_let_stmt() {
                        stmts.push(stmt);
                    }
                }
                _ => {
                    // Could be an expression statement or a tail expression
                    let expr = self.parse_expr();
                    self.skip_trivia();
                    if self.eat(TokenKind::Semicolon).is_some() {
                        let span = expr.span();
                        stmts.push(Stmt::Expr(expr, span));
                    } else if self.at(TokenKind::RBrace) {
                        // This is the tail expression
                        tail_expr = Some(Box::new(expr));
                    } else if self.at(TokenKind::Eof) {
                        tail_expr = Some(Box::new(expr));
                        break;
                    } else {
                        // Implicit statement (no semicolon but more follows)
                        let span = expr.span();
                        stmts.push(Stmt::Expr(expr, span));
                    }
                }
            }
        }

        let end = self.expect(TokenKind::RBrace)
            .map(|t| t.span)
            .unwrap_or(tail_expr
                .as_ref()
                .map(|e| e.span())
                .or(stmts.last().map(|s| s.span()))
                .unwrap_or(start));

        Some(Block {
            stmts,
            tail_expr,
            span: start.merge(end),
        })
    }

    // ── Statements ───────────────────────────────────────────────────────

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.skip_trivia();
        let start = self.current().span;
        self.expect(TokenKind::Let)?;
        self.skip_trivia();

        let name_tok = self.expect(TokenKind::Ident)?;
        let name = Ident::new(&name_tok.text, name_tok.span);

        // Optional type annotation
        let ty = if self.eat(TokenKind::Colon).is_some() {
            Some(self.parse_type())
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr();

        // Optional semicolon
        self.eat(TokenKind::Semicolon);

        let end = value.span();
        Some(Stmt::Let(LetStmt {
            name,
            ty,
            value,
            span: start.merge(end),
        }))
    }

    // ── Expressions (Pratt parser) ───────────────────────────────────────

    fn parse_expr(&mut self) -> Expr {
        self.parse_expr_bp(0)
    }

    /// Pratt parser: parse expression with minimum binding power.
    fn parse_expr_bp(&mut self, min_bp: u8) -> Expr {
        self.skip_trivia();
        let mut lhs = self.parse_prefix();

        loop {
            self.skip_trivia();
            if self.at_eof() {
                break;
            }

            // Check for postfix operators: field access and function call
            match self.current_kind() {
                TokenKind::Dot => {
                    let (l_bp, _) = (15, 16); // highest precedence
                    if l_bp < min_bp {
                        break;
                    }
                    self.bump(); // consume .
                    self.skip_trivia();
                    let field_tok = self.expect(TokenKind::Ident)
                        .unwrap_or_else(|| Token::new(TokenKind::Ident, "error", self.current().span));
                    let field = Ident::new(&field_tok.text, field_tok.span);
                    let span = lhs.span().merge(field.span);
                    lhs = Expr::FieldAccess(Box::new(FieldAccess {
                        expr: lhs,
                        field,
                        span,
                    }));
                    continue;
                }
                TokenKind::LParen => {
                    // Function call - only if lhs is an identifier
                    if let Expr::Ident(ref ident) = lhs {
                        let (l_bp, _) = (15, 16);
                        if l_bp < min_bp {
                            break;
                        }
                        let func_name = ident.clone();
                        let call = self.parse_call_args(func_name, lhs.span());
                        lhs = call;
                        continue;
                    }
                    break;
                }
                _ => {}
            }

            // Infix operators
            let op = match self.current_kind() {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::Percent => BinOp::Mod,
                TokenKind::EqEq => BinOp::Eq,
                TokenKind::Neq => BinOp::Neq,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Lte => BinOp::Lte,
                TokenKind::Gte => BinOp::Gte,
                TokenKind::And => BinOp::And,
                TokenKind::Or => BinOp::Or,
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            self.bump(); // consume operator
            let rhs = self.parse_expr_bp(r_bp);
            let span = lhs.span().merge(rhs.span());
            lhs = Expr::Binary(Box::new(BinaryExpr {
                left: lhs,
                op,
                right: rhs,
                span,
            }));
        }

        lhs
    }

    /// Parse a prefix expression (atom or unary operator).
    fn parse_prefix(&mut self) -> Expr {
        self.skip_trivia();
        let start = self.current().span;

        match self.current_kind() {
            // Unary minus
            TokenKind::Minus => {
                self.bump();
                let expr = self.parse_expr_bp(7); // unary has high precedence
                let span = start.merge(expr.span());
                Expr::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Neg,
                    expr,
                    span,
                }))
            }

            // Unary not
            TokenKind::Not => {
                self.bump();
                let expr = self.parse_expr_bp(7);
                let span = start.merge(expr.span());
                Expr::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Not,
                    expr,
                    span,
                }))
            }

            // Return expression
            TokenKind::Return => {
                self.bump();
                self.skip_trivia();
                // Check if there's a value to return
                let value = if self.current_kind().can_start_expr() {
                    Some(self.parse_expr())
                } else {
                    None
                };
                let end = value.as_ref().map(|e| e.span()).unwrap_or(start);
                Expr::Return(Box::new(ReturnExpr {
                    value,
                    span: start.merge(end),
                }))
            }

            // If expression
            TokenKind::If => self.parse_if_expr(),

            // Parenthesized expression
            TokenKind::LParen => {
                self.bump();
                let expr = self.parse_expr();
                self.expect(TokenKind::RParen);
                expr
            }

            // Block or struct expression
            TokenKind::LBrace => self.parse_brace_expr(),

            // Integer literal
            TokenKind::IntLit => {
                let tok = self.bump();
                let val: i64 = tok.text.parse().unwrap_or(0);
                Expr::IntLit(val, tok.span)
            }

            // Float literal
            TokenKind::FloatLit => {
                let tok = self.bump();
                let val: f64 = tok.text.parse().unwrap_or(0.0);
                Expr::FloatLit(val, tok.span)
            }

            // String literal
            TokenKind::StringLit => {
                let tok = self.bump();
                let content = tok.text[1..tok.text.len() - 1].to_string();
                Expr::StringLit(content, tok.span)
            }

            // Interpolated string
            TokenKind::StringStart => self.parse_interp_string(),

            // Boolean literals
            TokenKind::True => {
                let tok = self.bump();
                Expr::BoolLit(true, tok.span)
            }
            TokenKind::False => {
                let tok = self.bump();
                Expr::BoolLit(false, tok.span)
            }

            // Identifier
            TokenKind::Ident => {
                let tok = self.bump();
                Expr::Ident(Ident::new(&tok.text, tok.span))
            }

            _ => {
                let span = self.current().span;
                self.errors.push(Diagnostic::error(
                    format!("expected expression, found {}", self.current_kind()),
                    span,
                ));
                self.bump();
                Expr::Error(span)
            }
        }
    }

    /// Disambiguate `{` as either a block or a struct expression.
    /// Heuristic: if after `{` we see `ident :` (not `::` or `: Type`), it's a struct.
    fn parse_brace_expr(&mut self) -> Expr {
        // Look ahead: if `{ ident : expr` pattern, treat as struct
        if self.peek_kind() == TokenKind::Ident && self.peek_nth(2) == TokenKind::Colon {
            // Could be struct literal OR block with let/label
            // Further disambiguate: if the thing after `ident:` looks like an expression
            // and there's no `let`, it's likely a struct
            // But to keep it simple: struct literals require at least `{ name: expr }`
            self.parse_struct_expr()
        } else {
            match self.parse_block() {
                Some(block) => Expr::Block(block),
                None => Expr::Error(self.current().span),
            }
        }
    }

    fn parse_struct_expr(&mut self) -> Expr {
        self.skip_trivia();
        let start = self.current().span;
        self.bump(); // consume {

        let mut fields = Vec::new();
        loop {
            self.skip_trivia();
            if self.at(TokenKind::RBrace) || self.at_eof() {
                break;
            }

            let field_start = self.current().span;
            if let Some(name_tok) = self.expect(TokenKind::Ident) {
                let name = Ident::new(&name_tok.text, name_tok.span);
                self.expect(TokenKind::Colon);
                let value = self.parse_expr();
                let field_end = value.span();
                fields.push(FieldInit {
                    name,
                    value,
                    span: field_start.merge(field_end),
                });
            }
            self.eat(TokenKind::Comma);
        }

        let end = self.expect(TokenKind::RBrace)
            .map(|t| t.span)
            .unwrap_or(start);

        Expr::Struct(StructExpr {
            fields,
            span: start.merge(end),
        })
    }

    fn parse_call_args(&mut self, function: Ident, start: Span) -> Expr {
        self.bump(); // consume (
        let mut args = Vec::new();

        loop {
            self.skip_trivia();
            if self.at(TokenKind::RParen) || self.at_eof() {
                break;
            }

            let arg_start = self.current().span;
            // Named argument: name: expr
            if self.current_kind() == TokenKind::Ident && self.peek_kind() == TokenKind::Colon {
                let name_tok = self.bump();
                let name = Ident::new(&name_tok.text, name_tok.span);
                self.expect(TokenKind::Colon);
                let value = self.parse_expr();
                let arg_end = value.span();
                args.push(NamedArg {
                    name,
                    value,
                    span: arg_start.merge(arg_end),
                });
            } else {
                // Positional argument - use empty name
                let value = self.parse_expr();
                let arg_end = value.span();
                args.push(NamedArg {
                    name: Ident::new("_", arg_start),
                    value,
                    span: arg_start.merge(arg_end),
                });
            }
            self.eat(TokenKind::Comma);
        }

        let end = self.expect(TokenKind::RParen)
            .map(|t| t.span)
            .unwrap_or(start);

        Expr::Call(CallExpr {
            function,
            args,
            span: start.merge(end),
        })
    }

    fn parse_if_expr(&mut self) -> Expr {
        self.skip_trivia();
        let start = self.current().span;
        self.bump(); // consume `if`

        let condition = self.parse_expr();
        let then_block = match self.parse_block() {
            Some(b) => b,
            None => {
                return Expr::Error(start);
            }
        };

        let else_branch = if self.eat(TokenKind::Else).is_some() {
            self.skip_trivia();
            if self.at(TokenKind::If) {
                let else_if = self.parse_if_expr();
                match else_if {
                    Expr::If(if_expr) => Some(ElseBranch::ElseIf(if_expr)),
                    _ => None,
                }
            } else {
                self.parse_block().map(ElseBranch::Block)
            }
        } else {
            None
        };

        let end = match &else_branch {
            Some(ElseBranch::Block(b)) => b.span,
            Some(ElseBranch::ElseIf(i)) => i.span,
            None => then_block.span,
        };

        Expr::If(Box::new(IfExpr {
            condition,
            then_block,
            else_branch,
            span: start.merge(end),
        }))
    }

    fn parse_interp_string(&mut self) -> Expr {
        let start = self.current().span;
        let mut parts = Vec::new();

        // StringStart token
        let start_tok = self.bump();
        let text = &start_tok.text;
        // Extract text content (after opening " and before {)
        if text.len() > 2 {
            let lit_text = text[1..text.len() - 1].to_string();
            if !lit_text.is_empty() {
                parts.push(StringPart::Lit(lit_text, start_tok.span));
            }
        }

        loop {
            // Parse the interpolated expression
            let expr = self.parse_expr();
            let expr_span = expr.span();
            parts.push(StringPart::Expr(expr, expr_span));

            self.skip_trivia();
            match self.current_kind() {
                TokenKind::StringMiddle => {
                    let mid_tok = self.bump();
                    let text = &mid_tok.text;
                    // Extract text between } and {
                    if text.len() > 2 {
                        let lit_text = text[1..text.len() - 1].to_string();
                        if !lit_text.is_empty() {
                            parts.push(StringPart::Lit(lit_text, mid_tok.span));
                        }
                    }
                }
                TokenKind::StringEnd => {
                    let end_tok = self.bump();
                    let text = &end_tok.text;
                    // Extract text between } and closing "
                    if text.len() > 2 {
                        let lit_text = text[1..text.len() - 1].to_string();
                        if !lit_text.is_empty() {
                            parts.push(StringPart::Lit(lit_text, end_tok.span));
                        }
                    }
                    let span = start.merge(end_tok.span);
                    return Expr::StringInterp(StringInterp { parts, span });
                }
                _ => {
                    // Error: expected string continuation
                    let span = self.current().span;
                    self.errors.push(Diagnostic::error(
                        "expected string continuation",
                        span,
                    ));
                    return Expr::StringInterp(StringInterp {
                        parts,
                        span: start.merge(span),
                    });
                }
            }
        }
    }
}

/// Returns (left_binding_power, right_binding_power) for an infix operator.
/// Left-associative operators have r_bp = l_bp + 1.
fn infix_binding_power(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Or => (1, 2),
        BinOp::And => (3, 4),
        BinOp::Eq | BinOp::Neq => (5, 6),
        BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => (7, 8),
        BinOp::Add | BinOp::Sub => (9, 10),
        BinOp::Mul | BinOp::Div | BinOp::Mod => (11, 12),
    }
}

// ── Convenience functions ────────────────────────────────────────────────

/// Lex and parse a source string, returning the AST and all diagnostics.
pub fn parse(source: &str) -> (SourceFile, Vec<Diagnostic>) {
    let (tokens, mut lex_errors) = crate::Lexer::new(source).tokenize();
    let (ast, parse_errors) = Parser::new(tokens).parse();
    lex_errors.extend(parse_errors);
    (ast, lex_errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(src: &str) -> SourceFile {
        let (ast, errors) = parse(src);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        ast
    }

    fn parse_items(src: &str) -> Vec<Item> {
        parse_ok(src).items
    }

    fn parse_with_errors(src: &str) -> (SourceFile, Vec<Diagnostic>) {
        parse(src)
    }

    // ── Function parsing ─────────────────────────────────────────────────

    #[test]
    fn parse_simple_function() {
        let items = parse_items(
            "fn add:\n  a: int\n  b: int\n-> int {\n  return a + b\n}",
        );
        assert_eq!(items.len(), 1);
        match &items[0] {
            Item::Function(f) => {
                assert_eq!(f.name.name, "add");
                assert_eq!(f.params.len(), 2);
                assert_eq!(f.params[0].name.name, "a");
                assert_eq!(f.params[1].name.name, "b");
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_function_no_params() {
        let items = parse_items("fn hello: -> string { \"world\" }");
        assert_eq!(items.len(), 1);
        match &items[0] {
            Item::Function(f) => {
                assert_eq!(f.name.name, "hello");
                assert_eq!(f.params.len(), 0);
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_function_with_default_param() {
        let items = parse_items(
            "fn greet:\n  name: string = \"world\"\n-> string {\n  name\n}",
        );
        assert_eq!(items.len(), 1);
        match &items[0] {
            Item::Function(f) => {
                assert_eq!(f.params[0].default.is_some(), true);
            }
            _ => panic!("expected function"),
        }
    }

    // ── Type alias ───────────────────────────────────────────────────────

    #[test]
    fn parse_type_alias_struct() {
        let items = parse_items("type Point = { x: int, y: int }");
        assert_eq!(items.len(), 1);
        match &items[0] {
            Item::TypeAlias(ta) => {
                assert_eq!(ta.name.name, "Point");
                match &ta.ty {
                    TypeExpr::Struct { fields, .. } => {
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].name.name, "x");
                        assert_eq!(fields[1].name.name, "y");
                    }
                    _ => panic!("expected struct type"),
                }
            }
            _ => panic!("expected type alias"),
        }
    }

    #[test]
    fn parse_type_alias_named() {
        let items = parse_items("type Age = int");
        assert_eq!(items.len(), 1);
        match &items[0] {
            Item::TypeAlias(ta) => {
                assert_eq!(ta.name.name, "Age");
                match &ta.ty {
                    TypeExpr::Named(id) => assert_eq!(id.name, "int"),
                    _ => panic!("expected named type"),
                }
            }
            _ => panic!("expected type alias"),
        }
    }

    // ── Expressions ──────────────────────────────────────────────────────

    #[test]
    fn parse_binary_precedence() {
        // a + b * c should parse as a + (b * c)
        let items = parse_items("fn f: -> int { a + b * c }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::Binary(bin) => {
                        assert_eq!(bin.op, BinOp::Add);
                        match &bin.right {
                            Expr::Binary(inner) => {
                                assert_eq!(inner.op, BinOp::Mul);
                            }
                            _ => panic!("expected binary expr on right"),
                        }
                    }
                    _ => panic!("expected binary expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_unary_negation() {
        let items = parse_items("fn f: -> int { -x }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::Unary(u) => {
                        assert_eq!(u.op, UnaryOp::Neg);
                    }
                    _ => panic!("expected unary expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_if_else() {
        let items = parse_items(
            "fn f:\n  x: int\n-> int {\n  if x > 0 { x } else { -x }\n}",
        );
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::If(if_expr) => {
                        assert!(if_expr.else_branch.is_some());
                    }
                    _ => panic!("expected if expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_let_binding() {
        let items = parse_items("fn f: -> int {\n  let x = 42;\n  x\n}");
        match &items[0] {
            Item::Function(f) => {
                assert_eq!(f.body.stmts.len(), 1);
                match &f.body.stmts[0] {
                    Stmt::Let(l) => {
                        assert_eq!(l.name.name, "x");
                    }
                    _ => panic!("expected let stmt"),
                }
                assert!(f.body.tail_expr.is_some());
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_function_call() {
        let items = parse_items("fn f: -> int { add(a: 1, b: 2) }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::Call(call) => {
                        assert_eq!(call.function.name, "add");
                        assert_eq!(call.args.len(), 2);
                        assert_eq!(call.args[0].name.name, "a");
                        assert_eq!(call.args[1].name.name, "b");
                    }
                    _ => panic!("expected call expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_struct_expr() {
        let items = parse_items("fn f: -> int { { x: 1, y: 2 } }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::Struct(s) => {
                        assert_eq!(s.fields.len(), 2);
                    }
                    _ => panic!("expected struct expr, got {:?}", tail),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_field_access() {
        let items = parse_items("fn f:\n  p: Point\n-> int { p.x }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::FieldAccess(fa) => {
                        assert_eq!(fa.field.name, "x");
                    }
                    _ => panic!("expected field access"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_multiple_items() {
        let items = parse_items(
            "type Point = { x: int, y: int }\nfn origin: -> Point { { x: 0, y: 0 } }",
        );
        assert_eq!(items.len(), 2);
        assert!(matches!(&items[0], Item::TypeAlias(_)));
        assert!(matches!(&items[1], Item::Function(_)));
    }

    #[test]
    fn parse_attribute_on_function() {
        let items = parse_items(
            "@deprecated\nfn old: -> int { 0 }",
        );
        match &items[0] {
            Item::Function(f) => {
                assert_eq!(f.attributes.len(), 1);
                assert_eq!(f.attributes[0].name.name, "deprecated");
            }
            _ => panic!("expected function"),
        }
    }

    // ── Error recovery ───────────────────────────────────────────────────

    #[test]
    fn error_recovery_missing_return_type() {
        let (ast, errors) = parse_with_errors("fn broken:\n  x: int\nfn works: -> int { 42 }");
        // Should recover and parse the second function
        assert!(!errors.is_empty());
        // The second function should parse correctly
        let funcs: Vec<_> = ast.items.iter().filter_map(|i| match i {
            Item::Function(f) => Some(f),
            _ => None,
        }).collect();
        assert!(funcs.iter().any(|f| f.name.name == "works"));
    }

    #[test]
    fn parse_boolean_operators() {
        let items = parse_items(
            "fn f:\n  a: bool\n  b: bool\n-> bool { a and b or not a }",
        );
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                // Should parse as (a and b) or (not a) due to precedence
                match tail.as_ref() {
                    Expr::Binary(bin) => {
                        assert_eq!(bin.op, BinOp::Or);
                    }
                    _ => panic!("expected binary expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_return_statement() {
        let items = parse_items("fn f: -> int { return 42 }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::Return(ret) => {
                        assert!(ret.value.is_some());
                    }
                    _ => panic!("expected return expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn parse_comparison_chain() {
        let items = parse_items("fn f:\n  x: int\n-> bool { x >= 0 and x <= 100 }");
        match &items[0] {
            Item::Function(f) => {
                let tail = f.body.tail_expr.as_ref().unwrap();
                match tail.as_ref() {
                    Expr::Binary(bin) => {
                        assert_eq!(bin.op, BinOp::And);
                    }
                    _ => panic!("expected binary expr"),
                }
            }
            _ => panic!("expected function"),
        }
    }
}
