use crate::token::{Token, TokenKind};
use min_diagnostics::{Diagnostic, Span};

pub struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    pos: usize,
    char_pos: usize,
    tokens: Vec<Token>,
    errors: Vec<Diagnostic>,
    interp_depth: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            chars: source.chars().collect(),
            pos: 0,
            char_pos: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
            interp_depth: 0,
        }
    }

    pub fn tokenize(mut self) -> (Vec<Token>, Vec<Diagnostic>) {
        while self.current().is_some() {
            self.scan_token();
        }
        let eof_pos = self.byte_pos();
        self.tokens.push(Token::new(
            TokenKind::Eof,
            "",
            Span::new(eof_pos, eof_pos),
        ));
        (self.tokens, self.errors)
    }

    // ── helpers ──────────────────────────────────────────────────────────

    fn current(&self) -> Option<char> {
        self.chars.get(self.char_pos).copied()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.char_pos + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.char_pos).copied()?;
        self.pos += ch.len_utf8();
        self.char_pos += 1;
        Some(ch)
    }

    fn byte_pos(&self) -> u32 {
        self.pos as u32
    }

    fn skip_while(&mut self, pred: impl Fn(char) -> bool) {
        while let Some(ch) = self.current() {
            if pred(ch) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn eat(&mut self, ch: char) -> bool {
        if self.current() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Extract the source text between two byte positions.
    fn text_between(&self, start: u32, end: u32) -> &'a str {
        &self.source[start as usize..end as usize]
    }

    /// Push a token built from the current byte range.
    fn emit(&mut self, kind: TokenKind, start: u32) {
        let end = self.byte_pos();
        let text = self.text_between(start, end);
        self.tokens.push(Token::new(kind, text, Span::new(start, end)));
    }

    // ── scanning ─────────────────────────────────────────────────────────

    fn scan_token(&mut self) {
        // If we are inside a string interpolation and see `}`, continue the
        // string rather than emitting a normal RBrace.
        if self.interp_depth > 0 && self.current() == Some('}') {
            self.scan_string_continuation();
            return;
        }

        let start = self.byte_pos();
        let ch = match self.current() {
            Some(c) => c,
            None => return,
        };

        match ch {
            // ── whitespace (spaces / tabs) ───────────────────────────
            ' ' | '\t' => {
                self.advance();
                self.skip_while(|c| c == ' ' || c == '\t');
                self.emit(TokenKind::Whitespace, start);
            }

            // ── newlines ─────────────────────────────────────────────
            '\n' => {
                self.advance();
                self.emit(TokenKind::Newline, start);
            }
            '\r' => {
                self.advance();
                // consume optional \n after \r
                self.eat('\n');
                self.emit(TokenKind::Newline, start);
            }

            // ── slash or line comment ────────────────────────────────
            '/' => {
                if self.peek() == Some('/') {
                    self.scan_comment(start);
                } else {
                    self.advance();
                    self.emit(TokenKind::Slash, start);
                }
            }

            // ── strings / docstrings ─────────────────────────────────
            '"' => {
                if self.peek() == Some('"')
                    && self.chars.get(self.char_pos + 2).copied() == Some('"')
                {
                    self.scan_docstring(start);
                } else {
                    self.scan_string(start);
                }
            }

            // ── numbers ──────────────────────────────────────────────
            '0'..='9' => self.scan_number(start),

            // ── identifiers / keywords ───────────────────────────────
            'a'..='z' | 'A'..='Z' | '_' => self.scan_ident(start),

            // ── operators ────────────────────────────────────────────
            '+' => {
                self.advance();
                self.emit(TokenKind::Plus, start);
            }
            '-' => {
                self.advance();
                if self.eat('>') {
                    self.emit(TokenKind::Arrow, start);
                } else {
                    self.emit(TokenKind::Minus, start);
                }
            }
            '*' => {
                self.advance();
                self.emit(TokenKind::Star, start);
            }
            '%' => {
                self.advance();
                self.emit(TokenKind::Percent, start);
            }
            '=' => {
                self.advance();
                if self.eat('=') {
                    self.emit(TokenKind::EqEq, start);
                } else {
                    self.emit(TokenKind::Eq, start);
                }
            }
            '!' => {
                self.advance();
                if self.eat('=') {
                    self.emit(TokenKind::Neq, start);
                } else {
                    let end = self.byte_pos();
                    let text = self.text_between(start, end);
                    self.tokens.push(Token::new(
                        TokenKind::Error,
                        text,
                        Span::new(start, end),
                    ));
                    self.errors.push(Diagnostic::error(
                        "unexpected character '!'",
                        Span::new(start, end),
                    ));
                }
            }
            '<' => {
                self.advance();
                if self.eat('=') {
                    self.emit(TokenKind::Lte, start);
                } else {
                    self.emit(TokenKind::Lt, start);
                }
            }
            '>' => {
                self.advance();
                if self.eat('=') {
                    self.emit(TokenKind::Gte, start);
                } else {
                    self.emit(TokenKind::Gt, start);
                }
            }

            // ── delimiters ───────────────────────────────────────────
            '(' => {
                self.advance();
                self.emit(TokenKind::LParen, start);
            }
            ')' => {
                self.advance();
                self.emit(TokenKind::RParen, start);
            }
            '{' => {
                self.advance();
                self.emit(TokenKind::LBrace, start);
            }
            '}' => {
                self.advance();
                self.emit(TokenKind::RBrace, start);
            }
            ',' => {
                self.advance();
                self.emit(TokenKind::Comma, start);
            }
            ':' => {
                self.advance();
                self.emit(TokenKind::Colon, start);
            }
            ';' => {
                self.advance();
                self.emit(TokenKind::Semicolon, start);
            }
            '.' => {
                self.advance();
                self.emit(TokenKind::Dot, start);
            }
            '@' => {
                self.advance();
                self.emit(TokenKind::At, start);
            }

            // ── unknown character ────────────────────────────────────
            other => {
                self.advance();
                let end = self.byte_pos();
                let text = self.text_between(start, end);
                self.tokens.push(Token::new(
                    TokenKind::Error,
                    text,
                    Span::new(start, end),
                ));
                self.errors.push(Diagnostic::error(
                    format!("unexpected character '{other}'"),
                    Span::new(start, end),
                ));
            }
        }
    }

    // ── comment ──────────────────────────────────────────────────────────

    fn scan_comment(&mut self, start: u32) {
        // consume the two slashes
        self.advance(); // '/'
        self.advance(); // '/'
        // consume until end of line (but not the newline itself)
        self.skip_while(|c| c != '\n' && c != '\r');
        self.emit(TokenKind::Comment, start);
    }

    // ── numbers ──────────────────────────────────────────────────────────

    fn scan_number(&mut self, start: u32) {
        self.skip_while(|c| c.is_ascii_digit());

        // check for fractional part: `.` followed by a digit
        if self.current() == Some('.') {
            if let Some(next) = self.peek() {
                if next.is_ascii_digit() {
                    self.advance(); // consume '.'
                    self.skip_while(|c| c.is_ascii_digit());
                    self.emit(TokenKind::FloatLit, start);
                    return;
                }
            }
        }
        self.emit(TokenKind::IntLit, start);
    }

    // ── identifiers / keywords ───────────────────────────────────────────

    fn scan_ident(&mut self, start: u32) {
        self.skip_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let end = self.byte_pos();
        let text = self.text_between(start, end);
        let kind = Token::keyword_or_ident(text);
        self.tokens
            .push(Token::new(kind, text, Span::new(start, end)));
    }

    // ── docstrings ───────────────────────────────────────────────────────

    fn scan_docstring(&mut self, start: u32) {
        // consume opening """
        self.advance(); // "
        self.advance(); // "
        self.advance(); // "

        loop {
            match self.current() {
                None => {
                    // unterminated docstring
                    let end = self.byte_pos();
                    let text = self.text_between(start, end);
                    self.tokens.push(Token::new(
                        TokenKind::Error,
                        text,
                        Span::new(start, end),
                    ));
                    self.errors.push(Diagnostic::error(
                        "unterminated docstring",
                        Span::new(start, end),
                    ));
                    return;
                }
                Some('"')
                    if self.peek() == Some('"')
                        && self.chars.get(self.char_pos + 2).copied() == Some('"') =>
                {
                    // closing """
                    self.advance(); // "
                    self.advance(); // "
                    self.advance(); // "
                    self.emit(TokenKind::DocString, start);
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    // ── strings (with interpolation support) ─────────────────────────────

    /// Called when we encounter the opening `"` of a string literal.
    fn scan_string(&mut self, start: u32) {
        // consume the opening "
        self.advance();

        loop {
            match self.current() {
                None => {
                    // unterminated string
                    let end = self.byte_pos();
                    let text = self.text_between(start, end);
                    self.tokens.push(Token::new(
                        TokenKind::Error,
                        text,
                        Span::new(start, end),
                    ));
                    self.errors.push(Diagnostic::error(
                        "unterminated string literal",
                        Span::new(start, end),
                    ));
                    return;
                }
                Some('\\') => {
                    // escape sequence – consume backslash and next char
                    self.advance();
                    self.advance();
                }
                Some('{') => {
                    // interpolation start
                    self.advance(); // consume '{'
                    self.interp_depth += 1;
                    self.emit(TokenKind::StringStart, start);
                    return;
                }
                Some('"') => {
                    // end of a plain string (no interpolation)
                    self.advance(); // consume closing "
                    self.emit(TokenKind::StringLit, start);
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Called when `interp_depth > 0` and we encounter `}`.
    /// We need to continue scanning the rest of the string after the
    /// interpolated expression.
    fn scan_string_continuation(&mut self) {
        let start = self.byte_pos();
        // consume the closing '}'
        self.advance();

        loop {
            match self.current() {
                None => {
                    // unterminated string
                    let end = self.byte_pos();
                    let text = self.text_between(start, end);
                    self.tokens.push(Token::new(
                        TokenKind::Error,
                        text,
                        Span::new(start, end),
                    ));
                    self.errors.push(Diagnostic::error(
                        "unterminated string literal",
                        Span::new(start, end),
                    ));
                    self.interp_depth -= 1;
                    return;
                }
                Some('\\') => {
                    // escape sequence
                    self.advance();
                    self.advance();
                }
                Some('{') => {
                    // another interpolation
                    self.advance(); // consume '{'
                    self.emit(TokenKind::StringMiddle, start);
                    // interp_depth stays the same (we closed one `}` and
                    // opened another `{`)
                    return;
                }
                Some('"') => {
                    // end of the string
                    self.advance(); // consume closing "
                    self.interp_depth -= 1;
                    self.emit(TokenKind::StringEnd, start);
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: tokenize and return only the non-trivia tokens (no whitespace/newline/comment).
    fn tok(src: &str) -> Vec<(TokenKind, &str)> {
        let (tokens, _) = Lexer::new(src).tokenize();
        tokens
            .iter()
            .filter(|t| !t.kind.is_trivia() && t.kind != TokenKind::Eof)
            .map(|t| (t.kind, &src[t.span.start as usize..t.span.end as usize]))
            .collect()
    }

    /// Helper: tokenize and return *all* tokens including trivia.
    fn tok_all(src: &str) -> Vec<(TokenKind, &str)> {
        let (tokens, _) = Lexer::new(src).tokenize();
        tokens
            .iter()
            .filter(|t| t.kind != TokenKind::Eof)
            .map(|t| (t.kind, &src[t.span.start as usize..t.span.end as usize]))
            .collect()
    }

    #[test]
    fn empty_source() {
        let (tokens, errors) = Lexer::new("").tokenize();
        assert!(errors.is_empty());
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn single_operators() {
        let result = tok("+ - * / %");
        assert_eq!(
            result,
            vec![
                (TokenKind::Plus, "+"),
                (TokenKind::Minus, "-"),
                (TokenKind::Star, "*"),
                (TokenKind::Slash, "/"),
                (TokenKind::Percent, "%"),
            ]
        );
    }

    #[test]
    fn two_char_operators() {
        let result = tok("== != <= >= -> ");
        assert_eq!(
            result,
            vec![
                (TokenKind::EqEq, "=="),
                (TokenKind::Neq, "!="),
                (TokenKind::Lte, "<="),
                (TokenKind::Gte, ">="),
                (TokenKind::Arrow, "->"),
            ]
        );
    }

    #[test]
    fn delimiters() {
        let result = tok("( ) { } , : ; . @");
        assert_eq!(
            result,
            vec![
                (TokenKind::LParen, "("),
                (TokenKind::RParen, ")"),
                (TokenKind::LBrace, "{"),
                (TokenKind::RBrace, "}"),
                (TokenKind::Comma, ","),
                (TokenKind::Colon, ":"),
                (TokenKind::Semicolon, ";"),
                (TokenKind::Dot, "."),
                (TokenKind::At, "@"),
            ]
        );
    }

    #[test]
    fn keywords() {
        let result = tok("fn let return if else type and or not true false");
        let kinds: Vec<TokenKind> = result.iter().map(|(k, _)| *k).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Fn,
                TokenKind::Let,
                TokenKind::Return,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::Type,
                TokenKind::And,
                TokenKind::Or,
                TokenKind::Not,
                TokenKind::True,
                TokenKind::False,
            ]
        );
    }

    #[test]
    fn identifiers() {
        let result = tok("foo bar_baz _x Hello123");
        assert_eq!(
            result,
            vec![
                (TokenKind::Ident, "foo"),
                (TokenKind::Ident, "bar_baz"),
                (TokenKind::Ident, "_x"),
                (TokenKind::Ident, "Hello123"),
            ]
        );
    }

    #[test]
    fn integer_literals() {
        let result = tok("0 42 12345");
        assert_eq!(
            result,
            vec![
                (TokenKind::IntLit, "0"),
                (TokenKind::IntLit, "42"),
                (TokenKind::IntLit, "12345"),
            ]
        );
    }

    #[test]
    fn float_literals() {
        let result = tok("3.14 0.5 100.0");
        assert_eq!(
            result,
            vec![
                (TokenKind::FloatLit, "3.14"),
                (TokenKind::FloatLit, "0.5"),
                (TokenKind::FloatLit, "100.0"),
            ]
        );
    }

    #[test]
    fn integer_followed_by_dot_no_digit() {
        // `42.foo` should be IntLit(42), Dot, Ident(foo)
        let result = tok("42.foo");
        assert_eq!(
            result,
            vec![
                (TokenKind::IntLit, "42"),
                (TokenKind::Dot, "."),
                (TokenKind::Ident, "foo"),
            ]
        );
    }

    #[test]
    fn simple_string() {
        let result = tok(r#""hello""#);
        assert_eq!(result, vec![(TokenKind::StringLit, r#""hello""#)]);
    }

    #[test]
    fn string_with_escape() {
        let result = tok(r#""he\"llo""#);
        assert_eq!(result, vec![(TokenKind::StringLit, r#""he\"llo""#)]);
    }

    #[test]
    fn interpolated_string() {
        let src = r#""hello {name}!""#;
        let result = tok(src);
        assert_eq!(
            result,
            vec![
                (TokenKind::StringStart, "\"hello {"),
                (TokenKind::Ident, "name"),
                (TokenKind::StringEnd, "}!\""),
            ]
        );
    }

    #[test]
    fn interpolated_string_multiple() {
        let src = r#""a{x}b{y}c""#;
        let result = tok(src);
        assert_eq!(
            result,
            vec![
                (TokenKind::StringStart, "\"a{"),
                (TokenKind::Ident, "x"),
                (TokenKind::StringMiddle, "}b{"),
                (TokenKind::Ident, "y"),
                (TokenKind::StringEnd, "}c\""),
            ]
        );
    }

    #[test]
    fn docstring() {
        let src = r#""""This is a docstring""""#;
        let result = tok(src);
        assert_eq!(
            result,
            vec![(TokenKind::DocString, r#""""This is a docstring""""#)]
        );
    }

    #[test]
    fn line_comment() {
        let result = tok_all("x // a comment\ny");
        assert_eq!(
            result,
            vec![
                (TokenKind::Ident, "x"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Comment, "// a comment"),
                (TokenKind::Newline, "\n"),
                (TokenKind::Ident, "y"),
            ]
        );
    }

    #[test]
    fn whitespace_and_newlines() {
        let result = tok_all("  \n\t\t");
        assert_eq!(
            result,
            vec![
                (TokenKind::Whitespace, "  "),
                (TokenKind::Newline, "\n"),
                (TokenKind::Whitespace, "\t\t"),
            ]
        );
    }

    #[test]
    fn unknown_character_produces_error() {
        let (tokens, errors) = Lexer::new("~").tokenize();
        assert_eq!(tokens[0].kind, TokenKind::Error);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn bang_alone_is_error() {
        let (tokens, errors) = Lexer::new("!").tokenize();
        assert_eq!(tokens[0].kind, TokenKind::Error);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unterminated_string_produces_error() {
        let (_tokens, errors) = Lexer::new("\"hello").tokenize();
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unterminated_docstring_produces_error() {
        let (_tokens, errors) = Lexer::new("\"\"\"hello").tokenize();
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn span_accuracy() {
        let src = "fn foo";
        let (tokens, _) = Lexer::new(src).tokenize();
        // fn
        assert_eq!(tokens[0].span, Span::new(0, 2));
        // whitespace
        assert_eq!(tokens[1].span, Span::new(2, 3));
        // foo
        assert_eq!(tokens[2].span, Span::new(3, 6));
    }

    #[test]
    fn full_function() {
        let src = "fn add(a: Int, b: Int) -> Int { return a + b }";
        let result = tok(src);
        let kinds: Vec<TokenKind> = result.iter().map(|(k, _)| *k).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Fn,
                TokenKind::Ident,  // add
                TokenKind::LParen,
                TokenKind::Ident,  // a
                TokenKind::Colon,
                TokenKind::Ident,  // Int
                TokenKind::Comma,
                TokenKind::Ident,  // b
                TokenKind::Colon,
                TokenKind::Ident,  // Int
                TokenKind::RParen,
                TokenKind::Arrow,
                TokenKind::Ident,  // Int
                TokenKind::LBrace,
                TokenKind::Return,
                TokenKind::Ident,  // a
                TokenKind::Plus,
                TokenKind::Ident,  // b
                TokenKind::RBrace,
            ]
        );
    }
}
