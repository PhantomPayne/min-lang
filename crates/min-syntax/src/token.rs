use min_diagnostics::Span;

/// All token kinds in the Min language
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Keywords
    Fn,       // fn
    Let,      // let
    Return,   // return
    If,       // if
    Else,     // else
    Type,     // type
    And,      // and
    Or,       // or
    Not,      // not
    True,     // true
    False,    // false

    // Literals
    IntLit,    // 123
    FloatLit,  // 3.14
    StringLit, // "hello"

    // Identifiers
    Ident, // foo, bar_baz

    // Operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %
    EqEq,    // ==
    Neq,     // !=
    Lt,      // <
    Gt,      // >
    Lte,     // <=
    Gte,     // >=
    Eq,      // =

    // Delimiters
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    Arrow,     // ->
    Dot,       // .
    At,        // @

    // String interpolation
    StringStart,  // "hello {
    StringMiddle, // } world {
    StringEnd,    // } end"

    // Trivia
    Whitespace,
    Newline,
    Comment,   // // comment
    DocString, // """..."""

    // Special
    Eof,
    Error,
}

impl TokenKind {
    /// Returns true if this token is trivia (whitespace, comments, etc.)
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Newline | Self::Comment)
    }

    /// Returns true if this token is a keyword
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            Self::Fn
                | Self::Let
                | Self::Return
                | Self::If
                | Self::Else
                | Self::Type
                | Self::And
                | Self::Or
                | Self::Not
                | Self::True
                | Self::False
        )
    }

    /// Returns true if this token can start an expression
    pub fn can_start_expr(self) -> bool {
        matches!(
            self,
            Self::IntLit
                | Self::FloatLit
                | Self::StringLit
                | Self::StringStart
                | Self::True
                | Self::False
                | Self::Ident
                | Self::LParen
                | Self::LBrace
                | Self::If
                | Self::Not
                | Self::Minus
                | Self::Return
        )
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn => write!(f, "fn"),
            Self::Let => write!(f, "let"),
            Self::Return => write!(f, "return"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Type => write!(f, "type"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Not => write!(f, "not"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::IntLit => write!(f, "integer"),
            Self::FloatLit => write!(f, "float"),
            Self::StringLit => write!(f, "string"),
            Self::Ident => write!(f, "identifier"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::EqEq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Lte => write!(f, "<="),
            Self::Gte => write!(f, ">="),
            Self::Eq => write!(f, "="),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Arrow => write!(f, "->"),
            Self::Dot => write!(f, "."),
            Self::At => write!(f, "@"),
            Self::StringStart => write!(f, "string start"),
            Self::StringMiddle => write!(f, "string middle"),
            Self::StringEnd => write!(f, "string end"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::Newline => write!(f, "newline"),
            Self::Comment => write!(f, "comment"),
            Self::DocString => write!(f, "docstring"),
            Self::Eof => write!(f, "end of file"),
            Self::Error => write!(f, "error"),
        }
    }
}

/// A token with its kind, text content, and source span
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, text: impl Into<String>, span: Span) -> Self {
        Self {
            kind,
            text: text.into(),
            span,
        }
    }

    /// Returns a keyword TokenKind if the text is a keyword, otherwise Ident
    pub fn keyword_or_ident(text: &str) -> TokenKind {
        match text {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "type" => TokenKind::Type,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident,
        }
    }
}
