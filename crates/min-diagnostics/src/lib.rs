use std::fmt;

/// A byte-offset span in source text.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Create a new span from a start and end byte offset.
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    /// Merge two spans, producing a span that covers both.
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// The length of the span in bytes.
    pub fn len(&self) -> u32 {
        self.end - self.start
    }
}

/// The severity level of a diagnostic.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Info => write!(f, "info"),
        }
    }
}

/// A labeled span within a diagnostic, pointing at a region of source text
/// with an accompanying message.
#[derive(Clone, Debug)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

/// A suggested text replacement.
#[derive(Clone, Debug)]
pub struct TextEdit {
    pub span: Span,
    pub new_text: String,
}

/// A suggested fix for a diagnostic, consisting of a message and one or more
/// text edits.
#[derive(Clone, Debug)]
pub struct Fix {
    pub message: String,
    pub edits: Vec<TextEdit>,
}

/// A compiler diagnostic (error, warning, or informational message).
#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub primary_label: Label,
    pub secondary_labels: Vec<Label>,
    pub notes: Vec<String>,
    pub fixes: Vec<Fix>,
}

impl Diagnostic {
    /// Create an error diagnostic with the given message and primary span.
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        let message = message.into();
        Self {
            severity: Severity::Error,
            code: None,
            message: message.clone(),
            primary_label: Label {
                span,
                message,
            },
            secondary_labels: Vec::new(),
            notes: Vec::new(),
            fixes: Vec::new(),
        }
    }

    /// Create a warning diagnostic with the given message and primary span.
    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        let message = message.into();
        Self {
            severity: Severity::Warning,
            code: None,
            message: message.clone(),
            primary_label: Label {
                span,
                message,
            },
            secondary_labels: Vec::new(),
            notes: Vec::new(),
            fixes: Vec::new(),
        }
    }

    /// Add a secondary label to this diagnostic.
    pub fn with_label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.secondary_labels.push(Label {
            span,
            message: message.into(),
        });
        self
    }

    /// Add a note to this diagnostic.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Add a suggested fix to this diagnostic.
    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.fixes.push(fix);
        self
    }
}
