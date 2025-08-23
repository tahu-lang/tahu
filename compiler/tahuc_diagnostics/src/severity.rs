#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Help,
    Note,
    Info,
    Warning,
    Error,
    Fatal,
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Help => "help",
            Severity::Note => "note",
            Severity::Info => "info",
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Fatal => "fatal",
        }
    }

    pub fn color_code(&self) -> &'static str {
        match self {
            Severity::Help => "\x1b[36m",      // Cyan
            Severity::Note => "\x1b[34m",      // Blue
            Severity::Info => "\x1b[32m",      // Green
            Severity::Warning => "\x1b[33m",   // Yellow
            Severity::Error => "\x1b[31m",     // Red
            Severity::Fatal => "\x1b[35m",     // Magenta
        }
    }

    pub fn reset_code() -> &'static str {
        "\x1b[0m"
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Severity::Error | Severity::Fatal)
    }
}