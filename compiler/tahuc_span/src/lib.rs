use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub file_id: FileId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position {
    pub line: u32,    // 1-based
    pub column: u32,  // 1-based
    pub offset: u32,  // 0-based byte offset
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

impl Span {
    pub fn new(start: Position, end: Position, file_id: FileId) -> Self {
        Self { start, end, file_id }
    }

    pub fn point(pos: Position, file_id: FileId) -> Self {
        Self::new(pos, pos, file_id)
    }

    pub fn merge(self, other: Span) -> Span {
        debug_assert_eq!(self.file_id, other.file_id);
        Span {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
            file_id: self.file_id,
        }
    }

    pub fn contains(&self, pos: Position) -> bool {
        self.start <= pos && pos <= self.end
    }

    pub fn len(&self) -> u32 {
        self.end.offset - self.start.offset
    }

    pub fn dummy() -> Span {
        Span::new(Position::new(0, 0, 0), Position::new(0, 0, 0), FileId(0))
    }
}

impl Position {
    pub fn new(line: u32, column: u32, offset: u32) -> Self {
        Self { line, column, offset }
    }

    pub fn advance_line(&mut self) {
        self.line += 1;
        self.column = 1;
        self.offset += 1;
    }

    pub fn advance_column(&mut self) {
        self.column += 1;
        self.offset += 1;
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: FileId,
    pub path: String,
    pub content: String,
    pub lines: Vec<usize>, // Byte offsets of line starts
}

impl SourceFile {
    pub fn new(id: FileId, path: String, content: &String) -> Self {
        let mut lines = vec![0];
        for (i, ch) in content.char_indices() {
            if ch == '\n' {
                lines.push(i + 1);
            }
        }
        let path = pretty_path_from_string(path);
        let content = content.to_string();
        Self { id, path, content, lines }
    }

    pub fn line_content(&self, line: u32) -> Option<&str> {
        let line_idx = (line as usize).saturating_sub(1);
        let start = *self.lines.get(line_idx)?;
        let end = self.lines.get(line_idx + 1).copied().unwrap_or(self.content.len());
        Some(&self.content[start..end])
    }

    pub fn span_content(&self, span: Span) -> Option<&str> {
        let start = span.start.offset as usize;
        let end = span.end.offset as usize;
        self.content.get(start..end)
    }
}

fn pretty_path(path: &PathBuf) -> String {
    let s = path.display().to_string();
    if cfg!(windows) {
        s.trim_start_matches(r"\\?\").to_string()
    } else {
        s
    }
}

fn pretty_path_from_string(path: String) -> String {
    let path = PathBuf::from(path);
    pretty_path(&path)
}