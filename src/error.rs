use crate::parse::Rule;
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{emit, Config};
use pest::error::InputLocation;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct Error(Diagnostic);
impl Error {
    pub fn new(
        file: FileId,
        primary: impl Into<String>,
        span: impl Into<Span>,
        secondary: impl Into<String>,
    ) -> Self {
        let d = Diagnostic::new_error(primary, Label::new(file, span, secondary));
        Error(d)
    }
    pub fn from_pest(pest_err: pest::error::Error<Rule>, file: FileId) -> Self {
        match &pest_err {
            pest::error::Error {
                variant:
                    pest::error::ErrorVariant::ParsingError {
                        positives,
                        negatives,
                    },
                location,
                ..
            } => {
                let mut expected = String::new();
                let mut unexpected = String::new();
                let p_len = positives.len();
                for (n, i) in positives.iter().enumerate() {
                    if !expected.is_empty() {
                        expected.push_str(", ");
                        if n == p_len - 1 {
                            expected.push_str("or ");
                        }
                    }
                    expected.push_str(i.format());
                }
                let n_len = negatives.len();
                for (n, i) in negatives.iter().enumerate() {
                    if !unexpected.is_empty() {
                        unexpected.push_str(", ");
                        if n == n_len - 1 {
                            unexpected.push_str("or ");
                        }
                    }
                    unexpected.push_str(i.format());
                }

                let (start, end) = match location {
                    InputLocation::Span(t) => *t,
                    InputLocation::Pos(u) => (*u, *u),
                };

                let message = if !unexpected.is_empty() && !expected.is_empty() {
                    format!("unexpected {}, expected {}", unexpected, expected)
                } else if !unexpected.is_empty() {
                    format!("unexpected {}", unexpected)
                } else {
                    format!("expected {}", expected)
                };

                let span = Span::new(start as u32, end as u32);
                Error::new(file, format!("Parse error: {}", message), span, message)
            }
            pest::error::Error { location, .. } => {
                let (start, end) = match location {
                    InputLocation::Span(t) => *t,
                    InputLocation::Pos(u) => (*u, *u),
                };
                let span = Span::new(start as u32, end as u32);
                Error::new(file, format!("Pest error:\n{}", pest_err), span, "Here")
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct ErrorContext {
    files: Files,
    writer: RefCell<termcolor::StandardStream>,
    config: Config,
}
impl ErrorContext {
    pub fn new() -> Self {
        ErrorContext {
            files: Files::new(),
            writer: RefCell::new(termcolor::StandardStream::stderr(
                termcolor::ColorChoice::Always,
            )),
            config: Config::default(),
        }
    }
    pub fn add_file(&mut self, name: impl Into<String>, source: impl Into<String>) -> FileId {
        self.files.add(name, source)
    }
    pub fn write_error(&self, error: Error) -> std::io::Result<()> {
        emit(
            &mut *self.writer.borrow_mut(),
            &self.config,
            &self.files,
            &error.0,
        )
    }
}
