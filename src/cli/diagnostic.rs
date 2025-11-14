use crate::syntax::lexer::Span;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineCol {
    line: usize,
    col: usize,
}

impl fmt::Display for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let LineCol { line, col } = self;
        // index starts from 0 internally, but prints from 1
        write!(f, "line {}, col {}", line + 1, col + 1)
    }
}

fn get_line_col_vec(source: &str) -> Vec<LineCol> {
    let mut vec = Vec::with_capacity(source.len());
    let mut line = 0;
    let mut col = 0;
    for ch in source.chars() {
        vec.push(LineCol { line, col });
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    vec
}

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum DiagLevel {
    Error,
    Warn,
    Info,
}

impl fmt::Display for DiagLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DiagLevel::Error => write!(f, "Error"),
            DiagLevel::Warn => write!(f, "Warn"),
            DiagLevel::Info => write!(f, "Info"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Description {
    verbosity: u8,
    message: String,
    span: Option<Span>,
}

impl Description {
    pub fn message<S: Into<String>>(msg: S) -> Description {
        Description {
            verbosity: 10,
            message: msg.into(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Description {
        self.span = Some(span);
        self
    }

    pub fn with_verbosity(mut self, verbosity: u8) -> Description {
        self.verbosity = verbosity;
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Diagnostic {
    pub level: DiagLevel,
    title: String,
    descriptions: Vec<Description>,
}

impl Diagnostic {
    pub fn error<S: Into<String>>(title: S) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Error,
            title: title.into(),
            descriptions: Vec::new(),
        }
    }

    pub fn warn<S: Into<String>>(title: S) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Warn,
            title: title.into(),
            descriptions: Vec::new(),
        }
    }

    pub fn info<S: Into<String>>(title: S) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Info,
            title: title.into(),
            descriptions: Vec::new(),
        }
    }

    pub fn line<S: Into<String>>(mut self, msg: S) -> Diagnostic {
        self.descriptions.push(Description::message(msg));
        self
    }

    pub fn line_span<S: Into<String>>(mut self, span: Span, msg: S) -> Diagnostic {
        self.descriptions
            .push(Description::message(msg).with_span(span));
        self
    }

    pub fn line_verb<S: Into<String>>(mut self, verbosity: u8, msg: S) -> Diagnostic {
        self.descriptions
            .push(Description::message(msg).with_verbosity(verbosity));
        self
    }

    pub fn line_span_verb<S: Into<String>>(
        mut self,
        span: Span,
        verbosity: u8,
        msg: S,
    ) -> Diagnostic {
        self.descriptions.push(
            Description::message(msg)
                .with_span(span)
                .with_verbosity(verbosity),
        );
        self
    }

    /// minimal_report shows only spans, instead of source code.
    pub fn minimal_report(&self, source: &str, verbosity: u8) -> String {
        let mut output = format!("[{}]: {}\n", self.level, &self.title);
        let linecol = get_line_col_vec(source);
        for descr in &self.descriptions {
            if descr.verbosity > verbosity {
                // ignore those description with higher verbosity
                continue;
            }
            match &descr.span {
                Some(span) => {
                    output.push_str(&format!(
                        "{} - {}:\n{}\n",
                        linecol[span.start], linecol[span.end], descr.message,
                    ));
                }
                None => {
                    output.push_str(&descr.message);
                    output.push('\n');
                }
            }
        }
        output
    }

    pub fn report(&self, source: &str, verbosity: u8) -> String {
        let mut output = format!("[{}]: {}\n", self.level, &self.title);
        let linecol = get_line_col_vec(source);
        for descr in &self.descriptions {
            if descr.verbosity > verbosity {
                // ignore those description with higher verbosity
                continue;
            }
            match &descr.span {
                Some(span) => {
                    let start_line = linecol[span.start].line;
                    let start_col = linecol[span.start].col;
                    let end_line = linecol[span.end].line;
                    let end_col = linecol[span.end].col;
                    let text = source.lines().collect::<Vec<&str>>();
                    let mut vec: Vec<(usize, usize)> = Vec::new();
                    if start_line == end_line {
                        vec.push((start_col, end_col))
                    } else {
                        for (idx, line) in
                            text.iter().enumerate().take(end_line + 1).skip(start_line)
                        {
                            if idx == start_line {
                                vec.push((start_col, line.chars().count()))
                            } else if idx == end_line {
                                vec.push((0, end_col))
                            } else {
                                vec.push((0, line.chars().count()))
                            }
                        }
                    }

                    let head_width = (1 + end_line).to_string().len();
                    for (line, (s, e)) in (start_line..=end_line).zip(vec.into_iter()) {
                        // print header "xxx | ", where xxx is the line number
                        output.push_str(&format!("{:head_width$} | {}\n", line + 1, text[line]));
                        output.push_str(&format!("{:head_width$} | ", ' '));
                        for _ in 0..s {
                            output.push(' ');
                        }
                        if line == start_line {
                            output.push('^');
                            for _ in s + 1..e {
                                output.push('~');
                            }
                        } else if line == end_line {
                            for _ in s..e {
                                output.push('~');
                            }
                            output.push('^');
                        } else {
                            for _ in s..e {
                                output.push('~');
                            }
                        }
                        output.push('\n');
                    }
                    output.push_str(&descr.message);
                    output.push('\n');
                }
                None => {
                    output.push_str(&descr.message);
                    output.push('\n');
                }
            }
        }
        output
    }
}

#[test]
fn diagnostic_test() {
    let source = r#"1234567890
1234567890
1234567890
"#;

    let span = Span { start: 6, end: 25 };
    let diag = Diagnostic::error("Error Name")
        .line("some error description")
        .line_span(span, "some spanned error description")
        .line_verb(100, "this should not appear!");

    assert_eq!(
        diag.minimal_report(source, 30),
        r#"[Error]: Error Name
some error description
line 1, col 7 - line 3, col 4:
some spanned error description
"#
    );
    assert_eq!(
        diag.report(source, 30),
        r#"[Error]: Error Name
some error description
1 | 1234567890
  |       ^~~~
2 | 1234567890
  | ~~~~~~~~~~
3 | 1234567890
  | ~~~^
some spanned error description
"#
    );
}
