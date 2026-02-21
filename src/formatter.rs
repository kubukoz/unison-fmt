/// Formatter for Unison source code.
///
/// Operates on the flat token stream and applies formatting rules:
/// 1. Normalize indentation to 2-space levels
/// 2. Collapse multiple spaces to single space (within lines, not indentation)
/// 3. Strip trailing whitespace
/// 4. Break long lines at `|>` and other operators

use crate::lexer::{Token, TokenKind};

const MAX_LINE_WIDTH: usize = 100;
const INDENT_WIDTH: usize = 2;

/// A line is a sequence of tokens between newlines.
struct Line {
    /// Leading indentation (number of spaces in the original)
    original_indent: usize,
    /// Tokens on this line (excluding leading whitespace and trailing newline)
    tokens: Vec<Token>,
    /// Whether the line ends with a newline
    has_newline: bool,
    /// The original newline token text (to preserve \r\n vs \n)
    newline_text: String,
}

/// Format a token stream into a formatted string.
pub fn format(tokens: &[Token]) -> String {
    let lines = split_into_lines(tokens);
    let indent_unit = detect_indent_unit(&lines);
    let formatted_lines = normalize_lines(lines, indent_unit);
    render(formatted_lines)
}

/// Split the token stream into lines.
fn split_into_lines(tokens: &[Token]) -> Vec<Line> {
    let mut lines = Vec::new();
    let mut current_tokens: Vec<Token> = Vec::new();
    let mut leading_ws: Option<String> = None;
    let mut at_line_start = true;

    for tok in tokens {
        match tok.kind {
            TokenKind::Eof => break,
            TokenKind::Newline => {
                let indent = leading_ws.as_ref().map_or(0, |s| count_spaces(s));
                lines.push(Line {
                    original_indent: indent,
                    tokens: std::mem::take(&mut current_tokens),
                    has_newline: true,
                    newline_text: tok.text.clone(),
                });
                leading_ws = None;
                at_line_start = true;
            }
            TokenKind::Whitespace if at_line_start => {
                leading_ws = Some(tok.text.clone());
            }
            _ => {
                if at_line_start {
                    at_line_start = false;
                }
                current_tokens.push(tok.clone());
            }
        }
    }

    // Handle last line without newline
    if !current_tokens.is_empty() {
        let indent = leading_ws.as_ref().map_or(0, |s| count_spaces(s));
        lines.push(Line {
            original_indent: indent,
            tokens: current_tokens,
            has_newline: false,
            newline_text: String::new(),
        });
    } else if leading_ws.is_some() {
        // Trailing whitespace-only line without newline
        let indent = leading_ws.as_ref().map_or(0, |s| count_spaces(s));
        lines.push(Line {
            original_indent: indent,
            tokens: Vec::new(),
            has_newline: false,
            newline_text: String::new(),
        });
    }

    lines
}

/// Count spaces (treating tabs as 8 spaces).
fn count_spaces(s: &str) -> usize {
    s.chars()
        .map(|c| if c == '\t' { 8 } else { 1 })
        .sum()
}

/// Detect the indentation unit used in the file (e.g., 2, 4, 8 spaces).
fn detect_indent_unit(lines: &[Line]) -> usize {
    let mut indents: Vec<usize> = lines
        .iter()
        .filter(|l| !l.tokens.is_empty() && l.original_indent > 0)
        .map(|l| l.original_indent)
        .collect();
    indents.sort();
    indents.dedup();

    if indents.is_empty() {
        return INDENT_WIDTH;
    }

    // Find the GCD of all indent differences
    // First, compute differences between consecutive indent levels and the base indent
    let mut diffs: Vec<usize> = Vec::new();
    for &indent in &indents {
        diffs.push(indent);
    }
    for w in indents.windows(2) {
        let diff = w[1] - w[0];
        if diff > 0 {
            diffs.push(diff);
        }
    }

    let g = diffs.into_iter().reduce(gcd).unwrap_or(INDENT_WIDTH);

    if g == 0 {
        INDENT_WIDTH
    } else {
        g
    }
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

/// Normalize lines: fix indentation, collapse internal whitespace, break long lines,
/// then align consecutive `|>` lines.
fn normalize_lines(lines: Vec<Line>, indent_unit: usize) -> Vec<Line> {
    let mut result = Vec::new();

    for line in lines {
        // Normalize indentation: convert from original indent unit to INDENT_WIDTH
        let indent_level = if indent_unit > 0 && line.original_indent > 0 {
            line.original_indent / indent_unit
        } else {
            0
        };
        let new_indent = indent_level * INDENT_WIDTH;

        // Collapse multiple whitespace tokens within the line to single spaces
        let tokens = collapse_whitespace(&line.tokens);

        // Check if the line is too long and needs breaking
        let line_len = new_indent + tokens_display_width(&tokens);

        if line_len > MAX_LINE_WIDTH {
            // Try to break the line, then recursively break any sub-lines still too long
            let broken = break_long_line(&tokens, new_indent);
            let mut sub_lines: Vec<(usize, Vec<Token>)> = Vec::new();
            for (i, sub_tokens) in broken.iter().enumerate() {
                let indent = if i == 0 { new_indent } else { new_indent + INDENT_WIDTH };
                let sub_len = indent + tokens_display_width(sub_tokens);
                if sub_len > MAX_LINE_WIDTH && broken.len() > 1 {
                    // Recursively break this sub-line
                    let sub_broken = break_long_line(sub_tokens, indent);
                    for (j, sub_sub) in sub_broken.iter().enumerate() {
                        let sub_indent = if j == 0 { indent } else { indent + INDENT_WIDTH };
                        sub_lines.push((sub_indent, sub_sub.clone()));
                    }
                } else {
                    sub_lines.push((indent, sub_tokens.clone()));
                }
            }
            for (i, (indent, sub_tokens)) in sub_lines.iter().enumerate() {
                result.push(Line {
                    original_indent: *indent,
                    tokens: sub_tokens.clone(),
                    has_newline: if i < sub_lines.len() - 1 {
                        true
                    } else {
                        line.has_newline
                    },
                    newline_text: line.newline_text.clone(),
                });
            }
        } else {
            result.push(Line {
                original_indent: new_indent,
                tokens,
                has_newline: line.has_newline,
                newline_text: line.newline_text,
            });
        }
    }

    align_operators(result)
}

/// Align consecutive lines that start with the same operator to the same indentation level.
///
/// When we see a group of consecutive lines each starting with the same symboly operator,
/// we set them all to the indent of the first such line in that group.
fn align_operators(mut lines: Vec<Line>) -> Vec<Line> {
    let mut i = 0;
    while i < lines.len() {
        if let Some(op) = line_leading_operator(&lines[i]) {
            let target_indent = lines[i].original_indent;
            let mut j = i + 1;
            while j < lines.len() && line_leading_operator(&lines[j]).as_deref() == Some(&*op) {
                lines[j].original_indent = target_indent;
                j += 1;
            }
            i = j;
        } else {
            i += 1;
        }
    }
    lines
}

/// If the line starts with a symboly operator, return it.
fn line_leading_operator(line: &Line) -> Option<String> {
    line.tokens.first().and_then(|t| {
        if t.kind == TokenKind::SymbolyId {
            Some(t.text.clone())
        } else {
            None
        }
    })
}

/// Collapse multiple adjacent whitespace tokens into single spaces,
/// and remove trailing whitespace.
fn collapse_whitespace(tokens: &[Token]) -> Vec<Token> {
    let mut result = Vec::new();

    for tok in tokens {
        if tok.kind == TokenKind::Whitespace {
            // Replace any whitespace with a single space, but skip if at end or start
            if !result.is_empty() {
                result.push(Token {
                    kind: TokenKind::Whitespace,
                    text: " ".to_string(),
                    offset: tok.offset,
                });
            }
        } else {
            result.push(tok.clone());
        }
    }

    // Remove trailing whitespace
    while result.last().is_some_and(|t| t.kind == TokenKind::Whitespace) {
        result.pop();
    }

    result
}

/// Calculate the display width of a sequence of tokens.
fn tokens_display_width(tokens: &[Token]) -> usize {
    tokens.iter().map(|t| t.text.len()).sum()
}

/// Break a long line at sensible points.
/// Returns a list of sub-lines (each a Vec<Token>).
fn break_long_line(tokens: &[Token], base_indent: usize) -> Vec<Vec<Token>> {
    // Strategy: try to break at |> operators first, then at other infix operators
    let pipe_positions = find_break_positions(tokens);

    if !pipe_positions.is_empty() {
        return break_at_positions(tokens, &pipe_positions, base_indent);
    }

    // If no good break points, return as-is
    vec![tokens.to_vec()]
}

/// Find positions where we can break the line (before |> operators).
/// First tries top-level only; if none found, allows breaking inside parens.
fn find_break_positions(tokens: &[Token]) -> Vec<usize> {
    // First pass: only top-level |>
    let positions = find_pipe_positions(tokens, false);
    if !positions.is_empty() {
        return positions;
    }
    // Second pass: allow |> at any nesting depth
    find_pipe_positions(tokens, true)
}

fn find_pipe_positions(tokens: &[Token], allow_nested: bool) -> Vec<usize> {
    let mut positions = Vec::new();
    let mut paren_depth = 0u32;

    for (i, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => paren_depth += 1,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                paren_depth = paren_depth.saturating_sub(1)
            }
            _ => {}
        }

        if !allow_nested && paren_depth > 0 {
            continue;
        }

        // Break before |>
        if tok.kind == TokenKind::SymbolyId && tok.text == "|>" && i > 0 {
            positions.push(i);
        }
    }

    positions
}

/// Break a token sequence at the given positions.
/// Each break creates a new line. The break token starts the new line.
fn break_at_positions(
    tokens: &[Token],
    positions: &[usize],
    base_indent: usize,
) -> Vec<Vec<Token>> {
    let mut result = Vec::new();
    let mut start = 0;
    let continuation_indent = base_indent + INDENT_WIDTH;

    for &pos in positions {
        // Take tokens from start to pos (exclusive)
        // Skip trailing whitespace on the chunk
        let mut chunk: Vec<Token> = tokens[start..pos].to_vec();
        while chunk.last().is_some_and(|t| t.kind == TokenKind::Whitespace) {
            chunk.pop();
        }

        // Check if this chunk alone fits; if not, and we haven't accumulated much, keep going
        if !chunk.is_empty() {
            let indent = if result.is_empty() {
                base_indent
            } else {
                continuation_indent
            };
            let width = indent + tokens_display_width(&chunk);

            // If first chunk is short enough, emit it
            if result.is_empty() || width <= MAX_LINE_WIDTH {
                result.push(chunk);
            } else {
                // Merge with previous chunk if possible
                if let Some(last) = result.last_mut() {
                    last.push(Token {
                        kind: TokenKind::Whitespace,
                        text: " ".to_string(),
                        offset: 0,
                    });
                    last.extend(chunk);
                } else {
                    result.push(chunk);
                }
            }
        }
        start = pos;
    }

    // Remaining tokens after last break
    if start < tokens.len() {
        let mut chunk: Vec<Token> = tokens[start..].to_vec();
        // Skip leading whitespace on continuation
        while chunk.first().is_some_and(|t| t.kind == TokenKind::Whitespace) {
            chunk.remove(0);
        }
        // But we want the |> to start the line, so put it back
        // Actually, start already points at the |>, so chunk starts with |>
        // Just remove any whitespace after |> and re-add a single space
        if !chunk.is_empty() {
            result.push(chunk);
        }
    }

    // If breaking didn't help (only 1 chunk), return as-is
    if result.len() <= 1 {
        return vec![tokens.to_vec()];
    }

    // Now post-process: ensure continuation lines start without leading whitespace
    // (the renderer will add indentation)
    for chunk in &mut result {
        while chunk.first().is_some_and(|t| t.kind == TokenKind::Whitespace) {
            chunk.remove(0);
        }
        while chunk.last().is_some_and(|t| t.kind == TokenKind::Whitespace) {
            chunk.pop();
        }
    }

    // Remove any empty chunks
    result.retain(|c| !c.is_empty());

    result
}

/// Render formatted lines into a string.
fn render(lines: Vec<Line>) -> String {
    let mut out = String::new();

    for line in &lines {
        if !line.tokens.is_empty() {
            // Emit indentation
            for _ in 0..line.original_indent {
                out.push(' ');
            }
            // Emit tokens
            for tok in &line.tokens {
                out.push_str(&tok.text);
            }
        }
        // Emit newline (no trailing whitespace on blank lines)
        if line.has_newline {
            if line.newline_text.is_empty() {
                out.push('\n');
            } else {
                out.push_str(&line.newline_text);
            }
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_strip_trailing_whitespace() {
        let input = "foo = 42   \nbar = 1\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo = 42\nbar = 1\n");
    }

    #[test]
    fn test_collapse_multiple_spaces() {
        let input = "foo =     42\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo = 42\n");
    }

    #[test]
    fn test_preserve_indentation_structure() {
        let input = "foo =\n  bar = 1\n  baz = 2\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n  bar = 1\n  baz = 2\n");
    }

    #[test]
    fn test_normalize_4space_to_2space() {
        let input = "foo =\n    bar = 1\n    baz = 2\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n  bar = 1\n  baz = 2\n");
    }

    #[test]
    fn test_normalize_nested_indentation() {
        let input = "foo =\n    bar =\n        baz = 1\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n  bar =\n    baz = 1\n");
    }

    #[test]
    fn test_break_long_pipe_line() {
        let input = "x = foo |> bar |> baz |> quux |> something |> another |> more |> evenMore |> reallyLongFunction |> anotherOne\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        // Should be broken across multiple lines
        assert!(
            result.lines().count() > 1,
            "Long line should be broken:\n{result}"
        );
        // Each continuation should be indented
        for (i, line) in result.lines().enumerate() {
            if i > 0 {
                assert!(
                    line.starts_with("  "),
                    "Continuation line should be indented: {line:?}"
                );
            }
        }
    }

    #[test]
    fn test_preserve_comments() {
        let input = "foo = 42 -- a comment\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo = 42 -- a comment\n");
    }

    #[test]
    fn test_blank_lines_no_trailing_ws() {
        let input = "foo = 1\n   \nbar = 2\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo = 1\n\nbar = 2\n");
    }

    #[test]
    fn test_preserve_empty_file() {
        let input = "";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "");
    }

    #[test]
    fn test_preserve_single_newline() {
        let input = "\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "\n");
    }
}
