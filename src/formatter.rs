/// Formatter for Unison source code.
///
/// Operates on the flat token stream and applies formatting rules:
/// 1. Normalize indentation to configurable indent width
/// 2. Collapse multiple spaces to single space (within lines, not indentation)
/// 3. Strip trailing whitespace
/// 4. Break long lines at `|>`, `++` and other operators

use std::collections::HashMap;

use crate::lexer::{Token, TokenKind};

/// Configuration for the formatter.
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Width of a single indentation level in spaces.
    pub indent_width: usize,
    /// Maximum line width before breaking.
    pub max_line_width: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        FormatConfig {
            indent_width: 4,
            max_line_width: 120,
        }
    }
}

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

/// Format a token stream into a formatted string using default configuration.
pub fn format(tokens: &[Token]) -> String {
    format_with_config(tokens, &FormatConfig::default())
}

/// Format a token stream into a formatted string using the given configuration.
pub fn format_with_config(tokens: &[Token], config: &FormatConfig) -> String {
    let lines = split_into_lines(tokens);
    let formatted_lines = normalize_lines(lines, config);
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

/// Compute normalized indent levels from original indentation.
///
/// Two-pass approach:
/// 1. Walk lines with a stack to discover parent-child relationships between
///    indent levels (each indent's parent is the deepest stack entry less than it).
/// 2. Build a tree of indent levels, sort children by indent value, and assign
///    normalized levels via DFS. This ensures that distinct indents under the
///    same parent get distinct normalized levels, ordered by their original value.
fn compute_indent_map(lines: &[Line], indent_width: usize) -> HashMap<usize, usize> {
    // Collect distinct indents in order of appearance, tracking parent for each.
    // parent_of[indent] = the indent of the nearest enclosing scope.
    // first_seen[indent] = index of the first line where this indent appears.
    let mut parent_of: HashMap<usize, usize> = HashMap::new();
    let mut first_seen: HashMap<usize, usize> = HashMap::new();
    let mut stack: Vec<usize> = vec![0]; // stack of original indents

    first_seen.insert(0, 0);

    for (line_idx, line) in lines.iter().enumerate() {
        if line.tokens.is_empty() {
            continue;
        }
        let orig = line.original_indent;

        // Pop stack entries deeper than or equal to orig (except the base)
        while stack.len() > 1 && *stack.last().unwrap() >= orig {
            stack.pop();
        }

        if !first_seen.contains_key(&orig) {
            // Parent is current top of stack
            parent_of.insert(orig, *stack.last().unwrap());
            first_seen.insert(orig, line_idx);
        }

        // Push orig onto stack if it's deeper than top
        if orig > *stack.last().unwrap() {
            stack.push(orig);
        }
    }

    // Build children map: parent -> list of children sorted by first appearance.
    let mut children: HashMap<usize, Vec<usize>> = HashMap::new();
    for (&child, &parent) in &parent_of {
        children.entry(parent).or_default().push(child);
    }
    for v in children.values_mut() {
        v.sort_by_key(|&indent| first_seen.get(&indent).copied().unwrap_or(0));
    }

    // Each distinct child indent gets its own normalized level.
    let mut child_groups: HashMap<usize, Vec<Vec<usize>>> = HashMap::new();
    for (&parent, kids) in &children {
        let groups: Vec<Vec<usize>> = kids.iter().map(|&kid| vec![kid]).collect();
        child_groups.insert(parent, groups);
    }

    // DFS to assign levels. Each group of children shares a level.
    let mut map: HashMap<usize, usize> = HashMap::new();
    // (indent_or_group, level)
    let mut dfs_stack: Vec<(Vec<usize>, usize)> = vec![(vec![0], 0)];
    while let Some((indents, level)) = dfs_stack.pop() {
        for &indent in &indents {
            map.insert(indent, level * indent_width);
        }
        // Collect child groups from all indents in this group
        let mut all_groups: Vec<Vec<usize>> = Vec::new();
        for &indent in &indents {
            if let Some(groups) = child_groups.get(&indent) {
                all_groups.extend(groups.iter().cloned());
            }
        }
        // Sort by first appearance and push in reverse for DFS order
        all_groups.sort_by_key(|g| first_seen.get(&g[0]).copied().unwrap_or(0));
        for (i, group) in all_groups.iter().rev().enumerate() {
            dfs_stack.push((group.clone(), level + all_groups.len() - i));
        }
    }

    map
}

/// Normalize lines: fix indentation, collapse internal whitespace, break long lines,
/// then align consecutive `|>` lines.
fn normalize_lines(lines: Vec<Line>, config: &FormatConfig) -> Vec<Line> {
    let mut result = Vec::new();
    let indent_map = compute_indent_map(&lines, config.indent_width);

    for line in lines.into_iter() {
        let new_indent = if line.tokens.is_empty() {
            0
        } else {
            *indent_map.get(&line.original_indent).unwrap_or(&0)
        };

        // Collapse multiple whitespace tokens within the line to single spaces
        let tokens = collapse_whitespace(&line.tokens);

        // Check if the line is too long and needs breaking
        let line_len = new_indent + tokens_display_width(&tokens);

        if line_len > config.max_line_width {
            // Try to break the line, then recursively break any sub-lines still too long
            let broken = break_long_line(&tokens, new_indent, config);
            let mut sub_lines: Vec<(usize, Vec<Token>)> = Vec::new();
            for (i, sub_tokens) in broken.iter().enumerate() {
                let indent = if i == 0 {
                    new_indent
                } else {
                    new_indent + config.indent_width
                };
                let sub_len = indent + tokens_display_width(sub_tokens);
                if sub_len > config.max_line_width && broken.len() > 1 {
                    // Recursively break this sub-line
                    let sub_broken = break_long_line(sub_tokens, indent, config);
                    for (j, sub_sub) in sub_broken.iter().enumerate() {
                        let sub_indent = if j == 0 {
                            indent
                        } else {
                            indent + config.indent_width
                        };
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

    let result = align_operators(result, config.indent_width);
    fix_block_opener_indent(result, config.indent_width)
}

/// Align consecutive lines that start with the same operator to the same indentation level.
///
/// Only applies when there are 2+ consecutive lines starting with the same operator
/// (e.g., lines produced by line breaking). A single operator line keeps its indent
/// from the indent map to preserve Unison block structure.
///
/// The target indent is the preceding non-operator line's indent + one level.
fn align_operators(mut lines: Vec<Line>, indent_width: usize) -> Vec<Line> {
    let mut i = 0;
    while i < lines.len() {
        if let Some(op) = line_leading_operator(&lines[i]) {
            let mut j = i + 1;
            while j < lines.len() && line_leading_operator(&lines[j]).as_deref() == Some(&*op) {
                j += 1;
            }
            // Only align when there are 2+ consecutive operator lines
            if j - i >= 2 {
                let base_indent = (0..i)
                    .rev()
                    .find(|&k| {
                        !lines[k].tokens.is_empty()
                            && line_leading_operator(&lines[k]).is_none()
                    })
                    .map(|k| lines[k].original_indent)
                    .unwrap_or(0);
                let target_indent = base_indent + indent_width;
                for k in i..j {
                    lines[k].original_indent = target_indent;
                }
            }
            i = j;
        } else {
            i += 1;
        }
    }
    lines
}

/// Ensure that lines following a block-opening keyword (`do`, `cases`, `->`, etc.)
/// or a trailing operator (`|>`, `++`) are indented deeper than the opener.
/// Unison requires this for correct parsing.
fn fix_block_opener_indent(mut lines: Vec<Line>, indent_width: usize) -> Vec<Line> {
    for i in 0..lines.len().saturating_sub(1) {
        if lines[i].tokens.is_empty() {
            continue;
        }
        let is_block_opener = line_ends_with_block_opener(&lines[i]);
        let is_trailing_op = line_ends_with_breakable_operator(&lines[i]);
        if !is_block_opener && !is_trailing_op {
            continue;
        }
        let opener_indent = lines[i].original_indent;
        // Find the next non-empty line
        if let Some(j) = (i + 1..lines.len()).find(|&k| !lines[k].tokens.is_empty()) {
            let min_indent = if is_trailing_op {
                // Continuation must be past the RHS of `=` on this line.
                // e.g. `name = queryParameter "name" |>` -> continuation
                // must be at column(queryParameter) + indent_width.
                rhs_column(&lines[i]) + indent_width
            } else {
                opener_indent + indent_width
            };
            if lines[j].original_indent < min_indent {
                lines[j].original_indent = min_indent;
            }
        }
    }
    lines
}

/// Find the column of the first token after `=` on a line.
/// If no `=` is found, falls back to line indent + indent_width.
fn rhs_column(line: &Line) -> usize {
    let mut col = line.original_indent;
    let mut found_eq = false;
    for tok in &line.tokens {
        if found_eq {
            // Skip whitespace after `=`
            if tok.kind == TokenKind::Whitespace {
                col += tok.text.len();
                continue;
            }
            // This is the first real token after `=`
            return col;
        }
        if tok.kind == TokenKind::Eq {
            found_eq = true;
            col += tok.text.len();
            continue;
        }
        col += tok.text.len();
    }
    // No `=` found — fall back to line indent + one level
    line.original_indent
}

/// Check if a line ends with a token that opens a block in Unison.
fn line_ends_with_block_opener(line: &Line) -> bool {
    line.tokens.last().is_some_and(|t| {
        matches!(
            t.kind,
            TokenKind::KwDo
                | TokenKind::KwCases
                | TokenKind::KwWhere
                | TokenKind::KwWith
                | TokenKind::KwLet
                | TokenKind::Arrow
        )
    })
}

/// Check if a line ends with a breakable operator (`|>`, `++`).
fn line_ends_with_breakable_operator(line: &Line) -> bool {
    line.tokens.last().is_some_and(|t| is_breakable_operator(t))
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
fn break_long_line(
    tokens: &[Token],
    base_indent: usize,
    config: &FormatConfig,
) -> Vec<Vec<Token>> {
    // Strategy: try to break at |> and ++ operators
    let positions = find_break_positions(tokens);

    if !positions.is_empty() {
        return break_at_positions(tokens, &positions, base_indent, config);
    }

    // If no good break points, return as-is
    vec![tokens.to_vec()]
}

/// Find positions where we can break the line (before |> and ++ operators).
/// First tries top-level only; if none found, allows breaking inside parens.
fn find_break_positions(tokens: &[Token]) -> Vec<usize> {
    // First pass: only top-level operators
    let positions = find_operator_positions(tokens, false);
    if !positions.is_empty() {
        return positions;
    }
    // Second pass: allow operators at any nesting depth
    find_operator_positions(tokens, true)
}

/// Whether this token is a breakable operator (|> or ++).
fn is_breakable_operator(tok: &Token) -> bool {
    tok.kind == TokenKind::SymbolyId && (tok.text == "|>" || tok.text == "++")
}

fn find_operator_positions(tokens: &[Token], allow_nested: bool) -> Vec<usize> {
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

        // Break before |> and ++
        if is_breakable_operator(tok) && i > 0 {
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
    config: &FormatConfig,
) -> Vec<Vec<Token>> {
    let mut result = Vec::new();
    let mut start = 0;
    let continuation_indent = base_indent + config.indent_width;

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
            if result.is_empty() || width <= config.max_line_width {
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
        let input = "foo =\n    bar = 1\n    baz = 2\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n    bar = 1\n    baz = 2\n");
    }

    #[test]
    fn test_normalize_2space_to_4space() {
        let input = "foo =\n  bar = 1\n  baz = 2\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n    bar = 1\n    baz = 2\n");
    }

    #[test]
    fn test_normalize_nested_indentation() {
        let input = "foo =\n    bar =\n        baz = 1\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n    bar =\n        baz = 1\n");
    }

    #[test]
    fn test_break_long_pipe_line() {
        let input = "x = foo |> bar |> baz |> quux |> something |> another |> more |> evenMore |> reallyLongFunction |> anotherOne |> yetAnother\n";
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
                    line.starts_with("    "),
                    "Continuation line should be indented: {line:?}"
                );
            }
        }
    }

    #[test]
    fn test_break_long_concat_line() {
        let input = "x = foo ++ bar ++ baz ++ quux ++ something ++ another ++ more ++ evenMore ++ reallyLongFunction ++ anotherOne ++ yetAnother\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        // Should be broken across multiple lines
        assert!(
            result.lines().count() > 1,
            "Long ++ line should be broken:\n{result}"
        );
        // Each continuation should be indented
        for (i, line) in result.lines().enumerate() {
            if i > 0 {
                assert!(
                    line.starts_with("    "),
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

    #[test]
    fn test_distinct_indent_levels_preserved() {
        // Line 1 ends with `do`, so line 2 (the do body) must be deeper than line 1.
        // Original indents: 8, 16, 12 -> normalized must have:
        //   indent0 (name) < indent1 (|> ... do) < indent2 (raiseBadRequest)
        let input = "        name = queryParameter \"name\"\n                |> List.head |> Optional.getOrElse' do\n            raiseBadRequest \"missing\" ()\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines.len(), 3);
        let indent0 = lines[0].len() - lines[0].trim_start().len();
        let indent1 = lines[1].len() - lines[1].trim_start().len();
        let indent2 = lines[2].len() - lines[2].trim_start().len();
        assert!(
            indent0 < indent1,
            "name line ({indent0}) must be less indented than |> line ({indent1})"
        );
        assert!(
            indent1 < indent2,
            "|> line ({indent1}) must be less indented than raiseBadRequest line ({indent2})"
        );
    }

    #[test]
    fn test_multiple_distinct_indent_levels() {
        // 4 distinct levels: 0, 2, 4, 6 -> should become 0, 4, 8, 12
        let input = "foo =\n  bar =\n    baz =\n      qux = 1\n";
        let tokens = lexer::lex(input);
        let result = format(&tokens);
        assert_eq!(result, "foo =\n    bar =\n        baz =\n            qux = 1\n");
    }

    #[test]
    fn test_custom_indent_width() {
        let input = "foo =\n  bar = 1\n  baz = 2\n";
        let tokens = lexer::lex(input);
        let config = FormatConfig {
            indent_width: 2,
            ..FormatConfig::default()
        };
        let result = format_with_config(&tokens, &config);
        assert_eq!(result, "foo =\n  bar = 1\n  baz = 2\n");
    }

    #[test]
    fn test_custom_max_line_width() {
        let input = "x = foo |> bar |> baz |> quux\n";
        let tokens = lexer::lex(input);
        // With a narrow max width, should break
        let config = FormatConfig {
            max_line_width: 20,
            ..FormatConfig::default()
        };
        let result = format_with_config(&tokens, &config);
        assert!(
            result.lines().count() > 1,
            "Should break at narrow width:\n{result}"
        );
        // With a wide max width, should not break
        let config_wide = FormatConfig {
            max_line_width: 200,
            ..FormatConfig::default()
        };
        let result_wide = format_with_config(&tokens, &config_wide);
        assert_eq!(result_wide, "x = foo |> bar |> baz |> quux\n");
    }

    #[test]
    fn test_do_block_body_deeper_than_do_line() {
        // The `do` body must be indented deeper than the line ending with `do`
        let input = "        name = queryParameter \"name\" |>\n                List.head |> Optional.getOrElse' do\n            raiseBadRequest \"missing\" ()\n";
        let tokens = lexer::lex(input);
        let config = FormatConfig {
            indent_width: 2,
            ..FormatConfig::default()
        };
        let result = format_with_config(&tokens, &config);
        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines.len(), 3);
        let indent_do_line = lines[1].len() - lines[1].trim_start().len();
        let indent_body = lines[2].len() - lines[2].trim_start().len();
        assert!(
            indent_body > indent_do_line,
            "do body ({indent_body}) must be deeper than do line ({indent_do_line})"
        );
    }

    #[test]
    fn test_continuation_deeper_than_parent() {
        // A multi-line expression continuation must be deeper than the line it continues
        let input = "        name = queryParameter \"name\" |>\n                List.head |> Optional.getOrElse' do\n            raiseBadRequest \"missing\" ()\n";
        let tokens = lexer::lex(input);
        let config = FormatConfig {
            indent_width: 2,
            ..FormatConfig::default()
        };
        let result = format_with_config(&tokens, &config);
        let lines: Vec<&str> = result.lines().collect();
        let indent_name = lines[0].len() - lines[0].trim_start().len();
        let indent_list = lines[1].len() - lines[1].trim_start().len();
        assert!(
            indent_list > indent_name,
            "continuation ({indent_list}) must be deeper than parent ({indent_name})"
        );
    }

    #[test]
    fn test_idempotent_with_indent_2() {
        let input = "        name = queryParameter \"name\" |>\n                List.head |> Optional.getOrElse' do\n            raiseBadRequest \"missing\" ()\n";
        let tokens = lexer::lex(input);
        let config = FormatConfig {
            indent_width: 2,
            ..FormatConfig::default()
        };
        let pass1 = format_with_config(&tokens, &config);
        let tokens2 = lexer::lex(&pass1);
        let pass2 = format_with_config(&tokens2, &config);
        assert_eq!(pass1, pass2, "Formatting must be idempotent");
    }

    #[test]
    fn test_handler_routes_indent_2() {
        let input = "\nhandlerRoutes =\n    do\n        use Parser /\n        name = queryParameter \"name\" |>\n                List.head |> Optional.getOrElse' do\n            raiseBadRequest \"missing name param\" ()\n\n        ok.json (Hello.make\n            |> Hello.name.set (Some name)\n            |> age.set (Some +42)\n            |> encoder)\n";
        let tokens = lexer::lex(input);
        let config = FormatConfig {
            indent_width: 2,
            max_line_width: 80,
        };
        let result = format_with_config(&tokens, &config);
        // Verify structure: do body deeper than do line, continuation deeper than parent
        let content_lines: Vec<&str> = result.lines().filter(|l| !l.trim().is_empty()).collect();
        for (i, line) in content_lines.iter().enumerate() {
            if line.trim_end().ends_with(" do") {
                // Next content line must be deeper
                if let Some(next) = content_lines.get(i + 1) {
                    let do_indent = line.len() - line.trim_start().len();
                    let body_indent = next.len() - next.trim_start().len();
                    assert!(
                        body_indent > do_indent,
                        "Line after 'do' must be deeper: do@{do_indent} vs body@{body_indent}\ndo line: {line}\nbody line: {next}"
                    );
                }
            }
        }
        // Verify idempotent
        let tokens2 = lexer::lex(&result);
        let pass2 = format_with_config(&tokens2, &config);
        assert_eq!(result, pass2, "Formatting must be idempotent");
    }
}
