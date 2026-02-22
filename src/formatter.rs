/// Formatter for Unison source code.
///
/// Operates on the flat token stream and applies formatting rules:
/// 1. Normalize indentation to 2-space levels
/// 2. Collapse multiple spaces to single space (within lines, not indentation)
/// 3. Strip trailing whitespace
/// 4. Break long lines at `|>` and other operators

use crate::lexer::{Token, TokenKind};

/// Configuration for the formatter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormatterConfig {
    /// Maximum line width before breaking long lines.
    pub max_line_width: usize,
    /// Number of spaces per indentation level.
    pub indent_width: usize,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            max_line_width: 100,
            indent_width: 4,
        }
    }
}

/// A line is a sequence of tokens between newlines.
#[derive(Clone)]
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
    format_with_config(tokens, &FormatterConfig::default())
}

/// Format a token stream into a formatted string with custom configuration.
pub fn format_with_config(tokens: &[Token], config: &FormatterConfig) -> String {
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
/// Tracks a stack of seen indentation depths. Each new deeper indentation
/// increases the level by 1, regardless of how many spaces it jumped.
/// Returning to a shallower indentation pops back to that level.
fn compute_indent_levels(lines: &[Line]) -> Vec<usize> {
    let mut levels = Vec::with_capacity(lines.len());
    // Stack of (original_indent, level) pairs
    let mut stack: Vec<(usize, usize)> = vec![(0, 0)];

    for line in lines {
        // Skip blank lines — they get level 0 (doesn't matter, no tokens)
        if line.tokens.is_empty() {
            levels.push(0);
            continue;
        }

        let orig = line.original_indent;

        // Pop stack entries that are deeper than current indent
        while stack.len() > 1 && stack.last().unwrap().0 > orig {
            stack.pop();
        }

        let &(top_indent, top_level) = stack.last().unwrap();

        if orig == top_indent {
            levels.push(top_level);
        } else if orig > top_indent {
            let new_level = top_level + 1;
            stack.push((orig, new_level));
            levels.push(new_level);
        } else {
            // orig < top_indent but not found in stack — use closest
            levels.push(top_level);
        }
    }

    levels
}

/// Try to join multi-line expressions onto a single line when they fit.
///
/// Scans for groups of continuation lines (deeper indent than the base line)
/// and attempts to join them if the result fits within max_line_width.
/// Uses the pre-computed normalized indent levels to accurately check width.
fn try_join_lines(
    lines: Vec<Line>,
    indent_levels: &[usize],
    config: &FormatterConfig,
) -> Vec<Line> {
    let mut result = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        // Skip empty lines
        if lines[i].tokens.is_empty() {
            result.push(lines[i].clone());
            i += 1;
            continue;
        }

        let base_indent = lines[i].original_indent;
        let normalized_indent = indent_levels[i] * config.indent_width;

        // Find continuation lines (deeper indent)
        let mut j = i + 1;
        while j < lines.len()
            && !lines[j].tokens.is_empty()
            && lines[j].original_indent > base_indent
        {
            j += 1;
        }

        if j > i + 1 {
            // Try to join lines[i..j]
            if let Some(joined) = try_join_group(&lines[i..j], normalized_indent, config) {
                result.push(joined);
                i = j;
                continue;
            }
        }

        result.push(lines[i].clone());
        i += 1;
    }
    result
}

/// Check if a token indicates the start of a block structure (shouldn't be joined).
fn is_block_introducing_token(tok: &Token) -> bool {
    match tok.kind {
        // Specific punctuation tokens
        TokenKind::Eq | TokenKind::Arrow | TokenKind::Colon | TokenKind::Pipe => true,
        // Keywords that introduce blocks
        TokenKind::WordyId => {
            matches!(
                tok.text.as_str(),
                "where" | "with" | "cases" | "match" | "if" | "then" | "else" | "let" | "do"
            )
        }
        _ => false,
    }
}

/// Try to join a group of lines into a single line if they fit within max_line_width.
///
/// The group must start with a base line and have continuation lines with deeper indentation.
/// Will not join if the base line ends with block-introducing tokens like `=`, `->`, etc.
fn try_join_group(lines: &[Line], base_indent: usize, config: &FormatterConfig) -> Option<Line> {
    // Don't join if the base line ends with a block-introducing token
    // (like `=`, `->`, `:`, etc.) because that indicates the continuation
    // is a block body, not a continuation of an expression
    if let Some(last_tok) = lines[0]
        .tokens
        .iter()
        .rev()
        .find(|t| t.kind != TokenKind::Whitespace)
    {
        if is_block_introducing_token(last_tok) {
            return None;
        }
    }

    // Collect all tokens
    let mut joined_tokens = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        if idx > 0 && !joined_tokens.is_empty() {
            // Add single space between lines
            joined_tokens.push(Token {
                kind: TokenKind::Whitespace,
                text: " ".to_string(),
                offset: 0,
            });
        }
        joined_tokens.extend(line.tokens.iter().cloned());
    }

    // Collapse internal whitespace
    let joined_tokens = collapse_whitespace(&joined_tokens);

    // Check if it fits within max width
    let width = base_indent + tokens_display_width(&joined_tokens);
    if width > config.max_line_width {
        return None;
    }

    Some(Line {
        original_indent: base_indent,
        tokens: joined_tokens,
        has_newline: lines.last().unwrap().has_newline,
        newline_text: lines.last().unwrap().newline_text.clone(),
    })
}

/// Normalize lines: fix indentation, collapse internal whitespace, break long lines,
/// then align consecutive `|>` lines.
fn normalize_lines(lines: Vec<Line>, config: &FormatterConfig) -> Vec<Line> {
    // Compute indent levels first so we can use normalized indents for width checks
    let indent_levels = compute_indent_levels(&lines);

    // Try to join multi-line expressions that fit on one line
    let lines = try_join_lines(lines, &indent_levels, config);

    // Recompute indent levels after joining (structure may have changed)
    let indent_levels = compute_indent_levels(&lines);

    let mut result = Vec::new();

    for (line, &indent_level) in lines.into_iter().zip(indent_levels.iter()) {
        let new_indent = indent_level * config.indent_width;

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

    align_operators(result, config)
}

/// Align consecutive lines that start with the same operator to the same indentation level.
///
/// For pipe operators, indents relative to the content after `=`.
/// For other operators, uses the preceding line's indent + one level.
fn align_operators(mut lines: Vec<Line>, config: &FormatterConfig) -> Vec<Line> {
    let mut i = 0;
    while i < lines.len() {
        if let Some(op) = line_leading_operator(&lines[i]) {
            // Find the preceding non-empty, non-operator line to base indent on
            let base_line_idx = (0..i)
                .rev()
                .find(|&k| !lines[k].tokens.is_empty() && line_leading_operator(&lines[k]).is_none());

            let target_indent = if let Some(idx) = base_line_idx {
                let base_line = &lines[idx];
                if op == "|>" || op == "++" {
                    // For pipe and concat operators, indent relative to content after `=`
                    compute_pipe_continuation_indent(
                        &base_line.tokens,
                        base_line.original_indent,
                        config,
                    )
                } else {
                    base_line.original_indent + config.indent_width
                }
            } else {
                config.indent_width
            };

            lines[i].original_indent = target_indent;
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

/// Compute the continuation indent for pipe chains.
/// Returns position of first non-whitespace after `=`, plus indent_width.
fn compute_pipe_continuation_indent(
    tokens: &[Token],
    base_indent: usize,
    config: &FormatterConfig,
) -> usize {
    // Look for `=` token and find position of first non-whitespace after it
    let mut pos = base_indent;
    let mut found_eq = false;

    for tok in tokens.iter() {
        if found_eq {
            if tok.kind == TokenKind::Whitespace {
                pos += tok.text.len();
            } else {
                // Found first non-whitespace after `=`
                // Return this position + indent_width
                return pos + config.indent_width;
            }
        } else if tok.kind == TokenKind::Eq {
            found_eq = true;
            pos += tok.text.len();
        } else {
            pos += tok.text.len();
        }
    }

    // No `=` found or no content after it, use standard continuation indent
    base_indent + config.indent_width
}

/// Break a long line at sensible points.
/// Returns a list of sub-lines (each a Vec<Token>).
fn break_long_line(
    tokens: &[Token],
    base_indent: usize,
    config: &FormatterConfig,
) -> Vec<Vec<Token>> {
    // Strategy 1: try to break at |> operators first
    let pipe_positions = find_break_positions(tokens);
    if !pipe_positions.is_empty() {
        return break_at_positions(tokens, &pipe_positions, base_indent, config);
    }

    // Strategy 2: use fill style - pack as many args as fit per line
    if has_paren_content(tokens) {
        let result = break_fill_style(tokens, base_indent, config);
        if result.len() > 1 {
            return result;
        }
    }

    // If no good break points, return as-is
    vec![tokens.to_vec()]
}

/// Check if the line has parenthesized content that could be broken.
fn has_paren_content(tokens: &[Token]) -> bool {
    tokens.iter().any(|t| t.kind == TokenKind::LParen)
}

/// Break a long line using "fill" style - pack as many arguments as fit on each line.
/// Finds the first opening paren and breaks the arguments inside it.
fn break_fill_style(
    tokens: &[Token],
    base_indent: usize,
    config: &FormatterConfig,
) -> Vec<Vec<Token>> {
    // Find the first opening paren
    let paren_pos = tokens.iter().position(|t| t.kind == TokenKind::LParen);
    let paren_pos = match paren_pos {
        Some(p) => p,
        None => return vec![tokens.to_vec()],
    };

    // Everything before and including the paren is the "prefix"
    let prefix = &tokens[..=paren_pos];

    // Find the matching closing paren
    let mut depth = 1u32;
    let mut close_pos = None;
    for (i, tok) in tokens[paren_pos + 1..].iter().enumerate() {
        match tok.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                depth -= 1;
                if depth == 0 {
                    close_pos = Some(paren_pos + 1 + i);
                    break;
                }
            }
            _ => {}
        }
    }

    let close_pos = match close_pos {
        Some(p) => p,
        None => return vec![tokens.to_vec()],
    };

    // Parse the arguments inside the parens
    let inner = &tokens[paren_pos + 1..close_pos];
    let args = parse_args_inside_parens(inner);
    if args.is_empty() {
        return vec![tokens.to_vec()];
    }

    // Suffix is the closing paren and anything after
    let suffix = &tokens[close_pos..];

    // Build lines using fill style
    let continuation_indent = base_indent + 2; // Align with content after paren
    let mut lines: Vec<Vec<Token>> = Vec::new();
    let mut current_line: Vec<Token> = prefix.to_vec();
    let mut current_width = base_indent + tokens_display_width(prefix);

    for arg in args.iter() {
        let arg_width = tokens_display_width(arg);
        let space_needed = if current_line.last().is_some_and(|t| t.kind == TokenKind::LParen) {
            0
        } else {
            1
        };
        let new_width = current_width + space_needed + arg_width;

        if new_width > config.max_line_width && !is_just_prefix(&current_line, prefix) {
            // This arg doesn't fit, start a new line
            lines.push(std::mem::take(&mut current_line));
            current_width = continuation_indent;
            current_line = arg.clone();
            current_width += arg_width;
        } else {
            // Add to current line
            if space_needed > 0 {
                current_line.push(Token {
                    kind: TokenKind::Whitespace,
                    text: " ".to_string(),
                    offset: 0,
                });
                current_width += 1;
            }
            current_line.extend(arg.iter().cloned());
            current_width += arg_width;
        }
    }

    // Add the suffix (closing paren) to the last line
    current_line.extend(suffix.iter().cloned());
    lines.push(current_line);

    if lines.len() <= 1 {
        return vec![tokens.to_vec()];
    }

    lines
}

/// Check if current_line is just the prefix (nothing added yet)
fn is_just_prefix(current_line: &[Token], prefix: &[Token]) -> bool {
    current_line.len() == prefix.len()
}

/// Parse tokens inside parens into individual arguments.
fn parse_args_inside_parens(tokens: &[Token]) -> Vec<Vec<Token>> {
    let mut args = Vec::new();
    let mut current_arg = Vec::new();
    let mut paren_depth = 0u32;

    for tok in tokens {
        match tok.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => {
                paren_depth += 1;
                current_arg.push(tok.clone());
            }
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                paren_depth = paren_depth.saturating_sub(1);
                current_arg.push(tok.clone());
            }
            TokenKind::Whitespace if paren_depth == 0 => {
                // Top-level whitespace separates arguments
                if !current_arg.is_empty() {
                    args.push(std::mem::take(&mut current_arg));
                }
            }
            _ => {
                current_arg.push(tok.clone());
            }
        }
    }

    if !current_arg.is_empty() {
        args.push(current_arg);
    }

    args
}

/// Find positions where we can break the line (before |> or ++ operators).
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

/// Check if a token is a breakable operator (|> or ++).
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

        // Break before |> or ++
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
    config: &FormatterConfig,
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
                    line.starts_with("    "),
                    "Continuation line should be indented: {line:?}"
                );
            }
        }
    }

    #[test]
    fn test_break_long_concat_line() {
        let input = "message = \"Hello, \" ++ userName ++ \"! Welcome to \" ++ appName ++ \". Your session ID is: \" ++ sessionId\n";
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
    fn test_join_multiline_expression() {
        let input = "writeStatus\n  (ReplayStatus\n    true\n    false)\n";
        let tokens = lexer::lex(input);
        let config = FormatterConfig {
            max_line_width: 120,
            indent_width: 4,
        };
        let result = format_with_config(&tokens, &config);
        assert_eq!(result, "writeStatus (ReplayStatus true false)\n");
    }

    #[test]
    fn test_no_join_when_too_long() {
        let input = "writeStatus\n  (ReplayStatus\n    true\n    false)\n";
        let tokens = lexer::lex(input);
        let config = FormatterConfig {
            max_line_width: 30, // Too short to fit
            indent_width: 4,
        };
        let result = format_with_config(&tokens, &config);
        // Should stay multi-line (normalized indentation)
        assert!(
            result.lines().count() > 1,
            "Should remain multi-line when too long to fit: {result:?}"
        );
    }

    #[test]
    fn test_join_complex_multiline_expression() {
        // The motivating example from the plan
        let input = "writeStatus\n  (ReplayStatus\n    true\n    true\n    (Some since)\n    processed'\n    failed'\n    (Some startedAt)\n    (Some updatedAt)\n    None)\n";
        let tokens = lexer::lex(input);
        let config = FormatterConfig {
            max_line_width: 120,
            indent_width: 4,
        };
        let result = format_with_config(&tokens, &config);
        assert_eq!(
            result,
            "writeStatus (ReplayStatus true true (Some since) processed' failed' (Some startedAt) (Some updatedAt) None)\n"
        );
    }

    #[test]
    fn test_pipe_continuation_indent_after_equals() {
        // Pipe continuations should be indented indent_width after the first
        // non-whitespace character following the `=` sign
        let input = "_a = \"xxxxx\" |> toText |> toText |> toText\n";
        let tokens = lexer::lex(input);
        let config = FormatterConfig {
            max_line_width: 30,
            indent_width: 4,
        };
        let result = format_with_config(&tokens, &config);
        // `"` starts at column 5, so |> should be at column 5 + 4 = 9
        let expected = "_a = \"xxxxx\"\n         |> toText\n         |> toText\n         |> toText\n";
        assert_eq!(result, expected);
    }
}
