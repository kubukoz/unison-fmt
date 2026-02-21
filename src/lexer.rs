/// Lossless lexer for Unison source code.
/// Every byte of the input is represented in exactly one token.
/// The token stream can be concatenated to reconstruct the original source perfectly.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Trivia
    Whitespace, // spaces, tabs (no newlines)
    Newline,    // \n or \r\n
    LineComment,  // -- ... (including the --)
    BlockComment, // {- ... -} (nested)
    FoldComment,  // --- ... rest of file

    // Identifiers
    WordyId,   // alphanumeric identifier segment (e.g. foo, Nat, snake_case)
    SymbolyId, // operator identifier (e.g. ++, <*>)
    Dot,       // . (name separator)
    HashId,    // #abc123 (hash reference)

    // Literals
    NumericLit,  // integer, nat, float, hex, octal, binary
    StringLit,   // "..." single-line string
    RawStringLit, // """...""" raw multi-line string
    CharLit,     // ?X or ?\n etc.
    BytesLit,    // 0xs...

    // Keywords
    KwType, KwAbility, KwStructural, KwUnique,
    KwMatch, KwWith, KwCases,
    KwHandle, KwIf, KwThen, KwElse,
    KwLet, KwDo, KwWhere,
    KwUse, KwForall, KwForallUnicode, // ∀
    KwNamespace, KwTermLink, KwTypeLink,
    KwTrue, KwFalse,
    KwRewrite, // @rewrite

    // Punctuation / operators
    Eq,        // =
    Arrow,     // ->
    Colon,     // :
    AndAnd,    // &&
    OrOr,      // ||
    Pipe,      // |
    Bang,      // !
    Tick,      // '
    FatArrow,  // ==>
    At,        // @

    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma,
    #[allow(dead_code)]
    Question, // ?
    Semi,     // ;

    // Doc delimiters
    DocOpen,  // {{
    DocClose, // }}

    // Error recovery
    Error,

    // End of file
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub offset: usize,
}

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            tokens: Vec::new(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.input[self.pos + offset..].chars().next()
    }

    fn remaining(&self) -> &str {
        &self.input[self.pos..]
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn starts_with(&self, s: &str) -> bool {
        self.remaining().starts_with(s)
    }

    fn emit(&mut self, kind: TokenKind, start: usize) {
        self.tokens.push(Token {
            kind,
            text: self.input[start..self.pos].to_string(),
            offset: start,
        });
    }

    fn lex_all(&mut self) {
        while self.pos < self.input.len() {
            self.lex_token();
        }
        self.tokens.push(Token {
            kind: TokenKind::Eof,
            text: String::new(),
            offset: self.pos,
        });
    }

    fn lex_token(&mut self) {
        let start = self.pos;
        let ch = match self.peek() {
            Some(c) => c,
            None => return,
        };

        // Newlines
        if ch == '\n' {
            self.advance();
            self.emit(TokenKind::Newline, start);
            return;
        }
        if ch == '\r' && self.peek_at(1) == Some('\n') {
            self.advance();
            self.advance();
            self.emit(TokenKind::Newline, start);
            return;
        }

        // Whitespace (non-newline)
        if ch.is_whitespace() && ch != '\n' {
            while let Some(c) = self.peek() {
                if c.is_whitespace() && c != '\n' && c != '\r' {
                    self.advance();
                } else {
                    break;
                }
            }
            self.emit(TokenKind::Whitespace, start);
            return;
        }

        // Fold comment (--- to end of file)
        if self.starts_with("---") && self.is_fold_comment_start(start) {
            // consume rest of file
            while self.advance().is_some() {}
            self.emit(TokenKind::FoldComment, start);
            return;
        }

        // Line comment
        if self.starts_with("--") && !self.starts_with("---") {
            while let Some(c) = self.peek() {
                if c == '\n' {
                    break;
                }
                self.advance();
            }
            self.emit(TokenKind::LineComment, start);
            return;
        }

        // Block comment {- ... -} (nested)
        if self.starts_with("{-") {
            self.advance(); // {
            self.advance(); // -
            let mut depth = 1u32;
            while depth > 0 {
                match self.peek() {
                    None => break,
                    Some('{') if self.peek_at(1) == Some('-') => {
                        self.advance();
                        self.advance();
                        depth += 1;
                    }
                    Some('-') if self.peek_at(1) == Some('}') => {
                        self.advance();
                        self.advance();
                        depth -= 1;
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
            self.emit(TokenKind::BlockComment, start);
            return;
        }

        // Doc open/close {{ }}
        if self.starts_with("{{") {
            self.advance();
            self.advance();
            self.emit(TokenKind::DocOpen, start);
            return;
        }
        if self.starts_with("}}") {
            self.advance();
            self.advance();
            self.emit(TokenKind::DocClose, start);
            return;
        }

        // Raw string literal """..."""
        if self.starts_with("\"\"\"") {
            self.advance(); // "
            self.advance(); // "
            self.advance(); // "
            // count additional leading quotes
            let mut extra_quotes = 0;
            while self.peek() == Some('"') {
                self.advance();
                extra_quotes += 1;
            }
            let close_len = 3 + extra_quotes;
            let close_pattern: String = std::iter::repeat('"').take(close_len).collect();
            loop {
                if self.remaining().starts_with(&close_pattern) {
                    for _ in 0..close_len {
                        self.advance();
                    }
                    break;
                }
                if self.advance().is_none() {
                    break;
                }
            }
            self.emit(TokenKind::RawStringLit, start);
            return;
        }

        // Single-line string literal
        if ch == '"' {
            self.advance(); // opening "
            loop {
                match self.peek() {
                    None | Some('\n') => break,
                    Some('\\') => {
                        self.advance();
                        self.advance(); // skip escaped char
                    }
                    Some('"') => {
                        self.advance();
                        break;
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
            self.emit(TokenKind::StringLit, start);
            return;
        }

        // Character literal ?X
        if ch == '?' && self.is_char_literal_context() {
            self.advance(); // ?
            if self.peek() == Some('\\') {
                self.advance(); // backslash
                self.advance(); // escaped char
            } else if self.peek().is_some() {
                self.advance(); // the character
            }
            self.emit(TokenKind::CharLit, start);
            return;
        }

        // Bytes literal 0xs...
        if self.starts_with("0xs") {
            self.advance(); // 0
            self.advance(); // x
            self.advance(); // s
            while let Some(c) = self.peek() {
                if c.is_ascii_alphanumeric() {
                    self.advance();
                } else {
                    break;
                }
            }
            self.emit(TokenKind::BytesLit, start);
            return;
        }

        // Numeric literals: hex, octal, binary, float, int/nat
        if ch.is_ascii_digit() || ((ch == '+' || ch == '-') && self.is_signed_number_start()) {
            self.lex_numeric(start);
            return;
        }

        // Hash literal #...
        if ch == '#' {
            self.advance();
            while let Some(c) = self.peek() {
                if c.is_ascii_alphanumeric() || c == '.' {
                    self.advance();
                } else {
                    break;
                }
            }
            self.emit(TokenKind::HashId, start);
            return;
        }

        // Multi-char operators / keywords first
        // @rewrite
        if self.starts_with("@rewrite") && self.is_keyword_boundary(8) {
            for _ in 0..8 {
                self.advance();
            }
            self.emit(TokenKind::KwRewrite, start);
            return;
        }

        // ==>
        if self.starts_with("==>") {
            self.advance();
            self.advance();
            self.advance();
            self.emit(TokenKind::FatArrow, start);
            return;
        }

        // ==
        // In Unison, == is a regular symboly identifier, not a reserved op.

        // ->
        if self.starts_with("->") {
            self.advance();
            self.advance();
            self.emit(TokenKind::Arrow, start);
            return;
        }

        // &&
        if self.starts_with("&&") {
            self.advance();
            self.advance();
            self.emit(TokenKind::AndAnd, start);
            return;
        }

        // ||
        if self.starts_with("||") {
            self.advance();
            self.advance();
            self.emit(TokenKind::OrOr, start);
            return;
        }

        // Single-char punctuation
        match ch {
            '(' => { self.advance(); self.emit(TokenKind::LParen, start); return; }
            ')' => { self.advance(); self.emit(TokenKind::RParen, start); return; }
            '[' => { self.advance(); self.emit(TokenKind::LBracket, start); return; }
            ']' => { self.advance(); self.emit(TokenKind::RBracket, start); return; }
            '{' => { self.advance(); self.emit(TokenKind::LBrace, start); return; }
            '}' => { self.advance(); self.emit(TokenKind::RBrace, start); return; }
            ',' => { self.advance(); self.emit(TokenKind::Comma, start); return; }
            ';' => { self.advance(); self.emit(TokenKind::Semi, start); return; }
            _ => {}
        }

        // Reserved single-char operators — but only if not part of a longer symboly id
        if ch == '=' && !self.is_symboly_continuation(1) {
            self.advance();
            self.emit(TokenKind::Eq, start);
            return;
        }
        if ch == ':' && !self.is_symboly_continuation(1) {
            self.advance();
            self.emit(TokenKind::Colon, start);
            return;
        }
        if ch == '|' && !self.is_symboly_continuation(1) {
            self.advance();
            self.emit(TokenKind::Pipe, start);
            return;
        }
        if ch == '@' && !self.starts_with("@rewrite") {
            self.advance();
            self.emit(TokenKind::At, start);
            return;
        }

        // Delay/force — only in appropriate contexts
        if ch == '\'' && !self.is_symboly_continuation(1) {
            self.advance();
            self.emit(TokenKind::Tick, start);
            return;
        }
        if ch == '!' && !self.is_symboly_continuation(1) {
            self.advance();
            self.emit(TokenKind::Bang, start);
            return;
        }

        // Dot — name separator
        if ch == '.' && !self.is_symboly_id_char(self.peek_at(1)) {
            self.advance();
            self.emit(TokenKind::Dot, start);
            return;
        }

        // ∀ (forall unicode)
        if ch == '∀' {
            self.advance();
            self.emit(TokenKind::KwForallUnicode, start);
            return;
        }

        // Wordy identifier or keyword
        if is_wordy_id_start(ch) {
            self.lex_wordy(start);
            return;
        }

        // Symboly identifier
        if is_symboly_id_char(ch) {
            self.lex_symboly(start);
            return;
        }

        // Unknown character — emit as error
        self.advance();
        self.emit(TokenKind::Error, start);
    }

    fn lex_wordy(&mut self, start: usize) {
        while let Some(c) = self.peek() {
            if is_wordy_id_char(c) {
                self.advance();
            } else {
                break;
            }
        }
        let text = &self.input[start..self.pos];
        let kind = match text {
            "type" => TokenKind::KwType,
            "ability" => TokenKind::KwAbility,
            "structural" => TokenKind::KwStructural,
            "unique" => TokenKind::KwUnique,
            "match" => TokenKind::KwMatch,
            "with" => TokenKind::KwWith,
            "cases" => TokenKind::KwCases,
            "handle" => TokenKind::KwHandle,
            "if" => TokenKind::KwIf,
            "then" => TokenKind::KwThen,
            "else" => TokenKind::KwElse,
            "let" => TokenKind::KwLet,
            "do" => TokenKind::KwDo,
            "where" => TokenKind::KwWhere,
            "use" => TokenKind::KwUse,
            "forall" => TokenKind::KwForall,
            "namespace" => TokenKind::KwNamespace,
            "termLink" => TokenKind::KwTermLink,
            "typeLink" => TokenKind::KwTypeLink,
            "true" => TokenKind::KwTrue,
            "false" => TokenKind::KwFalse,
            _ => TokenKind::WordyId,
        };
        self.emit(kind, start);
    }

    fn lex_symboly(&mut self, start: usize) {
        while let Some(c) = self.peek() {
            if is_symboly_id_char(c) {
                self.advance();
            } else {
                break;
            }
        }
        let text = &self.input[start..self.pos];
        // Check if this is a reserved operator that got lexed as symboly
        let kind = match text {
            "=" => TokenKind::Eq,
            "->" => TokenKind::Arrow,
            ":" => TokenKind::Colon,
            "&&" => TokenKind::AndAnd,
            "||" => TokenKind::OrOr,
            "|" => TokenKind::Pipe,
            "!" => TokenKind::Bang,
            "'" => TokenKind::Tick,
            "==>" => TokenKind::FatArrow,
            _ => TokenKind::SymbolyId,
        };
        self.emit(kind, start);
    }

    fn lex_numeric(&mut self, start: usize) {
        // Handle optional sign
        if self.peek() == Some('+') || self.peek() == Some('-') {
            self.advance();
        }

        // Check for base prefixes
        if self.peek() == Some('0') {
            match self.peek_at(1) {
                Some('x') | Some('X') => {
                    self.advance(); // 0
                    self.advance(); // x
                    while let Some(c) = self.peek() {
                        if c.is_ascii_hexdigit() {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.emit(TokenKind::NumericLit, start);
                    return;
                }
                Some('o') | Some('O') => {
                    self.advance(); // 0
                    self.advance(); // o
                    while let Some(c) = self.peek() {
                        if ('0'..='7').contains(&c) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.emit(TokenKind::NumericLit, start);
                    return;
                }
                Some('b') | Some('B') => {
                    self.advance(); // 0
                    self.advance(); // b
                    while let Some(c) = self.peek() {
                        if c == '0' || c == '1' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.emit(TokenKind::NumericLit, start);
                    return;
                }
                _ => {}
            }
        }

        // Integer part
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Fractional part
        if self.peek() == Some('.') && self.peek_at(1).is_some_and(|c| c.is_ascii_digit()) {
            self.advance(); // .
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Exponent
        if self.peek() == Some('e') || self.peek() == Some('E') {
            self.advance();
            if self.peek() == Some('+') || self.peek() == Some('-') {
                self.advance();
            }
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.emit(TokenKind::NumericLit, start);
    }

    fn is_fold_comment_start(&self, start: usize) -> bool {
        // --- is a fold comment only at the beginning of a line or preceded by only whitespace on that line
        if start == 0 {
            return true;
        }
        // Check chars before start on the same line
        let before = &self.input[..start];
        for ch in before.chars().rev() {
            if ch == '\n' {
                return true;
            }
            if !ch.is_whitespace() {
                return false;
            }
        }
        true
    }

    fn is_signed_number_start(&self) -> bool {
        // +N or -N where N is a digit, and preceded by whitespace or operator
        let next = self.peek_at(1);
        next.is_some_and(|c| c.is_ascii_digit())
            && (self.pos == 0 || {
                let prev = self.input[..self.pos].chars().last();
                prev.is_some_and(|c| c.is_whitespace() || c == '(' || c == '[' || c == ',')
            })
    }

    fn is_keyword_boundary(&self, offset: usize) -> bool {
        match self.input[self.pos + offset..].chars().next() {
            None => true,
            Some(c) => !is_wordy_id_char(c),
        }
    }

    fn is_symboly_continuation(&self, offset: usize) -> bool {
        self.peek_at(offset).is_some_and(|c| is_symboly_id_char(c))
    }

    fn is_symboly_id_char(&self, ch: Option<char>) -> bool {
        ch.is_some_and(is_symboly_id_char)
    }

    fn is_char_literal_context(&self) -> bool {
        // ?X is a char literal if ? is not part of a symboly identifier
        let next = self.peek_at(1);
        next.is_some_and(|c| !c.is_whitespace())
    }
}

/// Unison wordy identifier start character
fn is_wordy_id_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

/// Unison wordy identifier continuation character
fn is_wordy_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}

/// Unison symboly identifier character
fn is_symboly_id_char(c: char) -> bool {
    // Based on Unison's symbolyIdChar
    // Includes operator characters but NOT delimiters like ()[]{}
    matches!(c, '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '-' | '='
        | '+' | '<' | '>' | '.' | '~' | '\\' | '/' | '\'' | '`' | '|' | ':'
    )
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    lexer.lex_all();
    lexer.tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_kinds(input: &str) -> Vec<TokenKind> {
        lex(input).into_iter().map(|t| t.kind).collect()
    }

    fn roundtrip(input: &str) {
        let tokens = lex(input);
        let reconstructed: String = tokens.iter().map(|t| t.text.as_str()).collect();
        assert_eq!(input, &reconstructed, "Lexer roundtrip failed");
    }

    #[test]
    fn test_roundtrip_simple() {
        roundtrip("foo bar = 42\n");
    }

    #[test]
    fn test_roundtrip_comments() {
        roundtrip("-- hello\nfoo = 1\n{- block {- nested -} -}\n");
    }

    #[test]
    fn test_roundtrip_string() {
        roundtrip("x = \"hello world\"\n");
    }

    #[test]
    fn test_roundtrip_operators() {
        roundtrip("f x = x + 1\ng = f . h\n");
    }

    #[test]
    fn test_keywords() {
        let kinds = lex_kinds("if true then 1 else 2");
        assert!(kinds.contains(&TokenKind::KwIf));
        assert!(kinds.contains(&TokenKind::KwTrue));
        assert!(kinds.contains(&TokenKind::KwThen));
        assert!(kinds.contains(&TokenKind::KwElse));
    }

    #[test]
    fn test_type_decl() {
        roundtrip("structural type Pair a b = Pair a b\n");
    }

    #[test]
    fn test_ability_decl() {
        roundtrip("ability Remote where\n  spawn : {Remote} Node\n");
    }

    #[test]
    fn test_doc_block() {
        roundtrip("{{ This is a doc block }}\n");
    }

    #[test]
    fn test_raw_string() {
        roundtrip("x = \"\"\"\nhello\nworld\n\"\"\"\n");
    }

    #[test]
    fn test_char_literal() {
        roundtrip("x = ?a\ny = ?\\n\n");
    }

    #[test]
    fn test_bytes_literal() {
        roundtrip("x = 0xsABCD\n");
    }

    #[test]
    fn test_numeric_literals() {
        roundtrip("a = 42\nb = 3.14\nc = 0xff\nd = 0o77\ne = 0b1010\n");
    }

    #[test]
    fn test_hash_qualified() {
        roundtrip("x = foo#abc123\n");
    }

    #[test]
    fn test_forall() {
        roundtrip("f : forall a. a -> a\n");
    }

    #[test]
    fn test_match_cases() {
        roundtrip("g = cases\n  0 -> \"hi\"\n  1 -> \"bye\"\n");
    }

    #[test]
    fn test_handle_with() {
        roundtrip("handle !r with step (Node.Node 0)\n");
    }
}
