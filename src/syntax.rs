/// Lossless CST (Concrete Syntax Tree) for Unison.
/// Every token from the lexer appears exactly once in the tree.
/// Concatenating all leaf tokens reproduces the original source.

use crate::lexer::{Token, TokenKind};

/// A node in the Concrete Syntax Tree.
#[derive(Debug)]
pub enum CstNode {
    /// A terminal token (leaf).
    Token(Token),
    /// A non-terminal node containing children.
    Node {
        #[allow(dead_code)]
        kind: NodeKind,
        children: Vec<CstNode>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum NodeKind {
    /// Root of the file.
    SourceFile,
    /// A top-level declaration (term, type, ability, etc.).
    Decl,
    /// A type declaration: [unique|structural] type Name = ...
    TypeDecl,
    /// An ability declaration: [unique|structural] ability Name where ...
    AbilityDecl,
    /// A use clause: use Namespace ...
    UseClause,
    /// A namespace directive
    NamespaceDirective,
    /// A term definition: name [: type] = expr
    TermDef,
    /// A type signature: name : type
    TypeSig,
    /// A watch expression: > expr
    WatchExpr,
    /// A block (indentation-delimited group of statements)
    Block,
    /// An expression
    Expr,
    /// A type expression
    TypeExpr,
    /// A pattern
    Pattern,
    /// A match/cases arm
    MatchArm,
    /// An if-then-else
    IfThenElse,
    /// A match-with expression
    MatchWith,
    /// A handle-with expression
    HandleWith,
    /// A let expression
    LetExpr,
    /// A do block
    DoBlock,
    /// A doc block {{ ... }}
    DocBlock,
    /// Parenthesized expression
    Paren,
    /// List literal [...]
    ListLit,
    /// Record/ability handler block { ... }
    BraceBlock,
    /// A function application (juxtaposition)
    App,
    /// An operator application
    BinOp,
    /// A lambda: a -> b
    Lambda,
    /// A forall type
    ForallType,
    /// A name (possibly qualified with dots)
    Name,
    /// An error recovery node
    Error,
}

/// Parser that builds a lossless CST from a token stream.
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    fuel: u32,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            fuel: 0,
        }
    }

    fn peek(&self) -> TokenKind {
        self.tokens.get(self.pos).map_or(TokenKind::Eof, |t| t.kind)
    }

    fn peek_significant(&self) -> TokenKind {
        let mut i = self.pos;
        loop {
            match self.tokens.get(i) {
                None => return TokenKind::Eof,
                Some(t) if is_trivia(t.kind) => i += 1,
                Some(t) => return t.kind,
            }
        }
    }

    /// Look ahead past trivia to find the nth significant token.
    fn peek_significant_nth(&self, n: usize) -> TokenKind {
        let mut i = self.pos;
        let mut count = 0;
        loop {
            match self.tokens.get(i) {
                None => return TokenKind::Eof,
                Some(t) if is_trivia(t.kind) => i += 1,
                Some(t) => {
                    if count == n {
                        return t.kind;
                    }
                    count += 1;
                    i += 1;
                }
            }
        }
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.tokens.len() || self.peek() == TokenKind::Eof
    }

    fn bump(&mut self) -> CstNode {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        self.fuel = 0;
        CstNode::Token(tok)
    }

    fn eat_trivia(&mut self, children: &mut Vec<CstNode>) {
        while self.pos < self.tokens.len() && is_trivia(self.peek()) {
            children.push(self.bump());
        }
    }

    #[allow(dead_code)]
    fn expect(&mut self, kind: TokenKind, children: &mut Vec<CstNode>) {
        self.eat_trivia(children);
        if self.peek() == kind {
            children.push(self.bump());
        }
        // If not found, we just don't consume — error recovery by omission
    }

    /// Get the current line's indentation column for the current token.
    fn current_indent(&self) -> usize {
        if self.pos >= self.tokens.len() {
            return 0;
        }
        let tok = &self.tokens[self.pos];
        offset_to_column(&self.tokens, tok.offset)
    }

    fn check_fuel(&mut self) -> bool {
        self.fuel += 1;
        if self.fuel > 1000 {
            return false;
        }
        true
    }
}

fn offset_to_column(tokens: &[Token], offset: usize) -> usize {
    // Walk backwards from offset to find the last newline
    // This is approximate — we use the token's text to figure out columns
    // For a proper implementation we'd track line/col during lexing
    // For now, we reconstruct from the full source by looking at tokens before us
    let mut col = 0;
    let mut byte_pos = 0;
    for tok in tokens {
        if tok.offset >= offset {
            break;
        }
        for ch in tok.text.chars() {
            if ch == '\n' {
                col = 0;
            } else {
                col += 1;
            }
            byte_pos += ch.len_utf8();
        }
        if byte_pos > offset {
            break;
        }
    }
    col
}

fn is_trivia(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::LineComment
            | TokenKind::BlockComment
            | TokenKind::FoldComment
    )
}

fn is_decl_start(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwType
            | TokenKind::KwAbility
            | TokenKind::KwStructural
            | TokenKind::KwUnique
            | TokenKind::KwUse
            | TokenKind::KwNamespace
    )
}

#[allow(dead_code)]
fn is_expr_start(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::WordyId
            | TokenKind::SymbolyId
            | TokenKind::NumericLit
            | TokenKind::StringLit
            | TokenKind::RawStringLit
            | TokenKind::CharLit
            | TokenKind::BytesLit
            | TokenKind::KwTrue
            | TokenKind::KwFalse
            | TokenKind::KwIf
            | TokenKind::KwMatch
            | TokenKind::KwHandle
            | TokenKind::KwLet
            | TokenKind::KwDo
            | TokenKind::KwCases
            | TokenKind::KwForall
            | TokenKind::KwForallUnicode
            | TokenKind::KwTermLink
            | TokenKind::KwTypeLink
            | TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::LBrace
            | TokenKind::Tick
            | TokenKind::Bang
            | TokenKind::DocOpen
            | TokenKind::HashId
            | TokenKind::Dot
    )
}

/// Parse a token stream into a lossless CST.
pub fn parse(tokens: Vec<Token>) -> CstNode {
    let mut parser = Parser::new(tokens);
    let mut children = Vec::new();

    while !parser.at_eof() {
        parser.eat_trivia(&mut children);
        if parser.at_eof() {
            break;
        }

        let before = parser.pos;
        let sig = parser.peek_significant();
        match sig {
            TokenKind::KwType
            | TokenKind::KwAbility
            | TokenKind::KwStructural
            | TokenKind::KwUnique => {
                children.push(parse_type_or_ability_decl(&mut parser));
            }
            TokenKind::KwUse => {
                children.push(parse_use_clause(&mut parser));
            }
            TokenKind::KwNamespace => {
                children.push(parse_namespace_directive(&mut parser));
            }
            TokenKind::DocOpen => {
                children.push(parse_doc_block(&mut parser));
            }
            _ => {
                children.push(parse_term_or_watch(&mut parser));
            }
        }

        // Safety: ensure we make progress
        if parser.pos == before {
            if !parser.at_eof() {
                children.push(parser.bump());
            } else {
                break;
            }
        }
    }

    CstNode::Node {
        kind: NodeKind::SourceFile,
        children,
    }
}

fn parse_type_or_ability_decl(parser: &mut Parser) -> CstNode {
    let mut children = Vec::new();
    parser.eat_trivia(&mut children);

    // Optional modifier: structural | unique
    let sig = parser.peek_significant();
    if sig == TokenKind::KwStructural || sig == TokenKind::KwUnique {
        children.push(parser.bump());
        parser.eat_trivia(&mut children);
        // Handle unique[guid] syntax
        if parser.peek_significant() == TokenKind::LBracket {
            children.push(parser.bump()); // [
            while !parser.at_eof() {
                let k = parser.peek();
                if k == TokenKind::RBracket {
                    children.push(parser.bump());
                    break;
                }
                children.push(parser.bump());
            }
            parser.eat_trivia(&mut children);
        }
    }

    let sig = parser.peek_significant();
    if sig == TokenKind::KwType {
        children.push(parser.bump());
        parser.eat_trivia(&mut children);
        // Type name and params until =
        consume_until_eq_or_newblock(parser, &mut children);
        // Constructor definitions (indented block or after =)
        if parser.peek_significant() == TokenKind::Eq {
            children.push(parser.bump());
            parser.eat_trivia(&mut children);
            // Parse constructor lines
            parse_indented_block(parser, &mut children);
        }
        CstNode::Node {
            kind: NodeKind::TypeDecl,
            children,
        }
    } else if sig == TokenKind::KwAbility {
        children.push(parser.bump());
        parser.eat_trivia(&mut children);
        // Ability name until "where"
        while !parser.at_eof() {
            let k = parser.peek_significant();
            if k == TokenKind::KwWhere {
                break;
            }
            if is_trivia(parser.peek()) {
                parser.eat_trivia(&mut children);
            } else {
                children.push(parser.bump());
            }
        }
        if parser.peek_significant() == TokenKind::KwWhere {
            children.push(parser.bump());
            parser.eat_trivia(&mut children);
            // Parse ability body (indented block)
            parse_indented_block(parser, &mut children);
        }
        CstNode::Node {
            kind: NodeKind::AbilityDecl,
            children,
        }
    } else {
        // Unexpected — just consume as term
        parse_term_or_watch(parser)
    }
}

fn parse_use_clause(parser: &mut Parser) -> CstNode {
    let mut children = Vec::new();
    parser.eat_trivia(&mut children);

    // use Namespace Ident1 Ident2 ...
    // Consume until end of line
    consume_until_newline(parser, &mut children);

    CstNode::Node {
        kind: NodeKind::UseClause,
        children,
    }
}

fn parse_namespace_directive(parser: &mut Parser) -> CstNode {
    let mut children = Vec::new();
    parser.eat_trivia(&mut children);
    consume_until_newline(parser, &mut children);
    CstNode::Node {
        kind: NodeKind::NamespaceDirective,
        children,
    }
}

fn parse_doc_block(parser: &mut Parser) -> CstNode {
    let mut children = Vec::new();
    parser.eat_trivia(&mut children);

    // {{ ... }}
    if parser.peek() == TokenKind::DocOpen {
        children.push(parser.bump());
        let mut depth = 1u32;
        while !parser.at_eof() && depth > 0 {
            if parser.peek() == TokenKind::DocOpen {
                depth += 1;
            } else if parser.peek() == TokenKind::DocClose {
                depth -= 1;
                if depth == 0 {
                    children.push(parser.bump());
                    break;
                }
            }
            children.push(parser.bump());
        }
    }

    CstNode::Node {
        kind: NodeKind::DocBlock,
        children,
    }
}

fn parse_term_or_watch(parser: &mut Parser) -> CstNode {
    let mut children = Vec::new();
    parser.eat_trivia(&mut children);

    if parser.at_eof() {
        return CstNode::Node {
            kind: NodeKind::Decl,
            children,
        };
    }

    // Detect watch expression: > expr  (at start of line, > is part of symboly)
    // In Unison, lines starting with > are "watch expressions"
    let start_col = parser.current_indent();

    // Check for various top-level forms
    // The general strategy: consume tokens greedily as a "declaration"
    // until we hit another top-level form at the same or lesser indentation.
    let base_indent = start_col;
    parse_block_body(parser, &mut children, base_indent);

    CstNode::Node {
        kind: NodeKind::Decl,
        children,
    }
}

/// Parse a block of code at a given indentation level.
/// Consumes tokens until we encounter a token at a lesser or equal indentation
/// that looks like a new top-level declaration.
fn parse_block_body(parser: &mut Parser, children: &mut Vec<CstNode>, base_indent: usize) {
    let mut last_was_newline = false;

    while !parser.at_eof() {
        if !parser.check_fuel() {
            break;
        }

        let kind = parser.peek();

        // Check if we've hit a new top-level thing
        if last_was_newline && !is_trivia(kind) && kind != TokenKind::Eof {
            let col = parser.current_indent();
            if col <= base_indent && parser.pos > 0 {
                // We've reached something at the same or lesser indentation.
                // Only break if it looks like a new declaration.
                let sig = parser.peek_significant();
                if is_decl_start(sig) || is_new_term_def(parser, col, base_indent) {
                    break;
                }
            }
        }

        if kind == TokenKind::Newline {
            last_was_newline = true;
            children.push(parser.bump());
        } else if is_trivia(kind) {
            children.push(parser.bump());
        } else {
            last_was_newline = false;
            children.push(parser.bump());
        }
    }
}

/// Check if the current position looks like a new term definition at top level.
fn is_new_term_def(parser: &Parser, col: usize, base_indent: usize) -> bool {
    if col > base_indent {
        return false;
    }
    // A new term def typically starts with an identifier followed by : or =
    // or another identifier (function args)
    let first = parser.peek_significant();
    if first == TokenKind::WordyId || first == TokenKind::SymbolyId || first == TokenKind::LParen {
        let second = parser.peek_significant_nth(1);
        // If followed by : or = or another identifier or (, it's likely a new decl
        return matches!(
            second,
            TokenKind::Colon
                | TokenKind::Eq
                | TokenKind::WordyId
                | TokenKind::SymbolyId
                | TokenKind::LParen
                | TokenKind::Dot
                | TokenKind::KwType
                | TokenKind::KwAbility
                | TokenKind::DocOpen
        );
    }
    // Watch expression
    if first == TokenKind::SymbolyId {
        return true;
    }
    false
}

fn consume_until_eq_or_newblock(parser: &mut Parser, children: &mut Vec<CstNode>) {
    while !parser.at_eof() {
        let k = parser.peek_significant();
        if k == TokenKind::Eq || k == TokenKind::KwWhere || k == TokenKind::Eof {
            break;
        }
        // Don't cross newlines that look like they end the header
        if parser.peek() == TokenKind::Newline {
            // peek ahead to see if next line is indented (continuation) or not
            children.push(parser.bump()); // newline
            parser.eat_trivia(children);
            let k = parser.peek_significant();
            if k == TokenKind::Eq || k == TokenKind::Pipe {
                break;
            }
            // Check if it looks like a constructor line (starts with name at same indent)
            // For type decls, the = might be on the same line
            if k == TokenKind::Eof {
                break;
            }
            continue;
        }
        if is_trivia(parser.peek()) {
            parser.eat_trivia(children);
        } else {
            children.push(parser.bump());
        }
    }
}

fn consume_until_newline(parser: &mut Parser, children: &mut Vec<CstNode>) {
    while !parser.at_eof() {
        if parser.peek() == TokenKind::Newline || parser.peek() == TokenKind::Eof {
            break;
        }
        children.push(parser.bump());
    }
}

fn parse_indented_block(parser: &mut Parser, children: &mut Vec<CstNode>) {
    // Consume the rest of the indented block after a = or where
    // We figure out the indent of the first non-trivia token and consume everything
    // at that indent or deeper
    parser.eat_trivia(children);
    if parser.at_eof() {
        return;
    }

    let block_indent = parser.current_indent();
    let mut last_was_newline = false;

    while !parser.at_eof() {
        let kind = parser.peek();
        if kind == TokenKind::Newline {
            last_was_newline = true;
            children.push(parser.bump());
            continue;
        }
        if is_trivia(kind) {
            children.push(parser.bump());
            continue;
        }
        if last_was_newline {
            let col = parser.current_indent();
            if col < block_indent {
                break;
            }
        }
        last_was_newline = false;
        children.push(parser.bump());
    }
}

/// Reconstruct the original source from a CST — perfectly lossless.
pub fn print_lossless(node: &CstNode) -> String {
    let mut out = String::new();
    print_node(node, &mut out);
    out
}

fn print_node(node: &CstNode, out: &mut String) {
    match node {
        CstNode::Token(tok) => {
            out.push_str(&tok.text);
        }
        CstNode::Node { children, .. } => {
            for child in children {
                print_node(child, out);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    fn roundtrip(input: &str) {
        let tokens = lexer::lex(input);
        let cst = parse(tokens);
        let printed = print_lossless(&cst);
        assert_eq!(input, &printed, "CST roundtrip failed");
    }

    #[test]
    fn test_roundtrip_empty() {
        roundtrip("");
    }

    #[test]
    fn test_roundtrip_simple_def() {
        roundtrip("foo = 42\n");
    }

    #[test]
    fn test_roundtrip_with_comment() {
        roundtrip("-- comment\nfoo = 42\n");
    }

    #[test]
    fn test_roundtrip_multiline() {
        roundtrip("foo x =\n  x + 1\n\nbar = 2\n");
    }

    #[test]
    fn test_roundtrip_type_decl() {
        roundtrip("structural type Pair a b = Pair a b\n");
    }

    #[test]
    fn test_roundtrip_ability() {
        roundtrip("ability Remote where\n  spawn : {Remote} Node\n");
    }

    #[test]
    fn test_roundtrip_use_clause() {
        roundtrip("use Optional None Some\n");
    }

    #[test]
    fn test_roundtrip_doc() {
        roundtrip("{{ This is a doc }}\n");
    }

    #[test]
    fn test_roundtrip_complex() {
        let input = r#"-- Unison is a statically typed functional language

increment : Nat -> Nat -- signature is optional
increment n = n + 1

-- Lines starting with `>` are evaluated and printed on every file save.
> increment 99

replicate : Nat -> a -> [a]
replicate n a = toSequence (take n (constant a))
"#;
        roundtrip(input);
    }

    #[test]
    fn test_roundtrip_basics() {
        let input = r#"
-- Unison is a statically typed functional language

increment : Nat -> Nat -- signature is optional
increment n = n + 1

-- this is nice for quick testing!

> replicate 3 "bye"

merge : (a -> a -> Boolean) -> [a] -> [a] -> [a]
merge lte a b =
  use Sequence ++
  use Optional None Some
  go acc a b = match at 0 a with
    None -> acc ++ b
    Some hd1 -> match at 0 b with
      None -> acc ++ a
      Some hd2 ->
        if hd1 `lte` hd2 then go (acc `snoc` hd1) (drop 1 a) b
        else go (acc `snoc` hd2) a (drop 1 b)
  go [] a b
"#;
        roundtrip(input);
    }
}
