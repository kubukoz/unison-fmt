pub mod formatter;
pub mod lexer;
pub mod syntax;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn format(input: &str) -> String {
    let tokens = lexer::lex(input);
    formatter::format(&tokens)
}

#[wasm_bindgen]
pub fn format_with_options(input: &str, indent_width: usize, max_line_width: usize) -> String {
    let tokens = lexer::lex(input);
    let config = formatter::FormatConfig {
        indent_width,
        max_line_width,
    };
    formatter::format_with_config(&tokens, &config)
}
