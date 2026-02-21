pub mod formatter;
pub mod lexer;
pub mod syntax;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn format(input: &str) -> String {
    let tokens = lexer::lex(input);
    formatter::format(&tokens)
}
