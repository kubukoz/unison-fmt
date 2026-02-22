pub mod formatter;
pub mod lexer;
pub mod syntax;

use formatter::FormatterConfig;
use wasm_bindgen::prelude::*;

/// Format options for WASM binding.
#[wasm_bindgen]
pub struct FormatOptions {
    indent_width: usize,
    max_line_width: usize,
}

#[wasm_bindgen]
impl FormatOptions {
    /// Create new format options.
    #[wasm_bindgen(constructor)]
    pub fn new(indent_width: usize, max_line_width: usize) -> FormatOptions {
        FormatOptions {
            indent_width,
            max_line_width,
        }
    }

    /// Get the indent width.
    #[wasm_bindgen(getter)]
    pub fn indent_width(&self) -> usize {
        self.indent_width
    }

    /// Get the max line width.
    #[wasm_bindgen(getter)]
    pub fn max_line_width(&self) -> usize {
        self.max_line_width
    }
}

impl From<&FormatOptions> for FormatterConfig {
    fn from(opts: &FormatOptions) -> Self {
        FormatterConfig {
            indent_width: opts.indent_width,
            max_line_width: opts.max_line_width,
        }
    }
}

/// Format Unison source code using default options.
#[wasm_bindgen]
pub fn format(input: &str) -> String {
    let tokens = lexer::lex(input);
    formatter::format(&tokens)
}

/// Format Unison source code with custom options.
#[wasm_bindgen]
pub fn format_with_options(input: &str, options: &FormatOptions) -> String {
    let tokens = lexer::lex(input);
    let config = FormatterConfig::from(options);
    formatter::format_with_config(&tokens, &config)
}
