use clap::{Parser, ValueEnum};
use std::io::Read;
use unison_fmt::formatter::{self, FormatterConfig};
use unison_fmt::lexer;
use unison_fmt::syntax;

#[derive(Debug, Clone, Copy, ValueEnum, Default)]
enum Mode {
    /// Format the input
    #[default]
    Fmt,
    /// Tokenize and print tokens
    Lex,
    /// Parse and print CST
    Parse,
    /// Check roundtrip parsing
    Check,
}

#[derive(Parser)]
#[command(name = "unison-fmt")]
#[command(about = "Format Unison source files")]
struct Args {
    /// Mode of operation
    #[arg(value_enum, default_value_t = Mode::Fmt)]
    mode: Mode,

    /// Input file (reads from stdin if not provided)
    file: Option<String>,

    /// Number of spaces per indentation level
    #[arg(long, default_value_t = 4)]
    indent_width: usize,

    /// Maximum line width before breaking
    #[arg(long, default_value_t = 100)]
    max_width: usize,
}

fn main() {
    let args = Args::parse();

    let config = FormatterConfig {
        indent_width: args.indent_width,
        max_line_width: args.max_width,
    };

    let input = if let Some(path) = &args.file {
        std::fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("Error reading {path}: {e}");
            std::process::exit(1);
        })
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf).unwrap();
        buf
    };

    match args.mode {
        Mode::Lex => {
            let tokens = lexer::lex(&input);
            for tok in &tokens {
                eprintln!("{:?}", tok);
            }
        }
        Mode::Parse => {
            let tokens = lexer::lex(&input);
            let cst = syntax::parse(tokens);
            eprintln!("{:#?}", cst);
            let printed = syntax::print_lossless(&cst);
            print!("{}", printed);
        }
        Mode::Check => {
            let tokens = lexer::lex(&input);
            let cst = syntax::parse(tokens);
            let printed = syntax::print_lossless(&cst);
            if printed == input {
                eprintln!("✓ Roundtrip OK");
            } else {
                eprintln!("✗ Roundtrip FAILED");
                let input_bytes = input.as_bytes();
                let printed_bytes = printed.as_bytes();
                for (i, (a, b)) in input_bytes.iter().zip(printed_bytes.iter()).enumerate() {
                    if a != b {
                        let line = input[..i].chars().filter(|c| *c == '\n').count() + 1;
                        eprintln!(
                            "  First diff at byte {i} (line {line}): expected {:?}, got {:?}",
                            *a as char, *b as char
                        );
                        let ctx_start = i.saturating_sub(20);
                        let ctx_end = (i + 20).min(input.len()).min(printed.len());
                        eprintln!("  Input:   {:?}", &input[ctx_start..ctx_end]);
                        eprintln!("  Printed: {:?}", &printed[ctx_start..ctx_end]);
                        break;
                    }
                }
                if input.len() != printed.len() {
                    eprintln!(
                        "  Length mismatch: input={}, printed={}",
                        input.len(),
                        printed.len()
                    );
                }
                std::process::exit(1);
            }
        }
        Mode::Fmt => {
            let tokens = lexer::lex(&input);
            let formatted = formatter::format_with_config(&tokens, &config);
            print!("{}", formatted);
        }
    }
}
