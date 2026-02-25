use unison_fmt::formatter::{self, FormatConfig};
use unison_fmt::lexer;
use unison_fmt::syntax;

use std::io::Read;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut mode = "fmt";
    let mut file_path: Option<&str> = None;
    let mut config = FormatConfig::default();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--indent-width" => {
                i += 1;
                config.indent_width = args
                    .get(i)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or_else(|| {
                        eprintln!("--indent-width requires a positive integer");
                        std::process::exit(1);
                    });
            }
            "--max-line-width" => {
                i += 1;
                config.max_line_width = args
                    .get(i)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or_else(|| {
                        eprintln!("--max-line-width requires a positive integer");
                        std::process::exit(1);
                    });
            }
            arg if !arg.starts_with('-') => {
                if mode == "fmt" && file_path.is_none() {
                    // First positional arg is the mode (if it looks like one) or file
                    match arg {
                        "lex" | "parse" | "check" | "fmt" => mode = arg,
                        _ => file_path = Some(arg),
                    }
                } else if file_path.is_none() {
                    file_path = Some(arg);
                }
            }
            other => {
                eprintln!("Unknown option: {other}");
                eprintln!(
                    "Usage: unison-fmt [lex|parse|check|fmt] [file] [--indent-width N] [--max-line-width N]"
                );
                std::process::exit(1);
            }
        }
        i += 1;
    }

    let input = if let Some(path) = file_path {
        std::fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("Error reading {path}: {e}");
            std::process::exit(1);
        })
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf).unwrap();
        buf
    };

    match mode {
        "lex" => {
            let tokens = lexer::lex(&input);
            for tok in &tokens {
                eprintln!("{:?}", tok);
            }
        }
        "parse" => {
            let tokens = lexer::lex(&input);
            let cst = syntax::parse(tokens);
            eprintln!("{:#?}", cst);
            let printed = syntax::print_lossless(&cst);
            print!("{}", printed);
        }
        "check" => {
            let tokens = lexer::lex(&input);
            let cst = syntax::parse(tokens);
            let printed = syntax::print_lossless(&cst);
            if printed == input {
                eprintln!("✓ Roundtrip OK");
            } else {
                eprintln!("✗ Roundtrip FAILED");
                // Find first difference
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
        "fmt" => {
            let tokens = lexer::lex(&input);
            let formatted = formatter::format_with_config(&tokens, &config);
            print!("{}", formatted);
        }
        other => {
            eprintln!("Unknown mode: {other}");
            eprintln!(
                "Usage: unison-fmt [lex|parse|check|fmt] [file] [--indent-width N] [--max-line-width N]"
            );
            std::process::exit(1);
        }
    }
}
