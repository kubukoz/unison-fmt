use unison_fmt::formatter;
use unison_fmt::lexer;
use unison_fmt::syntax;

use std::io::Read;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mode = args.get(1).map(|s| s.as_str()).unwrap_or("fmt");

    let input = if let Some(path) = args.get(2) {
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
            let formatted = formatter::format(&tokens);
            print!("{}", formatted);
        }
        other => {
            eprintln!("Unknown mode: {other}");
            eprintln!("Usage: unison-fmt [lex|parse|check|fmt] [file]");
            std::process::exit(1);
        }
    }
}
