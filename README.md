# unison-fmt

A vibe coded formatter for Unison files. No guarantees!

<!-- omit in toc -->
## Table of contents

- [unison-fmt](#unison-fmt)
  - [VS Code extension](#vs-code-extension)
  - [CLI](#cli)
    - [Download](#download)
    - [Build from source](#build-from-source)
    - [Usage](#usage)


A formatter for [Unison](https://www.unison-lang.org/) source files, built on a lossless parser that preserves all syntax information (whitespace, comments, indentation).

## VS Code extension

Install **Unison Formatter** (`kubukoz.unison-format`) from the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=kubukoz.unison-format), then format `.u` files with the standard **Format Document** command (`Shift+Alt+F` / `Shift+Option+F`).

The extension runs the formatter as WebAssembly — no external binary needed.

## CLI

### Download

Grab a prebuilt binary from the [latest release](https://github.com/kubukoz/unison-fmt/releases/latest):

| Platform                      | Binary                          |
| ----------------------------- | ------------------------------- |
| Linux x86_64                  | `unison-fmt-x86_64-linux`       |
| Linux aarch64                 | `unison-fmt-aarch64-linux`      |
| macOS x86_64                  | `unison-fmt-x86_64-macos`       |
| macOS aarch64 (Apple Silicon) | `unison-fmt-aarch64-macos`      |
| Windows x86_64                | `unison-fmt-x86_64-windows.exe` |

```sh
# Example: download and install on macOS Apple Silicon
curl -L -o unison-fmt https://github.com/kubukoz/unison-fmt/releases/latest/download/unison-fmt-aarch64-macos
chmod +x unison-fmt
sudo mv unison-fmt /usr/local/bin/
```

### Build from source

```sh
cargo install --path .
```

### Usage

```sh
# Format a file (prints to stdout)
unison-fmt fmt myfile.u

# Format from stdin
cat myfile.u | unison-fmt fmt

# Verify lossless roundtrip (exits non-zero on failure)
unison-fmt check myfile.u

# Dump the token stream (for debugging)
unison-fmt lex myfile.u

# Dump the CST and print lossless output (for debugging)
unison-fmt parse myfile.u
```

