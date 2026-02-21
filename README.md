# unison-fmt

A formatter for [Unison](https://www.unison-lang.org/) source files, built on a lossless parser that preserves all syntax information (whitespace, comments, indentation).

## What it does

- **Normalizes indentation** to 4 spaces
- **Collapses multiple spaces** to single spaces
- **Strips trailing whitespace**
- **Breaks long lines** at `|>` pipeline operators

### Example

Before:

<!-- `$ cat examples/before.u` as unison -->

```unison
handler = Handler cases
        req| Routes.get (root Path./ "hello") req ->
                parseName =
                                        do
                                                        req
                                                            |> HttpRequest.uri |> URI.query
                                                            |> fromRawQuery
        catchWith (e -> ok (Body (toUtf8 ("error: " ++ e)))) happyPath
```

After:

<!-- `$ cat examples/before.u | cargo run` as unison -->

```unison
handler = Handler cases
    req| Routes.get (root Path./ "hello") req ->
        parseName =
            do
                req
                    |> HttpRequest.uri |> URI.query
                    |> fromRawQuery
    catchWith (e -> ok (Body (toUtf8 ("error: " ++ e)))) happyPath
```

## VS Code extension

Install **Unison Formatter** (`kubukoz.unison-format`) from the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=kubukoz.unison-format).

Since the official Unison extension also provides formatting, you need to set this extension as the default formatter. Add to your `settings.json`:

```json
{
  "[unison]": {
    "editor.defaultFormatter": "kubukoz.unison-format"
  }
}
```

Then format `.u` files with the standard **Format Document** command (`Shift+Alt+F` / `Shift+Option+F`).

To also format on save, add `editor.formatOnSave`:

```json
{
  "[unison]": {
    "editor.defaultFormatter": "kubukoz.unison-format",
    "editor.formatOnSave": true
  }
}
```

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

