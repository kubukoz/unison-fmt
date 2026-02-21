#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
wasm-pack build --target nodejs --out-dir vscode-extension/pkg
echo "✓ VS Code extension built. WASM at vscode-extension/pkg/"
