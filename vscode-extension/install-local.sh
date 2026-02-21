#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
wasm-pack build --target nodejs --out-dir vscode-extension/pkg
cd vscode-extension
npx @vscode/vsce package -o unison-fmt.vsix
code --install-extension unison-fmt.vsix --force
echo "✓ Extension installed"
