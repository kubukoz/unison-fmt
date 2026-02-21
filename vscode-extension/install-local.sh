#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
wasm-pack build --target nodejs --out-dir vscode-extension/pkg
cd vscode-extension
VERSION=$(git describe --tags --abbrev=0 2>/dev/null | sed 's/^v//' || echo "0.0.0")
npx @vscode/vsce package "$VERSION" -o unison-fmt.vsix
code --install-extension unison-fmt.vsix --force
echo "✓ Extension installed (v$VERSION)"
