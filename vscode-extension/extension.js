const vscode = require("vscode");
const { execFileSync } = require("child_process");
const wasm = require("./pkg/unison_fmt.js");

function formatWithCli(cliPath, input, indentWidth, maxLineWidth) {
  return execFileSync(
    cliPath,
    ["fmt", "--indent-width", String(indentWidth), "--max-line-width", String(maxLineWidth)],
    {
      input,
      encoding: "utf-8",
      timeout: 10000,
    }
  );
}

function formatDocument(document) {
  const input = document.getText();
  const config = vscode.workspace.getConfiguration("unison-format");
  const cliPath = config.get("cliPath");
  const indentWidth = config.get("indentWidth");
  const maxLineWidth = config.get("maxLineWidth");

  const formatted = cliPath
    ? formatWithCli(cliPath, input, indentWidth, maxLineWidth)
    : wasm.format_with_options(input, indentWidth, maxLineWidth);

  if (formatted === input) {
    return [];
  }

  const fullRange = new vscode.Range(
    document.positionAt(0),
    document.positionAt(input.length)
  );
  return [vscode.TextEdit.replace(fullRange, formatted)];
}

function activate(context) {
  const formatter = vscode.languages.registerDocumentFormattingEditProvider(
    { language: "unison", scheme: "file" },
    { provideDocumentFormattingEdits: formatDocument }
  );

  context.subscriptions.push(formatter);
}

function deactivate() {}

module.exports = { activate, deactivate };
