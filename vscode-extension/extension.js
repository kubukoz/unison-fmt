const vscode = require("vscode");
const { execFileSync } = require("child_process");
const wasm = require("./pkg/unison_fmt.js");

function getConfig() {
  const config = vscode.workspace.getConfiguration("unison-format");
  return {
    cliPath: config.get("cliPath"),
    indentWidth: config.get("indentWidth") || 4,
    maxLineWidth: config.get("maxLineWidth") || 100,
  };
}

function formatWithCli(cliPath, input, indentWidth, maxLineWidth) {
  return execFileSync(
    cliPath,
    ["--indent-width", String(indentWidth), "--max-width", String(maxLineWidth), "fmt"],
    {
      input,
      encoding: "utf-8",
      timeout: 10000,
    }
  );
}

function formatWithWasm(input, indentWidth, maxLineWidth) {
  const options = new wasm.FormatOptions(indentWidth, maxLineWidth);
  try {
    return wasm.format_with_options(input, options);
  } finally {
    options.free();
  }
}

function formatDocument(document) {
  const input = document.getText();
  const config = getConfig();

  const formatted = config.cliPath
    ? formatWithCli(config.cliPath, input, config.indentWidth, config.maxLineWidth)
    : formatWithWasm(input, config.indentWidth, config.maxLineWidth);

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
