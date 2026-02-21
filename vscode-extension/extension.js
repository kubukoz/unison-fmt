const vscode = require("vscode");
const { execFileSync } = require("child_process");
const wasm = require("./pkg/unison_fmt.js");

function formatWithCli(cliPath, input) {
  return execFileSync(cliPath, ["fmt"], {
    input,
    encoding: "utf-8",
    timeout: 10000,
  });
}

function formatDocument(document) {
  const input = document.getText();
  const cliPath = vscode.workspace
    .getConfiguration("unison-format")
    .get("cliPath");

  const formatted =
    cliPath ? formatWithCli(cliPath, input) : wasm.format(input);

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
