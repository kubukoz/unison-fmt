const vscode = require("vscode");
const wasm = require("./pkg/unison_fmt.js");

function activate(context) {
  const formatter = vscode.languages.registerDocumentFormattingEditProvider(
    { language: "unison", scheme: "file" },
    {
      provideDocumentFormattingEdits(document) {
        const input = document.getText();
        const formatted = wasm.format(input);

        if (formatted === input) {
          return [];
        }

        const fullRange = new vscode.Range(
          document.positionAt(0),
          document.positionAt(input.length),
        );
        return [vscode.TextEdit.replace(fullRange, formatted)];
      },
    },
  );

  context.subscriptions.push(formatter);
}

function deactivate() {}

module.exports = { activate, deactivate };
