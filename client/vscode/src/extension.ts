import { ExtensionContext, window, commands } from "vscode";

import { Executable, LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  const disposable = commands.registerCommand("helloworld.helloWorld", () => {
    window.showInformationMessage("Hello World!");
  });
  context.subscriptions.push(disposable);
  const traceOutputChannel = window.createOutputChannel("Enderpy Language Server trace");
  const command = process.env.SERVER_PATH || "enderpy-lsp";
  console.log(`Running Lsp ${command}`);
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: "debug",
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "python" }],
    traceOutputChannel,
  };

  client = new LanguageClient("enderpy-language-server", "enderpy language server", serverOptions, clientOptions);

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
