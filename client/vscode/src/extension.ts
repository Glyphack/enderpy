import {
  ExtensionContext,
  window,
} from "vscode";

import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(_context: ExtensionContext) {
  const traceOutputChannel = window.createOutputChannel("Enderpy Language Server trace");
  const command = process.env.SERVER_PATH || "enderpy-lsp";
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
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "python" }],
    traceOutputChannel,
  };

  client = new LanguageClient("enderpy-language-server", "enderpy language server", serverOptions, clientOptions);
  console.log("server started 2")

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

