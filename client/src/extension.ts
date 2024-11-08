import {
  ExtensionContext,
  workspace,
  env as vsenv,
} from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import * as path from 'path';

let client: LanguageClient;
export async function activate(context: ExtensionContext) {
  console.info("jsonui-support client start!");
  // backend path
  const command = process.env.SERVER_PATH || path.join(context.extensionPath, 'dist', 'jsonui_lsp');
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_BACKTRACE: 1
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  let clientConfig: any = JSON.parse(
    JSON.stringify(workspace.getConfiguration("jsonui-support")),
  );
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "json" }],
    initializationOptions: {
      locale: vsenv.language,
      settings: clientConfig,
    },
  };
  client = new LanguageClient(
    "jsonui-support",
    "jsonui-support-server",
    serverOptions,
    clientOptions
  );

  workspace.onDidChangeConfiguration((e) => {
    let isAffected = e.affectsConfiguration("jsonui-support");
    if (!isAffected) {
      return;
    }
    let settings: any = JSON.parse(
      JSON.stringify(workspace.getConfiguration("jsonui-support")),
    );
    client.sendNotification("workspace/didChangeConfiguration", { settings });
  });
  await client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
