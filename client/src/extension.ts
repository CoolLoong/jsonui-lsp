import {
  CompletionItem,
  CancellationToken,
  commands,
  CompletionItemProvider,
  CompletionList,
  ExtensionContext,
  Position,
  ProviderResult,
  Range,
  Selection,
  TextDocument,
  Uri,
  window,
  workspace,
  CompletionContext,
  languages,
  OutputChannel,
} from "vscode";
import {
  CompletionParams,
  CompletionRegistrationOptions,
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ProtocolRequestType,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;
// type a = Parameters<>;

export async function activate(context: ExtensionContext) {
  console.info("jsonui-support client start!");
  // backend path
  const command = process.env.SERVER_PATH || "jsonui-lsp";
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env
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
      settings: clientConfig,
    },
  };
  client = new LanguageClient(
    "jsonui-support",
    "jsonui-support-server",
    serverOptions,
    clientOptions
  );

  //push workspace/didChangeConfiguration to server
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
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
