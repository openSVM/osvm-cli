import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
    // Get the path to the LSP server
    const config = vscode.workspace.getConfiguration('ovsm');
    const serverPath = config.get<string>('lsp.path', 'ovsm-lsp');

    // Server options - run the LSP server executable
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio,
            options: {
                env: { ...process.env, RUST_LOG: 'debug' }
            }
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        // Register for OVSM documents
        documentSelector: [{ scheme: 'file', language: 'ovsm' }],
        synchronize: {
            // Notify server about file changes in workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.ovsm')
        },
        outputChannelName: 'OVSM Language Server',
        traceOutputChannel: vscode.window.createOutputChannel('OVSM LSP Trace')
    };

    // Create the language client
    client = new LanguageClient(
        'ovsm',
        'OVSM Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (also launches the server)
    client.start();

    // Register commands
    context.subscriptions.push(
        vscode.commands.registerCommand('ovsm.restartServer', async () => {
            if (client) {
                await client.stop();
                await client.start();
                vscode.window.showInformationMessage('OVSM Language Server restarted');
            }
        })
    );

    vscode.window.showInformationMessage('OVSM extension activated');
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
