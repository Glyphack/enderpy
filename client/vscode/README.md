# vscode-enderpy

This is a VS Code extension for the `enderpy` static code analyzer/language server for Python, written in Rust.

It is entirely experimental and not ready for testing yet.

## Installation

To install the extension, you must first build the `enderpy` language server.

```console
cd ./lsp
cargo install --path .
```

Then, you can install the extension by running the following command:

```console
// if you don't have pnpm installed, you can install it by running `npm install -g pnpm`
// you also need to have vsce installed, you can install it by running `npm install -g vsce`
cd ./client/vscode
pnpm i
pnpm run build
pnpm run package
```

This script will generate a `.vsix` file in the `client/vscode` directory. You can install this file in VS Code by running the `Extensions: Install from VSIX...` command in the command palette.

You should run the `Hello World` command in the command palette to verify that you installed the extension correctly. You'll see a notification with `Hello, World!`.

After that, you can open a Python file and watch outputs (shortcut `Ctrl+Shift+U` or `Cmd+Shift+U`) to see the language server's stdout.
