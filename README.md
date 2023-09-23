<!-- markdownlint-configure-file {
  "MD033": false,
  "MD041": false
} -->
<div align="center">

<!-- <img src="" width="200" height="100"/> -->

# Enderpy

Enderpy is a code completion engine, static type checker & language server for python.

🏗️The project is under active development. There are breaking changes and it's not consider ready to use unless you want to contribute to it and have fun.

</div>

## Why Do I Need It?

it provides developers with faster autocompletion and fast feedback loop when writing Python.

## Goals

The primary goal of this project is providing instant feedback as you type.
[Ruff](https://github.com/charliermarsh/ruff) showed that there is a value
in providing faster implementation of static checkers.
That's why I'm continuing this path.

This project aims to build the components needed to achieve the goal of
providing fast autocompletion and type checking, including:

- Python parser
- Python semantic analyzer & type checker
- Python language server protocol implementation

## Installation

```
cargo install enderpy_lsp
```

Then install the editor client. Currently supported editors are:

- [ ] neovim
- [x] vscode


## Building From Source

For building Rust components all you need to do is:

```
cargo run enderpy tokenize
```

For using the language client like VS VS Code, you can open the project in VS Code and use the `Run & Debug` tab to run the extention from source.
For more information read:
<https://code.visualstudio.com/api/language-extensions/language-server-extension-guide>

## Usage

The project can be used in two ways, first as a CLI tool that can check your python projects, and also as a LSP inside an editor.

### CLI

The following commands are available:

```bash
Usage: enderpy <COMMAND>

Commands:
  tokenize  Print lexer tokens
  parse     Print abstract syntax tree
  check     Type check
  symbols   Prints Symbol table
  help      Print this message or the help of the given subcommand(s)
```

### LSP

For LSP you need to have the `enderpy_lsp` program installed, and then install the extention for your editor.

LSP supports:

- Type checking & showing diagnostic messages in files

### Configuration

There are no configuration available currently. These are the default behavior of the program.

**Project Root**: The path that is considered the project root. This affects import resolving, and gathering the files to check.
**Python Executable**: The path to python executable. This is for resovling 3rd party dependencies.

## Contributing

Contributions are very much appropriated. Take a look at our open issues, to get started.
Or [ send me a message ](discordapp.com/users/glyphack) for discussions/help.

You can use [these learning](https://glyphack.com/blog/compiler-resources/) resources 
to learn about each component.

## Inspired By

- [oxc](https://github.com/Boshen/oxc)
- [ruff](https://github.com/charliermarsh/ruff)
