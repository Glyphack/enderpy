<!-- markdownlint-configure-file {
  "MD033": false,
  "MD041": false
} -->
<div align="center">

<!-- <img src="" width="200" height="100"/> -->

# Enderpy

Enderpy is a code completion engine, static type checker & language server for python.

🏗️The project is under active development. There are breaking changes, and it's not considered ready to use unless you want to contribute to it and have fun.

</div>

## Why Do I Need It?

it provides developers with fast autocompletion and fast feedback loop when writing Python.

## Goals

[Ruff](https://github.com/charliermarsh/ruff) showed that there is a value
in providing faster implementation of static checkers.

This project aims to build the components needed to achieve the goal of
providing fast autocompletion and type checking, including:

- Python parser
- Python semantic analyzer & type checker
- Python language server protocol implementation
- CLI for parsing & analysis of Python programs

## Development Setup

Clone the repository

```
git submodule init
git submodule update
cargo build
```

The above test should run without any issues.

Then use the [editor client](#open-in-editor). Currently, supported editors are:

- [ ] neovim
- [x] vscode

## Contributing

If you are here then you want to write some rust code, let's get to it.
You can [ send me a message ](discordapp.com/users/glyphack) for discussions/help.

You can use [these learning](https://glyphack.com/blog/compiler-resources/) resources
to learn about each part of project.

The following will be a brief introduction into the project and how it works.
But it's always better to consult the code to see what exactly is going on.


### Open In Editor

1. Open vscode
2. Navigate to Run and Debug tab
3. Select "Launch Client" and run the app

By default the extension uses `enderpy-lsp` command to run the language server.
To change it set the `SERVER_PATH` env variable to custom executable.

### Core Concepts

The project consist of multiple crates each are separately published to crates.io:

- parser: parser and lexer
- typechecker: semantic analyzer and type checker
- enderpy: the CLI for interacting with lexer, parser, and type checker
- lsp: lsp for using with
- client: editor clients for using the language server

The cli and lsp are two ways to use this project. For example this is what happens when you use enderpy in an editor:

1. Editor connects to language server using the client extension
2. Language server receives text edit events like file save and send them for analysis for diagnosis
3. Typechecker uses parser to parse the code (code first goes through lexer and then parer) and get AST
4. Typechecker runs the semantic analysis pass and then runs the type checking path
5. Typechecker returns the errors to language server and language server sends them to editor
6. Editor shows the messages to user


### Tests

All the tests are done with [ insta.rs ](https://insta.rs/).

In each crate you find a folder called `test_data` which has inputs inside it.
These inputs are Python files that are used for testing that crate.
For example this is how the parser is tested againts the inputs:
https://github.com/Glyphack/enderpy/blob/4a6f49d88965b774a6780bffb9563a4a87da974a/parser/src/parser/parser.rs#L3695

When tests are run with insta they produce a snapshot that you can review,
and after saving those snapshots they will be the expected output of the tests.
Read the insta.rs documents to see how you can run and review tests.


### Parsing Phase

The parsing phase is about taking Python source code and turning it into AST.

The [lexer](https://github.com/Glyphack/enderpy/blob/4a6f49d88965b774a6780bffb9563a4a87da974a/parser/src/lexer/lexer.rs#L1)
is responsible for tokenizing the Python source code.

The [ parser ](https://github.com/Glyphack/enderpy/blob/4a6f49d88965b774a6780bffb9563a4a87da974a/parser/src/parser/parser.rs#L1)
uses the output of lexer to read the tokens and produce AST.

You can see the output of each of these steps using the CLI tool(use the help to find how to use it).

Also to compare the results to Python you can use the following Python modules:
- tokenizer: https://docs.python.org/3/library/tokenize.html
- ast: https://docs.python.org/3/library/ast.html#ast.Module

### Analysis Phase

Analysis phase starts with getting path to initial files to type check.
There's a struct called `Builder` which manages everything from getting the path to creating the diagnostics.

The `Builder` first resolves the imports in the initial files and import those for checking.
Then it will do a first pass on the files to bind the definitions and store them in symbol table.

Project uses visitor pattern to traverse the AST.

Python has a symbol table that you can use to see symbol table for a program. There's a script in `./scripts` folder.

After the symbol table is created bulider runs the typecheck for each file.

At the end builder populates errors in a list.

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

For LSP you need to have the `enderpy-lsp` program installed, and then install the extention for your editor.

LSP supports:

- Type checking & showing diagnostic messages in files

### Configuration

There are no configuration available currently. These are the default behavior of the program.

**Project Root**: The path that is considered the project root. This affects import resolving, and gathering the files to check.

**Python Executable**: The path to python executable. This is for resovling 3rd party dependencies.

## Inspired By

- [oxc](https://github.com/Boshen/oxc)
- [ruff](https://github.com/charliermarsh/ruff)
