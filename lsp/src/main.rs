use std::path::PathBuf;

use env_logger::Builder;
use log::LevelFilter;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use enderpy_python_type_checker::build::BuildManager;
use enderpy_python_type_checker::project::find_project_root;
use enderpy_python_type_checker::settings::{ImportDiscovery, Settings};

#[derive(Debug)]
struct Backend {
    client: Client,
}

impl Backend {
    async fn check_file(&self, path: &PathBuf) -> Vec<Diagnostic> {
        let root = PathBuf::from(find_project_root(path));
        let python_executable = None;
        let settings = Settings {
            debug: false,
            root,
            import_discovery: ImportDiscovery { python_executable },
        };

        let mut manager = BuildManager::new(vec![], settings);
        manager.add_source(path);
        manager.type_check();
        let errors = manager.get_errors();
        let mut diagnostics = Vec::new();
        for err in errors {
            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: err.line,
                        character: err.start,
                    },
                    end: Position {
                        line: err.line,
                        character: err.end,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("Enderpy".to_string()),
                message: String::from(err.msg),
                related_information: None,
                tags: None,
                data: None,
            });
        }
        diagnostics
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("typechecker".to_string()),
                        inter_file_dependencies: true,
                        workspace_diagnostics: true,
                        work_done_progress_options: Default::default(),
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        log::info!("server initialized!");
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
        let uri = params.text_document.uri;
        let path = uri.to_file_path();
        if let Ok(path) = path {
            let diagnostics = self.check_file(&path).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        let uri = params.text_document.uri;
        let path = uri.to_file_path();
        if let Ok(path) = path {
            let diagnostics = self.check_file(&path).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let mut builder = Builder::from_default_env();

    builder.filter(None, LevelFilter::Info).init();

    log::info!("starting enderpy language server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}