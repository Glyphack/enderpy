use std::path::PathBuf;

use env_logger::Builder;
use log::{info, LevelFilter};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use enderpy_python_type_checker::build::BuildManager;
use enderpy_python_type_checker::build_source::BuildSource;
use enderpy_python_type_checker::project::find_project_root;
use enderpy_python_type_checker::settings::{ImportDiscovery, Settings};

#[derive(Debug)]
struct Backend {
    client: Client,
}

impl Backend {
    async fn check_file(&self, path: PathBuf) -> Vec<Diagnostic> {
        let root = PathBuf::from(find_project_root(path.as_path()));
        let python_executable = None;
        let settings = Settings {
            debug: false,
            root,
            import_discovery: ImportDiscovery { python_executable },
            follow_imports: enderpy_python_type_checker::settings::FollowImports::Skip,
        };

        let mut manager =
            BuildManager::new(vec![BuildSource::from_path(path.clone(), false)], settings);
        manager.type_check();
        let mut diagnostics = Vec::new();
        info!("path: {:?}", path);
        match manager.get_state(path) {
            Some(state) => {
                for err in state.diagnostics.iter() {
                    diagnostics.push(from(err.clone()));
                }
            }
            None => {}
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
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("typechecker".to_string()),
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
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
            let diagnostics = self.check_file(path).await;
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
            let diagnostics = self.check_file(path).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file changed!")
            .await;
        let uri = params.text_document.uri;
        let path = uri.to_file_path();
        if let Ok(path) = path {
            let diagnostics = self.check_file(path).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        self.client
            .log_message(MessageType::INFO, "diagnostic!")
            .await;
        let uri = params.text_document.uri;
        let path = uri.to_file_path();

        info!("diagnostic: {:?}", path);
        match path {
            Ok(path) => {
                let diagnostics = self.check_file(path).await;
                info!("diagnostics: {:?}", diagnostics);
                Ok(DocumentDiagnosticReportResult::Report(
                    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                        related_documents: None,
                        full_document_diagnostic_report: FullDocumentDiagnosticReport {
                            result_id: None,
                            items: diagnostics,
                        },
                    }),
                ))
            }
            Err(_) => Ok(DocumentDiagnosticReportResult::Report(
                DocumentDiagnosticReport::Unchanged(RelatedUnchangedDocumentDiagnosticReport {
                    related_documents: None,
                    unchanged_document_diagnostic_report: UnchangedDocumentDiagnosticReport {
                        result_id: "typechecker".to_string(),
                    },
                }),
            )),
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn from(diagnostic: enderpy_python_type_checker::diagnostic::Diagnostic) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: diagnostic.range.start.line,
                character: diagnostic.range.start.character,
            },
            end: Position {
                line: diagnostic.range.end.line,
                character: diagnostic.range.end.character,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("Enderpy".to_string()),
        message: diagnostic.body,
        related_information: None,
        tags: None,
        data: None,
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
