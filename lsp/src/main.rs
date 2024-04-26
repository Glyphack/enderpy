use std::path::PathBuf;

use enderpy_python_type_checker::{
    build::BuildManager, build_source::BuildSource, find_project_root, settings::Settings,
};
use env_logger::Builder;
use log::{info, LevelFilter};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    manager: BuildManager,
}

impl Backend {
    async fn on_change(&self, path: PathBuf) -> Vec<Diagnostic> {
        let source = match BuildSource::from_path(path.to_path_buf(), false) {
            Ok(source) => source,
            Err(err) => {
                panic!("cannot read the file: {:?}", err);
            }
        };

        let root = PathBuf::from(find_project_root(&path));

        self.manager.add_source(source);
        self.manager.build(&root);
        self.manager.type_check();
        let mut diagnostics = Vec::new();

        if let Some(errors) = self.manager.diagnostics.get(&path) {
            for err in errors.iter() {
                diagnostics.push(from(err.clone()));
            }
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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
            let diagnostics = self.on_change(path).await;
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
            let diagnostics = self.on_change(path).await;
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
            let diagnostics = self.on_change(path).await;
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
                let diagnostics = self.on_change(path).await;
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.client.log_message(MessageType::INFO, "hover!").await;
        let uri = params.text_document_position_params.text_document.uri;
        let Ok(path) = uri.to_file_path() else {
            return Ok(None);
        };

        let position = params.text_document_position_params.position;

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Hover position: line={}, character={}",
                    position.line, position.character
                ),
            )
            .await;

        // TODO: Implement real logic to find the symbol at the hover position
        // For now, let's provide a sample hover message with placeholder values
        let hover_message =
            self.manager
                .get_type_information(&path, position.line, position.character);

        let markup_content = MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**Hover Information**\n\n{}\n\n- Type: `<type>`\n- Documentation: `<documentation>`", hover_message),
        };
        let hover = Hover {
            contents: HoverContents::Markup(markup_content),
            range: None,
        };

        // Log the hover content for debugging
        self.client
            .log_message(
                MessageType::INFO,
                format!("Hover content:\n{:?}", hover.contents),
            )
            .await;

        return Ok(Some(hover));
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

    // TODO: This is a hack to get the typeshed path
    // If it's not found we want to clone it from the typeshed repo
    let exe_path = std::env::current_exe().unwrap();
    let typeshed_path = exe_path
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("typeshed");
    let settings = Settings { typeshed_path };
    let manager = BuildManager::new(vec![], settings);
    let (service, socket) = LspService::new(|client| Backend { client, manager });
    Server::new(stdin, stdout, socket).serve(service).await;
}
