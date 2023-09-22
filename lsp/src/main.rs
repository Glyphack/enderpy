use std::path::PathBuf;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use typechecker::build::BuildManager;
use typechecker::settings::{ImportDiscovery, Settings};

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    #[allow(unused_variables)]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        let uri = params.text_document.uri;
        // check if the uri is path buf or just return
        let path = uri.to_file_path().unwrap_or_default();
        let file_name = path
            .file_name()
            .unwrap_or_default()
            .to_str()
            .unwrap_or_default();
        let file_name = file_name.to_string();
        let file_content = params.text_document.text;
        let settings = Settings {
            debug: true,
            root: path.clone(),
            import_discovery: ImportDiscovery {
                python_executable: None,
            },
        };
        let mut build_manager = BuildManager::new(vec![], settings);
        build_manager.add_source(&path);
        build_manager.build();
        let errs = build_manager.get_errors();
        for err in errs {
            self.client
                .publish_diagnostics(
                    uri.clone(),
                    vec![Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 0,
                            },
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("typechecker".to_string()),
                        message: err,
                        related_information: None,
                        tags: None,
                        data: None,
                    }],
                    None,
                )
                .await;
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let build_manager = Box::new(BuildManager::new(vec![], Settings::test_settings()));
    let (service, socket) = LspService::new(|client| Backend {
        client,
        build_manager,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
