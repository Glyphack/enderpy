use std::path::{Path, PathBuf};

use enderpy_python_type_checker::{build::BuildManager, find_project_root, settings::Settings};
use env_logger::Builder;
use log::LevelFilter;
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    manager: BuildManager,
}

impl Backend {
    fn build(&self, path: &Path) {
        let root = find_project_root(path);
        self.manager.build_one(root, path);
        self.manager.type_check(path);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, i: InitializeParams) -> Result<InitializeResult> {
        let root = match i.root_uri {
            Some(v) => v.to_file_path().unwrap_or(PathBuf::from("")),
            None => PathBuf::from(""),
        };
        self.manager.build(&root);
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
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
            self.build(&path);
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        let uri = params.text_document.uri;
        let path = uri.to_file_path();
        if let Ok(path) = path {
            self.build(&path);
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file changed!")
            .await;
        let uri = params.text_document.uri;
        let path = uri.to_file_path();
        if let Ok(path) = path {
            self.build(&path);
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
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
        let type_info =
            self.manager
                .get_hover_information(&path, position.line, position.character);

        let markup_content = MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**Hover Information**\n\n- Type: `{type_info}`\n",),
        };
        let hover = Hover {
            contents: HoverContents::Markup(markup_content),
            range: None,
        };

        return Ok(Some(hover));
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
    let settings = Settings::from_typeshed(typeshed_path);
    let manager = BuildManager::new(settings);
    let (service, socket) = LspService::new(|client| Backend { client, manager });
    Server::new(stdin, stdout, socket).serve(service).await;
}
