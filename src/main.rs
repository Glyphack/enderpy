use lexer::{Kind, Lexer};
use tracing::info;

mod lexer;

fn main() {
    tracing_subscriber::fmt::init();

    let lexer = Lexer::new("x = 1 + 2");
    read_all_tokens(lexer);
}

fn read_all_tokens(mut lexer: Lexer) {
    let mut token = lexer.read_next_token();
    while token.kind != Kind::Eof {
        info!("{:?}", token);
        token = lexer.read_next_token();
    }
}
