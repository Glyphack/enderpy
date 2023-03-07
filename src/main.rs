use lexer::Lexer;
use token::Kind;
use tracing::info;

mod lexer;
mod parser;
mod token;

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
