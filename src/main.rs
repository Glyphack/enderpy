mod lexer;
mod parser;
mod token;

use parser::parser::Parser;

fn main() {
    tracing_subscriber::fmt::init();

    Parser::new("1 + 2".to_string()).parse();
}
