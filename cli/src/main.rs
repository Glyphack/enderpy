use parser::Parser;

fn main() {
    let result = Parser::new("1 + 2".to_string()).parse();
    println!("{:?}", result);
}
