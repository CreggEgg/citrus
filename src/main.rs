#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
use std::fs;

mod ast;
mod compiler;
fn main() {
    let file = &fs::read_to_string("./test.ct").unwrap();
    match ast::parser::parse(file) {
        Ok(ast) => {
            dbg!(ast);
        }
        Err(error) => {
            eprintln!("{:?}", error);
        }
    };
}
