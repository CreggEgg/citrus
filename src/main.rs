#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
use std::fs;

use clap::{command, Parser};

mod ast;
mod compiler;
mod types;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// What to do
    mode: String,

    /// File to operate on
    file: String,
}

fn main() {
    let args = Args::parse();
    let file = &fs::read_to_string(args.file).unwrap();
    match args.mode.as_str() {
        "build" => {
            let ast = ast::parser::parse(file).unwrap();
            let typed = types::inference::type_file(ast::File { declarations: ast }).unwrap();
            compiler::compile(typed.declarations).unwrap();
        }
        "parse" => {
            let ast = ast::parser::parse(file).unwrap();
            dbg!(ast);
        }
        "type" => {
            let ast = ast::parser::parse(file).unwrap();
            dbg!(types::inference::type_file(ast::File { declarations: ast }).unwrap());
        }
        mode => {
            eprintln!("Invalid mode: {}", mode);
        }
    }
}
