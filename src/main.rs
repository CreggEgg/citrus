use std::{fs, path::Path};

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
        "run" => {
            let ast = ast::parser::parse(file).unwrap();
            let typed = types::inference::type_file(ast::File { declarations: ast }).unwrap();
            compiler::compile(typed.declarations).unwrap();
            let mut path = Path::new("./").canonicalize().unwrap();
            path.push(Path::new("out/main"));
            let out = std::process::Command::new(path).output().unwrap();
            println!("{}", String::from_utf8_lossy(&out.stdout));
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
