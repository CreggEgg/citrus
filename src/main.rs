use std::{
    ffi::OsStr,
    fs::{self, File},
    path::Path,
};

use clap::{command, Parser};

use crate::compiler::link;

mod ast;
mod compiler;
mod types;

fn main() {
    let args = Args::parse();
    let file = args.file.clone().map(|file| fs::read_to_string(file));
    match args.mode.as_str() {
        "build" => {
            // let ast = ast::parser::parse(file).unwrap();
            // let typed = types::inference::type_file(ast::File { declarations: ast }).unwrap();
            // compiler::compile("main".into(), typed.declarations).unwrap();
            let out = compiler::build_dir();
        }
        "run" => {
            let out = compiler::build_dir();
            let out = std::process::Command::new(out).output().unwrap();
            println!("{}", String::from_utf8_lossy(&out.stdout));
        }
        "parse" => {
            let ast = ast::parser::parse(
                &file
                    .expect("you must select a file to parse")
                    .expect("file does not exist"),
            )
            .unwrap();
            dbg!(ast);
        }
        "type" => {
            let ast = ast::parser::parse(
                &file
                    .expect("you must select a file to type")
                    .expect("file does not exist"),
            )
            .unwrap();
            dbg!(types::inference::type_file(ast::File { declarations: ast }).unwrap());
        }
        "init" => {
            let mut root_path = Path::new("./").canonicalize().unwrap();
            if let Some(path) = args.file {
                root_path.push(path);
                let _ = fs::create_dir(&root_path);
            };
            root_path.push("main.ct");
            dbg!(&root_path);
            let main = File::create(root_path.clone()).unwrap();
            fs::write(root_path, include_str!("../hello.ct")).unwrap();
        }
        mode => {
            eprintln!("Invalid mode: {}", mode);
        }
    }
}
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// What to do
    mode: String,
    /// File to operate on
    file: Option<String>,
}
