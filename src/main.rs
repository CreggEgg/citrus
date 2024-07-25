use std::{
    ffi::OsStr,
    fs::{self, File},
    path::Path,
};

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
            let out_folder = Path::new("./out");
            let _ = fs::create_dir(out_folder);
            let tmp_folder = Path::new("./tmp");
            let _ = fs::create_dir(tmp_folder);

            let mut core_path = tmp_folder.canonicalize().unwrap();
            core_path.push("core.c");
            fs::write(core_path, include_str!("../core.c")).unwrap();

            let citrus_files = fs::read_dir("./")
                .unwrap()
                .into_iter()
                .filter_map(|file| {
                    if let Ok(file) = file {
                        if file.path().is_file()
                            && file
                                .path()
                                .extension()
                                .map(|ex| ex == "ct")
                                .unwrap_or(false)
                        {
                            // && file.path().ends_with(".ct") {
                            Some(file.path())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                // .map(|file| file.unwrap().path())
                .collect::<Vec<_>>();

            for file in citrus_files {
                println!("{}.o", file.to_str().unwrap());
                let mut obj_path = tmp_folder.canonicalize().unwrap();
                obj_path.push(format!("{}.o", file.to_str().unwrap()));
                let out = File::create(obj_path).unwrap();
                let ast = ast::parser::parse(&fs::read_to_string(file).unwrap()).unwrap();
                let typed = types::inference::type_file(ast::File { declarations: ast }).unwrap();
                compiler::compile(
                    file.file_stem().unwrap().to_str().unwrap().to_string(),
                    typed.declarations,
                )
                .unwrap()
                .write_stream(out)
                .unwrap();
            }

            #[cfg(debug_assertions)]
            dbg!(Path::new("./").canonicalize().unwrap());
            // fs::remove_dir_all("./tmp").unwrap();

            let mut path = Path::new("./").canonicalize().unwrap();
            path.push(Path::new("out/main"));
            // let out = std::process::Command::new(path).output().unwrap();
            // println!("{}", String::from_utf8_lossy(&out.stdout));
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
