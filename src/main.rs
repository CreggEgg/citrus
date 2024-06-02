use std::fs;

mod ast;
fn main() {
    dbg!(ast::parser::parse(&fs::read_to_string("./test.ct").unwrap()).unwrap());
}
