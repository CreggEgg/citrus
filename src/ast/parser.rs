use lalrpop_util::lalrpop_mod;

use super::TopLevelDeclaration;

// #[derive(Debug)]
// pub enum ParseError {
//     InvalidToken(std::ops::Range<usize>, Vec<Token>),
//     UnexpectedEOF(std::ops::Range<usize>, Vec<Token>),
// }
//
lalrpop_mod!(
    #[allow(unused)]
    #[allow(clippy::all)]
    grammar
);

pub fn parse(file: &str) -> Result<Vec<TopLevelDeclaration>, ()> {
    Ok(grammar::FileParser::new().parse(file).unwrap())
}
