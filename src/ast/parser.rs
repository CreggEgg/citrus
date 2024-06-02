use logos::{Lexer, Logos};

use super::{lexer::Token, File, TopLevelDeclaration, UntypedExpr};

#[derive(Debug)]
pub enum ParseError {
    InvalidToken(std::ops::Range<usize>, Vec<Token>),
}

pub fn parse(file: &str) -> Result<File, ParseError> {
    let lexer = Token::lexer(file);
    let mut parser = Parser::new(lexer);
    parser.parse()
}

struct Parser<'a> {
    lexer: Lexer<'a, Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a, Token>) -> Self {
        Self { lexer }
    }

    fn parse(&mut self) -> Result<File, ParseError> {
        let mut declarations = Vec::new();
        loop {
            let declaration = self.parse_declaration();
            match declaration {
                Ok(Some(declaration)) => {
                    declarations.push(declaration);
                }
                Ok(None) => break,
                Err(error) => return Err(error),
            }
        }
        Ok(File { declarations })
    }

    fn parse_declaration(&mut self) -> Result<Option<super::TopLevelDeclaration>, ParseError> {
        if let Some(token) = self.lexer.next() {
            match token {
                Ok(Token::Type) => self.parse_type_declaration(),
                Ok(Token::Let) => self.parse_binding(),
                Err(_) | Ok(_) => {
                    return Err(ParseError::InvalidToken(
                        self.lexer.span(),
                        vec![Token::Type, Token::Let],
                    ));
                }
            }
        } else {
            return Ok(None);
        }
    }

    //Assumes type token has already been consumed (i.e. <consumed: type><remaining: foo = bar;>)
    fn parse_type_declaration(&self) -> Result<Option<super::TopLevelDeclaration>, ParseError> {
        todo!()
    }

    //Assumes let token has already been consumed (i.e. <consumed: let><remaining: foo = bar;>)
    fn parse_binding(&mut self) -> Result<Option<super::TopLevelDeclaration>, ParseError> {
        let ident = self.parse_ident()?;
        self.require_next(Token::Equal)?;
        let rhs = self.parse_expr()?;
        self.require_next(Token::Semicolon)?;
        Ok(Some(TopLevelDeclaration::Binding { lhs: ident, rhs }))
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        let next = self.lexer.next();
        if let Some(Ok(Token::Ident(ident))) = next {
            return Ok(ident);
        }
        return Err(ParseError::InvalidToken(
            self.lexer.span(),
            vec![Token::Ident("".to_string())],
        ));
    }

    fn parse_expr(&self) -> Result<UntypedExpr, ParseError> {
        todo!()
    }

    fn require_next(&mut self, required: Token) -> Result<(), ParseError> {
        let next = self.lexer.next();
        return if Some(Ok(required.clone())) == next {
            Ok(())
        } else {
            Err(ParseError::InvalidToken(self.lexer.span(), vec![required]))
        };
    }
}
