use logos::Logos;
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("type")]
    Type,
    #[token("let")]
    Let,
    #[regex("[a-zA-Z_]+", |lex| lex.slice().to_owned())]
    Ident(String),
    #[token("=")]
    Equal,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token(";")]
    Semicolon,
    #[token("fn")]
    Function,
    #[token("(")]
    LBracket,
    #[token(")")]
    RBracket,
    #[token("\"")]
    Quote,
    #[regex("[0-9]+", |lex| lex.slice().to_owned())]
    Number(String),
    #[token("@")]
    Await,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[regex("[\n\t ]")]
    Whitespace,
}
pub fn lex(file: String) -> Result<Vec<Token>, ()> {
    Token::lexer(&file)
        .filter(|x| {
            if let Ok(Token::Whitespace) = x {
                false // Python shaking in its boots
            } else {
                true
            }
        })
        .collect::<Result<Vec<Token>, ()>>()
}
