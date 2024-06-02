use logos::Logos;

use self::lexer::Token;

mod lexer;
pub mod parser;

#[derive(Debug)]
pub struct File {
    declarations: Vec<TopLevelDeclaration>,
}

#[derive(Debug)]
pub enum TopLevelDeclaration {
    Type(TypeDeclaration),
    Binding { lhs: String, rhs: UntypedExpr },
}

#[derive(Debug)]
pub enum TypeDeclaration {
    Struct(Vec<AnnotatedIdent>),
    Enum(Vec<String>),
    Alias(TypeName),
}

#[derive(Debug)]
pub enum UntypedExpr {
    BinaryOp {
        lhs: Box<UntypedExpr>,
        op: BinaryOperator,
        rhs: Box<UntypedExpr>,
    },
    Literal(Literal),
    Ident(String),
    FunctionCall(String, Vec<UntypedExpr>),
    Binding {
        lhs: String,
        rhs: Box<UntypedExpr>,
    },
}
#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Semicolon,
}
#[derive(Debug)]
pub enum UnaryOperator {
    Positive,
    Negative,
    Await,
}

#[derive(Debug)]
pub struct AnnotatedIdent {
    name: String,
    type_name: TypeName,
}
#[derive(Debug)]
pub enum Literal {
    Int(i32),
    Function {
        args: Vec<AnnotatedIdent>,
        body: Box<UntypedExpr>,
        ret_type: Option<TypeName>,
    },
}

pub type TypeName = String;
