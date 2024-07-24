use std::collections::HashMap;

use crate::ast::{BinaryOperator, UnaryOperator};

pub mod inference;

#[derive(Debug)]
pub struct TypedFile {
    pub declarations: Vec<TypedTopLevelDeclaration>,
}

#[derive(Debug)]
pub enum TypedTopLevelDeclaration {
    Binding { lhs: String, rhs: TypedExpr },
    Extern(crate::types::AnnotatedIdent),
}

// #[derive(Debug)]
// pub enum TypeDeclaration {
//     Struct(TypeName, Vec<AnnotatedIdent>),
//     Enum(TypeName, Vec<String>),
//     Alias(TypeName, TypeName),
// }

#[derive(Debug, Clone)]
pub enum TypedExpr {
    BinaryOp {
        r#type: Type,
        lhs: Box<TypedExpr>,
        op: BinaryOperator,
        rhs: Box<TypedExpr>,
    },
    Literal(Type, TypedLiteral),
    Ident(Type, String),
    FunctionCall(Type, String, Vec<TypedExpr>),
    Binding {
        r#type: Type,
        lhs: String,
        rhs: Box<TypedExpr>,
        local: bool,
    },
    IfElse {
        r#type: Type,
        condition: Box<TypedExpr>,
        then: Vec<TypedExpr>,
        r#else: Vec<TypedExpr>,
    },
    UnaryOp {
        r#type: Type,
        op: UnaryOperator,
        target: Box<TypedExpr>,
    },
    Mutate {
        r#type: Type,
        lhs: String,
        rhs: Box<TypedExpr>,
    },
    Access(Type, Box<TypedExpr>, i32),
}

#[derive(Debug, Clone)]
pub enum TypedLiteral {
    Int(i32),
    String(String),
    Function {
        args: Vec<AnnotatedIdent>,
        body: Vec<TypedExpr>,
        ret_type: Option<Type>,
    },
    Bool(bool),
    Array(Vec<TypedExpr>),
    Struct(Type, Vec<(String, TypedExpr)>),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AnnotatedIdent {
    pub name: String,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,
    Enum(Vec<String>),
    Array(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Struct(HashMap<String, Type>),
}
