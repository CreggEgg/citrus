pub mod parser;

#[derive(Debug)]
pub struct File {
    pub declarations: Vec<TopLevelDeclaration>,
}

#[derive(Debug)]
pub enum TopLevelDeclaration {
    Type(TypeDeclaration),
    Binding { lhs: String, rhs: UntypedExpr },
    Extern(AnnotatedIdent),
}

#[derive(Debug)]
pub enum TypeDeclaration {
    Struct(String, Vec<AnnotatedIdent>),
    Enum(String, Vec<String>),
    Alias(String, TypeName),
}
// #[derive(Debug, Copy, Clone)]
// enum Mode {
//     Global,
//     Local,
//     Exclave,
// }

#[derive(Debug, Clone)]
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
        local: bool,
        rhs: Box<UntypedExpr>,
    },
    IfElse {
        condition: Box<UntypedExpr>,
        then: Vec<UntypedExpr>,
        r#else: Vec<UntypedExpr>,
    },
    UnaryOp {
        op: UnaryOperator,
        target: Box<UntypedExpr>,
    },
    Mutate {
        lhs: String,
        rhs: Box<UntypedExpr>,
    },
}
#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Gt,
    Lt,
    Gte,
    Lte,
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Positive,
    Negative,
    Exclave,
}

#[derive(Debug, Clone)]
pub struct AnnotatedIdent {
    pub name: String,
    pub type_name: TypeName,
}
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    String(String),
    Function {
        args: Vec<AnnotatedIdent>,
        body: Vec<UntypedExpr>,
        ret_type: Option<TypeName>,
    },
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum TypeName {
    Named(String),
    Array(Box<TypeName>),
    Function(Vec<TypeName>, Box<TypeName>),
}
