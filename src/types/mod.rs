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
    Struct(TypeName, Vec<AnnotatedIdent>),
    Enum(TypeName, Vec<String>),
    Alias(TypeName, TypeName),
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
    IfElse {
        condition: Box<UntypedExpr>,
        then: Vec<UntypedExpr>,
        r#else: Vec<UntypedExpr>,
    },
    UnaryOp {
        op: UnaryOperator,
        target: Box<UntypedExpr>,
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
    Gt,
    Lt,
    Gte,
    Lte,
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
    String(String),
    Function {
        args: Vec<AnnotatedIdent>,
        body: Vec<UntypedExpr>,
        ret_type: Option<TypeName>,
    },
}

pub type TypeName = String;
