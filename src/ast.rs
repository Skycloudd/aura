use crate::lexer::Spanned;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<FunctionArg>>,
    pub body: Vec<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
pub struct FunctionArg {
    pub name: Spanned<String>,
    pub argtype: Spanned<String>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Error,
    Block(Vec<Spanned<Statement>>),
    Expr(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
    Let(Spanned<String>, Spanned<Expr>),
    Queue(Spanned<Expr>),
    If(Spanned<Expr>, Box<Spanned<Statement>>),
    Else(Box<Spanned<Statement>>),
    IfElse(Box<Spanned<Statement>>, Box<Option<Spanned<Statement>>>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Error,
    Value(Value),
    Var(String),
    Unary(Spanned<UnaryOp>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, Spanned<BinaryOp>, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
}
