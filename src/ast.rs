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
    Expr(Spanned<Expr>),
    Return(Spanned<Expr>),
    Let(Spanned<String>, Spanned<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Error,
    Value(Value),
    Var(String),
    Unary(Spanned<UnaryOp>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, Spanned<BinaryOp>, Box<Spanned<Self>>),
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
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
}
