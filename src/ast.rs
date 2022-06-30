use crate::lexer::Spanned;

#[derive(Clone, Debug)]
pub enum Expr {
    Error,
    Value(Value),
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
