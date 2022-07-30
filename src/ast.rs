use crate::lexer::Spanned;
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::fmt;

pub type Ast = Spanned<Vec<Spanned<Function>>>;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<String>,
    pub args: Spanned<Vec<Spanned<FunctionArg>>>,
    pub body: Spanned<Statement>,
}

#[derive(Clone, Debug)]
pub struct FunctionArg {
    pub name: Spanned<String>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Error,
    Block(Vec<Spanned<Statement>>),
    Expr(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
    Print(Spanned<Expr>),
    If(Spanned<Expr>, Box<Spanned<Statement>>),
    Else(Box<Spanned<Statement>>),
    IfElse(Box<Spanned<Statement>>, Box<Option<Spanned<Statement>>>),
}

impl Statement {
    pub fn get_type_str(&self) -> &'static str {
        match self {
            Statement::Error => "error",
            Statement::Block(_) => "block",
            Statement::Expr(_) => "expression",
            Statement::Return(_) => "return statement",
            Statement::Print(_) => "print statement",
            Statement::If(_, _) => "if statement",
            Statement::Else(_) => "else statement",
            Statement::IfElse(_, _) => "if else statement",
        }
    }
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

impl Expr {
    pub fn get_type_str(&self) -> &'static str {
        match self {
            Expr::Error => "error",
            Expr::Value(_) => "value",
            Expr::Var(_) => "variable",
            Expr::Unary(_, _) => "unary expression",
            Expr::Binary(_, _, _) => "binary expression",
            Expr::Call(_, _) => "function call",
        }
    }
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
    Assign,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(BigInt),
    Decimal(BigDecimal),
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{}", x),
            Self::Decimal(x) => write!(f, "{}", x),
            Self::Bool(x) => write!(f, "{}", x),
        }
    }
}
