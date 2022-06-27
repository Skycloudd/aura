use crate::lexer::{Span, Spanned};
use crate::parser::{BinaryOp, Expr, Value};

pub struct Error {
    pub span: Span,
    pub msg: String,
}

pub fn eval_expr(expr: &Spanned<Expr>) -> Result<Value, Error> {
    Ok(match &expr.0 {
        Expr::Error => unreachable!(),
        Expr::Value(val) => val.clone(),
        Expr::Binary(a, op, b) => match op {
            BinaryOp::Add => {
                Value::Num(eval_expr(a)?.num(a.1.clone())? + eval_expr(b)?.num(b.1.clone())?)
            }
            BinaryOp::Sub => {
                Value::Num(eval_expr(a)?.num(a.1.clone())? - eval_expr(b)?.num(b.1.clone())?)
            }
            BinaryOp::Mul => {
                Value::Num(eval_expr(a)?.num(a.1.clone())? * eval_expr(b)?.num(b.1.clone())?)
            }
            BinaryOp::Div => {
                Value::Num(eval_expr(a)?.num(a.1.clone())? / eval_expr(b)?.num(b.1.clone())?)
            }
        },
    })
}
