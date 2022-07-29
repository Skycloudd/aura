use crate::ast;
use crate::lexer::Spanned;
use crate::Error;
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::collections::HashMap;

#[derive(Debug)]
pub enum AuraValue {
    Int(BigInt),
    Decimal(BigDecimal),
    Bool(bool),
}

struct Environment {
    pub functions: HashMap<String, Spanned<ast::Function>>,
}

impl Environment {
    pub fn new(ast: &ast::Ast) -> Self {
        let functions: HashMap<String, Spanned<ast::Function>> = ast
            .0
            .iter()
            .map(|f| (f.0.name.0.clone(), f.clone()))
            .collect();

        Self { functions }
    }
}

pub fn interpret(ast: &ast::Ast) -> Result<AuraValue, Error> {
    let environment = Environment::new(ast);

    eval(&environment, ast)
}

fn eval(environment: &Environment, ast: &ast::Ast) -> Result<AuraValue, Error> {
    if let Some(main_func) = environment.functions.get("main") {
        eval_function(environment, ast, main_func)
    } else {
        Err(Error {
            span: ast.1.clone(),
            msg: "No main function found".to_string(),
        })
    }
}

fn eval_function(
    environment: &Environment,
    ast: &ast::Ast,
    func: &Spanned<ast::Function>,
) -> Result<AuraValue, Error> {
    todo!()
}

fn eval_statement(
    environment: &Environment,
    ast: &ast::Ast,
    stmt: &Spanned<ast::Expr>,
) -> Result<AuraValue, Error> {
    todo!()
}

fn eval_expression(
    environment: &Environment,
    ast: &ast::Ast,
    expr: &Spanned<ast::Expr>,
) -> Result<AuraValue, Error> {
    match expr.0.clone() {
        ast::Expr::Error => unreachable!("tried evaluating an Expr::Error"),
        ast::Expr::Value(v) => match v {
            ast::Value::Int(i) => Ok(AuraValue::Int(i)),
            ast::Value::Decimal(d) => Ok(AuraValue::Decimal(d)),
            ast::Value::Bool(b) => Ok(AuraValue::Bool(b)),
        },
        ast::Expr::Var(v) => {
            todo!()
        }
        ast::Expr::Unary(op, e) => {
            let e = eval_expression(environment, ast, e.as_ref())?;

            match op.0 {
                ast::UnaryOp::Neg => match e {
                    AuraValue::Int(i) => Ok(AuraValue::Int(-i)),
                    AuraValue::Decimal(d) => Ok(AuraValue::Decimal(-d)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Unary operator '-' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::UnaryOp::Pos => match e {
                    AuraValue::Int(i) => Ok(AuraValue::Int(i)),
                    AuraValue::Decimal(d) => Ok(AuraValue::Decimal(d)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Unary operator '+' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
            }
        }
        ast::Expr::Binary(lhs, op, rhs) => {
            let lhs = eval_expression(environment, ast, lhs.as_ref())?;
            let rhs = eval_expression(environment, ast, rhs.as_ref())?;

            match op.0 {
                ast::BinaryOp::Add => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l + r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l + r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '+' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Sub => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l - r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l - r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '-' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Mul => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l * r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l * r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '*' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Div => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l / r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l / r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '/' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Mod => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l % r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l % r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '%' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Eq => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l == r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l == r)),
                    (AuraValue::Bool(l), AuraValue::Bool(r)) => Ok(AuraValue::Bool(l == r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '==' can only be applied to integers, decimals, or booleans"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Neq => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l != r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l != r)),
                    (AuraValue::Bool(l), AuraValue::Bool(r)) => Ok(AuraValue::Bool(l != r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '!=' can only be applied to integers, decimals, or booleans"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Lt => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l < r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l < r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '<' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Lte => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l <= r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l <= r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '<=' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Gt => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l > r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l > r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '>' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Gte => match (lhs, rhs) {
                    (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l >= r)),
                    (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l >= r)),
                    _ => Err(Error {
                        span: expr.1.clone(),
                        msg: "Binary operator '>=' can only be applied to integers or decimals"
                            .to_string(),
                    }),
                },
                ast::BinaryOp::Assign => {
                        todo!()
                }
            }
        }
        ast::Expr::Call(e, params) => {
            todo!()
        }
    }
}
