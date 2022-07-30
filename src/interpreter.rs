use crate::ast;
use crate::lexer::Spanned;
use crate::Error;
use bigdecimal::BigDecimal;
use num_bigint::{BigInt, ToBigInt};
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub enum AuraValue {
    Int(BigInt),
    Decimal(BigDecimal),
    Bool(bool),
    Null,
}

impl AuraValue {
    pub fn get_type_str(&self) -> &'static str {
        match self {
            AuraValue::Int(_) => "int",
            AuraValue::Decimal(_) => "decimal",
            AuraValue::Bool(_) => "bool",
            AuraValue::Null => "null",
        }
    }
}

impl fmt::Display for AuraValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AuraValue::Int(i) => write!(f, "{}", i),
            AuraValue::Decimal(d) => write!(f, "{}", d),
            AuraValue::Bool(b) => write!(f, "{}", b),
            AuraValue::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, Debug)]
struct Environment {
    functions: HashMap<String, Spanned<ast::Function>>,
    variables: HashMap<String, AuraValue>,
}

impl Environment {
    pub fn new(ast: &ast::Ast, variables: HashMap<String, AuraValue>) -> Self {
        let functions: HashMap<String, Spanned<ast::Function>> = ast
            .0
            .iter()
            .map(|f| (f.0.name.0.clone(), f.clone()))
            .collect();

        Self {
            functions,
            variables,
        }
    }

    pub fn get_function(&self, name: &str) -> Option<&Spanned<ast::Function>> {
        self.functions.get(name)
    }

    pub fn get_variable(&self, name: &str) -> Option<&AuraValue> {
        self.variables.get(name)
    }

    pub fn set_variable(&mut self, name: &str, value: AuraValue) {
        self.variables.insert(name.to_string(), value);
    }
}

pub fn interpret(ast: &ast::Ast) -> Result<AuraValue, Error> {
    let variables: HashMap<String, AuraValue> = HashMap::new();

    let environment = Environment::new(ast, variables);

    eval(environment, ast)
}

fn eval(environment: Environment, ast: &ast::Ast) -> Result<AuraValue, Error> {
    if let Some(main_func) = environment.get_function("main") {
        let mut env = Environment {
            functions: environment.functions.clone(),
            variables: environment.variables.clone(),
        };

        eval_function(&mut env, main_func, vec![])
    } else {
        Err(Error {
            span: ast.1.clone(),
            msg: "No main function found".to_string(),
        })
    }
}

fn eval_function(
    environment: &mut Environment,
    func: &Spanned<ast::Function>,
    with_args: Vec<AuraValue>,
) -> Result<AuraValue, Error> {
    if func.0.args.0.len() != with_args.len() {
        return Err(Error {
            span: func.1.clone(),
            msg: "Function was called with wrong number of arguments".to_string(),
        });
    }

    for (arg, value) in func.0.args.0.iter().zip(with_args) {
        environment.set_variable(&arg.0.name.0, value);
    }

    match eval_statement(environment, &func.0.body) {
        Ok(Some(value)) => Ok(value),
        Ok(None) => Ok(AuraValue::Null),
        Err(e) => Err(e),
    }
}

fn eval_statement(
    environment: &mut Environment,
    stmt: &Spanned<ast::Statement>,
) -> Result<Option<AuraValue>, Error> {
    match stmt.0.clone() {
        ast::Statement::Error => unreachable!("tried evaluating a Statement::Error"),
        ast::Statement::Block(statements) => {
            let mut env = Environment {
                functions: environment.functions.clone(),
                variables: environment.variables.clone(),
            };

            for statement in &statements {
                match eval_statement(&mut env, statement)? {
                    Some(v) => return Ok(Some(v)),
                    None => continue,
                }
            }

            Ok(None)
        }
        ast::Statement::Expr(expr) => {
            eval_expression(environment, &expr)?;
            Ok(None)
        }
        ast::Statement::Return(expr) => match expr {
            Some(expr) => {
                let value = eval_expression(environment, &expr)?;
                Ok(Some(value))
            }
            None => Ok(None),
        },
        ast::Statement::Print(expr) => {
            let value = eval_expression(environment, &expr)?;
            println!("{}", value);
            Ok(None)
        }
        ast::Statement::IfElse(if_else_stmt) => {
            let condition = eval_expression(environment, &if_else_stmt.if_stmt.0 .0)?;

            let mut env = Environment {
                functions: environment.functions.clone(),
                variables: environment.variables.clone(),
            };

            match condition {
                AuraValue::Bool(true) => eval_statement(&mut env, &if_else_stmt.if_stmt.0 .1),
                AuraValue::Bool(false) => match &if_else_stmt.else_stmt {
                    Some((s, _)) => eval_statement(&mut env, s),
                    None => Ok(None),
                },
                _ => Err(Error {
                    span: if_else_stmt.if_stmt.0 .0 .1.clone(),
                    msg: "Condition must be a boolean".to_string(),
                }),
            }
        }
        ast::Statement::While(_while_stmt) => {
            todo!()
        }
    }
}

fn eval_expression(
    environment: &mut Environment,
    expr: &Spanned<ast::Expr>,
) -> Result<AuraValue, Error> {
    match expr.0.clone() {
        ast::Expr::Error => unreachable!("tried evaluating an Expr::Error"),
        ast::Expr::Value(v) => match v {
            ast::Value::Int(i) => Ok(AuraValue::Int(i)),
            ast::Value::Decimal(d) => Ok(AuraValue::Decimal(d)),
            ast::Value::Bool(b) => Ok(AuraValue::Bool(b)),
            ast::Value::Null => Ok(AuraValue::Null),
        },
        ast::Expr::Var(v) => {
            let var = environment.get_variable(&v).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable '{}' is not defined here", &v),
            })?;

            Ok(var.clone())
        }
        ast::Expr::Unary(op, e) => match op.0 {
            ast::UnaryOp::Neg => match eval_expression(environment, e.as_ref())? {
                AuraValue::Int(i) => Ok(AuraValue::Int(-i)),
                AuraValue::Decimal(d) => Ok(AuraValue::Decimal(-d)),
                v => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Unary operator '-' cannot be applied to {}",
                        v.get_type_str()
                    ),
                }),
            },
            ast::UnaryOp::Pos => match eval_expression(environment, e.as_ref())? {
                AuraValue::Int(i) => Ok(AuraValue::Int(i)),
                AuraValue::Decimal(d) => Ok(AuraValue::Decimal(d)),
                v => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Unary operator '+' cannot be applied to {}",
                        v.get_type_str()
                    ),
                }),
            },
            ast::UnaryOp::Not => match eval_expression(environment, e.as_ref())? {
                AuraValue::Bool(b) => Ok(AuraValue::Bool(!b)),
                v => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Unary operator '!' cannot be applied to {}",
                        v.get_type_str()
                    ),
                }),
            },
            ast::UnaryOp::Truncate => match eval_expression(environment, e.as_ref())? {
                AuraValue::Int(i) => Ok(AuraValue::Int(i)),
                AuraValue::Decimal(d) => Ok(AuraValue::Int(d.to_bigint().unwrap())),
                v => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Unary operator '~' cannot be applied to {}",
                        v.get_type_str()
                    ),
                }),
            },
        },
        ast::Expr::Binary(lhs, op, rhs) => match op.0 {
            ast::BinaryOp::Add => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l + r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l + r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '+' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Sub => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l - r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l - r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '-' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Mul => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l * r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l * r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '*' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Div => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l / r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l / r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '/' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Mod => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Int(l % r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Decimal(l % r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '%' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Eq => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l == r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l == r)),
                (AuraValue::Bool(l), AuraValue::Bool(r)) => Ok(AuraValue::Bool(l == r)),
                (AuraValue::Null, AuraValue::Null) => Ok(AuraValue::Bool(true)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '==' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Neq => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l != r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l != r)),
                (AuraValue::Bool(l), AuraValue::Bool(r)) => Ok(AuraValue::Bool(l != r)),
                (AuraValue::Null, AuraValue::Null) => Ok(AuraValue::Bool(false)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '!=' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Lt => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l < r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l < r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '<' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Lte => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l <= r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l <= r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '<=' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Gt => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l > r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l > r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '>' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Gte => match (
                &eval_expression(environment, lhs.as_ref())?,
                &eval_expression(environment, rhs.as_ref())?,
            ) {
                (AuraValue::Int(l), AuraValue::Int(r)) => Ok(AuraValue::Bool(l >= r)),
                (AuraValue::Decimal(l), AuraValue::Decimal(r)) => Ok(AuraValue::Bool(l >= r)),
                (lhs_value, rhs_value) => Err(Error {
                    span: expr.1.clone(),
                    msg: format!(
                        "Binary operator '>=' cannot be applied to {} and {}",
                        lhs_value.get_type_str(),
                        rhs_value.get_type_str()
                    ),
                }),
            },
            ast::BinaryOp::Assign => match &lhs.as_ref().0 {
                ast::Expr::Var(ident) => {
                    let value = eval_expression(environment, rhs.as_ref())?;
                    environment.set_variable(ident, value.clone());
                    Ok(value)
                }
                e => Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot assign to '{}'", e.get_type_str()),
                }),
            },
        },
        ast::Expr::Call(e, params) => match &e.as_ref().0 {
            ast::Expr::Var(ident) => {
                let func = environment.get_function(ident).ok_or_else(|| Error {
                    span: expr.1.clone(),
                    msg: format!("Function '{}' not found", ident),
                })?;

                if func.0.args.0.len() != params.0.len() {
                    return Err(Error {
                        span: expr.1.clone(),
                        msg: format!(
                            "Function '{}' expects {} parameters, but {} were given",
                            ident,
                            func.0.args.0.len(),
                            params.0.len()
                        ),
                    });
                }

                let mut with_args = vec![];
                for param in params.0.iter() {
                    let value = eval_expression(&mut environment.clone(), param)?;
                    with_args.push(value);
                }

                let mut env = Environment {
                    functions: environment.functions.clone(),
                    variables: HashMap::new(),
                };

                eval_function(&mut env, func, with_args)
            }
            e => Err(Error {
                span: expr.1.clone(),
                msg: format!("Cannot call '{}'", e.get_type_str()),
            }),
        },
    }
}
