use crate::ast;
use crate::lexer::Spanned;
use crate::Error;
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::collections::HashMap;

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
        eval_function(&environment, main_func, vec![])
    } else {
        Err(Error {
            span: ast.1.clone(),
            msg: "No main function found".to_string(),
        })
    }
}

fn eval_function(
    environment: &Environment,
    func: &Spanned<ast::Function>,
    with_args: Vec<AuraValue>,
) -> Result<AuraValue, Error> {
    if func.0.args.0.len() != with_args.len() {
        return Err(Error {
            span: func.1.clone(),
            msg: "Function was called with wrong number of arguments".to_string(),
        });
    }

    let mut env = Environment {
        functions: environment.functions.clone(),
        variables: HashMap::new(),
    };

    for (arg, value) in func.0.args.0.iter().zip(with_args) {
        env.set_variable(&arg.0.name.0, value);
    }

    match eval_statement(&env, &func.0.body) {
        Ok(value) => match value {
            Some(v) => Ok(v),
            None => Ok(AuraValue::Null),
        },
        Err(e) => Err(e),
    }
}

fn eval_statement(
    environment: &Environment,
    stmt: &Spanned<ast::Statement>,
) -> Result<Option<AuraValue>, Error> {
    match stmt.0.clone() {
        ast::Statement::Error => unreachable!("tried evaluating a Statement::Error"),
        ast::Statement::Block(statements) => {
            let env = Environment {
                functions: environment.functions.clone(),
                variables: environment.variables.clone(),
            };

            for statement in &statements {
                eval_statement(&env, statement)?;
            }

            Ok(None)
        }
        ast::Statement::Expr(expr) => {
            eval_expression(&environment, &expr)?;
            Ok(None)
        }
        ast::Statement::Return(expr) => {
            todo!()
        }
        ast::Statement::Print(expr) => {
            todo!()
        }
        ast::Statement::If(condition, stmt) => {
            todo!()
        }
        ast::Statement::Else(stmt) => {
            todo!()
        }
        ast::Statement::IfElse(if_stmt, else_stmt) => {
            todo!()
        }
    }
}

fn eval_expression(
    environment: &Environment,
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
            let var = environment.get_variable(&v).ok_or_else(|| Error {
                span: expr.1.clone(),
                msg: format!("Variable {} is not defined", &v),
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
                    todo!()
                }
                e => Err(Error {
                    span: expr.1.clone(),
                    msg: format!("Cannot assign to {}", e.get_type_str()),
                }),
            },
        },
        ast::Expr::Call(e, params) => match &e.as_ref().0 {
            ast::Expr::Var(ident) => {
                let func = &environment.get_function(&ident).ok_or_else(|| Error {
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
                    let value = eval_expression(&environment, param)?;
                    with_args.push(value);
                }

                eval_function(&environment, func, with_args)
            }
            e => Err(Error {
                span: expr.1.clone(),
                msg: format!("Cannot call {}", e.get_type_str()),
            }),
        },
    }
}
