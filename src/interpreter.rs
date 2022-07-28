use crate::ast::Ast;
use crate::ast::Function;
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

struct EvalContext {
    pub functions: HashMap<String, Spanned<Function>>,
}

impl EvalContext {
    pub fn new(ast: &Ast) -> Self {
        let functions: HashMap<String, Spanned<Function>> = ast
            .0
            .iter()
            .map(|f| (f.0.name.0.clone(), f.clone()))
            .collect();

        EvalContext { functions }
    }
}

pub fn interpret(ast: &Ast) -> Result<AuraValue, Vec<Error>> {
    let ctx = EvalContext::new(ast);

    eval(&ctx, ast)
}

fn eval(ctx: &EvalContext, ast: &Ast) -> Result<AuraValue, Vec<Error>> {
    if let Some(main_func) = ctx.functions.get("main") {
        eval_function(ctx, ast, main_func)
    } else {
        Err(vec![Error {
            span: ast.1.clone(),
            msg: "No main function found".to_string(),
        }])
    }
}

fn eval_function(
    ctx: &EvalContext,
    ast: &Ast,
    func: &Spanned<Function>,
) -> Result<AuraValue, Vec<Error>> {
    Ok(AuraValue::Int(BigInt::from(0)))
}
