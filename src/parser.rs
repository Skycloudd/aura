use crate::interpreter::Error;
use crate::lexer::Span;
use crate::lexer::{Spanned, Token};
use chumsky::prelude::*;
use std::fmt;

#[derive(Debug)]
pub enum Expr {
    Error,
    Value(Value),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug)]
pub enum Value {
    Num(u32),
}

impl Value {
    pub fn num(self, span: Span) -> Result<u32, Error> {
        // TODO: remove this when it wont do a warning anymore
        #[allow(irrefutable_let_patterns)]
        if let Value::Num(x) = self {
            Ok(x)
        } else {
            Err(Error {
                span,
                msg: format!("'{}' is not a number", self),
            })
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Num(x) => write!(f, "{}", x),
        }
    }
}

pub fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let val = select! {
            Token::Num(n) => Expr::Value(Value::Num(n.parse().unwrap())),
        }
        .labelled("value");

        // 'Atoms' are expressions that contain no ambiguity
        let atom = val
            .map_with_span(|expr, span| (expr, span))
            // Atoms can also just be normal expressions, but surrounded with parentheses
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [],
                |span| (Expr::Error, span),
            ));

        // Product ops (multiply and divide) have equal precedence
        let op = just(Token::Op("*".to_string()))
            .to(BinaryOp::Mul)
            .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));

        let product =
            atom.clone()
                .then(op.then(atom).repeated())
                .foldl(|a: Spanned<Expr>, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

        // Sum ops (add and subtract) have equal precedence
        let op = just(Token::Op("+".to_string()))
            .to(BinaryOp::Add)
            .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub));
        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            });

        sum
    })
}
