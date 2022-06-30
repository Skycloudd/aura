use crate::ast::{BinaryOp, Expr, UnaryOp, Value};
use crate::lexer::{Spanned, Token};
use crate::Span;
use chumsky::prelude::*;
use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{}", x),
        }
    }
}

pub fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let val = select! {
            Token::Num(n) => Expr::Value(Value::Int(n.parse().unwrap())),
        }
        .labelled("value");

        let atom = val
            .map_with_span(|expr, span| (expr, span))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .recover_with(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [],
                |span| (Expr::Error, span),
            ));

        let op = just(Token::Op("-".to_string()))
            .to(UnaryOp::Neg)
            .or(just(Token::Op("+".to_string())).to(UnaryOp::Pos))
            .map_with_span(|op, span: Span| (op, span));

        let unary = op.repeated().then(atom).foldr(|op, a: Spanned<Expr>| {
            let span = op.1.start..a.1.end;
            (Expr::Unary(op, Box::new(a)), span)
        });

        let op = just(Token::Op("*".to_string()))
            .to(BinaryOp::Mul)
            .or(just(Token::Op("/".to_string())).to(BinaryOp::Div))
            .or(just(Token::Op("%".to_string())).to(BinaryOp::Mod))
            .map_with_span(|op, span| (op, span));

        let product =
            unary
                .clone()
                .then(op.then(unary).repeated())
                .foldl(|a: Spanned<Expr>, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

        let op = just(Token::Op("+".to_string()))
            .to(BinaryOp::Add)
            .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub))
            .map_with_span(|op, span: Span| (op, span));

        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            });

        sum
    })
    .then_ignore(end())
}
