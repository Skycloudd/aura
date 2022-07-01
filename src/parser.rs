use crate::ast::{BinaryOp, Expr, Function, FunctionArg, Statement, UnaryOp, Value};
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

pub fn parser() -> impl Parser<Token, Vec<Spanned<Function>>, Error = Simple<Token>> + Clone {
    let function = function_parser();

    function.repeated().then_ignore(end())
}

pub fn function_parser() -> impl Parser<Token, Spanned<Function>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let functionarg = ident
        .clone()
        .map_with_span(|name, span| (name, span))
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident.clone().map_with_span(|name, span| (name, span)))
        .map_with_span(|(name, argtype), span| (FunctionArg { name, argtype }, span));

    let functionargs = functionarg
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    let body = statement_parser().repeated();

    let function = just(Token::Fn)
        .ignore_then(
            ident
                .map_with_span(|ident, span| (ident, span))
                .labelled("function name"),
        )
        .then(functionargs)
        .then(
            body.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                .recover_with(nested_delimiters(
                    Token::Ctrl('{'),
                    Token::Ctrl('}'),
                    [(Token::Ctrl('('), Token::Ctrl(')'))],
                    |span| vec![(Statement::Error, span)],
                )),
        )
        .map_with_span(|((name, args), body), span| (Function { name, args, body }, span));

    function
}

pub fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let val = select! {
            Token::Num(n) => Expr::Value(Value::Int(n.parse().unwrap())),
        }
        .labelled("value");

        let ident = select! { Token::Ident(ident) => ident.clone() }.labelled("identifier");

        let atom = val
            .map_with_span(|expr, span| (expr, span))
            .or(ident.map_with_span(|name, span| (Expr::Var(name), span)))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .recover_with(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [(Token::Ctrl('{'), Token::Ctrl('}'))],
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
}

pub fn statement_parser() -> impl Parser<Token, Spanned<Statement>, Error = Simple<Token>> + Clone {
    let ident = select! { Token::Ident(ident) => ident.clone() }
        .map_with_span(|ident, span| (ident, span))
        .labelled("identifier");
    let expression = expr_parser().labelled("expression");

    let expr = expression
        .clone()
        .then_ignore(just(Token::Ctrl(';')))
        .map_with_span(|expr, span| (Statement::Expr(expr), span));

    let return_ = just(Token::Return)
        .ignore_then(expression.clone())
        .map_with_span(|expr, span| (Statement::Return(expr), span))
        .then_ignore(just(Token::Ctrl(';')))
        .map_with_span(|(stmt, span), _| (stmt, span));

    let let_ = just(Token::Let)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Op("=".to_string())))
        .then(expression.clone())
        .map_with_span(|(name, expr), span| (Statement::Let(name, expr), span))
        .then_ignore(just(Token::Ctrl(';')))
        .map_with_span(|(stmt, span), _| (stmt, span));

    let statement = expr.or(return_).or(let_);

    statement
}
