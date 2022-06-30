use chumsky::prelude::*;
use std::{fmt, ops};

pub type Span = ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Op(String),
    Ctrl(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Num(n) => write!(f, "{}", n),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let num = text::int(10).map(Token::Num);

    let op = just("+")
        .or(just("-"))
        .or(just("*"))
        .or(just("/"))
        .or(just("%"))
        .map(|s| Token::Op(s.to_string()));

    let ctrl = one_of("()").map(|c| Token::Ctrl(c));

    let token = num.or(op).or(ctrl);

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}
