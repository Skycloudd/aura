use chumsky::prelude::*;
use std::{fmt, ops};

pub type Span = ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Num(String),
    Op(String),
    Ctrl(char),

    Fn,
    Return,
    Let,
    Queue,
    If,
    Else,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{}", s),
            Token::Num(n) => write!(f, "{}", n),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),

            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::Let => write!(f, "let"),
            Token::Queue => write!(f, "queue"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let num = text::int(10).map(Token::Num);

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Fn,
        "return" => Token::Return,
        "let" => Token::Let,
        "queue" => Token::Queue,
        "if" => Token::If,
        "else" => Token::Else,
        _ => Token::Ident(ident),
    });

    let op = just("==")
        .or(just("!="))
        .or(just("<="))
        .or(just(">="))
        .or(just("+"))
        .or(just("-"))
        .or(just("*"))
        .or(just("/"))
        .or(just("%"))
        .or(just("<"))
        .or(just(">"))
        .or(just("="))
        .map(|s| Token::Op(s.to_string()));

    let ctrl = just('(')
        .or(just(')'))
        .or(just('{'))
        .or(just('}'))
        .or(just(','))
        .or(just(';'))
        .or(just(':'))
        .map(|c| Token::Ctrl(c));

    let token = num
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}
