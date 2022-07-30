use chumsky::prelude::*;
use std::{fmt, ops};

pub type Span = ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Int(String),
    Decimal(String),
    Op(String),
    Ctrl(char),

    Fn,
    Return,
    If,
    Else,
    True,
    False,
    Print,
    Null,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{}", s),
            Token::Int(n) => write!(f, "{}", n),
            Token::Decimal(n) => write!(f, "{}", n),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),

            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Print => write!(f, "print"),
            Token::Null => write!(f, "null"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let int = text::int(10).map(Token::Int);

    let decimal = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Token::Decimal);

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Fn,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::True,
        "false" => Token::False,
        "print" => Token::Print,
        "null" => Token::Null,
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
        .or(just("!"))
        .or(just("~"))
        .map(|s| Token::Op(s.to_string()));

    let ctrl = just('(')
        .or(just(')'))
        .or(just('{'))
        .or(just('}'))
        .or(just('['))
        .or(just(']'))
        .or(just(','))
        .or(just(';'))
        .map(Token::Ctrl);

    let token = decimal
        .or(int)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}
