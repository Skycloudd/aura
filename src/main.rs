use crate::interpreter::{interpret, AuraValue};
use crate::lexer::lexer;
use crate::lexer::Span;
use crate::parser::parser;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use bigdecimal::ToPrimitive;
use chumsky::prelude::*;
use chumsky::Parser as _;
use chumsky::Stream;
use clap::Parser;
use std::{error, fs, process};

mod ast;
mod interpreter;
mod lexer;
mod parser;

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    filename: String,

    #[clap(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();

    match run(&args) {
        Ok(_) => {
            process::exit(0);
        }
        Err(e) => {
            eprintln!("aura: {}", e);
            process::exit(1);
        }
    }
}

fn run(args: &Args) -> Result<(), Box<dyn error::Error>> {
    let input = fs::read_to_string(&args.filename)?;

    let (tokens, mut errs) = lexer().parse_recovery(input.as_str());

    let parse_errs = if let Some(tokens) = tokens {
        if args.debug {
            dbg!(&tokens);
        }

        let len = input.chars().count();
        let (ast, parse_errs) =
            parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if args.debug {
            dbg!(&ast);
        }

        if let Some(ast) = ast {
            match interpret(&ast) {
                Ok(val) => {
                    if args.debug {
                        dbg!(&val);
                    }

                    match &val {
                        AuraValue::Int(n) => match n.to_i32() {
                            Some(n) => process::exit(n),
                            None => process::exit(1),
                        },
                        _ => process::exit(1),
                    }
                }
                Err(e) => errs.push(Simple::custom(e.span, e.msg)),
            }
        }

        parse_errs
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report.finish().eprint(Source::from(&input)).unwrap();
        });

    Ok(())
}

pub struct Error {
    pub span: Span,
    pub msg: String,
}
