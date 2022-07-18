use crate::ast::Function;
use crate::lexer::Spanned;
use crate::Span;

pub struct Error {
    pub span: Span,
    pub msg: String,
}

pub fn compile(ast: &Vec<Spanned<Function>>) -> Result<(), Vec<Error>> {
    Ok(())
}
