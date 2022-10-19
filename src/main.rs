use self::parser::parser;
use chumsky::Parser;
use std::error::Error;

mod ast;
mod interpreter;
mod parser;

fn main() -> Result<(), Box<dyn Error>> {
    let src: String = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let parser = parser();
    let ast = parser.parse(src).map_err(|err| format!("{err:?}"))?;

    println!("AST: {ast:?}");

    let result = interpreter::eval(&ast);

    println!("RESULT: {result:?}");

    Ok(())
}
