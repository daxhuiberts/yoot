use self::interpreter::Interpreter;
use self::parser::parser;
use chumsky::Parser;
use std::error::Error;

mod ast;
mod interpreter;
mod parser;
mod tokenizer;
mod type_checker;
mod typed_ast;
mod util;

fn main() -> Result<(), Box<dyn Error>> {
    let src: String = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("SOURCE:\n{src}");

    let parser = parser();
    let program = parser.parse(src).map_err(|err| format!("{err:?}"))?;

    println!("PROGRAM: {program:#?}");

    let mut interpreter = Interpreter::new(&program);
    let result = interpreter.execute();

    println!("RESULT: {result:?}");

    let typed_program = type_checker::type_check(&program);

    println!("TYPED PROGRAM: {typed_program:#?}");

    Ok(())
}
