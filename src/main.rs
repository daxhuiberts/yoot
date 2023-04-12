mod ast;
mod interpreter;
mod parser;
mod tokenizer;
mod type_checker;
mod typed_ast;
mod util;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source: String = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    println!("SOURCE:\n{source}");

    let program = parser::parse(&source)?;
    println!("PARSED PROGRAM: {program:#?}");

    let result = interpreter::execute(&program);
    println!("INTERPRETER RESULT: {result:?}");

    let typed_program = type_checker::check(&program)?;
    println!("TYPED PROGRAM: {typed_program:#?}");

    Ok(())
}
