mod ast;
mod interpreter;
mod parser;
mod tokenizer;
mod type_checker;
mod typed_ast;
mod util;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let src: String = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("SOURCE:\n{src}");

    let program = parser::parse(&src)?;

    println!("PROGRAM: {program:#?}");

    let mut interpreter = interpreter::Interpreter::new(&program);
    let result = interpreter.execute();

    println!("INTERPRETER RESULT: {result:?}");

    let typed_program = type_checker::type_check(&program)?;

    println!("TYPED PROGRAM: {typed_program:#?}");

    Ok(())
}
