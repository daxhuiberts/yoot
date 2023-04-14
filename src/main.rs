fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source: String = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    println!("SOURCE:\n{source}");

    let program = yoot::parse(&source)?;
    println!("PARSED PROGRAM: {program:#?}");

    let result = yoot::interpret(&program);
    println!("INTERPRETER RESULT: {result:?}");

    let typed_program = yoot::type_check(&program)?;
    println!("TYPED PROGRAM: {typed_program:#?}");

    // Only prints 'hello world' wasm program, not the provided program.
    let result = yoot::compile_to_wasm(&typed_program);
    println!("COMPILE RESULT: {result:#?}");

    Ok(())
}
