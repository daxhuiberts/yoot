use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    #[arg(long)]
    print_tokens: bool,

    #[arg(long)]
    print_ast: bool,

    #[arg(long)]
    print_typed_ast: bool,

    #[arg(long)]
    print_all: bool,

    #[arg(long)]
    no_interpret: bool,

    filename: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let source: String = std::fs::read_to_string(args.filename).unwrap();
    if args.print_all {
        println!("SOURCE:\n{source}");
    }

    let program = yoot::parse(&source)?;
    if args.print_all || args.print_ast {
        println!("PARSED PROGRAM: {program:#?}");
    }

    let typed_program = yoot::type_check(&program);
    if args.print_all || args.print_typed_ast {
        if let Ok(typed_program) = &typed_program {
            println!("TYPED PROGRAM: {typed_program:#?}");
        }
    }

    if !args.no_interpret {
        let result = yoot::interpret(&program);
        println!("INTERPRETER RESULT: {result:?}");
    }

    if let Err(error) = typed_program {
        println!("TYPED_PROGRAM ERROR: {error}")
    }

    // Only prints 'hello world' wasm program, not the provided program.
    // let result = yoot::compile_to_wasm(&typed_program);
    // println!("COMPILE RESULT: {result:#?}");

    Ok(())
}
