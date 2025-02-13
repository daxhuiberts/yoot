use std::io::Write;
use std::path::Path;

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

    #[arg(short, long)]
    output_file: Option<String>,

    filename: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let path = Path::new(&args.filename);

    let source: String = std::fs::read_to_string(path).unwrap();
    if args.print_all {
        println!("SOURCE:\n{source}");
    }

    let program = yoot::parse(&source)?;
    if args.print_all || args.print_ast {
        println!("PARSED PROGRAM: {program:#?}");
    }

    let print_typed = args.print_all || args.print_typed_ast;

    let typed_program = yoot::type_check(&program, print_typed);
    if print_typed {
        if let Ok(typed_program) = &typed_program {
            println!("TYPED PROGRAM: {typed_program:#?}");
        }
    }

    if !args.no_interpret {
        let result = yoot::interpret(&program);
        println!("INTERPRETER RESULT: {result:?}");
    }

    let typed_program = typed_program?;

    let module = yoot::to_wasm_module(&typed_program)?;
    println!("WASM MODULE:");
    yoot::print_module(&module);
    let result = yoot::compile_to_wasm(&module)?;
    yoot::print2(&result);

    let result2 = yoot::compile2(&typed_program)?;
    yoot::print2(&result2);

    if let Some(output_file) = args.output_file {
        std::fs::File::create(&output_file)?.write_all(&result)?;
    } else {
        yoot::run(&result)?;
        yoot::run(&result2)?;
    }

    Ok(())
}
