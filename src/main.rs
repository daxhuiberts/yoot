use std::io::Write;
use std::path::Path;

use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    #[arg(long)]
    print_ast: bool,

    #[arg(long)]
    print_typed_ast: bool,

    #[arg(long)]
    print_wasm: bool,

    #[arg(long)]
    print_all: bool,

    #[arg(long)]
    interpret: bool,

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
    let typed_program = yoot::type_check(&program, print_typed)?;
    if print_typed {
        println!("TYPED PROGRAM: {typed_program:#?}");
    }

    let result = yoot::compile(&typed_program)?;
    if args.print_all || args.print_wasm {
        println!("WASM MODULE:");
        yoot::print(&result);
    }

    if args.interpret {
        let result = yoot::interpret(&program);
        println!("INTERPRETER RESULT: {result:?}");
    }

    if let Some(output_file) = args.output_file {
        std::fs::File::create(&output_file)?.write_all(&result)?;
    } else {
        yoot::run(&result)?;
    }

    Ok(())
}
