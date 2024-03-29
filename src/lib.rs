mod ast;
mod binaryoot;
mod indent_stream;
mod interpreter;
mod parser;
mod type_checker;
mod typed_ast;
mod util;
mod wasm_compiler;

pub use interpreter::execute as interpret;
pub use parser::parse;
pub use type_checker::check as type_check;
pub use wasm_compiler::compile as compile_to_wasm;
