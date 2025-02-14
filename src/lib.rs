mod ast;
mod indent_stream;
mod interpreter;
mod parser;
mod runtime;
mod type_checker;
mod typed_ast;
mod util;
mod wasm_builder;
mod wasm_compiler;

pub use interpreter::execute as interpret;
pub use parser::parse;
pub use runtime::run;
pub use type_checker::check as type_check;
pub use wasm_compiler::compile;
pub use wasm_compiler::print;
