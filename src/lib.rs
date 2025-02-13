mod ast;
mod binaryoot;
mod indent_stream;
mod interpreter;
mod parser;
mod runtime;
mod type_checker;
mod typed_ast;
mod util;
mod wasm_builder;
mod wasm_compiler;
mod wasm_compiler2;

pub use interpreter::execute as interpret;
pub use parser::parse;
pub use runtime::run;
pub use type_checker::check as type_check;
pub use wasm_compiler::compile as compile_to_wasm;
pub use wasm_compiler::print as print_module;
pub use wasm_compiler::to_wasm_module;
pub use wasm_compiler2::compile as compile2;
pub use wasm_compiler2::print as print2;
