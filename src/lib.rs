mod ast;
mod binaryoot;
mod indent_lexer;
mod indent_parser;
mod interpreter;
mod type_checker;
mod typed_ast;
mod util;
mod wasm_compiler;

pub use indent_lexer::lex;
pub use indent_parser::parse;
pub use interpreter::execute as interpret;
pub use type_checker::check as type_check;
pub use wasm_compiler::compile as compile_to_wasm;
