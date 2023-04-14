/// Uses the Binaryoot abstraction to build a wasm program.
use crate::binaryoot::{Op::*, Type::*, *};
use crate::typed_ast::TypedProgram;
use crate::util::Result;

pub fn compile(_program: &TypedProgram) -> Result<()> {
    let module = Module::new();

    module.add_function(
        "adder",
        &FnType::new(&[Int32, Int32], Int32),
        module.binary(
            AddInt32,
            module.local_get(0, Int32),
            module.local_get(1, Int32),
        ),
    );

    module.print();

    Ok(())
}
