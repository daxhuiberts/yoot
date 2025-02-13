use crate::util::Result;
use wasmtime::*;

pub fn run(binary: &[u8]) -> Result<()> {
    run_inner(binary).map_err(|e| e.to_string())
}

pub fn run_inner(binary: &[u8]) -> std::result::Result<(), wasmtime::Error> {
    let engine = Engine::default();
    let module = Module::new(&engine, binary)?;
    let mut linker = Linker::new(&engine);
    linker.func_wrap(
        "foo",
        "print_i64",
        |_caller: Caller<'_, u32>, param: i64| {
            println!("{param}");
        },
    )?;
    let mut store: Store<u32> = Store::new(&engine, 4);
    let instance = linker.instantiate(&mut store, &module)?;

    let main = instance.get_func(&mut store, "main").unwrap();
    let mut result = vec![Val::I32(0); main.ty(&store).results().count()];
    main.call(&mut store, &[], &mut result)?;

    println!("RUN RESULT: {result:?}");

    Ok(())
}
