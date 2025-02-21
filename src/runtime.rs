use std::str::from_utf8;

use wasmtime::*;

pub fn run(binary: &[u8]) -> std::result::Result<(), wasmtime::Error> {
    let engine = Engine::default();
    let module = Module::new(&engine, binary)?;
    let mut store = Store::new(&engine, ());

    let mut linker = Linker::new(&engine);
    linker.func_wrap("foo", "print_i64", print_i64)?;
    linker.func_wrap("foo", "print_string", print_string)?;

    let instance = linker.instantiate(&mut store, &module)?;

    let main = instance.get_func(&mut store, "main").unwrap();
    let mut result = vec![Val::I32(0); main.ty(&store).results().count()];
    main.call(&mut store, &[], &mut result)?;

    let memory = instance
        .get_export(&mut store, "memory")
        .unwrap()
        .into_memory()
        .unwrap();
    let data = memory.data(&store);
    let data = &data[0..100];
    println!("MEMORY: {data:?}");

    println!("RUN RESULT: {result:?}");

    Ok(())
}

fn print_i64(input: i64) {
    println!("{input}");
}

fn print_string(mut caller: Caller<()>, length: u32, offset: u32) -> wasmtime::Result<()> {
    let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
    let data = memory.data(&caller);
    let string = &data[(offset as usize)..((offset + length) as usize)];
    let string = from_utf8(string).unwrap();
    println!("{string}");
    Ok(())
}
