use std::str::from_utf8;

use byteorder::{LittleEndian, ReadBytesExt};
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

fn print_string(mut caller: Caller<()>, input: u32) -> wasmtime::Result<()> {
    let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
    let data = memory.data(&caller);
    let mut data_len = &data[(input as usize)..((input + 4) as usize)];
    let len = data_len.read_u32::<LittleEndian>().unwrap();
    let string = &data[((input + 4) as usize)..((input + 4 + len) as usize)];
    let string = from_utf8(string).unwrap();
    println!("{string}");
    Ok(())
}
