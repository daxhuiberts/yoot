# Yoot

Yoot is an experiment in programming language design and development as a result of having read the amazing book named Crafting Interpreters by Robert Nystrom.

### Todo

- [ ] Multi line blocks
  - [ ] Create indent parser which correctly handles nesting
    - [x] Add setup for indent parser
    - [x] Split indent parser into separate tokenizer and parser modules
    - [ ] Fix newline handling in indent parser
- [ ] Merge bytecode compiler to main
- [ ] Prototype no-panicing int type system
- [ ] Compile to Binaryen
- [ ] Compile to Cranelift
- [ ] Compile to LLVM
- [ ] Transpile to Rust

### Ideas

- Integer types (Integer, Number, Int8, I8)
- Backends (wasm, cranelift, llvm, rust)
- Wasm interface types, idls.
- Panic, no panic.
- Optional return type, result return type.
