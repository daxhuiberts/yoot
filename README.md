# Yoot

Yoot is an experiment in programming language design and development as a result of having read the amazing book named Crafting Interpreters by Robert Nystrom.

### Todo

- [ ] Add support for comments
- [ ] Add support for updating variables
- [ ] Add support for multi line functions
- [ ] Add support for while statements
- [ ] Implement type inference
- [ ] Compile to Binaryen
- [ ] Add testing framework to check examples against expected (debug) outputs
- [ ] Add parsing error supports
  - [ ] Handle spans in tokenizer and parser
  - [ ] Integrate ariadne diagnostic rendering
- [ ] Prototype no-panicing int type system
- [ ] Compile to Cranelift
- [ ] Compile to LLVM
- [ ] Transpile to Rust
  - [ ] Use rustc-plugin to hook into Rust?
- [ ] Work out new experimental block parser architecture

### Ideas

- Integer types (Integer, Number, Int8, I8)
- Backends (wasm, cranelift, llvm, rust)
- Wasm interface types, idls.
- Panic, no panic.
- Optional return type, result return type.
