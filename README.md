# Yoot

Yoot is an experiment in programming language design and development as a result of having read the amazing book named Crafting Interpreters by Robert Nystrom.

### Todo

- [ ] Improve parser/lexer
  - [x] Rename Token::Op to Token::Punct
  - [x] Remove old immediate text parser
  - [ ] Work out new experimental block parser architecture
- [ ] Multi line blocks
  - [ ] Create indent parser which correctly handles nesting
    - [x] Add setup for indent parser
    - [x] Split indent parser into separate tokenizer and parser modules
    - [ ] Fix newline handling in indent parser
  - [ ] Support nested if statements
  - [ ] Support nested call statements
  - [ ] Support nested assignment statements
  - [ ] Support nested function statements
- [ ] Add CLI flags for outputting debug information (tokens, AST, typed AST, etc)
- [ ] Add testing framework to check examples against expected (debug) outputs
- [ ] Add support for while statements
- [ ] Add parsing error supports
  - [ ] Handle spans in tokenizer and parser
  - [ ] Integrate ariadne diagnostic rendering
- [ ] Add all (branch) experiments to main branch inside experiments folder
  - [ ] Merge bytecode compiler to main
  - [ ] Move tokenizer.rs to experiments folder
  - [ ] Merge chumsky experiments branch
  - [ ] Merge block parser branch
- [ ] Prototype no-panicing int type system
- [ ] Compile to Binaryen
- [ ] Compile to Cranelift
- [ ] Compile to LLVM
- [ ] Transpile to Rust
  - [ ] Use rustc-plugin to hook into Rust?
- [ ] Change language syntax
  - [ ] Change function definition to always require parentheses?
  - [ ] Only allow functions in top level
  - [ ] Only allow assignments in functions
  - [ ] Start with a main function

### Ideas

- Integer types (Integer, Number, Int8, I8)
- Backends (wasm, cranelift, llvm, rust)
- Wasm interface types, idls.
- Panic, no panic.
- Optional return type, result return type.
