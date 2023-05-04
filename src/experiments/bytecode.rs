use super::ast::{self, *};

use std::io::Write;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
#[repr(u8)]
pub enum Opcode {
    Num = 1,
    Bool,
    Add,
    Eq,
    Br,
    Brz,
    Exit,
}

impl Opcode {
    fn from_u8(val: u8) -> Result<Self> {
        match val {
            1 => Ok(Opcode::Num),
            2 => Ok(Opcode::Bool),
            3 => Ok(Opcode::Add),
            4 => Ok(Opcode::Eq),
            5 => Ok(Opcode::Br),
            6 => Ok(Opcode::Brz),
            7 => Ok(Opcode::Exit),
            x => Err(format!("{x} is not a valid opcode").into()),
        }
    }
}

#[derive(Debug)]
pub struct Bytecode {
    data: Vec<u8>,
}

pub fn compile(program: &ast::Program) -> Result<Bytecode> {
    fn parse_expr(data: &mut Vec<u8>, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Num(val) => data.extend_from_slice(&[Opcode::Num as u8, *val as u8]),
            Expr::Bool(val) => data.extend_from_slice(&[Opcode::Bool as u8, *val as u8]),

            Expr::Add(left, right) => {
                parse_expr(data, left)?;
                parse_expr(data, right)?;
                data.push(Opcode::Add as u8);
            }

            Expr::Eq(left, right) => {
                parse_expr(data, left)?;
                parse_expr(data, right)?;
                data.push(Opcode::Eq as u8);
            }

            Expr::If(cond, then, else_) => {
                parse_expr(data, cond)?;
                data.push(Opcode::Brz as u8);
                let brz_offset = data.len();
                data.push(0); // placeholder
                parse_expr(data, then)?;

                if let Some(else_) = else_ {
                    data.push(Opcode::Br as u8);
                    let br_offset = data.len();
                    data.push(0); // placeholder
                    data[brz_offset] = data.len() as u8; // set data[brz_offset] to jump to here.
                    parse_expr(data, else_)?;
                    data[br_offset] = data.len() as u8; // set data[br_offset] to jump to here.
                } else {
                    data[brz_offset] = data.len() as u8; // set data[brz_offset] to jump to here.
                }
            }

            expr => panic!("expression '{expr:?}' not supported"),
        };

        Ok(())
    }

    let mut data = vec![];

    let Decl::Stm { ref expr } = program.decls()[0] else { panic!("only a single top level expression statement supported") };

    parse_expr(&mut data, expr)?;
    data.push(Opcode::Exit as u8);

    Ok(Bytecode { data })
}

pub fn disassemble<W: Write>(bytecode: &Bytecode, mut writer: W) -> Result<()> {
    let mut data = bytecode.data.iter().enumerate();

    while let Some((index, byte)) = data.next() {
        let opcode = Opcode::from_u8(*byte)?;

        match opcode {
            Opcode::Num => {
                let (_, val) = data.next().ok_or("missing value")?;
                writeln!(writer, "{index:02} Num {val}")?;
            }
            Opcode::Bool => {
                let (_, val) = data.next().ok_or("missing value")?;
                let bool = *val != 0;
                writeln!(writer, "{index:02} Bool {bool}")?;
            }
            Opcode::Add => writeln!(writer, "{index:02} Add")?,
            Opcode::Eq => writeln!(writer, "{index:02} Eq")?,
            Opcode::Br => {
                let (_, val) = data.next().ok_or("missing value")?;
                writeln!(writer, "{index:02} Br {val}")?;
            }
            Opcode::Brz => {
                let (_, val) = data.next().ok_or("missing value")?;
                writeln!(writer, "{index:02} Brz {val}")?;
            }
            Opcode::Exit => writeln!(writer, "{index:02} Exit")?,
        }
    }

    Ok(())
}

struct Program<'a> {
    bytecode: &'a Bytecode,
    index: usize,
    stack: Vec<u8>,
}

impl<'a> Program<'a> {
    fn next(&mut self) -> u8 {
        let result = self.bytecode.data[self.index];
        self.index += 1;
        result
    }

    fn jump(&mut self, index: usize) {
        self.index = index;
    }

    fn push(&mut self, value: u8) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> u8 {
        self.stack.pop().unwrap()
    }
}

pub fn execute(bytecode: &Bytecode) -> Result<u8> {
    let mut program = Program {
        bytecode,
        index: 0,
        stack: vec![],
    };

    let result = loop {
        let byte = program.next();
        let opcode = Opcode::from_u8(byte)?;

        match opcode {
            Opcode::Num => {
                let val = program.next();
                program.push(val);
            }
            Opcode::Bool => {
                let val = program.next();
                program.push(val);
            }
            Opcode::Add => {
                let right = program.pop();
                let left = program.pop();
                let result = left + right;
                program.push(result);
            }
            Opcode::Eq => {
                let right = program.pop();
                let left = program.pop();
                let result = left == right;
                program.push(result as u8);
            }
            Opcode::Br => {
                let index = program.next();
                program.jump(index as usize);
            }
            Opcode::Brz => {
                let index = program.next();
                let value = program.pop();
                if value == 0 {
                    program.jump(index as usize);
                }
            }
            Opcode::Exit => {
                let result = program.pop();
                break result;
                // let None = stack.pop() else { return Err("did not expect another value".into()) };
                // return Ok(result);
            }
        }
    };

    Ok(result)
}
