use std::fmt::Display;
use num_enum::TryFromPrimitive;

#[repr(u8)]
#[derive(TryFromPrimitive)]
pub enum Opcode {
    Return = 1,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Return => write!(f, "OP_RETURN"),
        }
    }
}

pub struct Chunk {
    name: String,
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new(name: &str) -> Chunk {
        Chunk {
            name: name.to_string(),
            code: Vec::new(),
        }
    }

    pub fn push(&mut self, byte: u8) {
        self.code.push(byte);
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== {} ==", self.name)?;
        
        let mut offset = 0;
        while offset < self.code.len() {
            write!(f, "{:04} ", offset)?;
            
            let opcode = self.code[offset];
            let opcode = Opcode::try_from(opcode).expect("Invalid opcode");
            writeln!(f, "{}", opcode)?;
            
            offset = match opcode {
                Opcode::Return => offset + 1,
            };
        }

        Ok(())
    }
}