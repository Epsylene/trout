mod chunk;

use chunk::{Chunk, Opcode};

fn main() {
    let mut chunk = Chunk::new("test");
    chunk.push(Opcode::Return as u8);
    println!("{}", chunk);
}
