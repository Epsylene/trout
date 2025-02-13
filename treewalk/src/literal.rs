use std::fmt::Debug;

// A literal is a value that is directly written in the code.
// The atomic values we want to support are:
//  - Nil (the void value)
//  - String (a chain of characters)
//  - Float (a 32-bit IEEE 754 floating point number)
//  - Int (a 32-bit signed integer)
//  - Bool (a boolean value)
#[derive(Clone, PartialEq)]
pub enum LiteralType {
    Nil,
    String(String),
    Float(f32),
    Int(i32),
    Bool(bool),
}

impl Debug for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LiteralType::Nil => write!(f, "()"),
            LiteralType::String(s) => write!(f, "{:?}", s),
            LiteralType::Float(n) => write!(f, "{:?}", n),
            LiteralType::Int(n) => write!(f, "{:?}", n),
            LiteralType::Bool(b) => write!(f, "{:?}", b),
        }
    }
}