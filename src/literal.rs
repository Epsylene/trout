use std::fmt::{Display, Debug};

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralType {
    Nil,
    String(String),
    Float(f32),
    Int(i32),
    Bool(bool),
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LiteralType::Nil => write!(f, "()"),
            LiteralType::String(s) => write!(f, "{}", s),
            LiteralType::Float(n) => write!(f, "{}", n),
            LiteralType::Int(n) => write!(f, "{}", n),
            LiteralType::Bool(b) => write!(f, "{}", b),
        }
    }
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

pub type Value = LiteralType;