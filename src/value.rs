use std::fmt::{Display, Debug};

use crate::{
    error::Result, 
    function::{Callable, Function}, 
    interpreter::Interpreter, 
    literal::LiteralType,
    token::Token,
    ast::Stmt,
};

#[derive(Clone, PartialEq)]
pub enum Value {
    // Literals
    Nil,
    String(String),
    Float(f32),
    Int(i32),
    Bool(bool),

    // Functions
    Function(Function),
}

impl Value {
    pub fn function(name: &Token, params: &[Token], body: &Stmt) -> Self {
        let declaration = Stmt::function(name.clone(), params.to_vec(), body.clone());
        Value::Function(Function { declaration })
    }
}

impl Callable for Value {
    fn arity(&self) -> usize {
        match self {
            Value::Function(func) => func.arity(),
            _ => unreachable!("Not a function"),
        }
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value> {
        match self {
            Value::Function(func) => func.call(interpreter, args),
            _ => unreachable!("Not a function"),
        }
    }
}

impl From<LiteralType> for Value {
    fn from(literal: LiteralType) -> Self {
        match literal {
            LiteralType::Nil => Value::Nil,
            LiteralType::String(s) => Value::String(s),
            LiteralType::Float(n) => Value::Float(n),
            LiteralType::Int(n) => Value::Int(n),
            LiteralType::Bool(b) => Value::Bool(b),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "()"),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Float(n) => write!(f, "{:?}", n),
            Value::Int(n) => write!(f, "{:?}", n),
            Value::Bool(b) => write!(f, "{:?}", b),
            Value::Function(func) => write!(f, "{:?}", func),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "()"),
            Value::String(s) => write!(f, "{}", s),
            Value::Float(n) => write!(f, "{}", n),
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(func) => write!(f, "{}()", func.declaration),
        }
    }
}