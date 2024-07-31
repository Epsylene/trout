use std::fmt::{Display, Debug};

use crate::{
    ast::Stmt, 
    error::Result, 
    function::{Callable, Function, Lambda, NativeFunction}, 
    environment::Environment,
    interpreter::Interpreter, 
    literal::LiteralType, 
    token::Token,
    class::Class,
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
    Lambda(Lambda),
    NativeFunction(NativeFunction),

    // Classes
    Class(Class),
}

impl Value {
    pub fn function(name: &Token, params: &[Token], body: &Stmt) -> Self {
        Value::Function(Function::new(name.lexeme.clone(), params.to_vec(), body.clone()))
    }

    pub fn lambda(params: &[Token], body: &Stmt, closure: Environment) -> Self {
        Value::Lambda(Lambda::new(params.to_vec(), body.clone(), closure))
    }

    pub fn class(name: &Token) -> Self {
        Value::Class(Class::new(name.lexeme.clone()))
    }
}

impl Callable for Value {
    fn arity(&self) -> usize {
        match self {
            Value::Function(func) => func.arity(),
            Value::NativeFunction(func) => func.arity(),
            _ => unreachable!("Not a function"),
        }
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value> {
        match self {
            Value::Function(func) => func.call(interpreter, args),
            Value::NativeFunction(func) => func.call(interpreter, args),
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
            Value::Nil => write!(f, "Nil()"),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Float(n) => write!(f, "Float({:?})", n),
            Value::Int(n) => write!(f, "Int({:?})", n),
            Value::Bool(b) => write!(f, "Bool({:?})", b),
            Value::Function(func) => write!(f, "{:?}", func),
            Value::Lambda(lambda) => write!(f, "{:?}", lambda),
            Value::NativeFunction(func) => write!(f, "{:?}", func),
            Value::Class(class) => write!(f, "{:?}", class),
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
            Value::Function(func) => write!(f, "{}()", func.name),
            Value::Lambda(lambda) => write!(f, "fn({})", lambda.params.join(", ")),
            Value::NativeFunction(func) => write!(f, "{}()", func.name),
            Value::Class(class) => write!(f, "{}", class.name),
        }
    }
}