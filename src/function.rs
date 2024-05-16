use std::slice;
use core::iter::zip;

use crate::{
    ast::Stmt, 
    environment::Environment, 
    error::Result, 
    interpreter::Interpreter, 
    value::Value,
    token::Token,
};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value>;
}

// A function is 
#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub name: String,
    pub body: Stmt,
    pub params: Vec<String>,
}

impl Function {
    pub fn new(name: &str, params: Vec<Token>, body: &Stmt) -> Self {
        Function {
            name: name.to_string(),
            body: body.clone(),
            params: params.iter().map(|t| t.lexeme.clone()).collect(),
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value> {
        let enclosing = interpreter.environment.clone();
        let mut env = Environment::local(Box::new(enclosing));
        
        for (param, arg) in zip(&self.params, args) {
            env.define(param.clone(), Some(arg));
        }
        
        interpreter.environment = env;
        let res = interpreter.interpret(slice::from_ref(&self.body));
        interpreter.environment.to_enclosing();

        res
    }
}