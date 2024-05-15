use std::slice;
use core::iter::zip;

use crate::{
    ast::Stmt, environment::Environment, error::Result, interpreter::Interpreter, value::Value
};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value>;
}

//
#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub declaration: Stmt,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        match &self.declaration {
            Stmt::Function { params, .. } => params.len(),
            _ => unreachable!("Not a function"),
        }
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value> {
        let res = match &self.declaration {
            Stmt::Function { body, params, .. } => {
                let mut env = Environment::global();
                for (param, arg) in zip(params, args) {
                    env.define(param.lexeme.clone(), Some(arg));
                }
                
                interpreter.interpret(slice::from_ref(body))
            }
            _ => unreachable!("Not a function"),
        };

        match res {
            Ok(Some(value)) => Ok(value),
            Ok(None) => Ok(Value::Nil),
            Err(e) => Err(e),
        }
    }
}