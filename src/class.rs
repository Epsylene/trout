use std::{fmt::Debug, collections::{HashMap, hash_map::Entry}};
use crate::{
    error::{Error, ErrorKind, Result}, 
    function::Callable, 
    interpreter::Interpreter, 
    token::Token, 
    value::Value
};

#[derive(Clone, PartialEq)]
pub struct Class {
    pub name: String,
}

impl Class {
    pub fn new(name: String) -> Self {
        Class { name }
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<Value>) -> Result<Value> {
        // "Calling" a class is the way that instances are
        // created: "let a = A()" binds an object of type
        // 'A' to the variable 'a'.
        Ok(Value::instance(self.clone()))
    }
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<class '{}'>", self.name)
    }
}

#[derive(Clone, PartialEq)]
pub struct Instance {
    pub class: Class,
    fields: HashMap<Token, Value>,
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Instance { class, fields: HashMap::new() }
    }

    pub fn get(&self, field: &Token) -> Result<Value> {
        match self.fields.get(field) {
            Some(value) => Ok(value.clone()),
            None => Err(Error::new(
                &field.location,
                ErrorKind::UnknownField(field.lexeme.clone())
            )),
        }
    }

    pub fn set(&mut self, field: Token, value: Value) {
        // Update the field, and create it if it doesn't exist.
        self.fields.insert(field, value);
    }
}

impl Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<instance of '{}'>", self.class.name)
    }
}