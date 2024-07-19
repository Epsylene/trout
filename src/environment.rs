use std::{
    collections::HashMap,
    collections::hash_map::Entry,
    fmt::Debug,
};

use lazy_static::lazy_static;
use crate::value::Value;
use crate::function::*;

// The "environment" of the program is all the data that it has
// created and is handling as part of its execution. All
// variables are not created equal, however: some are part of a
// scope, like a function or a block, and some are global. To
// account for these different levels of visibility, we use a
// stack of environments, where each environment has:
//  - A map of strings to (optional) values, representing the
//    variables in the environment;
//  - A reference to the parent environment, representing the
//    enclosing scope.
#[derive(Clone, PartialEq)]
pub struct Environment {
    values: HashMap<String, Option<Value>>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn global() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn local(enclosing: Box<Environment>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: String, value: Option<Value>) {
        // Defining a variable either inserts a new (label,
        // value) pair in the environment or updates a label
        // with a new value. Note here that we don't check if
        // the label already exists, nor the type of the value,
        // meaning we can redefine variables to any value we
        // like.
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) {
        // Assigning a value to a variable is similar to
        // defining it, but we need to check if the variable
        // exists in the current environment. If it does, we
        // update it; otherwise, we check the parent
        // environment, and so on up to the global scope.
        if let Entry::Occupied(mut e) = self.values.entry(name.clone()) {
            e.insert(Some(value));
        }
    }

    pub fn assign_at(&mut self, depth: usize, name: String, value: Value) {
        // Assigning a value to a variable in the environment
        // that is 'depth' levels up the stack.
        let mut env = self;
        for _ in 0..depth {
            match &mut env.enclosing {
                Some(enclosing) => { env = enclosing },
                None => return,
            }
        }

        env.assign(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Option<Value>> {
        // Returning the value of the variable with the given
        // label in the environment. We need a getter function
        // to allow calling from a environment higher on the
        // stack visited to look for a variable.
        self.values.get(name)
    }

    pub fn get_at(&self, depth: usize, name: &str) -> Option<&Option<Value>> {
        // Returning the value of the variable with the given
        // label, but only in the environment that is 'depth'
        // levels up the stack.
        let mut env = self;
        for _ in 0..depth {
            match &env.enclosing {
                // While there is an enclosing environment and
                // we have not reached 'depth', we move up the
                // stack.
                Some(enclosing) => { env = enclosing },
                // If at any point we get None, it means we
                // have reached the end of the stack--'depth'
                // is too big.
                None => return None,
            }
        }

        // Once we have the correct environment, we can return
        // the value of the variable with the given label.
        env.get(name)
    }

    pub fn to_enclosing(&mut self) {
        // Changing the current environment to the enclosing
        // one.
        if let Some(enclosing) = self.enclosing.take() {
            self.values = enclosing.values;
            self.enclosing = enclosing.enclosing;
        }
    }

    pub fn to_env(&mut self, env: Environment) {
        // Changing the current environment to the given one.
        self.values = env.values;
        self.enclosing = env.enclosing;
    }
}

impl Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.values)
    }
}

// The global environment is a singleton, meaning there is only
// one instance of it in the program. This is because the
// global environment is the root of the environment stack, and
// all other environments are created from it. This is
// implemented as a lazy static variable, which is initialized
// the first time it is accessed.
lazy_static! {
    pub static ref GLOBAL_ENV: Environment = {
        let mut env = Environment::global();
        
        // Pre-define the native functions in the environment.
        for func in NATIVE_FUNCTIONS.iter() {
            if let Value::NativeFunction(native) = func {
                env.define(native.name.clone(), Some(func.clone()));
            }
        }

        env
    };
}