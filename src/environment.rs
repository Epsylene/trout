use std::collections::HashMap;
use crate::literal::Value;

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
#[derive(Clone)]
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
        // The definition of a variable inserts a new (label,
        // value) pair in the environment. Note here that we
        // don't check if the label already exists, nor the
        // type of the value, meaning we can redefine variables
        // to any value we like.
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Option<Value>> {
        // Returning the value of the variable with the given
        // label. If the variable exists in the current
        // environment, we return it directly; otherwise, we
        // check if it exists in the parent environment, which
        // will itself check its parent if needed, and so on up
        // to the global scope.
        match self.values.get(name) {
            Some(value) => Some(value),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(name),
                None => None,
            }
        }
    }
}