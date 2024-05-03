use std::collections::HashMap;
use crate::literal::Value;

// The "environment" of the program is all the data that it has
// created and is handling as part of its execution. Variables
// can thus be stored as a map of their identifiers to their
// values in memory.
pub struct Environment {
    values: HashMap<String, Option<Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
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
        // Checking if there is an entry for the variable
        // (first Option), meaning that it exists, or if has
        // been defined with a value (second Option) is done
        // during the interpretation phase, when this function
        // is called.
        self.values.get(name)
    }
}