use std::fmt::Debug;

#[derive(Clone, PartialEq)]
pub struct Class {
    pub name: String,
}

impl Class {
    pub fn new(name: String) -> Self {
        Class { name }
    }
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<class '{}'>", self.name)
    }
}