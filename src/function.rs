use core::iter::zip;
use lazy_static::lazy_static;

use crate::{
    ast::Stmt, 
    environment::Environment, 
    error::Result, 
    interpreter::Interpreter, 
    value::Value,
    token::Token,
};

// A callable is anything that can be called as a function. It
// has an arity (the number of arguments it expects) and a
// generic "call" method, taking the interpreter and function
// arguments as parameters. Writing this as a trait allows us
// to define the same interface over different kinds of
// function objects (user-defined, native, closures, etc).
pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value>;
}

// A user-defined function (with the sintax "fn a() { ... }")
// is a callable object that has a name (its identifier), a
// body (the code it executes), and a list of parameters (the
// variables it expects to receive). When called, it creates a
// new environment, populates it with the arguments passed to
// the function, and interprets the body within that scope,
// returning a value at the end.
#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub name: String,
    pub body: Stmt,
    pub params: Vec<String>,
}

impl Function {
    pub fn new(name: String, params: Vec<Token>, body: Stmt) -> Self {
        Function {
            name,
            body,
            params: params.iter().map(|t| t.lexeme.clone()).collect(),
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value> {
        // Functions have bodies and parameters, so they carry
        // data in a separate scope from the global
        // environment. This environment is not tied to the
        // function itself, however, but to the function call:
        // a recursive function, for example, has multiple
        // local scopes nested within each other, each calling
        // the same code on different data.
        let enclosing = interpreter.environment.clone();
        let mut env = Environment::local(Box::new(enclosing));
        
        // The environment of a function call is pre-populated
        // with the values of the arguments passed to the
        // function.
        for (param, arg) in zip(&self.params, args) {
            env.define(param.clone(), Some(arg));
        }
        
        // Then, the body can be interpreted within its
        // environment, which is then restored to the enclosing
        // scope.
        interpreter.environment = env;
        let res = interpreter.interpret_body(&self.body);
        interpreter.environment.to_enclosing();

        // The result of the function call is the result of
        // interpreting its body.
        res
    }
}

// A native function (also "primitive", "external" or "foreign"
// function) is a function that is not defined with user code,
// but is implemented using the language that hosts the
// interpreter (in our case Rust). This is necessary for
// functions which cannot be provided by the language itself,
// like I/O operations, system calls, or other operations that
// are not part of the language's standard library. Languages
// might also provide a Foreign Function Interface (FFI), a
// mechanism for users to define their own native functions in
// a host language, which can then be called from the
// interpreted language.

// In our case, a native function is defined as a struct
// wrapping a Rust function pointer, which is executed under
// the hood when the interpreter hits call().
#[derive(Clone, PartialEq, Debug)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub function: fn(args: Vec<Value>) -> Result<Value>,
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, _: &mut Interpreter, args: Vec<Value>) -> Result<Value> {
        (self.function)(args)
    }
}

// Predefined native functions
lazy_static! {
    // print(x) - Print a value
    pub static ref PRINT: Value = Value::NativeFunction(
        NativeFunction {
            name: "print".to_string(),
            arity: 1,
            function: |args: Vec<Value>| {
                println!("{}", args[0]);
                Ok(Value::Nil)
            },
        });

    // clock() - Get the current time in seconds
    pub static ref CLOCK: Value = Value::NativeFunction(
        NativeFunction {
            name: "clock".to_string(),
            arity: 0,
            function: |_: Vec<Value>| {
                let now = std::time::SystemTime::now();
                let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
                Ok(Value::Float(duration.as_secs_f64() as f32))
            },
        });
}