use core::slice;

use crate::value::Value;
use crate::token::{Token, TokenKind};
use crate::error::{Error, ErrorKind, Result};
use crate::ast::{Expr, Stmt};
use crate::function::Callable;
use crate::environment::{Environment, GLOBAL_ENV};
use crate::resolver::ScopeDepth;

pub struct Interpreter {
    // The interpreter needs to keep track of the environment
    // in which the program is running, so that it can store
    // and retrieve variables.
    pub environment: Environment,
    pub scopes: ScopeDepth,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::local(Box::new(GLOBAL_ENV.clone())),
            scopes: ScopeDepth::new(),
        }
    }

    pub fn set_depths(&mut self, scopes: &ScopeDepth) {
        self.scopes = scopes.clone();
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> Result<Value> {
        // The interpreter works in a similar way to the
        // parser, except it works on the AST instead of the
        // token stream. The program is represented as a list
        // of statements, which each have to be executed in
        // order.
        let mut res = Value::Nil;
        for stmt in program {
            res = self.execute(stmt)?;
        }
        
        // Since we are working with a REPL, we want to return
        // the result of the last statement, so that it can be
        // printed to the user.
        Ok(res)
    }

    pub fn interpret_body(&mut self, body: &Stmt) -> Result<Value> {
        // When interpreting the body of a function, execution
        // might stop early because of a return statement. To
        // stop the interpretation at the right moment, a
        // special error type is thrown, which contains the
        // return value.
        match self.interpret(slice::from_ref(body)) {
            // Either the body is executed normally, and the
            // interpreter returns the last value to the
            // enclosing scope...
            Ok(value) => Ok(value),
            // ...or an error is returned early:
            Err(e) => match e.kind {
                // - Either a "return" error, which contains
                //   the value of a return statement, which is
                //   returned to the enclosing scope normally;
                ErrorKind::Return(value) => Ok(value),
                // - Or any other error, which is passed up the
                //   chain.
                _ => Err(e),
            },
        }
    }
    
    fn execute(&mut self, stmt: &Stmt) -> Result<Value> {
        // We need to match the type of statement to execute it
        // and optionally return a value.
        match stmt {
            Stmt::Expression { expression } => self.expr_stmt(expression),
            Stmt::Var { name, initializer } => self.var_stmt(name, initializer),
            Stmt::Block { statements } => self.block_stmt(statements),
            Stmt::If { condition, then_branch, else_branch } => self.if_stmt(condition, then_branch, else_branch.as_deref()),
            Stmt::While { condition, body } => self.while_stmt(condition, body),
            Stmt::For { loop_var, start, stop, step, body } => self.for_stmt(loop_var, start, stop, step, body),
            Stmt::Function { name, params, body } => self.function(name, params, body),
            Stmt::Return { keyword, value } => self.return_stmt(keyword, value),
            Stmt::Class { name, methods } => self.class_stmt(name, methods),
        }
    }

    fn expr_stmt(&mut self, expr: &Expr) -> Result<Value> {
        // An expression statement is just an expression that
        // is evaluated and whose value is returned.
        let value = self.evaluate(expr)?;
        Ok(value)
    }

    fn var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<Value> {
        // A variable can be declared but not defined, so the
        // "initializer" expression is optional.
        match initializer {
            Some(expr) => {
                let value = self.evaluate(expr)?;
                self.environment.define(name.lexeme.clone(), Some(value));
            }
            None => {
                self.environment.define(name.lexeme.clone(), None);
            }
        }
        
        // Declaring a variable doesn't return a value by
        // itself (although it could).
        Ok(Value::Nil)
    }

    fn block_stmt(&mut self, statements: &[Stmt]) -> Result<Value> {
        // A block is a new scope, so we save the environment
        // and update it to this new local one.
        let enclosing = self.environment.clone();
        self.environment = Environment::local(Box::new(enclosing));

        // Then we can do the interpretation on the sub-program
        // (the list of statements inside the block).
        let res = self.interpret(statements)?;
        
        // After the block is executed, we discard the
        // environment and go back to the parent.
        self.environment.to_enclosing();

        Ok(res)
    }

    fn if_stmt(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> Result<Value> {
        // First evaluate the condition.
        let condition = self.evaluate(condition)?;
        
        // Then execute the corresponding branch: if true,
        // "then", if false, either "else" or do nothing.
        if truthy(condition) {
            self.execute(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.execute(else_branch)
        } else {
            Ok(Value::Nil)
        }
    }

    fn while_stmt(&mut self, condition: &Expr, body: &Stmt) -> Result<Value> {        
        // To implement a while loop, we just need to evaluate
        // the condition, check its truthiness, and execute the
        // body while it stays true.
        while truthy(self.evaluate(condition)?) {
            self.execute(body)?;
        }
        
        Ok(Value::Nil)
    }

    fn for_stmt(&mut self, loop_var: &Token, start: &Expr, stop: &Expr, step: &Option<Expr>, body: &Stmt) -> Result<Value> {
        // The for loop is just syntactic sugar for a while
        // loop with a counter variable. We want first to
        // evaluate the start and stop expressions, checking
        // that they are integers.

        let mut get_integer = |val| {
            let val = self.evaluate(val)?;
            match val {
                Value::Int(i) => Ok(i),
                _ => Err(Error::new(
                    &loop_var.location, 
                    ErrorKind::ForStartStopStepInt)
                ),
            }
        };

        let start = get_integer(start)?;
        let stop = get_integer(stop)?;

        // Then we can do the same with the step value.
        let step = match step {
            Some(expr) => get_integer(expr)?,
            None => 1,
        };

        // We can now execute the loop, initializing the loop
        // variable to the start value.
        let mut counter = start;
        self.environment.define(loop_var.lexeme.clone(), Some(Value::Int(start)));

        while counter < stop {
            // The loop condition is that the loop variable is
            // less than the stop value. After checking the
            // condition, we can execute the body of the loop.
            self.execute(body)?;

            // Then, the counter is updated by the step value.
            counter += step;
            self.environment.assign(loop_var.lexeme.clone(), Value::Int(counter));
        }

        // Note: given that a for loop in the form
        //
        //      for i=a..b..s { 
        //          // statements 
        //      }
        //
        // is equivalent to the following code
        //
        //      let i = a;
        //      while i < b {
        //          // statements
        //          i = i + s;
        //      }
        //
        // we could have implemented the for loop directly in
        // the parser as a combination of a declaration and a
        // while loop with a body that includes the update of
        // the counter variable. In theory, any syntax that can
        // be "desugared" to a simpler sintax in the language
        // does not need to be implemented but only parsed as a
        // sum of the simpler statements; given the sintax of
        // the for loop we went for, though, it was simpler to
        // implement it as its own statement.

        Ok(Value::Nil)
    }
    
    fn function(&mut self, name: &Token, params: &[Token], body: &Stmt) -> Result<Value> {
        // A function is defined by its name, its parameters,
        // and its body.
        let function = Value::function(name, params, body);

        // The function is stored in the environment, because
        // it is an object (a value) like any other.
        self.environment.define(
            name.lexeme.clone(), 
            Some(function)
        );

        Ok(Value::Nil)
    }

    fn return_stmt(&mut self, keyword: &Token, value: &Option<Expr>) -> Result<Value> {
        // A return statement exits the function, returning the
        // value of an expression. If there is no expression
        // provided, the return value is Nil.
        let value = match value {
            Some(expr) => self.evaluate(expr)?,
            None => Value::Nil,
        };

        // We return the value, but we don't want to return
        // from the whole program, only from the function in
        // which the return statement is found. To do this, we
        // use a special error type that will be caught by the
        // program, so that the execution for the current scope
        // stops here.
        Error::new(
            &keyword.location,
            ErrorKind::Return(value)
        ).into()
    }

    fn class_stmt(&mut self, name: &Token, methods: &[Stmt]) -> Result<Value> {
        // The class exists by itself in the environment, so we
        // bind its identifier.
        self.environment.define(name.lexeme.clone(), None);
        self.environment.assign(name.lexeme.clone(), Value::class(name));

        // The "class statement" itself does not return a
        // value.
        Ok(Value::Nil)
    }

    fn lambda(&mut self, params: &[Token], body: &Stmt) -> Result<Value> {
        // A lambda is a function without a name, so it is
        // defined by its parameters and its body. It also has
        // a closure, which is the environment in which it was
        // defined.
        let lambda = Value::lambda(params, body, self.environment.clone());

        // We don't store the lambda in the environment,
        // because by itself it is only an object value, which
        // is passed around and stored.
        Ok(lambda)
    }

    fn field_get(&mut self, object: &Expr, field: &Token) -> Result<Value> {
        // We first need to get the "field-accessing"
        // expression, which should evaluate to an object of a
        // class.
        let object = self.evaluate(object)?;
        match object {
            // If that is the case, the value returned is that
            // of the field accessed.
            Value::Instance(instance) => {
                Ok(instance.get(field)?)
            }
            _ => Err(Error::new(
                &field.location,
                ErrorKind::NotInstance)
            ),
        }
    }

    fn field_set(&mut self, object: &Expr, field: &Token, value: &Expr) -> Result<Value> {
        // What is trying to set the field?
        let object = self.evaluate(object)?;
        match object {
            Value::Instance(mut instance) => {
                // If it is an object, then we can evaluate the
                // value and set the field to it.
                let value = self.evaluate(value)?;
                instance.set(field.clone(), value.clone());

                // Like with variable assignment, we return the
                // value of the assignment, so that we can nest
                // set assignments.
                Ok(value)
            }
            _ => Err(Error::new(
                &field.location,
                ErrorKind::NotInstance)
            ),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        // However complicated, an expression is just the
        // composition of a finite number of different
        // sub-expressions. Then, for each expression, we just
        // need to match its type, and call the corresponding
        // function, which will do the same with the
        // sub-expressions, and so on, until we reach the
        // leaves of the tree, which are literals, and we can
        // return all the way up with a final value for the
        // whole expression. This is called a post-order
        // traversal, because we first "visit" (evaluate) the
        // branches of the tree before visiting the starting
        // node: the resulting value is computed in the same
        // way as a postfix notation (also called "reverse
        // Polish notation": 3 + 4 is 3 4 +, for example).
        match expr {
            Expr::Literal { value } => self.literal(value),
            Expr::Unary { operator, right } => self.unary(operator, right),
            Expr::Binary { left, operator, right } => self.binary(left, operator, right),
            Expr::Logical { left, operator, right } => self.logical(left, operator, right),
            Expr::Grouping { expression } => self.grouping(expression),
            Expr::Variable { name } => self.variable(name),
            Expr::Assign { lhs, rhs } => self.assign(lhs, rhs),
            Expr::Call { callee, arguments, close_paren } => self.call(callee, arguments, close_paren),
            Expr::Lambda { params, body } => self.lambda(params, body),
            Expr::Get { object, field } => self.field_get(object, field),
            Expr::Set { object, field, value } => self.field_set(object, field, value),
        }
    }
    
    fn literal(&self, token: &Token) -> Result<Value> {
        // The value of a literal is the literal value.
        Ok(token.literal.clone().into())
    }

    fn variable(&self, name: &Token) -> Result<Value> {
        let depth = self.scopes.get(name).cloned().unwrap_or_else(|| panic!("Variable '{}' not resolved", name.lexeme.as_str()));

        // A variable is evaluated by looking up its value in
        // the environment.
        match self.environment.get_at(depth, &name.lexeme).cloned().unwrap() {
            // Declared and initialized
            Some(value) => Ok(value.clone()),
            // Declared but not initialized
            None => Err(Error::new(
                &name.location, 
                ErrorKind::VariableNotDefined(name.lexeme.clone()))
            ),
        }
    }

    fn assign(&mut self, lhs: &Token, rhs: &Expr) -> Result<Value> {        
        // First check if the variable to assign actually
        // exists, that is, if it has been declared in this
        // environment (scope) or any enclosing one. Thanks to
        // static analysis, this means that we just have to
        // check for the depth of the variable in the
        // environment stack--if it is not None, the variable
        // has been declared at that depth.
        if let Some(depth) = self.scopes.get(lhs).cloned() {
            // In that case, evaluate the expression and assign
            // at the corresponding depth.
            let value = self.evaluate(rhs)?;
            self.environment.assign_at(depth, lhs.lexeme.clone(), value.clone());
            
            // Returning the value from the assignment allows
            // nesting assignments inside expressions.
            Ok(value)
        } else {
            Err(Error::new(
                &lhs.location, 
                ErrorKind::VariableNotDeclared(lhs.lexeme.clone()))
            )
        }
    }

    fn call(&mut self, callee: &Expr, arguments: &[Expr], close_paren: &Token) -> Result<Value> {
        // We first need to evaluate the callee expression to
        // get what is calling (an identifier, another function
        // call, etc).
        let callee = self.evaluate(callee)?;
        
        // Then evaluate the arguments.
        let mut args = Vec::new();
        for arg in arguments {
            args.push(self.evaluate(arg)?);
        }

        // Finally, we can call the function (which may
        // actually be a class, since it is by "calling" a
        // class that we create new instances of that type).
        match callee {
            Value::Function(f) => self.make_call(f, args, close_paren),
            Value::Lambda(f) => self.make_call(f, args, close_paren),
            Value::NativeFunction(f) => self.make_call(f, args, close_paren),
            Value::Class(c) => self.make_call(c, args, close_paren),
            _ => Err(Error::new(
                &close_paren.location,
                ErrorKind::NotCallable)
            ),
        }
    }

    fn make_call(&mut self, f: impl Callable, args: Vec<Value>, close_paren: &Token) -> Result<Value> {
        // Check that the number of arguments is correct...
        if f.arity() != args.len() {
            return Err(Error::new(
                &close_paren.location,
                ErrorKind::FunctionArity(f.arity(), args.len()))
            );
        }

        // ...and only then actually call the function.
        f.call(self, args)
    }
    
    fn unary(&mut self, operator: &Token, right: &Expr) -> Result<Value> {
        // First get whatever value the right expression holds...
        let right = self.evaluate(right)?;
    
        // ...then match the operator.
        let result = match operator.kind {
            // -a
            TokenKind::Minus => match right {
                Value::Int(i) => Value::Int(-i),
                Value::Float(f) => Value::Float(-f),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // !a
            TokenKind::Bang => Value::Bool(!truthy(right)),
            _ => return Err(Error::new(
                &operator.location, 
                ErrorKind::NotUnaryOperator(operator.lexeme.clone()))
            ),
        };
    
        Ok(result)
    }
    
    fn binary(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Value> {
        // Get the values on the two sides...
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
    
        // ...then match the operator and apply the operation.
        let result = match operator.kind {
            // a + b
            TokenKind::Plus => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
                (Value::Int(l), Value::Float(r)) => Value::Float(l as f32 + r),
                (Value::Float(l), Value::Int(r)) => Value::Float(l + r as f32),
                (Value::String(l), Value::String(r)) => Value::String(l + &r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotAddOrConcat)
                ),
            },
            // a - b
            TokenKind::Minus => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
                (Value::Int(l), Value::Float(r)) => Value::Float(l as f32 - r),
                (Value::Float(l), Value::Int(r)) => Value::Float(l - r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a * b
            TokenKind::Star => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
                (Value::Int(l), Value::Float(r)) => Value::Float(l as f32 * r),
                (Value::Float(l), Value::Int(r)) => Value::Float(l * r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a / b
            TokenKind::Slash => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Float(l as f32 / r as f32),
                (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                (Value::Int(l), Value::Float(r)) => Value::Float(l as f32 / r),
                (Value::Float(l), Value::Int(r)) => Value::Float(l / r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a < b
            TokenKind::Less => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l < r),
                (Value::Int(l), Value::Float(r)) => Value::Bool((l as f32) < r),
                (Value::Float(l), Value::Int(r)) => Value::Bool(l < r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a <= b
            TokenKind::LessEqual => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l <= r),
                (Value::Int(l), Value::Float(r)) => Value::Bool(l as f32 <= r),
                (Value::Float(l), Value::Int(r)) => Value::Bool(l <= r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a > b
            TokenKind::Greater => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l > r),
                (Value::Int(l), Value::Float(r)) => Value::Bool(l as f32 > r),
                (Value::Float(l), Value::Int(r)) => Value::Bool(l > r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a >= b
            TokenKind::GreaterEqual => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l >= r),
                (Value::Int(l), Value::Float(r)) => Value::Bool(l as f32 >= r),
                (Value::Float(l), Value::Int(r)) => Value::Bool(l >= r as f32),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotIntOrFloat)
                ),
            },
            // a == b
            TokenKind::EqualEqual => match (left.clone(), right.clone()) {
                (Value::Int(l), Value::Float(r)) => Value::Bool((l as f32) == r),
                (Value::Float(l), Value::Int(r)) => Value::Bool(l == (r as f32)),
                _ => Value::Bool(left == right),
            }
            // a != b
            TokenKind::BangEqual => match (left.clone(), right.clone()) {
                (Value::Int(l), Value::Float(r)) => Value::Bool((l as f32) != r),
                (Value::Float(l), Value::Int(r)) => Value::Bool(l != (r as f32)),
                _ => Value::Bool(left != right),
            },
            // Something else
            _ => return Err(Error::new(
                &operator.location, 
                ErrorKind::NotBinaryOperator(operator.lexeme.clone()))
            ),
        };
    
        Ok(result)
    }

    fn logical(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Value> {
        // The binary && and || operators are special, in that
        // they can short-circuit: an expression of the form a
        // && b, for example, will always evaluate to false if
        // a is false. Similarly, a || b will always evaluate
        // to true if a is true. Thus, we start by evaluating
        // only the left expression.
        let left = self.evaluate(left)?;
        
        // Then we can match the operator.
        let result = match operator.kind {
            // a || b
            TokenKind::Or => {
                // Logical operators generally return a boolean
                // value, but given that non-boolean values are
                // accepted as operands, we don't necessarily
                // want a boolean result. We will say for the
                // OR operator that the value returned is that
                // of the operand with appropriate truthiness:
                // for example, "a" || false will return "a";
                // nil || 5 || "x" will return 5, etc.
                if truthy(left.clone()) {
                    // If the left expression is true, we don't
                    // need to evaluate the right one to know
                    // the result.
                    left
                } else {
                    // In the other case, we can only get a
                    // result from the right operand.
                    self.evaluate(right)?
                }
            }
            // a && b
            TokenKind::And => {
                // Similarly, the result of a logical AND is
                // that of the operand with appropriate
                // non-truthiness: for example, "a" && false
                // will return false; 0 && nil && "x" will
                // return 0, etc.
                if truthy(left.clone()) {
                    // If the left expression is true, we need
                    // to evaluate the right one to know the
                    // result.
                    self.evaluate(right)?
                } else {
                    // In the other case, it is always the
                    // value of the left operand.
                    left
                }
            }
            // Something else
            _ => return Err(Error::new(
                &operator.location, 
                ErrorKind::NotLogicalOperator(operator.lexeme.clone()))
            ),
        };
    
        Ok(result)
    }
    
    fn grouping(&mut self, expression: &Expr) -> Result<Value> {
        // The value of a grouping is just the value of the
        // expression contained within.
        self.evaluate(expression)
    }
}

fn truthy(val: Value) -> bool {
    // "Truthy": something that is not True, but convertible to
    // True. We will say that anything that is not Nil or False
    // is truthy.
    match val {
        Value::Nil => false,
        Value::Bool(b) => b,
        _ if zero_like(val) => false,
        _ => true,
    }
}

fn zero_like(val: Value) -> bool {
    // A value is zero-like if it is 0 or 0.0.
    match val {
        Value::Int(i) => i == 0,
        Value::Float(f) => f == 0.0,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;
    use crate::scanner::Scanner;
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    use crate::token::Location;

    fn interpret(input: &str) -> Result<Value, Vec<Error>> {
        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan()?;
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;
        let mut resolver = Resolver::new();
        resolver.resolve(&program)?;
        let mut interpreter = Interpreter::new();
        interpreter.set_depths(resolver.depths());
        
        interpreter.interpret(&program).map_err(|e| e.into())
    }

    #[test]
    fn test_grouping_expr() {
        let input = "(5 + 6) * 2";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(22));
    }

    #[test]
    fn test_decl_assign() {
        let input = 
            r"
                let a = 5;
                a = a + 1;
                a
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(6));
    }

    #[test]
    fn test_print() {
        let input = 
            r"
                let a = 5;
                print(a);
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Nil);
    }

    #[test]
    fn test_if() {
        let input = 
            r"  
                let a = 5;
                if a < 6 { 
                    a = a + 1;
                }
                a
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(6));
    }

    #[test]
    fn test_while() {
        let input = 
            r"
                let a = 2; 
                while a < 6 {
                    a = a + 1;
                } 
                a
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(6));
    }

    #[test]
    fn test_for() {
        let input = 
            r"
                let a = 0;
                for i=0..5 {
                    a = a + i;
                }
                a
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(10));
    }

    #[test]
    fn test_scopes() {
        let input = 
            r"
                let a = 5;
                {
                    let b = 6;
                    {
                        let c = 7;
                        a = b + c;
                    }
                    a = b;
                }
                a
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(6));
    }

    #[test]
    fn test_function() {
        let input = 
            r"
                fn add(a, b) {
                    a + b;
                }
                add(5, 6)
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(11));
    }

    #[test]
    fn test_lambda() {
        let input = 
            r"
                let add = fn(a, b) {
                    a + b;
                };
                add(5, 6)
            ";
        let res = interpret(input).unwrap();
        assert_eq!(res, Value::Int(11));
    }

    #[test]
    fn test_autoinit() {
        let input = "let a = a + 2";
        let res = interpret(input);
        if let Err(errors) = res {
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0], Error::new(
                &Location::new(1, 9),
                ErrorKind::AutoInitialization("a".to_string())
            ));
        } else {
            panic!();
        }
    }
}