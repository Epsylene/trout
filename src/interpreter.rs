use crate::literal::Value;
use crate::token::{Token, TokenKind};
use crate::error::{Error, ErrorKind, Result};
use crate::ast::{Expr, Stmt};
use crate::environment::Environment;

pub struct Interpreter {
    // The interpreter needs to keep track of the environment
    // in which the program is running, so that it can store
    // and retrieve variables.
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::global(),
        }
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> Result<()> {
        // The interpreter works in a similar way to the parser,
        // except it works on the AST instead of the token stream.
        // The program is represented as a list of statements,
        // which each have to be executed in order.
        program.iter().try_for_each(|stmt| self.execute(stmt))
    }
    
    fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        // A statement is either:
        match stmt {
            // - An expression, which is evaluated;
            Stmt::Expression { expression } => {
                self.evaluate(expression)?;
            }
            // - A print statement, whose expression is
            //   evaluated and printed;
            Stmt::Print { expression } => {
                let value = self.evaluate(expression)?;
                println!("{}", value);
            }
            // - A variable declaration, which is evaluated and
            //   stored in the environment.
            Stmt::Var { name, initializer } => {
                // A variable can be declared but not defined.
                // If it is, we evaluate the expression and
                // store the result in the environment.
                match initializer {
                    Some(expr) => {
                        let value = self.evaluate(expr)?;
                        self.environment.define(name.lexeme.clone(), Some(value));
                    }
                    None => {
                        self.environment.define(name.lexeme.clone(), None);
                    }
                }
            }
            // - A block, which is a list of statements that
            //   are executed in order.
            Stmt::Block { statements } => {
                // A block is a new scope, so we save the
                // environment and update it to this new local
                // one.
                let enclosing = self.environment.clone();
                self.environment = Environment::local(Box::new(enclosing.clone()));
                
                // Then we can execute the statements contained
                // within the block.
                statements.iter().try_for_each(|stmt| self.execute(stmt))?;
                
                // After the block is executed, we discard the
                // environment and go back to the parent.
                self.environment = enclosing;
            }
        }
    
        Ok(())
    }
    
    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        // However complicated, an expression is just the
        // composition of 4 different types of subexpressions:
        // literals, unary expressions, binary expressions, and
        // groupings. Then, we just need to match the global
        // expression, and call the corresponding function, which
        // will do the same with the sub-expressions, and so on,
        // until we reach the leaves of the tree, which are
        // literals, and we can return all the way up with a final
        // value for the whole expression. This is called a
        // post-order traversal, because we first "visit"
        // (evaluate) the branches of the tree before visiting the
        // starting node: the resulting value is computed in the
        // same way as a postfix notation (also called "reverse
        // Polish notation": 3 + 4 is 3 4 +, for example).
        match expr {
            Expr::Literal { value } => self.literal(value),
            Expr::Unary { operator, right } => self.unary(operator, right),
            Expr::Binary { left, operator, right } => self.binary(left, operator, right),
            Expr::Grouping { expression } => self.grouping(expression),
            Expr::Variable { name } => self.variable(name),
            Expr::Assign { lhs, rhs } => self.assign(lhs, rhs),
        }
    }
    
    fn literal(&self, token: &Token) -> Result<Value> {
        // The value of a literal is the literal value.
        Ok(token.literal.clone())
    }

    fn variable(&self, name: &Token) -> Result<Value> {
        // A variable is evaluated by looking up its
        // value in the environment.
        match self.environment.get(&name.lexeme) {
            // Declared and initialized
            Some(Some(value)) => Ok(value.clone()),
            // Declared but not initialized
            Some(None) => Err(Error::new(
                &name.location, 
                ErrorKind::UndefinedVariable(name.lexeme.clone()))
            ),
            // Not declared
            None => Err(Error::new(
                &name.location, 
                ErrorKind::VariableNotDeclared(name.lexeme.clone()))
            ),
        }
    }

    fn assign(&mut self, lhs: &Token, rhs: &Expr) -> Result<Value> {
        // An assignment "a = expr;" is evaluated with the
        // value of "expr", but only if "a" has been declared
        // in the first place.
        let value = self.evaluate(rhs)?;
        if self.environment.get(&lhs.lexeme).is_some() {
            self.environment.define(lhs.lexeme.clone(), Some(value.clone()));
        } else {
            return Err(Error::new(
                &lhs.location, 
                ErrorKind::VariableNotDeclared(lhs.lexeme.clone()))
            );
        }
        
        // Returning the value from the assignment allows
        // nesting assignments inside expressions.
        Ok(value)
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
            // a and b
            TokenKind::And => Value::Bool(truthy(left) && truthy(right)),
            // a or b
            TokenKind::Or => Value::Bool(truthy(left) || truthy(right)),
            // Something else
            _ => return Err(Error::new(
                &operator.location, 
                ErrorKind::NotBinaryOperator(operator.lexeme.clone()))
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