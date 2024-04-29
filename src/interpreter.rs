use crate::token::{LiteralType, Token, TokenKind};
use crate::error::{Error, ErrorKind, Result};
use crate::ast::Expr;

struct Interpreter;
type Value = LiteralType;

impl Interpreter{
    pub fn interpret(expr: &Expr) -> Result<Value> {
        // The interpreter works in a similar way to the
        // parser, except the tree has already been built, in
        // the form of an expression object. However
        // complicated, it is just a composition of 4 different
        // types of subexpressions: literals, unary
        // expressions, binary expressions, and groupings.
        // Then, we just need to match the global expression,
        // and call the corresponding function, which will do
        // the same with the sub-expressions, and so on, until
        // we reach the leaves of the tree, which are literals,
        // and we can go return all the way up with a final
        // value for the whole expression. This is called a
        // post-order traversal, because we first visit the
        // branches of the tree before visiting the starting
        // node: the resulting value is computed in the same
        // way as a postfix notation (also called "reverse
        // Polish notation": 3 + 4 is 3 4 +, for example).
        match expr {
            Expr::Literal { value } => Self::literal(value),
            Expr::Unary { operator, right } => Self::unary(operator, right),
            Expr::Binary { left, operator, right } => Self::binary(left, operator, right),
            Expr::Grouping { expression } => Self::grouping(expression),
        }
    }

    fn literal(token: &Token) -> Result<Value> {
        // The value of a literal is the literal value.
        Ok(token.literal.clone())
    }

    fn unary(operator: &Token, right: &Expr) -> Result<Value> {
        // First get whatever value the right expression holds...
        let right = Self::interpret(right)?;

        // ...then match the operator.
        let result = match operator.kind {
            // -a
            TokenKind::Minus => match right {
                Value::Int(i) => Value::Int(-i),
                Value::Float(f) => Value::Float(-f),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
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

    fn binary(left: &Expr, operator: &Token, right: &Expr) -> Result<Value> {
        // Get the values on the two sides...
        let left = Self::interpret(left)?;
        let right = Self::interpret(right)?;

        // ...then match the operator and apply the operation.
        let result = match operator.kind {
            // a + b
            TokenKind::Plus => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a - b
            TokenKind::Minus => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a * b
            TokenKind::Star => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a / b
            TokenKind::Slash => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a < b
            TokenKind::Less => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l < r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a <= b
            TokenKind::LessEqual => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l <= r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a > b
            TokenKind::Greater => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l > r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a >= b
            TokenKind::GreaterEqual => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l >= r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
            // a == b
            TokenKind::EqualEqual => Value::Bool(left == right),
            // a != b
            TokenKind::BangEqual => Value::Bool(left != right),
            _ => return Err(Error::new(
                &operator.location, 
                ErrorKind::NotBinaryOperator(operator.lexeme.clone()))
            ),
        };

        Ok(result)
    }

    fn grouping(expression: &Expr) -> Result<Value> {
        // The value of a grouping is just the value of the
        // expression contained within.
        Self::interpret(expression)
    }
}

fn truthy(val: Value) -> bool {
    // "Truthy": something that is not True, but convertible to
    // True. We will say that anything that is not Nil or False
    // is truthy.
    match val {
        Value::Nil => false,
        Value::Bool(b) => b,
        _ => true,
    }
}