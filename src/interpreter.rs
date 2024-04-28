use crate::token::{LiteralType, Token, TokenKind};
use crate::error::{Error, ErrorKind, Result};
use crate::ast::Expr;

struct Interpreter;
type Value = LiteralType;

impl Interpreter{
    pub fn interpret(expr: &Expr) -> Result<Value> {
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
            TokenKind::Minus => match right {
                Value::Int(i) => Value::Int(-i),
                Value::Float(f) => Value::Float(-f),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::NotArithmetic)
                ),
            },
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
                    ErrorKind::IncorrectPrimary(operator.lexeme.clone()))
                ),
            },
            // a / b
            TokenKind::Slash => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                _ => return Err(Error::new(
                    &operator.location, 
                    ErrorKind::IncorrectPrimary(operator.lexeme.clone()))
                ),
            },
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