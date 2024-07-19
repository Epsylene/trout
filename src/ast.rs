use crate::token::Token;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};

// We have a source code, which is a string of characters from
// which we derive a series of tokens -- the vocabulary of our
// language. Now, we need to define how and when those tokens
// can interact, that is, to define a "syntactic grammar" on
// the tokens, just like we defined a lexical grammar on the
// characters forming the tokens. In general, a "formal
// grammar" is a set of rules that describes which strings from
// a given alphabet (characters for a lexical grammar, tokens
// for a syntactic grammar) are valid according to the
// language's syntax. Formal grammars are ranked in a four-tier
// set of inclusions, the Chomksky hierarchy:
//
// - Regular languages (type 3): languages described by regular
//   expressions that can be parsed with a finite automaton
//   (abstract machine that can be in exactly one of a finite
//   number of states at any given time);
// - Context-free languages (type 2): languages recognized by
//   pushdown automata (finite automata with a stack memory it
//   can push and pop symbols to and from), this is the level
//   at which programming languages operate most of the time
//   (except when context is needed);
// - Context-sensitive languages (type 1): languages that can
//   be recognized by a linear-bounded automaton (a Turing
//   machine with a tape that is bounded in size by a linear
//   function of the input length);
// - Recursively enumerable languages (type 0): languages that
//   can be recognized by a Turing machine.
//
// In our case, the lexical grammar is of type 3 (regular),
// while the syntactic grammar is of type 2 (context-free). We
// can't list each possible string of tokens that is valid in
// the grammar, since there are infinitely many of them;
// rather, we define a set of rules that describe how to
// combine tokens to form valid strings. These rules are called
// "productions" (since they "produce" strings), which act on
// symbols, forming expressions. The symbols are of one of two
// types: terminal (a unit from the grammar's alphabet, like
// the keyword "if" or the number "123", which does not expand
// further) and non-terminal (a symbol that references another
// production rule, like a variable in a mathematical formula).
// Production rules are tipically written down using the
// Backus-Naur form (BNF), a notation in the form "<symbol> :=
// _expr_", where _expr_ is a sequence of symbols and terminals
// that can be expanded into a valid string. For example, the
// BNF rule for an if statement in a C-like language could be
// written as:
//
//     if_statement := "if" "(" expression ")" statement
//                   | "if" "(" expression ")" statement "else" statement
//
// This rule states that an if statement is in the form
// "if(expr) statement [else statement]". The vertical bar "|"
// separates alternative productions, and the parentheses "()"
// are used for grouping.
//
// The AST (Abstract Syntax Tree) is the data structure used to
// represented the formal grammar of the language. Each of its
// nodes represents a construct in the language: statements or
// declarations, made of expressions, made of other
// expressions, and so on, forming a tree structure. The root
// of the tree is the program itself, and the leaves are the
// literals (numbers, strings, booleans, nil) or variables.

// An expression is something that ultimately produces a value.
// Expression in our language can be either:
// - A literal (number, string, boolean, or nil);
// - A unary operation (an operator applied on a single
//   expression)
// - A binary operation (two expressions separated by an
//   operator)
// - A logical operation (two expressions combined with a
//   logical operator)
// - A grouping (an expression enclosed in parentheses)
// - A variable (a reference to a value)
// - An assignment (putting a new value into a variable)
// - A function call (a function applied to a list of
//   arguments; the "paren" field is the closing parenthesis,
//   used for error reporting)
#[derive(Clone, PartialEq)]
pub enum Expr {
    Literal { value: Token },
    Unary { operator: Token, right: Box<Expr> },
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Logical { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Grouping { expression: Box<Expr> },
    Variable { name: Token },
    Assign { lhs: Token, rhs: Box<Expr> },
    Call { callee: Box<Expr>, arguments: Vec<Expr>, close_paren: Token},
    Lambda { params: Vec<Token>, body: Box<Stmt> },
}

impl Expr {
    pub fn literal(value: Token) -> Self {
        Expr::Literal { value }
    }

    pub fn unary(operator: Token, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn grouping(expression: Expr) -> Self {
        Expr::Grouping {
            expression: Box::new(expression),
        }
    }

    pub fn variable(name: Token) -> Self {
        Expr::Variable { name }
    }

    pub fn assign(name: Token, value: Expr) -> Self {
        Expr::Assign {
            lhs: name,
            rhs: Box::new(value),
        }
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn call(callee: Expr, arguments: Vec<Expr>, paren: Token) -> Self {
        Expr::Call {
            callee: Box::new(callee),
            arguments,
            close_paren: paren,
        }
    }

    pub fn lambda(params: Vec<Token>, body: Stmt) -> Self {
        Expr::Lambda {
            params,
            body: Box::new(body),
        }
    }
}

// A statement is a construct expressing some action to be
// carried out: declarations specify the data on which a
// program is to operate, while statements specify the actions
// to be taken with that data. In our language, statements are
// either:
// - An expression ("1 + 2 / 3"), producing a value;
// - A print statement ("print expr"), printing the value of
//   the argument expression;
// - A variable declaration ("let name [= expr]"), declaring a
//   label for some value, optionally initialized with an
//   expression;
// - A block ("{ stmt1; stmt2; ... }"), enclosing a sequence of
//   statements in a scope.
// - An if statement ("if expr block [else block]"), executing
//   the first block if the condition is true, the second block
//   if it is false.
// - A while statement ("while expr block"), executing the
//   block while the condition is true.
// - A for statement ("for var = start..stop [..step] block"),
//   executing the block for each value of the variable from
//   start to stop, optionally with a step.
// - A function declaration ("fn name(params) block"), defining
//   a function with a name, a list of parameters, and a block
//   of statements to execute.
#[derive(Clone, PartialEq)]
pub enum Stmt {
    Expression { expression: Expr },
    Var { name: Token, initializer: Option<Expr> },
    Block { statements: Vec<Stmt> },
    If { condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> },
    While { condition: Expr, body: Box<Stmt> },
    For { loop_var: Token, start: Expr, stop: Expr, step: Option<Expr>, body: Box<Stmt> },
    Function { name: Token, params: Vec<Token>, body: Box<Stmt> },
    Return { keyword: Token, value: Option<Expr> },
}

impl Stmt {
    pub fn expression(expression: Expr) -> Self {
        Stmt::Expression { expression }
    }

    pub fn var(name: Token, initializer: Option<Expr>) -> Self {
        Stmt::Var { name, initializer }
    }

    pub fn block(statements: Vec<Stmt>) -> Self {
        Stmt::Block { statements }
    }

    pub fn if_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }
    }

    pub fn while_stmt(condition: Expr, body: Stmt) -> Self {
        Stmt::While {
            condition,
            body: Box::new(body),
        }
    }

    pub fn for_stmt(loop_var: Token, start: Expr, stop: Expr, step: Option<Expr>, body: Stmt) -> Self {
        Stmt::For {
            loop_var,
            start,
            stop,
            step,
            body: Box::new(body),
        }
    }

    pub fn function(name: Token, params: Vec<Token>, body: Stmt) -> Self {
        Stmt::Function {
            name,
            params,
            body: Box::new(body),
        }
    }

    pub fn return_stmt(keyword: Token, value: Option<Expr>) -> Self {
        Stmt::Return { keyword, value }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Literal { value } => write!(f, "{}", value.lexeme),
            Expr::Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
            Expr::Binary { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
            Expr::Logical { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
            Expr::Grouping { expression } => write!(f, "{}", expression),
            Expr::Variable { name } => write!(f, "{}", name.lexeme),
            Expr::Assign { lhs, rhs } => write!(f, "{} = {}", lhs.lexeme, rhs),
            Expr::Call { callee, arguments, close_paren: _ } => {
                write!(f, "{}(", callee)?;
                for arg in arguments {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
            Expr::Lambda { params, body } => {
                write!(f, "fn (")?;
                for param in params {
                    write!(f, "{}, ", param.lexeme)?;
                }
                write!(f, ") ")?;
                write!(f, "{}", body)
            }
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Literal { value } => write!(f, "Expr::Literal({:?})", value),
            Expr::Unary { operator, right } => write!(f, "Expr::Unary({:?}, {:?})", operator, right),
            Expr::Binary { left, operator, right } => write!(f, "Expr::Binary({:?}, {:?}, {:?})", left, operator, right),
            Expr::Logical { left, operator, right } => write!(f, "Expr::Logical({:?}, {:?}, {:?})", left, operator, right),
            Expr::Grouping { expression } => write!(f, "Expr::Grouping({:?})", expression),
            Expr::Variable { name } => write!(f, "Expr::Variable({:?})", name),
            Expr::Assign { lhs, rhs } => write!(f, "Expr::Assign({:?}, {:?})", lhs, rhs),
            Expr::Call { callee, arguments, close_paren: _ } => {
                write!(f, "Expr::Call({:?}, {:?})", callee, arguments)
            }
            Expr::Lambda { params, body } => write!(f, "Expr::Lambda({:?}, {:?})", params, body),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Stmt::Expression { expression } => write!(f, "{}", expression),
            Stmt::Var { name, initializer } => match initializer {
                    Some(expr) => write!(f, "var {} = {}", name.lexeme, expr),
                    None => write!(f, "var {}", name.lexeme)
                },
            Stmt::Block { statements } => {
                writeln!(f, "{{\n")?;
                for stmt in statements {
                    writeln!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::If { condition, then_branch, else_branch } => {
                write!(f, "if {} {} ", condition, then_branch)?;
                if let Some(else_branch) = else_branch {
                    write!(f, "else {}", else_branch)
                } else {
                    Ok(())
                }
            }
            Stmt::While { condition, body } => {
                write!(f, "while {} {}", condition, body)
            }
            Stmt::For { loop_var, start, stop, step, body } => {
                write!(f, "for {} = {}..{} ", loop_var.lexeme, start, stop)?;
                if let Some(step) = step {
                    write!(f, "..{} ", step)
                } else {
                    Ok(())
                }?;
                write!(f, "{}", body)
            }
            Stmt::Function { name, params, body } => {
                write!(f, "fn {}(", name.lexeme)?;
                for param in params {
                    write!(f, "{}, ", param.lexeme)?;
                }
                write!(f, ") ")?;
                write!(f, "{}", body)
            }
            Stmt::Return { keyword: _, value } => {
                match value {
                    Some(expr) => write!(f, "return {}", expr),
                    None => write!(f, "return")
                }
            }
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Stmt::Expression { expression } => write!(f, "Stmt::Expression({:?})", expression),
            Stmt::Var { name, initializer } => write!(f, "Stmt::Variable({:?}, {:?})", name, initializer),
            Stmt::Block { statements } => write!(f, "Stmt::Block({:?})", statements),
            Stmt::If { condition, then_branch, else_branch } => write!(f, "Stmt::If({:?}, {:?}, {:?})", condition, then_branch, else_branch),
            Stmt::While { condition, body } => write!(f, "Stmt::While({:?}, {:?})", condition, body),
            Stmt::For { loop_var, start, stop, step, body } => write!(f, "Stmt::For({:?}, {:?}, {:?}, {:?}, {:?})", loop_var, start, stop, step, body),
            Stmt::Function { name, params, body } => write!(f, "Stmt::Function({:?}, {:?}, {:?})", name, params, body),
            Stmt::Return { keyword: _, value } => write!(f, "Stmt::Return({:?})", value),
        }
    }
}