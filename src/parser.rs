use crate::token::{Token, TokenKind};
use crate::error::{Error, ErrorKind, Result};
use crate::ast::{Expr, Stmt};

// The job of the scanner (or lexer) is to take a string of
// characters and convert it into a series of tokens. The job
// of the parser is to take a series of tokens and build an AST
// from it. There is a large number of different parser
// families, depending on the scanning or derivation strategy:
// top-down (start at the top grammar rule and work all the way
// to the leaves) or bottom-up (construct the sintax from ever
// larger combinations of primary expressions), leftmost
// derivation or rightmost derivation (which nonterminal to
// rewrite at each step)... Some known types of parsers are the
// LL(k) (top-down, leftmost, with k tokens of lookahead),
// LR(k) (bottom-up, righmost, with k tokens of lookahead),
// LALR (a LR(0) coupled with one token of lookahead, which is
// simpler and more efficient than a LR(1)), the Earley parser
// (top-down dynamic programming algorithm), the shunting yard
// algorithm (process the elements one symbol at a time,
// letting through literals and pushing operators on a separate
// stack, then popping when an operator with a lower precedence
// is pushed or there is nothing left), or the Packrate parser
// (which takes PEGs, parsing expression grammars, rather than
// LL grammars).
//
// We will use a recursive descent parser, which is the
// simplest way of traversing the sintax tree, translating each
// rule of the grammar into a function. To avoid ambiguity in
// expressions such as "1 * 2 / 3", we first need to define the
// precedence and associativity of each operator, such that
// from highest to lowest precedence:
// (1) Unary operators (! -) associate right;
// (2) Factor (* /) associate left;
// (3) Term (+ -) associate left;
// (4) Comparison (< > <= >=) associate left;
// (5) Equality (== !=) associate left.
// 
// To be able to "read" the precedence while parsing the
// grammar, we need to write it so that each rule matches
// expressions at a precedence level equal or lower to the
// following one, so that the parser calls the highest
// precedence rule (the highest precedence operators) on the
// leaves (literals) of the tree. Because equality < comparison
// < term < factor < unary, we get the following grammar:
//
//  expression := equality
//  equality   := comparison ( ( "!=" | "==" ) comparison )*
//  comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
//  term       := factor ( ( "-" | "+" ) factor )*
//  factor     := unary ( ( "/" | "*" ) unary )*
//  unary      := ( "!" | "-" ) unary | primary
//  primary    := NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
//
// Now, we want to extend this grammar with statements.
// Expressions produce values, while statements express actions
// to be carried out, like assignments, conditionals, or
// functions. Thus, the topmost rule of our grammar (the
// program itself) will be simply:
// 
//  program := statement* EOF
//  statement := expr_stmt | print_stmt
//  expr_stmt := expression ";"
//  print_stmt := "print" expression ";"
//
// A program is a list of statements ending with an EOF token;
// each statement is either an expression followed by ";", or
// the keyword "print", followed by an expression and ";".
// Then, starting from expression := equality, we work our way
// up to primary expressions, which are juste literals or a
// grouping (closing the group of rules). Note that there is no
// left-recursion in the grammar (rules in the form "s := s
// ..."), which we would not be able to parse without entering
// an infinite recursive loop.

//  program := declaration* EOF
//  declaration := var_decl | statement
//
//  var_decl := "var" IDENTIFIER ("=" expression)? ";"
//  statement := expr_stmt | print_stmt | block
//
//  expr_stmt := expression ";"
//  print_stmt := "print" expression ";"
//
//  expression := equality
//  equality   := comparison ( ( "!=" | "==" ) comparison )*
//  comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
//  term       := factor ( ( "-" | "+" ) factor )*
//  factor     := unary ( ( "/" | "*" ) unary )*
//  unary      := ( "!" | "-" ) unary | primary
//  primary    := NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn match_next(&self, expected: TokenKind) -> bool {
        !self.is_at_end() && self.zero().kind == expected
    }

    fn matches_one_in(&self, tokens: &[TokenKind]) -> bool {
        tokens.iter().any(|&kind| self.match_next(kind))
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn until(&mut self, expected: TokenKind, error: ErrorKind) -> Result<Token> {
        if self.match_next(expected) {
            Ok(self.advance())
        } else {
            Err(Error::new(&self.zero().location, error))
        }
    }

    fn is_at_end(&self) -> bool {
        self.zero().kind == TokenKind::Eof
    }

    fn zero(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        // program := declaration* EOF
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        
        // The program is a list of "declarations" terminated
        // by an EOF token.
        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => statements.push(decl),
                Err(e) => errors.push(e),
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        // declaration := var_decl | statement

        // A declaration is either a variable declaration or a
        // statement.
        if self.match_next(TokenKind::Var) {
            self.advance();
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        // var_decl := "var" IDENTIFIER ("=" expression)? ";"

        // A variable declaration is in the form "var name [=
        // expr];", where the expression assignment is
        // optional.
        let name = self.until(TokenKind::Identifier, ErrorKind::ExpectedIdentifier)?;
        let initializer = if self.match_next(TokenKind::Equal) {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };
        self.until(TokenKind::Semicolon, ErrorKind::ExpectedSemicolon)?;

        Ok(Stmt::var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt> {
        // statement := expr_stmt | print_stmt

        // A statement is either an expression followed by a
        // semicolon, or the keyword "print" followed by an
        // expression and a semicolon.
        if self.match_next(TokenKind::Print) {
            self.advance();
            let expr = self.expression()?;
            self.until(TokenKind::Semicolon, ErrorKind::ExpectedSemicolon)?;

            Ok(Stmt::print(expr))
        } else {
            let expr = self.expression()?;
            self.until(TokenKind::Semicolon, ErrorKind::ExpectedSemicolon)?;

            Ok(Stmt::expression(expr))
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        // expression := equality 

        // The first rule is that of any expression, which
        // calls the rule for the lowest precedence operator,
        // the equality.
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        // equality := comparison ( ( "!=" | "==" ) comparison )*

        // An equality is composed of a comparison...
        let mut expr = self.comparison()?;

        // ...followed by zero or more comparisons chained with
        // != or == operators (meaning that expressions such as
        // a == b == c are allowed, and are parsed
        // unambiguously as (a == b) == c, because of
        // left-associativity).
        while self.matches_one_in(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            // If there is match, advance to consume the
            // operator, then call the comparison rule, and
            // build a binary expression from the left
            // expression, the operator and the right
            // expression.
            let operator = self.advance();
            let right = self.comparison()?;
            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        // comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
        
        // Much like the equality, a comparison has a term...
        let mut expr = self.term()?;

        // ...followed by either >, >=, < or <= and another
        // term, zero or more times. In the same fashion,
        // expressions such as a < b < c are parsed as (a < b)
        // < c.
        while self.matches_one_in(&[TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::Less, TokenKind::LessEqual]) {
            let operator = self.advance();
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        // term := factor ( ( "-" | "+" ) factor )*

        // Then, a term is the composition of a factor...
        let mut expr = self.factor()?;

        // ...with another factor by either the + or -
        // operator, zero or more times.
        while self.matches_one_in(&[TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.advance();
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        // factor := unary ( ( "/" | "*" ) unary )*

        // And a factor, is a unary expression...
        let mut expr = self.unary()?;

        // ...a / or * and another unary expression, zero or
        // more times.
        while self.matches_one_in(&[TokenKind::Slash, TokenKind::Star]) {
            let operator = self.advance();
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        // unary := ( "!" | "-" ) unary | primary

        // A unary expression is first a ! or - operator
        // followed by another unary expression, or simply a
        // primary expression.
        if self.matches_one_in(&[TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.advance();
            let right = self.unary()?;
            
            Ok(Expr::unary(operator, right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr> {
        // primary := NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
        
        // And finally, a primary expression is either a
        // number, a string, true, false, nil or the grouping
        // of an expression (making it all come full circle!)
        if self.matches_one_in(&[TokenKind::Number, TokenKind::String, TokenKind::True, TokenKind::False, TokenKind::Nil]) {
            // If it is a literal, consume it right away
            Ok(Expr::literal(self.advance()))
        } else if self.match_next(TokenKind::LeftParen) {
            // If the matched token is a left paren, the
            // expression is a grouping. We first have to
            // consume the left paren, take the expression...
            self.advance();
            let expr = self.expression()?;
            // ...then consume the right paren (while checking
            // that it is really there!)
            self.until(TokenKind::RightParen, ErrorKind::ExpectedRightParen)?;

            Ok(Expr::grouping(expr))
        } else if self.match_next(TokenKind::Identifier) {
            // If it is an identifier, it is a variable
            // expression.
            Ok(Expr::variable(self.advance()))
        } else {
            // If it is none of the above, there is something
            // wrong going on.
            Err(Error::new(
                &self.zero().location, 
                ErrorKind::IncorrectPrimary(self.zero().lexeme))
            )
        }

        // The recursive descent ends here (or begins again at
        // the top level, in the case of a grouping). Note in
        // particular that recursive descent parsers like this
        // one, which descends the tree while only looking
        // ahead in the token list, are termed because of this
        // "predictive parsers". This is the simplest form of
        // parser (limited to LL(k) grammars), and it runs in
        // linear time, which is why it's widely used.
    }
}