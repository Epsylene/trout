use crate::token::{Token, TokenKind};
use crate::error::Error;
use crate::ast::*;

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
// expressions at a precedence level equal or higher to the
// previous one. In the end, we get:
//
//  expression := equality
//  equality   := comparison ( ( "!=" | "==" ) comparison )*
//  comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
//  term       := factor ( ( "-" | "+" ) factor )*
//  factor     := unary ( ( "/" | "*" ) unary )*
//  unary      := ( "!" | "-" ) unary | primary
//  primary    := NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
//
// Starting from the lowest precedence (the equality), we work
// our way up to primary expressions, which are juste literals
// or a grouping (closing the group of rules). Note that there
// is no left-recursion in the grammar (rules in the form "s :=
// s ..."), which we would not be able to parse without
// entering an infinite recursive loop. 

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn match_next(&self, kind: TokenKind) -> bool {
        !self.is_at_end() && self.zero().kind == kind
    }

    fn matches(&self, tokens: &[TokenKind]) -> bool {
        tokens.iter().any(|&kind| self.match_next(kind))
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
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

    fn expression(&mut self) -> Expr {
        // expression := equality
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        // equality := comparison ...
        let mut expr = self.comparison();

        // ... ( ( "!=" | "==" ) comparison )*
        while self.matches(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = self.zero();
            let right = self.comparison();
            expr = Expr::binary(expr, operator, right);
            
            self.advance();
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        // comparison := term ...
        let mut expr = self.term();

        // ... ( ( ">" | ">=" | "<" | "<=" ) term )*
        while self.matches(&[TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::Less, TokenKind::LessEqual]) {
            let operator = self.zero();
            let right = self.term();
            expr = Expr::binary(expr, operator, right);

            self.advance();
        }

        expr
    }

    fn term(&mut self) -> Expr {
        // term := factor ( ( "-" | "+" ) factor )*
        let mut expr = self.factor();

        while self.matches(&[TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.previous();
            let right = self.factor();
            expr = Expr::binary(expr, operator, right);

            self.advance();
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        // factor := unary ( ( "/" | "*" ) unary )*
        let mut expr = self.unary();

        while self.matches(&[TokenKind::Slash, TokenKind::Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::binary(expr, operator, right);
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        // unary := ( "!" | "-" ) unary | primary
        if self.matches(&[TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.previous();
            let right = self.unary();
            
            Expr::unary(operator, right)
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expr {
        // primary := NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
        if self.matches(&[TokenKind::Number, TokenKind::String, TokenKind::True, TokenKind::False, TokenKind::Nil]) {
            let t = self.advance();
            
            Expr::literal(t)
        } else if self.match_next(TokenKind::LeftParen) {
            self.advance(); // for the "("
            let expr = self.expression();
            self.advance(); // for the ")"

            Expr::grouping(expr)
        } else {
            // todo: add location (line, column) attributes to
            // tokens so that we can report errors in the
            // scanner too

            // Err(Error::new("Expected expression".to_string()))
            todo!()
        }
    }
}