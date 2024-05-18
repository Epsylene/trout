use crate::token::{Token, TokenKind, TokenMatch};
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
// rule of the grammar into a construct of the language. To
// avoid ambiguity in expressions such as "1 * 2 / 3", we first
// need to define the precedence and associativity of each
// operator, such that from highest to lowest precedence:
//
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
// leaves (literals) of the tree. That is, because equality <
// comparison < term < factor < unary, we get a grammar such
// as:
//
//      expression := equality
//      equality   := comparison ( ( "!=" | "==" ) comparison )*
//      ...
//
// To create a language, we also want statements and
// declarations. Expressions produce values; statements use
// expressions to carry out some action; declarations bind data
// to labels. The definition of a program, then, is a list of
// statements and/or declarations, based on expressions, and
// terminated by an EOF token. Starting from the the rule for a
// statement and that for a declaration, we can build a tree
// that fully describes the sintax of a given program.

// The full grammar for the language is presented below. Note
// that there is no left-recursion (rules in the form "s := s
// ..."), which we would not be able to parse without entering
// an infinite recursive loop.
//
//  program := (declaration | statement)* EOF
//
//  declaration := var_decl | fn_decl
//  var_decl := "var" IDENTIFIER ("=" expression)?
//  fn_decl := "fn" IDENTIFIER "(" parameters? ")" block
//  parameters := IDENTIFIER ("," IDENTIFIER)*
//
//  statement := expr_stmt | print_stmt | block 
//                  | if_stmt | while_stmt | for_stmt
//
//  block := "{" declaration* "}"
//  expr_stmt := expression
//  print_stmt := "print" expression
//  if_stmt := "if" expression block ("else" block)?
//  while_stmt := "while" expression block
//  for_stmt := "for" IDENTIFIER "=" expression ".."
//              expression (".." expression)? block
//
//  expression := assignment
//  assignment := IDENTIFIER "=" assignment | logical_or
//  logical_or := logical_and ( "or" logical_and )*
//  logical_and := equality ( "and" equality )*
//  equality   := comparison ( ( "!=" | "==" ) comparison )*
//  comparison := term ( ( ">" | ">=" | "<" | "<=" | "and" | "or" ) term )*
//  term       := factor ( ( "-" | "+" ) factor )*
//  factor     := unary ( ( "/" | "*" ) unary )*
//  unary      := ( "!" | "-" ) unary | call
//  call       := primary ( "(" arguments? ")" )*
//  primary    := NUMBER | STRING | "true" | "false" | "nil" 
//                  | "(" expression ")" | IDENTIFIER
//  arguments  := expression ( "," expression )*
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn match_next(&self, expected: impl TokenMatch) -> bool {
        !self.is_at_end() && expected.matches(&self.zero())
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn consume(&mut self, expected: impl TokenMatch, error: ErrorKind) -> Result<Token> {
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
        // program := (declaration | statement)* EOF
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        
        // The program is a list of "constructs" (declarations
        // or statements) terminated by an EOF token.
        while !self.is_at_end() {
            match self.traverse() {
                Some(Ok(stmt)) => statements.push(stmt),
                Some(Err(e)) => {
                    self.advance();
                    errors.push(e);
                },
                None => (),
            }
        }

        match errors.len() {
            0 => Ok(statements),
            _ => Err(errors),
        }
    }

    fn traverse(&mut self) -> Option<Result<Stmt>> {
        // Choose between:
        match self.zero().kind {
            // - An empty statement (an extra semicolon or a
            //   blank line, for example)
            TokenKind::Semicolon | TokenKind::Newline => {
                self.advance();
                None
            },
            // - A variable declaration;
            TokenKind::Var => Some(self.declaration()),
            // - A function declaration;
            TokenKind::Fn => Some(self.function()),
            // - A regular statement.
            _ => Some(self.statement()),
        }
    }

    fn function(&mut self) -> Result<Stmt> {
        // fn_decl := "fn" IDENTIFIER "(" parameters? ")" block
        self.advance(); // Consume the "fn" keyword

        // A function declaration is first comprised of a name,
        // which is an identifier...
        let name = self.consume(TokenKind::Identifier, ErrorKind::ExpectedIdentifierFn)?;
        self.consume(TokenKind::LeftParen, ErrorKind::ExpectedLeftParenFn)?;

        // ...followed by a list of parameters, which are also
        // identifiers.
        let mut parameters = Vec::new();
        while !self.match_next(TokenKind::RightParen) {
            // Consume the parameter and add it to the list.
            parameters.push(self.consume(TokenKind::Identifier, ErrorKind::ExpectedIdentifierArg)?);

            // Check the comma separating parameters...
            if !self.match_next(TokenKind::Comma) {
                break;
            }

            // ...and consume it.
            self.advance();
        }

        // After the parameters, comes the body of the
        // function, which is just a block of statements.
        self.consume(TokenKind::RightParen, ErrorKind::ExpectedRightParenFn)?;
        let body = self.block()?;

        Ok(Stmt::function(name, parameters, body))
    }

    fn declaration(&mut self) -> Result<Stmt> {
        // var_decl := "var" IDENTIFIER ("=" expression)?

        // A variable declaration is comprised of an
        // identifier, following the "var" keyword...
        self.advance(); // Consume the "var" keyword
        let name = self.consume(TokenKind::Identifier, ErrorKind::ExpectedIdentifierAssignment)?;
        
        // ...and optionally an initializer expression.
        let initializer = if self.match_next(TokenKind::Equal) {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&[TokenKind::Semicolon, TokenKind::Newline], ErrorKind::ExpectedSeparator)?;

        Ok(Stmt::var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt> {
        // statement := expr_stmt | print_stmt | block
        //  | if_stmt | while_stmt | for_stmt

        match self.zero().kind {
            TokenKind::If => self.if_stmt(),
            TokenKind::LeftBrace => self.block(),
            TokenKind::While => self.while_stmt(),
            TokenKind::For => self.for_stmt(),
            _ => self.expr_stmt(),
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        // if_stmt := "if" expression block ("else" block)?

        self.advance(); // Consume the "if" keyword

        // An if statement is comprised of three parts: a
        // condition, in the form of an expression...
        let condition = self.expression()?;

        // ...a block of statements to execute if the condition
        // is true (the "then" branch)...
        let then_branch = self.block()?;

        // ...and an optional block of statements to execute if
        // the condition is false (the "else" branch).
        let else_branch = if self.match_next(TokenKind::Else) {
            self.advance();
            Some(self.block()?)
        } else {
            None
        };

        // A note on the sintax: in other languages, if-else
        // clauses might not have neatly delimited execution
        // blocks, but something of the form "if cond a else
        // b". This poses a problem, however, in the case of
        // nested conditionals: the statement "if c1 if c2 a
        // else b" can be parsed as "if c1 { if c2 a } else b"
        // as well as "if c1 { if c2 a else b }". This is known
        // as the "dangling else" problem, which is
        // conventionally solved by attaching the else to the
        // innermost if. In the theory of LR parsers, this is
        // an example of a shift-reduce conflict (where "shit"
        // is the action of advancing the parser by one token
        // and "reduce" to join recently parsed nodes in a tree
        // by applying a grammar rule).

        Ok(Stmt::if_stmt(condition, then_branch, else_branch))
    }

    fn block(&mut self) -> Result<Stmt> {
        // block := "{" declaration* "}"

        self.advance(); // Consume the left brace
        let mut statements = Vec::new();
        
        // A block is like a subprogram, with its own tree to
        // traverse.
        while !self.match_next(TokenKind::RightBrace) && !self.is_at_end() {
            match self.traverse() {
                Some(Ok(stmt)) => statements.push(stmt),
                Some(Err(e)) => return Err(e),
                None => (),
            }
        }

        // After all statements have been parsed, consume until
        // reaching the right brace.
        self.consume(TokenKind::RightBrace, ErrorKind::ExpectedRightBrace)?;

        Ok(Stmt::block(statements))
    }

    fn while_stmt(&mut self) -> Result<Stmt> {
        // while_stmt := "while" expression block

        self.advance(); // Consume the "while" keyword

        // A while statement is comprised of a condition and a
        // block of statements to execute while the condition
        // is true.
        let condition = self.expression()?;
        let body = self.block()?;

        Ok(Stmt::while_stmt(condition, body))
    }

    fn for_stmt(&mut self) -> Result<Stmt> {
        // for_stmt := "for" IDENTIFIER "=" expression ".."
        //              expression (".." expression)? block

        self.advance(); // Consume the "for" keyword

        // The for statement first takes an identifier, which
        // will be the loop variable.
        let loop_var = self.consume(TokenKind::Identifier, ErrorKind::ExpectedIdentifierAssignment)?;
        self.consume(TokenKind::Equal, ErrorKind::ForExpectedEqual)?;
        
        // Then a start value, given from an expression...
        let start = self.expression()?;
        self.consume(TokenKind::DotDot, ErrorKind::ForExpectedDoubleDot)?;
        
        // ...a stop value...
        let stop = self.expression()?;
        
        // ...and optionally a step value.
        let step = if self.match_next(TokenKind::DotDot) {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };
        let body = self.block()?;

        Ok(Stmt::for_stmt(loop_var, start, stop, step, body))
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        // expr_stmt := expression

        // An expression statement is just an expression
        // followed by a separator.
        let expr = self.expression()?;
        self.consume(&[TokenKind::Semicolon, TokenKind::Newline], ErrorKind::ExpectedSeparator)?;

        Ok(Stmt::expression(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        // expression := assignment

        // The first rule is that of any expression, which
        // calls the rule for the lowest precedence operator,
        // the equality.
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        // assignment := IDENTIFIER "=" assignment | equality

        // An assignment puts into an identifier an expression,
        // potentially itself an assignment. The left-hand side
        // of the assignment, however, can also be an
        // expression (think of accessing the field of a
        // struct, for example): to account for this, we shall
        // parse first the l-value with logical_or() (which
        // calls the rest of the expression tree)...
        let mut lhs = self.logical_or()?;

        // ...advancing the parser (which we recall has only
        // one token of lookahead) enough to check if the next
        // token is an equal sign and then parse the right-hand
        // side of the assignment. If not, then this is not an
        // assignment, but a simple expression, which has
        // already been parsed.
        if self.match_next(TokenKind::Equal) {
            // Parsing the right-hand side of the assignment
            // means calling assignment() again, since it is
            // right-associative (a = b = c = ...).
            let equals = self.advance();
            let rhs = self.assignment()?;

            // Once this is done, we can check if the LHS is an
            // actual variable we can assign to (and not an
            // expression of the type "a + b = c", where a + b
            // is clearly an r-value, not an l-value).
            if let Expr::Variable { name } = lhs {
                lhs = Expr::assign(name, rhs);
            } else {
                return Err(Error::new(
                    &equals.location,
                    ErrorKind::ExpectedIdentifierAssignment
                ));
            }
        }

        Ok(lhs)
    }

    fn logical_or(&mut self) -> Result<Expr> {
        // logical_or := logical_and ( "or" logical_and )*

        // The logical OR operator is left-associative, and
        // consists of "logical and" expressions separated by
        // the "or" keyword. It is important to separate the
        // two rules in this way so as to give precedence to OR
        // over AND.
        let mut expr = self.logical_and()?;

        // While we have an "or" keyword after the just-parsed
        // expression (meaning that expressions such as a == b
        // == c are allowed, and are parsed unambiguously as (a
        // == b) == c, because of left-associativity)...
        while self.match_next(TokenKind::Or) {
            // ...keep parsing the right side of the operator
            // and add branches to this subtree. 
            let operator = self.advance();
            let right = self.logical_and()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr> {
        // logical_and := equality ( "and" equality )*

        // The logical AND operator is left-associative, and
        // consists of equality expressions separated by the
        // "and" keyword.
        let mut expr = self.equality()?;

        // Parse the operator chain.
        while self.match_next(TokenKind::And) {
            let operator = self.advance();
            let right = self.equality()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        // equality := comparison ( ( "!=" | "==" ) comparison )*

        // An equality is composed of a comparison...
        let mut expr = self.comparison()?;

        // ...followed by zero or more comparisons chained with
        // != or == operators .
        while self.match_next(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
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

        // ...followed by a comparison operator and another
        // term, zero or more times. Like equality, expressions
        // such as a < b < c are parsed as (a < b) < c.
        while self.match_next(&[TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::Less, TokenKind::LessEqual]) {
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
        while self.match_next(&[TokenKind::Minus, TokenKind::Plus]) {
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
        while self.match_next(&[TokenKind::Slash, TokenKind::Star]) {
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
        if self.match_next(&[TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.advance();
            let right = self.unary()?;
            
            Ok(Expr::unary(operator, right))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        // call := primary ( "(" arguments? )*

        // A call expression is a primary expression (an
        // identifier, typically)...
        let mut expr = self.primary()?;

        // ...followed by an argument list between parentheses.
        // Since functions can return functions, we can have
        // code of the form "f()()()...". To account for this,
        // we keep parsing arguments while matching for left
        // parens, and update each time the callee of the
        // expression with the returned call expression (that
        // is, f(a)(b,c) has callee f(a) and arglist (b,c), and
        // f(a) has callee f and arglist (a)).
        while self.match_next(TokenKind::LeftParen) {
            expr = self.arguments(expr)?;
        }

        Ok(expr)
    }

    fn arguments(&mut self, callee: Expr) -> Result<Expr> {        
        self.advance(); // Consume the left paren
        let mut arguments = Vec::new();
        
        // While we have not reached the right paren, keep
        // parsing arguments and adding them to the list.
        while !self.match_next(TokenKind::RightParen) {
            arguments.push(self.expression()?);

            // If we do not encounter a comma after an
            // argument, it means that we have reached the end
            // of the argument list, and do not need to advance
            // the parser anymore.
            if !self.match_next(TokenKind::Comma) {
                break;
            }

            // Otherwise, consume the comma and keep parsing.
            self.advance();
        }

        // Consume the right paren.
        let paren = self.consume(TokenKind::RightParen, ErrorKind::ExpectedRightParenFn)?;

        // The result is a call expression with a single
        // argument list, in the form f(...), which can then be
        // composed with other calls.
        Ok(Expr::call(callee, arguments, paren))
    }

    fn primary(&mut self) -> Result<Expr> {
        // primary := NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
        
        // And finally, a primary expression is either a
        // number, a string, true, false, nil or the grouping
        // of an expression (making it all come full circle!)
        if self.match_next(&[TokenKind::Number, TokenKind::String, TokenKind::True, TokenKind::False, TokenKind::Nil]) {
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
            self.consume(TokenKind::RightParen, ErrorKind::ExpectedRightParenGrouping)?;

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::literal::LiteralType;
    use crate::scanner::Scanner;
    use crate::token::{TokenKind, Location};

    fn parse(input: &str) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_empty_program() {
        let result = parse("");
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_var_declaration() {
        let result = parse("var a = 1;");
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0], 
            Stmt::var(
                Token::new(TokenKind::Identifier, "a".to_string(), LiteralType::Nil, Location::new(1, 5)),
                Some(Expr::literal(Token::new(TokenKind::Number, "1".to_string(), LiteralType::Int(1), Location::new(1, 9)))))
        );
    }

    #[test]
    fn test_block_statement() {
        let result = parse("{ var a = 1; }");
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0],
            Stmt::block(vec![
                Stmt::var(
                    Token::new(TokenKind::Identifier, "a".to_string(), LiteralType::Nil, Location::new(1, 7)),
                    Some(Expr::literal(Token::new(TokenKind::Number, "1".to_string(), LiteralType::Int(1), Location::new(1, 11)))
                )),
            ])
        );
    }

    #[test]
    fn test_if_statement() {
        let result = parse("if true { 1; } else { 2; }");
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0],
            Stmt::if_stmt(
                Expr::literal(Token::new(TokenKind::True, "true".to_string(), LiteralType::Bool(true), Location::new(1, 4))),
                Stmt::block(vec![
                    Stmt::expression(
                        Expr::literal(Token::new(TokenKind::Number, "1".to_string(), LiteralType::Int(1), Location::new(1, 11)))
                    )
                ]),
                Some(Stmt::block(vec![
                    Stmt::expression(
                        Expr::literal(Token::new(TokenKind::Number, "2".to_string(), LiteralType::Int(2), Location::new(1, 23)))
                    )
                ]))
            )
        );
    }

    #[test]
    fn test_while_statement() {
        let result = parse("while true { 1; }");
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0],
            Stmt::while_stmt(
                Expr::literal(Token::new(TokenKind::True, "true".to_string(), LiteralType::Bool(true), Location::new(1, 7))),
                Stmt::block(vec![
                    Stmt::expression(
                        Expr::literal(Token::new(TokenKind::Number, "1".to_string(), LiteralType::Int(1), Location::new(1, 14)))
                    )
                ])
            )
        );
    }

    #[test]
    fn test_for_statement() {
        let result = parse("for i = 0..10 { i; }");
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0],
            Stmt::for_stmt(
                Token::new(TokenKind::Identifier, "i".to_string(), LiteralType::Nil, Location::new(1, 5)),
                Expr::literal(Token::new(TokenKind::Number, "0".to_string(), LiteralType::Int(0), Location::new(1, 9))),
                Expr::literal(Token::new(TokenKind::Number, "10".to_string(), LiteralType::Int(10), Location::new(1, 12))),
                None,
                Stmt::block(vec![
                    Stmt::expression(
                        Expr::variable(Token::new(TokenKind::Identifier, "i".to_string(), LiteralType::Nil, Location::new(1, 17)))
                    )
                ])
            )
        );
    }
}