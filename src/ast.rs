use crate::token::Token;

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
// nodes represents a construct in the language: internal nodes
// are made of operators (in the broad sense), which build one
// upon each other until reaching the terminal symbols in the
// leaves (like numbers or strings).

pub trait Expr {
    // fn print(&self);
}

macro_rules! make_expr {
    ($name: ident, $($element: ident: $ty: ty),*) => {
        struct $name { $($element: $ty),* }

        impl Expr for $name {
            // fn print(&self) {
            //     println!("{:?}", self);
            // }
        }
    }
}

// Let us define a grammar for our language. Any expression in
// the language can be either:
// - A literal (number, string, boolean, or nil);
// - A unary operation (a single ! or - operator followed by an
//   expression)
// - A binary operation (two expressions separated by an
//   operator)
// - A grouping (an expression enclosed in parentheses)
make_expr!(Literal, value: Token);
make_expr!(Unary, operator: Token, right: Box<dyn Expr>);
make_expr!(Binary, left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>);
make_expr!(Grouping, expression: Box<dyn Expr>);