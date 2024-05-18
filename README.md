# trout

![minecraft trout](img/trout.webp)

Interpreter for a simple dynamically-typed programming
language, from the book [*Crafting
Interpreters*](https://craftinginterpreters.com/) by Robert
Nystrom, implemented in Rust. The program consists of three main parts:

1. A scanner that reads the source code, either from a file or a REPL, and produces a sequence of tokens.
2. A parser that produces an abstract sintax tree from the sequence of tokens.
3. An interpreter that traverses the AST and executes the code.

## Sintax

The language is a simple dynamically-typed language with the following sintax:

- Literals: integers `123`, floats `123.45`, strings `"hello"`, booleans `true`, `false`, void value `nil`
- Arithmetic operators: `+`, `-`, `*`, `/`
- Comparison operators: `>`, `<`, `>=`, `<=`, `==`, `!=`
- Logical operators: `and`, `or`
- Conditionals: `if condition { ... } else { ... }`
- Loops: `while condition { ... }`, `for i=a..b { ... }`
- Functions: `fn add(a, b) { ... }`
- Variables: `var a;`, `x = 10;`

### Binary operators
1. Arithmetic operators are binary infix operators with the usual precedence rules. Ints are casted to floats if the other operand is a float, except for the division `/`, where both a operands are casted to floats. The `+` operator is overloaded to concatenate strings.

2. Comparison operators only accept numbers, casting ints to floats if one of the operands is a float. The equality operators `==` and `!=` can be used with any two types.

3. The logical operators are `&&` (AND) and `||` (OR), the first taking precedence over the second. Operands can be of any type, with `false`, `nil`, `0` and `0.0` evaluated to false, and anything else to true.
   
4. All binary operators are left-associative and can be chained (`a < b < c`). Logical operators take precedence over comparison operators, which take precedence over arithmetic operators.

### Variables and assignment
Variables can be assigned and re-assigned to any value. Variables can be declared without an initial value, but they must be assigned before being used. Assignements are expressions that return the assigned value, so they can be used in other statements or expressions like `a = b = 5;` or `var x = (y = 10);`.

### Conditionals and loops

1. Conditionals are implemented with the `if` statement in the form `if condition { ... } else { ... }`. The `else` block is optional and can be omitted.
2. `while` loops are implemented with the sintax `while condition { ... }`. The condition is evaluated before each iteration.
3. `for` loops are implemented with the sintax `for i=a..b..s { ... }`. The loop variable `i` is initialized with value `a` and incremented by `s` at the end of each iteration; the loop runs while `i < b`. The increment `s` is optional and defaults to 1.

### Functions

Functions are defined with the sintax `fn name(a, b, ...) { ... }`.  Functions can be defined inside other functions. The arguments and return value of a function can be of any type, including other functions, which are passed around using their identifier.