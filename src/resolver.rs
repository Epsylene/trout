use crate::{
    ast::{Expr, Stmt}, 
    error::{Result, Error, ErrorKind}, 
    interpreter::Interpreter,
    token::Token,
};

use std::collections::HashMap;

// 
struct Resolver {
    interpreter: Interpreter,
    scopes: Vec<HashMap<String, Binding>>,
}

enum Binding {
    Declaration,
    Definition,
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Self {
        Resolver { interpreter, scopes: Vec::new() }
    }

    pub fn resolve(&mut self, statements: &[Stmt]) {
        for stmt in statements {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block { statements } => self.block_stmt(statements),
            Stmt::Var { name, initializer } => self.var_stmt(name, initializer),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable { name } => self.var_expr(name),
            Expr::Assign { lhs, rhs } => self.assign_expr(lhs, rhs),
        }
    }

    fn resolve_scope(&self, expr: &Expr, name: &Token) {
        // To find the scope of definition of the variable, we
        // iterate in reverse order (from innermost to
        // outermost) the scopes stack.
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                // If the variable is found in a scope, then we
                // can ask the interpreter to actually resolve
                // it, providing the depth at which the
                // variable is situated.
                self.interpreter.resolve(expr, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn block_stmt(&mut self, body: &[Stmt]) {
        // A block creates a new, temporary scope, so we push
        // one onto the stack, resolve the body, and then pop
        // when we're done.
        self.scopes.push(HashMap::new());
        self.resolve(body);
        self.scopes.pop();
    }

    fn var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) {
        // Declaration and definition are separated in order to
        // avoid cases where a local variable is initialized
        // with the value of an outer scope variable of the
        // same name (that is, code of the form "var a = ...; {
        // var a = a; }"). First, we declare the variable, then
        // resolve the initializer expression, if any...
        self.declare(name);
        if let Some(initializer) = initializer {
            self.resolve_expr(initializer);
        }

        // ...and only then define it, if there was no error
        // during the resolution of the initializer (like
        // binding to the same name). Note that we define even
        // if there is no initializer: the variable will just
        // be set to Nil at runtime.
        self.define(name);
    }

    fn declare(&mut self, var: &Token) -> Result<()> {
        // From the current scope (the last one in the stack),
        // we insert a (name, binding) pair, where 'name' is
        // the identifier of the variable and a binding that
        // marks if the variable is in declaration or in
        // definition, so that we can catch the cases where the
        // variable is initialized with itself or a shadowed
        // variable.
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(var.lexeme.clone(), Binding::Declaration);
        }

        Ok(())
    }

    fn define(&mut self, var: &Token) -> Result<()> {
        // On definition, we update the variable binding tag,
        // so that it can now be given a value.
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(var.lexeme.clone(), Binding::Definition);
        }

        Ok(())
    }

    fn var_expr(&mut self, var: &Token) -> Result<()> {
        // If there is a scope and a variable with that name...
        if let Some(scope) = self.scopes.last() {
            if let Some(binding) = scope.get(&var.lexeme) {
                match binding {
                    // ...check that it has been defined, and
                    // not only declared, so as to avoid
                    // auto-initializations.
                    Binding::Declaration => {
                        return Error::new(
                            &var.location, 
                            ErrorKind::VariableNotDefined(var.lexeme.clone())
                        ).into();
                    }
                    // If it has been defined, we can resolve
                    // its scope of definition (where in the
                    // program it has been defined).
                    Binding::Definition => self.resolve_scope(var),
                }
            }
        }

        Ok(())
    }
}