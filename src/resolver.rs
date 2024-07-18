use crate::{
    ast::{Expr, Stmt}, 
    error::{Error, ErrorKind, Result}, 
    function::NATIVE_FUNCTIONS, 
    token::Token,
    value::Value,
};

use core::slice;
use std::collections::HashMap;

type ScopeBinding = HashMap<String, Binding>;
pub type ScopeDepth = HashMap<Token, usize>;

pub struct Resolver {
    bindings: Vec<ScopeBinding>,
    scopes: ScopeDepth,
}

enum Binding {
    Declaration,
    Definition,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver { 
            bindings: {
                // Put the native functions in the global scope
                // beforehand.
                let mut global = ScopeBinding::new();
                for func in NATIVE_FUNCTIONS.iter() {
                    if let Value::NativeFunction(native) = func {
                        global.insert(native.name.clone(), Binding::Definition);
                    }
                }
                vec![global]
            }, 
            scopes: ScopeDepth::new() }
    }

    pub fn depths(&self) -> &ScopeDepth {
        &self.scopes
    }

    fn declare(&mut self, var: &Token) {
        // From the current scope (the last one in the stack),
        // we insert a (name, binding) pair, where 'name' is
        // the identifier of the variable and a binding that
        // marks if the variable is in declaration or in
        // definition, so that we can catch the cases where the
        // variable is initialized with itself or a shadowed
        // variable.
        if let Some(scope) = self.bindings.last_mut() {
            scope.insert(var.lexeme.clone(), Binding::Declaration);
        }
    }

    fn define(&mut self, var: &Token) {
        // On definition, we update the variable binding tag,
        // so that it can now be given a value.
        if let Some(scope) = self.bindings.last_mut() {
            scope.insert(var.lexeme.clone(), Binding::Definition);
        }
    }

    pub fn resolve(&mut self, statements: &[Stmt]) -> Result<()> {
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression { expression } => self.resolve_expr(expression),
            Stmt::Block { statements } => self.block_stmt(statements),
            Stmt::Var { name, initializer } => self.var_stmt(name, initializer),
            Stmt::Function { name, params, body } => self.function_stmt(name, params, body),
            Stmt::If { condition, then_branch, else_branch } => self.if_stmt(condition, then_branch, else_branch),
            Stmt::Return { keyword: _, value } => self.return_stmt(value),
            Stmt::While { condition, body } => self.while_stmt(condition, body),
            Stmt::For { loop_var, start, stop, step, body } => self.for_stmt(loop_var, start, stop, step, body),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable { name } => self.var_expr(name),
            Expr::Assign { lhs, rhs } => self.assign_expr(lhs, rhs),
            Expr::Logical { left, operator: _, right } => self.binary_expr(left, right),
            Expr::Binary { left, operator: _, right } => self.binary_expr(left, right),
            Expr::Call { callee, arguments, close_paren: _ } => self.call_expr(callee, arguments),
            Expr::Lambda { params, body } => self.lambda_expr(params, body),
            Expr::Grouping { expression } => self.resolve_expr(expression),
            Expr::Unary { operator: _, right } => self.resolve_expr(right),
            Expr::Literal { value: _ } => Ok(()),
        }
    }

    fn resolve_scope(&mut self, name: &Token) -> Result<()> {
        // To find the scope of declaration of the variable, we
        // iterate in reverse order the scopes stack, from
        // innermost to outermost.
        for (i, scope) in self.bindings.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                // If the variable is found in a scope, then we
                // provide the interpreter the depth at which
                // the variable is situated.
                self.scopes.insert(name.clone(), self.bindings.len() - 1 - i);
                return Ok(())
            }
        }

        // If the variable is not found in any scope, it means
        // it has not been declared.
        Err(Error::new(
            &name.location,
            ErrorKind::VariableNotDeclared(name.lexeme.clone())
        ))
    }

    fn block_stmt(&mut self, body: &[Stmt]) -> Result<()> {
        // A block creates a new, temporary scope, so we push
        // one onto the stack, resolve the body, and then pop
        // when we're done.
        self.bindings.push(HashMap::new());
        self.resolve(body)?;
        self.bindings.pop();

        Ok(())
    }

    fn var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<()> {
        // Declaration and definition are separated in order to
        // avoid cases where a local variable is initialized
        // with the value of an outer scope variable of the
        // same name (that is, code of the form "var a = ...; {
        // var a = a; }") or with itself. First, we declare the
        // variable, then resolve the initializer expression,
        // if any...
        self.declare(name);
        if let Some(initializer) = initializer {
            self.resolve_expr(initializer)?;
        }

        // ...and only then define it, if there was no error
        // during the resolution of the initializer (like
        // binding to the same name). Note that we define even
        // if there is no initializer: the variable will just
        // be set to Nil at runtime.
        self.define(name);

        Ok(())
    }

    fn function_stmt(&mut self, name: &Token, params: &[Token], body: &Stmt) -> Result<()> {
        // Functions are declared and defined at the same time,
        // so we bind the name tag directly.
        self.declare(name);
        self.define(name);

        // Then, a scope is created for the function's body, in
        // which we readily declare and define the parameters,
        // since we need to be able to "use" them in the body
        // and they take values from the arguments in a call.
        self.bindings.push(ScopeBinding::new());
        for param in params {
            self.declare(param);
            self.define(param);
        }

        // Then we can resolve the body itself, and pop the
        // function scope at the end.
        self.resolve(slice::from_ref(body))?;
        self.bindings.pop();

        Ok(())
    }

    fn if_stmt(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: &Option<Box<Stmt>>) -> Result<()> {
        // The condition is resolved first, then the branches.
        self.resolve_expr(condition)?;
        self.resolve_stmt(then_branch)?;
        if let Some(else_branch) = else_branch {
            self.resolve_stmt(else_branch)?;
        }

        Ok(())
    }

    fn return_stmt(&mut self, value: &Option<Expr>) -> Result<()> {
        // If there is a return value, we resolve it.
        if let Some(value) = value {
            self.resolve_expr(value)?;
        }

        Ok(())
    }

    fn while_stmt(&mut self, condition: &Expr, body: &Stmt) -> Result<()> {
        // The condition is resolved first, then the body.
        self.resolve_expr(condition)?;
        self.resolve_stmt(body)?;

        Ok(())
    }

    fn for_stmt(&mut self, loop_var: &Token, start: &Expr, stop: &Expr, step: &Option<Expr>, body: &Stmt) -> Result<()> {
        // The loop variable is declared and defined first.
        self.declare(loop_var);
        self.define(loop_var);

        // Then, the start, stop, and step expressions are
        // resolved...
        self.resolve_expr(start)?;
        self.resolve_expr(stop)?;
        if let Some(step) = step {
            self.resolve_expr(step)?;
        }

        // ...and the body last.
        self.resolve_stmt(body)?;

        Ok(())
    }

    fn var_expr(&mut self, var: &Token) -> Result<()> {
        // In the current scope...
        let scope = self.bindings.last().expect("No scope found");

        // ...check if the variable is in "declaration" state
        // to catch auto-initializations.
        if let Some(Binding::Declaration) = scope.get(&var.lexeme) {
            return Err(Error::new(
                &var.location,
                ErrorKind::AutoInitialization(var.lexeme.clone())
            ))
        }

        // If it has been defined, we can resolve its innermost
        // scope of definition (where in the program it was
        // defined last).
        self.resolve_scope(var)
    }

    fn assign_expr(&mut self, lhs: &Token, rhs: &Expr) -> Result<()> {
        // We first resolve the right-hand side of the
        // equation, then resolve the scope of the left-hand
        // side being assigned to.
        self.resolve_expr(rhs)?;
        self.resolve_scope(lhs)?;

        Ok(())
    }

    fn binary_expr(&mut self, left: &Expr, right: &Expr) -> Result<()> {
        // We resolve the left and right expressions of the
        // binary operation.
        self.resolve_expr(left)?;
        self.resolve_expr(right)?;

        Ok(())
    }

    fn call_expr(&mut self, callee: &Expr, arguments: &[Expr]) -> Result<()> {
        // First resolve the callee expression, then each of
        // the arguments.
        self.resolve_expr(callee)?;
        for arg in arguments {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }

    fn lambda_expr(&mut self, params: &[Token], body: &Stmt) -> Result<()> {
        // Lambdas are like functions, but without a name. We
        // create a scope for the lambda's body, declare and
        // define the parameters, resolve the body, and then
        // pop the scope.
        self.bindings.push(HashMap::new());
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve(slice::from_ref(body))?;
        self.bindings.pop();

        Ok(())
    }
}