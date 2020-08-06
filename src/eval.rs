use std::fmt;

use crate::{intern, ast, parser, lexer};

#[derive(Debug)]
pub enum EvalError {
    TypeError {
        loc: ast::Span,
        reason: String,
    },
    ParseError(Vec<parser::ParseError>),
}

impl EvalError {
    fn type_error(loc: ast::Span, msg: String) -> Self {
        EvalError::TypeError {
            loc,
            reason: format!("[{}:{}] Type error: {}",
                            loc.line, loc.col, msg),
        }
    }
}

pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
        }
    }
}

impl Object {
    pub fn kind(&self) -> &'static str {
        match *self {
            Object::Int(_) => "Int",
            Object::Bool(_) => "Bool",
            Object::Null => "null",
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Object::Int(0) | Object::Bool(false) => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub struct Environment {}

#[derive(Debug)]
pub struct Interpreter {
    env: Environment,
    intern: intern::Intern,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment{},
            intern: intern::Intern::new_with_capacity(1024),
        }
    }
    pub fn eval(&mut self, code: &str) -> EvalResult {
        let mut l = lexer::Lexer::new(code, &mut self.intern);
        let toks = l.lex_to_vec();
        let mut parser = parser::Parser::new(toks);
        let (prog, errs) = parser.parse_program();
        if errs.len() > 0 {
            return Err(EvalError::ParseError(errs))
        }
        let mut res = Object::Null;
        for stmt in prog.stmts.iter() {
            res = self.eval_statement(stmt)?;
        }
        Ok(res)
    }

    fn eval_statement(&mut self, stmt: &ast::Statement) -> EvalResult {
        match stmt.kind {
            ast::StatementKind::Let {ident, ref value} => {
                todo!()
            },
            ast::StatementKind::Return { ref value } => {
                todo!()
            },
            ast::StatementKind::Expression { ref value } => {
                self.eval_expr(value)
            }
        }
    }

    fn eval_expr(&mut self, expr: &ast::Expression) -> EvalResult {
        match expr.kind {
            ast::ExpressionKind::Int(i) => Ok(Object::Int(i)),
            ast::ExpressionKind::Bool(b) => Ok(Object::Bool(b)),
            ast::ExpressionKind::PrefixOp { operator,
                                            ref expr} => self.eval_prefix(expr.loc,operator, &expr),
            ast::ExpressionKind::InfixOp { operator,
                                           ref left,
                                           ref right} => self.eval_infix(expr.loc, operator, &left, &right),
            ref e => todo!("{:?} is not implemented yet", e),
        }
    }

    fn eval_prefix(&mut self, loc: ast::Span, op: ast::PrefixOperator, expr: &ast::Expression) -> EvalResult {
        let expr = self.eval_expr(expr)?;
        match op {
            ast::PrefixOperator::Neg => {
                match expr {
                    Object::Int(i) => Ok(Object::Int(-i)),
                    o => Err(EvalError::type_error(loc, format!("Expected '-Int' but got '-{}'", o.kind()))),
                }
            },
            ast::PrefixOperator::Bang => {
                Ok(Object::Bool(!expr.truthy()))
            }
        }
    }

    fn eval_infix(&mut self, loc: ast::Span, op: ast::InfixOperator, left: &ast::Expression, right: &ast::Expression) -> EvalResult {
        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;
        match op {
            ast::InfixOperator::Eq => Ok(Object::Bool(left == right)),
            ast::InfixOperator::NotEq => Ok(Object::Bool(left != right)),
            ast::InfixOperator::LtEq => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l <= r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int <= Int' but got '{} <= {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::GtEq => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l >= r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int >= Int' but got '{} >= {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Lt => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l < r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int < Int' but got '{} < {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Gt => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l > r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int > Int' but got '{} > {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Plus => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int + Int' but got '{} + {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Minus => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int - Int' but got '{} - {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Star => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int * Int' but got '{} * {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Slash => {
                match (left, right) {
                    (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l / r)),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int + Int' but got '{} + {}'", l.kind(), r.kind())))
                }
            },
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atoms() {
        let mut interp = Interpreter::new();
        let tests = [
            ("1", Object::Int(1)),
            ("2", Object::Int(2)),
            ("3", Object::Int(3)),
            ("true", Object::Bool(true)),
            ("false", Object::Bool(false)),
            ("1; 2; 3", Object::Int(3)),
        ];
        for (test, expected) in tests.iter() {
            let got = interp.eval(test).unwrap();
            assert_eq!(*expected, got)
        }
    }

    #[test]
    fn test_prefix() {
        let mut interp = Interpreter::new();
        let tests = [
            ("-1", Object::Int(-1)),
            ("-(-2)", Object::Int(2)),
            ("!0", Object::Bool(true)),
            ("!3", Object::Bool(false)),
            ("!true", Object::Bool(false)),
            ("!false", Object::Bool(true)),
            ("!!true", Object::Bool(true)),
        ];
        for (test, expected) in tests.iter() {
            let got = interp.eval(test).unwrap();
            assert_eq!(*expected, got)
        }
    }
}