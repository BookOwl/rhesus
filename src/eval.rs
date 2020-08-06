use std::fmt;
use std::rc::Rc;
use std::cell::{RefCell};
use std::collections::HashMap;
use std::cmp::{PartialEq, Eq};

use crate::{intern, ast, parser, lexer};

pub enum Object {
    Int(i64),
    Bool(bool),
    PrimFunction{
        name: &'static str,
        func: Box<dyn Fn(&[GcObject]) -> EvalResult>
    },
    Null,
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(i) => write!(f, "Int({})", i),
            Object::Bool(b) => write!(f, "Bool({})", b),
            Object::PrimFunction { name, ..} => write!(f, "PrimFunction(<'{}'>)", name),
            Object::Null => write!(f, "Null"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::PrimFunction { name, ..} => write!(f, "<primitive function '{}'>", name),
            Object::Null => write!(f, "null"),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Int(x), Object::Int(y)) => x == y,
            (Object::Bool(x), Object::Bool(y)) => x == y,
            (Object::PrimFunction { func: ref f1, ..},
                Object::PrimFunction { func: ref f2, ..}) => (f1 as *const _ as usize) == (f2 as *const _ as usize),
            (Object::Null, Object::Null) => true,
            (_, _) => false,
        }
    }
}
impl Eq for Object {}

impl Object {
    pub fn kind(&self) -> &'static str {
        match *self {
            Object::Int(_) => "Int",
            Object::Bool(_) => "Bool",
            Object::PrimFunction { .. } => "Function",
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

pub type GcObject = Rc<RefCell<Object>>;

#[inline]
fn gc(o: Object) -> GcObject {
    Rc::new(RefCell::new(o))
}

#[derive(Debug)]
pub struct UnboundVariableError;

#[derive(Debug)]
pub enum EvalError {
    TypeError {
        loc: ast::Span,
        reason: String,
    },
    UnboundVariable {
        loc: ast::Span,
        name: String,
    },
    ParseError(Vec<parser::ParseError>),
    EarlyReturn(GcObject),
}

impl EvalError {
    fn type_error(loc: ast::Span, msg: String) -> Self {
        EvalError::TypeError {
            loc,
            reason: format!("Type Error at line {}, column {}: {}",
                            loc.line, loc.col, msg),
        }
    }
}

pub type EvalResult = Result<GcObject, EvalError>;


#[derive(Debug)]
struct Environment {
    prev: Option<Rc<RefCell<Environment>>>,
    env: HashMap<intern::Id, GcObject>,
}

impl Environment {
    fn empty() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            prev: None,
            env: HashMap::new(),
        }))
    }
    fn sub_scope(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            prev: Some(Rc::clone(&parent)),
            env: HashMap::new(),
        }))
    }
    fn lookup(&self, name: intern::Id) -> Result<GcObject, UnboundVariableError> {
        if let Some(obj) = self.env.get(&name) {
            Ok(Rc::clone(obj))
        } else if let Some(parent) = self.prev.as_ref() {
            parent.borrow().lookup(name)
        } else {
            Err(UnboundVariableError)
        }
    }
    fn create_primitive(&mut self, i: &mut intern::Intern, name: &'static str, f: impl Fn(&[GcObject]) -> EvalResult + 'static) {
        let id = i.intern(name);
        self.assign(id, gc(Object::PrimFunction {
            name,
            func: Box::new(f),
        }));
    }
    fn assign(&mut self, name: intern::Id, val: GcObject) {
        self.env.insert(name, val);
    }
    fn mutate(&mut self, name: intern::Id, val: GcObject) -> Result<(), UnboundVariableError> {
        if let Some(obj) = self.env.get_mut(&name) {
            *obj = val;
            Ok(())
        } else if let Some(parent) = self.prev.as_ref() {
            parent.borrow_mut().mutate(name, val)
        } else {
            Err(UnboundVariableError)
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    global_env: Rc<RefCell<Environment>>,
    intern: intern::Intern,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut intern = intern::Intern::new_with_capacity(1024);
        let global_env = Environment::empty();
        global_env.borrow_mut().create_primitive(&mut intern,
                                                 "print",
                                                 |args| {
                                                     println!("{}", args.iter()
                                                                         .map(|o| format!("{}", o.borrow()))
                                                                         .collect::<Vec<_>>()
                                                                         .join(" ")
                                                     );
                                                     Ok(gc(Object::Null))
                                                 }
        );
        Interpreter {
            global_env,
            intern,
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
        let mut res = gc(Object::Null);
        for stmt in prog.stmts.iter() {
            res = self.eval_statement(Rc::clone(&self.global_env), stmt)?;
        }
        Ok(res)
    }

    fn eval_statement(&mut self, env: Rc<RefCell<Environment>>, stmt: &ast::Statement) -> EvalResult {
        match stmt.kind {
            ast::StatementKind::Let {ident, ref value} => {
                let val = self.eval_expr(Rc::clone(&env), value)?;
                env.borrow_mut().assign(ident, Rc::clone(&val));
                Ok(val)
            },
            ast::StatementKind::Return { ref value } => {
                Err(EvalError::EarlyReturn(self.eval_expr(Rc::clone(&env), value)?))
            },
            ast::StatementKind::Expression { ref value } => {
                self.eval_expr(Rc::clone(&env),value)
            }
        }
    }

    fn eval_expr(&mut self, env: Rc<RefCell<Environment>>, expr: &ast::Expression) -> EvalResult {
        match expr.kind {
            ast::ExpressionKind::Int(i) => Ok(gc(Object::Int(i))),
            ast::ExpressionKind::Bool(b) => Ok(gc(Object::Bool(b))),
            ast::ExpressionKind::Variable(name) => {
                env.borrow().lookup(name)
                    .map_err(|_| EvalError::UnboundVariable {
                        loc: expr.loc,
                        name: self.intern.lookup(name).unwrap().to_string()
                    })
            }
            ast::ExpressionKind::Block(ref b) => self.eval_block(Rc::clone(&env),b),
            ast::ExpressionKind::PrefixOp { operator,
                                            ref expr} => self.eval_prefix(expr.loc, Rc::clone(&env),operator, &expr),
            ast::ExpressionKind::InfixOp { operator,
                                           ref left,
                                           ref right} => self.eval_infix(expr.loc, Rc::clone(&env),operator, &left, &right),
            ast::ExpressionKind::If { ref condition,
                                      ref consequence,
                                      ref alternative,} => {
                if self.eval_expr(Rc::clone(&env),&condition)?.borrow().truthy() {
                    self.eval_block(Rc::clone(&env), consequence)
                } else {
                    alternative.as_ref().map_or_else(|| Ok(gc(Object::Null)), |b| self.eval_block(Rc::clone(&env), &b))
                }
            },
            ast::ExpressionKind::Call { ref func, ref args} => self.eval_call(expr.loc, Rc::clone(&env), func, args),
            ref e => todo!("{:?} is not implemented yet", e),
        }
    }

    fn eval_prefix(&mut self, loc: ast::Span, env: Rc<RefCell<Environment>>, op: ast::PrefixOperator, expr: &ast::Expression) -> EvalResult {
        let expr = self.eval_expr(Rc::clone(&env), expr)?;
        match op {
            ast::PrefixOperator::Neg => {
                match &*expr.borrow() {
                    Object::Int(i) => Ok(gc(Object::Int(-(*i)))),
                    o => Err(EvalError::type_error(loc, format!("Expected '-Int' but got '-{}'", o.kind()))),
                }
            },
            ast::PrefixOperator::Bang => {
                Ok(gc(Object::Bool(!expr.borrow().truthy())))
            }
        }
    }

    fn eval_infix(&mut self, loc: ast::Span, env: Rc<RefCell<Environment>>, op: ast::InfixOperator, left: &ast::Expression, right: &ast::Expression) -> EvalResult {
        let left = self.eval_expr(Rc::clone(&env),left)?;
        let right = self.eval_expr(Rc::clone(&env),right)?;
        match op {
            ast::InfixOperator::Eq => Ok(gc(Object::Bool(left == right))),
            ast::InfixOperator::NotEq => Ok(gc(Object::Bool(left != right))),
            ast::InfixOperator::LtEq => {
                match (&*left.borrow(), &*right.borrow()) {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Bool(l <= r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int <= Int' but got '{} <= {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::GtEq => {
                match (&*left.borrow(), &*right.borrow()) {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Bool(l >= r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int >= Int' but got '{} >= {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Lt => {
                match (&*left.borrow(), &*right.borrow()) {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Bool(l < r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int < Int' but got '{} < {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Gt => {
                match (&*left.borrow(), &*right.borrow())  {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Bool(l > r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int > Int' but got '{} > {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Plus => {
                match (&*left.borrow(), &*right.borrow()) {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Int(l + r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int + Int' but got '{} + {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Minus => {
                match (&*left.borrow(), &*right.borrow())  {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Int(l - r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int - Int' but got '{} - {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Star => {
                match (&*left.borrow(), &*right.borrow())  {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Int(l * r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int * Int' but got '{} * {}'", l.kind(), r.kind())))
                }
            },
            ast::InfixOperator::Slash => {
                match (&*left.borrow(), &*right.borrow())  {
                    (Object::Int(l), Object::Int(r)) => Ok(gc(Object::Int(l / r))),
                    (l, r) => Err(EvalError::type_error(loc, format!("Expected 'Int + Int' but got '{} + {}'", l.kind(), r.kind())))
                }
            },
        }
    }

    fn eval_block(&mut self, env: Rc<RefCell<Environment>>, block: &ast::Block) -> EvalResult {
        let mut res = gc(Object::Null);
        for stmt in block.stmts.iter() {
            res = self.eval_statement(Rc::clone(&env), stmt)?;
        }
        Ok(res)
    }

    fn eval_call(&mut self, loc: ast::Span, env: Rc<RefCell<Environment>>, func: &ast::Expression, args: &[ast::Expression]) -> EvalResult {
        let f = self.eval_expr(Rc::clone(&env),func)?;
        let x = f.borrow();
        if let Object::PrimFunction { ref func, ..} = *x{
            let args: Vec<GcObject> = args.iter().map(|arg| self.eval_expr(Rc::clone(&env),arg)).collect::<Result<Vec<_>, _>>()?;
            return (*func)(&args)
        } else {
            return Err(EvalError::type_error(loc, format!("Expected a 'Function(..)', but got '{}(..)'", f.borrow().kind())))
        };
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