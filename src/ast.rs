use crate::intern;
use std::fmt::Write;
use crate::lexer::TokenKind;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Span {
    pub line: u64,
    pub col: u64,
    pub start_idx: u64,
    pub end_idx: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub stmts: Vec<Statement>,
}

impl Program {
    pub fn to_code(&self, i: &intern::Intern, out: &mut String) {
        for stmt in self.stmts.iter() {
            stmt.to_code(i, out);
            write!(out, "\n");
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Statement {
    pub loc: Span,
    pub kind: StatementKind
}

impl Statement {
    pub fn to_code(&self, i: &intern::Intern, out: &mut String) {
        match &self.kind {
            StatementKind::Let {ident, value} => {
                write!(out, "let {} = ", i.lookup(*ident).unwrap());
                value.to_code(i, out);
                write!(out, ";");
            },
            StatementKind::Return {value} => {
                write!(out, "return ");
                value.to_code(i, out);
                write!(out, ";");
            },
            StatementKind::Expression {value} => {
                value.to_code(i, out);
                write!(out, ";");
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementKind {
    Let {
        ident: intern::Id,
        value: Expression,
    },
    Return {
        value: Expression,
    },
    Expression {
        value: Expression,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expression {
    pub loc: Span,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn to_code(&self, i: &intern::Intern, out: &mut String) {
        match &self.kind {
            ExpressionKind::Int(i) => {
                write!(out, "{}", *i);
            },
            ExpressionKind::Variable(id) => {
                out.push_str(i.lookup(*id).unwrap());
            },
            ExpressionKind::PrefixOp {operator, expr} => {
                out.push_str(match operator {
                    PrefixOperator::Bang => "!(",
                    PrefixOperator::Neg => "-(",
                });
                expr.to_code(i, out);
                out.push(')');
            },
            ExpressionKind::InfixOp {operator, left, right} => {
                out.push('(');
                left.to_code(i, out);
                out.push_str(match operator {
                    InfixOperator::NotEq => " != ",
                    InfixOperator::Eq => " == ",
                    InfixOperator::LtEq => " <= ",
                    InfixOperator::Lt => " < ",
                    InfixOperator::GtEq => " >= ",
                    InfixOperator::Gt => " > ",
                    InfixOperator::Plus => " + ",
                    InfixOperator::Minus => " - ",
                    InfixOperator::Star => " * ",
                    InfixOperator::Slash => " / ",
                });
                right.to_code(i, out);
                out.push(')');
            },
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum ExpressionPrecedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOperator {
    Bang,
    Neg,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum InfixOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Eq,
    NotEq,
}

impl InfixOperator {
    pub fn token_kinds() -> &'static [TokenKind] {
        &[TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash,
          TokenKind::GT, TokenKind::GtEq, TokenKind::LT, TokenKind::LtEq,
          TokenKind::Eq, TokenKind::NotEq]
    }
}


#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Variable(intern::Id),
    Int(i64),
    PrefixOp {
        operator: PrefixOperator,
        expr: Box<Expression>,
    },
    InfixOp {
        operator: InfixOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

