
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Span {
    pub line: u64,
    pub col: u64,
    pub start_idx: u64,
    pub end_idx: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Ident(&'a str),
    Int(i64),
    Assign,
    Plus,
    Minus,
    Slash,
    Star,
    Bang,
    Eq,
    NotEq,
    GtEq,
    LtEq,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LT,
    GT,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Illegal,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub loc: Span,
    pub kind: TokenKind<'a>,
}