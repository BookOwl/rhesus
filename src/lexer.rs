use std::str::CharIndices;
use std::ops::Fn;
use crate::ast::{Span, ExpressionPrecedence};
use crate::intern::{Intern, Id};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Token {
    pub loc: Span,
    pub kind: TokenKind,
    data: TokenData,
}

impl Token {
    pub fn ident(loc: Span, s: Id) -> Self {
        Token {
            loc,
            kind: TokenKind::Ident,
            data: TokenData::String(s),
        }
    }
    pub fn int(loc: Span, i: i64) -> Self {
        Token {
            loc,
            kind: TokenKind::Int,
            data: TokenData::Int(i),
        }
    }
    pub fn string(loc: Span, id: Id) -> Self {
        Token {
            loc,
            kind: TokenKind::String,
            data: TokenData::String(id)
        }
    }
    pub fn new(loc: Span, kind: TokenKind) -> Self {
        Token {
            loc,
            kind,
            data: TokenData::Empty,
        }
    }
    pub fn str_data(&self) -> Option<Id> {
        match self.data {
            TokenData::String(s) => Some(s),
            _ => None,
        }
    }
    pub fn int_data(&self) -> Option<i64> {
        match self.data {
            TokenData::Int(i) => Some(i),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum TokenData {
    String(Id),
    Int(i64),
    Empty,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenKind {
    Ident,
    Int,
    String,
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
    LBracket,
    RBracket,
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
    EOF,
}

impl TokenKind {
    pub fn precedence(&self) -> ExpressionPrecedence {
        use TokenKind::*;
        match *self {
            Eq | NotEq => ExpressionPrecedence::Equals,
            LT | LtEq | GT | GtEq => ExpressionPrecedence::LessGreater,
            Plus | Minus => ExpressionPrecedence::Sum,
            Star | Slash => ExpressionPrecedence::Product,
            LParen => ExpressionPrecedence::Call,
            _ => ExpressionPrecedence::Lowest,
        }
    }
}


pub struct Lexer<'a, 'b> {
    src_str: &'a str,
    intern: &'b mut Intern,
    src: CharIndices<'a>,
    ch: Option<(usize, char)>,
    peek: Option<(usize, char)>,
    line: u64,
    col: u64,
    eof: Option<Token>,
}

impl<'a, 'b> Lexer<'a, 'b> {
    pub fn new(src_str: &'a str, intern: &'b mut Intern) -> Self {
        let src = src_str.char_indices();
        let mut l = Lexer {
            src_str,
            intern,
            src,
            ch: None,
            peek: None,
            line: 1,
            col: 0,
            eof: None,
        };
        // Read twice to set self.ch and self.peek
        l.read_char();
        l.read_char();
        // Reset line and col
        l.line = 1;
        l.col = 1;
        l
    }

    pub fn reset(&mut self, src_str: &'a str) {
        self.src_str = src_str;
        self.src = src_str.char_indices();
        self.ch = None;
        self.peek = None;
        self.line = 1;
        self.col = 0;
        self.eof = None;

        self.read_char();
        self.read_char();
        self.line = 1;
        self.col = 1;
    }

    #[inline]
    fn read_char(&mut self) {
        self.ch = self.peek;
        self.peek = self.src.next();
        self.col += 1;
        if matches!(self.ch, Some((_, '\n'))) {
            self.line += 1;
            self.col = 1;
        }
    }

    #[inline]
    fn loc(&self) -> u64 {
        if let Some((idx, _)) = self.ch {
            idx as u64
        } else {
            self.src_str.len() as u64
        }
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        // or-patterns syntax would make this much nicer
        while matches!(self.ch, Some((_, ' ')) | Some((_, '\t')) | Some((_, '\r')) | Some((_, '\n'))) {
            self.read_char();
        }
    }

    #[inline]
    fn is_letter(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    #[inline]
    fn is_digit(c: char) -> bool {
        matches!(c, '0'..='9')
    }

    fn read_while(&mut self, pred: impl Fn(char) -> bool) -> &'a str {
        let start_idx = self.loc() as usize;
        loop {
            match self.peek {
                Some((_, c)) if pred(c) => self.read_char(),
                Some((end_idx, _)) => {
                    return &self.src_str[start_idx..end_idx]
                },
                None => {
                    return &self.src_str[start_idx..]
                },
            }
        }
    }

    fn read_ident(&mut self) -> &'a str {
        self.read_while(|c| Self::is_letter(c) | Self::is_digit(c))
    }

    fn read_number(&mut self) -> i64 {
        let n = self.read_while(Self::is_digit);
        // The `unwrap()` will never panic because `n` can only contain valid digits
        n.parse().unwrap()
    }

    fn read_string(&mut self) -> &'a str {
        self.read_char();
        let s = self.read_while(|c| c != '"');
        self.read_char();
        s
    }

    pub fn read_token(&mut self) -> Token {
        if let Some(end) = self.eof {
            return end
        }
        self.skip_whitespace();
        let line = self.line;
        let col = self.col;
        let start_idx = self.loc();
        let mut ident = None;
        let mut int: i64 = 0;
        let mut string = None;
        let mut eof = false;
        let kind = if let Some((_, c)) = self.ch {
            // todo: make an abstraction for the two character tokens
            match c {
                '=' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        TokenKind::Eq
                    } else {
                        TokenKind::Assign
                    }
                },
                ';' => TokenKind::Semicolon,
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                ',' => TokenKind::Comma,
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                '!' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        TokenKind::NotEq
                    } else {
                        TokenKind::Bang
                    }
                },
                '<' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        TokenKind::LtEq
                    } else {
                        TokenKind::LT
                    }
                },
                '>' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        TokenKind::GtEq
                    } else {
                        TokenKind::GT
                    }
                },
                '"' => {
                    let s = self.read_string();
                    string = Some(self.intern.intern(s));
                    TokenKind::String
                }
                d if Self::is_digit(d) => {
                    int = self.read_number();
                    TokenKind::Int
                },
                l if Self::is_letter(l) => {
                    let id = self.read_ident();
                    match id {
                        "fn" => TokenKind::Function,
                        "let" => TokenKind::Let,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "return" => TokenKind::Return,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        _ => {
                            ident = Some(self.intern.intern(id));
                            TokenKind::Ident
                        },
                    }
                },
                _ => TokenKind::Illegal,
            }
        } else {
            eof = true;
            TokenKind::EOF
        };
        self.read_char();
        let end_idx = self.loc();
        let loc = Span { line, col, start_idx, end_idx };
        if eof {
            self.eof = Some(Token::new(loc, kind));
        }
        match kind {
            TokenKind::Int => Token::int(loc, int),
            TokenKind::Ident => Token::ident(loc, ident.unwrap()),
            TokenKind::String => Token::string(loc, string.unwrap()),
            _ => Token::new(loc, kind),
        }
    }

    pub fn lex_to_vec(&mut self) -> Vec<Token> {
        let mut lexed = Vec::with_capacity(2<<15);
        loop {
            let token = self.read_token();
            lexed.push(token);
            if token.kind == TokenKind::EOF {
                return lexed;
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::intern::*;
    use TokenKind::*;

    // TODO: Fix tests to include EOF tokens

    #[test]
    fn special_chars() {
        let input = "=+(){},;-/*<>!";
        let mut intern = Intern::new();
        let mut lex = Lexer::new(input, &mut intern);
        let expected = vec![
            Token { loc: Span { line: 1, col: 1, start_idx: 0, end_idx: 1 }, kind: Assign, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 2, start_idx: 1, end_idx: 2 }, kind: Plus, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 3, start_idx: 2, end_idx: 3 }, kind: LParen, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 4, start_idx: 3, end_idx: 4 }, kind: RParen, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 5, start_idx: 4, end_idx: 5 }, kind: LBrace, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 6, start_idx: 5, end_idx: 6 }, kind: RBrace, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 7, start_idx: 6, end_idx: 7 }, kind: Comma, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 8, start_idx: 7, end_idx: 8 }, kind: Semicolon, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 9, start_idx: 8, end_idx: 9 }, kind: Minus, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 10, start_idx: 9, end_idx: 10 }, kind: Slash, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 11, start_idx: 10, end_idx: 11 }, kind: Star, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 12, start_idx: 11, end_idx: 12 }, kind: LT, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 13, start_idx: 12, end_idx: 13 }, kind: GT, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 14, start_idx: 13, end_idx: 14 }, kind: Bang, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 15, start_idx: 14, end_idx: 14 }, kind: EOF, data: TokenData::Empty },
        ];
        let got = lex.lex_to_vec();
        assert_eq!(expected, got);
    }

    #[test]
    fn test_code_block() {
        let input = include_str!("tests/code_block_lexer_test.rh");
        let mut intern = Intern::new();
        let mut lex = Lexer::new(input, &mut intern);
        let got = lex.lex_to_vec();
        let expected_kinds = vec![
            Let, Ident, Assign, Int, Semicolon, Let, Ident, Assign, Int, Semicolon, Let, Ident, Assign, Function,
            LParen, Ident, Comma, Ident, RParen, LBrace, Ident, Plus, Ident, Semicolon, RBrace, Semicolon, Let, Ident,
            Assign, Ident, LParen, Ident, Comma, Ident, RParen, Semicolon, Int, LT, Int, GT, Int, Bang, Bang, Int,
            Ident, Star, Ident, Minus, Ident, Slash, Ident, If, True, LBrace, Ident, LParen, Int, RParen, RBrace,
            Else, LBrace, Ident, LParen, False, RParen, RBrace, Let, Ident, Assign, Function, LParen, Ident, RParen,
            LBrace, If, Ident, LtEq, Int, LBrace, Return, Int, Semicolon, RBrace, Else, LBrace, Return, Ident, LParen,
            Ident, Minus, Int, RParen, Plus, Ident, LParen, Ident, Minus, Int, RParen, Semicolon, RBrace, RBrace,
            Ident, LParen, Ident, LParen, Int, RParen, RParen, EOF,
           ];
        let got_kinds = got.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(expected_kinds, got_kinds);
        let expected_idents = vec![
            "five", "ten", "add", "x", "y", "x", "y", "result", "add", "five", "ten",
            "spam", "eggs", "foo", "bar", "print", "print", "fib", "n", "n", "fib", "n", "fib", "n", "print", "fib",
        ];
        let got_idents = got.iter()
            .filter(|t| t.kind == Ident)
            .map(|t| intern.lookup(t.str_data().unwrap()).unwrap()).collect::<Vec<_>>();
        assert_eq!(expected_idents, got_idents);
    }


    #[test]
    fn test_double_len_ops() {
        let input = r#"== != <= >="#;
        let mut intern = Intern::new();
        let mut lex = Lexer::new(input, &mut intern);
        let expected: Vec<Token> = vec![
            Token { loc: Span { line: 1, col: 1, start_idx: 0, end_idx: 2 }, kind: Eq, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 4, start_idx: 3, end_idx: 5 }, kind: NotEq, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 7, start_idx: 6, end_idx: 8 }, kind: LtEq, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 10, start_idx: 9, end_idx: 11 }, kind: GtEq, data: TokenData::Empty },
            Token { loc: Span { line: 1, col: 12, start_idx: 11, end_idx: 11 }, kind: EOF, data: TokenData::Empty },
        ];
        let got = lex.lex_to_vec();
        assert_eq!(expected, got);
    }
}
