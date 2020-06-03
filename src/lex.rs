use std::str::CharIndices;
use std::iter::Peekable;
use std::ops::Fn;

use crate::ast::Token;

pub fn lex<'a>(src: &'a str) -> impl Iterator<Item=Token<'a>> {
    Lexer::new(src)
}

pub struct Lexer<'a> {
    src_str: &'a str,
    src: Peekable<CharIndices<'a>>,
    ch: Option<(usize, char)>,
    peek: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    fn new(src_str: &'a str) -> Self {
        let src = src_str.char_indices().peekable();
        Lexer {
            src_str,
            src,
            ch: None,
            peek: None,
        }
    }

    #[inline]
    fn read_char(&mut self) {
        self.ch = self.src.next();
        self.peek = self.src.peek().cloned();
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        // or-patterns syntax would make this much nicer
        self.read_char();
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

    fn read_while(&mut self, pred: impl Fn(char) -> bool) -> Option<&'a str> {
        let (start_idx, _) = self.ch?;
        loop {
            match self.peek {
                Some((_, c)) if pred(c) => self.read_char(),
                Some((idx, _)) => return Some(&self.src_str[start_idx..idx]),
                None => return Some(&self.src_str[start_idx..]),
            }

        }
    }

    fn read_ident(&mut self) -> Option<&'a str> {
        self.read_while(Self::is_letter)
    }

    fn read_number(&mut self) -> Option<i64> {
        let n = self.read_while(Self::is_digit)?.parse().unwrap();
        // The `unwrap()` will never panic because `n` can only contain valid digits
        Some(n)
    }

    pub fn read_token(&mut self) -> Option<Token<'a>> {
        self.skip_whitespace();
        if let Some((_, c)) = self.ch {
            // todo: make an abstraction for the two character tokens
            Some(match c {
                '=' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        Token::Eq
                    } else {
                        Token::Assign
                    }
                },
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '!' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        Token::NotEq
                    } else {
                        Token::Bang
                    }
                },
                '<' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        Token::LtEq
                    } else {
                        Token::LT
                    }
                },
                '>' => {
                    if matches!(self.peek, Some((_, '='))) {
                        self.read_char();
                        Token::GtEq
                    } else {
                        Token::GT
                    }
                },
                d if Self::is_digit(d) => Token::Int(self.read_number()?),
                l if Self::is_letter(l) => {
                    let id = self.read_ident()?;
                    match id {
                        "fn" => Token::Function,
                        "let" => Token::Let,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        "true" => Token::True,
                        "false" => Token::False,
                        _ => Token::Ident(id),
                    }
                },
                _ => Token::Illegal,
            })
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.read_token()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn special_chars() {
        let input = "=+(){},;-/*<>!";
        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Minus,
            Token::Slash,
            Token::Star,
            Token::LT,
            Token::GT,
            Token::Bang,
        ];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }

    #[test]
    fn test_code_block() {
        let input = r#"let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);

        5 < 10 > 3
        !!1
        spam * eggs - foo / bar
        "#;
        let expected = vec![
            // Line 1
            Token::Let,
            Token::Ident("five"),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            // Line 2
            Token::Let,
            Token::Ident("ten"),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            // Line 4
            Token::Let,
            Token::Ident("add"),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x"),
            Token::Comma,
            Token::Ident("y"),
            Token::RParen,
            Token::LBrace,
            // Line 5
            Token::Ident("x"),
            Token::Plus,
            Token::Ident("y"),
            Token::Semicolon,
            // Line 6
            Token::RBrace,
            Token::Semicolon,
            //Line 8
            Token::Let,
            Token::Ident("result"),
            Token::Assign,
            Token::Ident("add"),
            Token::LParen,
            Token::Ident("five"),
            Token::Comma,
            Token::Ident("ten"),
            Token::RParen,
            Token::Semicolon,
            //Line 10
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::GT,
            Token::Int(3),
            //Line 11
            Token::Bang,
            Token::Bang,
            Token::Int(1),
            //Line 12
            Token::Ident("spam"),
            Token::Star,
            Token::Ident("eggs"),
            Token::Minus,
            Token::Ident("foo"),
            Token::Slash,
            Token::Ident("bar")
        ];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }

    #[test]
    fn test_bools() {
        let input = r#"true false;
        if true { 1 } else {2}
        if!false{return spam;}
        "#;
        let expected = vec![
            //Line 1
            Token::True,
            Token::False,
            Token::Semicolon,
            //Line 2
            Token::If,
            Token::True,
            Token::LBrace,
            Token::Int(1),
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Int(2),
            Token::RBrace,
            //Line 3
            Token::If,
            Token::Bang,
            Token::False,
            Token::LBrace,
            Token::Return,
            Token::Ident("spam"),
            Token::Semicolon,
            Token::RBrace,
        ];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }

    #[test]
    fn test_double_len_ops() {
        let input = r#"== != <= >="#;
        let expected = vec![Token::Eq, Token::NotEq, Token::LtEq, Token::GtEq];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }
}
