use std::str::CharIndices;
use std::iter::Peekable;
use std::ops::Fn;

use crate::ast;
use crate::ast::TokenKind;

pub fn lex<'a>(src: &'a str) -> impl Iterator<Item=ast::Token<'a>> {
    Lexer::new(src)
}

pub struct Lexer<'a> {
    src_str: &'a str,
    src: Peekable<CharIndices<'a>>,
    ch: Option<(usize, char)>,
    peek: Option<(usize, char)>,
    line: u64,
    col: u64,
}

impl<'a> Lexer<'a> {
    fn new(src_str: &'a str) -> Self {
        let src = src_str.char_indices().peekable();
        let mut l = Lexer {
            src_str,
            src,
            ch: None,
            peek: None,
            line: 1,
            col: 0,
        };
        l.read_char();
        l
    }

    #[inline]
    fn read_char(&mut self) {
        self.ch = self.src.next();
        self.peek = self.src.peek().cloned();
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
        //self.read_char();
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
        let start_idx = self.loc() as usize;
        loop {
            match self.peek {
                Some((_, c)) if pred(c) => self.read_char(),
                Some((end_idx, _)) => {
                    return Some(&self.src_str[start_idx..end_idx])
                },
                None => {
                    return Some(&self.src_str[start_idx..])
                },
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

    pub fn read_token(&mut self) -> Option<ast::Token<'a>> {
        self.skip_whitespace();
        let line = self.line;
        let col = self.col;
        let start_idx = self.loc();
        let kind = if let Some((_, c)) = self.ch {
            // todo: make an abstraction for the two character tokens
            Some(match c {
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
                d if Self::is_digit(d) => TokenKind::Int(self.read_number()?),
                l if Self::is_letter(l) => {
                    let id = self.read_ident()?;
                    match id {
                        "fn" => TokenKind::Function,
                        "let" => TokenKind::Let,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "return" => TokenKind::Return,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        _ => TokenKind::Ident(id),
                    }
                },
                _ => TokenKind::Illegal,
            })
        } else {
            None
        }?;
        self.read_char();
        let end_idx = self.loc();
        let loc = ast::Span { line, col, start_idx, end_idx };
        Some(ast::Token { loc, kind })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = ast::Token<'a>;

    fn next(&mut self) -> Option<ast::Token<'a>> {
        self.read_token()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use ast::TokenKind::*;
    #[test]
    fn special_chars() {
        let input = "=+(){},;-/*<>!";
        let expected = vec![
            Token {
                loc: Span {
                    line: 1,
                    col: 1,
                    start_idx: 0,
                    end_idx: 1,
                },
                kind: Assign,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 2,
                    start_idx: 1,
                    end_idx: 2,
                },
                kind: Plus,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 3,
                    start_idx: 2,
                    end_idx: 3,
                },
                kind: LParen,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 4,
                    start_idx: 3,
                    end_idx: 4,
                },
                kind: RParen,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 5,
                    start_idx: 4,
                    end_idx: 5,
                },
                kind: LBrace,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 6,
                    start_idx: 5,
                    end_idx: 6,
                },
                kind: RBrace,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 7,
                    start_idx: 6,
                    end_idx: 7,
                },
                kind: Comma,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 8,
                    start_idx: 7,
                    end_idx: 8,
                },
                kind: Semicolon,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 9,
                    start_idx: 8,
                    end_idx: 9,
                },
                kind: Minus,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 10,
                    start_idx: 9,
                    end_idx: 10,
                },
                kind: Slash,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 11,
                    start_idx: 10,
                    end_idx: 11,
                },
                kind: Star,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 12,
                    start_idx: 11,
                    end_idx: 12,
                },
                kind: LT,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 13,
                    start_idx: 12,
                    end_idx: 13,
                },
                kind: GT,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 14,
                    start_idx: 13,
                    end_idx: 14,
                },
                kind: Bang,
            },
        ];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }

    #[test]
    fn test_code_block() {
        let input = include_str!("tests/code_block_lexer_test.rh");
        let expected = vec![
            Token { loc: Span { line: 1, col: 1, start_idx: 0, end_idx: 3 }, kind: Let },
            Token { loc: Span { line: 1, col: 5, start_idx: 4, end_idx: 8 }, kind: Ident("five") },
            Token { loc: Span { line: 1, col: 10, start_idx: 9, end_idx: 10 }, kind: Assign },
            Token { loc: Span { line: 1, col: 12, start_idx: 11, end_idx: 12 }, kind: Int(5) },
            Token { loc: Span { line: 1, col: 13, start_idx: 12, end_idx: 13 }, kind: Semicolon },
            Token { loc: Span { line: 2, col: 2, start_idx: 15, end_idx: 18 }, kind: Let },
            Token { loc: Span { line: 2, col: 6, start_idx: 19, end_idx: 22 }, kind: Ident("ten") },
            Token { loc: Span { line: 2, col: 10, start_idx: 23, end_idx: 24 }, kind: Assign },
            Token { loc: Span { line: 2, col: 12, start_idx: 25, end_idx: 27 }, kind: Int(10) },
            Token { loc: Span { line: 2, col: 14, start_idx: 27, end_idx: 28 }, kind: Semicolon },
            Token { loc: Span { line: 4, col: 2, start_idx: 32, end_idx: 35 }, kind: Let },
            Token { loc: Span { line: 4, col: 6, start_idx: 36, end_idx: 39 }, kind: Ident("add") },
            Token { loc: Span { line: 4, col: 10, start_idx: 40, end_idx: 41 }, kind: Assign },
            Token { loc: Span { line: 4, col: 12, start_idx: 42, end_idx: 44 }, kind: Function },
            Token { loc: Span { line: 4, col: 14, start_idx: 44, end_idx: 45 }, kind: LParen },
            Token { loc: Span { line: 4, col: 15, start_idx: 45, end_idx: 46 }, kind: Ident("x") },
            Token { loc: Span { line: 4, col: 16, start_idx: 46, end_idx: 47 }, kind: Comma },
            Token { loc: Span { line: 4, col: 18, start_idx: 48, end_idx: 49 }, kind: Ident("y") },
            Token { loc: Span { line: 4, col: 19, start_idx: 49, end_idx: 50 }, kind: RParen },
            Token { loc: Span { line: 4, col: 21, start_idx: 51, end_idx: 52 }, kind: LBrace },
            Token { loc: Span { line: 5, col: 6, start_idx: 58, end_idx: 59 }, kind: Ident("x") },
            Token { loc: Span { line: 5, col: 8, start_idx: 60, end_idx: 61 }, kind: Plus },
            Token { loc: Span { line: 5, col: 10, start_idx: 62, end_idx: 63 }, kind: Ident("y") },
            Token { loc: Span { line: 5, col: 11, start_idx: 63, end_idx: 64 }, kind: Semicolon },
            Token { loc: Span { line: 6, col: 2, start_idx: 66, end_idx: 67 }, kind: RBrace },
            Token { loc: Span { line: 6, col: 3, start_idx: 67, end_idx: 68 }, kind: Semicolon },
            Token { loc: Span { line: 8, col: 2, start_idx: 72, end_idx: 75 }, kind: Let },
            Token { loc: Span { line: 8, col: 6, start_idx: 76, end_idx: 82 }, kind: Ident("result") },
            Token { loc: Span { line: 8, col: 13, start_idx: 83, end_idx: 84 }, kind: Assign },
            Token { loc: Span { line: 8, col: 15, start_idx: 85, end_idx: 88 }, kind: Ident("add") },
            Token { loc: Span { line: 8, col: 18, start_idx: 88, end_idx: 89 }, kind: LParen },
            Token { loc: Span { line: 8, col: 19, start_idx: 89, end_idx: 93 }, kind: Ident("five") },
            Token { loc: Span { line: 8, col: 23, start_idx: 93, end_idx: 94 }, kind: Comma },
            Token { loc: Span { line: 8, col: 25, start_idx: 95, end_idx: 98 }, kind: Ident("ten") },
            Token { loc: Span { line: 8, col: 28, start_idx: 98, end_idx: 99 }, kind: RParen },
            Token { loc: Span { line: 8, col: 29, start_idx: 99, end_idx: 100 }, kind: Semicolon },
            Token { loc: Span { line: 10, col: 2, start_idx: 104, end_idx: 105 }, kind: Int(5) },
            Token { loc: Span { line: 10, col: 4, start_idx: 106, end_idx: 107 }, kind: LT },
            Token { loc: Span { line: 10, col: 6, start_idx: 108, end_idx: 110 }, kind: Int(10) },
            Token { loc: Span { line: 10, col: 9, start_idx: 111, end_idx: 112 }, kind: GT },
            Token { loc: Span { line: 10, col: 11, start_idx: 113, end_idx: 114 }, kind: Int(3) },
            Token { loc: Span { line: 11, col: 2, start_idx: 116, end_idx: 117 }, kind: Bang },
            Token { loc: Span { line: 11, col: 3, start_idx: 117, end_idx: 118 }, kind: Bang },
            Token { loc: Span { line: 11, col: 4, start_idx: 118, end_idx: 119 }, kind: Int(1) },
            Token { loc: Span { line: 12, col: 2, start_idx: 121, end_idx: 125 }, kind: Ident("spam") },
            Token { loc: Span { line: 12, col: 7, start_idx: 126, end_idx: 127 }, kind: Star },
            Token { loc: Span { line: 12, col: 9, start_idx: 128, end_idx: 132 }, kind: Ident("eggs") },
            Token { loc: Span { line: 12, col: 14, start_idx: 133, end_idx: 134 }, kind: Minus },
            Token { loc: Span { line: 12, col: 16, start_idx: 135, end_idx: 138 }, kind: Ident("foo") },
            Token { loc: Span { line: 12, col: 20, start_idx: 139, end_idx: 140 }, kind: Slash },
            Token { loc: Span { line: 12, col: 22, start_idx: 141, end_idx: 144 }, kind: Ident("bar") },
            Token { loc: Span { line: 14, col: 2, start_idx: 148, end_idx: 150 }, kind: If },
            Token { loc: Span { line: 14, col: 5, start_idx: 151, end_idx: 155 }, kind: True },
            Token { loc: Span { line: 14, col: 10, start_idx: 156, end_idx: 157 }, kind: LBrace },
            Token { loc: Span { line: 14, col: 12, start_idx: 158, end_idx: 163 }, kind: Ident("print") },
            Token { loc: Span { line: 14, col: 17, start_idx: 163, end_idx: 164 }, kind: LParen },
            Token { loc: Span { line: 14, col: 18, start_idx: 164, end_idx: 165 }, kind: Int(1) },
            Token { loc: Span { line: 14, col: 19, start_idx: 165, end_idx: 166 }, kind: RParen },
            Token { loc: Span { line: 14, col: 21, start_idx: 167, end_idx: 168 }, kind: RBrace },
            Token { loc: Span { line: 14, col: 23, start_idx: 169, end_idx: 173 }, kind: Else },
            Token { loc: Span { line: 14, col: 28, start_idx: 174, end_idx: 175 }, kind: LBrace },
            Token { loc: Span { line: 14, col: 30, start_idx: 176, end_idx: 181 }, kind: Ident("print") },
            Token { loc: Span { line: 14, col: 35, start_idx: 181, end_idx: 182 }, kind: LParen },
            Token { loc: Span { line: 14, col: 36, start_idx: 182, end_idx: 187 }, kind: False },
            Token { loc: Span { line: 14, col: 41, start_idx: 187, end_idx: 188 }, kind: RParen },
            Token { loc: Span { line: 14, col: 43, start_idx: 189, end_idx: 190 }, kind: RBrace },
            Token { loc: Span { line: 16, col: 2, start_idx: 194, end_idx: 197 }, kind: Let },
            Token { loc: Span { line: 16, col: 6, start_idx: 198, end_idx: 201 }, kind: Ident("fib") },
            Token { loc: Span { line: 16, col: 10, start_idx: 202, end_idx: 203 }, kind: Assign },
            Token { loc: Span { line: 16, col: 12, start_idx: 204, end_idx: 206 }, kind: Function },
            Token { loc: Span { line: 16, col: 14, start_idx: 206, end_idx: 207 }, kind: LParen },
            Token { loc: Span { line: 16, col: 15, start_idx: 207, end_idx: 208 }, kind: Ident("n") },
            Token { loc: Span { line: 16, col: 16, start_idx: 208, end_idx: 209 }, kind: RParen },
            Token { loc: Span { line: 16, col: 18, start_idx: 210, end_idx: 211 }, kind: LBrace },
            Token { loc: Span { line: 17, col: 6, start_idx: 217, end_idx: 219 }, kind: If },
            Token { loc: Span { line: 17, col: 9, start_idx: 220, end_idx: 221 }, kind: Ident("n") },
            Token { loc: Span { line: 17, col: 11, start_idx: 222, end_idx: 224 }, kind: LtEq },
            Token { loc: Span { line: 17, col: 14, start_idx: 225, end_idx: 226 }, kind: Int(2) },
            Token { loc: Span { line: 17, col: 16, start_idx: 227, end_idx: 228 }, kind: LBrace },
            Token { loc: Span { line: 18, col: 10, start_idx: 238, end_idx: 244 }, kind: Return },
            Token { loc: Span { line: 18, col: 17, start_idx: 245, end_idx: 246 }, kind: Int(1) },
            Token { loc: Span { line: 18, col: 18, start_idx: 246, end_idx: 247 }, kind: Semicolon },
            Token { loc: Span { line: 19, col: 6, start_idx: 253, end_idx: 254 }, kind: RBrace },
            Token { loc: Span { line: 19, col: 8, start_idx: 255, end_idx: 259 }, kind: Else },
            Token { loc: Span { line: 19, col: 13, start_idx: 260, end_idx: 261 }, kind: LBrace },
            Token { loc: Span { line: 20, col: 10, start_idx: 271, end_idx: 277 }, kind: Return },
            Token { loc: Span { line: 20, col: 17, start_idx: 278, end_idx: 281 }, kind: Ident("fib") },
            Token { loc: Span { line: 20, col: 20, start_idx: 281, end_idx: 282 }, kind: LParen },
            Token { loc: Span { line: 20, col: 21, start_idx: 282, end_idx: 283 }, kind: Ident("n") },
            Token { loc: Span { line: 20, col: 22, start_idx: 283, end_idx: 284 }, kind: Minus },
            Token { loc: Span { line: 20, col: 23, start_idx: 284, end_idx: 285 }, kind: Int(1) },
            Token { loc: Span { line: 20, col: 24, start_idx: 285, end_idx: 286 }, kind: RParen },
            Token { loc: Span { line: 20, col: 26, start_idx: 287, end_idx: 288 }, kind: Plus },
            Token { loc: Span { line: 20, col: 28, start_idx: 289, end_idx: 292 }, kind: Ident("fib") },
            Token { loc: Span { line: 20, col: 31, start_idx: 292, end_idx: 293 }, kind: LParen },
            Token { loc: Span { line: 20, col: 32, start_idx: 293, end_idx: 294 }, kind: Ident("n") },
            Token { loc: Span { line: 20, col: 33, start_idx: 294, end_idx: 295 }, kind: Minus },
            Token { loc: Span { line: 20, col: 34, start_idx: 295, end_idx: 296 }, kind: Int(2) },
            Token { loc: Span { line: 20, col: 35, start_idx: 296, end_idx: 297 }, kind: RParen },
            Token { loc: Span { line: 20, col: 36, start_idx: 297, end_idx: 298 }, kind: Semicolon },
            Token { loc: Span { line: 21, col: 6, start_idx: 304, end_idx: 305 }, kind: RBrace },
            Token { loc: Span { line: 22, col: 2, start_idx: 307, end_idx: 308 }, kind: RBrace },
            Token { loc: Span { line: 24, col: 2, start_idx: 312, end_idx: 317 }, kind: Ident("print") },
            Token { loc: Span { line: 24, col: 7, start_idx: 317, end_idx: 318 }, kind: LParen },
            Token { loc: Span { line: 24, col: 8, start_idx: 318, end_idx: 321 }, kind: Ident("fib") },
            Token { loc: Span { line: 24, col: 11, start_idx: 321, end_idx: 322 }, kind: LParen },
            Token { loc: Span { line: 24, col: 12, start_idx: 322, end_idx: 323 }, kind: Int(5) },
            Token { loc: Span { line: 24, col: 13, start_idx: 323, end_idx: 324 }, kind: RParen },
            Token { loc: Span { line: 24, col: 14, start_idx: 324, end_idx: 325 }, kind: RParen }];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }


    #[test]
    fn test_double_len_ops() {
        let input = r#"== != <= >="#;
        let expected = vec![
            Token {
                loc: Span {
                    line: 1,
                    col: 1,
                    start_idx: 0,
                    end_idx: 2,
                },
                kind: TokenKind::Eq,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 4,
                    start_idx: 3,
                    end_idx: 5,
                },
                kind: TokenKind::NotEq,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 7,
                    start_idx: 6,
                    end_idx: 8,
                },
                kind: TokenKind::LtEq,
            },
            Token {
                loc: Span {
                    line: 1,
                    col: 10,
                    start_idx: 9,
                    end_idx: 11,
                },
                kind: TokenKind::GtEq,
            },
        ];
        let got: Vec<Token> = lex(input).collect();
        assert_eq!(expected, got);
    }
}
