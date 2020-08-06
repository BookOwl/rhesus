use std::fmt;

use crate::lexer;
use lexer::{Token, TokenKind};
use crate::intern::*;
use crate::ast::*;


#[derive(Debug, Clone)]
pub struct ParseError {
    pub loc: Span,
    pub reason: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error at line {}, column {}: {}", self.loc.line, self.loc.col, &self.reason)
    }
}



pub type ParseStatementResult = Result<Statement, ParseError>;
pub type ParseExpressionResult = Result<Expression, ParseError>;
pub type ParseBlockResult = Result<Block, ParseError>;

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
    //intern: &'a Intern,
    cur: Option<Token>,
    peek: Option<Token>,
}

impl Parser {
    pub fn new<T>(toks: T) -> Self
    where T: Into<Vec<Token>> {
        let mut p = Parser {
            toks: toks.into(),
            idx: 0,
           // intern,
            cur: None,
            peek: None,
        };
        p.read_token();
        p
    }

    #[inline]
    fn read_token(&mut self) {
        self.cur = self.toks.get(self.idx).cloned();
        self.idx += 1;
        self.peek = self.toks.get(self.idx).cloned();
    }

    fn expect_peek(&mut self, expected: TokenKind, in_: &str) -> Result<Token, ParseError> {
        match self.peek {
            Some(Token {kind, ..}) if kind == expected => {
                // This can not panic because in this case self.peek is a Some(Token)
                let t = self.peek.unwrap();
                self.read_token();
                Ok(t)
            },
            Some(Token {kind, loc, ..}) => {
                // If we encounter the wrong kind of token give an error
                // We then try to recover from the error by skipping to the next statement marked by a semicolon
                self.recover_from_parse_error();
                Err(ParseError{
                    loc,
                    reason: format!("Expected a {:?} while parsing {} but found a {:?}", expected, in_, kind),
                })
            },
            None => panic!("Unexpected end of token stream in expect_peek! Is this a bug?"),
        }
    }

    fn recover_from_parse_error(&mut self) {
        // In order to keep parsing and find more errors the parser skips to the next semicolon
        // so it can try and parse the next statement
        while !self.cur_token_is(TokenKind::Semicolon) && !self.peek_token_is(TokenKind::EOF) {
            if self.cur_token_is(TokenKind::EOF) {
                return
            }
            self.read_token();
        };
        self.read_token();
    }

    #[inline]
    fn cur_token_is(&self, expected: TokenKind) -> bool {
        match &self.cur {
            Some(Token {kind, ..}) => *kind == expected,
            _ => false
        }
    }

    #[inline]
    fn peek_token_is(&self, expected: TokenKind) -> bool {
        match &self.peek {
            Some(Token {kind, ..}) => *kind == expected,
            _ => false
        }
    }

    #[inline]
    fn peek_precedence(&self) -> ExpressionPrecedence {
        self.peek.map_or(ExpressionPrecedence::Lowest, |t| t.kind.precedence())
    }

    #[inline]
    fn cur_precedence(&self) -> ExpressionPrecedence {
        self.cur.map_or(ExpressionPrecedence::Lowest, |t| t.kind.precedence())
    }

    #[inline]
    fn parse_optional_semicolon(&mut self) {
        self.read_token();
        if self.cur_token_is(TokenKind::Semicolon) {
            self.read_token()
        }
    }

    pub fn parse_program(&mut self) -> (Program, Vec<ParseError>) {
        let mut statements = vec![];
        let mut errs = vec![];
        loop {
            match self.parse_statement() {
                Some(Ok(s)) => statements.push(s),
                Some(Err(e)) => {
                    errs.push(e);
                    // Try to recover from the error by reading until the next semicolon or EOF
                    self.recover_from_parse_error();
                },
                None => break
            }
        };
        (Program {stmts: statements}, errs)
    }

    pub fn parse_statement(&mut self) -> Option<ParseStatementResult> {
        let cur = self.cur.as_ref()?;
        Some(match cur.kind {
            TokenKind::Let => self.parse_let(),
            TokenKind::Return => self.parse_return(),
            TokenKind::EOF => return None,
            _ => self.parse_expression_stmt(),
        })
    }

    fn parse_let(&mut self) -> ParseStatementResult {
        // Because parse_return is only called when self.cur is a Let token, this can not panic
        let loc = self.cur.as_ref().unwrap().loc;
        let ident = self.expect_peek(TokenKind::Ident, "a let statement")?.str_data().unwrap();
        self.expect_peek(TokenKind::Assign, "a let statement")?;
        self.read_token();
        let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.parse_optional_semicolon();
        Ok(Statement {
            loc,
            kind: StatementKind::Let {
                ident,
                value,
            }
        })
    }

    fn parse_return(&mut self) -> ParseStatementResult {
        // Because parse_return is only called when self.cur is a Return token, this can not panic
        let loc = self.cur.as_ref().unwrap().loc;
        self.read_token();
        let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.parse_optional_semicolon();
        Ok(Statement{
            loc,
            kind: StatementKind::Return {
                value,
            }
        })

    }

    fn parse_expression_stmt(&mut self) -> ParseStatementResult {
        let loc = self.cur.as_ref().unwrap().loc;
        let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.parse_optional_semicolon();
        Ok(Statement {
            loc,
            kind: StatementKind::Expression {
                value,
            }
        })
    }

    fn parse_expression(&mut self, precedence: ExpressionPrecedence) -> ParseExpressionResult {
        let cur = self.cur.unwrap();
        let mut left = match cur.kind {
            TokenKind::Ident => self.parse_ident()?,
            TokenKind::Int => self.parse_int()?,
            TokenKind::True | TokenKind::False => self.parse_bool()?,
            TokenKind::Bang | TokenKind::Minus => self.parse_prefix()?,
            TokenKind::LParen => self.parse_grouped()?,
            TokenKind::LBrace => self.parse_block_expression()?,
            TokenKind::If => self.parse_if()?,
            TokenKind::Function => self.parse_function()?,
            _ => return Err(ParseError {
                    loc: cur.loc,
                    reason: format!("No prefix parse function for {:?}", cur.kind)
            }),
        };

        while !self.peek_token_is(TokenKind::Semicolon) && precedence < self.peek_precedence() {
            if !InfixOperator::token_kinds().contains(&self.peek.unwrap().kind) {
                return Ok(left);
            }
            self.read_token();
            left = self.parse_infix(left)?;
        }
        Ok(left)
    }

    fn parse_ident(&mut self) -> ParseExpressionResult {
        match self.cur.unwrap() {
            t @ Token {kind: TokenKind::Ident, ..} => Ok(Expression {
                loc: t.loc,
                kind: ExpressionKind::Variable(t.str_data().unwrap()),
            }),
            t => Err(ParseError{
                loc: t.loc,
                reason: format!("Expected an identifier, found a {:?}", t.kind)
            }),
        }
    }

    fn parse_int(&mut self) -> ParseExpressionResult {
        match self.cur.unwrap() {
            t @ Token {kind: TokenKind::Int, ..} => Ok(Expression {
                loc: t.loc,
                kind: ExpressionKind::Int(t.int_data().unwrap()),
            }),
            t => Err(ParseError{
                loc: t.loc,
                reason: format!("Expected an integer, found a {:?}", t.kind)
            }),
        }
    }

    fn parse_bool(&mut self) -> ParseExpressionResult {
        match self.cur.unwrap() {
            Token {kind: TokenKind::True, loc, ..} => Ok(Expression {
                loc,
                kind: ExpressionKind::Bool(true),
            }),
            Token {kind: TokenKind::False, loc, ..} => Ok(Expression {
                loc,
                kind: ExpressionKind::Bool(false),
            }),
            t => Err(ParseError{
                loc: t.loc,
                reason: format!("Expected a boolean, found a {:?}", t.kind)
            }),
        }
    }

    fn parse_prefix(&mut self) -> ParseExpressionResult {
        // Can't panic because parse_prefix is only called when self.cur is a Some(Token)
        let cur = self.cur.expect("parse_prefix_bang called when self.cur is None");
        let operator = match cur.kind {
            TokenKind::Minus => PrefixOperator::Neg,
            TokenKind::Bang => PrefixOperator::Bang,
            _ => return Err(ParseError {
                loc: cur.loc,
                reason: format!("Expected a prefix operator expression but found a {:?}", cur.kind),
            }),
        };
        self.read_token();
        let target = self.parse_expression(ExpressionPrecedence::Prefix)?;
        Ok(Expression {
            loc: cur.loc,
            kind: ExpressionKind::PrefixOp {
                operator,
                expr: Box::new(target),
            }
        })
    }

    fn parse_grouped(&mut self) -> ParseExpressionResult {
        self.read_token();
        let exp = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.expect_peek(TokenKind::RParen, "grouped expression")?;
        Ok(exp)
    }

    fn parse_infix(&mut self, left: Expression) -> ParseExpressionResult {
        let cur = self.cur.unwrap();
        let loc = cur.loc;
        let operator = match cur.kind {
            TokenKind::Plus => InfixOperator::Plus,
            TokenKind::Minus => InfixOperator::Minus,
            TokenKind::Star => InfixOperator::Star,
            TokenKind::Slash => InfixOperator::Slash,
            TokenKind::GT => InfixOperator::Gt,
            TokenKind::GtEq => InfixOperator::GtEq,
            TokenKind::LT => InfixOperator::Lt,
            TokenKind::LtEq => InfixOperator::LtEq,
            TokenKind::Eq => InfixOperator::Eq,
            TokenKind::NotEq => InfixOperator::NotEq,
            TokenKind::LParen => return self.parse_call(left),
            _ => return Err(ParseError {
                loc,
                reason: format!("Expected an infix operator, found a {:?}", cur.kind),
            })
        };
        let left = Box::new(left);
        let prec = self.cur_precedence();
        self.read_token();
        let right = Box::new(self.parse_expression(prec)?);
        Ok(Expression {
            loc,
            kind: ExpressionKind::InfixOp {
                left,
                operator,
                right,
            }
        })
    }

    fn parse_block(&mut self) -> ParseBlockResult {
        let cur = self.cur.expect("parse_block called when self.cur is None");
        let loc = cur.loc;
        if cur.kind != TokenKind::LBrace {
            return Err(ParseError { loc, reason: format!("Expected a {{ but found a {:?}", cur.kind)})
        }
        let mut stmts = Vec::new();
        self.read_token();
        while let Some(Token { kind, ..}) = self.cur {
            if matches!(kind, TokenKind::RBrace | TokenKind::EOF) {
                break
            }
            let stmt = self.parse_statement().unwrap()?;
            stmts.push(stmt);
        }
        Ok(Block { stmts })
    }

    fn parse_block_expression(&mut self) -> ParseExpressionResult {
        let loc = self.cur.unwrap().loc;
        let block = self.parse_block()?;
        Ok(Expression {
            loc,
            kind: ExpressionKind::Block(block),
        })
    }

    fn parse_if(&mut self) -> ParseExpressionResult {
        let loc = self.cur.expect("parse_if called when self.cur is None").loc;
        self.read_token();
        let condition = Box::new(self.parse_expression(ExpressionPrecedence::Lowest)?);
        self.read_token();
        let consequence = self.parse_block()?;
        let alternative = if self.peek_token_is(TokenKind::Else) {
            self.read_token();
            self.read_token();
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Expression {
            loc,
            kind: ExpressionKind::If {
                condition,
                consequence,
                alternative,
            },
        })
    }

    fn parse_function(&mut self) -> ParseExpressionResult {
        let loc = self.cur.expect("parse_function called when self.cur is None").loc;
        let name = if self.peek_token_is(TokenKind::Ident) {
            let id = self.peek;
            self.read_token();
            id.unwrap().str_data()
        } else {
            None
        };
        self.expect_peek(TokenKind::LParen, "function argument list")?;
        let mut params = Vec::new();
        while !self.peek_token_is(TokenKind::RParen) {
            let param = self.expect_peek(TokenKind::Ident, "function argument list")?.str_data().unwrap();
            if !self.peek_token_is(TokenKind::RParen) {
                self.expect_peek(TokenKind::Comma, "function argument list")?;
            }
            params.push(param);
        }
        self.expect_peek(TokenKind::RParen, "function argument list")?;
        self.expect_peek(TokenKind::LBrace, "function body")?;
        let body = self.parse_block()?;
        Ok(Expression {
            loc,
            kind: ExpressionKind::Function {
                name,
                params,
                body,
            }
        })
    }

    fn parse_call(&mut self, func: Expression) -> ParseExpressionResult {
        let func = Box::new(func);
        let loc = self.cur.unwrap().loc;
        let mut args = Vec::new();
        while !self.peek_token_is(TokenKind::RParen) {
            self.read_token();
            let arg = self.parse_expression(ExpressionPrecedence::Lowest)?;
            args.push(arg);
            if !self.peek_token_is(TokenKind::RParen) {
                self.expect_peek(TokenKind::Comma, "in function call")?;
            };
        }
        self.expect_peek(TokenKind::RParen, "in function call")?;
        Ok(Expression {
            loc,
            kind: ExpressionKind::Call {
                func,
                args,
            }
        })

    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intern;
    use crate::ast;

    macro_rules! parse_test {
        ($n:ident: $input:expr => $expected:expr) => {
            #[test]
            fn $n() {
                let test = $input;
                let expected = $expected;
                let mut intern = intern::Intern::new();
                let mut l = lexer::Lexer::new(&test, &mut intern);
                let toks = l.lex_to_vec();
                let mut parser = Parser::new(toks);
                let (prog, errs) = parser.parse_program();
                assert_eq!(errs.len(), 0, "Had {} parse errors: {:?}", errs.len(), errs);
                let mut code = String::new();
                prog.to_code(&intern, &mut code);
                assert_eq!(code, expected);
            }
        };
    }
    #[test]
    fn test_let_parsing() {
        let test = r#"
        let x = 5;
        let y = 10;
        let foo = 1728;
        "#;
        let mut intern = intern::Intern::new();
        let mut l = lexer::Lexer::new(&test, &mut intern);
        let toks = l.lex_to_vec();
        let mut parser = Parser::new(toks);
        let (prog, errs) = parser.parse_program();
        let expected_names = vec!["x", "y", "foo"];
        for (got, expected) in prog.stmts.iter().zip(expected_names.iter()) {
            if let ast::StatementKind::Let {ident, ..} = got.kind {
                assert_eq!(&intern.lookup(ident).unwrap(), expected, "mismatched names in let parsing");
            } else {
                panic!("Expected a let statement, parsed a {:?}", got.kind)
            }
        }
    }

    parse_test!(prefix_parsing: "!5;\n-x;" => "!(5);\n-(x);\n");

    parse_test!(infix_parsing:
           "1 + 2;
            3 - 5;
            5 * 5;
            6 / 2
            2 > 1;
            2 < 1
            10 == 5;
            5 != 5;
            true == false
            false == true;
            1 > 2 == false;
            false + true;
        " => "\
            (1 + 2);\n\
            (3 - 5);\n\
            (5 * 5);\n\
            (6 / 2);\n\
            (2 > 1);\n\
            (2 < 1);\n\
            (10 == 5);\n\
            (5 != 5);\n\
            (true == false);\n\
            (false == true);\n\
            ((1 > 2) == false);\n\
            (false + true);\n\
        ");

    parse_test!(grouping:
               "(1+1);
                1 + (1 * 2);
                (1 + 2) / 3;
                (1 + 2) * (4 - 2);
                " => "\
                (1 + 1);\n\
                (1 + (1 * 2));\n\
                ((1 + 2) / 3);\n\
                ((1 + 2) * (4 - 2));\n\
                "
    );

    parse_test!(blocks:
                "{ x };
                { x; y; z}
                { 1 + 1; 2}
                {}
                " => "\
                { x; };\n\
                { x; y; z; };\n\
                { (1 + 1); 2; };\n\
                { };\n\
                "
    );

    parse_test!(if_:
                "if true { 1 };
                if false { 0 } else { x };
                if (1 == 2) { yes } else { no };
                if (3 - 2 == 1) { a } else { b };
                " => "\
                if true { 1; };\n\
                if false { 0; } else { x; };\n\
                if (1 == 2) { yes; } else { no; };\n\
                if ((3 - 2) == 1) { a; } else { b; };\n\
                "
    );

    parse_test!(functions:
                "
                fn() {
                    hello
                };
                fn add1(x) {
                    x + 1;
                };
                let x = fn(a) { a > 2 };
                print(1);
                let y = add1(2);
                foo(bar)(baz);
                spam(eggs, 1 + 1, bacon);
                k((1 + 2) / 3);
                " => "\
                fn() { hello; };\n\
                fn add1(x) { (x + 1); };\n\
                let x = fn(a) { (a > 2); };\n\
                print(1);\n\
                let y = add1(2);\n\
                foo(bar)(baz);\n\
                spam(eggs, (1 + 1), bacon);\n\
                k(((1 + 2) / 3));\n\
                "
    );
}