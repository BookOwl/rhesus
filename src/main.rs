use std::io;
use std::io::prelude::*;

use rhesus;

fn main() -> io::Result<()> {
    let mut stdout  = io::stdout();
    let stdin = io::stdin();
    let mut buf = String::with_capacity(256);
    let mut out = String::with_capacity(256);
    let mut intern = rhesus::intern::Intern::new();
    println!("🐵 rhesus v0.1 🐵");
    loop {
        buf.clear();
        out.clear();
        write!(&mut stdout, "> ")?;
        stdout.flush()?;
        stdin.read_line(&mut buf)?;
        let mut lex = rhesus::lexer::Lexer::new(&buf, &mut intern);
        let mut parser = rhesus::parser::Parser::new(lex.lex_to_vec());
        let (prog, errs) = parser.parse_program();
        if errs.len() == 0 {
            prog.to_code(&intern, &mut out);
            write!(&mut stdout, "{}", out);
        } else {
            for err in errs.iter() {
                println!("Parse Error at line {}, column {}: {}", err.loc.line, err.loc.col, err.reason);
            }
            println!();
        }
    }
}
