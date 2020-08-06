use std::io;
use std::io::prelude::*;

use rhesus;

fn main() -> io::Result<()> {
    let mut stdout  = io::stdout();
    let stdin = io::stdin();
    let mut buf = String::with_capacity(256);
    let mut out = String::with_capacity(256);
    let mut interp = rhesus::eval::Interpreter::new();
    println!("🐵 rhesus v0.1 🐵");
    loop {
        buf.clear();
        out.clear();
        write!(&mut stdout, "> ")?;
        stdout.flush()?;
        stdin.read_line(&mut buf)?;
        let res = interp.eval(&buf);
        match res {
            Ok(o) => println!("{}", o),
            Err(rhesus::eval::EvalError::ParseError(ref errs)) => {
                for err in errs.iter() {
                    println!("Parse Error at line {}, column {}: {}", err.loc.line, err.loc.col, err.reason);
                }
                println!();
            },
            Err(rhesus::eval::EvalError::TypeError {reason, ..}) => {
                println!("{}", reason);
            }
        }
    }
}
