use std::io;
use std::io::prelude::*;

use rhesus;

fn main() -> io::Result<()> {
    let mut stdout  = io::stdout();
    let stdin = io::stdin();
    let mut buf = String::with_capacity(128);
    println!("🐵 rhesus v0.1 🐵");
    loop {
        write!(&mut stdout, "> ")?;
        stdout.flush()?;
        stdin.read_line(&mut buf)?;
        println!("{:#?}", rhesus::lex::lex(&buf).collect::<Vec<_>>());
        buf.clear();
    }
}
