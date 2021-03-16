use rustyline::error::ReadlineError;
use rustyline::Editor;

use rsmonkey_eval::{eval, Env};
use rsmonkey_parser::{Lexer, NodeKind, Parser};

use std::rc::Rc;
use std::cell::RefCell;

const PROMPT: &str = ">> ";

fn main() {
    let mut rl = Editor::<()>::new();
    let env = Rc::new(RefCell::new(Env::new(None)));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    println!("Welcome to the RSMonkey programming language");
    println!("Press CTRL-c to exit.");
    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = Lexer::new(line);
                let mut parser = Parser::new(lexer);
                let prgrm = parser.parse_program();
                if !parser.errors.is_empty() {
                    println!(
                        "{} parser errors: {:#?}",
                        parser.errors.len(),
                        parser.errors
                    );
                    continue;
                }
                let out = eval(NodeKind::Program(prgrm), Rc::clone(&env));
                println!("{}", out);
            }
            Err(ReadlineError::Interrupted) => {
                println!("Exiting.");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Eof");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
