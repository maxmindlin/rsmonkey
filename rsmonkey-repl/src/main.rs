use rustyline::error::ReadlineError;
use rustyline::Editor;

use rsmonkey_lexer::{Lexer, TokenType};

const PROMPT: &str = ">> ";

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    println!("Welcome to the RSMonkey programming language");
    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let mut lexer = Lexer::new(line);
                let mut tok = lexer.next_token();
                while tok.t_type != TokenType::EOF {
                    println!("{:?}", tok);
                    tok = lexer.next_token();
                };
            },
            Err(ReadlineError::Interrupted) => {
                println!("Exiting.");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("Eof");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
