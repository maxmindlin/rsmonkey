mod token;

pub use token::TokenType;
use token::is_to_keyword;

#[derive(Default)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    read_pos: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input: input.chars().collect(),
            ..Default::default()
        }
    }

    fn next(&mut self) -> Option<&char> {
        let out = self.input.get(self.read_pos);
        self.pos = self.read_pos;
        self.read_pos += 1;
        out
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.get(self.read_pos)
    }

    fn read_identifier(&mut self) -> String {
        let start = self.pos;
        let mut i = vec![self.input[start]];
        while self.peek().is_some() && self.peek().unwrap().is_alphabetic() {
            i.push(*self.next().unwrap());
        };
        i.iter().collect()
    }

    fn read_numeric(&mut self) -> String {
        let start = self.pos;
        let mut i = vec![self.input[start]];
        while self.peek().is_some() && self.peek().unwrap().is_numeric() {
            i.push(*self.next().unwrap());
        };
        i.iter().collect()
    }

    pub fn next_token(&mut self) -> TokenType {
        match self.next() {
            Some(c) => {
                match *c {
                    '=' => {
                        match self.peek() {
                            Some(pc) => {
                                match *pc {
                                    '=' => {
                                        let _ = self.next();
                                        TokenType::EQ
                                    },
                                    _ => TokenType::Assign,
                                }
                            },
                            None => TokenType::Assign,
                        }
                    },
                    ';' => TokenType::Semicolon,
                    '(' => TokenType::LParen,
                    ')' => TokenType::RParen,
                    ',' => TokenType::Comma,
                    '+' => TokenType::Plus,
                    '{' => TokenType::LBrace,
                    '}' => TokenType::RBrace,
                    '-' => TokenType::Minus,
                    '!' => {
                        match self.peek() {
                            Some(pc) => {
                                match *pc {
                                    '=' => {
                                        let _ = self.next();
                                        TokenType::NEQ
                                    },
                                    _ => TokenType::Bang,
                                }
                            },
                            None => TokenType::Bang,
                        }
                    },
                    '*' => TokenType::Asterisk,
                    '/' => TokenType::Slash,
                    '<' => TokenType::LT,
                    '>' => TokenType::GT,
                    _ if c.is_whitespace() => self.next_token(),
                    _ if c.is_alphabetic() => {
                        let literal = self.read_identifier();
                        match is_to_keyword(&literal) {
                            Some(key_token) => key_token,
                            None => TokenType::Ident(literal),
                        }
                    }
                    _ if c.is_numeric() => {
                        let literal = self.read_numeric();
                        TokenType::Int(literal)
                    },
                    _ => TokenType::Illegal,
                }
            },
            None => TokenType::EOF,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let i = "=+(){},;".to_string();
        let mut l = Lexer::new(i);
        let exp = vec![
            TokenType::Assign,
            TokenType::Plus,
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Comma,
            TokenType::Semicolon,
            TokenType::EOF,
        ];

        exp.iter()
            .for_each(|c| {
                let tok = l.next_token();
                assert_eq!(tok, *c);
            });
    }

    #[test]
    fn test_operators() {
        let i = "!-/*5;
5 < 10 > != 5 ==;".to_string();

        let mut l = Lexer::new(i);
        let exp = vec![
            TokenType::Bang,
            TokenType::Minus,
            TokenType::Slash,
            TokenType::Asterisk,
            TokenType::Int("5".to_string()),
            TokenType::Semicolon,
            TokenType::Int("5".to_string()),
            TokenType::LT,
            TokenType::Int("10".to_string()),
            TokenType::GT,
            TokenType::NEQ,
            TokenType::Int("5".to_string()),
            TokenType::EQ,
            TokenType::Semicolon,
        ];

        exp.iter()
            .for_each(|c| {
                let tok = l.next_token();
                assert_eq!(tok, *c);
            });
    }

    #[test]
    fn test_keywords() {
        let i = "fn let true false if else return".to_string();

        let mut l = Lexer::new(i);
        let exp = vec![
            TokenType::Function,
            TokenType::Let,
            TokenType::True,
            TokenType::False,
            TokenType::If,
            TokenType::Else,
            TokenType::Return,
        ];

        exp.iter()
            .for_each(|c| {
                let tok = l.next_token();
                assert_eq!(tok, *c);
            });
    }

    #[test]
    fn test_identifiers() {
        let i = "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);".to_string();

        let mut l = Lexer::new(i);
        let exp = vec![
            TokenType::Let,
            TokenType::Ident("five".to_string()),
            TokenType::Assign,
            TokenType::Int("5".to_string()),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident("ten".to_string()),
            TokenType::Assign,
            TokenType::Int("10".to_string()),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident("add".to_string()),
            TokenType::Assign,
            TokenType::Function,
            TokenType::LParen,
            TokenType::Ident("x".to_string()),
            TokenType::Comma,
            TokenType::Ident("y".to_string()),
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::Ident("x".to_string()),
            TokenType::Plus,
            TokenType::Ident("y".to_string()),
            TokenType::Semicolon,
            TokenType::RBrace,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident("result".to_string()),
            TokenType::Assign,
            TokenType::Ident("add".to_string()),
            TokenType::LParen,
            TokenType::Ident("five".to_string()),
            TokenType::Comma,
            TokenType::Ident("ten".to_string()),
            TokenType::RParen,
            TokenType::Semicolon,
            TokenType::EOF,
        ];

        exp.iter()
            .for_each(|c| {
                let tok = l.next_token();
                assert_eq!(tok, *c);
            });
    }
}
