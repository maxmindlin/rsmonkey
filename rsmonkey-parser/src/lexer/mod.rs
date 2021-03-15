mod token;

use token::is_to_keyword;
pub use token::{Token, TokenKind};

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
        }
        i.iter().collect()
    }

    fn read_numeric(&mut self) -> String {
        let start = self.pos;
        let mut i = vec![self.input[start]];
        while self.peek().is_some() && self.peek().unwrap().is_numeric() {
            i.push(*self.next().unwrap());
        }
        i.iter().collect()
    }

    pub fn next_token(&mut self) -> Token {
        match self.next() {
            Some(c) => match *c {
                '=' => match self.peek() {
                    Some(pc) => match *pc {
                        '=' => {
                            let mut lit = '='.to_string();
                            lit.push(*self.next().unwrap());
                            Token::new(TokenKind::EQ, lit)
                        }
                        _ => Token::new(TokenKind::Assign, '='.to_string()),
                    },
                    None => Token::new(TokenKind::Assign, '='.to_string()),
                },
                ';' => Token::new(TokenKind::Semicolon, c.to_string()),
                '(' => Token::new(TokenKind::LParen, c.to_string()),
                ')' => Token::new(TokenKind::RParen, c.to_string()),
                ',' => Token::new(TokenKind::Comma, c.to_string()),
                '+' => Token::new(TokenKind::Plus, c.to_string()),
                '{' => Token::new(TokenKind::LBrace, c.to_string()),
                '}' => Token::new(TokenKind::RBrace, c.to_string()),
                '-' => Token::new(TokenKind::Minus, c.to_string()),
                '!' => match self.peek() {
                    Some(pc) => match *pc {
                        '=' => {
                            let mut lit = '!'.to_string();
                            lit.push(*self.next().unwrap());
                            Token::new(TokenKind::NEQ, lit)
                        }
                        _ => Token::new(TokenKind::Bang, '!'.to_string()),
                    },
                    _ => Token::new(TokenKind::Bang, '!'.to_string()),
                },
                '*' => Token::new(TokenKind::Asterisk, c.to_string()),
                '/' => Token::new(TokenKind::Slash, c.to_string()),
                '<' => Token::new(TokenKind::LT, c.to_string()),
                '>' => Token::new(TokenKind::GT, c.to_string()),
                _ if c.is_whitespace() => self.next_token(),
                _ if c.is_alphabetic() => {
                    let literal = self.read_identifier();
                    match is_to_keyword(&literal) {
                        Some(key_token) => Token::new(key_token, literal),
                        None => Token::new(TokenKind::Ident, literal),
                    }
                }
                _ if c.is_numeric() => {
                    let literal = self.read_numeric();
                    Token::new(TokenKind::Int, literal)
                }
                _ => Token::new(TokenKind::Illegal, c.to_string()),
            },
            None => Token::new(TokenKind::EOF, "".to_string()),
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
            TokenKind::Assign,
            TokenKind::Plus,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::Comma,
            TokenKind::Semicolon,
            TokenKind::EOF,
        ];

        exp.iter().for_each(|c| {
            let tok = l.next_token();
            assert_eq!(tok.t_type, *c);
        });
    }

    #[test]
    fn test_operators() {
        let i = "!-/*5;
5 < 10 > != 5 ==;"
            .to_string();

        let mut l = Lexer::new(i);
        let exp = vec![
            TokenKind::Bang,
            TokenKind::Minus,
            TokenKind::Slash,
            TokenKind::Asterisk,
            TokenKind::Int,
            TokenKind::Semicolon,
            TokenKind::Int,
            TokenKind::LT,
            TokenKind::Int,
            TokenKind::GT,
            TokenKind::NEQ,
            TokenKind::Int,
            TokenKind::EQ,
            TokenKind::Semicolon,
        ];

        exp.iter().for_each(|c| {
            let tok = l.next_token();
            assert_eq!(tok.t_type, *c);
        });
    }

    #[test]
    fn test_keywords() {
        let i = "fn let true false if else return".to_string();

        let mut l = Lexer::new(i);
        let exp = vec![
            TokenKind::Function,
            TokenKind::Let,
            TokenKind::True,
            TokenKind::False,
            TokenKind::If,
            TokenKind::Else,
            TokenKind::Return,
        ];

        exp.iter().for_each(|c| {
            let tok = l.next_token();
            assert_eq!(tok.t_type, *c);
        });
    }

    #[test]
    fn test_identifiers() {
        let i = "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);"
            .to_string();

        let mut l = Lexer::new(i);
        let exp = vec![
            TokenKind::Let,
            TokenKind::Ident,
            TokenKind::Assign,
            TokenKind::Int,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident,
            TokenKind::Assign,
            TokenKind::Int,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident,
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::LParen,
            TokenKind::Ident,
            TokenKind::Comma,
            TokenKind::Ident,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Ident,
            TokenKind::Plus,
            TokenKind::Ident,
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident,
            TokenKind::Assign,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::Ident,
            TokenKind::Comma,
            TokenKind::Ident,
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::EOF,
        ];

        exp.iter().for_each(|c| {
            let tok = l.next_token();
            assert_eq!(tok.t_type, *c);
        });
    }
}
