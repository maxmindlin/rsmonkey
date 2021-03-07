#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Illegal,
    EOF,
    Ident(String),
    Int(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    EQ,
    NEQ,

    LT,
    GT,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn is_to_keyword(token: &str) -> Option<TokenType> {
    match token {
        "fn" => Some(TokenType::Function),
        "let" => Some(TokenType::Let),
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "return" => Some(TokenType::Return),
        _ => None
    }
}

pub struct Token {
    t_type: TokenType,
    literal: String,
}

impl Token {
    pub fn new(t_type: TokenType, literal: String) -> Self {
        Self { t_type, literal }
    }
}
