#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Illegal,
    EOF,
    Ident,
    Int,

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

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Assign => write!(f, "="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Bang => write!(f, "!"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            EQ => write!(f, "=="),
            NEQ => write!(f, "!="),
            LT => write!(f, "<"),
            GT => write!(f, ">"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            _ => unreachable!(),
        }
    }
}

pub fn is_to_keyword(token: &str) -> Option<TokenKind> {
    match token {
        "fn" => Some(TokenKind::Function),
        "let" => Some(TokenKind::Let),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "return" => Some(TokenKind::Return),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub t_type: TokenKind,
    pub literal: String,
}

impl Token {
    pub fn new(t_type: TokenKind, literal: String) -> Self {
        Self { t_type, literal }
    }
}
