use crate::lexer::TokenKind;
use std::fmt;

pub enum NodeKind {
    Program(Program),
    Statement(StmtKind),
    Expr(ExprKind),
}

pub struct Program {
    pub stmts: Vec<StmtKind>,
}

impl Program {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Let(Identifier, Option<ExprKind>),
    Return(Option<ExprKind>),
    Expr(ExprKind),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Ident(Identifier),
    Int(i64),
    Prefix(TokenKind, Box<ExprKind>),
    Infix(TokenKind, Box<ExprKind>, Box<ExprKind>),
    Boolean(bool),
    If(Box<ExprKind>, BlockStmt, BlockStmt),
    Fn(Vec<Identifier>, BlockStmt),
    // func & arguments
    Call(Box<ExprKind>, Box<Vec<ExprKind>>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<StmtKind>,
}

impl BlockStmt {
    pub fn new(stmts: Vec<StmtKind>) -> Self {
        Self { stmts }
    }
}

impl fmt::Display for BlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in self.stmts.iter() {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

impl fmt::Display for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(id, s_v) => {
                write!(f, "let {}", id.name)?;
                match s_v {
                    Some(e) => write!(f, " = {};", e),
                    None => write!(f, ";"),
                }
            }
            Self::Return(v) => write!(f, "return {:?}", v),
            Self::Expr(e) => write!(f, "{}", e),
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(id) => write!(f, "{}", id.name),
            Self::Int(n) => write!(f, "{}", n),
            Self::Prefix(op, rhs) => write!(f, "({}{})", op, rhs),
            Self::Infix(op, lhs, rhs, ..) => write!(f, "({} {} {})", lhs, op, rhs),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::If(cond, cons, alt) => {
                write!(f, "if {}", cond)?;
                write!(f, " {}", cons)?;
                if !alt.stmts.is_empty() {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Self::Fn(params, block) => {
                write!(f, "fn(")?;
                let s = params
                    .iter()
                    .map(|i| i.name.to_owned())
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}", s)?;
                write!(f, ")")?;
                write!(f, "{}", block)?;
                Ok(())
            }
            Self::Call(func, args) => {
                write!(f, "{}(", func)?;
                let a_str = args
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{})", a_str)
            }
        }
    }
}
