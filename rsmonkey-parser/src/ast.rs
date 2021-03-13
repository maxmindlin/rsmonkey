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
        Self {
            stmts: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Let(Identifier, Option<ExprKind>),
    Return(Option<ExprKind>),
    Expr(ExprKind),
}

impl fmt::Display for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(id, s_v) => write!(f, "let {} = {:?}", id.name, s_v),
            Self::Return(v) => write!(f, "return {:?}", v),
            Self::Expr(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Ident(Identifier),
    Int(u64),
    Prefix(String, Box<ExprKind>),
    Infix(String, Box<ExprKind>, Box<ExprKind>),
    Boolean(bool),
    If(Box<ExprKind>, Option<BlockStmt>, Option<BlockStmt>),
    Fn(Vec<Identifier>, Option<BlockStmt>),
    Call(Box<ExprKind>, Vec<Box<ExprKind>>),
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(id) => write!(f, "{}", id.name),
            Self::Int(n) => write!(f, "{}", n),
            Self::Prefix(op, rhs) => write!(f, "({}{})", op, rhs),
            Self::Infix(op, lhs, rhs, ..) => write!(f, "({} {} {})", lhs, op, rhs),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::If(cond, m_cons, m_alt) => {
                write!(f, "if {}", cond)?;
                if let Some(cons) = m_cons {
                    write!(f, " {}", cons)?;
                }
                if let Some(alt) = m_alt {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            },
            Self::Fn(params, m_block) => {
                write!(f, "fn(")?;
                let s = params.iter()
                    .map(|i| i.name.to_owned())
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}", s)?;
                write!(f, ")")?;
                if let Some(block) = m_block {
                    write!(f, "{}", block)?;
                }
                Ok(())
            },
            Self::Call(func, args) => {
                write!(f, "{}(", func)?;
                let a_str = args.iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{})", a_str)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStmt {
    stmts: Vec<StmtKind>,
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
