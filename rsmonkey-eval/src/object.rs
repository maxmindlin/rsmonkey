use rsmonkey_parser::{BlockStmt, Identifier, TokenKind};
use std::rc::Rc;
use std::fmt;
use crate::EnvPointer;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    TypeMismatch(TokenKind, Object, Object),
    InvalidUsage(String),
    NonFunction(Object),
    UnknownIdent(Identifier),
    UnknownPrefixOp(TokenKind, Object),
    UnknownInfixOp(TokenKind, Object, Object),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            TypeMismatch(op, o1, o2) => write!(f, "type mismatch: {} {} {}", op, o1, o2),
            InvalidUsage(s) => write!(f, "invalid usage {}", s),
            NonFunction(obj) => write!(f, "{} is not a function", obj),
            UnknownPrefixOp(op, obj) => write!(f, "unknown operator: {}{}", op, obj),
            UnknownInfixOp(op, o1, o2) => write!(f, "unknown operator: {} {} {}", o1, op, o2),
            UnknownIdent(i) => write!(f, "unknown identifier: {}", i.name),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Return(Rc<Object>),
    Func(Rc<FuncLiteral>),
    Null,
    Error(Rc<EvalError>),
}

impl Object {
    pub fn inspect(&self) -> String {
        use Object::*;
        match self {
            Int(i) => i.to_string(),
            Boolean(b) => b.to_string(),
            Return(r) => r.inspect(),
            Null => "null".to_string(),
            Error(e) => e.to_string(),
            Func(fn_lit) => {
                let p_str = fn_lit.params
                    .iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<String>>()
                    .join(",");
                format!("fn ({}) {{\n{}\n}}", p_str, fn_lit.body)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncLiteral {
    pub params: Vec<Identifier>,
    pub body: BlockStmt,
    pub env: EnvPointer,
}

impl FuncLiteral {
    pub fn new(params: Vec<Identifier>, body: BlockStmt, env: EnvPointer) -> Self {
        Self { params, body, env }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}
