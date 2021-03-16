use crate::{EnvPointer, new_error_pntr};
use rsmonkey_parser::{BlockStmt, Identifier, TokenKind};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    TypeMismatch(TokenKind, Object, Object),
    InvalidUsage(String),
    InvalidIndexOp(Object),
    NonFunction(Object),
    UnknownIdent(Identifier),
    UnknownPrefixOp(TokenKind, Object),
    UnknownInfixOp(TokenKind, Object, Object),
}

pub fn new_object_error_wrapper(err: EvalError) -> Object {
    Object::Error(Rc::new(err))
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            TypeMismatch(op, o1, o2) => write!(f, "type mismatch: {} {} {}", op, o1, o2),
            InvalidUsage(s) => write!(f, "invalid usage {}", s),
            InvalidIndexOp(o) => write!(f, "invalid index usage: {}", o),
            NonFunction(obj) => write!(f, "{} is not a function", obj),
            UnknownPrefixOp(op, obj) => write!(f, "unknown operator: {}{}", op, obj),
            UnknownInfixOp(op, o1, o2) => write!(f, "unknown operator: {} {} {}", o1, op, o2),
            UnknownIdent(i) => write!(f, "unknown identifier: {}", i.name),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Array(Vec<Rc<Object>>),
    Int(i64),
    Boolean(bool),
    Builtin(BuiltinKind),
    Str(String),
    Return(Rc<Object>),
    Func(Rc<FuncLiteral>),
    Null,
    Error(Rc<EvalError>),
}

impl Object {
    pub fn inspect(&self) -> String {
        use Object::*;
        match self {
            Array(v) => v
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<String>>()
                .join(","),
            Int(i) => i.to_string(),
            Boolean(b) => b.to_string(),
            Builtin(b) => b.to_string(),
            Return(r) => r.inspect(),
            Str(s) => s.clone(),
            Null => "null".to_string(),
            Error(e) => e.to_string(),
            Func(fn_lit) => {
                let p_str = fn_lit
                    .params
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
pub enum BuiltinKind {
    Len,
    First,
    Last,
    Rest,
}

impl BuiltinKind {
    pub fn is_from(s: &str) -> Option<Self> {
        use BuiltinKind::*;
        match s {
            "len" => Some(Len),
            "first" => Some(First),
            "last" => Some(Last),
            "rest" => Some(Rest),
            _ => None,
        }
    }

    fn check_arg_len(&self, args: &Vec<Rc<Object>>, size: usize) -> Result<(), Object> {
        if args.len() != size {
            let msg = format!("{} only takes {} parameter(s)", self.to_string(), size);
            Err(new_object_error_wrapper(EvalError::InvalidUsage(msg)))
        } else {
            Ok(())
        }
    }

    pub fn apply(&self, args: &Vec<Rc<Object>>) -> Rc<Object> {
        use BuiltinKind::*;
        match self {
            Len => {
                if let Err(eo) = self.check_arg_len(args, 1) {
                    return Rc::new(eo);
                }
                let first = Rc::clone(args.first().unwrap());
                let out = match &*first {
                    Object::Str(s) => Object::Int(s.len() as i64),
                    Object::Array(a) => Object::Int(a.len() as i64),
                    _ => new_object_error_wrapper(EvalError::InvalidUsage(format!(
                        "{} does not support {}",
                        self.to_string(),
                        first
                    ))),
                };
                Rc::new(out)
            },
            First => {
                if let Err(eo) = self.check_arg_len(args, 1) {
                    return Rc::new(eo);
                }
                let first = Rc::clone(args.first().unwrap());
                match &*first {
                    Object::Array(a) => match a.first() {
                        Some(o) => Rc::clone(o),
                        None => Rc::new(Object::Null),
                    }
                    _ => new_error_pntr(EvalError::InvalidUsage(format!(
                        "{} does not support type {}",
                        self.to_string(),
                        first,
                    )))
                }
            },
            Last => {
                if let Err(eo) = self.check_arg_len(args, 1) {
                    return Rc::new(eo);
                }
                let first = Rc::clone(args.first().unwrap());
                match &*first {
                    Object::Array(a) => match a.last() {
                        Some(o) => Rc::clone(o),
                        None => Rc::new(Object::Null)
                    },
                    _ => new_error_pntr(EvalError::InvalidUsage(format!(
                        "{} does not support type {}",
                        self.to_string(),
                        first,
                    )))
                }
            }
            Rest => {
                if let Err(eo) = self.check_arg_len(args, 1) {
                    return Rc::new(eo);
                }
                let first = Rc::clone(args.first().unwrap());
                match &*first {
                    Object::Array(a) => {
                        let mut out = Vec::new();
                        match a.get(1..a.len()) {
                            None => {},
                            Some(s) => {
                                for o in s.iter() {
                                    out.push(Rc::new((**o).clone()));
                                }
                            },
                        };

                        Rc::new(Object::Array(out))
                    },
                    _ => new_error_pntr(EvalError::InvalidUsage(format!(
                        "{} does not support type {}",
                        self.to_string(),
                        first,
                    )))
                }
            }
        }
    }
}

impl ToString for BuiltinKind {
    fn to_string(&self) -> String {
        use BuiltinKind::*;
        match self {
            Len => "len".to_string(),
            First => "first".to_string(),
            Last => "last".to_string(),
            Rest => "rest".to_string(),
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
