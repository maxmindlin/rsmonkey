mod env;
mod object;

pub use env::Env;
use object::{BuiltinKind, EvalError, FuncLiteral, Object};
use rsmonkey_parser::{BlockStmt, ExprKind, Identifier, NodeKind, Program, StmtKind, TokenKind};
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) type EnvPointer = Rc<RefCell<Env>>;

pub fn eval(node: NodeKind, env: EnvPointer) -> Rc<Object> {
    use NodeKind::*;
    match node {
        Program(p) => eval_program(p, env),
        Statement(s) => eval_stmt(&s, Rc::clone(&env)),
        Expr(e) => eval_expr(&e, env),
    }
}

fn eval_stmt(stmt: &StmtKind, env: EnvPointer) -> Rc<Object> {
    use StmtKind::*;
    match stmt {
        Expr(e) => eval_expr(e, env),
        Return(re) => match re {
            None => Rc::new(Object::Null),
            Some(e) => {
                let val = eval_expr(e, env);
                default_if_error(val, |obj| Rc::new(Object::Return(obj)))
            }
        },
        Let(id, m_exp) => match m_exp {
            Some(e) => {
                let val = eval_expr(e, Rc::clone(&env));
                default_if_error(val, |obj| {
                    env.borrow_mut().set(&id, obj);
                    Rc::new(Object::Null)
                })
            }
            None => {
                env.borrow_mut().set(&id, Rc::new(Object::Null));
                Rc::new(Object::Null)
            }
        },
    }
}

fn eval_expr(expr: &ExprKind, env: EnvPointer) -> Rc<Object> {
    use ExprKind::*;
    match expr {
        Array(v) => {
            let elems = eval_exprs(v, env);
            if elems.len() == 1 {
                let first = elems.first().unwrap();
                if let Object::Error(_) = **first {
                    return Rc::clone(first);
                }
            }
            Rc::new(Object::Array(elems))
        }
        Int(i) => Rc::new(Object::Int(*i)),
        Boolean(b) => Rc::new(Object::Boolean(*b)),
        Str(s) => Rc::new(Object::Str(s.to_owned())),
        Prefix(op, e) => {
            let val = eval_expr(e, env);
            default_if_error(val, |obj| Rc::new(eval_prefix_expr(&op, obj)))
        }
        Infix(op, lhs, rhs) => {
            let clhs = eval_expr(lhs, Rc::clone(&env));
            if let Object::Error(_) = *clhs {
                return clhs;
            }
            let crhs = eval_expr(rhs, env);
            if let Object::Error(_) = *crhs {
                return crhs;
            }
            Rc::new(eval_infix_expr(op, &clhs, &*crhs))
        }
        If(cond, conseq, alt) => {
            let val = eval_expr(cond, Rc::clone(&env));
            match *val {
                Object::Error(_) => val,
                _ if is_truthy(&val) => eval_block_stmt(conseq, env),
                _ => eval_block_stmt(alt, env),
            }
        }
        Ident(i) => eval_ident(&i, env),
        Fn(i, block) => Rc::new(Object::Func(Rc::new(FuncLiteral::new(
            i.clone(),
            block.clone(),
            env,
        )))),
        Call(fexpr, args) => {
            let func = eval_expr(&fexpr, Rc::clone(&env));
            if let Object::Error(_) = *func {
                return func;
            }
            let args = eval_exprs(&*args, env);
            if args.len() == 1 {
                let first = args.first().unwrap();
                if let Object::Error(_) = **first {
                    return Rc::clone(first);
                }
            }
            apply_fn(&func, &args)
        }
        Index(lexpr, iexpr) => {
            let lhs = eval_expr(&lexpr, Rc::clone(&env));
            if let Object::Error(_) = *lhs {
                return lhs;
            }
            let index = eval_expr(&iexpr, env);
            if let Object::Error(_) = *index {
                return index;
            }
            eval_index_expr(&lhs, &index)
        }
    }
}

fn eval_index_expr(lhs: &Object, index: &Object) -> Rc<Object> {
    use Object::*;
    match (lhs, index) {
        (Array(a), _) => eval_array_index(a, index),
        _ => new_error_pntr(EvalError::InvalidIndexOp((*lhs).clone())),
    }
}

fn eval_array_index(array: &Vec<Rc<Object>>, index: &Object) -> Rc<Object> {
    match index {
        Object::Int(i) => match i {
            _ if *i < 0 => Rc::new(Object::Null),
            _ => match array.get(*i as usize) {
                Some(o) => Rc::clone(o),
                None => Rc::new(Object::Null),
            },
        }
        _ => new_error_pntr(EvalError::InvalidUsage("invalid index type".to_string())),
    }
}

fn eval_exprs(exps: &Vec<ExprKind>, env: EnvPointer) -> Vec<Rc<Object>> {
    let mut out = Vec::new();
    for e in exps {
        let evaluated = eval_expr(e, Rc::clone(&env));
        match *evaluated {
            Object::Error(_) => return vec![evaluated],
            _ => out.push(evaluated),
        };
    }
    out
}

fn apply_fn(func: &Object, args: &Vec<Rc<Object>>) -> Rc<Object> {
    match func {
        Object::Func(f_lit) => {
            let env = extend_fn_env(f_lit, args);
            let eval = eval_block_stmt(&f_lit.body, env);
            match &*eval {
                Object::Return(v) => Rc::clone(v),
                _ => eval,
            }
        }
        Object::Builtin(b) => b.apply(args),
        _ => new_error_pntr(EvalError::NonFunction((*func).clone()))
    }
}

fn extend_fn_env(func: &FuncLiteral, args: &Vec<Rc<Object>>) -> EnvPointer {
    let mut new_env = Env::new(Some(Rc::clone(&func.env)));
    for (i, param) in func.params.iter().enumerate() {
        new_env.set(param, Rc::clone(args.get(i).unwrap()));
    }

    Rc::new(RefCell::new(new_env))
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => *b,
        _ => true,
    }
}

fn eval_ident(id: &Identifier, env: EnvPointer) -> Rc<Object> {
    let def = Object::Error(Rc::new(EvalError::UnknownIdent(id.clone())));
    match env.borrow().get(id) {
        Some(ro) => ro,
        None => match BuiltinKind::is_from(&id.name) {
            None => Rc::new(def),
            Some(built) => Rc::new(Object::Builtin(built)),
        },
    }
}

fn eval_infix_expr(op: &TokenKind, lhs: &Object, rhs: &Object) -> Object {
    use Object::*;
    match (lhs, rhs, op) {
        (Int(i), Int(j), _) => eval_int_infix(op, *i, *j),
        (Str(s1), Str(s2), _) => eval_str_infix(op, &s1, &s2),
        (Int(i), Str(s), TokenKind::Plus) => Object::Str(i.to_string() + s),
        (Str(s), Int(i), TokenKind::Plus) => Object::Str(s.to_owned() + &i.to_string()),
        (_, _, TokenKind::EQ) => Boolean(lhs == rhs),
        (_, _, TokenKind::NEQ) => Boolean(lhs != rhs),
        _ => Error(Rc::new(EvalError::UnknownInfixOp(
            *op,
            lhs.clone(),
            rhs.clone(),
        ))),
    }
}

fn eval_str_infix(op: &TokenKind, s1: &str, s2: &str) -> Object {
    use Object::*;
    use TokenKind as TK;
    match op {
        TK::Plus => Str(s1.to_string() + s2),
        _ => Error(Rc::new(EvalError::UnknownInfixOp(
            *op,
            Str(s1.to_string()),
            Str(s2.to_string()),
        ))),
    }
}

fn eval_int_infix(op: &TokenKind, i: i64, j: i64) -> Object {
    use Object::*;
    use TokenKind as TK;
    match op {
        TK::Plus => Int(i + j),
        TK::Minus => Int(i - j),
        TK::Asterisk => Int(i * j),
        TK::Slash => Int(i / j),
        TK::LT => Boolean(i < j),
        TK::GT => Boolean(i > j),
        TK::EQ => Boolean(i == j),
        TK::NEQ => Boolean(i != j),
        _ => Error(Rc::new(EvalError::UnknownInfixOp(*op, Int(i), Int(j)))),
    }
}

fn eval_prefix_expr(op: &TokenKind, rhs: Rc<Object>) -> Object {
    match op {
        TokenKind::Bang => eval_bang_op(rhs),
        TokenKind::Minus => eval_minus_prefix_op(rhs),
        _ => Object::Error(Rc::new(EvalError::UnknownPrefixOp(*op, (*rhs).clone()))),
    }
}

fn eval_minus_prefix_op(rhs: Rc<Object>) -> Object {
    use Object::*;
    match *rhs {
        Int(i) => Int(-i),
        _ => Error(Rc::new(EvalError::UnknownPrefixOp(
            TokenKind::Minus,
            (*rhs).clone(),
        ))),
    }
}

fn eval_bang_op(rhs: Rc<Object>) -> Object {
    use Object::*;
    match *rhs {
        Boolean(b) => Boolean(!b),
        Null => Boolean(true),
        _ => Boolean(false),
    }
}

fn eval_block_stmt(block: &BlockStmt, env: EnvPointer) -> Rc<Object> {
    let mut res = Rc::new(Object::Null);
    for s in &block.stmts {
        let val = eval_stmt(s, Rc::clone(&env));
        match *val {
            Object::Return(_) => return val,
            Object::Error(_) => return val,
            _ => res = val,
        }
    }
    res
}

fn eval_program(prgrm: Program, env: EnvPointer) -> Rc<Object> {
    let mut res = Rc::new(Object::Null);
    for s in prgrm.stmts {
        let val = eval(NodeKind::Statement(s), Rc::clone(&env));
        match &*val {
            Object::Return(r) => return Rc::clone(r),
            Object::Error(_) => return val,
            _ => res = val,
        }
    }
    res
}

fn default_if_error<F: Fn(Rc<Object>) -> Rc<Object>>(obj: Rc<Object>, wrap: F) -> Rc<Object> {
    match *obj {
        Object::Error(_) => obj,
        _ => wrap(obj),
    }
}

pub(crate) fn new_error_pntr(err: EvalError) -> Rc<Object> {
    Rc::new(Object::Error(Rc::new(err)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rsmonkey_parser::{Lexer, Parser};
    use test_case::test_case;

    fn get_eval_output(input: String) -> Rc<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prgrm = p.parse_program();
        let env = Env::new(None);
        eval(NodeKind::Program(prgrm), Rc::new(RefCell::new(env)))
    }

    #[test_case(
        "
let adder = fn(x) {
    fn(y) { x + y };
};

let addTwo = adder(2);
addTwo(2);",
        Object::Int(4);
        "simple closure call"
    )]
    fn test_closure_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case("let a = 5; a;", Object::Int(5))]
    #[test_case("let a = 5 * 5; a;", Object::Int(25))]
    fn test_let_stmt_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case(
        "5 + true;",
        Object::Error(
            Rc::new(
                EvalError::UnknownInfixOp(
                    TokenKind::Plus,
                    Object::Int(5),
                    Object::Boolean(true)
                )
            )
        ); "int + bool"
    )]
    #[test_case(
        "-true;",
        Object::Error(
            Rc::new(
                EvalError::UnknownPrefixOp(
                    TokenKind::Minus,
                    Object::Boolean(true)
                )
            )
        ); "neg bool"
    )]
    #[test_case(
        "true + false;",
        Object::Error(
            Rc::new(
                EvalError::UnknownInfixOp(
                    TokenKind::Plus,
                    Object::Boolean(true),
                    Object::Boolean(false)
                )
            )
        ); "bool + bool"
    )]
    #[test_case(
        "if (10 > 1) {
            if (10 > 1) {
                return true + false;
            }
            return 1;
        }",
        Object::Error(
            Rc::new(
                EvalError::UnknownInfixOp(
                    TokenKind::Plus,
                    Object::Boolean(true),
                    Object::Boolean(false)
                )
            )
        ); "block stmts"
    )]
    fn test_error(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case(
        "
        if (10 > 1) {
            if (10 > 1) {
                return 10;
            }
            return 1;
        }",
        Object::Int(10);
        "nested return"
    )]
    fn test_return(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case("5", 5; "base case")]
    #[test_case("10", 10; "base case 2")]
    #[test_case("-5", -5; "neg case")]
    #[test_case("-10", -10; "neg case 2")]
    #[test_case("5 + 5 + 5 - 10", 5; "add & sub")]
    #[test_case("40 / 2 * 3 + 10", 70; "divide & mult")]
    #[test_case("10 - (3 + 2) * 2", 0; "precedence check")]
    fn test_int_obj(input: &str, exp: i64) {
        match &*get_eval_output(input.to_string()) {
            Object::Int(i) => assert_eq!(*i, exp),
            n @ _ => panic!("expected int object, got {:#?}", n),
        };
    }

    #[test_case(r#""hello world!""#, Object::Str("hello world!".to_string()); "hello object")]
    #[test_case(r#""hello " + "world!""#, Object::Str("hello world!".to_string()); "add strings")]
    #[test_case(r#"1 + "hello!""#, Object::Str("1hello!".to_string()); "int + string")]
    #[test_case(r#""hello" + 2"#, Object::Str("hello2".to_string()); "string + int")]
    fn test_str_obj(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case("if (true) {10}", Object::Int(10); "base case")]
    #[test_case("if (false) {10}", Object::Null; "missing alt")]
    #[test_case("if (1) {10}", Object::Int(10); "int is truthy")]
    #[test_case("if (1 < 3) {10}", Object::Int(10); "eval condition")]
    fn test_if_else_expr(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case("10 > 5", true; "base GT")]
    #[test_case("10 < 5", false; "base LT")]
    #[test_case("5 * 3 > 5", true; "GT comp")]
    #[test_case("5 == 5", true; "base EQ")]
    #[test_case("10 != 4", true; "base NEQ")]
    fn test_int_to_bool(input: &str, exp: bool) {
        match &*get_eval_output(input.to_string()) {
            Object::Boolean(i) => assert_eq!(*i, exp),
            n @ _ => panic!("expected bool object, got {:#?}", n),
        };
    }

    #[test_case(r#"len("hello!")"#, Object::Int(6); "len str")]
    fn test_builtins(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }

    #[test_case("true", true; "base true")]
    #[test_case("false", false; "base false")]
    #[test_case("!true", false; "bang true")]
    #[test_case("!false", true; "bang false")]
    #[test_case("!5", false; "bang int")]
    #[test_case("!!true", true; "bang bang true")]
    #[test_case("!!false", false; "bang bang false")]
    #[test_case("!!5", true; "bang bang int")]
    #[test_case("(1 < 2) == true", true; "comp EQ")]
    fn test_bool_obj(input: &str, exp: bool) {
        match &*get_eval_output(input.to_string()) {
            Object::Boolean(b) => assert_eq!(*b, exp),
            n @ _ => panic!("expected bool object, got {:#?}", n),
        }
    }

    #[test_case(
        r#"[1, 1 * 3, "hello"]"#, 
        Object::Array(vec!(
                Rc::new(Object::Int(1)),
                Rc::new(Object::Int(3)),
                Rc::new(Object::Str("hello".to_string()))
        )); "mixed vec"
    )]
    fn test_array_literal(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input.to_string()), exp);
    }
}
