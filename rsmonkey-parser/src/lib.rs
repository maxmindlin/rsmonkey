mod ast;
mod lexer;

pub use ast::{BlockStmt, ExprKind, Identifier, NodeKind, Program, StmtKind};
pub use lexer::{Lexer, Token, TokenKind};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<ExprKind>;
type InfixParseFn = fn(parser: &mut Parser, ExprKind) -> Option<ExprKind>;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(TokenKind, TokenKind),
    InvalidInteger(String),
    UnknownPrefixTokenFn(TokenKind),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(exp, got) => {
                write!(f, "expected next to be {:?}, got {:?} instead", exp, got)
            }
            Self::InvalidInteger(s) => write!(f, "could not parse {} as integer", s),
            Self::UnknownPrefixTokenFn(t) => write!(f, "unknown prefix fn for type {:?}", t),
        }
    }
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
enum Precendence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<TokenKind> for Precendence {
    fn from(value: TokenKind) -> Self {
        use TokenKind::*;
        match value {
            EQ => Self::Equals,
            NEQ => Self::Equals,
            LT => Self::LessGreater,
            GT => Self::LessGreater,
            Plus => Self::Sum,
            Minus => Self::Sum,
            Slash => Self::Product,
            Asterisk => Self::Product,
            LParen => Self::Call,
            LBracket => Self::Index,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser {
    lex: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<ParserError>,
}

fn map_prefix_fn(t_type: &TokenKind) -> Option<PrefixParseFn> {
    use TokenKind::*;
    match t_type {
        Ident => Some(Parser::parse_identifier),
        Int => Some(Parser::parse_integer_literal),
        Bang => Some(Parser::parse_prefix_expression),
        Minus => Some(Parser::parse_prefix_expression),
        True => Some(Parser::parse_boolean),
        False => Some(Parser::parse_boolean),
        LParen => Some(Parser::parse_grouped_expr),
        LBracket => Some(Parser::parse_array_literal),
        If => Some(Parser::parse_if_expr),
        Function => Some(Parser::parse_fn_expr),
        Str => Some(Parser::parse_str_literal),
        _ => None,
    }
}

fn map_infix_fn(t_type: &TokenKind) -> Option<InfixParseFn> {
    use TokenKind::*;
    match t_type {
        Plus => Some(Parser::parse_infix_expr),
        Minus => Some(Parser::parse_infix_expr),
        Slash => Some(Parser::parse_infix_expr),
        Asterisk => Some(Parser::parse_infix_expr),
        EQ => Some(Parser::parse_infix_expr),
        NEQ => Some(Parser::parse_infix_expr),
        LT => Some(Parser::parse_infix_expr),
        GT => Some(Parser::parse_infix_expr),
        LParen => Some(Parser::parse_call_expr),
        LBracket => Some(Parser::parse_index_expr),
        _ => None,
    }
}

impl Parser {
    pub fn new(mut lex: Lexer) -> Self {
        let curr_token = lex.next_token();
        let peek_token = lex.next_token();
        Self {
            lex,
            curr_token,
            peek_token,
            errors: Default::default(),
        }
    }

    fn next_token(&mut self) {
        let prev = std::mem::replace(&mut self.peek_token, self.lex.next_token());
        self.curr_token = prev;
    }

    fn parse_statement(&mut self) -> Option<StmtKind> {
        match self.curr_token.t_type {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Return => Some(self.parse_return_stmt()),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_return_stmt(&mut self) -> StmtKind {
        self.next_token();
        let val = self.parse_expression(Precendence::Lowest);
        StmtKind::Return(val)
    }

    fn parse_block_stmt(&mut self) -> BlockStmt {
        let mut stmts: Vec<StmtKind> = Vec::new();
        self.next_token();

        while self.curr_token.t_type != TokenKind::RBrace
            && self.curr_token.t_type != TokenKind::EOF
        {
            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            }
            self.next_token();
        }

        BlockStmt::new(stmts)
    }

    fn parse_let_stmt(&mut self) -> Option<StmtKind> {
        if !self.expect_peek(TokenKind::Ident) {
            return None;
        }

        let id = Identifier::new(self.curr_token.literal.clone());

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        self.next_token();

        let val = self.parse_expression(Precendence::Lowest);

        if self.peek_token.t_type == TokenKind::Semicolon {
            self.next_token();
        }

        Some(StmtKind::Let(id, val))
    }

    fn parse_expression_stmt(&mut self) -> Option<StmtKind> {
        let expression = self.parse_expression(Precendence::Lowest)?;
        let stmt = StmtKind::Expr(expression);

        if self.peek_token.t_type == TokenKind::Semicolon {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_prefix_expression(&mut self) -> Option<ExprKind> {
        let op = self.curr_token.t_type.clone();
        self.next_token();

        let rhs = self.parse_expression(Precendence::Prefix)?;
        Some(ExprKind::Prefix(op, Box::new(rhs)))
    }

    fn parse_infix_expr(&mut self, lhs: ExprKind) -> Option<ExprKind> {
        let op = self.curr_token.t_type.clone();
        let precendence = self.curr_precedence();
        self.next_token();
        let rhs = self.parse_expression(precendence)?;

        Some(ExprKind::Infix(op, Box::new(lhs), Box::new(rhs)))
    }

    fn parse_if_expr(&mut self) -> Option<ExprKind> {
        if !self.expect_peek(TokenKind::LParen) {
            return None;
        }

        self.next_token();
        let cond = self.parse_expression(Precendence::Lowest)?;
        if !self.expect_peek(TokenKind::RParen) {
            return None;
        }

        if !self.expect_peek(TokenKind::LBrace) {
            return None;
        }

        let conseq = self.parse_block_stmt();
        let mut alt: BlockStmt = BlockStmt::new(Vec::new());
        if self.peek_token.t_type == TokenKind::Else {
            self.next_token();
            if !self.expect_peek(TokenKind::LBrace) {
                return None;
            }

            alt = self.parse_block_stmt();
        }
        Some(ExprKind::If(Box::new(cond), conseq, alt))
    }

    fn parse_fn_expr(&mut self) -> Option<ExprKind> {
        if !self.expect_peek(TokenKind::LParen) {
            return None;
        }

        let params = self.parse_fn_params().ok()?;

        if !self.expect_peek(TokenKind::LBrace) {
            return None;
        }

        let body = self.parse_block_stmt();
        Some(ExprKind::Fn(params, body))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Identifier>, ()> {
        let mut params: Vec<Identifier> = Vec::new();
        if self.peek_token.t_type == TokenKind::RParen {
            self.next_token();
            return Ok(params);
        }

        self.next_token();

        params.push(Identifier::new(self.curr_token.literal.clone()));
        while self.peek_token.t_type == TokenKind::Comma {
            self.next_token();
            self.next_token();
            params.push(Identifier::new(self.curr_token.literal.clone()));
        }

        if !self.expect_peek(TokenKind::RParen) {
            return Err(());
        }

        Ok(params)
    }

    fn parse_index_expr(&mut self, lhs: ExprKind) -> Option<ExprKind> {
        self.next_token();
        let index = self.parse_expression(Precendence::Lowest)?;
        if !self.expect_peek(TokenKind::RBracket) {
            None
        } else {
            Some(ExprKind::Index(Box::new(lhs), Box::new(index)))
        }
    }

    fn parse_array_literal(&mut self) -> Option<ExprKind> {
        let elems = self.parse_expr_list(TokenKind::RBracket).ok()?;
        Some(ExprKind::Array(elems))
    }

    fn parse_call_expr(&mut self, func: ExprKind) -> Option<ExprKind> {
        let args = self.parse_expr_list(TokenKind::RParen).ok()?;
        Some(ExprKind::Call(Box::new(func), args))
    }

    fn parse_expr_list(&mut self, end: TokenKind) -> Result<Box<Vec<ExprKind>>, ()> {
        let mut args: Vec<ExprKind> = Vec::new();
        if self.peek_token.t_type == end {
            self.next_token();
            return Ok(Box::new(args));
        }

        self.next_token();
        match self.parse_expression(Precendence::Lowest) {
            Some(e) => {
                args.push(e);
            }
            None => {
                return Err(());
            }
        };

        while self.peek_token.t_type == TokenKind::Comma {
            self.next_token();
            self.next_token();
            match self.parse_expression(Precendence::Lowest) {
                Some(e) => {
                    args.push(e);
                }
                None => {
                    return Err(());
                }
            };
        }

        if !self.expect_peek(end) {
            return Err(());
        }

        Ok(Box::new(args))
    }

    fn parse_grouped_expr(&mut self) -> Option<ExprKind> {
        self.next_token();
        let exp = self.parse_expression(Precendence::Lowest);
        if !self.expect_peek(TokenKind::RParen) {
            return None;
        }
        exp
    }

    fn peek_precedence(&self) -> Precendence {
        Precendence::from(self.peek_token.t_type)
    }

    fn curr_precedence(&self) -> Precendence {
        Precendence::from(self.curr_token.t_type)
    }

    fn parse_expression(&mut self, precendence: Precendence) -> Option<ExprKind> {
        match map_prefix_fn(&self.curr_token.t_type) {
            None => {
                self.errors
                    .push(ParserError::UnknownPrefixTokenFn(self.curr_token.t_type));
                None
            }
            Some(f) => {
                let mut lhs = f(self)?;
                while self.peek_token.t_type != TokenKind::Semicolon
                    && precendence < self.peek_precedence()
                {
                    match map_infix_fn(&self.peek_token.t_type) {
                        None => {
                            return Some(lhs);
                        }
                        Some(in_fn) => {
                            self.next_token();

                            lhs = in_fn(self, lhs)?;
                        }
                    }
                }

                Some(lhs)
            }
        }
    }

    fn parse_integer_literal(&mut self) -> Option<ExprKind> {
        let to_parse = self.curr_token.clone().literal;
        match to_parse.parse::<i64>() {
            Ok(i) => Some(ExprKind::Int(i)),
            Err(_) => {
                self.errors.push(ParserError::InvalidInteger(to_parse));
                None
            }
        }
    }

    fn parse_str_literal(&mut self) -> Option<ExprKind> {
        Some(ExprKind::Str(self.curr_token.literal.clone()))
    }

    fn parse_boolean(&mut self) -> Option<ExprKind> {
        let value = self.curr_token.t_type == TokenKind::True;
        Some(ExprKind::Boolean(value))
    }

    fn parse_identifier(&mut self) -> Option<ExprKind> {
        Some(ExprKind::Ident(Identifier::new(
            self.curr_token.clone().literal,
        )))
    }

    fn expect_peek(&mut self, expected: TokenKind) -> bool {
        if self.peek_token.t_type == expected {
            self.next_token();
            true
        } else {
            self.peek_error(expected);
            false
        }
    }

    fn peek_error(&mut self, expected: TokenKind) {
        self.errors.push(ParserError::UnexpectedToken(
            expected,
            self.peek_token.t_type,
        ));
    }

    pub fn parse_program(&mut self) -> Program {
        let mut prgm = Program::new();
        while self.curr_token.t_type != TokenKind::EOF {
            if let Some(stmt) = self.parse_statement() {
                prgm.stmts.push(stmt);
            }
            self.next_token();
        }

        prgm
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    fn setup_prgm(input: String) -> (Parser, Program) {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let pgrm = p.parse_program();
        (p, pgrm)
    }

    #[test_case(
        String::from("!15;-5;"),
        2,
        vec!("(!15)", "(-5)");
        "happy path prefix"
    )]
    #[test_case(
        String::from("5 > 5;5 < 5;5 == 5;5 != 5"),
        4,
        vec!("(5 > 5)", "(5 < 5)", "(5 == 5)", "(5 != 5)");
        "happy path infix"
    )]
    #[test_case(
        String::from("foobar;"),
        1,
        vec!("foobar");
        "basic statement expr"
    )]
    #[test_case(
        String::from("a + b / c"),
        1,
        vec!("(a + (b / c))");
        "divide > add"
    )]
    #[test_case(
        String::from("3 + 4 * 5 == 3 * 1 + 4 * 5"),
        1,
        vec!("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))");
        "complicated equivalence check"
    )]
    #[test_case(
        String::from("true;false;"),
        2,
        vec!("true", "false");
        "basic booleans"
    )]
    #[test_case(
        String::from("3 > 5 == false; 3 < 5 == true;"),
        2,
        vec!("((3 > 5) == false)", "((3 < 5) == true)");
        "nested booleans"
    )]
    #[test_case(
        String::from("1 + (2 + 3) + 4;(5 + 5) * 2;"),
        2,
        vec!("((1 + (2 + 3)) + 4)", "((5 + 5) * 2)");
        "parenthesis precendence"
    )]
    #[test_case(
        String::from("if (x < y) { x }"),
        1,
        vec!("if (x < y) x");
        "basic if condition"
    )]
    #[test_case(
        String::from("if (x < y) { x } else { y }"),
        1,
        vec!("if (x < y) x else y");
        "basic else condition"
    )]
    #[test_case(
        String::from("if (a + b * c > 10) { x }"),
        1,
        vec!("if ((a + (b * c)) > 10) x");
        "precedence if"
    )]
    #[test_case(
        String::from("a + add(b * c) + d"),
        1,
        vec!("((a + add((b * c))) + d)");
        "precedence call"
    )]
    #[test_case(
        String::from("add(a + b + c * d / f + g)"),
        1,
        vec!("add((((a + b) + ((c * d) / f)) + g))");
        "nested precedence call"
    )]
    #[test_case(
        String::from("a * [1, 2, 3][b * c] * d"),
        1,
        vec!("((a * ([1,2,3][(b * c)])) * d)");
        "array precedence"
    )]
    fn parse_stmt(input: String, len: usize, exp_literals: Vec<&str>) {
        let (p, prgrm) = setup_prgm(input);

        if !p.errors.is_empty() {
            panic!("{} parser errors present: {:#?}", p.errors.len(), p.errors);
        }
        assert_eq!(prgrm.stmts.len(), len);

        let t_types: Vec<String> = prgrm.stmts.iter().map(|s| format!("{}", s)).collect();
        assert_eq!(t_types, exp_literals);
        println!("{:#?}", prgrm.stmts);
    }

    #[test_case(
        String::from("fn() {};"),
        vec!();
        "empty fn"
    )]
    #[test_case(
        String::from("fn(x) {};"),
        vec!("x");
        "single fn param"
    )]
    #[test_case(
        String::from("fn(x, y, z) {}"),
        vec!("x", "y", "z");
        "multi param"
    )]
    fn parse_fn_expr(input: String, exp_params: Vec<&str>) {
        let (p, prgrm) = setup_prgm(input);

        if !p.errors.is_empty() {
            panic!("{} parser errors present: {:#?}", p.errors.len(), p.errors);
        }

        assert_eq!(prgrm.stmts.len(), 1);
        let stmt = &prgrm.stmts[0];
        let params: Vec<String> = match stmt {
            StmtKind::Expr(e) => match e {
                ExprKind::Fn(p, ..) => p.iter().map(|i| i.name.to_owned()).collect(),
                _ => panic!("unexpected exprkind {:#?}", e),
            },
            _ => panic!("unexpected stmtkind {:#?}", stmt),
        };

        assert_eq!(params, exp_params);
    }

    #[test_case(
        "let x = 5;".to_string(),
        Identifier::new("x".to_string()),
        Some(ExprKind::Int(5));
        "base case"
    )]
    #[test_case(
        "let y = true;".to_string(),
        Identifier::new("y".to_string()),
        Some(ExprKind::Boolean(true));
        "base boolean case"
    )]
    fn parse_let_stmt(input: String, exp_id: Identifier, exp_val: Option<ExprKind>) {
        let (p, prgrm) = setup_prgm(input);

        if !p.errors.is_empty() {
            panic!("{} parser errors present: {:#?}", p.errors.len(), p.errors);
        }

        assert_eq!(prgrm.stmts.len(), 1);
        let stmt = &prgrm.stmts[0];
        match stmt {
            StmtKind::Let(id, m_expr) => {
                assert_eq!(*id, exp_id);
                assert_eq!(*m_expr, exp_val);
            }
            _ => panic!("unexpected stmtkind {:#?}", stmt),
        };
    }

    #[test_case(
        "return y".to_string(),
        Some(ExprKind::Ident(Identifier::new("y".to_string())));
        "basic ident val"
    )]
    #[test_case(
        "return 10".to_string(),
        Some(ExprKind::Int(10));
        "basic int val"
    )]
    fn parse_return_stmt(input: String, exp_val: Option<ExprKind>) {
        let (p, prgrm) = setup_prgm(input);

        if !p.errors.is_empty() {
            panic!("{} parser errors present: {:#?}", p.errors.len(), p.errors);
        }

        assert_eq!(prgrm.stmts.len(), 1);
        let stmt = &prgrm.stmts[0];
        match stmt {
            StmtKind::Return(v) => assert_eq!(*v, exp_val),
            _ => panic!("unexpected stmtkind {:#?}", stmt),
        };
    }

    #[test_case("[1, 2]", ExprKind::Array(
            Box::new(vec![ExprKind::Int(1), ExprKind::Int(2)])
        ); "int array")]
    fn parse_array_literal(input: &str, exp: ExprKind) {
        let (p, prgrm) = setup_prgm(input.to_string());

        if !p.errors.is_empty() {
            panic!("{} parser errors present: {:#?}", p.errors.len(), p.errors);
        }

        assert_eq!(prgrm.stmts.len(), 1);
        let stmt = &prgrm.stmts[0];
        match stmt {
            StmtKind::Expr(e) => assert_eq!(*e, exp),
            _ => panic!("unexpected stmtkind {:#?}", stmt),
        }
    }

    #[test_case(r#""hello world";"#, "hello world"; "hello str literal")]
    fn parse_str_literal(input: &str, exp: &str) {
        let (p, prgrm) = setup_prgm(input.to_string());

        if !p.errors.is_empty() {
            panic!("{} parser errors present: {:#?}", p.errors.len(), p.errors);
        }

        assert_eq!(prgrm.stmts.len(), 1);
        let stmt = &prgrm.stmts[0];
        match stmt {
            StmtKind::Expr(ExprKind::Str(s)) => assert_eq!(*s, exp.to_string()),
            _ => panic!("unexpected stmtkind {:#?}", stmt),
        }
    }
}
