use std::{fs, env::args, process::{exit, Command}, io, collections::HashMap};

type Error = (ErrorLevel, String);

#[allow(unused)]
#[derive(Debug, Clone)]
enum ErrorLevel {
    Warn,
    Err,
    Fatal
}

impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            ErrorLevel::Warn => "warning",
            ErrorLevel::Err => "error",
            ErrorLevel::Fatal => "FATAL Error",
        })
    }
}

#[derive(Debug, Clone)]
struct Lexer {
    filename : String,
    source   : Vec<char>,
    pos      : usize,
    line     : usize,
    ch       : usize
}

/// keyword list
const KEYWORDS: &[&str] = &[
    "if",
    "else",
    "func",
    "alias",
    "intrinsic"
];

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Primitive(PrimitiveType),
    Custom(String),
    Array(Box<Type>/*, u32*/),
    Pointer(Box<Type>),
    Invalid,
}

#[derive(Debug, Clone, PartialEq)]
enum PrimitiveType {
    Int,
    Float,
    Char,
    Void,
    Bool,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Type::Primitive(a) => match a {
                PrimitiveType::Int => "int",
                PrimitiveType::Float => "float",
                PrimitiveType::Char => "char",
                PrimitiveType::Void => "void",
                PrimitiveType::Bool => "bool",
            }.into(),
            Type::Custom(s) => s.clone(),
            Type::Array(a) => {
                format!("[{a}]")
            },
            Type::Pointer(p) => {
                format!("{p}*")
            },
            Type::Invalid => "__invalid__".into(),
        }.as_str())
    }
}

impl Type {
    fn dealias(&self, aliases: &HashMap<String, Type>) -> Type {
        let mut ret = self.clone();

        if let Type::Custom(val) = self {
            if aliases.contains_key(val) {
                ret = aliases.get(val).unwrap().clone();
            }
        }
        ret
    }
    
    fn is_compatible(&self, type_r: &Type, aliases: &HashMap<String, Type>) -> bool {
        let a = self.dealias(aliases);
        let b = type_r.dealias(aliases);
        
        if a == b {
            true
        } else {
            if a == Type::Primitive(PrimitiveType::Float) && b == Type::Primitive(PrimitiveType::Int) {
                true
            } else if let Type::Array(aa) = a {
                if let Type::Array(bb) = b {
                    aa.is_compatible(&bb, aliases)
                } else {
                    false
                }
            } else {
                false
            }
        }
    }

    fn size(&self) -> usize {
        match self {
            Type::Primitive(a) => match a {
                PrimitiveType::Int => 8,
                PrimitiveType::Float => 8,
                PrimitiveType::Char => 1,
                PrimitiveType::Void => 0,
                PrimitiveType::Bool => 1,
            },
            Type::Custom(s) => unimplemented!("{s} shouldn't be a type"),
            Type::Array(_) => 8,             // 4 <- size, 4 <- pointer
            Type::Pointer(_) => 4,
            Type::Invalid => 0,
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
        })
    }
}

impl Op {
    fn combine_type(&self, a: &Type, b: &Type, aliases: &HashMap<String, Type>) -> Type {
        match self {
            Op::Add | Op::Sub | Op::Mul | Op::Div => {
                match a {
                    Type::Primitive(ref v) => {
                        match v {
                            PrimitiveType::Int => if a.is_compatible(&b, aliases) {
                                a.clone()
                            } else {
                                Type::Invalid
                            },
                            PrimitiveType::Float => if a.is_compatible(&b, aliases) {
                                a.clone()
                            } else {
                                Type::Invalid
                            },
                            PrimitiveType::Char => Type::Invalid,
                            PrimitiveType::Void => Type::Invalid,
                            PrimitiveType::Bool => Type::Invalid,
                        }
                    },
                    Type::Custom(_) => Type::Invalid,
                    Type::Array(_) => Type::Invalid,
                    Type::Pointer(_) => Type::Invalid,
                    Type::Invalid => Type::Invalid,
                }
            },
        }
    }
}

fn is_valid_type(val: &str) -> bool {
    if val == "int" || val == "float" || val == "char" || val == "void" {
        true
    } else {
        // check for custom types
        false
    }
}

fn parse_type(string: String) -> Option<Type> {
    if string.len() <= 0 || !string.chars().next().unwrap().is_alphabetic() {
        return None;
    }
    match string.as_str() {
        "int"    => Some(Type::Primitive(PrimitiveType::Int)),
        "float"  => Some(Type::Primitive(PrimitiveType::Float)),
        "char"   => Some(Type::Primitive(PrimitiveType::Char)),
        "void"   => Some(Type::Primitive(PrimitiveType::Void)),
        _ => {
            if string.ends_with("*") {
                Some(Type::Pointer(Box::new(parse_type(string.as_str()[0..string.len()-1].into())?)))
            } else {
                Some(Type::Custom(string))
            }
        }
    }
}

/// return an error string in the following format:
///     ERR: This is a error at ~/file.name:430
/// argument 2 (msg_fmt) is a format string
macro_rules! error {
    ($lexer:expr, $pos:expr, $msg_fmt:literal) => {
        | | -> String {
            let (l, c) = $lexer.pos_to_line_char($pos);
            return format!("{msg}\n\t at {file}:{line}:{ch}",
                    msg = format!($msg_fmt),
                    file = $lexer.filename,
                    line = l+1,
                    ch = c+1
            )
        }()
    }
}

impl Lexer {
    fn new(filename: String) -> Result<Self, io::Error> {
        Ok(Lexer {
            source: fs::read_to_string(filename.clone())?.chars().collect(),
            pos: 0,
            filename,
            line: 0,
            ch: 0,
        })
    }

    fn pos_to_line_char(&mut self, pos: usize) -> (usize, usize) {
        assert!(pos < self.source.len());
        let mut p: usize = 0;
        let mut ch: usize = 0;
        let mut line: usize = 0;

        while p < pos {
            ch += 1;
            if self.source[p] == '\n' {
                ch = 0;
                line += 1;
            }
            p += 1;
        }

        (line, ch)
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        if self.pos+1 >= self.source.len() {
            return Ok(Token {
                pos: self.pos,
                ttype: TokenType::Eof,
                value: "".into(),
            });
        }        
        
        let mut token_pos = self.pos;
        let mut val: String = "".to_string();
        let mut ttype = TokenType::Undef;

        fn check_keywords_and_types(val: &str, tp: TokenType) -> TokenType {
            let mut ttype = tp;
            if KEYWORDS.contains(&val) {
                ttype = TokenType::Keyword;
            }

            if is_valid_type(&val) {
                ttype = TokenType::Type;
            }

            if val == "true" || val == "false" {
                ttype = TokenType::Bool;
            }
            ttype
        }

        loop {
            let ch = self.source[self.pos];
            if match ttype {
                TokenType::Char => {
                    if val.len() == 1 && ch == '\'' {
                        self.pos += 1;
                        return Ok(Token {
                            pos: token_pos,
                            ttype,
                            value: val,
                        })
                    }
                    if !val.is_empty() {
                        return Err((ErrorLevel::Err, error!(self, self.pos, "invalid character literal")));
                    }
                    val.push(ch);
                    false
                },
                TokenType::String => {
                    if ch == '"' {
                        self.pos += 1;
                        return Ok(Token {
                            pos: token_pos,
                            ttype,
                            value: val,
                        })
                    }
                    val.push(ch);
                    false
                },
                _ => true
            } {
                match ch {
                    '\'' => {
                        if ttype == TokenType::Undef {
                            ttype = TokenType::Char;
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    '"' => {
                        if ttype == TokenType::Undef {
                            ttype = TokenType::String;
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    'a'..='z'|'_'|'A'..='Z' => {
                        if ttype == TokenType::Undef || ttype == TokenType::Ident {
                            ttype = TokenType::Ident;
                            val.push(ch);
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    '0'..='9'|'.' => {
                        if ttype == TokenType::Float || ( (ttype == TokenType::Undef || ttype == TokenType::Int) && ch == '.') {
                            ttype = TokenType::Float;
                            val.push(ch);
                        } else if ttype == TokenType::Undef || ttype == TokenType::Int {
                            ttype = TokenType::Int;
                            val.push(ch);
                        } else if ttype == TokenType::Ident {
                            ttype = TokenType::Ident;
                            val.push(ch);
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    '='|'+'|'-'|'*'|'/' => {
                        if ttype == TokenType::Type && ch == '*' {
                            val.push(ch);
                            ttype = TokenType::Type;
                        } else if ttype == TokenType::Undef || ttype == TokenType::Operator {
                            ttype = TokenType::Operator;
                            val.push(ch);
                        } else if ttype == TokenType::Special && val.starts_with("<") && ch == '-' {
                            val.push(ch);
                            ttype = TokenType::Special;
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    ';' | '(' | ')' | ',' | '{' | '}'|'<'|'>'|'['|']' => {
                        if ttype == TokenType::Undef {
                            ttype = TokenType::Special;
                            val.push(ch);
                        } else if ttype == TokenType::Operator && val == "-" && ch == '>' {
                            val.push(ch);
                            ttype = TokenType::Special;
                        } else if ttype == TokenType::Special && !val.contains(")") && !val.contains("(") && !val.contains("[") && !val.contains("]") && !val.contains("}") && !val.contains("{") && !(ch == ';') {
                            val.push(ch);
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    ' '|'\n' => {
                        if ttype != TokenType::Undef {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(), ttype);
                            
                            return Ok(Token{
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        } else {
                            token_pos = self.pos+1;
                        }
                    }
                    _ => return Err((ErrorLevel::Err, error!(self, self.pos, "invalid character `{ch}`")))
                };
            }

            self.pos += 1;
            if self.pos >= self.source.len() {
                return Ok(Token {
                    pos: token_pos,
                    ttype,
                    value: val,
                })
            }
            
            (self.line, self.ch) = self.pos_to_line_char(self.pos);
        }
    }

    fn peek_token(&mut self) -> Result<Token, Error> {
        let last_pos = self.pos;
        let token = self.next_token();
        self.pos = last_pos;
        token
    }
}

#[derive(Debug, Clone)]
struct Token {
    pos: usize,
    ttype: TokenType,
    value: String,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum TokenType {
    Int,
    Float,
    Char,
    String,
    Ident,
    Type,
    Eof,
    Special,
    Operator,
    Undef,
    Keyword,
    Bool,
}

#[derive(Debug, Clone)]
struct Parser {
    lexer: Lexer,
}

type ASTNode = (usize, ASTNodeR);

#[derive(Debug, Clone, PartialEq)]
enum ASTNodeR {
    Block(Vec<ASTNode>),
    VarDec(Type, String),
    VarInit(String, Expression),
    FunctionCall(String, Vec<Expression>),
    If(Expression, Box<ASTNode>),
    FunctionDecl(String, Vec<(Type, String)>, Type, Box<ASTNode>),
    Return(Expression),
    TypeAlias(String, Type),
}

type Expression = (usize, ExpressionR);

#[derive(Debug, Clone, PartialEq)]
enum ExpressionR {
    T(Box<Expression>, Op, Box<Expression>, u32),
    Val(Type, String),
    Var(String),
    F(String, Vec<Expression>),
    Arr(Vec<Expression>),
    Undef,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenType::Int => "integer literal",
            TokenType::Bool => "boolean literal",
            TokenType::Float => "floating-point literal",
            TokenType::Char => "character literal",
            TokenType::String => "string literal",
            TokenType::Ident => "identifier",
            TokenType::Type => "type",
            TokenType::Eof => "end of file",
            TokenType::Special => "special character",
            TokenType::Operator => "operator",
            TokenType::Undef => "undeinfed",
            TokenType::Keyword => "keyword",
        })
    }
}

impl Op {
    fn from_char(c: char) -> Result<Self, Error> {
        match c {
            '+' => Ok(Self::Add),
            '-' => Ok(Self::Sub),
            '*' => Ok(Self::Mul),
            '/' => Ok(Self::Div),
            _ => Err((ErrorLevel::Fatal, format!("FATAL: invalid operator `{c}` set from lexer"))),
        }
    }
    
    fn get_precedence(op: Op) -> u32 {
        match op {
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div => 2,
        }
    }
}

macro_rules! get_token {
    ($lexer:expr, $errors:ident) => {
        | | -> Token {
            while let Err(e) = $lexer.peek_token() {
                $errors.push(e);
                $lexer.next_token().unwrap();
            }
            return $lexer.next_token().unwrap()
        }()
    }
}

macro_rules! get_peek_token {
    ($lexer:expr, $errors:ident) => {
        | | -> Token {
            while let Err(e) = $lexer.peek_token() {
                $errors.push(e);
                $lexer.next_token().unwrap();
            }
            return $lexer.peek_token().unwrap()
        }()
    }
}

macro_rules! err_ret {
    ($a:expr, $errors:ident) => {
        match $a {
            Ok(a) => a,
            Err(e) => {
                $errors.push(e);
                return Err($errors);
            }
        }
    }
}

macro_rules! err_break {
    ($a:expr, $errors:ident) => {
        match $a {
            Ok(a) => a,
            Err(e) => {
                $errors.push(e);
                break;
            }
        }
    }
}

#[allow(unused)]
macro_rules! err_add {
    ($a:expr, $errors:ident) => {
        match $a {
            Ok(a) => a,
            Err(e) => {
                for err in e {
                    $errors.push(err);
                }
            }
        }
    }
}

impl Parser {
    fn new(filename: String) -> Result<Self, io::Error> {
        Ok(Parser {
            lexer: Lexer::new(filename)?
        })
    }

    fn parse(&mut self) -> Result<ASTNode, Vec<Error>> {
        self.parse_block_(true, 0)
    }

    fn parse_block(&mut self, pos: usize) -> Result<ASTNode, Vec<Error>> {
        self.parse_block_(false, pos)
    }
    
    fn parse_block_(&mut self, is_root: bool, pos: usize) -> Result<ASTNode, Vec<Error>> {
        let mut block: ASTNode = (pos, ASTNodeR::Block(vec![]));
        let mut errors: Vec<Error> = vec![];
        loop {
            match block {
                (_, ASTNodeR::Block(ref mut vec)) => {
                    let bs = match self.parse_block_statement(is_root) {
                        Ok(a) => a,
                        Err(e) => {
                            for a in e {
                                errors.push(a);
                            }
                            // advance to semicolon / block
                            loop {
                                let tk = self.lexer.peek_token();
                                match tk {
                                    Ok(a) => {
                                        if a.ttype == TokenType::Eof || (a.ttype == TokenType::Special && (a.value == ";" || a.value == "{")) {
                                            break;
                                        } else if a.ttype == TokenType::Special && a.value == "}" {
                                            return Err(errors);
                                        } else {
                                            self.lexer.next_token().unwrap();
                                        }
                                    },
                                    Err(e) => {
                                        errors.push(e);
                                        break;
                                    },
                                }
                            }
                            continue;
                        },
                    };
                    if bs.is_none() {
                        break;
                    }
                    vec.push(bs.unwrap());
                },
                _ => todo!(),
            }
            let token = get_peek_token!(self.lexer, errors);
            if token.ttype == TokenType::Eof {
                break;
            }
        }
        if errors.is_empty() {
            Ok(block)
        } else {
            Err(errors)
        }
    }

    fn parse_expr(&mut self, seperators: &[&'static str]) -> Result<(Expression, Token), Vec<Error>> {
        let mut errors: Vec<Error> = vec![];
        let check_precedence_and_update = |op: Op, right_expr: Box<Expression>, working_expr: &Expression| -> Box<Expression> {
            let ret: Box<Expression>;
            let is_first: bool = match working_expr {
                (_, exp) => match exp {
                    ExpressionR::T(_, _, _, precedence) => {
                        Op::get_precedence(op) <= *precedence
                    },
                    ExpressionR::Val(..) | ExpressionR::Arr(..) | ExpressionR::F(..) | ExpressionR::Var(..) => {
                        true
                    },
                    ExpressionR::Undef => unreachable!(),
                }
            };

            if is_first {
                let tmp_expr = (working_expr.0, ExpressionR::T(Box::new(working_expr.clone()), op, right_expr
                                             , Op::get_precedence(op)));
                ret = Box::new(tmp_expr);
            } else {
                assert!(matches!(*working_expr, (_, ExpressionR::T(..))));
                let mut expr = working_expr.clone();
                let tmp_expr = if let (pos, ExpressionR::T(_, _, right, _)) = &expr {
                    (*pos, ExpressionR::T(right.clone(), op, right_expr, Op::get_precedence(op)))
                } else {
                    unreachable!();
                };

                if let (pos, ExpressionR::T(a, b, _, d)) = expr {
                    expr = (pos, ExpressionR::T(a, b, Box::new(tmp_expr), d));
                }
                    
                ret = Box::new(expr);
            }

            ret
        };

        let mut left_expr = Box::new((0, ExpressionR::Undef));

        // parse left side
        let subexpr = self.parse_subexpr(seperators)?;
        let mut nleft_expr = subexpr.0;

        loop {
            if let (_, ExpressionR::Undef) = *nleft_expr {
                return Ok((*left_expr, subexpr.1));
            }
            left_expr = nleft_expr.clone();
            
            // parse operation
            let tk_op = get_peek_token!(self.lexer, errors);
            let op = if tk_op.ttype == TokenType::Operator && tk_op.value.len() == 1 {
                match Op::from_char(tk_op.value.chars().next().unwrap()) {
                    Ok(a) => a,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    },
                }
            } else if tk_op.ttype == TokenType::Eof || (tk_op.ttype == TokenType::Special && seperators.contains(&tk_op.value.as_str())) {
                self.lexer.next_token().unwrap();
                return Ok((*left_expr, tk_op));
            } else {
                let val = tk_op.value;
                errors.push((ErrorLevel::Err, error!(self.lexer, tk_op.pos, "invalid token `{val}`")));
                return Err(errors);
            };

            self.lexer.next_token().unwrap();

            // parse right side
            let subexpr_r = self.parse_subexpr(seperators)?;

            if let (_, ExpressionR::Undef) = *subexpr_r.0 {
                return Ok((*left_expr, subexpr.1));
            }
            
            let right_expr = subexpr_r.0;
            nleft_expr = check_precedence_and_update(op, right_expr.clone(), &mut left_expr);
        }
    }

    fn parse_block_statement(&mut self, is_root: bool) -> Result<Option<ASTNode>, Vec<Error>> {
        let mut errors: Vec<Error> = vec![];        
        let mut token = get_token!(self.lexer, errors);
        let val = token.value;

        fn parse_var_dec(parser: &mut Parser, typ: Type) -> Result<Option<ASTNode>, Error> {
            // declare variable
            let pos = parser.lexer.pos;
            
            let next_token = parser.expect(Some(TokenType::Ident), None)?;
            let ident = next_token.value;
            let ident_pos = next_token.pos;
            
            // check next token
            let next_token = parser.lexer.next_token()?;
            let ntv = next_token.value.clone();
            match next_token.ttype {
                TokenType::Special => {
                        if next_token.value == ";" {
                            // token ended, normal return
                        } else {
                            return Err((ErrorLevel::Err, error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`")));
                        }
                },
                TokenType::Operator => {
                    if next_token.value == "=" {
                        // variable initialization, reset position
                        parser.lexer.pos = ident_pos;
                    } else {
                        return Err((ErrorLevel::Err, error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`")));
                    }
                },
                _ => return Err((ErrorLevel::Err, error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`"))),
            }
            
            return Ok(Some((pos, ASTNodeR::VarDec(typ, ident))));
        }


        // may be a later defined type
        if token.ttype == TokenType::Ident && self.lexer.peek_token().unwrap().ttype == TokenType::Ident {
            token.ttype = TokenType::Type;
        }
        
        match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Char | TokenType::String | TokenType::Bool => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "invalid literal")));
                return self.parse_block_statement(is_root);
            },
            TokenType::Ident => {
                let expr = self.parse_ident_expr(val.clone(), token.pos,  &[";"])?;
                match *expr {
                    (_, exp) => match exp {
                        ExpressionR::Var(..) => {
                            let var_token = get_token!(self.lexer, errors);
                            let var_val = var_token.value;
                            
                            if var_token.ttype == TokenType::Operator && var_val == "=" {
                                let expr = self.parse_expr(&[";"])?.0;
                                return Ok(Some((token.pos, ASTNodeR::VarInit(val, expr))));
                            } else {
                                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{var_val}`")));
                            }
                        }
                        ExpressionR::Val(..) => unreachable!(),
                        ExpressionR::T(..) => unreachable!(),
                        ExpressionR::Arr(..) => unreachable!(),
                        ExpressionR::F(name, args) => {
                            // function call: only permitted inside of function
                            if is_root {
                                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "function calls are not allowed at top level")));
                            }

                            // expect semicolon
                            err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors);

                            return Ok(Some((token.pos, ASTNodeR::FunctionCall(name, args))));
                        },
                        ExpressionR::Undef => unreachable!(),
                    }
                }
            },
            TokenType::Eof => return Ok(None),
            TokenType::Special => match val.as_str() {
                "}" => {
                    if is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexprected  token '}}'")));
                    }
                    return if errors.is_empty() {
                        Ok(None)
                    } else {
                        Err(errors)
                    };
                },
                "{" => {
                    match self.parse_block(token.pos) {
                        Ok(a) => return Ok(Some(a)),
                        Err(e) => {
                            for a in e {
                                errors.push(a);
                            }
                            return Err(errors);
                        },
                    }
                },
                "<-" => {
                    if is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "returns are not allowed at top level")));
                    }
                    // parse expression
                    let expression = self.parse_expr(&[";"])?.0;
                    return Ok(Some((token.pos, ASTNodeR::Return(expression))));
                },
                "[" => {
                    // array declaration (e. g. [int, 2])

                    let array_type = match self.parse_array() {
                        Ok(a) => a,
                        Err(e) => {
                            errors.push(e);
                            return Err(errors);
                        },
                    };

                    return match parse_var_dec(self, array_type) {
                        Ok(a) => {
                            Ok(a)
                        },
                        Err(e) => {
                            errors.push(e);
                            Err(errors)
                        }
                    }
                }
                ";" => {
                    // ignore
                    return self.parse_block_statement(is_root);
                }
                _ => {},
            },
            TokenType::Operator => {},
            TokenType::Type => {
                let typ = match parse_type(val.clone()) {
                    Some(a) => a,
                    None => {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "invalid type `{val}`")));
                        return Err(errors);
                    },
                };
                return match parse_var_dec(self, typ) {
                    Ok(a) => Ok(a),
                    Err(e) => {
                        errors.push(e);
                        Err(errors)
                    },
                };
            },
            TokenType::Keyword => match val.as_str() {
                "if" => {
                    if is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "`if` not allowed at top level")));
                        return Err(errors);
                    }
                    // next token: '('
                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors);

                    let predicate = self.parse_expr(&[")"])?.0;

                    // next token: '{'
                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors);
                    
                    let block = self.parse_block(token.pos)?;

                    return Ok(Some((token.pos, ASTNodeR::If(predicate, Box::new(block)))));
                    
                },
                "func" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "functions are only allowed at top level")));
                        return Err(errors);
                    }
                    
                    // parse function name
                    let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                    

                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors).value;
                    
                    // parse args
                    let mut args: Vec<(Type, String)> = vec![];
                    
                    let check = get_peek_token!(self.lexer, errors);
                    if check.ttype != TokenType::Special || check.value != ")" {
                        loop {
                            let t = match self.parse_type() {
                                Ok(a) => a,
                                Err(e) => {
                                    errors.push(e);
                                    return Err(errors);
                                },
                            };

                            let nval = err_break!(self.expect(Some(TokenType::Ident), None), errors).value;
                            args.insert(args.len(), (t, nval));

                            let token = get_token!(self.lexer, errors);
                            let val = token.value;
                            if token.ttype != TokenType::Special {
                                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, `,` or `)` expect")));
                                return Err(errors);
                            }
                            
                            if val == "," {
                                continue;
                            } else if val == ")" {
                                break;
                            } else {
                                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, `,` or `)` expect")));
                                return Err(errors);
                            }
                        }
                    } else {
                        get_token!(self.lexer, errors);
                    }

                    // optional return type (-> type)
                    // else {
                    let mut ret_type = Type::Primitive(PrimitiveType::Void);
                    
                    let next_token = get_peek_token!(self.lexer, errors);
                    if next_token.ttype == TokenType::Special && next_token.value == "->" {
                        self.lexer.next_token().unwrap();
                        // expect return type

                        let tp = match self.parse_type() {
                            Ok(a) => a,
                            Err(e) => {
                                errors.push(e);
                                return Err(errors);
                            },
                        };
                        ret_type = tp;//parse_type(val);
                    }
                    
                    // expect {
                    err_ret!(self.expect(Some(TokenType::Special), None), errors);

                    // parse block
                    let block = self.parse_block(token.pos)?;

                    return Ok(Some((token.pos, ASTNodeR::FunctionDecl(name, args, ret_type, Box::new(block)))));

//                    let func_expr: Expression = Expression::F(ident, args);
                },
                "alias" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "type aliases only allowed at top-level")));
                        return Err(errors);
                    }
                    
                    // expect identifier
                    let ident = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;

                    // expect `=`
                    let next_token = get_token!(self.lexer, errors);
                    if next_token.ttype != TokenType::Operator || next_token.value != "=" {
                        let ntv = next_token.value;
                        errors.push((ErrorLevel::Err, error!(self.lexer, next_token.pos, "unexpected token `{ntv}`, identifier expect")));
                        return Err(errors);
                    }

                    let typ = match self.parse_type() {
                        Ok(a) => a,
                        Err(e) => {
                            errors.push(e);
                            return Err(errors);
                        },
                    };

                    return Ok(Some((token.pos, ASTNodeR::TypeAlias(ident, typ))));
                    
                },
                _ => {}
            },
            TokenType::Undef => return self.parse_block_statement(is_root),
        }
        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
        Err(errors)
    }

    fn parse_ident_expr(&mut self, ident: String, ident_pos: usize, seperators: &[&'static str]) -> Result<Box<Expression>, Vec<Error>> {
        let mut errors: Vec<Error> = vec![];

        let token = get_peek_token!(self.lexer, errors);
        let val = token.value;

        match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Ident | TokenType::Type | TokenType::Keyword | TokenType::String | TokenType::Char | TokenType::Bool => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected identifier `{ident}`, type expected")));
                Err(errors)
            }
            TokenType::Eof => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected end of file")));
                Err(errors)
            },
            TokenType::Special => match val.as_str() {
                "(" => {
                    get_token!(self.lexer, errors);
                    // parse args
                    let mut args: Vec<Expression> = vec![];
                    loop {
                        let ret = self.parse_expr(&[")", ","])?;
                        let token: Token = ret.1;
                        if let (_, ExpressionR::Undef) = ret.0 {
                            break;
                        }
                        args.insert(args.len(), ret.0);
                        if token.value == ")" {
                            break;
                        }
                    }
                    let func_expr: Expression = (ident_pos, ExpressionR::F(ident, args));
                    Ok(Box::new(func_expr))
                },
                a => if seperators.contains(&a) {
                    Ok(Box::new((ident_pos, ExpressionR::Var(ident))))
                } else {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                    Err(errors)
                },
            },
            TokenType::Operator => {
                Ok(Box::new((token.pos, ExpressionR::Var(ident))))
            },
            TokenType::Undef => unreachable!(),
        }
    }

    fn parse_subexpr(&mut self, seperators: &[&'static str]) -> Result<(Box<Expression>, Token), Vec<Error>> {
        let inverse = |right_expr: &mut Box<Expression>| {
            let tmp_expr = right_expr.clone();
            **right_expr = (tmp_expr.0, ExpressionR::T(Box::new((0, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), "-1".into()))), Op::Mul, tmp_expr, Op::get_precedence(Op::Mul)));
        };

        let mut errors: Vec<Error> = vec![];
        
        let token = get_token!(self.lexer, errors);
        let val = token.clone().value;
        match token.ttype {
            TokenType::Type | TokenType::Eof | TokenType::Undef | TokenType::Keyword => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                Err(errors)
            },
            TokenType::Int => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), val))), token))
            },
            TokenType::Bool => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Bool), val))), token))
            },
            TokenType::Float => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Float), val))), token))
            },
            TokenType::Char => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Char), val))), token))
            },
            TokenType::String => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Array(Box::new(Type::Primitive(PrimitiveType::Char))), val))), token))
            },
          TokenType::Ident => Ok((self.parse_ident_expr(val, token.pos, seperators)?, token)),
            TokenType::Special => match val.as_str() {
                "(" => {
                    Ok((Box::new(self.parse_expr(&[")"])?.0), token))
                },
                "{" => {
                    // expect array definition
                    let mut arr = vec![];
                    loop {
                        let next_expr = self.parse_expr(&[",","}"])?;
                        arr.push(next_expr.0);
                        if next_expr.1.value == "}" {
                            break;
                        }
                    };
                    Ok((Box::new((token.pos, ExpressionR::Arr(arr))), Token {
                        pos: 0,
                        ttype: TokenType::Undef,
                        value: "".into(),
                    }))
                },
                _ => if seperators.contains(&val.as_str()) {
                    Ok((Box::new((0, ExpressionR::Undef)), token))
                } else {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected symbol `{val}`")));
                    Err(errors)
                }
            },
            TokenType::Operator => {
                if val == "-" {
                    // negate
                    let mut a = self.parse_subexpr(seperators)?;
                    inverse(&mut a.0);
                    Ok(a)
                } else if val == "+" {
                    // ignore
                    self.parse_subexpr(seperators)
                } else {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected operator `{val}`")));
                    Err(errors)
                }
            },
        }
    }

    fn parse_array(&mut self) -> Result<Type, Error> {
        // expect type
        let typ = self.parse_type()?;
        let array_type  = Type::Array(Box::new(typ));

        // expect ]
        let next_token = self.lexer.next_token()?;
        if next_token.ttype != TokenType::Special || next_token.value != "]" {
            let val = next_token.value;
            return Err((ErrorLevel::Err, error!(self.lexer, next_token.pos, "invalid token `{val}`, `]` expect")));
        }

        Ok(array_type)
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let nt = self.lexer.next_token()?;
        let val = nt.value;
        if nt.ttype == TokenType::Type {
            match parse_type(val.clone()) {
                Some(a) => Ok(a),
                None => Err((ErrorLevel::Err, error!(self.lexer, nt.pos, "invalid type `{val}`"))),
            }
        } else if nt.ttype == TokenType::Special && val == "[" {
            self.parse_array()
        } else if nt.ttype == TokenType::Ident {
            Ok(Type::Custom(val))
        } else {
            Err((ErrorLevel::Err, error!(self.lexer, nt.pos, "invalid type `{val}`")))
        }
    }

    fn expect(&mut self, typ: Option<TokenType>, val: Option<String>) -> Result<Token, Error> {
        let token = self.lexer.next_token()?;

        if val.is_some() {
            let mut is_valid: bool = val.as_ref().unwrap() == &token.value;
            if typ.is_some() {
                is_valid &= token.ttype == typ.unwrap();
            }

            if ! is_valid {
                let exp_val = val.unwrap();
                let val = token.value;
                Err((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, `{exp_val}` expected")))
          } else {
                Ok(token)
            }
        } else if typ.is_some() {
            let is_valid: bool = token.ttype == typ.unwrap();
            if ! is_valid {
                let val = token.value;
                let ttype = typ.unwrap().to_string();
                Err((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, {ttype} expected")))
          } else {
                Ok(token)
            }
        } else {
            Ok(token)
        }
    }
}

fn check(ast: ASTNode, mut lexer: Lexer) -> Result<(), Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    
    // collect all aliases + functions
    let mut type_aliases: HashMap<String, Type> = HashMap::new();
    let mut functions: HashMap<String, (Vec<Type>, Type)> = HashMap::new();
    let mut vars: HashMap<String, Type> = HashMap::new();
    
    if let (_, ASTNodeR::Block(arr)) = ast.clone() {
        for a in arr {
            match a {
                (_, ASTNodeR::TypeAlias(alias, typ)) => {
                    type_aliases.insert(alias, typ);
                },
                (_, ASTNodeR::FunctionDecl(name, args, ret_type, _)) => {
                    functions.insert(name, (args.into_iter().map(|(a, _)| a).collect(), ret_type));
                },
                (_, ASTNodeR::VarDec(tp, name)) => {
                    vars.insert(name, tp);
                }
                _ => {}
            }
        }
    } else {
        unreachable!();
    }

    fn check_references_expr(expr: &Expression, functions: &HashMap<String, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
        match expr {
            (pos, e) => match e {
                ExpressionR::T(left, op, right, _) => {
                    let left = check_references_expr(left, functions, vars, aliases, errors, lexer);
                    let right = check_references_expr(right, functions, vars, aliases, errors, lexer);

                    return match op.combine_type(&left, &right, aliases) {
                        Type::Invalid => {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{left}` and `{right}` for operation `{op}`")));
                            Type::Invalid
                        },
                        a => a,
                    };
                },
                ExpressionR::Val(tp, _) => {
                    return tp.clone();
                },
                ExpressionR::Var(name) => {
                    if ! vars.contains_key(name) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{name}`")));
                        return Type::Invalid;
                    }
                    return vars.get(name).unwrap().clone();
                },
                ExpressionR::F(name, vec) => {
                    if ! functions.contains_key(name) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to function `{name}`")));
                        return Type::Invalid;
                    }
                    for a in vec {
                        check_references_expr(a, functions, vars, aliases, errors, lexer);
                    }
                    return functions.get(name).unwrap().1.clone();
                },
                ExpressionR::Arr(vec) => {
                    let mut last_tp = Type::Invalid;
                    for a in vec {
                        let tp = check_references_expr(a, functions, vars, aliases, errors, lexer);
                        if last_tp == Type::Invalid {
                            last_tp = tp.clone();
                        }
                        if ! last_tp.is_compatible(&tp, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{last_tp}` and `{tp}` in array literal")));
                            last_tp = Type::Invalid;
                            break;
                        }
                        last_tp = tp;
                    }
                    return Type::Array(Box::new(last_tp));
                },
                ExpressionR::Undef => {},
            }
        }

        Type::Invalid
    }
    
    // check
    fn check_references(node: &ASTNode, functions: &HashMap<String, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, f: String, lexer: &mut Lexer) {
        if let (_, ASTNodeR::Block(arr)) = node {
            let vars_sub = &mut vars.clone();
            for a in arr {
                match a {
                    (_, ASTNodeR::FunctionDecl(func, args, _, block)) => {
                        for a in args.clone() {
                            vars_sub.insert(a.1, a.0);
                        }
                        check_references(block, functions, vars_sub, aliases, errors, func.clone(), lexer);
                    },
                    (pos, ASTNodeR::FunctionCall(name, args)) => {
                        if ! functions.contains_key(name) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to function `{name}`")));
                        }
                        let func_args = &functions.get(name).unwrap().0;
                        let mut index = 0;

                        let len_a = func_args.len();
                        let len_b = args.len();
                        if len_a != len_b {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        };
                        
                        for a in args {
                            let typa = check_references_expr(a, functions, vars_sub, aliases, errors, lexer);
                            let typb = func_args.get(index).unwrap();

                            if ! typa.is_compatible(&typb, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                                break;
                            }
                            
                            index += 1;
                        }
                    },
                    (_, ASTNodeR::Block(..)) => {
                        check_references(a, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    (pos, ASTNodeR::If(expr, block)) => {
                        let bool_type = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if bool_type != Type::Invalid {
                            if ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                            }
                        }
                        check_references(block, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    (pos, ASTNodeR::Return(expr)) => {
                        let tp = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        let ret_type  = &functions.get(&f).unwrap().1;
                        if ! ret_type.is_compatible(&tp, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{ret_type}`, found `{tp}`, because of return type")));
                        }
                    },
                    (_, ASTNodeR::VarInit(var, expr)) => {
                        if ! vars_sub.contains_key(var) {
                            errors.push((ErrorLevel::Err, error!(lexer, a.0, "undefined reference to variable `{var}`")));
                        } else {
                            let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                            if type_r == &Type::Invalid {
                                continue;
                            }
                            let type_l = vars_sub.get(var).unwrap();
                            if ! type_l.is_compatible(type_r, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, a.0, "incompatible types: expeected `{type_l}`, found `{type_r}`")));
                            }
                            
                        }
//                        if 
                    },
                    (_, ASTNodeR::VarDec(tp, name)) => {
                        vars_sub.insert(name.clone(), tp.clone());
                    },
                    (_, ASTNodeR::TypeAlias(_, _)) => {}
                }
            };
        }
    }

    check_references(&ast, &functions, &vars, &type_aliases, &mut errors, "".into(), &mut lexer);

    // type checking
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Inst {
    Func(String),
    VarSet(usize, usize),
    If(usize),
    Endif,
    Ret(usize),
    Call(String),
    Op((usize, usize), Op),
    Val(usize, Type, String),
    Var(usize, usize),
    RetVal(usize, String)
}

fn intermediate_expr(expr: ExpressionR, index: usize, indicies: &mut HashMap<String, usize>) -> Vec<Inst> {
    let mut ret = vec![];
    match expr {
        ExpressionR::T(fst, op, snd, _) => {
            ret.append(&mut intermediate_expr(fst.1, index+0, indicies));
            ret.append(&mut intermediate_expr(snd.1, index+1, indicies));
            ret.push(Inst::Op((index+0, index+1), op));
        },
        ExpressionR::Val(tp, val) => {
            ret.push(Inst::Val(index, tp, val));
        },
        ExpressionR::Var(name) => {
            ret.push(Inst::Var(index, *indicies.get(&name).unwrap()))
        },
        ExpressionR::F(name, args) => {
            let mut index = 4;
            for a in args {
                ret.append(&mut intermediate_expr(a.1, index, indicies));
                index += 1;
            }
            ret.push(Inst::Call(name.clone()));
            ret.push(Inst::RetVal(index, name));
        },
        ExpressionR::Arr(_) => {
            todo!();
        },
        ExpressionR::Undef => todo!(),
    }
    ret
}


fn last_ptr(offsets: &HashMap<String, usize>) -> usize {
    let mut last = 0;
    for el in offsets {
        last = last.max(*el.1);
    }
    last
}

fn intermediate(ast: ASTNodeR, offsets: &mut HashMap<String, usize>) -> Vec<Inst> {
    let mut ret = vec![];
    
    match ast {
        ASTNodeR::Block(a) => {
            for i in a {
                ret.append(&mut intermediate(i.1, offsets));
            }
        },
        ASTNodeR::VarDec(tp, ref name) => {
            offsets.insert(name.clone(), last_ptr(offsets) + tp.size());
        },
        ASTNodeR::VarInit(ref name, expr) => {
            ret.append(&mut intermediate_expr(expr.1, 0, offsets));
            ret.push(Inst::VarSet(0, *offsets.get(name).unwrap()))
        },
        ASTNodeR::FunctionCall(name, args) => {
            let mut index = 4;
            for a in args {
                ret.append(&mut intermediate_expr(a.1, index, offsets));
                index += 1;
            }
            ret.push(Inst::Call(name));
        },
        ASTNodeR::If(expr, block) => {
            ret.append(&mut intermediate_expr(expr.1, 0, offsets));
            ret.push(Inst::If(0));
            ret.append(&mut intermediate(block.1, offsets));
            ret.push(Inst::Endif);
        },
        ASTNodeR::FunctionDecl(name, a, _, block) => {
            let mut offsets: HashMap<String, usize> = HashMap::new();
            for arg in a {
                offsets.insert(arg.1, last_ptr(&offsets) + arg.0.size());
            }
            
            ret.push(Inst::Func(name));
            ret.append(&mut intermediate(block.1, &mut offsets));
        },
        ASTNodeR::Return(expr) => {
            ret.append(&mut intermediate_expr(expr.1, 0, offsets));
            ret.push(Inst::Ret(0));
        },
        // ignore
        ASTNodeR::TypeAlias(..) => {},
    }
    ret
}

fn gnereate(insts: Vec<Inst>) -> String {

    let register = |reg| match reg {
        0 => "rdx",
        1 => "rax",
        _ => unimplemented!()
    };

    let mut global_strings: Vec<String> = vec![];
    
    let mut ret: String = "\
    global _start\n\
    section .text\n\
    _start:\n\
    \tpush rbp\n\
    \tmov rbp, rsp\n\
    \tcall main\n\
    \tjmp _end\n\
    ".into();
    for a in insts {
        ret.push_str(match a {
            Inst::Func(name) => {
                format!("{name}:\n\tpush rbp\n\tmov rbp,rsp\n")
            },
            Inst::Call(name) => {
                format!("\tcall {name}\n")
            },
            Inst::If(reg) => {
                format!("\tcmp {}, 1\n\tjne .l2\n.l1:\n", register(reg))
            },
            Inst::Endif => {
                format!(".l2:\n")
            },
            Inst::Val(reg, tp, val) => {
                let str_val = match tp {
                    Type::Primitive(a) => {
                        match a {
                            PrimitiveType::Int => val,
                            PrimitiveType::Float => unimplemented!(),
                            PrimitiveType::Char => {
                                format!("{}", val.as_bytes()[0])
                            },
                            PrimitiveType::Void => unreachable!(),
                            PrimitiveType::Bool => {
                                if val == "true" {
                                    "1"
                                } else {
                                    "0"
                                }.into()
                            },
                        }
                    },
                    Type::Array(ref bx) => {
                        match **bx {
                            Type::Primitive(PrimitiveType::Char) => {
                                global_strings.push(val.clone());
                                format!("{}", val.replace(" ", "_"))
                            },
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                };

                let register = register(reg);

                format!("\tmov {register}, {str_val}\n")
            },
            Inst::Ret(reg) => {
                let register = register(reg);
                format!("\tmov rax, {register}\n\tpop rbp\n\tret\n")
            },
            Inst::Op(reg, op) => {
                match op {
                    Op::Add => format!("\tadd {}, {}\n",  register(reg.0), register(reg.1)),
                    Op::Sub => format!("\tsub {}, {}\n",  register(reg.0), register(reg.1)),
                    Op::Mul => format!("\timul {}, {}\n", register(reg.0), register(reg.1)),
                    Op::Div => format!("\tidiv {}, {}\n", register(reg.0), register(reg.1)),
                }
            },
            Inst::VarSet(reg, index) => {
                format!("\tmov [rbp-{}], {}\n", index, register(reg))
            },
            Inst::Var(reg, index) => {
                format!("\tmov {}, [rbp-{}]\n", register(reg), index)
            }
            _ => format!("")
        }.as_str())
    }
    ret.push_str("_end:\n\tmov rdi, rax\n\tmov rax, 60\n\tsyscall\nsection .bss\n");

    for a in global_strings {
        ret.push_str(format!("{}:\n\tdq {}\n\tdd \"{a}\"\n", a.replace(" ", "_"), a.len()).as_str());
    }
    
    ret
}

fn main() {
    let outfile: String = "./a.out".into();
    
    let mut args = args();
    let arg0 = args.next().unwrap_or_else(|| {"<program>".into()});

    let mut error: bool = false;
    
    let filename: String = match args.next() {
        Some(a) => a,
        None => {
            eprintln!("{esc}[31mERROR: <filename> not given\n{esc}[0mUsage: {arg0} {esc}[31m<filename>{esc}[0m", esc = 27 as char);
            exit(1);
        },
    };

    let mut parser: Parser = Parser::new(filename).unwrap_or_else(|a| {
        eprintln!("Error reading file: {}", a);
        exit(1);
    });
    
    eprintln!("---  START PARSING ---");

    let a = parser.parse();
    match a {
        Err(ref e) => {
            for a in e {
                let (t, v) = a;
                eprintln!("{}: {}", t, v);
            }
            error = true;
        }
        Ok(_) => {}
    }

    let checked: bool;
    
    if let Ok(ast) = a.clone() {
        eprintln!("--- END PARSING ---\nAST NODE: \n{:#?}", ast);
        eprintln!("---  START CHECKING ---: \n");
        match check(ast, parser.lexer) {
            Err(e) => {
                for a in e {
                    let (t, v) = a;
                    eprintln!("{}: {}", t, v);
                };
                checked = false;
                error = true;
            },
            Ok(_) => checked = true,
        };
    } else {
        checked = false;
    }

    if checked {
        eprintln!("--- END CHECKING ---\n");
        if let Ok(ast) = a.clone() {
            let intermediate = intermediate(ast.1, &mut HashMap::new());
            println!("--- START INTERMEDIATE REPRESANTATION ---");
            println!("{:#?}", intermediate);
            println!("--- END INTERMEDIATE REPRESANTATION ---");

            let asm = gnereate(intermediate);
            println!("--- START ASSEMBLY ---");
            println!("{}", asm);
            println!("--- END ASSEMBY ---");

            match fs::write("tmp.asm", asm) {
                Ok(_) => {},
                Err(a) => {
                    eprintln!("Error writing file: {}", a);
                    exit(1);
                },
            };

            // call nasm and ld
            let nasm =  Command::new("nasm").arg("-felf64").arg("tmp.asm").output().unwrap();
            if ! nasm.status.success() {
                eprintln!("ERROR executing nasm: \n{}", std::str::from_utf8(&nasm.stderr).unwrap());
                error = true;
            }

            let ld = Command::new("ld").arg("-o").arg(outfile.clone()).arg("tmp.o").output().unwrap();
            if ! ld.status.success() {
                eprintln!("ERROR executing ld:\n{}", std::str::from_utf8(&ld.stderr).unwrap());
                error = true;
            }
        }
    }


    if error {
        exit(1);
    }
}


