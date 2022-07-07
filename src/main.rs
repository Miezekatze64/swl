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
    "intrinsic",
    "as",
    "arr"
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
        
        if a == b || a == Type::Primitive(PrimitiveType::Float) && b == Type::Primitive(PrimitiveType::Int) {
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

    fn size(&self, aliases: &HashMap<String, Type>) -> usize {
        match self {
            Type::Primitive(a) => match a {
                PrimitiveType::Int => 8,
                PrimitiveType::Float => 8,
                PrimitiveType::Char => 1,
                PrimitiveType::Void => 0,
                PrimitiveType::Bool => 1,
            },
            Type::Custom(s) => {
                if aliases.contains_key(s) {
                    return aliases.get(s).unwrap().size(aliases);
                }
                unimplemented!("{s} shouldn't be a type")
            },
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
            Op::Eq => "==",
            Op::Less => "<",
            Op::Greater => ">",
            Op::Mod => "%",
        })
    }
}

impl Op {
    fn combine_type(&self, a: &Type, b: &Type, aliases: &HashMap<String, Type>) -> Type {
        match self {
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod => {
                match a {
                    Type::Primitive(ref v) => {
                        match v {
                            PrimitiveType::Int => if a.is_compatible(b, aliases) {
                                a.clone()
                            } else {
                                Type::Invalid
                            },
                            PrimitiveType::Float => if a.is_compatible(b, aliases) {
                                a.clone()
                            } else {
                                Type::Invalid
                            },
                            PrimitiveType::Char => Type::Invalid,
                            PrimitiveType::Void => Type::Invalid,
                            PrimitiveType::Bool => Type::Invalid,
                        }
                    },
                    _ => Type::Invalid,
                }
            },
            Op::Less | Op::Greater => match a {
                Type::Primitive(ref p) => match p {
                    PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Char => if a.is_compatible(b, aliases) {
                        Type::Primitive(PrimitiveType::Bool)
                    } else {
                        Type::Invalid
                    },
                    _ => Type::Invalid,
                },
                _ => Type::Invalid,
            },
            Op::Eq => if a == b {
                Type::Primitive(PrimitiveType::Bool)
            } else {
                Type::Invalid
            },
        }
    }
}

fn is_valid_type(val: &str) -> bool {
    // TODO: check for custom types
    val == "int" || val == "float" || val == "char" || val == "void"
}

fn parse_type(string: String) -> Option<Type> {
    if string.is_empty() || !string.chars().next().unwrap().is_alphabetic() {
        return None;
    }
    match string.as_str() {
        "int"    => Some(Type::Primitive(PrimitiveType::Int)),
        "float"  => Some(Type::Primitive(PrimitiveType::Float)),
        "char"   => Some(Type::Primitive(PrimitiveType::Char)),
        "void"   => Some(Type::Primitive(PrimitiveType::Void)),
        _ => {
            if string.ends_with('*') {
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

            if is_valid_type(val) {
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
                    '='|'+'|'-'|'*'|'/'|'<'|'>'|'%' => {
                        if ttype == TokenType::Type && ch == '*' {
                            val.push(ch);
                            ttype = TokenType::Type;
                        } else if ttype == TokenType::Operator && val == "/" && ch == '/' {
                            // comment
                            loop {
                                if self.source[self.pos] == '\n' {
                                    break;
                                }
                                self.pos += 1;
                            }
                            return self.next_token();
                        } else if ttype == TokenType::Operator && val == "/" && ch == '*' {
                            // multi-line comment
                            loop {
                                if self.pos < 2 {
                                    self.pos += 1;
                                    continue;
                                }
                                if self.source[self.pos-1] == '*' && self.source[self.pos] == '/' {
                                    self.pos += 1;
                                    break;
                                }
                                self.pos += 1;
                            }
                            return self.next_token();
                        } else if ttype == TokenType::Operator && val == "-" && ch == '>' {
                            val.push(ch);
                            ttype = TokenType::Special;
                        } else if ttype == TokenType::Operator && val.starts_with('<') && ch == '-' {
                            val.push(ch);
                            ttype = TokenType::Special;
                        } else if ttype == TokenType::Undef || ttype == TokenType::Operator {
                            ttype = TokenType::Operator;
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
                    ';' | '(' | ')' | ',' | '{' | '}'|'['|']' => {
                        if ttype == TokenType::Undef {
                            ttype = TokenType::Special;
                            val.push(ch);
                        } else if ttype == TokenType::Special && !val.contains(')') && !val.contains('(') && !val.contains('[') && !val.contains(']') && !val.contains('}') && !val.contains('{') && ch != ';' {
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
                    _ => {
                        self.pos += 1;
                        return Err((ErrorLevel::Err, error!(self, self.pos, "invalid character `{ch}`")))
                    }
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
    Intrinsic(String, String, Vec<(Type, String)>, Type),
    ArrIndexInit(String, Expression, Expression),
}

type Expression = (usize, ExpressionR, Option<Type>);

#[derive(Debug, Clone, PartialEq)]
enum ExpressionR {
    T(Box<Expression>, Op, Box<Expression>, u32),
    Val(Type, String),
    Var(String),
    F(String, Vec<Expression>),
    Arr(Vec<Expression>),
    Undef,
    ArrAlloc(Type, usize),
    Index(String, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Less,
    Greater,
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
    fn from_str(s: String) -> Result<Self, Error> {
        match s.as_str() {
            "+" =>  Ok(Self::Add),
            "-" =>  Ok(Self::Sub),
            "*" =>  Ok(Self::Mul),
            "/" =>  Ok(Self::Div),
            "%" => Ok(Self::Mod),
            "==" => Ok(Self::Eq),
            "<" => Ok(Self::Less),
            ">" => Ok(Self::Greater),
            _ => Err((ErrorLevel::Fatal, format!("FATAL: invalid operator `{s}` set from lexer"))),
        }
    }
    
    fn get_precedence(op: Op) -> u32 {
        match op {
            Op::Eq | Op::Less | Op::Greater => 0,
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div | Op::Mod => 2,
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
                _ = $lexer.next_token();
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
    ($a:expr, $errors:expr) => {
        match $a {
            Ok(a) => a,
            Err(e) => {
                for err in e {
                    $errors.push(err);
                }
                return Err($errors);
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
            let (_, exp, _) = working_expr;
            let is_first: bool = match exp {
                ExpressionR::T(_, _, _, precedence) => {
                    Op::get_precedence(op) <= *precedence
                },
                ExpressionR::Val(..) | ExpressionR::Arr(..) | ExpressionR::F(..) | ExpressionR::Var(..) | ExpressionR::ArrAlloc(..) | ExpressionR::Index(..) => {
                    true
                },
                ExpressionR::Undef => unreachable!(),
            };

            if is_first {
                let tmp_expr = (working_expr.0, ExpressionR::T(Box::new(working_expr.clone()), op, right_expr
                                             , Op::get_precedence(op)), None);
                ret = Box::new(tmp_expr);
            } else {
                assert!(matches!(*working_expr, (_, ExpressionR::T(..), _)));
                let mut expr = working_expr.clone();
                let tmp_expr = if let (pos, ExpressionR::T(_, _, right, _), _) = &expr {
                    (*pos, ExpressionR::T(right.clone(), op, right_expr, Op::get_precedence(op)), None)
                } else {
                    unreachable!();
                };

                if let (pos, ExpressionR::T(a, b, _, d), _) = expr {
                    expr = (pos, ExpressionR::T(a, b, Box::new(tmp_expr), d), None);
                }
                    
                ret = Box::new(expr);
            }

            ret
        };
        let mut left_expr = Box::new((0, ExpressionR::Undef, None));

        // parse left side
        let subexpr = self.parse_subexpr(seperators)?;
        let mut nleft_expr = subexpr.0;

        loop {
            if let (_, ExpressionR::Undef, _) = *nleft_expr {
                return Ok((*left_expr, subexpr.1));
            }
            left_expr = nleft_expr.clone();
            
            // parse operation
            let tk_op = get_peek_token!(self.lexer, errors);
            let op = if tk_op.ttype == TokenType::Operator {
                match Op::from_str(tk_op.value) {
                    Ok(a) => a,
                    Err(e) => {
                        errors.push(e);
                        self.lexer.next_token().unwrap();
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

            if let (_, ExpressionR::Undef, _) = *subexpr_r.0 {
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
            
            Ok(Some((pos, ASTNodeR::VarDec(typ, ident))))
        }


        // may be a later defined type
        if token.ttype == TokenType::Ident && self.lexer.peek_token().unwrap().ttype == TokenType::Ident {
            token.ttype = TokenType::Type;
        }

        type Args = Vec<(Type, String)>;

        fn parse_function_decl(parser: &mut Parser, errors: &mut Vec<(ErrorLevel, String)>) -> Result<(Args, Type), Vec<Error>> {
            // parse args
            let mut args: Args = vec![];
            
            let check = get_peek_token!(parser.lexer, errors);
            if check.ttype != TokenType::Special || check.value != ")" {
                loop {
                    let t = match parser.parse_type() {
                        Ok(a) => a,
                        Err(e) => {
                            errors.push(e);
                            return Err(errors.clone());
                        },
                    };

                    let nval = err_break!(parser.expect(Some(TokenType::Ident), None), errors).value;
                    args.insert(args.len(), (t, nval));

                    let token = get_token!(parser.lexer, errors);
                    let val = token.value;
                    if token.ttype != TokenType::Special {
                        errors.push((ErrorLevel::Err, error!(parser.lexer, token.pos, "unexpected token `{val}`, `,` or `)` expect")));
                        return Err(errors.clone());
                    }
                    
                    if val == "," {
                        continue;
                    } else if val == ")" {
                        break;
                    } else {
                        errors.push((ErrorLevel::Err, error!(parser.lexer, token.pos, "unexpected token `{val}`, `,` or `)` expect")));
                        return Err(errors.clone());
                    }
                }
            } else {
                get_token!(parser.lexer, errors);
            }

            // optional return type (-> type)
            // else {
            let mut ret_type = Type::Primitive(PrimitiveType::Void);
            
            let next_token = get_peek_token!(parser.lexer, errors);
            if next_token.ttype == TokenType::Special && next_token.value == "->" {
                parser.lexer.next_token().unwrap();
                // expect return type

                let tp = match parser.parse_type() {
                    Ok(a) => a,
                    Err(e) => {
                        errors.push(e);
                        return Err(errors.clone());
                    },
                };
                ret_type = tp;//parse_type(val);
            }

            Ok((args, ret_type))
        }
        
        match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Char | TokenType::String | TokenType::Bool => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "invalid literal")));
                return self.parse_block_statement(is_root);
            },
            TokenType::Ident => {
                let expr = self.parse_ident_expr(val.clone(), token.pos,  &[";"])?;
                let (_, exp, _) = *expr;
                match exp {
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
                    ExpressionR::Index(a, ind) => {
                        let var_token = get_token!(self.lexer, errors);
                        let var_val = var_token.value;
                        
                        if var_token.ttype == TokenType::Operator && var_val == "=" {
                            let expr = self.parse_expr(&[";"])?.0;
                            return Ok(Some((token.pos, ASTNodeR::ArrIndexInit(a, *ind, expr))));
                        } else {
                            errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{var_val}`")));
                        }
                    },
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
                    ExpressionR::ArrAlloc(..) => todo!(),
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
                    // array declaration (e. g. [int])

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
                    
                    let (args, ret_type) = err_add!(parse_function_decl(self, &mut errors), errors);
                    
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
                "intrinsic" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "intrinsics only allowed at top-level")));
                        return Err(errors);
                    }
                    
                    // expect identifier
                    let intr_name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;

                    // expect `as`
                    err_ret!(self.expect(Some(TokenType::Keyword), Some("as".into())), errors);
                    
                    // expect identifier
                    let intr_func = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                    
                    // expect `(`
                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors);

                    let (args, ret_val) = err_add!(parse_function_decl(self, &mut errors), errors);

                    return Ok(Some((token.pos, ASTNodeR::Intrinsic(intr_name, intr_func, args, ret_val))));
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
                        if let (_, ExpressionR::Undef, _) = ret.0 {
                            break;
                        }
                        args.insert(args.len(), ret.0);
                        if token.value == ")" {
                            break;
                        }
                    }
                    let func_expr: Expression = (ident_pos, ExpressionR::F(ident, args), None);
                    Ok(Box::new(func_expr))
                },
                "[" => {
                    // array indexing
                    self.lexer.next_token().unwrap();
                    let index = self.parse_expr(&["]"])?;
                    return Ok(Box::new((ident_pos, ExpressionR::Index(ident, Box::new(index.0)), None)));
                }
                a => if seperators.contains(&a) {
                    Ok(Box::new((ident_pos, ExpressionR::Var(ident), None)))
                } else {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                    Err(errors)
                },
            },
            TokenType::Operator => {
                Ok(Box::new((token.pos, ExpressionR::Var(ident), None)))
            },
            TokenType::Undef => unreachable!(),
        }
    }

    fn parse_subexpr(&mut self, seperators: &[&'static str]) -> Result<(Box<Expression>, Token), Vec<Error>> {
        let inverse = |right_expr: &mut Box<Expression>| {
            let tmp_expr = right_expr.clone();
            **right_expr = (tmp_expr.0, ExpressionR::T(Box::new((0, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), "-1".into()), None)), Op::Mul, tmp_expr, Op::get_precedence(Op::Mul)), None);
        };

        let mut errors: Vec<Error> = vec![];

        let token = get_token!(self.lexer, errors);
        let val = token.clone().value;
        match token.ttype {
            TokenType::Type | TokenType::Eof | TokenType::Undef => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                Err(errors)
            },
            TokenType::Keyword => match val.as_str() {
                "arr" => {
                    err_ret!(self.expect(Some(TokenType::Special), Some("[".into())), errors);

                    // parse type
                    let tp = err_ret!(self.parse_type(), errors);

                    // comma
                    err_ret!(self.expect(Some(TokenType::Special), Some(",".into())), errors);

                    // parse size
                    let size_tk = err_ret!(self.expect(Some(TokenType::Int), None), errors);
                    let size: usize = size_tk.value.parse::<usize>().unwrap();

                    err_ret!(self.expect(Some(TokenType::Special), Some("]".into())), errors);
                    
                    Ok((Box::new((token.pos, ExpressionR::ArrAlloc(tp, size), None)), token))
                },
                _ => {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected keyword `{val}`")));
                    Err(errors)
                }
            },
            TokenType::Int => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), val), None)), token))
            },
            TokenType::Bool => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Bool), val), None)), token))
            },
            TokenType::Float => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Float), val), None)), token))
            },
            TokenType::Char => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Char), val), None)), token))
            },
            TokenType::String => {
                Ok((Box::new((token.pos, ExpressionR::Val(Type::Array(Box::new(Type::Primitive(PrimitiveType::Char))), val), None)), token))
            },
            TokenType::Ident => {
                let ident = self.parse_ident_expr(val, token.pos, seperators)?;
                Ok((ident, token))
            },
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
                    Ok((Box::new((token.pos, ExpressionR::Arr(arr), None)), Token {
                        pos: 0,
                        ttype: TokenType::Undef,
                        value: "".into(),
                    }))
                }
                _ => if seperators.contains(&val.as_str()) {
                    Ok((Box::new((0, ExpressionR::Undef, None)), token))
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

        if let Some(ival) = val {
            let mut is_valid: bool = ival == token.value;
            if let Some(t) = typ {
                is_valid &= token.ttype == t;
            }

            if ! is_valid {
                let exp_val = ival;
                let val = token.value;
                Err((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, `{exp_val}` expected")))
          } else {
                Ok(token)
            }
        } else if let Some(t) = typ {
            let is_valid: bool = token.ttype == t;
            if ! is_valid {
                let val = token.value;
                let ttype = t.to_string();
                Err((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, {ttype} expected")))
          } else {
                Ok(token)
            }
        } else {
            Ok(token)
        }
    }
}

fn check(ast: &mut ASTNode, mut lexer: Lexer) -> Result<HashMap<String, Type>, Vec<Error>> {
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
                },
                (_, ASTNodeR::Intrinsic(_, fname, args, rt)) => {
                    functions.insert(fname, (args.into_iter().map(|(a, _)|a).collect(), rt));
                }
                _ => {}
            }
        }
    } else {
        unreachable!();
    }

    fn check_references_expr(expr: &mut Expression, functions: &HashMap<String, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
        let (pos, e, _) = expr;
        let tp = || -> Type {match e {
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
            ExpressionR::Undef => {
                return Type::Primitive(PrimitiveType::Void);
            },
            ExpressionR::ArrAlloc(tp, _) => {
                return Type::Array(Box::new(tp.clone()));
            },
            ExpressionR::Index(ident, a) => {
                let inner_type = check_references_expr(a, functions, vars, aliases, errors, lexer);
                if ! Type::Primitive(PrimitiveType::Int).is_compatible(&inner_type, aliases) {
                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `int` and `{inner_type}` in array length definition")));
                    return Type::Invalid;
                } else {
                    let ident_type = check_references_expr(&mut (*pos, ExpressionR::Var(ident.to_string()), None), functions, vars, aliases, errors, lexer);
                    if let Type::Array(inner) = ident_type {
                        return *inner;
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "`{ident_type}` is not an array type")));
                        return Type::Invalid;
                    }
                }
            }
        }}();
        expr.2 = Some(tp.clone());
        tp
    }
    
    // check
    fn check_references(node: &mut ASTNode, functions: &HashMap<String, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, f: String, lexer: &mut Lexer) {
        if let (_, ASTNodeR::Block(ref mut arr)) = node {
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
                            continue;
                        }
                        
                        let func_args = &functions.get(name).unwrap().0;

                        let len_a = func_args.len();
                        let len_b = args.len();
                        if len_a != len_b {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        };

                        let mut index = 0;
                        for mut a in args {
                            let typa = check_references_expr(&mut a, functions, vars_sub, aliases, errors, lexer);
                            let typb = func_args.get(index).unwrap();

                            if ! typa.is_compatible(typb, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                                break;
                            }
                            index += 1;
                        }
                    },
                    (_, ASTNodeR::Block(..)) => {
                        check_references(a, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    (pos, ASTNodeR::If(ref mut expr, ref mut block)) => {
                        let bool_type = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                        }
                        check_references(block, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    (pos, ASTNodeR::Return(ref mut expr)) => {
                        let tp = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        let ret_type  = &functions.get(&f).unwrap().1;
                        if ! ret_type.is_compatible(&tp, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{ret_type}`, found `{tp}`, because of return type")));
                        }
                    },
                    (_, ASTNodeR::ArrIndexInit(var, ref mut ind, ref mut expr)) => {
                        if ! vars_sub.contains_key(var) {
                            errors.push((ErrorLevel::Err, error!(lexer, a.0, "undefined reference to variable `{var}`")));
                        } else {
                            let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                            if type_r == &Type::Invalid {
                                continue;
                            }

                            let type_ind = &check_references_expr(ind, functions, vars_sub, aliases, errors, lexer);
                            if type_ind == &Type::Invalid {
                                continue;
                            }
                            
                            let type_l = vars_sub.get(var).unwrap();
                            if let Type::Array(_) = type_l {} else {
                                errors.push((ErrorLevel::Err, error!(lexer, a.0, "{type_l} used in array indexing is not an array type")));
                            }
                            
                            if ! type_l.is_compatible(&Type::Array(Box::new(type_r.clone())), aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, a.0, "incompatible types: expeected `{type_l}`, found `{type_r}`")));
                            }

                            if ! Type::Primitive(PrimitiveType::Int).is_compatible(type_ind, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, a.0, "incompatible types: expeected int, found `{type_r}`")));
                            }
                            
                        }
                    },
                    (_, ASTNodeR::VarInit(var, ref mut expr)) => {
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
                    (_, ASTNodeR::TypeAlias(_, _)) => {},
                    (pos, ASTNodeR::Intrinsic(iname, ..)) => {
                        if ! intrinsics().contains_key(iname.as_str()) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to compiler intrinsic `{iname}`")));
                        }
                    },
                }
            };
        }
    }

    fn check_function_ret_paths(ast: &ASTNodeR, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) {
        match ast {
            ASTNodeR::Block(ref vec) => {
                for a in vec {
                    if let ASTNodeR::FunctionDecl(_, _, rt, block) = &a.1 {
                        if *rt == Type::Primitive(PrimitiveType::Void) {
                            continue;
                        }
                        let mut rs: bool = false;
                        if let ASTNodeR::Block(blk_vec) = &block.1 {
                            for statement in blk_vec {
                                match &statement.1 {
                                    ASTNodeR::If(_, _sub) => {
                                        // TODO: implement here, if else is implemented
                                    },
                                    ASTNodeR::Return(_) => {
                                        rs = true;
                                    },
                                    _ => {}
                                }
                            }
                        }
                        if ! rs {
                            errors.push((ErrorLevel::Err, error!(lexer, a.0, "not all paths lead to a return statement")));
                        }
                    }
                }
            },
            _ => unreachable!()
        }
    }

    check_references(ast, &functions, &vars, &type_aliases, &mut errors, "".into(), &mut lexer);
    check_function_ret_paths(&ast.1, &mut errors, &mut lexer);

    // type checking
    if errors.is_empty() {
        Ok(type_aliases)
    } else {
        Err(errors)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Inst {
    Func(String, HashMap<String, (usize, usize)>),
    VarSet(usize, usize, usize),
    If(usize),
    Endif,
    Ret(usize),
    Call(String),
    Op((usize, usize), usize, Op),
    Val(usize, Type, String),
    Var(usize, usize),
    RetVal(usize),
    Intrinsic(String, String),
    Push(usize),
    Pop(usize),
    Arr(usize, usize),
    Index(usize, usize),
    ArraySet(usize, usize, usize),
}

fn intermediate_expr(expr: Expression, index: usize, indicies: &mut HashMap<String, (usize, usize)>, aliases: &HashMap<String, Type>) -> Vec<Inst> {
    let mut ret = vec![];
    let (_pos, expr, tp) = expr;
    match expr {
        ExpressionR::T(fst, op, snd, _) => {
            ret.append(&mut intermediate_expr(*fst, index, indicies, aliases));
            ret.push(Inst::Push(index));
            ret.append(&mut intermediate_expr(*snd, index+1, indicies, aliases));
            ret.push(Inst::Pop(index));
            ret.push(Inst::Op((index, index+1), tp.unwrap().size(aliases), op));
        },
        ExpressionR::Val(tp, val) => {
            ret.push(Inst::Val(index, tp, val));
        },
        ExpressionR::Var(name) => {
            ret.push(Inst::Var(index, indicies.get(&name).unwrap().0));
        },
        ExpressionR::F(name, args) => {
            let mut ind = 0;
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, ind, indicies, aliases));
                ret.push(Inst::Push(ind));
                ind += 1;
            }

            for _ in args.iter().rev() {
                ind -= 1;
                ret.push(Inst::Pop(ind));
            }
            
            ret.push(Inst::Call(name));
            ret.push(Inst::RetVal(index));
        },
        ExpressionR::Arr(_) => {
            todo!();
        },
        ExpressionR::Undef => {
            
        },
        ExpressionR::ArrAlloc(tp, sz) => {
            ret.push(Inst::Arr(index, sz * tp.size(&aliases)));
        },
        ExpressionR::Index(var, expr) => {
            ret.append(&mut intermediate_expr(*expr, index, indicies, aliases));
            ret.push(Inst::Var(index+1, indicies.get(&var).unwrap().0));
            ret.push(Inst::Index(index, index+1));
        },
    }
    ret
}


fn last_ptr(offsets: &HashMap<String, (usize, usize)>) -> usize {
    let mut last = 0;
    for el in offsets {
        last = last.max(el.1.0);
    }
    last
}

fn intrinsics() -> HashMap<&'static str, &'static str> {
    [("syscall","\tpush rax\n\tpush rbx\n\tpush rcx\n\tpush rdx\n\tpop rdx\n\tpop rsi\n\tpop rdi\n\tpop rax\n\tsyscall\n\tret"),
     ("convert", "\tret\n"),
     ("str_to_ptr", "\tadd rax, 8\n\tret\n"),
     ("dereference", "\tmov rax, [rax]\n\tret\n")
    ]
        .iter()
        .cloned()
        .collect()
}

fn intermediate(ast: ASTNodeR, offsets: &mut HashMap<String, (usize, usize)>, aliases: HashMap<String, Type>) -> Vec<Inst> {
    let mut ret = vec![];
    
    match ast {
        ASTNodeR::Block(a) => {
            for i in a {
                ret.append(&mut intermediate(i.1, offsets, aliases.clone()));
            }
        },
        ASTNodeR::VarDec(tp, ref name) => {
            offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
        },
        ASTNodeR::ArrIndexInit(arr, ind, expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, &aliases));
            ret.append(&mut intermediate_expr(ind, 1, offsets, &aliases));
            ret.push(Inst::ArraySet(0, 1, offsets.get(&arr).unwrap().0))
        },
        ASTNodeR::VarInit(ref name, expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, &aliases));
            let vals = offsets.get(name).unwrap();
            ret.push(Inst::VarSet(0, vals.1, vals.0))
        },
        ASTNodeR::FunctionCall(name, args) => {
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, 0, offsets, &aliases));
                ret.push(Inst::Push(0));
            }

            let mut index = args.len();
            for _ in args.iter().rev() {
                index -= 1;
                ret.push(Inst::Pop(index));
            }
            
            ret.push(Inst::Call(name));
        },
        ASTNodeR::If(expr, block) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, &aliases));
            ret.push(Inst::If(0));
            ret.append(&mut intermediate(block.1, offsets, aliases));
            ret.push(Inst::Endif);
        },
        ASTNodeR::FunctionDecl(name, a, rt, block) => {
            let mut offsets: HashMap<String, (usize, usize)> = HashMap::new();
            for arg in a {
                offsets.insert(arg.1, (last_ptr(&offsets) + arg.0.size(&aliases), arg.0.size(&aliases)));
            }
            
            ret.push(Inst::Func(name, offsets.clone()));
            ret.append(&mut intermediate(block.1, &mut offsets, aliases));
            if rt == Type::Primitive(PrimitiveType::Void) {
                ret.push(Inst::Ret(0));
            }
        },
        ASTNodeR::Return(expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, &aliases));
            ret.push(Inst::Ret(0));
        },
        ASTNodeR::Intrinsic(iname, fname, _, _) => {
            ret.push(Inst::Intrinsic(iname, fname));
        },
        // ignore
        ASTNodeR::TypeAlias(..) => {},
        
    }
    ret
}

fn gnereate(insts: Vec<Inst>) -> String {

    let datatype = |a: usize| match a {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        _ => "__invalid__"
    };

    let register = |reg| match reg {
        0 => "rax",
        1 => "rbx",
        2 => "rcx",
        3 => "rdx",
        4 => "rdi",
        5 => "rsi",
        a => unimplemented!("index: {a}")
    };

    let register_sz = |reg: usize, sz: usize| match register(reg) {
        "rax" => match sz {
            1 => "al",
            2 => "ax",
            4 => "eax",
            8 => "rax",
            _ => "__invalid__"
        },
        "rbx" => match sz {
            1 => "bl",
            2 => "bx",
            4 => "ebx",
            8 => "rbx",
            _ => "__invalid__"
        },
        "rcx" => match sz {
            1 => "cl",
            2 => "cx",
            4 => "ecx",
            8 => "rcx",
            _ => "__invalid__"
        },
        "rdx" => match sz {
            1 => "dl",
            2 => "dx",
            4 => "edx",
            8 => "rdx",
            _ => "__invalid__"
        },
        _ => "__invalid__"
    };

    let mut global_strings: Vec<String> = vec![];
    let mut global_arrays: Vec<usize> = vec![];
    
    let mut ret: String = "\
    global _start\n\
    section .text\n\
    _start:\n\
    \tpush rbp\n\
    \tmov rbp, rsp\n\
    \tcall main\n\
    \tjmp _end\n\
    ".into();

    let mut intrinsic_labels: Vec<String> = vec![];

    let mut arr_index: usize = 0;
    
    for a in insts {
        ret.push_str(match a {
            Inst::Func(name, offsets) => {
                let string = (0..offsets.len()).map(|a| format!("\tsub rsp, {ind}\n\tmov [rbp-{ind}], {}\n", register(a), ind = offsets.iter().nth(a).unwrap().1.0)).collect::<String>();
                format!("{name}:\n\tpush rbp\n\tmov rbp,rsp\n{string}")
            },
            Inst::Call(name) => {
                format!("\tcall {name}\n")
            },
            Inst::If(reg) => {
                format!("\tcmp {}, 1\n\tjne .l2\n.l1:\n", register(reg))
            },
            Inst::Endif => {
                ".l2:\n".into()
            },
            Inst::Push(reg) => {
                format!("\tpush {}\n", register(reg))
            },
            Inst::Pop(reg) => {
                format!("\tpop {}\n", register(reg))
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
                                format!("str_{}", val.to_lowercase().chars().map(|a| if a.is_alphanumeric() {a} else {'_'}).collect::<String>())
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
                if register != "rax" {
                    format!("\tmov rax, {register}\n\tleave\n\tret\n")
                } else {
                    "\tleave\n\tret\n".into()
                }
            },
            Inst::Op(reg, sz, op) => {
                match op {
                    Op::Add => format!("\tadd {}, {}\n",  register(reg.0), register(reg.1)),
                    Op::Sub => format!("\tsub {}, {}\n",  register(reg.0), register(reg.1)),
                    Op::Mul => format!("\timul {}, {}\n", register(reg.0), register(reg.1)),
                    Op::Div => format!("\tidiv {}, {}\n", register(reg.0), register(reg.1)),
                    Op::Eq => format!("\tcmp {r0}, {r1}\n\tsete al\n\tmov {r0}, al\n", r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz)),
                    Op::Less => format!("\tcmp {r0}, {r1}\n\tsetl al\n\tmov {r0}, al\n", r0 = register(reg.0), r1 = register(reg.1)),
                    Op::Greater => format!("\tcmp {r0}, {r1}\n\tsetg al\n\tmov {r0}, al\n", r0 = register(reg.0), r1 = register(reg.1)),
                    Op::Mod => format!("\tmov rax, {r0}\n\tidiv {r1}\n\tmov {r0}, rdx\n", r0 = register(reg.0), r1 = register(reg.1)),
                }
            },
            Inst::VarSet(reg, sz, index) => {
                format!("\tsub rsp, {ind}\n\tmov {tp} [rbp-{ind}], {}\n", register_sz(reg, sz), ind = index, tp = datatype(sz))
            },
            Inst::Var(reg, index) => {
                format!("\tmov {}, [rbp-{}]\n", register(reg), index)
            },
            Inst::Intrinsic(iname, fname) => {
                if ! intrinsic_labels.contains(&iname) {
                    intrinsic_labels.push(iname.clone());
                    format!("{fname}: \n\tjmp {iname}\n{iname}: \n{}\n", intrinsics().get(iname.as_str()).unwrap())
                } else {
                    format!("{fname}: \n\tjmp {iname}\n")
                }
            },
            Inst::RetVal(reg) => {
                if reg != 0 {
                    format!("\tmov {}, rax\n", register(reg))
                } else {
                    "".into()
                }
            },
            Inst::Arr(reg, size) => {
                global_arrays.push(size);
                arr_index += 1;
                format!("\tmov {r0}, arr_{}\n", arr_index -1, r0 = register(reg))
            },
            Inst::Index(reg0, reg1) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                format!("\tadd {r1}, 8\n\tadd {r1}, {r0}\n\tmov {r0}, [{r1}]\n")
            },
            Inst::ArraySet(reg0, reg1, ind) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                format!("\tpush {r0}\n\tmov {r0}, [rbp-{ind}]\n\tadd {r0}, 8\n\tadd {r0}, {r1}\n\tpop {r1}\n\tmov [{r0}], {r1}\n")
//                format!("\tadd {r1}, rsp\n\tsub {r1}, {arr}\n\tmov [{r1}], {r0}\n")
//                format!("\tadd {r1}, 8\n\tadd {r1}, {r0}\n\tmov {r0}, [{r1}]\n")
            },
            
        }.as_str())
    }
    ret.push_str("_end:\n\tmov rdi, rax\n\tmov rax, 60\n\tsyscall\nsection .data\n");

    for a in global_strings {
        ret.push_str(format!("str_{}:\n\tdq {}\n\tdb `{a}`\n", a.to_lowercase().chars().map(|a| if a.is_alphanumeric() {a} else {'_'}).collect::<String>(), a.len()).as_str());
    }

//    ret.push_str("section .bss\n");
    
    let mut ind = 0;
    for a in global_arrays {
        ret.push_str(format!("arr_{}:\n\tdq {a}\n\tresb {a}\n", ind).as_str());
        ind += 1;
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

    let mut a = parser.parse();
    if let Err(ref e) = a {
        for a in e {
            let (t, v) = a;
            eprintln!("{}: {}", t, v);
        }
        error = true;
    }

    let checked: bool;

    let mut aliases: HashMap<String, Type> = HashMap::new();
    
    if let Ok(ref mut ast) = a {
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
            Ok(a) => {
                aliases = a;
                checked = true
            },
        };
    } else {
        checked = false;
    }

    if checked {
        eprintln!("--- END CHECKING ---\n");
        if let Ok(ast) = a {
            let intermediate = intermediate(ast.1, &mut HashMap::new(), aliases);
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

            let ld = Command::new("ld").arg("-o").arg(outfile).arg("tmp.o").output().unwrap();
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

