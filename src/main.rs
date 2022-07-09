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
    "arr",
    "while",
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
    Unchecked,    // VERY UNSAFE, DOESN'T USE TYPE CHECKKING (SIZE: 8)
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
                PrimitiveType::Unchecked => "unchecked"
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

        // UNSAFE: DON'T USE THIS IF NOT REALLY NEEDED
        if a == Type::Primitive(PrimitiveType::Unchecked) || b == Type::Primitive(PrimitiveType::Unchecked) {
            return true;
        }

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
                PrimitiveType::Unchecked => 8,
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

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            UnaryOp::Not       => "!",
        })
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add     => "+",
            BinaryOp::Sub     => "-",
            BinaryOp::Mul     => "*",
            BinaryOp::Div     => "/",
            BinaryOp::Eq      => "==",
            BinaryOp::Less    => "<",
            BinaryOp::Greater => ">",
            BinaryOp::Mod     => "%",
        })
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Op::Binary(a) => a.to_string(),
            Op::Unary(a) => a.to_string(),
        })
    }
}

impl UnaryOp {
    fn with_type(&self, t: &Type, _aliases: &HashMap<String, Type>) -> Type {
        match self {
            UnaryOp::Not => match t {
                Type::Primitive(PrimitiveType::Bool | PrimitiveType::Unchecked) => Type::Primitive(PrimitiveType::Bool),
                _ => Type::Invalid
            },
        }
    }
}


impl Op {
    fn combine_type(&self, a: &Type, b: &Type, aliases: &HashMap<String, Type>) -> Type {
        match self {
            Op::Unary(_) => {
                unreachable!()
            },
            Op::Binary(bin) => match bin {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
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
                                PrimitiveType::Unchecked => b.clone(),
                            }
                        },
                        _ => Type::Invalid,
                    }
                },
                BinaryOp::Less | BinaryOp::Greater => match a {
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
                BinaryOp::Eq => if (a == b) || (a == &Type::Primitive(PrimitiveType::Unchecked) || b == &Type::Primitive(PrimitiveType::Unchecked)) {
                    Type::Primitive(PrimitiveType::Bool)
                } else{
                    Type::Invalid
                },
            }
        }
    }
}

fn is_valid_type(val: &str) -> bool {
    // TODO: check for custom types
    val == "int" || val == "float" || val == "char" || val == "void" || val == "bool" || val == "unchecked"
}

fn parse_type(string: String) -> Option<Type> {
    if string.is_empty() || !string.chars().next().unwrap().is_alphabetic() {
        return None;
    }
    match string.as_str() {
        "int"       => Some(Type::Primitive(PrimitiveType::Int)),
        "float"     => Some(Type::Primitive(PrimitiveType::Float)),
        "char"      => Some(Type::Primitive(PrimitiveType::Char)),
        "void"      => Some(Type::Primitive(PrimitiveType::Void)),
        "bool"      => Some(Type::Primitive(PrimitiveType::Bool)),
        "unchecked" => Some(Type::Primitive(PrimitiveType::Unchecked)),
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
                        } else if ttype == TokenType::Operator && val == "-" {
                            val.push(ch);
                            ttype = TokenType::Int;
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
                    '!'|'='|'+'|'-'|'*'|'/'|'<'|'>'|'%' => {
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
                        } else if (ttype == TokenType::Operator && val == "-" && ch == '>')  ||  (ttype == TokenType::Operator && val.starts_with('<') && ch == '-') {
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

#[derive(Debug, Clone, PartialEq)]
struct ASTNode(usize, ASTNodeR);

#[derive(Debug, Clone, PartialEq)]
enum ASTNodeR {
    Block(Vec<ASTNode>),
    VarDec(bool, Type, String),
    VarInit(String, Expression),
    VarOp(String, BinaryOp, Expression),
    VarDecInit(bool, Type, String, Expression),
    FunctionCall(String, Vec<Expression>),
    If(Expression, Box<ASTNode>),
    FunctionDecl(String, Vec<(Type, String)>, Type, Box<ASTNode>),
    Return(Expression),
    TypeAlias(String, Type),
    Intrinsic(String, String, Vec<(Type, String)>, Type),
    ArrIndexInit(String, Expression, Expression),
    While(Expression, Box<ASTNode>),
}

#[derive(Debug, Clone, PartialEq)]
struct Expression(usize, ExpressionR, Option<Type>);

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
    UnaryOp(UnaryOp, Box<Expression>, u32),
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Op {
    Unary(UnaryOp),
    Binary(BinaryOp),
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Less,
    Greater,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum UnaryOp {
    Not,
}

// indents a strinig by 4 spaces
fn indent(string: String) -> String {
    let mut ret = String::new();
    for line in string.lines() {
        ret.push_str("    ");
        ret.push_str(line);
        ret.push_str("\n");
    }
    ret
}

/// to_string implementation of AST
impl std::fmt::Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl std::fmt::Display for ASTNodeR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTNodeR::Block(a) => {
                for l in a {
                    writeln!(f, "{}", l)?;
                }
                Ok(())
            },
            ASTNodeR::VarDec(_, typ, name) => {
                write!(f, "{typ} {name};")
            },
            ASTNodeR::VarInit(name, expr) => {
                write!(f, "{name} = {expr};")
            },
            ASTNodeR::FunctionCall(name, args) => {
                write!(f, "{name}(")?;
                for (index, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if index < args.len()-1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ");")
            },
            ASTNodeR::If(pred, block) => {
                write!(f, "if ({pred}) {{\n{}}}", indent(block.to_string()))
            },
            ASTNodeR::FunctionDecl(name, args, ret_type, block) => {
                write!(f, "\nfunc {name}(")?;
                for (index, (arg_type, arg_name)) in args.iter().enumerate() {
                    write!(f, "{arg_type} {arg_name}")?;
                    if index < args.len()-1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") ")?;
                if *ret_type != Type::Primitive(PrimitiveType::Void) {
                    write!(f, "-> {ret_type} ")?;
                }
                write!(f, "{{\n{}}}", indent(block.to_string()))
            },
            ASTNodeR::Return(expr) => {
                write!(f, "<- {expr};")
            },
            ASTNodeR::TypeAlias(name, typ) => {
                write!(f, "alias {name} = {typ};")
            },
            ASTNodeR::Intrinsic(iname, fname, args, ret_type) => {
                write!(f, "intrinsic {iname} as {fname}(")?;
                for (index, (arg_type, arg_name)) in args.iter().enumerate() {
                    write!(f, "{arg_type} {arg_name}")?;
                    if index < args.len()-1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") ")?;
                if *ret_type != Type::Primitive(PrimitiveType::Void) {
                    write!(f, "-> {ret_type} ")?;
                }
                write!(f, ";")
            },
            ASTNodeR::ArrIndexInit(array, index, value) => {
                write!(f, "{array}[{index}] = {value};")
            },
            ASTNodeR::VarDecInit(_, typ, name, expr) => {
                write!(f, "{typ} {name} = {expr};")
            },
            ASTNodeR::While(cond, block) => {
                write!(f, "while {cond} {{\n{block}}}")
            },
            ASTNodeR::VarOp(name, op, expr) => {
                write!(f, "{name} {op}= {expr};")
            },
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl std::fmt::Display for ExpressionR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionR::T(left, op, right, _) => {
                write!(f, "({left} {op} {right})")
            },
            ExpressionR::Val(tp, val) => {
                match tp {
                    Type::Primitive(_) => write!(f, "{val}"),
                    Type::Custom(_) => write!(f, "{val}[INVALID]"),
                    Type::Array(a) => match **a {
                        Type::Primitive(PrimitiveType::Char) => write!(f, "\"{val}\""),
                        _ => write!(f, "{{{val}}}[INVALID]"),
                    },
                    Type::Pointer(_) => write!(f, "{val}*[INVALID]"),
                    Type::Invalid => write!(f, "[INVALID]"),
                }
            },
            ExpressionR::Var(var) => {
                write!(f, "{var}")
            },
            ExpressionR::F(name, args) => {
                write!(f, "{name}(")?;
                for (index, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if index < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            },
            ExpressionR::Arr(vals) => {
                write!(f, "{{")?;
                for (index, arg) in vals.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if index < vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            },
            ExpressionR::Undef => {
                write!(f, "")
            },
            ExpressionR::ArrAlloc(typ, sz) => {
                write!(f, "arr[{typ}, {sz}]")
            },
            ExpressionR::Index(array, index) => {
                write!(f, "{array}[{index}]")
            },
            ExpressionR::UnaryOp(op, expr ,_) => {
                write!(f, "{op}{expr}")
            },
        }
    }
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
            "+"  => Ok(Op::Binary(BinaryOp::Add)),
            "-"  => Ok(Op::Binary(BinaryOp::Sub)),
            "*"  => Ok(Op::Binary(BinaryOp::Mul)),
            "/"  => Ok(Op::Binary(BinaryOp::Div)),
            "%"  => Ok(Op::Binary(BinaryOp::Mod)),
            "==" => Ok(Op::Binary(BinaryOp::Eq)),
            "<"  => Ok(Op::Binary(BinaryOp::Less)),
            ">"  => Ok(Op::Binary(BinaryOp::Greater)),
            "!"  => Ok(Op::Unary(UnaryOp::Not)),
            _ => Err((ErrorLevel::Fatal, format!("FATAL: invalid operator `{s}` set from lexer"))),
        }
    }
    
    fn get_precedence(op_type: Op) -> u32 {
        match op_type {
            Op::Binary(bin_op) => match bin_op {
                BinaryOp::Eq | BinaryOp::Less | BinaryOp::Greater => 0,
                BinaryOp::Add | BinaryOp::Sub => 1,
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 2,
            },
            Op::Unary(un_op) => match un_op {
                UnaryOp::Not => 0,
            }
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
        let mut block: ASTNode = ASTNode(pos, ASTNodeR::Block(vec![]));
        let mut errors: Vec<Error> = vec![];
        loop {
            match block {
                ASTNode(_, ASTNodeR::Block(ref mut vec)) => {
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
                _ => unimplemented!(),
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
            let Expression(_, exp, _) = working_expr;
            let is_first: bool = match exp {
                ExpressionR::UnaryOp(_, _, precedence) => {
                    Op::get_precedence(op) <= *precedence
                },
                ExpressionR::T(_, _, _, precedence) => {
                    Op::get_precedence(op) <= *precedence
                },
                ExpressionR::Val(..) | ExpressionR::Arr(..) | ExpressionR::F(..) | ExpressionR::Var(..) | ExpressionR::ArrAlloc(..) | ExpressionR::Index(..) => {
                    true
                },
                ExpressionR::Undef => unreachable!(),
            };

            if is_first {
                let tmp_expr = Expression(working_expr.0, ExpressionR::T(Box::new(working_expr.clone()), op, right_expr
                                             , Op::get_precedence(op)), None);
                ret = Box::new(tmp_expr);
            } else {
                assert!(matches!(*working_expr, Expression(_, ExpressionR::T(..), _)));
                let mut expr: Expression = working_expr.clone();
                let tmp_expr = if let Expression(pos, ExpressionR::T(_, _, right, _), _) = &expr {
                    Expression(*pos, ExpressionR::T(right.clone(), op, right_expr, Op::get_precedence(op)), None)
                } else {
                    unreachable!();
                };

                if let Expression(pos, ExpressionR::T(a, b, _, d), _) = expr {
                    expr = Expression(pos, ExpressionR::T(a, b, Box::new(tmp_expr), d), None);
                }
                    
                ret = Box::new(expr);
            }

            ret
        };
        let mut left_expr = Box::new(Expression(0, ExpressionR::Undef, None));

        // parse left side
        let subexpr = self.parse_subexpr(seperators)?;
        let mut nleft_expr = subexpr.0;

        loop {
            if let Expression(_, ExpressionR::Undef, _) = *nleft_expr {
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

            if let Expression(_, ExpressionR::Undef, _) = *subexpr_r.0 {
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

        fn parse_var_dec(parser: &mut Parser, typ: Type, is_root: bool) -> Result<Option<ASTNode>, Vec<Error>> {
            // declare variable
            let pos = parser.lexer.pos;
            let mut errors: Vec<Error> = vec![];
            
            let next_token = err_ret!(parser.expect(Some(TokenType::Ident), None), errors);
            let ident = next_token.value;
            
            // check next token
            let next_token = err_ret!(parser.lexer.next_token(), errors);
            let ntv = next_token.value.clone();
            match next_token.ttype {
                TokenType::Special => {
                    if next_token.value == ";" {
                        Ok(Some(ASTNode(pos, ASTNodeR::VarDec(is_root, typ, ident))))
                        } else {
                            return Err(vec![(ErrorLevel::Err, error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`"))]);
                        }
                },
                TokenType::Operator => {
                    if next_token.value == "=" {
                        // variable initialization
                        let expr = parser.parse_expr(&[";"])?;
                        Ok(Some(ASTNode(pos, ASTNodeR::VarDecInit(is_root, typ, ident, expr.0))))
                    } else {
                        return Err(vec![(ErrorLevel::Err, error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`"))]);
                    }
                },
                _ => return Err(vec![(ErrorLevel::Err, error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`"))]),
            }
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
                let Expression(_, exp, _) = *expr;
                match exp {
                    ExpressionR::Var(..) => {
                        let var_token = get_token!(self.lexer, errors);
                        let var_val = var_token.value;
                        
                        if var_token.ttype == TokenType::Operator && var_val == "=" {
                            let expr = self.parse_expr(&[";"])?.0;
                            return Ok(Some(ASTNode(token.pos, ASTNodeR::VarInit(val, expr))));
                        } else if var_token.ttype == TokenType::Operator && var_val.ends_with("=") {
                            let expr = self.parse_expr(&[";"])?.0;
                            let op = err_ret!(Op::from_str(var_val.as_str()[..(var_val.len()-1)].into()), errors);
                            if let Op::Binary(bop) = op {
                                return Ok(Some(ASTNode(token.pos, ASTNodeR::VarOp(val, bop, expr))));
                            }
                        } else {
                            errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{var_val}`")));
                        }
                    }
                    ExpressionR::Val(..) => unreachable!(),
                    ExpressionR::UnaryOp(_, _, _) => unreachable!(),
                    ExpressionR::T(..) => unreachable!(),
                    ExpressionR::Arr(..) => unreachable!(),
                    ExpressionR::Index(a, ind) => {
                        let var_token = get_token!(self.lexer, errors);
                        let var_val = var_token.value;
                        
                        if var_token.ttype == TokenType::Operator && var_val == "=" {
                            let expr = self.parse_expr(&[";"])?.0;
                            return Ok(Some(ASTNode(token.pos, ASTNodeR::ArrIndexInit(a, *ind, expr))));
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
                        
                        return Ok(Some(ASTNode(token.pos, ASTNodeR::FunctionCall(name, args))));
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
                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Return(expression))));
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

                    return match parse_var_dec(self, array_type, is_root) {
                        Ok(a) => {
                            Ok(a)
                        },
                        Err(mut e) => {
                            errors.append(&mut e);
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
                return match parse_var_dec(self, typ, is_root) {
                    Ok(a) => Ok(a),
                    Err(mut e) => {
                        errors.append(&mut e);
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

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::If(predicate, Box::new(block)))));
                    
                },
                "while" => {
                     if is_root {
                         errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "`if` not allowed at top level")));
                         return Err(errors);
                     }
                    // next token: '('
                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors);
                    // loop condition
                    let condition = err_add!(self.parse_expr(&[")"]), errors).0;

                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors);
                    let block = self.parse_block(token.pos)?;
                    
                    return Ok(Some(ASTNode(token.pos, ASTNodeR::While(condition, Box::new(block)))));
                },
                // "for" => {
                //     if is_root {
                //         errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "`if` not allowed at top level")));
                //         // next token: '('
                //         err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors);

                //         // var init
                //         // get type
                //         let init_type = err_ret!(self.expect(Some(TokenType::Type), None), errors);
                //         // get identifier
                //         let init_ident = err_ret!(self.expect(Some(TokenType::Ident), None), errors);
                //         // next token: '='
                //         err_ret!(self.expect(Some(TokenType::Operator), Some("=".into())), errors);
                //         let init_expr = err_add!(self.parse_expr(&[";"]), errors);

                //         // loop condition
                //         let cond_expr = err_add!(self.parse_expr(&[";"]), errors);
                        
                //         // incrementor
                //         let inc = self.parse_block_statement(false);
                        
                //         return Err(errors);
                //     }
                // },
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

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::FunctionDecl(name, args, ret_type, Box::new(block)))));

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

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::TypeAlias(ident, typ))));
                    
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

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Intrinsic(intr_name, intr_func, args, ret_val))));
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
                        if let Expression(_, ExpressionR::Undef, _) = ret.0 {
                            break;
                        }
                        args.insert(args.len(), ret.0);
                        if token.value == ")" {
                            break;
                        }
                    }
                    let func_expr: Expression = Expression(ident_pos, ExpressionR::F(ident, args), None);
                    Ok(Box::new(func_expr))
                },
                "[" => {
                    // array indexing
                    self.lexer.next_token().unwrap();
                    let index = self.parse_expr(&["]"])?;
                    Ok(Box::new(Expression(ident_pos, ExpressionR::Index(ident, Box::new(index.0)), None)))
                }
                a => if seperators.contains(&a) {
                    Ok(Box::new(Expression(ident_pos, ExpressionR::Var(ident), None)))
                } else {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                    Err(errors)
                },
            },
            TokenType::Operator => {
                Ok(Box::new(Expression(token.pos, ExpressionR::Var(ident), None)))
            },
            TokenType::Undef => unreachable!(),
        }
    }

    fn parse_subexpr(&mut self, seperators: &[&'static str]) -> Result<(Box<Expression>, Token), Vec<Error>> {
        let inverse = |right_expr: &mut Box<Expression>| {
            let tmp_expr = right_expr.clone();
            **right_expr = Expression(tmp_expr.0, ExpressionR::T(Box::new(Expression(0, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), "-1".into()), None)), Op::Binary(BinaryOp::Mul), tmp_expr, Op::get_precedence(Op::Binary(BinaryOp::Mul))), None);
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
                    
                    Ok((Box::new(Expression(token.pos, ExpressionR::ArrAlloc(tp, size), None)), token))
                },
                _ => {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected keyword `{val}`")));
                    Err(errors)
                }
            },
            TokenType::Int => {
                Ok((Box::new(Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), val), None)), token))
            },
            TokenType::Bool => {
                Ok((Box::new(Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Bool), val), None)), token))
            },
            TokenType::Float => {
                Ok((Box::new(Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Float), val), None)), token))
            },
            TokenType::Char => {
                Ok((Box::new(Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Char), val), None)), token))
            },
            TokenType::String => {
                Ok((Box::new(Expression(token.pos, ExpressionR::Val(Type::Array(Box::new(Type::Primitive(PrimitiveType::Char))), val), None)), token))
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
                    Ok((Box::new(Expression(token.pos, ExpressionR::Arr(arr), None)), Token {
                        pos: 0,
                        ttype: TokenType::Undef,
                        value: "".into(),
                    }))
                }
                _ => if seperators.contains(&val.as_str()) {
                    Ok((Box::new(Expression(0, ExpressionR::Undef, None)), token))
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
                } else /* check for unary operators */ {
                    match Op::from_str(val.clone()) {
                        Ok(Op::Unary(op)) => {
                            let sub = self.parse_subexpr(seperators)?.0;
                            Ok((Box::new(Expression(token.pos, ExpressionR::UnaryOp(op, sub, Op::get_precedence(Op::Unary(op))), None)), token))
                        },
                        Err(e) => {
                            errors.push(e);
                            Err(errors)
                        },
                        _ => {
                            errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "invalid operator {val}")));
                            Err(errors)
                        }
                    }
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

fn check(ast: &mut ASTNode, mut lexer: Lexer) -> Result<(HashMap<String, usize>, HashMap<String, Type>,), Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    
    // collect all aliases + functions
    let mut type_aliases: HashMap<String, Type> = HashMap::new();
    let mut functions: HashMap<String, (Vec<Type>, Type)> = HashMap::new();
    let mut vars: HashMap<String, Type> = HashMap::new();
    let mut globals: HashMap<String, usize> = HashMap::new();
    
    if let ASTNode(_, ASTNodeR::Block(arr)) = ast.clone() {
        for a in arr.clone() {
            match a {
                ASTNode(_, ASTNodeR::TypeAlias(alias, typ)) => {
                    type_aliases.insert(alias, typ);
                },
                ASTNode(_, ASTNodeR::FunctionDecl(name, args, ret_type, _)) => {
                    functions.insert(name, (args.into_iter().map(|(a, _)| a).collect(), ret_type));
                },
                ASTNode(_, ASTNodeR::Intrinsic(_, fname, args, rt)) => {
                    functions.insert(fname, (args.into_iter().map(|(a, _)|a).collect(), rt));
                }
                _ => {}
            }
        }
        for a in arr {
            match a {
                ASTNode(_, ASTNodeR::VarDec(_, tp, name)) => {
                    vars.insert(name.clone(), tp.clone());
                    globals.insert(name, tp.size(&type_aliases));
                },
                ASTNode(_, ASTNodeR::VarDecInit(_, tp, name, _)) => {
                    vars.insert(name.clone(), tp.clone());
                    globals.insert(name, tp.size(&type_aliases));
                },
                _ => {}
            }
        }
    } else {
        unreachable!();
    }

    fn check_references_expr(expr: &mut Expression, functions: &HashMap<String, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
        let Expression(pos, e, _) = expr;
        let tp = || -> Type {match e {
            ExpressionR::T(left, op, right, _) => {
                let left = check_references_expr(left, functions, vars, aliases, errors, lexer);
                let right = check_references_expr(right, functions, vars, aliases,  errors, lexer);
                
                match op.combine_type(&left, &right, aliases) {
                    Type::Invalid => {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{left}` and `{right}` for operation `{op}`")));
                        Type::Invalid
                    },
                    a => a,
                }
            },
            ExpressionR::UnaryOp(op, left, _) => {
                let left = check_references_expr(left, functions, vars, aliases, errors, lexer);
                
                match op.with_type(&left, aliases) {
                    Type::Invalid => {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible type `{left}` for operation `{op}`")));
                        Type::Invalid
                    },
                    a => a,
                }
            },
            ExpressionR::Val(tp, _) => {
                tp.clone()
            },
            ExpressionR::Var(name) => {
                if ! vars.contains_key(name) {
                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{name}`")));
                    return Type::Invalid;
                }
                vars.get(name).unwrap().clone()
            },
            ExpressionR::F(name, vec) => {
                if ! functions.contains_key(name) {
                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to function `{name}`")));
                    return Type::Invalid;
                }
                for a in vec {
                    check_references_expr(a, functions, vars, aliases, errors, lexer);
                }
                functions.get(name).unwrap().1.clone()
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
                Type::Array(Box::new(last_tp))
            },
            ExpressionR::Undef => {
                Type::Primitive(PrimitiveType::Void)
            },
            ExpressionR::ArrAlloc(tp, _) => {
                Type::Array(Box::new(tp.clone()))
            },
            ExpressionR::Index(ident, a) => {
                let inner_type = check_references_expr(a, functions, vars, aliases, errors, lexer);
                if ! Type::Primitive(PrimitiveType::Int).is_compatible(&inner_type, aliases) {
                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `int` and `{inner_type}` in array length definition")));
                    Type::Invalid
                } else {
                    let ident_type = check_references_expr(&mut Expression(*pos, ExpressionR::Var(ident.to_string()), None), functions, vars, aliases, errors, lexer);
                    if let Type::Array(inner) = ident_type {
                        *inner
                    } else if let Type::Custom(ref inner) = ident_type {
                        if aliases.contains_key(inner) {
                            if let Type::Array(ref inner2) = aliases[inner] {
                                return *inner2.clone();
//                                return aliases.get(inner).unwrap().clone();
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "`{ident_type}` is not an array type")));
                                Type::Invalid
                            }
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "`{ident_type}` is not an array type")));
                            Type::Invalid
                        }
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "`{ident_type}` is not an array type")));
                        Type::Invalid
                    }
                }
            }
        }}();
        expr.2 = Some(tp.clone());
        tp
    }
    
    // check
    fn check_references(node: &mut ASTNode, functions: &HashMap<String, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, f: String, lexer: &mut Lexer) {
        if let ASTNode(_, ASTNodeR::Block(ref mut arr)) = node {
            let vars_sub = &mut vars.clone();
            for a in arr {
                match a {
                    ASTNode(_, ASTNodeR::FunctionDecl(func, args, _, block)) => {
                        for a in args.clone() {
                            vars_sub.insert(a.1, a.0);
                        }
                        check_references(block, functions, vars_sub, aliases, errors, func.clone(), lexer);
                    },
                    ASTNode(pos, ASTNodeR::FunctionCall(name, args)) => {
                        if ! functions.contains_key(name) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to function `{name}`")));
                            continue;
                        }
                        
                        let func_args = &functions.get(name).unwrap().0;

                        let len_a = func_args.len();
                        let len_b = args.len();
                        if len_a != len_b {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                            continue;
                        };

                        for (index, a) in args.iter_mut().enumerate() {
                            let typa = check_references_expr(a, functions, vars_sub, aliases, errors, lexer);
                            let typb = func_args.get(index).unwrap().clone();

                            if ! typa.is_compatible(&typb, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                                break;
                            }
                        }
                    },
                    ASTNode(_, ASTNodeR::Block(..)) => {
                        check_references(a, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    ASTNode(pos, ASTNodeR::If(ref mut expr, ref mut block)) => {
                        let bool_type = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                        }
                        check_references(block, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    ASTNode(pos, ASTNodeR::While(ref mut expr, ref mut block)) => {
                        let bool_type = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                        }
                        check_references(block, functions, vars_sub, aliases, errors, f.clone(), lexer);
                    },
                    ASTNode(pos, ASTNodeR::Return(ref mut expr)) => {
                        let tp = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        let ret_type  = &functions.get(&f).unwrap().1;
                        if ! ret_type.is_compatible(&tp, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{ret_type}`, found `{tp}`, because of return type")));
                        }
                    },
                    ASTNode(_, ASTNodeR::ArrIndexInit(var, ref mut ind, ref mut expr)) => {
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
                            if let Type::Array(_) = type_l {} else if let Type::Custom(inner) = type_l {
                                if aliases.contains_key(inner) {
                                    if let Type::Array(_) = aliases.get(inner).unwrap() {} else {
                                        errors.push((ErrorLevel::Err, error!(lexer, a.0, "{type_l} used in array indexing is not an array type")));
                                    }
                                } else {
                                    errors.push((ErrorLevel::Err, error!(lexer, a.0, "{type_l} used in array indexing is not an array type")));
                                }
                            } else {
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
                    ASTNode(_, ASTNodeR::VarInit(var, ref mut expr)) => {
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
                    },
                    ASTNode(_, ASTNodeR::VarOp(var, op, ref mut expr)) => {
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

                            if Op::combine_type(&Op::Binary(*op), type_l, type_l, aliases) == Type::Invalid {
                                errors.push((ErrorLevel::Err, error!(lexer, a.0, "incompatible types `{type_l}` and `{type_r}` for operation `{op}`")));
                            }
                            
                        }
                    },
                    ASTNode(_, ASTNodeR::VarDec(_, tp, name)) => {
                        vars_sub.insert(name.clone(), tp.clone());
                    },
                    ASTNode(_, ASTNodeR::VarDecInit(_, tp, name, expr)) => {
                        let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if type_r == &Type::Invalid {
                            continue;
                        }
                        if ! tp.is_compatible(type_r, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, a.0, "incompatible types: expeected `{tp}`, found `{type_r}`")));
                        }
                        vars_sub.insert(name.clone(), tp.clone());
                    },
                    ASTNode(_, ASTNodeR::TypeAlias(_, _)) => {},
                    ASTNode(pos, ASTNodeR::Intrinsic(iname, ..)) => {
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
        Ok((globals, type_aliases))
    } else {
        Err(errors)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Inst {
    Func(String, HashMap<String, (usize, usize)>),
    VarSet(usize, usize, usize),
    If(usize, usize),
    Endif(usize),
    WhileCheck(usize, usize),
    WhileStart(usize, usize),
    Endwhile(usize),
    Ret(usize),
    Call(String),
    UnOp(usize, usize, UnaryOp),
    BinOp((usize, usize), usize, BinaryOp),
    Val(usize, Type, String),
    Var(usize, usize),
    RetVal(usize),
    Intrinsic(String, String),
    Push(usize),
    Pop(usize),
    Arr(usize, usize),
    Index(usize, usize, usize),
    ArraySet(usize, usize, usize),
    Global(usize, String),
    GlobalSet(usize, String, usize),
    Jump(String),
    GlobalArraySet(usize, usize, String),
}

fn intermediate_expr(expr: Expression, index: usize, indicies: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: &HashMap<String, Type>) -> Vec<Inst> {
    let mut ret = vec![];
    let Expression(_pos, expr, tp) = expr;
    match expr {
        ExpressionR::UnaryOp(op, fst, _) => {
            ret.append(&mut intermediate_expr(*fst, index, indicies, globals, aliases));
            ret.push(Inst::Push(index));
            ret.push(Inst::UnOp(index, tp.unwrap().size(aliases), op));
        },
        ExpressionR::T(fst, op, snd, _) => {
            ret.append(&mut intermediate_expr(*fst.clone(), index, indicies, globals, aliases));
            ret.push(Inst::Push(index));
            ret.append(&mut intermediate_expr(*snd, index+1, indicies, globals, aliases));
            ret.push(Inst::Pop(index));
            if let Op::Binary(op) = op {
                ret.push(Inst::BinOp((index, index+1), fst.2.unwrap().size(aliases), op));
            } else {
                unreachable!();
            }
        },
        ExpressionR::Val(tp, val) => {
            ret.push(Inst::Val(index, tp, val));
        },
        ExpressionR::Var(name) => {
            if indicies.contains_key(&name) {
                ret.push(Inst::Var(index, indicies.get(&name).unwrap().0));
            } else {
                ret.push(Inst::Global(index, name));
            }
        },
        ExpressionR::F(name, args) => {
            let mut ind = 0;
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, ind, indicies, globals, aliases));
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
            ret.push(Inst::Arr(index, sz * tp.size(aliases)));
        },
        ExpressionR::Index(var, expr) => {
            ret.append(&mut intermediate_expr(*expr, index, indicies, globals, aliases));
            if indicies.contains_key(&var) {
                ret.push(Inst::Var(index+1, indicies.get(&var).unwrap().0));
                ret.push(Inst::Index(index, index+1, indicies.get(&var).unwrap().1));
            } else {
                ret.push(Inst::Global(index+1, var.clone()));
                ret.push(Inst::Index(index, index+1, *globals.get(&var).unwrap()));
            }
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
     ("dereference", "\tmov rax, [rax]\n\tret\n"),
     ("set_ptr", "\tmov [rax], rbx\n\tret\n")
    ]
        .iter()
        .cloned()
        .collect()
}

fn intermediate(ast: ASTNodeR, offsets: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: HashMap<String, Type>, index:  usize, is_top_level: bool) -> (Vec<Inst>, HashMap<String, usize>) {
    let mut ret = vec![];

    if is_top_level {
        // check for global initializations
        if let ASTNodeR::Block(ref a) = ast {
            for inst in a {
                match inst.1 {
                    ASTNodeR::VarInit(ref name, ref expr) => {
                        ret.append(&mut intermediate_expr(expr.clone(), 0, offsets, globals, &aliases));
                        if ! offsets.contains_key(name) {
                            let len = globals.get(name).unwrap();
                            ret.push(Inst::GlobalSet(0, name.clone(), *len))
                        }
                    },
                    ASTNodeR::VarDecInit(_, _, ref name, ref expr) => {
                        ret.append(&mut intermediate_expr(expr.clone(), 0, offsets, globals, &aliases));
                        if ! offsets.contains_key(name) {
                            let len = globals.get(name).unwrap();
                            ret.push(Inst::GlobalSet(0, name.clone(), *len))
                        }
                    },
                    _ => {}
                }
            }
        }
        
        // call main function
        ret.push(Inst::Call("main".into()));
        ret.push(Inst::Jump("_end".into()));
    }
        
    match ast {
        ASTNodeR::Block(a) => {
            let mut count: usize = index;
            for i in a {
                count += 1;
                ret.append(&mut intermediate(i.1, offsets, globals, aliases.clone(), count, false).0);
            }
        },
        ASTNodeR::VarDec(is_global, tp, ref name) => {
            if ! is_global {
                offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
            }
        },
        ASTNodeR::ArrIndexInit(arr, ind, expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases));
            ret.append(&mut intermediate_expr(ind, 1, offsets, globals, &aliases));
            if offsets.contains_key(&arr) {
                ret.push(Inst::ArraySet(0, 1, offsets.get(&arr).unwrap().0))
            } else {
                ret.push(Inst::GlobalArraySet(0, 1, arr))
            }
        },
        ASTNodeR::VarDecInit(is_global, tp, ref name, expr) => {
            if ! is_global {
                offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
            }
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases));
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                ret.push(Inst::VarSet(0, vals.1, vals.0))
            } else {
                let len = globals.get(name).unwrap();
                ret.push(Inst::GlobalSet(0, name.clone(), *len))
            }
        },
        ASTNodeR::VarOp(ref name, op, expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases));
            
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                ret.push(Inst::Var(1, vals.0));
                ret.push(Inst::BinOp((0, 1), vals.1, op));
                ret.push(Inst::VarSet(0, vals.1, vals.0))
            } else {
                let len = globals.get(name).unwrap();
                ret.push(Inst::Global(1, name.into()));
                ret.push(Inst::BinOp((0, 1), *len, op));
                ret.push(Inst::GlobalSet(0, name.clone(), *len))
            }
        },
        ASTNodeR::VarInit(ref name, expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases));
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                ret.push(Inst::VarSet(0, vals.1, vals.0))
            } else {
                let len = globals.get(name).unwrap();
                ret.push(Inst::GlobalSet(0, name.clone(), *len))
            }
        },
        ASTNodeR::FunctionCall(name, args) => {
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, 0, offsets, globals, &aliases));
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
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases));
            ret.push(Inst::If(0, index));
            ret.append(&mut intermediate(block.1, offsets, globals, aliases, index + 1, false).0);
            ret.push(Inst::Endif(index));
        },
        ASTNodeR::While(cond, block) => {
            ret.push(Inst::WhileStart(0, index));
            ret.append(&mut intermediate_expr(cond, 0, offsets, globals, &aliases));
            ret.push(Inst::WhileCheck(0, index));
            ret.append(&mut intermediate(block.1, offsets, globals, aliases, index + 1, false).0);
            ret.push(Inst::Endwhile(index));
        },
        ASTNodeR::FunctionDecl(name, a, rt, block) => {
            let mut offsets: HashMap<String, (usize, usize)> = HashMap::new();
            for arg in a {
                offsets.insert(arg.1, (last_ptr(&offsets) + arg.0.size(&aliases), arg.0.size(&aliases)));
            }
            
            ret.push(Inst::Func(name, offsets.clone()));
            ret.append(&mut intermediate(block.1, &mut offsets, globals, aliases, index + 1, false).0);
            if rt == Type::Primitive(PrimitiveType::Void) {
                ret.push(Inst::Ret(0));
            }
        },
        ASTNodeR::Return(expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases));
            ret.push(Inst::Ret(0));
        },
        ASTNodeR::Intrinsic(iname, fname, _, _) => {
            ret.push(Inst::Intrinsic(iname, fname));
        },
        // ignore
        ASTNodeR::TypeAlias(..) => {},
        
    }
    (ret, globals.clone())
}

fn gnereate(insts: Vec<Inst>, globals: &HashMap<String, usize>) -> String {

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
    ".into();

    let mut intrinsic_labels: Vec<String> = vec![];

    let mut arr_index: usize = 0;
    
    for a in insts {
        ret.push_str(match a {
            Inst::Func(name, offsets) => {
                let string = (0..offsets.len()).map(|a| format!("\tsub rsp, {ind}\n\tmov [rbp-{ind}], {}\n", register_sz(a, offsets.iter().nth(a).unwrap().1.1), ind = offsets.iter().nth(a).unwrap().1.0)).collect::<String>();
                format!("{name}:\n\tpush rbp\n\tmov rbp,rsp\n{string}")
            },
            Inst::Call(name) => {
                format!("\tcall {name}\n")
            },
            Inst::If(reg, id) => {
                format!("\tcmp {}, 1\n\tjne .l2_{id}\n.l1_{id}:\n", register_sz(reg, 1))
            },
            Inst::Endif(id) => {
                format!(".l2_{id}:\n")
            },
            Inst::WhileCheck(reg, id) => {
                format!("\tcmp {}, 1\n\tjne .while_{id}_end\n", register(reg))
            },
            Inst::WhileStart(_, id) => {
                format!(".while_{id}:\n")
            }
            Inst::Endwhile(id) => {
                format!("\tjmp .while_{id}\n.while_{id}_end:\n")
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
                            PrimitiveType::Unchecked => val,
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
            Inst::UnOp(reg, sz, op) => {
                match op {
                    UnaryOp::Not => format!("\ttest {r}, {r}\n\tsetz {r}\n", r = register_sz(reg, sz)),
                }
            },
            Inst::BinOp(reg, sz, op) => {
                match op {
                    BinaryOp::Add => format!("\tadd {}, {}\n",  register(reg.0), register(reg.1)),
                    BinaryOp::Sub => format!("\tsub {}, {}\n",  register(reg.0), register(reg.1)),
                    BinaryOp::Mul => format!("\timul {}, {}\n", register(reg.0), register(reg.1)),
                    BinaryOp::Div => format!("\tidiv {}, {}\n", register(reg.0), register(reg.1)),
                    BinaryOp::Eq  => format!("\tcmp {r0}, {r1}\n\tsete al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::Less => format!("\tcmp {r0}, {r1}\n\tsetl al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::Greater => format!("\tcmp {r0}, {r1}\n\tsetg al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::Mod => format!("\tmov rax, {r0}\n\tidiv {r1}\n\tmov {r0}, rdx\n", r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz)),
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
                    format!("{fname}: \n\tjmp intrinsic_{iname}\nintrinsic_{iname}: \n{}\n", intrinsics().get(iname.as_str()).unwrap())
                } else {
                    format!("{fname}: \n\tjmp intrinsic_{iname}\n")
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
            Inst::Index(reg0, reg1, sz) => {
                let r0 = register_sz(reg0, sz);
                let r1 = register_sz(reg1, sz);
                format!("\tadd {r1}, 8\n\tadd {r1}, {r0}\n\tmov {r0}, [{r1}]\n")
            },
            Inst::ArraySet(reg0, reg1, ind) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                format!("\tpush {r0}\n\tmov {r0}, [rbp-{ind}]\n\tadd {r0}, 8\n\tadd {r0}, {r1}\n\tpop {r1}\n\tmov [{r0}], {r1}\n")
            },
            Inst::Global(reg0, name) => {
                format!("\tmov {}, [global_{name}]\n", register(reg0))
            },
            Inst::GlobalSet(reg0, name, sz) => {
                format!("\tmov {tp} [global_{name}], {}\n", register_sz(reg0, sz), tp = datatype(sz))
            },
            Inst::Jump(name) => {
                format!("\tjmp {name}\n")
            },
            Inst::GlobalArraySet(reg0, reg1, arr) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                format!("\tpush {r0}\n\tmov {r0}, [global_{arr}]\n\tadd {r0}, 8\n\tadd {r0}, {r1}\n\tpop {r1}\n\tmov [{r0}], {r1}\n")
            },
            
        }.as_str())
    }
    ret.push_str("_end:\n\tmov rdi, rax\n\tmov rax, 60\n\tsyscall\nsection .data\n");

    for a in global_strings {
        ret.push_str(format!("str_{}:\n\tdq {}\n\tdb `{a}`\n", a.to_lowercase().chars().map(|a| if a.is_alphanumeric() {a} else {'_'}).collect::<String>(), a.len() - a.matches("\\").count()).as_str());
    }

    for (ind, a) in global_arrays.into_iter().enumerate() {
        ret.push_str(format!("arr_{}:\n\tdq {a}\n\tresb {a}\n", ind).as_str());
    }

    for (val, sz) in globals {
        ret.push_str(format!("global_{val}:\n\tresb {sz}\n").as_str());
    }
    
    ret
}

fn main() {
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

    let path: String = filename.chars().rev().skip_while(|x| x != &'/').collect::<String>().chars().rev().collect();
    let name: String = filename.chars().rev().skip_while(|x| x != &'.').skip(1).take_while(|x| x != &'/').collect::<String>().chars().rev().collect();
    println!("PATH: {path}, NAME: {name}");

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
    let mut globals: HashMap<String, usize> = HashMap::new();
    
    if let Ok(ref mut ast) = a {
        eprintln!("--- END PARSING ---\nAST NODE: \n{:#?}", ast);
        eprintln!("--- AST (converted back to code) ---\n{}\n", ast);
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
            Ok((g, a)) => {
                aliases = a;
                globals = g;
                checked = true
            },
        };
    } else {
        checked = false;
    }

    if checked {
        eprintln!("--- END CHECKING ---\n");
        if let Ok(ast) = a {
            let (intermediate, globals) = intermediate(ast.1, &mut HashMap::new(), &globals, aliases, 0, true);
            println!("--- START INTERMEDIATE REPRESANTATION ---");
            println!("{:#?}", intermediate);
            println!("--- END INTERMEDIATE REPRESANTATION ---");

            let asm = gnereate(intermediate, &globals);
            println!("--- START ASSEMBLY ---");
            println!("{}", asm);
            println!("--- END ASSEMBY ---");

            let mut asm_path = path.clone();
            asm_path.push_str(&name);
            asm_path.push_str(".asm");
            match fs::write(asm_path.clone(), asm) {
                Ok(_) => {},
                Err(a) => {
                    eprintln!("Error writing file: {}", a);
                    exit(1);
                },
            };

            // call nasm and ld
            let nasm =  Command::new("nasm").arg("-felf64").arg(asm_path).output().unwrap();
            if ! nasm.status.success() {
                eprintln!("ERROR executing nasm: \n{}", std::str::from_utf8(&nasm.stderr).unwrap());
                error = true;
            }

            let mut obj_path = path.clone();
            obj_path.push_str(&name);
            obj_path.push_str(".o");

            let mut outfile = path.clone();
            outfile.push_str(&name);
            let ld = Command::new("ld").arg("-o").arg(outfile).arg(obj_path).output().unwrap();
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


