use std::{fs, env::args, process::{exit, Command}, io, collections::HashMap,
          time::SystemTime, hash::Hash};
use std::fmt::Write as _;

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
    "break",
    "struct",
    "ref",
    "deref",
    "include",
    "from"
];

#[derive(Debug, Clone, Eq)]
enum Type {
    Primitive(PrimitiveType),
    Custom(String),
    Array(Box<Type>/*, u32*/),
    Pointer(Box<Type>),
    Invalid,
    Struct(String, HashMap<String, (Type, usize)>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(l0), Self::Primitive(r0)) => l0 == r0,
            (Self::Custom(l0), Self::Custom(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::Pointer(l0), Self::Pointer(r0)) => l0 == r0,
            (Self::Struct(l0, l1), Self::Struct(r0, r1)) => l0 == r0 && l1 == r1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Type::Primitive(a) => a.hash(state),
            Type::Custom(a) => a.hash(state),
            Type::Array(a) => a.hash(state),
            Type::Pointer(a) => a.hash(state),
            Type::Invalid => 0.hash(state),
            Type::Struct(x, map) => {
                map.iter().map(|(a, b)| (a.clone(), b.clone()))
                    .collect::<Vec<(String, (Type, usize))>>().hash(state);
                x.hash(state);
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            Type::Struct(name, fields) => {
                // convert hashmap to vector
                let mut vector_: Vec<String> = fields.iter()
                    .map(|(x, _)| x.clone()).collect();
                vector_.sort();
                let vector: Vec<(String, Type)> = vector_.iter()
                    .map(|x| (x.clone(), fields[x].0.clone())).collect();


                let mut s = format!("struct {name} {{");
                for (i, (n, t)) in vector.iter().enumerate() {
                    let _ = write!(s, "{t} {n}");
                    if i < vector.len() - 1 {
                        s += ", ";
                    }
                }
                s += "}";
                s
            },
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

    fn is_compatible(&self, type_r: &Type, aliases: &HashMap<String, Type>)
                     -> bool {
        let a = self.dealias(aliases);
        let b = type_r.dealias(aliases);

        // UNSAFE: DON'T USE THIS IF NOT REALLY NEEDED
        if a == Type::Primitive(PrimitiveType::Unchecked) ||
            b == Type::Primitive(PrimitiveType::Unchecked) {
            return true;
        }

        if a == b || a == Type::Primitive(PrimitiveType::Float) &&
            b == Type::Primitive(PrimitiveType::Int) {
            true
        } else if let Type::Array(aa) = a {
            if let Type::Array(bb) = b {
                aa.is_compatible(&bb, aliases)
            } else {
                false
            }
        } else if let Type::Pointer(aa) = a {
            if let Type::Pointer(bb) = b {
                aa.is_compatible(&bb, aliases)
            } else {
                false
            }
        } else if let Type::Struct(a_name, a_fields) = a {
            if let Type::Struct(b_name, b_fields) = b {
                if a_name == b_name {
                    let mut comp = true;
                    for (string, typ) in a_fields.iter() {
                        if ! b_fields.contains_key(string) {
                            // TODO: Error
                            return false;
                        }
                        comp &= typ.0.is_compatible(&b_fields[string].0,
                                                    aliases);
                    }
                    for (string, _) in b_fields {
                        if ! a_fields.contains_key(&string) {
                            // TODO: Error
                            return false;
                        }
                    }
                    comp
                } else {
                    false
                }
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
            Type::Array(_) => 8,             // 4 <- size, 8 <- pointer
            Type::Pointer(_) => 8,
            Type::Invalid => 0,
            Type::Struct(_, fields) => {
                let mut size: usize = 0;
                for (_, t) in fields.iter() {
                    size += t.0.size(aliases);
                }
                size
            },
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
            BinaryOp::Add          => "+",
            BinaryOp::Sub          => "-",
            BinaryOp::Mul          => "*",
            BinaryOp::Div          => "/",
            BinaryOp::Eq           => "==",
            BinaryOp::Less         => "<",
            BinaryOp::LessEq       => "<=",
            BinaryOp::Greater      => ">",
            BinaryOp::GreaterEq    => ">=",
            BinaryOp::Mod          => "%",
            BinaryOp::BoolAnd      => "&&",
            BinaryOp::BoolOr       => "||",
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
                Type::Primitive(PrimitiveType::Bool | PrimitiveType::Unchecked)
                    => Type::Primitive(PrimitiveType::Bool),
                _ => Type::Invalid
            },
        }
    }
}


impl Op {
    fn combine_type(&self, a: &Type, b: &Type, aliases: &HashMap<String, Type>)
                    -> Type {
        match self {
            Op::Unary(_) => {
                unreachable!()
            },
            Op::Binary(bin) => match bin {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div |
                BinaryOp::Mod => {
                    match a {
                        Type::Primitive(ref v) => {
                            match v {
                                PrimitiveType::Int =>
                                    if a.is_compatible(b, aliases) {
                                        a.clone()
                                    } else {
                                        Type::Invalid
                                    },
                                PrimitiveType::Float =>
                                    if a.is_compatible(b, aliases) {
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
                BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEq |
                BinaryOp::GreaterEq => match a {
                    Type::Primitive(ref p) => match p {
                        PrimitiveType::Int | PrimitiveType::Float |
                        PrimitiveType::Char => if a.is_compatible(b, aliases) {
                            Type::Primitive(PrimitiveType::Bool)
                        } else {
                            Type::Invalid
                        },
                        _ => Type::Invalid,
                    },
                    _ => Type::Invalid,
                },
                BinaryOp::Eq => if (a == b) ||
                    (a == &Type::Primitive(PrimitiveType::Unchecked) ||
                     b == &Type::Primitive(PrimitiveType::Unchecked)) {
                    Type::Primitive(PrimitiveType::Bool)
                } else{
                    Type::Invalid
                },
                BinaryOp::BoolAnd | BinaryOp::BoolOr =>
                    if (a == b) && *a == Type::Primitive(PrimitiveType::Bool) {
                        Type::Primitive(PrimitiveType::Bool)
                    } else {
                        Type::Invalid
                    }
            }
        }
    }
}

fn is_valid_type(val: &str) -> bool {
    // TODO: check for custom types
    val == "int" || val == "float" || val == "char" ||
        val == "void" || val == "bool" || val == "unchecked"
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
                Some(Type::Pointer(Box::new(
                    parse_type(string.as_str()[0..string.len()-1].into())?)))
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
            return format!("{file}:{line}:{ch}: {msg}",
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
                    if val.len() == 2 && val.starts_with('\\') && ch == '\'' {
                        val = match val.chars().nth(1).unwrap() {
                            'n' => "\n",
                            'r' => "\r",
                            't' => "\t",
                            '\\' => "\\",
                            _ => {
                                return Err((ErrorLevel::Err,
                                    error!(self, self.pos,
                                           "invalid escape literal: '{val}'")));
                            }
                        }.to_string();
                        self.pos += 1;
                        return Ok(Token {
                            pos: token_pos,
                            ttype,
                            value: val,
                        })
                    }
                    
                    if val.len() == 1 && val.starts_with('\\') {
                        val.push(ch);
                        self.pos += 1;
                        continue;
                    }
                    if !val.is_empty() {
                        return Err((ErrorLevel::Err,
                                    error!(self, self.pos,
                                           "invalid character literal: {val}")));
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
                            ttype = check_keywords_and_types(val.as_str(),
                                                             ttype);

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
                            ttype = check_keywords_and_types(val.as_str(),
                                                             ttype);

                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    'a'..='z'|'_'|'A'..='Z' => {
                        if ttype == TokenType::Undef ||
                            ttype == TokenType::Ident {
                            ttype = TokenType::Ident;
                            val.push(ch);
                        } else {
                            // check for keywords / types
                                ttype = check_keywords_and_types(
                                    val.as_str(), ttype);

                            if val == "." {
                                ttype = TokenType::Special;
                            }

                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    '0'..='9'|'.' => {
                        if ttype == TokenType::Float ||
                            ((ttype == TokenType::Int) && ch == '.') {
                                if ttype != TokenType::Float && self.pos + 1 < self.source.len() && ! self.source[self.pos+1].is_numeric() {
                                    return Ok(Token {
                                        pos: token_pos,
                                        ttype,
                                        value: val,
                                    });
                                }
                                ttype = TokenType::Float;
                                val.push(ch);
                            } else if ttype == TokenType::Undef ||
                            ttype == TokenType::Int {
                                ttype = TokenType::Int;
                                val.push(ch);
                            } else if ttype == TokenType::Ident && ch != '.' {
                                ttype = TokenType::Ident;
                                val.push(ch);
                            } else if ttype == TokenType::Operator &&
                            val == "-" {
                                val.push(ch);
                                ttype = TokenType::Int;
                            } else {
                                // check for keywords / types
                                ttype = check_keywords_and_types(
                                    val.as_str(), ttype);

                                if val == "." {
                                    ttype = TokenType::Special;
                                }

                                return Ok(Token {
                                    pos: token_pos,
                                ttype,
                                    value: val,
                                });
                            }
                    },
                    '!'|'='|'+'|'-'|'*'|'/'|'<'|'>'|'%'|'|'|'&' => {
                        if ttype == TokenType::Type && ch == '*' {
                            val.push(ch);
                            ttype = TokenType::Type;
                        } else if ttype == TokenType::Operator &&
                            val == "/" && ch == '/' {
                                // comment
                                loop {
                                    if self.source[self.pos] == '\n' {
                                        break;
                                    }
                                    self.pos += 1;
                                }
                                return self.next_token();
                            } else if ttype == TokenType::Operator &&
                            val == "/" && ch == '*' {
                                // multi-line comment
                                loop {
                                    if self.pos < 2 {
                                        self.pos += 1;
                                        continue;
                                    }
                                    if self.source[self.pos-1] == '*' &&
                                        self.source[self.pos] == '/' {
                                            self.pos += 1;
                                            break;
                                        }
                                    self.pos += 1;
                                }
                                return self.next_token();
                            } else if (ttype == TokenType::Operator &&
                                       val == "-" && ch == '>') ||
                            (ttype == TokenType::Operator &&
                             val.starts_with('<') && ch == '-') {
                                val.push(ch);
                                ttype = TokenType::Special;
                            } else if ttype == TokenType::Undef ||
                            ttype == TokenType::Operator {
                                ttype = TokenType::Operator;
                                val.push(ch);
                            } else {
                                // check for keywords / types
                                ttype = check_keywords_and_types(
                                    val.as_str(), ttype);

                                return Ok(Token {
                                    pos: token_pos,
                                    ttype,
                                    value: val,
                                });
                            }
                    },
                    ';' | '(' | ')' | ',' | '{' | '}' | '[' | ']' | ':' => {
                        if ttype == TokenType::Undef {
                            ttype = TokenType::Special;
                            val.push(ch);
                        } else if ttype == TokenType::Special &&
                            !val.contains(')') && !val.contains('(') &&
                            !val.contains('[') && !val.contains(']') &&
                            !val.contains('}') && !val.contains('{') &&
                            ch != ';' {
                            val.push(ch);
                        } else {
                            // check for keywords / types
                                ttype = check_keywords_and_types(
                                    val.as_str(), ttype);

                            return Ok(Token {
                                pos: token_pos,
                                ttype,
                                value: val,
                            });
                        }
                    },
                    ' '|'\t'|'\n' => {
                        if ttype != TokenType::Undef {
                            // check for keywords / types
                            ttype = check_keywords_and_types(
                                val.as_str(), ttype);

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
                        return Err((ErrorLevel::Err,
                                    error!(self, self.pos,
                                           "invalid character `{ch}`")))
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

#[derive(Debug, Clone)]
struct ASTNode(usize, ASTNodeR);

#[derive(Debug, Clone)]
enum ASTNodeR {
    Block(Vec<ASTNode>),
    VarDec(bool, Type, String),
    VarInit(String, Expression, Option<Type>),
    VarOp(String, BinaryOp, Expression),
    VarDecInit(bool, Type, String, Expression),
    FunctionCall(String, Vec<Expression>),
    If(Expression, Box<ASTNode>),
    FunctionDecl(Option<Type>, String, Vec<(Type, String)>, Type, Box<ASTNode>),
    Return(Expression),
    TypeAlias(String, Type),
    Intrinsic(String, String, Vec<(Type, String)>, Type),
    ArrIndexInit(Expression, Expression, Expression, Vec<usize>),
    While(Expression, Box<ASTNode>),
    Struct(String, HashMap<String, (Type, usize)>),
    SetField(Expression, String, Expression, Option<Type>),
    Include(String, Box<ASTNode>, Lexer),
    Break(),
    MemberFunction(Expression, String, Vec<Expression>),
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
    Index(Box<Expression>, Box<Expression>, Vec<usize>),
    UnaryOp(UnaryOp, Box<Expression>, u32),
    StructLiteral(String, HashMap<String, Expression>),
    StructField(Box<Expression>, String, Option<Type>),
    Ref(Box<Expression>),
    Deref(Box<Expression>),
    MemberFunction(Box<Expression>, String, Vec<Expression>),
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
    BoolAnd,
    BoolOr,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum UnaryOp {
    Not,
}

// indents a strinig by 4 spaces
fn indent(string: String) -> String {
    let mut ret = String::new();
    for line in string.lines() {
        ret += "    ";
        ret += line;
        ret += "\n";
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
            ASTNodeR::VarInit(name, expr, _) => {
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
            ASTNodeR::FunctionDecl(_, name, args, ret_type, block) => {
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
            ASTNodeR::ArrIndexInit(array, index, value, _) => {
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
            ASTNodeR::Struct(name, fields) => {
                writeln!(f, "struct {name} {{")?;
                for (typ, (name, _)) in fields {
                    writeln!(f, "    {typ} {name};")?;
                }
                write!(f, "}}")
            },
            ASTNodeR::SetField(left, field, right, _) => {
                write!(f, "{left}.{field} = {right};")
            },
            ASTNodeR::Include(file, _, _) => {
                write!(f, "include \"{file}\";")
            },
            ASTNodeR::Break() => {
                write!(f, "break;")
            }
            ASTNodeR::MemberFunction(lexpr, name, args) => {
                write!(f, "{lexpr}.{name}(")?;
                for (index, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if index < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ");")
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
                        Type::Primitive(PrimitiveType::Char) =>
                            write!(f, "\"{val}\""),
                        _ => write!(f, "{{{val}}}[INVALID]"),
                    },
                    Type::Pointer(_) => write!(f, "{val}*[INVALID]"),
                    Type::Invalid => write!(f, "[INVALID]"),
                    Type::Struct(name, _) =>
                        write!(f,
                               "{name}[INVALID, SHOULD BE A STRUCT LITERAL]"),
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
            ExpressionR::MemberFunction(lexpr, name, args) => {
                write!(f, "{lexpr}.{name}(")?;
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
            ExpressionR::Index(array, index, _) => {
                write!(f, "{array}[{index}]")
            },
            ExpressionR::UnaryOp(op, expr ,_) => {
                write!(f, "{op}{expr}")
            },
            ExpressionR::StructLiteral(name, fields) => {
                writeln!(f, "{name} {{")?;
                for (name, expr) in fields {
                    writeln!(f, "    {name}: {expr};")?;
                }
                write!(f, "}}")
            },
            ExpressionR::StructField(expr, field,_) => {
                write!(f, "{expr}.{field}")
            },
            ExpressionR::Ref(expr) => {
                write!(f, "ref {expr}")
            },
            ExpressionR::Deref(expr) => {
                write!(f, "deref {expr}")
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
            "<=" => Ok(Op::Binary(BinaryOp::LessEq)),
            ">=" => Ok(Op::Binary(BinaryOp::GreaterEq)),
            "&&" => Ok(Op::Binary(BinaryOp::BoolAnd)),
            "||" => Ok(Op::Binary(BinaryOp::BoolOr)),
            "!"  => Ok(Op::Unary(UnaryOp::Not)),
            _ => Err((ErrorLevel::Fatal,
                     format!("FATAL: invalid operator `{s}` set from lexer"))),
        }
    }

    fn get_precedence(op_type: Op) -> u32 {
        match op_type {
            Op::Binary(bin_op) => match bin_op {
                BinaryOp::BoolAnd | BinaryOp::BoolOr => 0,
                BinaryOp::Eq | BinaryOp::Less |
                BinaryOp::Greater | BinaryOp::GreaterEq | BinaryOp::LessEq => 1,
                BinaryOp::Add | BinaryOp::Sub => 2,
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 3,
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
                _ = $lexer.next_token();
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

    fn parse(&mut self, verbose: usize) -> Result<ASTNode, Vec<Error>> {
        self.parse_block_(true, 0, verbose)
    }

    fn parse_block(&mut self, pos: usize, verbose: usize)
                   ->Result<ASTNode, Vec<Error>> {
        self.parse_block_(false, pos, verbose)
    }

    fn parse_block_(&mut self, is_root: bool, pos: usize, verbose: usize)
                    ->Result<ASTNode, Vec<Error>> {
        let mut block: ASTNode = ASTNode(pos, ASTNodeR::Block(vec![]));
        let mut errors: Vec<Error> = vec![];
        loop {
            match block {
                ASTNode(_, ASTNodeR::Block(ref mut vec)) => {
                    let bs = match self.parse_block_statement(
                        is_root, verbose) {
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
                                        if a.ttype == TokenType::Eof ||
                                            (a.ttype == TokenType::Special &&
                                             (a.value == ";" || a.value == "{"))
                                        {
                                            break;
                                        } else if a.ttype == TokenType::Special
                                            && a.value == "}" {
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

    fn parse_expr(&mut self, seperators: &[&'static str])
                  -> Result<(Expression, Token), Vec<Error>> {
        let mut errors: Vec<Error> = vec![];
        let check_precedence_and_update =
            |op: Op, right_expr: Box<Expression>, working_expr: &Expression|
                                     -> Box<Expression> {
            let ret: Box<Expression>;
            let Expression(_, exp, _) = working_expr;
            let is_first: bool = match exp {
                ExpressionR::UnaryOp(_, _, precedence) => {
                    Op::get_precedence(op) <= *precedence
                },
                ExpressionR::T(_, _, _, precedence) => {
                    Op::get_precedence(op) <= *precedence
                },
                ExpressionR::Val(..) | ExpressionR::Arr(..) |
                ExpressionR::F(..) | ExpressionR::Var(..) |
                ExpressionR::ArrAlloc(..) | ExpressionR::Index(..) |
                ExpressionR::StructLiteral(..) | ExpressionR::StructField(..) |
                ExpressionR::Ref(..) | ExpressionR::Deref(..) |
                ExpressionR::MemberFunction(..) => {
                    true
                },
                ExpressionR::Undef => unreachable!(),
            };

            if is_first {
                let tmp_expr =
                    Expression(working_expr.0,
                               ExpressionR::T(Box::new(working_expr.clone()),
                                              op, right_expr,
                                              Op::get_precedence(op)), None);
                ret = Box::new(tmp_expr);
            } else {
                assert!(matches!(*working_expr,
                                 Expression(_, ExpressionR::T(..), _)));
                let mut expr: Expression = working_expr.clone();
                let tmp_expr =
                    if let Expression(pos,ExpressionR::T(_, _, right, _), _) =
                    &expr {
                        Expression(*pos, ExpressionR::T(right.clone(),
                                                        op, right_expr,
                                                        Op::get_precedence(op)),
                                   None)
                } else {
                    unreachable!();
                };

                if let Expression(pos, ExpressionR::T(a, b, _, d), _) = expr {
                    expr = Expression(pos,ExpressionR::T(a, b,
                                                         Box::new(tmp_expr),
                                                         d), None);
                }

                ret = Box::new(expr);
            }

            ret
        };
        let mut left_expr;

        // parse left side
        let subexpr = self.parse_subexpr(seperators)?;
        let mut nleft_expr = subexpr.0;
        if let ExpressionR::Undef =  nleft_expr.1  {
            return Ok((*nleft_expr, subexpr.1));
        }


        loop {
            if let Expression(_, ExpressionR::Undef, _) = *nleft_expr {
                return Ok((*nleft_expr, subexpr.1));
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
            } else if tk_op.ttype == TokenType::Eof ||
                (tk_op.ttype == TokenType::Special &&
                 (seperators.contains(&tk_op.value.as_str()) || tk_op.value == ".")) {
                    self.lexer.next_token().unwrap();
                    return Ok((*nleft_expr, tk_op));
            } else {
                    let val = tk_op.clone().value;
                    errors.push((ErrorLevel::Err, error!(
                        self.lexer, tk_op.pos, "invalid token `{val}`")));
                    return Err(errors);
                };


            self.lexer.next_token().unwrap();

            // parse right side
            let subexpr_r = self.parse_subexpr(seperators)?;

            if let Expression(_, ExpressionR::Undef, _) = *subexpr_r.0 {
                return Ok((*left_expr, subexpr.1));
            }

            let right_expr = subexpr_r.0;
            nleft_expr = check_precedence_and_update(
                op, right_expr.clone(), &mut left_expr);
        }
    }

    fn parse_block_statement(&mut self, is_root: bool, verbose: usize)
                             -> Result<Option<ASTNode>, Vec<Error>> {
        // save position for type parsing
        let start_pos = self.lexer.pos;

        let mut errors: Vec<Error> = vec![];
        let mut token = get_token!(self.lexer, errors);
        let val = token.clone().value;


        fn parse_var_dec(parser: &mut Parser, typ: Type, is_root: bool)
                         -> Result<Option<ASTNode>, Vec<Error>> {
            // declare variable
            let pos = parser.lexer.pos;
            let mut errors: Vec<Error> = vec![];

            let next_token = err_ret!(parser.expect(
                Some(TokenType::Ident), None), errors);
            let ident = next_token.value;

            // check next token
            let next_token = err_ret!(parser.lexer.next_token(), errors);
            let ntv = next_token.value.clone();
            match next_token.ttype {
                TokenType::Special => {
                    if next_token.value == ";" {
                        Ok(Some(ASTNode(pos, ASTNodeR::VarDec(
                            is_root, typ, ident))))
                    } else {
                        Err(vec![(ErrorLevel::Err, error!(
                            parser.lexer, next_token.pos,
                            "unexpected token `{ntv}`"))])
                    }
                },
                TokenType::Operator => {
                    if next_token.value == "=" {
                        // variable initialization
                        let expr = parser.parse_expr(&[";"])?.0;
                        Ok(Some(ASTNode(pos, ASTNodeR::VarDecInit(
                            is_root, typ, ident, expr))))
                    } else {
                        Err(vec![(ErrorLevel::Err, error!(
                            parser.lexer, next_token.pos,
                            "unexpected token `{ntv}`"))])
                    }
                },
                _ => Err(vec![(ErrorLevel::Err, error!(
                    parser.lexer, next_token.pos,
                    "unexpected token `{ntv}`"))]),
            }
        }


        // may be a later defined type
        if token.ttype == TokenType::Ident &&
            self.lexer.peek_token().is_ok() &&
            (self.lexer.peek_token().unwrap().ttype == TokenType::Ident ||
             self.lexer.peek_token().unwrap().value == "*") {
                token.ttype = TokenType::Type;
        }

        type Args = Vec<(Type, String)>;

        fn parse_function_decl(parser: &mut Parser,
                               errors: &mut Vec<(ErrorLevel, String)>)
                               -> Result<(Args, Type), Vec<Error>> {
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

                    let nval = err_break!(parser.expect(Some(TokenType::Ident)
                                                        , None), errors).value;
                    args.insert(args.len(), (t, nval));

                    let token = get_token!(parser.lexer, errors);
                    let val = token.clone().value;
                    if token.ttype != TokenType::Special {
                        errors.push((
                            ErrorLevel::Err,error!(
                                parser.lexer,token.pos,
                                "unexpected token `{val}`, `,` or `)` expect"
                            )));
                        return Err(errors.clone());
                    }

                    if val == "," {
                        continue;
                    } else if val == ")" {
                        break;
                    } else {
                        errors.push((
                            ErrorLevel::Err, error!(
                                parser.lexer, token.pos,
                                "unexpected token `{val}`, `,` or `)` expect"
                            )));
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
            if next_token.ttype == TokenType::Special &&
                next_token.value == "->" {
                parser.lexer.next_token().unwrap();
                // expect return type

                let tp = match parser.parse_type() {
                    Ok(a) => a,
                    Err(e) => {
                        errors.push(e);
                        return Err(errors.clone());
                    },
                };
                ret_type = tp;
            }

            Ok((args, ret_type))
        }


        match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Char |
            TokenType::String | TokenType::Bool => {
                errors.push((ErrorLevel::Err,
                             error!(self.lexer, token.pos, "invalid literal")));
                return self.parse_block_statement(is_root, verbose);
            },
            TokenType::Ident => {
                let expr = self.parse_ident_expr(val.clone(),
                                                 token.pos,  &[";"])?;
                let exp = self.parse_member(*expr, &[";"])?.1;

                match exp {
                    ExpressionR::Var(..) => {
                        let var_token = get_token!(self.lexer, errors);
                        let var_val = var_token.value;

                        if var_token.ttype == TokenType::Operator &&
                            var_val == "=" {
                                let expr = self.parse_expr(&[";"])?.0;
                                return Ok(Some(ASTNode(
                                    token.pos,ASTNodeR::VarInit(
                                        val, expr, None))));
                            } else if var_token.ttype == TokenType::Operator &&
                            var_val.ends_with('=') {
                                let expr = self.parse_expr(&[";"])?.0;
                                let op = err_ret!(
                                    Op::from_str(
                                        var_val.as_str()
                                            [..(var_val.len()-1)]
                                            .into()), errors);
                                if let Op::Binary(bop) = op {
                                    return Ok(Some(ASTNode(
                                        token.pos,
                                        ASTNodeR::VarOp(val, bop, expr))));
                                }
                            } else if var_token.ttype == TokenType::Special &&
                            var_val == "[" {
                                // array indexing
                                let index = self.parse_expr(&["]"])?.0;
                                err_ret!(self.expect(Some(TokenType::Operator),
                                                     Some("=".into())), errors);
                                let right_expr = self.parse_expr(&[";"])?.0;
                                let left_expr =
                                    Expression(token.pos,
                                               ExpressionR::Var(val), None);
                                return Ok(Some(
                                    ASTNode(
                                        token.pos,
                                        ASTNodeR::ArrIndexInit(left_expr,
                                                               index,
                                                               right_expr,
                                                               vec![])
                                    )
                                ));
                        } else {
                                errors.push((
                                    ErrorLevel::Err,
                                    error!(self.lexer,
                                           token.pos,
                                           "unexpected token `{var_val}`"
                                    )
                                ));
                        }
                    },
                    ExpressionR::StructLiteral(..) => unreachable!(),
                    ExpressionR::StructField(strct, field, _) => {
                        err_ret!(self.expect(Some(TokenType::Operator),
                                             Some("=".into())), errors);
                        let expr = self.parse_expr(&[";"])?.0;
                        return Ok(Some(ASTNode(token.pos, ASTNodeR::SetField(*strct, field, expr, None))));
                    },
                    ExpressionR::MemberFunction(expr, name, args) => {
                        return Ok(Some(ASTNode(token.pos, ASTNodeR::MemberFunction(*expr, name, args))));
                    },
                    ExpressionR::Val(..) => unreachable!(),
                    ExpressionR::Ref(..) => unreachable!(),
                    ExpressionR::Deref(..) => unreachable!(),
                    ExpressionR::UnaryOp(_, _, _) => unreachable!(),
                    ExpressionR::T(..) => unreachable!(),
                    ExpressionR::Arr(..) => unreachable!(),
                    ExpressionR::Index(a, ind,_) => {
                        let var_token = get_token!(self.lexer, errors);
                        let var_val = var_token.value;

                        if var_token.ttype == TokenType::Operator && var_val == "=" {
                            let expr = self.parse_expr(&[";"])?.0;
                            return Ok(Some(ASTNode(token.pos, ASTNodeR::ArrIndexInit(*a, *ind, expr, vec![]))));
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
                    match self.parse_block(token.pos, verbose) {
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
                    return self.parse_block_statement(is_root, verbose);
                }
                _ => {},
            },
            TokenType::Operator => {},
            TokenType::Type => {
                
                self.lexer.pos = start_pos;
                let typ = err_ret!(self.parse_type(), errors);
                
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

                    let predicate = self.parse_expr(&[")"])?;

                    // next token: '{'
                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors);
                    
                    let block = self.parse_block(token.pos, verbose)?;

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::If(predicate.0, Box::new(block)))));
                    
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
                    let block = self.parse_block(token.pos, verbose)?;
                    
                    return Ok(Some(ASTNode(token.pos, ASTNodeR::While(condition, Box::new(block)))));
                },
                "break" => {
                    if is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "`if` not allowed at top level")));
                        return Err(errors);
                    }
                    err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors);
                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Break())));
                }
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

                    let mut from_type = None;
                    
                    let check_from = get_peek_token!(self.lexer, errors);
                    if check_from.ttype == TokenType::Keyword && check_from.value == "from" {
                        self.lexer.next_token().unwrap();
                        // expect type
                        from_type = Some(err_ret!(self.parse_type(), errors));
                    }
                    // expect {
                    err_ret!(self.expect(Some(TokenType::Special), None), errors);

                    // parse block
                    let block = self.parse_block(token.pos, verbose)?;

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::FunctionDecl(from_type, name, args, ret_type, Box::new(block)))));

                    //                    let func_expr: Expression = Expression::F(ident, args);
                },
                "include" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "include is only allowed at top level")));
                        return Err(errors);
                    }
                    
                    // parse file name
                    let mut filename: String = self.lexer.filename.chars().rev().skip_while(|x| x != &'/').collect::<String>().chars().rev().collect();
                    let file = err_ret!(self.expect(Some(TokenType::String), None), errors).value;
                    filename.push_str(&file);
                    err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors).value;

                    if verbose > 0 {
                        eprintln!("[*] generatinng AST for included file `{filename}`");
                    }
                    let mut file_parser = match Parser::new(filename.clone()) {
                        Ok(a) => a,
                        Err(e) => {
                            errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "ERROR during loading of file: {e}")));
                            return Err(errors);
                        },
                    };
                    
                    let ast = match file_parser.parse(verbose) {
                        Ok(a) => a,
                        Err(vec) => {
                            for e in vec {
                                eprintln!("ERROR during parsing of included file `{filename}`:");
                                eprintln!("{}: {}", e.0, e.1);
                            }
                            return Err(errors);
                        },
                    };

                    if verbose > 1 {
                        eprintln!("{:#?}", ast);
                    }
                    if verbose > 2 {
                        eprintln!("-- INCLUDED AST: {filename} (converted) --");
                        eprintln!("{}", ast);
                        eprintln!("-- INCLUDED AST END: {filename} (converted) --");
                    }


                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Include(file, Box::new(ast), file_parser.lexer))));
                },
                "struct" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "structs are only allowed at top level")));
                        return Err(errors);
                    }
                    
                    // parse struct name
                    let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;

                    // expect {
                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors).value;

                    let mut fields: HashMap<String, (Type, usize)> = HashMap::new();
                    loop {
                        let next_token = err_ret!(self.lexer.peek_token(), errors);
                        if next_token.ttype == TokenType::Special && next_token.value == "}" {
                            self.lexer.next_token().unwrap();
                            break;
                        }
                        let typ = err_ret!(self.parse_type(), errors);
                        let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                        err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors);
                        fields.insert(name, (typ, fields.len()));
                    }

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Struct(name, fields))));
                }
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
                        let ntv = next_token.clone().value;
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
            TokenType::Undef => return self.parse_block_statement(is_root, verbose),
        }
        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
        Err(errors)
    }

    fn parse_ident_expr(&mut self, ident: String, ident_pos: usize, seperators: &[&'static str]) -> Result<Box<Expression>, Vec<Error>> {
        let mut errors: Vec<Error> = vec![];

        let token = get_peek_token!(self.lexer, errors);
        let val = token.clone().value;

        let res = match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Ident | TokenType::Type | TokenType::Keyword | TokenType::String | TokenType::Char | TokenType::Bool => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected identifier `{ident}`, type expected")));
                Err(errors.clone())
            }
            TokenType::Eof => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected end of file")));
                Err(errors.clone())
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
                    let func_expr: Expression = Expression(ident_pos, ExpressionR::F(ident.clone(), args), None);
                    Ok(Box::new(func_expr))
                },
                "{" => {
                    self.lexer.next_token().unwrap();
                    let mut fields: HashMap<String, Expression> = HashMap::new();
                    loop {
                        let peek = get_peek_token!(self.lexer, errors);
                        if peek.ttype == TokenType::Special && peek.value == "}" {
                            self.lexer.next_token().unwrap();
                            break;
                        }
                        
                        let field = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                        err_ret!(self.expect(Some(TokenType::Special), Some(":".into())), errors);
                        let expr = err_add!(self.parse_expr(&[";"]), errors).0;
                        fields.insert(field, expr);
                    }

                    Ok(Box::new(Expression(ident_pos, ExpressionR::StructLiteral(ident, fields), None)))
                },
                "."|"[" => {
                    Ok(Box::new(Expression(token.pos, ExpressionR::Var(ident), None)))
                },
                a => if seperators.contains(&a) {
                    let expr_ = Expression(ident_pos, ExpressionR::Var(ident), None);
                    let expr = self.parse_member(expr_, seperators)?;
                    Ok(Box::new(expr))
                } else {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                    Err(errors.clone())
                },
            },
            TokenType::Operator => {
                Ok(Box::new(Expression(token.pos, ExpressionR::Var(ident), None)))
            },
            TokenType::Undef => unreachable!(),
        }?;
        Ok(res)
    }

    fn parse_subexpr(&mut self, seperators: &[&'static str]) -> Result<(Box<Expression>, Token), Vec<Error>> {
        let inverse = |right_expr: &mut Box<Expression>| {
            let tmp_expr = right_expr.clone();
            **right_expr = Expression(tmp_expr.0, ExpressionR::T(Box::new(Expression(0, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), "-1".into()), None)), Op::Binary(BinaryOp::Mul), tmp_expr, Op::get_precedence(Op::Binary(BinaryOp::Mul))), None);
        };

        let mut errors: Vec<Error> = vec![];

        let token = get_token!(self.lexer, errors);
        let val = token.clone().value;

        let res = match token.ttype {
            TokenType::Type | TokenType::Eof | TokenType::Undef => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
                Err(errors.clone())
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
                "ref" => {
                    let expr = self.parse_subexpr(seperators)?;
                    Ok((Box::new(Expression(token.pos, ExpressionR::Ref(expr.0), None)), token))
                },
                "deref" => {
                    let expr = self.parse_subexpr(seperators)?;
                    Ok((Box::new(Expression(token.pos, ExpressionR::Deref(expr.0), None)), token))
                },
                _ => {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected keyword `{val}`")));
                    Err(errors.clone())
                }
            },
            TokenType::Int => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), val), None);
                Ok((Box::new(self.parse_member(expr_, seperators)?), token))

            },
            TokenType::Bool => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Bool), val), None);
                    Ok((Box::new(self.parse_member(expr_, seperators)?), token))
            },
            TokenType::Float => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Float), val), None);
                Ok((Box::new(self.parse_member(expr_, seperators)?), token))
            },
            TokenType::Char => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Char), val), None);
                Ok((Box::new(self.parse_member(expr_, seperators)?), token))
            },
            TokenType::String => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Array(Box::new(Type::Primitive(PrimitiveType::Char))), val), None);
                Ok((Box::new(self.parse_member(expr_, seperators)?), token))
            },
            TokenType::Ident => {
                let expr_ = self.parse_ident_expr(val, token.pos, seperators)?;
                Ok((Box::new(self.parse_member(*expr_, seperators)?), token))
            },
            TokenType::Special => match val.as_str() {
                "(" => {
                    let expr_ = self.parse_expr(&[")"])?.0;
                    Ok((Box::new(self.parse_member(expr_, seperators)?), token))
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
                    Err(errors.clone())
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
                            Err(errors.clone())
                        },
                        _ => {
                            errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "invalid operator {val}")));
                            Err(errors.clone())
                        }
                    }
                }
            },
        }?;
        Ok(res)
    }

    fn parse_member(&mut self, ident_expr: Expression, seperators: &[&'static str]) -> Result<Expression, Vec<Error>> {
        let mut errors = vec![];
        let mut res = ident_expr.clone();
        loop {
            let tk = get_peek_token!(self.lexer, errors);
            if tk.ttype == TokenType::Special && tk.value == "[" {
                // array indexing
                self.lexer.next_token().unwrap();
                let index = self.parse_expr(&["]"])?;
                res = Expression(ident_expr.0, ExpressionR::Index(Box::new(ident_expr.clone()), Box::new(index.0), vec![]), None);
            } else if tk.ttype == TokenType::Special && tk.value == "." {
                self.lexer.next_token().unwrap();
                let ident = err_ret!(self.expect(Some(TokenType::Ident), None), errors);
                let ident_expr = err_add!(self.parse_ident_expr(ident.clone().value, ident.pos, seperators), errors);
                match ident_expr.1 {
                    ExpressionR::Var(ref field) => {
                        let expr = Expression(ident_expr.0, ExpressionR::StructField(Box::new(res.clone()), field.clone(), None), None);
                        res = expr;
                        continue;
                    },
                    ExpressionR::F(ref name, ref args) => {
                        let expr = Expression(ident_expr.0, ExpressionR::MemberFunction(Box::new(res.clone()), name.clone(), args.clone()), None);
                        res = expr;
                        continue;
                    },
                    a => {
                        errors.push((ErrorLevel::Err, error!(self.lexer, tk.pos, "invalid token `{a}`, struct field or function expected")));
                        return Err(errors);
                    }
                }
            } else {
                break;
            }
        }
        Ok(res)
    }

    fn parse_array(&mut self) -> Result<Type, Error> {
        // expect type
        let typ = self.parse_type()?;
        let array_type  = Type::Array(Box::new(typ));

        // expect ]
        let next_token = self.lexer.next_token()?;
        if next_token.ttype != TokenType::Special || next_token.value != "]" {
            let val = next_token.clone().value;
            return Err((ErrorLevel::Err, error!(self.lexer, next_token.pos, "invalid token `{val}`, `]` expect")));
        }

        Ok(array_type)
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let nt = self.lexer.next_token()?;
        let val = nt.clone().value;

        let mut tmp_tp = if nt.ttype == TokenType::Type {
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
        }?;

        loop {
            let ptr_check = self.lexer.peek_token()?;
            if ptr_check.ttype == TokenType::Operator && ptr_check.value == "*" {
                self.lexer.next_token().unwrap();
                tmp_tp = Type::Pointer(Box::new(tmp_tp));
            } else {
                return Ok(tmp_tp);
            }
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
                let val = token.clone().value;
                Err((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`, `{exp_val}` expected")))
            } else {
                Ok(token)
            }
        } else if let Some(t) = typ {
            let is_valid: bool = token.ttype == t;
            if ! is_valid {
                let val = token.clone().value;
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

type Globals = HashMap<String, usize>;
type Aliases = HashMap<String, Type>;
type Vars = Aliases;
type Functions = HashMap<(Option<Type>, String), (Vec<Type>, Type)>;
fn check(ast: &mut ASTNode, mut lexer: Lexer) -> Result<(Globals, Aliases, Functions), Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    
    // collect all aliases + functions
    let mut type_aliases: Aliases = HashMap::new();
    let mut functions: Functions = HashMap::new();
    let mut vars: Vars = HashMap::new();
    let mut globals: Globals = HashMap::new();
    
    if let ASTNode(_, ASTNodeR::Block(arr)) = ast.clone() {
        for a in arr.clone() {
            match a {
                ASTNode(_, ASTNodeR::TypeAlias(alias, typ)) => {
                    type_aliases.insert(alias, typ);
                },
                ASTNode(_, ASTNodeR::Struct(name, fields)) => {
                    type_aliases.insert(name.clone(), Type::Struct(name, fields));
                },
                ASTNode(_, ASTNodeR::Intrinsic(_, fname, args, rt)) => {
                    functions.insert((None, fname), (args.into_iter().enumerate().map(|(_, (a, _))| a).collect(), rt));
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
                ASTNode(pos, ASTNodeR::FunctionDecl(tp, name, args, ret_type, _)) => {
                    let left_type = tp.map(|x| x.dealias(&type_aliases));

                    if let Some(lt) = left_type.clone() {
                        let first_type = args[0].clone().0;
                        if ! lt.is_compatible(&first_type, &type_aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: first parameter of member function declaration has to be the same type as the type used on. (got `{first_type}`, expected `{lt}`)")));
                        }
                    }
                    functions.insert((left_type, name), (args.into_iter().map(|(a, _)| a).collect(), ret_type));
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

    fn check_references_expr(expr: &mut Expression, functions: &Functions, vars: &HashMap<String, Type>, aliases: &Aliases, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
        
        //        fn tp(expr: &mut Expression, functions: &HashMap<Option<String>, (Vec<Type>, Type)>, vars: &HashMap<String, Type>, aliases: &HashMap<String, Type>, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
        let tp = | | -> Type {
            let pos = expr.0;
            match &mut expr.1 {
                ExpressionR::T(ref mut left, op, ref mut right, _) => {
                    let left = check_references_expr(left, functions, vars, aliases, errors, lexer);
                    let right = check_references_expr(right, functions, vars, aliases,  errors, lexer);
                    
                    match op.combine_type(&left, &right, aliases) {
                        Type::Invalid => {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `{left}` and `{right}` for operation `{op}`")));
                            Type::Invalid
                        },
                        a => a,
                    }
                },
                ExpressionR::Ref(expr) => {
                    let tp = check_references_expr(expr, functions, vars, aliases, errors, lexer);
                    Type::Pointer(Box::new(tp))
                },
                ExpressionR::Deref(expr) => {
                    let tp = check_references_expr(expr, functions, vars, aliases, errors, lexer);
                    if let Type::Pointer(inner) = tp {
                        *inner
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "type `{tp}` cannot be dereferenced")));
                        Type::Invalid
                    }
                },
                ExpressionR::StructLiteral(name, fields) => {
                    let mut fields_: HashMap<String, (Type, usize)> = HashMap::new();
                    for (name_, expr) in fields {
                        let tp = check_references_expr(expr, functions, vars, aliases, errors, lexer);
                        fields_.insert(name_.clone(), (tp.clone(), fields_.len()));
                    }
                    Type::Struct(name.clone(), fields_)
                },
                ExpressionR::StructField(ref mut expr_, ref mut name, _) => {
                    let struct_type = check_references_expr(expr_, functions, vars, aliases, errors, lexer).dealias(aliases);
                    if let Type::Struct(ref struct_name, ref map) = struct_type {
                        if map.contains_key(name) {
                            let ret = map.get(name).unwrap().0.clone();
                            expr.1 = ExpressionR::StructField(expr_.clone(), name.clone(), Some(struct_type.clone()));
                            return ret;
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "undefinded field `{name}` for type `{struct_name}`")));
                        }
                    }
                    Type::Invalid
                }
                ExpressionR::UnaryOp(op, ref mut left, _) => {
                    let left = check_references_expr(left, functions, vars, aliases, errors, lexer);
                    
                    match op.with_type(&left, aliases) {
                        Type::Invalid => {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible type `{left}` for operation `{op}`")));
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
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to variable `{name}`")));
                        Type::Invalid
                    } else {
                        vars.get(name).unwrap().clone()
                    }
                },
                ExpressionR::F(name, ref mut vec) => {
                    if ! functions.contains_key(&(None, name.clone())) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to function `{name}`")));
                        return Type::Invalid;
                    }
                    
                    let func_args = &functions.get(&(None, name.clone())).unwrap().0;

                    let len_a = func_args.len();
                    let len_b = vec.len();
                    if len_a != len_b {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        return Type::Invalid;
                    };
                    
                    for (index, a) in vec.iter_mut().enumerate() {
                        let typa = check_references_expr(a, functions, vars, aliases, errors, lexer);
                        let typb = func_args.get(index).unwrap().clone();
                        
                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }
                    
                    functions.get(&(None, name.clone())).unwrap().1.clone()
                },
                ExpressionR::MemberFunction(lexpr, name, ref mut vec) => {
                    let left_type = check_references_expr(lexpr, functions, vars, aliases, errors, lexer).dealias(aliases);
                    if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to member function `{name}` from type {left_type}")));
                        return Type::Invalid;
                    }
                    
                    let func_args = &functions.get(&(Some(left_type.clone()), name.clone())).unwrap().0;

                    let len_a = func_args.len();
                    let len_b = vec.len()+1;
                    if len_a != len_b {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        return Type::Invalid;
                    };

                    
                    for (index, a) in vec.iter_mut().enumerate() {
                        let typa = check_references_expr(a, functions, vars, aliases, errors, lexer);
                        let typb = func_args.get(index+1).unwrap().clone();
                        
                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }
                    if !errors.is_empty() {
                        return Type::Invalid;
                    }
                    
                    functions.get(&(Some(left_type.clone()), name.clone())).unwrap().1.clone()
                },
                ExpressionR::Arr(ref mut vec) => {
                    let mut last_tp = Type::Invalid;
                    for a in vec {
                        let tp = check_references_expr(a, functions, vars, aliases, errors, lexer);
                        if last_tp == Type::Invalid {
                            last_tp = tp.clone();
                        }
                        if ! last_tp.is_compatible(&tp, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `{last_tp}` and `{tp}` in array literal")));
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
                ExpressionR::Index(ident, ref mut a, _) => {

                    let inner_type = check_references_expr(a, functions, vars, aliases, errors, lexer);
                    if ! Type::Primitive(PrimitiveType::Int).is_compatible(&inner_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `int` and `{inner_type}` in array length definition")));
                        Type::Invalid
                    } else {
                        let ident_type = check_references_expr(ident, functions, vars, aliases, errors, lexer);
                        let inner_tp = if let Type::Array(inner) = ident_type {
                            *inner
                        } else if let Type::Custom(ref inner) = ident_type {
                            if aliases.contains_key(inner) {
                                if let Type::Array(ref inner2) = aliases[inner] {
                                    *inner2.clone()
                                } else {
                                    errors.push((ErrorLevel::Err, error!(lexer, pos, "`{ident_type}` is not an array type")));
                                    Type::Invalid
                                }
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, pos, "`{ident_type}` is not an array type")));
                                Type::Invalid
                            }
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "`{ident_type}` is not an array type")));
                            Type::Invalid
                        };
                        if let Type::Struct(_, ref map) = inner_tp.dealias(aliases) {
                            let mut vector_: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                            vector_.sort();
                            let vec: Vec<usize> = vector_.iter().map(|x| map[x].0.size(aliases)).collect();
                            expr.1 = ExpressionR::Index(ident.clone(), a.clone(), vec);
                        } else {
                            expr.1 = ExpressionR::Index(ident.clone(), a.clone(), vec![inner_tp.size(aliases)]);
                        }
                        inner_tp
                    }
                }
            }}();
        let typ = tp;//expr, functions, vars, aliases, errors, lexer);
        expr.2 = Some(typ.clone());
        typ
    }

    type ListArgs<'a> = (&'a mut ASTNode, &'a mut Functions, &'a Vars, &'a mut Aliases, &'a mut Globals, &'a mut Vec<(ErrorLevel, String)>);
    
    // check
    fn check_references(largs: ListArgs, f: (Option<Type>, String), is_loop: bool, lexer: &mut Lexer) {
        let (node, functions, vars, aliases, globals, errors) = largs;
        if let ASTNode(_, ASTNodeR::Block(ref mut arr)) = node {
            let vars_sub = &mut vars.clone();
            for mut a in arr {
                match a {
                    ASTNode(pos, ASTNodeR::FunctionDecl(tp, func, args, _, block)) => {
                        for arg in args.clone() {
                            if let Type::Custom(t) = arg.0.dealias(aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                                continue;
                            }
                            vars_sub.insert(arg.1, arg.0);
                        }
                        let ac = aliases.clone();
                        check_references((block, functions, vars_sub, aliases, globals, errors), (tp.as_ref().map(|x| x.dealias(&ac)), func.clone()), false, lexer);
                    },
                    ASTNode(pos, ASTNodeR::FunctionCall(name, args)) => {
                        if ! functions.contains_key(&(None, name.clone())) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to function `{name}`")));
                            continue;
                        }
                        
                        let func_args = &functions.get(&(None, name.clone())).unwrap().0;

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
                    ASTNode(pos, ASTNodeR::MemberFunction(lexpr, name, ref mut vec)) => {
                        let left_type = check_references_expr(lexpr, functions, vars, aliases, errors, lexer).dealias(aliases);
                        if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to member function `{name}` from type {left_type}")));
                            continue;
                        }
                        let func_args = &functions.get(&(Some(left_type), name.clone())).unwrap().0;

                        let len_a = func_args.len();
                        let len_b = vec.len()+1;
                        if len_a != len_b {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                            continue;
                        };

                        for (index, a) in vec.iter_mut().enumerate() {
                            let typa = check_references_expr(a, functions, vars_sub, aliases, errors, lexer);
                            let typb = func_args.get(index+1).unwrap().clone();

                            if ! typa.is_compatible(&typb, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                                break;
                            }
                        }
                    }
                    ASTNode(_, ASTNodeR::Block(..)) => {
                        check_references((a, functions, vars_sub, aliases, globals, errors), f.clone(), false, lexer);
                    },
                    ASTNode(pos, ASTNodeR::SetField(ref mut expr_, ref mut name, ref mut rexpr, _)) => {
                        let struct_type = check_references_expr(expr_, functions, vars_sub, aliases, errors, lexer).dealias(aliases);
                        if let Type::Struct(ref struct_name, ref map) = struct_type {
                            if map.contains_key(name) {
                                let field_type = map.get(name).unwrap().0.clone();
                                let rtype = check_references_expr(rexpr, functions, vars_sub, aliases, errors, lexer);
                                if !field_type.is_compatible(&rtype, aliases) {
                                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "imcompatible types: expected `{field_type}`, found `{rtype}`")));
                                    continue;
                                }
                                a.1 = ASTNodeR::SetField(expr_.clone(), name.clone(), rexpr.clone(), Some(struct_type));
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefinded field `{name}` for type `{struct_name}`")));
                            }
                        }
                    }
                    ASTNode(pos, ASTNodeR::If(ref mut expr, ref mut block)) => {
                        let bool_type = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                        }
                        check_references((block, functions, vars_sub, aliases, globals, errors), f.clone(), is_loop, lexer);
                    },
                    ASTNode(pos, ASTNodeR::While(ref mut expr, ref mut block)) => {
                        let bool_type = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                        }
                        check_references((block, functions, vars_sub, aliases, globals, errors), f.clone(), true, lexer);
                    },
                    ASTNode(pos, ASTNodeR::Return(ref mut expr)) => {
                        let tp = check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        let ret_type  = &functions.get(&f.clone()).unwrap().1;
                        if ! ret_type.is_compatible(&tp, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{ret_type}`, found `{tp}`, because of return type")));
                        }
                    },
                    ASTNode(pos, ASTNodeR::ArrIndexInit(lexpr, ref mut ind, ref mut expr, _)) => {
                        let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if type_r == &Type::Invalid {
                            continue;
                        }

                        let type_ind = &check_references_expr(ind, functions, vars_sub, aliases, errors, lexer);
                        if type_ind == &Type::Invalid {
                            continue;
                        }
                        
                        let type_l = check_references_expr(lexpr, functions, vars_sub, aliases, errors, lexer);
                        if let Type::Array(_) = type_l {} else if let Type::Custom(ref inner) = type_l {
                            if aliases.contains_key(inner) {
                                if let Type::Array(_) = aliases.get(inner).unwrap() {} else {
                                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "{type_l} used in array indexing is not an array type")));
                                }
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "{type_l} used in array indexing is not an array type")));
                            }
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "{type_l} used in array indexing is not an array type")));
                        }
                        
                        if ! type_l.is_compatible(&Type::Array(Box::new(type_r.clone())), aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{type_l}`, found `{type_r}`")));
                        }
                        
                        if ! Type::Primitive(PrimitiveType::Int).is_compatible(type_ind, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected int, found `{type_ind}`")));
                        }
                        if let Type::Struct(_, map) = type_r {
                            let mut vector_: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                            vector_.sort();
                            let vec: Vec<usize> = vector_.iter().map(|x| map[x].0.size(aliases)).collect();
                            
                            a.1 = ASTNodeR::ArrIndexInit(lexpr.clone(), ind.clone(), expr.clone(), vec);
                        } else {
                            a.1 = ASTNodeR::ArrIndexInit(lexpr.clone(), ind.clone(), expr.clone(), vec![type_r.size(aliases)]);
                        }
                        
                    },
                    ASTNode(pos, ASTNodeR::VarInit(var, ref mut expr, _)) => {
                        if ! vars_sub.contains_key(var) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{var}`")));
                        } else {
                            let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                            if type_r == &Type::Invalid {
                                continue;
                            }
                            let type_l = vars_sub.get(var).unwrap().dealias(aliases);
                            if ! type_l.is_compatible(type_r, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{type_l}`, found `{type_r}`")));
                            }
                            a.1 = ASTNodeR::VarInit(var.clone(), expr.clone(), Some(type_l));
                        }
                    },
                    ASTNode(pos, ASTNodeR::VarOp(var, op, ref mut expr)) => {
                        if ! vars_sub.contains_key(var) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{var}`")));
                        } else {
                            let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                            if type_r == &Type::Invalid {
                                continue;
                            }
                            let type_l = vars_sub.get(var).unwrap();
                            if ! type_l.is_compatible(type_r, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{type_l}`, found `{type_r}`")));
                            }

                            if Op::combine_type(&Op::Binary(*op), type_l, type_l, aliases) == Type::Invalid {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{type_l}` and `{type_r}` for operation `{op}`")));
                            }
                            
                        }
                    },
                    ASTNode(pos, ASTNodeR::VarDec(_, tp, name)) => {
                        if let Type::Custom(t) = tp.dealias(aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                            continue;
                        }
                        vars_sub.insert(name.clone(), tp.clone());
                    },
                    ASTNode(pos, ASTNodeR::VarDecInit(_, tp, name, expr)) => {
                        if let Type::Custom(t) = tp.dealias(aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                            continue;
                        }
                        let type_r = &check_references_expr(expr, functions, vars_sub, aliases, errors, lexer);
                        if type_r == &Type::Invalid {
                            continue;
                        }
                        let typ = tp.dealias(aliases);
                        if ! typ.is_compatible(type_r, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typ}`, found `{type_r}`")));
                        }
                        vars_sub.insert(name.clone(), typ.clone());
                    },
                    ASTNode(pos, ASTNodeR::Break()) => {
                        if ! is_loop {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "break outside of loop")));
                        }
                    },
                    ASTNode(_, ASTNodeR::TypeAlias(_, _)) => {},
                    ASTNode(_, ASTNodeR::Struct(_, _)) => {},
                    ASTNode(_, ASTNodeR::Include(_, ref mut ast, ref lexer_)) => {
                        let (g, a, f) = match check(ast, lexer_.clone()) {
                            Ok(a) => a,
                            Err(vec) => {
                                for e in vec {
                                    errors.push(e);
                                }
                                return;
                            },
                        };
                        for (k, v) in g {
                            globals.insert(k, v);
                        }
                        for (k, v) in a {
                            aliases.insert(k, v);
                        }
                        for (k, v) in f {
                            functions.insert(k, v);
                        }
                    }
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
                    if let ASTNodeR::FunctionDecl(_, _, _, rt, block) = &a.1 {
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

    check_references((ast, &mut functions, &vars, &mut type_aliases, &mut globals, &mut errors), (None, "".into()), false, &mut lexer);
    check_function_ret_paths(&ast.1, &mut errors, &mut lexer);

    // type checking
    if errors.is_empty() {
        Ok((globals, type_aliases, functions))
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
    Var(usize, usize, usize, bool),
    RetVal(usize),
    Intrinsic(String, String),
    Push(usize),
    Pop(usize),
    Arr(usize, usize),
    Index(usize, usize, Vec<usize>, bool),
    ArraySet(usize, usize, usize, Vec<usize>),
    Global(usize, String, usize, usize, bool),
    GlobalSet(usize, String, usize, usize),
    Jump(String),
    Field(usize, usize, Type),
    Deref(usize),
    SetField(usize, usize, usize, usize),
    Break(usize),
}

fn intermediate_expr(expr: Expression, index: usize, indicies: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: &HashMap<String, Type>, is_ref: bool) -> Vec<Inst> {
    let mut ret = vec![];
    let Expression(_pos, expr, tp) = expr;
    match expr {
        ExpressionR::UnaryOp(op, fst, _) => {
            ret.append(&mut intermediate_expr(*fst, index, indicies, globals, aliases, false));
            ret.push(Inst::Push(index));
            ret.push(Inst::UnOp(index, tp.unwrap().size(aliases), op));
        },
        ExpressionR::T(fst, op, snd, _) => {
            ret.append(&mut intermediate_expr(*fst.clone(), index, indicies, globals, aliases, false));
            ret.push(Inst::Push(index));
            ret.append(&mut intermediate_expr(*snd, index+1, indicies, globals, aliases, false));
            ret.push(Inst::Pop(index));
            if let Op::Binary(op) = op {
                ret.push(Inst::BinOp((index, index+1), fst.2.unwrap().size(aliases), op));
            } else {
                unreachable!();
            }
        },
        ExpressionR::StructLiteral(_, fields) => {
            // convert hashmap to vector
            let mut vector: Vec<String> = fields.iter().map(|(x, _)| x.clone()).collect();
            vector.sort();
            
            for (i, key) in vector.into_iter().enumerate() {
                ret.append(&mut intermediate_expr(fields[&key].clone(), index+i, indicies, globals, aliases, false));
            }
        },
        ExpressionR::Val(tp, val) => {
            if !is_ref {
                ret.push(Inst::Val(index, tp, val));
            } else {
                ret.push(Inst::Val(index, tp, val));
                let pos = last_ptr(indicies) + 8;
                ret.push(Inst::VarSet(index, 8, pos));
                ret.push(Inst::Var(index, pos, 8, true));
            }
        },
        ExpressionR::Var(name) => {
            if indicies.contains_key(&name) {
                ret.push(Inst::Var(index, indicies.get(&name).unwrap().0, indicies.get(&name).unwrap().1, is_ref));
            } else {
                let size = *globals.get(&name).unwrap();
                if size < 8 || is_ref {
                    ret.push(Inst::Global(index, name, 8, 0, is_ref));
                }  else {
                    let mut sz = size;
                    let mut ind = 0;
                    while sz > 8 {
                        ret.push(Inst::Global(index+ind/8, name.clone(), 8, ind, is_ref));
                        ind += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::Global(index+ind/8, name, sz, ind, is_ref));
                }
            }
        },
        ExpressionR::F(name, args) => {
            let mut ind = 0;
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, ind, indicies, globals, aliases, false));
                ret.push(Inst::Push(ind));
                ind += 1;
            }

            for _ in args.iter().rev() {
                ind -= 1;
                ret.push(Inst::Pop(ind));
            }
            
            ret.push(Inst::Call(name));
            if is_ref {
                let len = tp.unwrap().size(aliases);
                for i in (0..(len/8)).rev() {
                    indicies.insert("__struct_reserved__".into(), (last_ptr(indicies)+8, 8));
                    ret.push(Inst::VarSet(index+i, 8, last_ptr(indicies)));
                }
                ret.push(Inst::Var(index, last_ptr(indicies), 8, true));
            }
            ret.push(Inst::RetVal(index));
        },
        ExpressionR::MemberFunction(lexpr, name, args) => {
            ret.append(&mut intermediate_expr(*lexpr, 0, indicies, globals, aliases, false));
            let mut ind = 1;
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, ind, indicies, globals, aliases, false));
                ret.push(Inst::Push(ind));
                ind += 1;
            }

            for _ in args.iter().rev() {
                ind -= 1;
                ret.push(Inst::Pop(ind));
            }
            
            ret.push(Inst::Call(name));
            if is_ref {
                let len = tp.unwrap().size(aliases);
                for i in (0..(len/8)).rev() {
                    indicies.insert("__struct_reserved__".into(), (last_ptr(indicies)+8, 8));
                    ret.push(Inst::VarSet(index+i, 8, last_ptr(indicies)));
                }
                ret.push(Inst::Var(index, last_ptr(indicies), 8, true));
            }
            ret.push(Inst::RetVal(index));
        },
        ExpressionR::Ref(expr) => {
            ret.append(&mut intermediate_expr(*expr, index, indicies, globals, aliases, true));
        },
        ExpressionR::Deref(expr) => {
            ret.append(&mut intermediate_expr(*expr, index, indicies, globals, aliases, false));
            ret.push(Inst::Deref(index));
        }
        ExpressionR::Arr(_) => {
            todo!();
        },
        ExpressionR::Undef => {
            
        },
        ExpressionR::ArrAlloc(tp, sz) => {
            ret.push(Inst::Arr(index, sz * tp.size(aliases)));
        },
        ExpressionR::Index(lexpr, expr, expr_vec) => {
            ret.append(&mut intermediate_expr(*expr, index, indicies, globals, aliases, false));
            ret.push(Inst::Val(index+1, Type::Primitive(PrimitiveType::Int), tp.unwrap().size(aliases).to_string()));
            ret.push(Inst::BinOp((index, index+1), 8, BinaryOp::Mul));
            ret.push(Inst::Push(index));
            ret.append(&mut intermediate_expr(*lexpr, index+1, indicies, globals, aliases, false));
            ret.push(Inst::Pop(index));
            ret.push(Inst::Index(index, index+1, expr_vec, is_ref));
        },
        ExpressionR::StructField(expr, field, struct_type) => {
            ret.append(&mut intermediate_expr(*expr, index, indicies, globals, aliases, true));
            
            let mut offset = 0;
            if let Some(Type::Struct(_, map)) = struct_type {
                // convert hashmap to vector
                let mut vector_: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                vector_.sort();
                let vector: Vec<(String, Type)> = vector_.iter().map(|x| (x.clone(), map[x].0.clone())).collect();
                for (key, tp) in vector {
                    if key == field {
                        break;
                    }
                    offset += tp.size(aliases);
                }
            } else {
                unreachable!("should always be a struct (error in type-checking)")
            }
            ret.push(Inst::Field(index, offset, tp.unwrap().dealias(aliases)));
        }
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
     ("set_ptr", "\tmov [rax], rbx\n\tret\n"),
     ("get_args", "\tmov rax, [ARGS]\n\tret\n")
    ]
        .iter()
        .cloned()
        .collect()
}

fn intermediate(ast: ASTNodeR, offsets: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: HashMap<String, Type>, loop_idx: usize, index:  usize, is_top_level: bool) -> (Vec<Inst>, HashMap<String, usize>) {
    let mut ret = vec![];

    if is_top_level {
        // check for global initializations
        if let ASTNodeR::Block(ref a) = ast {
            for inst in a {
                match &inst.1 {
                    ASTNodeR::VarInit(ref name, ref expr, tp) => {
                        ret.append(&mut intermediate_expr(expr.clone(), 0, offsets, globals, &aliases, false));
                        if ! offsets.contains_key(name) {
                            let len = globals.get(name).unwrap();
                            if *len < 8 {
                                ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
                            } else if let Some(Type::Struct(_, map)) = tp {
                                let mut vector: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                                vector.sort();
                                let mut offset = 0;
                                for (index, key) in vector.iter().enumerate() {
                                    let vals = &map[key];
                                    let sz = vals.0.size(&aliases);
                                    ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                                    offset += sz;
                                }
                            } else {
                                let mut sz = *len;
                                let mut index = 0;
                                while sz > 8 {
                                    ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                                    index += 8;
                                    sz -= 8;
                                }
                                ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                            }
                        }
                    },
                    ASTNodeR::VarDecInit(_, tp, ref name, ref expr) => {
                        ret.append(&mut intermediate_expr(expr.clone(), 0, offsets, globals, &aliases, false));
                        if ! offsets.contains_key(name) {
                            let len = globals.get(name).unwrap();
                            if *len < 8 {
                                ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
                            } else if let Type::Struct(_, map) = tp.dealias(&aliases) {
                                let mut vector: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                                vector.sort();
                                let mut offset = 0;
                                for (index, key) in vector.iter().enumerate() {
                                    let vals = &map[key];
                                    let sz = vals.0.size(&aliases);
                                    ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                                    offset += sz;
                                }
                            } else {
                                let mut sz = *len;
                                let mut index = 0;
                                while sz > 8 {
                                    ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                                    index += 8;
                                    sz -= 8;
                                }
                                ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                            }
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
                count *= 2;
                ret.append(&mut intermediate(i.1, offsets, globals, aliases.clone(), loop_idx, count, false).0);
            }
        },
        ASTNodeR::VarDec(is_global, tp, ref name) => {
            if ! is_global {
                offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
            }
        },
        ASTNodeR::ArrIndexInit(lexpr, ind, expr, expr_vec) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases, false));
            let index = expr_vec.len();
            ret.append(&mut intermediate_expr(ind, index, offsets, globals, &aliases, false));
            if let Type::Array(tp) = lexpr.2.as_ref().unwrap().dealias(&aliases) {
                ret.push(Inst::Val(index+1, Type::Primitive(PrimitiveType::Int), tp.size(&aliases).to_string()));
                ret.push(Inst::BinOp((index, index+1), 8, BinaryOp::Mul));
                ret.append(&mut intermediate_expr(lexpr, index+1, offsets, globals, &aliases, false));
                ret.push(Inst::ArraySet(0, index, index+1, expr_vec));
            } else {
                unreachable!()
            }
        },
        ASTNodeR::VarDecInit(is_global, tp, ref name, expr) => {
            if ! is_global {
                offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
            }
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases, false));
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                let size = vals.1;
                if size < 8 {
                    ret.push(Inst::VarSet(0, vals.1, vals.0))
                } else if let Type::Struct(_, map) = tp.dealias(&aliases) {
                    let mut vector: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                    vector.sort();
                    let mut offset = vals.0;
                    for (index, key) in vector.iter().enumerate() {
                        let vals = &map[key];
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::VarSet(index, sz, offset));
                        offset -= sz;
                    }
                } else {
                    let mut sz = size;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::VarSet(0, 8, vals.0 + index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::VarSet(0, sz, vals.0 + index));
                }
            } else {
                let len = globals.get(name).unwrap();
                if *len < 8 {
                    ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
                } else if let Type::Struct(_, map) = tp {
                    let mut vector: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                    vector.sort();
                    let mut offset = 0;
                    for (index, key) in vector.iter().enumerate() {
                        let vals = &map[key];
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                        offset += sz;
                    }
                } else {
                    let mut sz = *len;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                }
            }
        },
        ASTNodeR::VarOp(ref name, op, expr) => {
            ret.append(&mut intermediate_expr(expr, 1, offsets, globals, &aliases, false));
            
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                ret.push(Inst::Var(0, vals.0, vals.1, false));
                ret.push(Inst::BinOp((0, 1), vals.1, op));
                ret.push(Inst::VarSet(0, vals.1, vals.0))
            } else {
                let len = globals.get(name).unwrap();
                ret.push(Inst::Global(0, name.into(), 8, 0, false));
                ret.push(Inst::BinOp((0, 1), *len, op));
                ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
            }
        },
        ASTNodeR::VarInit(ref name, expr, tp) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases, false));
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                let size = vals.1;
                if size < 8 {
                    ret.push(Inst::VarSet(0, vals.1, vals.0))
                } else if let Some(Type::Struct(_, map)) = tp {
                    let mut vector: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                    vector.sort();
                    let mut offset = vals.0;
                    for (index, key) in vector.iter().enumerate() {
                        let vals = &map[key];
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::VarSet(index, sz, offset));
                        offset -= sz;
                    }
                } else {
                    let mut sz = size;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::VarSet(0, 8, vals.0 + index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::VarSet(0, sz, vals.0 + index));
                }
            } else {
                let len = globals.get(name).unwrap();
                if *len < 8 {
                    ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
                } else if let Some(Type::Struct(_, map)) = tp {
                    let mut vector: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                    vector.sort();
                    let mut offset = 0;
                    for (index, key) in vector.iter().enumerate() {
                        let vals = &map[key];
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                        offset += sz;
                    }
                } else {
                    let mut sz = *len;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                }
            }
        },
        ASTNodeR::FunctionCall(name, args) => {
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, 0, offsets, globals, &aliases, false));
                ret.push(Inst::Push(0));
            }

            let mut index = args.len();
            for _ in args.iter().rev() {
                index -= 1;
                ret.push(Inst::Pop(index));
            }
            
            ret.push(Inst::Call(name));
        },
        ASTNodeR::MemberFunction(lexpr, name, args) => {
            ret.append(&mut intermediate_expr(lexpr, 0, offsets, globals, &aliases, false));
            let mut ind = 1;
            for a in args.clone() {
                ret.append(&mut intermediate_expr(a, ind, offsets, globals, &aliases, false));
                ret.push(Inst::Push(ind));
                ind += 1;
            }

            for _ in args.iter().rev() {
                ind -= 1;
                ret.push(Inst::Pop(ind));
            }
            
            ret.push(Inst::Call(name));
        },
        ASTNodeR::If(expr, block) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases, false));
            ret.push(Inst::If(0, index));
            ret.append(&mut intermediate(block.1, offsets, globals, aliases, loop_idx, index * 3, false).0);
            ret.push(Inst::Endif(index));
        },
        ASTNodeR::While(cond, block) => {
            ret.push(Inst::WhileStart(0, index));
            ret.append(&mut intermediate_expr(cond, 0, offsets, globals, &aliases, false));
            ret.push(Inst::WhileCheck(0, index));
            ret.append(&mut intermediate(block.1, offsets, globals, aliases, index, index * 3, false).0);
            ret.push(Inst::Endwhile(index));
        },
        ASTNodeR::Break() => {
            ret.push(Inst::Break(loop_idx));
        }
        ASTNodeR::SetField(lexpr, name, rexpr, struct_type) => {
            if let Some(Type::Struct(_, map)) = struct_type {
                let map_result = &map[&name];
                let mut vec: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                vec.sort();
                let mut off = 0;
                for key in vec {
                    if key == name {
                        break;
                    }
                    off += map[&key].0.size(&aliases);
                }
                ret.append(&mut intermediate_expr(lexpr, 0, offsets, globals, &aliases, true));
                ret.append(&mut intermediate_expr(rexpr, 1, offsets, globals, &aliases, false));
                ret.push(Inst::SetField(0, 1, off, map_result.0.size(&aliases)));
            } else {
                unreachable!()
            }
        },
        ASTNodeR::FunctionDecl(_, name, a, rt, block) => {
            let mut offsets: HashMap<String, (usize, usize)> = HashMap::new();
            let mut offset = 0;
            for arg in a {
                offsets.insert(arg.1, (offset + arg.0.size(&aliases), arg.0.size(&aliases)));
                offset += arg.0.size(&aliases);
            }

            ret.push(Inst::Func(name, offsets.clone()));
            ret.append(&mut intermediate(block.1, &mut offsets, globals, aliases, 0, index * 3, false).0);
            if rt == Type::Primitive(PrimitiveType::Void) {
                ret.push(Inst::Ret(0));
            }
        },
        ASTNodeR::Return(expr) => {
            ret.append(&mut intermediate_expr(expr, 0, offsets, globals, &aliases, false));
            ret.push(Inst::Ret(0));
        },
        ASTNodeR::Intrinsic(iname, fname, _, _) => {
            ret.push(Inst::Intrinsic(iname, fname));
        },
        // ignore
        ASTNodeR::TypeAlias(..) => {},
        ASTNodeR::Struct(..) => {},
        ASTNodeR::Include(_, ast, _) => {
            ret.append(&mut intermediate(ast.1, offsets, globals, aliases, 0, index, is_top_level).0);
        },
        
    }
    (ret, globals.clone())
}

fn generate(insts: Vec<Inst>, globals: &HashMap<String, usize>) -> String {

    let datatype = |a: usize| match a {
        1 => "byte",
        2 => "word",
        4 => "word",
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
        6 => "r8d",
        7 => "r9d",
        8 => "r10d",
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
        a => match sz {
            8 => a,
            _ => "__invalid__",
        }
    };

    let mut global_strings: Vec<String> = vec![];
    let mut global_arrays: Vec<usize> = vec![];
    
    let mut ret: String = "\
    global _start\n\
    section .text\n\
    _start:\n\
    \tmov [ARGS], rsp\n\
    \tpush rbp\n\
    \tmov rbp, rsp\n\
    ".into();

    let mut intrinsic_labels: Vec<String> = vec![];

    let mut arr_index: usize = 0;
    
    for a in insts {
        ret.push_str(match a {
            Inst::Func(name, offsets) => {
                let mut string = String::new();
                
                let mut vec: Vec<(usize, String)> = offsets.iter().map(|(x, (o, _))| (*o, x.clone())).collect();
                vec.sort();

                for (index, a) in (0..offsets.len()).enumerate() {
                    let mut sz = offsets[&vec[a].1].1;
                    let mut add_off = 0;
                    while sz > 8 {
                        let _ = write!(string, "\tsub rsp, {ind}\n\tmov [rbp-{ind}], {}\n", register(index), ind = offsets[&vec[a].1].0 + add_off);
                        sz -= 8;
                        add_off += 8;
                    }
                    let _ = write!(string, "\tsub rsp, {ind}\n\tmov [rbp-{ind}], {}\n", register_sz(index, sz), ind = offsets[&vec[a].1].0 + add_off);
                }
                format!(";; FUNCTION DECL {name}\n{name}:\n\tpush rbp\n\tmov rbp,rsp\n{string}")
            },
            Inst::Call(name) => {
                format!(";; FUNCTION CALL {name}\n\tcall {name}\n")
            },
            Inst::If(reg, id) => {
                format!(";; IF {id} START\n\tcmp {}, 1\n\tjne .l2_{id}\n.l1_{id}:\n", register_sz(reg, 1))
            },
            Inst::Endif(id) => {
                format!(";; IF {id} END\n.l2_{id}:\n")
            },
            Inst::WhileCheck(reg, id) => {
                format!(";; WHILE {id} CHECK\n\tcmp {}, 1\n\tjne .loop_{id}_end\n", register_sz(reg, 1))
            },
            Inst::WhileStart(_, id) => {
                format!(";; WHILE {id} START\n.loop_{id}:\n")
            }
            Inst::Endwhile(id) => {
                format!(";; WHILE {id} END\n\tjmp .loop_{id}\n.loop_{id}_end:\n")
            },
            Inst::Break(id) => {
                format!(";; BREAK {id}\n\tjmp .loop_{id}_end\n")
            },
            Inst::Push(reg) => {
                format!(";; PUSH\n\tpush {}\n", register(reg))
            },
            Inst::Pop(reg) => {
                format!(";; POP\n\tpop {}\n", register(reg))
            },
            Inst::Val(reg, tp, val) => {
                let str_val = match tp {
                    Type::Primitive(a) => {
                        match a {
                            PrimitiveType::Int => val.clone(),
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
                            PrimitiveType::Unchecked => val.clone(),
                        }
                    },
                    Type::Array(ref bx) => {
                        match **bx {
                            Type::Primitive(PrimitiveType::Char) => {
                                if ! global_strings.contains(&val.clone()) {
                                    global_strings.push(val.clone());
                                }
                                format!("str_{}", val.to_lowercase().chars().map(|a| if a.is_alphanumeric() {a} else {'_'}).collect::<String>())
                            },
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                };

                let register = register(reg);

                format!(";; VALUE {val}\n\tmov {register}, {str_val}\n")
            },
            Inst::Ret(reg) => {
                let register = register(reg);
                if register != "rax" {
                    format!(";; RETURN\n\tmov rax, {register}\n\tleave\n\tret\n")
                } else {
                    ";; RETURN\n\tleave\n\tret\n".into()
                }
            },
            Inst::UnOp(reg, sz, op) => {
                match op {
                    UnaryOp::Not => format!(";; NOT\n\ttest {r}, {r}\n\tsetz {r}\n", r = register_sz(reg, sz)),
                }
            },
            Inst::BinOp(reg, sz, op) => {
                match op {
                    BinaryOp::Add => format!(";; ADD\n\tadd {}, {}\n",  register(reg.0), register(reg.1)),
                    BinaryOp::Sub => format!(";; SUB\n\tsub {}, {}\n",  register(reg.0), register(reg.1)),
                    BinaryOp::Mul => format!(";; MUL\n\timul {}, {}\n", register(reg.0), register(reg.1)),
                    BinaryOp::Div => format!(";; DIV\n\txor rdx, rdx\n\tmov rax, {}\n\tidiv {}\n", register(reg.0), register(reg.1)),
                    BinaryOp::Eq  => format!(";; EQ\n\tcmp {r0}, {r1}\n\tsete al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::Less => format!(";; LESS\n\tcmp {r0}, {r1}\n\tsetl al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::Greater => format!(";; GREATER\n\tcmp {r0}, {r1}\n\tsetg al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::LessEq => format!(";; LESS OR EQUAL\n\tcmp {r0}, {r1}\n\tsetle al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::GreaterEq => format!(";; LESS\n\tcmp {r0}, {r1}\n\tsetge al\n\tmov {rr}, al\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz), rr = register_sz(reg.0, 1)),
                    BinaryOp::Mod => format!(";; MOD\n\tmov rax, {r0}\n\txor rdx, rdx\n\tidiv {r1}\n\tmov {r0}, rdx\n", r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz)),
                    BinaryOp::BoolAnd  => format!(";; AND\n\tand {r0}, {r1}\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz)),
                    BinaryOp::BoolOr  => format!(";; AND\n\tor {r0}, {r1}\n",   r0 = register_sz(reg.0, sz), r1 = register_sz(reg.1, sz)),

                }
            },
            Inst::VarSet(reg, sz, index) => {
                format!(";; SET VAR {index}\n\tsub rsp, {ind}\n\tmov {tp} [rbp-{ind}], {}\n", register_sz(reg, sz), ind = index, tp = datatype(sz))
            },
            Inst::Var(reg, index, sz, is_ref) => {
                if is_ref {
                    format!(";; GET VAR REF {index}\n\tmov {r}, rbp\n\tsub {r}, {}\n", index, r = register(reg))
                } else {
                    let mut size = sz;
                    let mut string = String::new();
                    let mut i = 0;
                    while size > 8 {
                        let _ = writeln!(string, "\tmov {}, [rbp-{}]", register(reg+i/8), index+i);
                        size -= 8;
                        i += 8;
                    }
                    format!(";; GET VAR {index}\n{string}\tmov {}, [rbp-{}]\n", register_sz(reg+i/8, size), index+i)
                }
            },
            Inst::Intrinsic(iname, fname) => {
                if ! intrinsic_labels.contains(&iname) {
                    intrinsic_labels.push(iname.clone());
                    format!(";; INTRINSIC {iname}\n{fname}: \n\tjmp intrinsic_{iname}\nintrinsic_{iname}: \n{}\n", intrinsics().get(iname.as_str()).unwrap())
                } else {
                    format!(";; INTRINSIC {iname}\n{fname}: \n\tjmp intrinsic_{iname}\n")
                }
            },
            Inst::RetVal(reg) => {
                if reg != 0 {
                    format!(";; GET RETURN VALUE\n\tmov {}, rax\n", register(reg))
                } else {
                    "".into()
                }
            },
            Inst::Arr(reg, size) => {
                global_arrays.push(size);
                arr_index += 1;
                format!(";; GET ARRAY\n\tmov {r0}, arr_{arr}\n", arr = arr_index -1, r0 = register(reg))
            },
            Inst::Index(reg0, reg1, sz_vec, is_ref) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                if is_ref {
                    format!(";; ARRAY INDEX\n\tadd {r1}, 8\n\tadd {r1}, {r0}\n\tmov {r0}, {r1}\n")
                } else {
                    let mut string: String = String::new();
                    string.push_str(";; ARRAY INDEX\n");
                    let rr = register(sz_vec.len());
                    string.push_str(format!("\tadd {r1}, 8\n\tadd {r1}, {r0}\n\tmov {rr}, rbx\n").as_str());
                    let mut offset = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = register_sz(reg0 + off, *sz);
                        string.push_str(format!("\tmov {r}, [{rr}+{offset}]\n").as_str());
                        offset += sz;
                    }

                    string                  
                }
            },
            Inst::ArraySet(reg0, reg1, reg2, sz_vec) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                let r2 = register(reg2);
                
                if sz_vec.len() == 1 {
                    let r0s = register_sz(reg0, sz_vec[0]);
                    let r1s = register_sz(reg1, sz_vec[0]);
                    format!(";; SET ARRAY INDEX\n\tpush {r0}\n\tmov {r0}, {r2}\n\tadd {r0}, 8\n\tadd {r0s}, {r1s}\n\tpop {r1}\n\tmov [{r0}], {r1s}\n")
                } else {
                    let mut string: String = String::new();
                    string.push_str(";; SET ARRAY INDEX\n");
                    for (off, _) in sz_vec.iter().enumerate() {
                        let r = register(reg0+(sz_vec.len() - off-1));
                        string.push_str(format!("\tpush {r}\n").as_str());
                    }
                    
                    string.push_str(format!("\tmov {r0}, {r2}\n\tadd {r0}, 8\n\tadd {r0}, {r1}\n").as_str());
                    let mut offset = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = register(reg0+off+1);
                        let rs = register_sz(reg0+off+1, *sz);
                        string.push_str(format!("\tpop {r}\n\tmov [{r0}+{}], {rs}\n", offset).as_str());
                        offset += sz;
                    }

                    string
                }
            },
            Inst::Global(reg0, name, sz, off, is_ref) => {
                if is_ref {
                    format!(";; GET GLOBAL REF\n\tmov {r}, global_{name}\n\tadd {r}, {off}\n", r = register_sz(reg0, sz))
                } else {
                    format!(";; GET GLOBAL\n\tmov {}, [global_{name}+{off}]\n", register_sz(reg0, sz))
                }
            },
            Inst::GlobalSet(reg0, name, sz, offset) => {
                format!(";; SET GLOBAL\n\tmov {tp} [global_{name}+{offset}], {}\n", register_sz(reg0, sz), tp = datatype(sz))
            },
            Inst::Jump(name) => {
                format!(";; JUMP\n\tjmp {name}\n")
            },
            Inst::Field(reg, off, _) => {
                let r0 = register(reg);
                format!(";; GET STRUCT FIELD\n\tadd {r0}, {off}\n\tmov {r0}, [{r0}]\n")
            },
            Inst::SetField(reg0, reg1, off, sz) => {
                let r0 = register(reg0);
                let r1 = register_sz(reg1, sz);
                format!(";; SET STRUCT FIELD\n\tadd {r0}, {off}\n\tmov [{r0}], {r1}\n")
            }
            Inst::Deref(reg) => {
                format!(";; DEREFERENCE\n\tmov {r}, [{r}]\n", r = register(reg))
            },
            
        }.as_str())
    }
    ret.push_str("_end:\n;; EXIT\n\tmov rdi, rax\n\tmov rax, 60\n\tsyscall\nsection .data\nARGS:\n\tresb 8\n");

    for a in global_strings {
        ret.push_str(format!("str_{}:\n\tdq {}\n\tdb `{a}`\n", a.to_lowercase().chars().map(|a| if a.is_alphanumeric() {a} else {'_'}).collect::<String>(), a.len() - a.matches('\\').count()).as_str());
    }

    for (ind, a) in global_arrays.into_iter().enumerate() {
        ret.push_str(format!("arr_{}:\n\tdq {a}\n\tresb {a}\n", ind).as_str());
    }

    for (val, sz) in globals {
        ret.push_str(format!("global_{val}:\n\tresb {sz}\n").as_str());
    }
    
    ret
}
type PosArgs = Vec<String>;
type GnuArgs = HashMap<String, String>;
type UnixArgs = HashMap<char, String>;

fn parse_args(vec: Vec<String>) -> Result<(PosArgs, GnuArgs, UnixArgs), String> {    
    let mut pos_args = vec![];
    let mut gnu_args = HashMap::new();
    let mut unix_args = HashMap::new();

    let mut tmp_gnu_name = String::new();
    let mut tmp_unix_name = 'X';

    const NONE: usize = 0;
    const GNU:  usize = 1;
    const UNIX: usize = 2;

    let mut state = NONE;
    
    for a in vec {
        if a.starts_with("--") {
            if a.len() == 2 {
                continue;
            }
            tmp_gnu_name = a.strip_prefix("--").unwrap().to_string();
            state = GNU;
        } else if a.starts_with('-') {
            if a.len() > 2 {
                return Err(format!("short (unix-like) argument `{a}` should only be one character long (e. g. `-o`)"));
            }
            tmp_unix_name = a.chars().nth(1).unwrap();
            state = UNIX;
        } else {
            match state {
                NONE => {
                    pos_args.push(a.clone());
                },
                GNU => {
                    gnu_args.insert(tmp_gnu_name.clone(), a.clone());
                    state = NONE;
                },
                UNIX => {
                    unix_args.insert(tmp_unix_name, a.clone());
                    state = NONE;
                },
                _ => {
                    return Err("invalid state".into());
                }
            }
        }
    }
    Ok((pos_args, gnu_args, unix_args))
}

fn main() {
    let start_time = SystemTime::now();
    
    let mut args_ = args();
    let arg0 = args_.next().unwrap_or_else(|| {"<program>".into()});

    let mut error: bool = false;

    let vec: Vec<String> = args_.collect();
    let (pos_args, gnu_args, unix_args) = match parse_args(vec) {
        Ok(a) => a,
        Err(e) => {
            println!("ERROR during argument parsing: {e}");
            return;
        },
    };

    let verbose = if gnu_args.contains_key("verbose") {
        &gnu_args["verbose"]
    } else if unix_args.contains_key(&'v') {
        &unix_args[&'v']
    } else {
        "1"
    }.parse::<usize>().unwrap_or(0);

    let filename: String = match pos_args.get(0) {
        Some(a) => a.to_string(),
        None => {
            eprintln!("{esc}[31mERROR: <filename> not given\n{esc}[0mUsage: {arg0} {esc}[31m<filename>{esc}[0m", esc = 27 as char);
            exit(1);
        },
    };

    let path: String = filename.chars().rev().skip_while(|x| x != &'/').collect::<String>().chars().rev().collect();
    let name: String =  if gnu_args.contains_key("output") {
        gnu_args["output"].clone()
    } else if unix_args.contains_key(&'o') {
        unix_args[&'o'].clone()
    } else {
        filename.chars().rev().skip_while(|x| x != &'.').skip(1).take_while(|x| x != &'/').collect::<String>().chars().rev().collect()
    };
    if verbose > 2 {
        println!("PATH: {path}, NAME: {name}");
    }

    let mut parser: Parser = Parser::new(filename.clone()).unwrap_or_else(|a| {
        eprintln!("Error reading file: {}", a);
        exit(1);
    });

    if verbose > 0 {
        eprintln!("[*] generating AST");
    }

    let mut a = parser.parse(verbose);
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
        if verbose > 1 {
            eprintln!("\nAST NODE: \n{:#?}", ast);
        }
        if verbose > 2 {
            eprintln!("--- AST (converted back to code) ---\n{}\n", ast);
        }
        if verbose > 0 {
            eprintln!("[*] type-checking");
        }
        match check(ast, parser.lexer) {
            Err(e) => {
                for a in e {
                    let (t, v) = a;
                    eprintln!("{}: {}", t, v);
                };
                checked = false;
                error = true;
            },
            Ok((g, a, f)) => {
                aliases = a;
                globals = g;
                let _functions = f;
                checked = true
            },
        };
    } else {
        checked = false;
    }

    if checked {
        if let Ok(ast) = a {
            if verbose > 0 {
                println!("[*] generating intermediate represantation");
            }
            
            let (intermediate, globals) = intermediate(ast.1, &mut HashMap::new(), &globals, aliases, 0, 1, true);
            if verbose > 2 {
                println!("{:#?}", intermediate);
            }
            
            if verbose > 0 {
                println!("[*] generating assembly");
            }
            let asm = generate(intermediate, &globals);
            if verbose > 1 {
                println!("{}", asm);
            }

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
            if verbose > 0 {
                println!("[*] CMD: nasm -felf64 {asm_path}");
            }
            
            let nasm =  Command::new("nasm").arg("-felf64").arg(asm_path).output().unwrap();
            if ! nasm.status.success() {
                eprintln!("ERROR executing nasm: \n{}", std::str::from_utf8(&nasm.stderr).unwrap());
                error = true;
            }

            let mut obj_path = path.clone();
            obj_path.push_str(&name);
            obj_path.push_str(".o");

            let mut outfile = path;
            outfile.push_str(&name);

            if verbose > 0 {
                println!("[*] CMD: ld -o {outfile} {obj_path}");
            }
            
            let ld = Command::new("ld").arg("-o").arg(outfile.clone()).arg(obj_path).output().unwrap();
            if ! ld.status.success() {
                eprintln!("ERROR executing ld:\n{}", std::str::from_utf8(&ld.stderr).unwrap());
                error = true;
            }

            let time = SystemTime::now().duration_since(start_time).expect("not possible: backwards time...").as_secs_f32();

            println!("[!] compiled {filename} -> {outfile} in {time}s");
        }
    }


    if error {
        exit(1);
    }
}
