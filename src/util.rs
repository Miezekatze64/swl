use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Write;
use std::hash::{Hash, Hasher};

pub type Error = (ErrorLevel, String);

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorLevel {
    Warn,
    Err,
    Fatal,
    Note,
}

#[macro_export]
macro_rules! error_str {
    ($lexer:expr, $pos:expr) => {
        | | -> String {
            let (l, c) = $lexer.pos_to_line_char($pos);
            return format!("{COLOR_GREEN}{file}:{line}:{ch}: {COLOR_RESET}",
                           COLOR_GREEN = "\x1b[32m",
                           COLOR_RESET = "\x1b[0m",
                           file = $lexer.filename,
                           line = l+1,
                           ch = c+1,
            )
        }()
    };
}

#[macro_export]
macro_rules! error_arrow {
    ($lexer:expr, $pos:expr, $offset:expr, $msg:expr, $len:expr) => {
        | | -> String {
            let mut rep_len = ErrorLevel::Err.to_string().chars().count() + 2 + $crate::error_str!($lexer, $pos).chars().count() + $offset;
            if $msg.chars().count() < rep_len {
                rep_len -= $msg.chars().count();
                $msg.to_owned() + &" ".repeat(rep_len) + &("^".repeat($len))
            } else {
                $msg.to_owned() + &"\n" + &" ".repeat(rep_len) + &("^".repeat($len))
            }
        }()
    }
}

/// return an error string in the following format:
///     error: ./file.name:420:: This is a error
/// argument 2 (msg_fmt) is a format string
#[macro_export]
macro_rules! error {
    ($lexer:expr, $pos:expr, $msg_fmt:literal) => {
        | | -> String {
            return format!("{err}{msg} (at {f}:{l})",
                           err = $crate::error_str!($lexer, $pos),
                           msg = format!($msg_fmt),
                           f = file!(),
                           l = line!()
            )
        }()
    }
}


impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            ErrorLevel::Note => "note",
            ErrorLevel::Warn => "warning",
            ErrorLevel::Err => "error",
            ErrorLevel::Fatal => "FATAL Error",
        })
    }
}

pub fn is_valid_type(val: &str) -> bool {
    val == "int" || val == "float" || val == "char" ||
        val == "void" || val == "bool" || val == "unchecked" || val == "long"
}


#[derive(Debug, Clone, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    Custom(String),
    Array(Box<Type>/*, u32*/),
    Pointer(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Invalid,
    Struct(String, Vec<(String, (Type, usize))>),
    Var(String),
    Bounded(String),
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
            Type::Array(a) => {
                a.hash(state);
                23.hash(state);
            },
            Type::Pointer(a) => {
                a.hash(state);
                17.hash(state);
            },
            Type::Invalid => 0.hash(state),
            Type::Struct(x, map) => {
                map.iter().map(|(a, b)| (a.clone(), b.clone()))
                    .collect::<Vec<(String, (Type, usize))>>().hash(state);
                x.hash(state);
            },
            Type::Function(args, ret) => {
                args.hash(state);
                ret.hash(state);
            },
            Type::Var(a) => ("_".to_string() + a).hash(state),
            Type::Bounded(a) => a.hash(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Long,
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
                PrimitiveType::Long => "long",
                PrimitiveType::Float => "float",
                PrimitiveType::Char => "char",
                PrimitiveType::Void => "void",
                PrimitiveType::Bool => "bool",
                PrimitiveType::Unchecked => "unchecked",
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
                
                let mut s = format!("struct {name} {{");
                for (i, (n, (t, _))) in fields.iter().enumerate() {
                    write!(s, "{t} {n}")?;
                    if i < fields.len() - 1 {
                        s += ", ";
                    }
                }
                s += "}";
                s
            },
            Type::Function(args, ret_type) => {
                let mut s = "function<(".to_string();
                for (i, a) in args.iter().enumerate() {
                    write!(s, "{a}")?;
                    if i < args.len() - 1 {
                        s += ", ";
                    }
                }
                s += ") -> ";
                write!(s, "{ret_type}>")?;
                s
            },
            Type::Var(a) => a.to_string(),
            Type::Bounded(a) => format!("Bounded<{a}>"),
        }.as_str())
    }
}

impl Type {
    pub fn dealias(&self, aliases: &HashMap<String, Type>) -> Type {
        match self {
            Type::Primitive(_)        => self.clone(),
            Type::Custom(val)         => {
                if aliases.contains_key(val) {
                    aliases.get(val).unwrap().clone()
                } else {
                    self.clone()
                }
            },
            Type::Array(a)            => Type::Array(Box::new(a.dealias(aliases))),
            Type::Pointer(a)          => Type::Pointer(Box::new(a.dealias(aliases))),
            Type::Function(args, ret) => {
                Type::Function(args.iter().map(|x| x.dealias(aliases)).collect(), Box::new(ret.dealias(aliases)))
            },
            Type::Invalid             => Type::Invalid,
            Type::Struct(name, fields)        => {
                Type::Struct(name.clone(), fields.iter().
                             map(|(nm, (tp, sz))|(nm.clone(), (tp.dealias(aliases), *sz))).collect())
            },
            Type::Var(val)            => {
                if aliases.contains_key(val) {
                    aliases.get(val).unwrap().clone()
                } else {
                    self.clone()
                }
            },
            Type::Bounded(_) => self.clone(),
        }
    }

    pub fn is_numeric(&self) -> bool {
        self == &Type::Primitive(PrimitiveType::Char) || self == &Type::Primitive(PrimitiveType::Float) || self == &Type::Primitive(PrimitiveType::Int)
    }

    pub fn is_integral(&self) -> bool {
        self == &Type::Primitive(PrimitiveType::Char) || self == &Type::Primitive(PrimitiveType::Int)
    }

    pub fn is_compatible(&self, type_r: &Type, aliases: &HashMap<String, Type>)
                -> bool {
        let a = self.dealias(aliases);
        let b = type_r.dealias(aliases);

        // UNSAFE: DON'T USE THIS IF NOT REALLY NEEDED
        if a == Type::Primitive(PrimitiveType::Unchecked) ||
            b == Type::Primitive(PrimitiveType::Unchecked) {
            return true;
        }

        if a == b || (a == Type::Primitive(PrimitiveType::Float) &&
                     b == Type::Primitive(PrimitiveType::Int)) ||
            ((a == Type::Primitive(PrimitiveType::Int) && b == Type::Primitive(PrimitiveType::Int)) ||
              b == Type::Primitive(PrimitiveType::Int) && a == Type::Primitive(PrimitiveType::Int)) {
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
                        if ! b_fields.iter().any(|(n, _)| n == string) {
                            return false;
                        }
                        comp &= typ.0.is_compatible(&b_fields.iter().find(|(s, _)| s == string).unwrap().1.0,
                                                    aliases);
                    }
                    for (string, _) in b_fields {
                        if ! a_fields.iter().any(|(n, _)| n == &string) {
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

    pub fn size(&self, aliases: &HashMap<String, Type>) -> usize {
        match self {
            Type::Primitive(a) => match a {
                PrimitiveType::Int => 4,
                PrimitiveType::Long => 8,
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
            // = pointer -> 8
            Type::Function(_, _) => 8,
            Type::Var(a) => {
                unreachable!("unevavluated type-variable `{a}`")
            },
            Type::Bounded(a) => unreachable!("unevaluated bounded generic from typeclass `{a}`"),
        }
    }

    pub fn to_label(&self) -> String {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash = hasher.finish().to_string();
        "__member__".to_string() + &hash + "_"
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Op {
    Unary(UnaryOp),
    Binary(BinaryOp),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    BoolAnd,
    BoolOr,
    BitwiseAnd,
    BitwiseOr,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Neq,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOp {
    Not,
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
            BinaryOp::Neq          => "!=",
            BinaryOp::Less         => "<",
            BinaryOp::LessEq       => "<=",
            BinaryOp::Greater      => ">",
            BinaryOp::GreaterEq    => ">=",
            BinaryOp::Mod          => "%",
            BinaryOp::BitwiseAnd   => "&",
            BinaryOp::BitwiseOr    => "|",
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
    pub fn with_type(&self, t: &Type, _aliases: &HashMap<String, Type>) -> Type {
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
    pub fn combine_type(&self, a: &Type, b: &Type, aliases: &HashMap<String, Type>)
                    -> Type {
        match self {
            Op::Unary(_) => {
                unreachable!()
            },
            Op::Binary(bin) => match bin {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div |
                BinaryOp::Mod |  BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr => {
                    match a {
                        Type::Primitive(ref v) => {
                            match v {
                                PrimitiveType::Int | PrimitiveType::Long => {
                                    let rb = b.dealias(aliases);
                                    if rb == Type::Primitive(PrimitiveType::Int) || rb == Type::Primitive(PrimitiveType::Char) {
                                        a.clone()
                                    } else if rb == Type::Primitive(PrimitiveType::Long) {
                                        rb
                                    } else {
                                        Type::Invalid
                                    }
                                }
                                PrimitiveType::Float =>
                                    if a.is_compatible(b, aliases) {
                                        a.clone()
                                    } else {
                                        Type::Invalid
                                    },
                                PrimitiveType::Char => {
                                    let rb = b.dealias(aliases);
                                    if rb == Type::Primitive(PrimitiveType::Int) || rb == Type::Primitive(PrimitiveType::Char) {
                                        a.clone()
                                    } else {
                                        Type::Invalid
                                    }
                                },
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
                        PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Long | 
                        PrimitiveType::Char => if a.is_compatible(b, aliases) {
                            Type::Primitive(PrimitiveType::Bool)
                        } else {
                            Type::Invalid
                        },
                        _ => Type::Invalid,
                    },
                    _ => Type::Invalid,
                },
                BinaryOp::Eq | BinaryOp::Neq => if (a.dealias(aliases) == b.dealias(aliases)) ||
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

pub fn parse_type(string: String) -> Option<Type> {
    if string.is_empty() || !string.chars().next().unwrap().is_alphabetic() {
        return None;
    }
    match string.as_str() {
        "int"       => Some(Type::Primitive(PrimitiveType::Int)),
        "long"      => Some(Type::Primitive(PrimitiveType::Long)),
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


// indents a strinig by 4 spaces
pub fn indent(string: String) -> String {
    let mut ret = String::new();
    for line in string.lines() {
        ret += "    ";
        ret += line;
        ret += "\n";
    }
    ret
}


type PosArgs = Vec<String>;
type GnuArgs = HashMap<String, String>;
type UnixArgs = HashMap<char, String>;

pub fn parse_args(vec: Vec<String>) -> Result<(PosArgs, GnuArgs, UnixArgs), String> {    
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
            match state {
                NONE => {},
                GNU => {
                    gnu_args.insert(tmp_gnu_name.clone(), String::new());
                },
                UNIX => {
                    unix_args.insert(tmp_unix_name, String::new());
                },
                _ => {
                    return Err("invalid state".into());
                }
            }
            if a.len() == 2 {
                continue;
            }
            tmp_gnu_name = a.strip_prefix("--").unwrap().to_string();
            state = GNU;
        } else if a.starts_with('-') {
            match state {
                NONE => {},
                GNU => {
                    gnu_args.insert(tmp_gnu_name.clone(), String::new());
                },
                UNIX => {
                    unix_args.insert(tmp_unix_name, String::new());
                },
                _ => {
                    return Err("invalid state".into());
                }
            }
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

    match state {
        NONE => {},
        GNU => {
            gnu_args.insert(tmp_gnu_name, String::new());
        },
        UNIX => {
            unix_args.insert(tmp_unix_name, String::new());
        },
        _ => {
            return Err("invalid state".into());
        }
    }
    
    Ok((pos_args, gnu_args, unix_args))
}

