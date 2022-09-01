use std::{fs, path::Path};

use crate::{preprocessor, Target};

use {crate::{util, util::{indent, PrimitiveType, Type, BinaryOp, UnaryOp, ErrorLevel, Op, Error}, error}, std::{io, collections::HashMap}, crate::lexer::{TokenType, Lexer, Token}};

#[derive(Debug, Clone)]
pub struct Parser {
    pub lexer       : Lexer,
    pub links       : Vec<String>,
    pub linked_libs : Vec<String>,
    pub target      : Target,
}

#[derive(Debug, Clone)]
pub struct ASTNode(pub usize, pub ASTNodeR);

type Functions   = HashMap<(Option<Type>, String), (Vec<Type>, Type)>;

#[derive(Debug, Clone)]
pub enum ASTNodeR {
    Block(Vec<ASTNode>),
    VarDec(bool, Type, String),
    VarInit(String, Expression, Option<Type>),
    VarOp(String, BinaryOp, Expression),
    VarDecInit(bool, Type, String, Expression),
    FunctionCall(Box<Expression>, Vec<Expression>),
    If(Expression, Box<ASTNode>, Option<Box<ASTNode>>),
    FunctionDecl(Option<Type>, String, Vec<(Type, String)>, Type, Box<ASTNode>),
    Return(Expression),
    TypeAlias(String, Type),
    Intrinsic(String, String, Vec<(Type, String)>, Type),
    ArrIndexInit(Expression, Expression, Expression, Vec<usize>),
    While(Expression, Box<ASTNode>),
    Struct(String, Vec<(String, (Type, usize))>),
    SetField(Expression, String, Expression, Option<Type>, bool),
    Include(String, Box<ASTNode>, Lexer),
    Break(),
    MemberFunction(Type, Expression, String, Vec<Expression>),
    TypeClass(String, String, Functions),
    Instance(String, Type, Vec<ASTNode>),
    DerefSet(Expression, Expression),
    Extern(String, String, Vec<(Type, String)>, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub usize, pub ExpressionR, pub Option<Type>);

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionR {
    T(Box<Expression>, Op, Box<Expression>, u32),
    Val(Type, String),
    Var(String),
    F(Box<Expression>, Vec<Expression>),
    Arr(Vec<Expression>),
    Undef,
    ArrAlloc(Type, usize),
    Index(Box<Expression>, Box<Expression>, Vec<usize>),
    UnaryOp(UnaryOp, Box<Expression>, u32),
    StructLiteral(String, HashMap<String, Expression>),
    StructField(Box<Expression>, String, Option<Type>, bool),
    Ref(Box<Expression>),
    Deref(Box<Expression>),
    MemberFunction(Type, Box<Expression>, String, Vec<Expression>),
    Cast(Type, Box<Expression>, Option<Type>),
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
            ASTNodeR::If(pred, block1, block2) => {
                if block2.is_some() {
                    write!(f, "if ({pred}) {{\n{}}} else {{\n{}}}", indent(block1.to_string()), indent(block2.as_ref().unwrap().to_string()))
                } else {
                    write!(f, "if ({pred}) {{\n{}}}", indent(block1.to_string()))
                }
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
            ASTNodeR::Extern(ename, name, args, ret) => {
                write!(f, "extern {ename} as func {name}(")?;
                for (index, (arg_type, arg_name)) in args.iter().enumerate() {
                    write!(f, "{arg_type} {arg_name}")?;
                    if index < args.len()-1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") ")?;
                if *ret != Type::Primitive(PrimitiveType::Void) {
                    write!(f, "-> {ret} ")?;
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
                write!(f, "while {cond} {{\n{}}}", indent(block.to_string()))
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
            ASTNodeR::SetField(left, field, right, _,deref) => {
                if *deref {
                    write!(f, "{left}->{field} = {right};")
                } else {
                    write!(f, "{left}.{field} = {right};")
                }
            },
            ASTNodeR::Include(file, _, _) => {
                write!(f, "include \"{file}\";")
            },
            ASTNodeR::Break() => {
                write!(f, "break;")
            }
            ASTNodeR::MemberFunction(_, lexpr, name, args) => {
                write!(f, "{lexpr}.{name}(")?;
                for (index, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if index < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ");")
            }
            ASTNodeR::TypeClass(name, arg, funcs) => {
                writeln!(f, "typeclass {name} {arg} {{")?;
                for func in funcs {
                    write!(f, "    func {}(", func.0.1)?;
                    let (args, ret) = func.1.clone();
                    for (index, arg) in args.iter().enumerate() {
                        write!(f, "{arg}")?;
                        if index < args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")?;
                    if ret != Type::Primitive(PrimitiveType::Void) {
                        write!(f, " -> {ret}")?;
                    }
                    let from = func.0.0.as_ref();
                    if from.is_some() {
                        write!(f, " from {}", from.unwrap())?;
                    }
                    writeln!(f, ";")?;
                }
                writeln!(f, "}}")
            },
            ASTNodeR::Instance(name, arg, funcs) => {
                write!(f, "instance {name} {arg} {{")?;
                for func in funcs {
                    writeln!(f, "{}", indent(func.to_string()))?;
                }
                writeln!(f, "}}")?;
                Ok(())
            },
            ASTNodeR::DerefSet(l, r) => {
                writeln!(f, "deref {l} = {r};")
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
                    Type::Primitive(PrimitiveType::Char) => write!(f, "'{}'", val.escape_default()),
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
                    Type::Function(_, _) => write!(f, "[INVALID] This sould be a lambda functino not a 'function literal'..."),
                    Type::Var(val) => write!(f, "{val}[INVALID, TYPE VARIABLE]"),
                    Type::Bounded(a, b) => write!(f, "Bounded<{a}, {b}>[INVALID]"),
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
            ExpressionR::MemberFunction(_, lexpr, name, args) => {
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
            ExpressionR::StructField(expr, field, _, deref) => {
                if *deref {
                    write!(f, "{expr}->{field}")
                } else {
                    write!(f, "{expr}.{field}")
                }
            },
            ExpressionR::Ref(expr) => {
                write!(f, "ref {expr}")
            },
            ExpressionR::Deref(expr) => {
                write!(f, "deref {expr}")
            },
            ExpressionR::Cast(tp, expr, _) => {
                write!(f, "as[{tp}, {expr}]")
            },
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenType::Int => "integer literal",
            TokenType::Long => "long literal",
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
            "!=" => Ok(Op::Binary(BinaryOp::Neq)),
            "<"  => Ok(Op::Binary(BinaryOp::Less)),
            ">"  => Ok(Op::Binary(BinaryOp::Greater)),
            "<=" => Ok(Op::Binary(BinaryOp::LessEq)),
            ">=" => Ok(Op::Binary(BinaryOp::GreaterEq)),
            "&&" => Ok(Op::Binary(BinaryOp::BoolAnd)),
            "||" => Ok(Op::Binary(BinaryOp::BoolOr)),
            "&" => Ok(Op::Binary(BinaryOp::BitwiseAnd)),
            "|" => Ok(Op::Binary(BinaryOp::BitwiseOr)),
            "!"  => Ok(Op::Unary(UnaryOp::Not)),
            _ => Err((ErrorLevel::Fatal,
                     format!("FATAL: invalid operator `{s}` set from lexer"))),
        }
    }

    fn get_precedence(op_type: Op) -> u32 {
        match op_type {
            Op::Binary(bin_op) => match bin_op {
                BinaryOp::BoolAnd | BinaryOp::BoolOr => 0,
                BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Less |
                BinaryOp::Greater | BinaryOp::GreaterEq | BinaryOp::LessEq => 1,
                BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr => 2,
                BinaryOp::Add | BinaryOp::Sub => 3,
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 4,
                // PARENS => 99
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
    pub fn new(src: Vec<char>, filename: String, verbose: usize, target: Target) -> Result<Self, io::Error> {
        let (contents, l, ll) = preprocessor::preprocess(src, filename.clone(), target);
        Ok(Parser {
            lexer: Lexer::new(filename, contents, verbose)?,
            links: l,
            linked_libs: ll,
            target,
        })
    }

    pub fn parse(&mut self, verbose: usize) -> Result<ASTNode, Vec<Error>> {
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
                        is_root, verbose, &[";"]) {
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
                if ! is_root {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected end-of-file")));
                }
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
                ExpressionR::MemberFunction(..) | ExpressionR::Cast(..)
=> {
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

        let tk = subexpr.1;
        
        let mut nleft_expr = subexpr.0;
        if let ExpressionR::Undef =  nleft_expr.1  {
            return Ok((*nleft_expr, tk));
        }

        loop {
            if let Expression(_, ExpressionR::Undef, _) = *nleft_expr {
                return Ok((*nleft_expr, tk));
            }
            left_expr = nleft_expr.clone();

            // parse operation
            let tk_op = get_peek_token!(self.lexer, errors);

            if seperators.contains(&tk_op.value.as_str()) {
                self.lexer.next_token().unwrap();
                return Ok((*nleft_expr, tk_op));
            }
            
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
                } else if tk_op.ttype == TokenType::Special && tk_op.value == "(" {
                    self.lexer.next_token().unwrap();
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
                    return Ok((Expression(nleft_expr.0, ExpressionR::F(nleft_expr, args), None), tk_op));
                } else {
                    let val = tk_op.clone().value;
                    errors.push((ErrorLevel::Err, error!(
                        self.lexer, tk_op.pos, "invalid token `{val}`\n\t`.`, `(` or `;` expected")));
                    return Err(errors);
                };


            self.lexer.next_token().unwrap();

            // parse right side
            let subexpr_r = self.parse_subexpr(seperators)?;

            if let Expression(_, ExpressionR::Undef, _) = *subexpr_r.0 {
                return Ok((*left_expr,subexpr_r.1));
            }

            let right_expr = subexpr_r.0;

            nleft_expr = check_precedence_and_update(
                op, right_expr.clone(), &mut left_expr);
        }
    }

    fn parse_block_statement(&mut self, is_root: bool, verbose: usize, seperators: &[&'static str])
                             -> Result<Option<ASTNode>, Vec<Error>> {
        // save position for type parsing
        let start_pos = self.lexer.pos;

        let mut errors: Vec<Error> = vec![];
        let mut token = get_token!(self.lexer, errors);
        let val = token.clone().value;

        fn parse_var_dec(parser: &mut Parser, typ: Type, is_root: bool, seperators: &[&'static str])
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
                    if seperators.contains(&next_token.value.as_str()) {
                        Ok(Some(ASTNode(pos, ASTNodeR::VarDec(
                            is_root, typ, ident))))
                    } else {
                        Err(vec![(ErrorLevel::Err, error!(
                            parser.lexer, next_token.pos,
                            "unexpected token `{ntv}`:\n\t`;` or `=` expected."))])
                    }
                },
                TokenType::Operator => {
                    if next_token.value == "=" {
                        // variable initialization
                        let expr = parser.parse_expr(seperators)?.0;
                        Ok(Some(ASTNode(pos, ASTNodeR::VarDecInit(
                            is_root, typ, ident, expr))))
                    } else {
                        Err(vec![(ErrorLevel::Err, error!(
                            parser.lexer, next_token.pos,
                            "unexpected token `{ntv}`:\n\t`;` or `=` expected."))])
                    }
                },
                _ => Err(vec![(ErrorLevel::Err, error!(
                    parser.lexer, next_token.pos,
                    "unexpected token `{ntv}`:\n\t`;` or `=` expected."))]),
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
                               mut errors: Vec<(ErrorLevel, String)>)
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


            if errors.is_empty() {
                Ok((args, ret_type))
            } else {
                Err(errors.to_owned())
            }
        }

        fn parse_if(parser: &mut Parser, pos: usize, verbose: usize,
                    mut errors: Vec<Error>)
                    ->Result<Option<ASTNode>, Vec<Error>> {
            // next token: '('
            err_ret!(parser.expect(Some(TokenType::Special),
                                   Some("(".into())), errors);

            let predicate = parser.parse_expr(&[")"])?;

            // next token: '{'
            err_ret!(parser.expect(Some(TokenType::Special),
                                   Some("{".into())), errors);

            let block = parser.parse_block(pos, verbose)?;

            // check for else
            let next_token = get_peek_token!(parser.lexer, errors);
            let block2 = if next_token.ttype == TokenType::Keyword && next_token.value == "else" {
                    parser.lexer.next_token().unwrap();

                    let nt = get_token!(parser.lexer, errors);
                    if nt.ttype == TokenType::Special && nt.value == "{" {
                    Some(Box::new(parser.parse_block(pos, verbose)?))
                } else if nt.ttype == TokenType::Keyword && nt.value == "if" {
                    Some(Box::new(ASTNode(nt.pos, ASTNodeR::Block(vec![
                        parse_if(parser, nt.pos, verbose, errors)?.unwrap()
                    ]))))
                } else {
                    let ntv = nt.value;
                    errors.push((ErrorLevel::Err, error!(parser.lexer, nt.pos, "unexpected token `{ntv}`, block or `if` expected.")));
                    return Err(errors);
                }
            } else {
                None
            };

            Ok(Some(ASTNode(pos, ASTNodeR::If(predicate.0, Box::new(block), block2))))
        }


        match token.ttype {
            TokenType::Int | TokenType::Long | TokenType::Float | TokenType::Char |
            TokenType::String | TokenType::Bool => {
                errors.push((ErrorLevel::Err,
                             error!(self.lexer, token.pos, "invalid literal")));
                return self.parse_block_statement(is_root, verbose, seperators);
            },
            TokenType::Ident => {
                let expr = self.parse_ident_expr(val.clone(),
                                                 token.pos,  seperators)?;
                let exp = self.parse_member(*expr, seperators)?.1;

                match exp {
                    ExpressionR::Var(..) => {
                        let var_token = get_token!(self.lexer, errors);
                        let var_val = var_token.value;

                        if var_token.ttype == TokenType::Operator &&
                            var_val == "=" {
                                let expr = self.parse_expr(seperators)?.0;
                                return Ok(Some(ASTNode(
                                    token.pos,ASTNodeR::VarInit(
                                        val, expr, None))));
                            } else if var_token.ttype == TokenType::Operator &&
                            var_val.ends_with('=') {
                                let expr = self.parse_expr(seperators)?.0;
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
                                let right_expr = self.parse_expr(seperators)?.0;
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
                    ExpressionR::StructField(strct, field, _, deref) => {
                        err_ret!(self.expect(Some(TokenType::Operator),
                                             Some("=".into())), errors);
                        let expr = self.parse_expr(seperators)?.0;
                        return Ok(Some(ASTNode(token.pos, ASTNodeR::SetField(*strct, field, expr, None, deref))));
                    },
                    ExpressionR::MemberFunction(tp, expr, name, args) => {
                        return Ok(Some(ASTNode(token.pos, ASTNodeR::MemberFunction(tp, *expr, name, args))));
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
                            let expr = self.parse_expr(seperators)?.0;
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
                    ExpressionR::ArrAlloc(..) => unreachable!(),
                    ExpressionR::Cast(..) => unreachable!(),
                }
            },
            TokenType::Eof => return Ok(None),
            TokenType::Special => match val.as_str() {
                "}" => {
                    if is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unmatched '}}':\n\tno curresponding '{{' foud")));
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
                    let expression = self.parse_expr(seperators)?.0;
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

                    return match parse_var_dec(self, array_type, is_root, seperators) {
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
                    return self.parse_block_statement(is_root, verbose, seperators);
                }
                _ => {},
            },
            TokenType::Operator => {},
            TokenType::Type => {
                self.lexer.pos = start_pos;
                let typ = err_ret!(self.parse_type(), errors);
                
                return match parse_var_dec(self, typ, is_root, seperators) {
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
                    return parse_if(self, token.pos, verbose, errors.clone());
                    
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
                "for" => {
                    if is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "`for` not allowed at top level")));
                        return Err(errors);
                    }
                    // next token: '('
                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors);
                    
                    // initialization
                    let init_node = self.parse_block_statement(false, verbose, &[";"])?;
                    
                    // loop condition
                    let cond_expr = err_add!(self.parse_expr(&[";"]), errors);
                    
                    // incrementor
                    let inc_node = self.parse_block_statement(false, verbose, &[")"])?;

                    // next token: '{'
                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors);

                    // parse block
                    let mut block = self.parse_block(token.pos, verbose)?;
                    if let ASTNode(_, ASTNodeR::Block(ref mut vec)) = block {
                        if let Some(a) = inc_node {
                            vec.push(a);
                        }
                    }
                    let while_node = ASTNode(token.pos, ASTNodeR::While(cond_expr.0, Box::new(block)));

                    let blk_vec = if init_node.is_some() {
                        vec![init_node.unwrap(), while_node]
                    } else {
                        vec![while_node]
                    };
                    
                    let for_node = ASTNode(token.pos, ASTNodeR::Block(blk_vec));
                    
                    return Ok(Some(for_node));
                },
                "func" => {
                    if ! is_root {
                        self.lexer.pos = start_pos;
                        let typ = err_ret!(self.parse_type(), errors);

                        return match parse_var_dec(self, typ, is_root, seperators) {
                            Ok(a) => Ok(a),
                            Err(mut e) => {
                                errors.append(&mut e);
                                Err(errors)
                            },
                        };
                    }
                    
                    // parse function name
                    let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;

                    let mut bounds: HashMap<String, String> = HashMap::new();
                    let ntk = get_peek_token!(self.lexer, errors);
                    if ntk.ttype == TokenType::Operator && ntk.value == "<" {
                        self.lexer.next_token().unwrap();
                        // parse generic bounds
                        loop {
                            let class = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                            let nm = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;

                            if ! nm.starts_with('_') {
                                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "bounded generics / type variables have to start with an `_`")));
                                return Err(errors);
                            }
                            
                            bounds.insert(nm, class);
                            
                            let ntk = get_token!(self.lexer, errors);
                            if ntk.ttype == TokenType::Special && ntk.value == "," {
                                continue;
                            } else if ntk.ttype == TokenType::Operator && ntk.value == ">" {
                                break;
                            } else {
                                let ntv = ntk.value;
                                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{ntv}`, `,` or `>` exptected")));
                                return Err(errors);
                            }
                        }
                    }

                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors).value;
                    
                    let (args_, ret_type_) = err_add!(parse_function_decl(self, errors.clone()), errors);

                    let args = args_.into_iter().map(|(x, n)| (if let Type::Var(ref t) = x {
                        if bounds.contains_key(t) {
                            Type::Bounded(t.to_owned(), bounds[t].clone())
                        } else {
                            x
                        }
                    } else {
                        x
                    }, n)).collect();

                    let ret_type = if let Type::Var(ref t) = ret_type_ {
                        if bounds.contains_key(t) {
                            Type::Bounded(t.to_owned(), bounds[t].clone())
                        } else {
                            ret_type_
                        }
                    } else {
                        ret_type_
                    };

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

                    if ! Path::new(&filename).exists() {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "could not include file `{filename}`: File does not exist")));
                        return Err(errors);
                    }

                    let contents = fs::read_to_string(filename.clone()).expect("File read error: ").chars().collect();
                    let mut file_parser = match Parser::new(contents, filename.clone(), verbose, self.target) {
                        Ok(a) => a,
                        Err(e) => {
                            errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "ERROR during loading of file: {e}")));
                            return Err(errors);
                        },
                    };

                    let ast = match file_parser.parse(verbose) {
                        Ok(a) => a,
                        Err(mut vec) => {
                            errors.append(&mut vec);
                            return Err(errors);
                        },
                    };

                    self.links.append(&mut file_parser.links);
                    self.linked_libs.append(&mut file_parser.linked_libs);

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

                    let mut fields: Vec<(String, (Type, usize))> = vec![];
                    loop {
                        let next_token = err_ret!(self.lexer.peek_token(), errors);
                        if next_token.ttype == TokenType::Special && next_token.value == "}" {
                            self.lexer.next_token().unwrap();
                            break;
                        }
                        let typ = err_ret!(self.parse_type(), errors);
                        let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                        err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors);
                        fields.push((name, (typ, fields.len())));
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

                    let (args, ret_val) = err_add!(parse_function_decl(self, errors.clone()), errors);

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Intrinsic(intr_name, intr_func, args, ret_val))));
                },
                "typeclass" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "typeclasses are only allowed at top-level")));
                        return Err(errors);
                    }

                    // tyoclass name:
                    let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                    
                    // type arg name:
                    let arg = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;

                    // expect {
                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors).value;

                    let mut funcs: Functions = HashMap::new();

                    while get_peek_token!(self.lexer, errors).ttype == TokenType::Keyword {
                        err_ret!(self.expect(Some(TokenType::Keyword), Some("func".into())), errors).value;
                        // expect name:
                        let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                        
                        err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors).value;
                        
                        let (args, ret_type) = err_add!(parse_function_decl(self, errors.clone()), errors);
                        
                        let mut from_type = None;
                        
                        let check_from = get_peek_token!(self.lexer, errors);
                        if check_from.ttype == TokenType::Keyword && check_from.value == "from" {
                            self.lexer.next_token().unwrap();
                            // expect type
                            from_type = Some(err_ret!(self.parse_type(), errors));
                        }

                        err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors).value;

                        funcs.insert((from_type, name),
                                     (
                                         args.iter().
                                             map(|(n, _)| n.clone()).
                                             collect(),
                                         ret_type
                                     )
                        );
                    }

                    // expect }
                    err_ret!(self.expect(Some(TokenType::Special), Some("}".into())), errors).value;
                    
                    return Ok(Some(ASTNode(0, ASTNodeR::TypeClass(name, arg, funcs))));
                },
                "instance" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "typeclasses are only allowed at top-level")));
                        return Err(errors);
                    }

                    // tyoclass name:
                    let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                    
                    // type arg:
                    let arg = err_ret!(self.parse_type(), errors);

                    // expect {
                    err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors).value;

                    let mut funcs: Vec<ASTNode> = vec![];

                    while get_peek_token!(self.lexer, errors).ttype == TokenType::Keyword {
                        err_ret!(self.expect(Some(TokenType::Keyword), Some("func".into())), errors).value;
                        // expect name:
                        let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                        
                        err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors).value;
                        
                        let (args, ret_type) = err_add!(parse_function_decl(self, errors.clone()), errors);
                        
                        let mut from_type = None;
                        
                        let check_from = get_peek_token!(self.lexer, errors);
                        if check_from.ttype == TokenType::Keyword && check_from.value == "from" {
                            self.lexer.next_token().unwrap();
                            // expect type
                            from_type = Some(err_ret!(self.parse_type(), errors));
                        }

                        err_ret!(self.expect(Some(TokenType::Special), Some("{".into())), errors).value;
                        let block = err_add!(self.parse_block(token.pos, verbose), errors);

                        funcs.push(ASTNode(block.0,
                                           ASTNodeR::FunctionDecl(
                                               from_type,
                                               name,
                                               args,
                                               ret_type,
                                               Box::new(block))
                                     )
                        );
                    }

                    // expect }
                    err_ret!(self.expect(Some(TokenType::Special), Some("}".into())), errors).value;
                    
                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Instance(name, arg, funcs))));
                },
                "deref" => {
                    let lexpr = self.parse_expr(&["="])?.0;
                    let rexpr = self.parse_expr(seperators)?.0;

                    return Ok(Some(ASTNode(token.pos, ASTNodeR::DerefSet(lexpr, rexpr))));
                },
                "extern" => {
                    if ! is_root {
                        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "extern function declerations are only allowed at top-level")));
                        return Err(errors);
                    }

                    let mut nm: Option<String> = None;

                    let nxt = get_peek_token!(self.lexer, errors);
                    if nxt.ttype == TokenType::Ident {
                        self.lexer.next_token().unwrap();
                        nm = Some(nxt.value);
                        err_ret!(self.expect(Some(TokenType::Keyword), Some("as".into())), errors);
                    }
                    
                    err_ret!(self.expect(Some(TokenType::Keyword), Some("func".into())), errors);
                    let name = err_ret!(self.expect(Some(TokenType::Ident), None), errors).value;
                    err_ret!(self.expect(Some(TokenType::Special), Some("(".into())), errors);
                    
                    let (args, ret) = parse_function_decl(self, errors.clone())?;
                    err_ret!(self.expect(Some(TokenType::Special), Some(";".into())), errors);

                    let ename = nm.unwrap_or_else(|| name.clone());
                    
                    return Ok(Some(ASTNode(token.pos, ASTNodeR::Extern(ename, name, args, ret))));
                }
                _ => {}
            },
            TokenType::Undef => return self.parse_block_statement(is_root, verbose, &[";"]),
        }
        errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected token `{val}`")));
        Err(errors)
    }

    fn parse_ident_expr(&mut self, ident: String, ident_pos: usize, seperators: &[&'static str]) -> Result<Box<Expression>, Vec<Error>> {
        let mut errors: Vec<Error> = vec![];

        let token = get_peek_token!(self.lexer, errors);
        let val = token.clone().value;

        let res = match token.ttype {
            TokenType::Int | TokenType::Long | TokenType::Float | TokenType::Ident | TokenType::Type | TokenType::Keyword | TokenType::String | TokenType::Char | TokenType::Bool => {
                errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected construct `{ident} {val}`, BlockStatement expected")));
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

                    let var = Expression(ident_pos, ExpressionR::Var(ident), None);
                    let func_expr: Expression = Expression(ident_pos, ExpressionR::F(Box::new(var), args), None);
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
                "."|"->"|"[" => {
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
                "as" => {
                    err_ret!(self.expect(Some(TokenType::Special), Some("[".into())), errors);

                    // parse type
                    let tp = err_ret!(self.parse_type(), errors);

                    // comma
                    err_ret!(self.expect(Some(TokenType::Special), Some(",".into())), errors);

                    // parse size
                    let value = err_add!(self.parse_expr(&["]"]), errors).0;

//                    err_ret!(self.expect(Some(TokenType::Special), Some("]".into())), errors);

                    Ok((Box::new(Expression(token.pos, ExpressionR::Cast(tp, Box::new(value), None), None)), token))
                }
                _ => {
                    errors.push((ErrorLevel::Err, error!(self.lexer, token.pos, "unexpected keyword `{val}`")));
                    Err(errors.clone())
                }
            },
            TokenType::Int => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Int), val), None);
                Ok((Box::new(self.parse_member(expr_, seperators)?), token))

            },
            TokenType::Long => {
                let expr_ = Expression(token.pos, ExpressionR::Val(Type::Primitive(PrimitiveType::Long), val), None);
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
                    let (mut expr_, tk) = self.parse_expr(&[")"])?;
                    if let ExpressionR::T(l, o, r, _) = expr_.1 {
                        expr_.1 = ExpressionR::T(l, o, r, 99);
                    }
                    Ok((Box::new(self.parse_member(expr_, &[")"])?), tk))
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

                if index.0.1 == ExpressionR::Undef {
                    errors.push((ErrorLevel::Err, error!(self.lexer, tk.pos, "Unexpected token `]`, index expected")));
                    return Err(errors);
                }

                res = Expression(ident_expr.0, ExpressionR::Index(Box::new(ident_expr.clone()), Box::new(index.0), vec![]), None);
            } else if tk.ttype == TokenType::Special && (tk.value == "." || tk.value == "->") {
                self.lexer.next_token().unwrap();
                let ident = err_ret!(self.expect(Some(TokenType::Ident), None), errors);
                let ident_expr = err_add!(self.parse_ident_expr(ident.clone().value, ident.pos, seperators), errors);
                
                match ident_expr.1 {
                    ExpressionR::Var(ref field) => {
                        let expr = Expression(ident_expr.0, ExpressionR::StructField(Box::new(res.clone()), field.clone(), None, tk.value == "->"), None);
                        res = expr;
                        continue;
                    },
                    ExpressionR::F(ref var, ref args) => {
                        if let Expression(_, ExpressionR::Var(ref name), _) = **var {
                            let expr = Expression(ident_expr.0, ExpressionR::MemberFunction(Type::Invalid, Box::new(res.clone()), name.clone(), args.clone()), None);
                            res = expr;
                            continue;
                        } else {
                            unreachable!("memberfunction is not a name...");
                        }
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
            match util::parse_type(val.clone()) {
                Some(a) => Ok(a),
                None => Err((ErrorLevel::Err, error!(self.lexer, nt.pos, "invalid type `{val}`"))),
            }
        } else if nt.ttype == TokenType::Special && val == "[" {
            self.parse_array()
        } else if nt.ttype == TokenType::Ident {
            if val.starts_with('_') {
                Ok(Type::Var(val))
            } else {
                Ok(Type::Custom(val))
            }
        } else if nt.ttype == TokenType::Keyword || nt.value == "func" {
            self.expect(Some(TokenType::Operator), Some("<".into()))?;
            self.expect(Some(TokenType::Special), Some("(".into()))?;
            let mut types = vec![];
            let tmp_tk = self.lexer.peek_token()?;
            if ! (tmp_tk.ttype == TokenType::Special && tmp_tk.value == ")") {
                loop {
                    types.push(self.parse_type()?);
                    let next_tk = self.lexer.next_token()?;
                    if next_tk.ttype == TokenType::Special && next_tk.value == "," {
                        continue;
                    } else if next_tk.ttype == TokenType::Special && next_tk.value == ")" {
                        break;
                    } else {
                        let val_ = next_tk.value;
                        return Err((ErrorLevel::Err, error!(self.lexer, nt.pos, "invalid token `{val_}`, `,` or `)` expected")));
                    }
                }
            } else {
                self.lexer.next_token().unwrap();
            }

            let ret_type;
            if self.lexer.peek_token()?.value == ">" {
                self.lexer.next_token().unwrap();
                ret_type = Type::Primitive(PrimitiveType::Void)
            } else {
                self.expect(Some(TokenType::Special), Some("->".into()))?;
                ret_type = self.parse_type()?;
                self.expect(Some(TokenType::Operator), Some(">".into()))?;
            }
            Ok(Type::Function(types, Box::new(ret_type)))
        } else {
            Err((ErrorLevel::Err, error!(self.lexer, nt.pos, "invalid type `{val}`")))
        }?;

        loop {
            let ptr_check = self.lexer.peek_token()?;
            if ptr_check.ttype == TokenType::Operator && ptr_check.value.chars().all(|x| x == '*') {
                self.lexer.next_token().unwrap();
                for _ in ptr_check.value.chars() {
                    tmp_tp = Type::Pointer(Box::new(tmp_tp));
                }
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
