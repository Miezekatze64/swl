use std::{fs, env::args, process::exit, io};

#[allow(unused)]
#[derive(Debug, Clone)]
enum Error {
    Lexer(String),
    Parse(String),
    Syntax(String),
}

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
];

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Primitive(PrimitiveType),
    Custom(String),
    Array(Box<Type>/*, u32*/),
    Pointer(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
enum PrimitiveType {
    Int,
    Float,
    Char,
    Void,
//    String,
}

fn is_valid_type(val: &str) -> bool {
    if val == "int" || val == "float" || val == "char" || val == "void" /*|| val == "string"*/ {
        true
    } else {
        // check for custom types
        false
    }
}

fn parse_type(string: String) -> Type {
    match string.as_str() {
        "int"    => Type::Primitive(PrimitiveType::Int),
        "float"  => Type::Primitive(PrimitiveType::Float),
        "char"   => Type::Primitive(PrimitiveType::Char),
        "void"   => Type::Primitive(PrimitiveType::Void),
//        "string" => Type::Primitive(PrimitiveType::String),
        _ => {
            if string.ends_with("*") {
                Type::Pointer(Box::new(parse_type(string.as_str()[0..string.len()-1].into())))
            } else {
                Type::Custom(string)
            }
        }
    }
}

/// return an error string in the following format:
///     ERR: This is a error at ~/file.name:430
/// argument 2 (msg_fmt) is a format string
macro_rules! error {
    ($lexer:expr, $pos:expr, $msg_fmt:literal, $level:expr) => {
        | | -> String {
            let (l, c) = $lexer.pos_to_line_char($pos);
            return format!("{lvl}: {msg} \n\t--> {file}:{line}:{ch}",
                    msg = format!($msg_fmt),
                    lvl = $level.to_string(),
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
                        return Err(Error::Lexer(error!(self, self.pos, "invalid character literal", ErrorLevel::Err)));
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
                        if ttype == TokenType::Undef || ttype == TokenType::Operator {
                            ttype = TokenType::Operator;
                            val.push(ch);
                        } else if ttype == TokenType::Special && val == "<" && ch == '-' {
                            val.push(ch);
                            ttype = TokenType::Special;
                        } else if ttype == TokenType::Type && ch == '*' {
                            val.push(ch);
                            ttype = TokenType::Type;
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
                    _ => return Err(Error::Lexer(error!(self, self.pos, "invalid character `{ch}`", ErrorLevel::Err)))
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
}

#[derive(Debug, Clone)]
struct Parser {
    lexer: Lexer,
}

#[derive(Debug, Clone, PartialEq)]
enum ASTNode {
    Block(Vec<ASTNode>),
    VarDec(Type, String),
    VarInit(String, Expression),
    FunctionCall(String, Vec<Expression>),
    If(Expression, Box<ASTNode>),
    FunctionDecl(String, Vec<(Type, String)>, Type, Box<ASTNode>),
    Return(Expression),
    TypeAlias(String, Type),
}

#[derive(Debug, Clone, PartialEq)]
enum Expression {
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

impl Op {
    fn from_char(c: char) -> Result<Self, Error> {
        match c {
            '+' => Ok(Self::Add),
            '-' => Ok(Self::Sub),
            '*' => Ok(Self::Mul),
            '/' => Ok(Self::Div),
            _ => Err(Error::Parse(format!("FATAL: invalid operator `{c}` set from lexer"))),
        }
    }
    
    fn get_precedence(op: Op) -> u32 {
        match op {
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div => 2,
        }
    }
}

impl Parser {
    fn new(filename: String) -> Result<Self, io::Error> {
        Ok(Parser {
            lexer: Lexer::new(filename)?
        })
    }

    fn parse(&mut self) -> Result<ASTNode, Error> {
        self.parse_block_(true)
    }

    fn parse_block(&mut self) -> Result<ASTNode, Error> {
        self.parse_block_(false)
    }
    
    fn parse_block_(&mut self, is_root: bool) -> Result<ASTNode, Error> {
        let mut block: ASTNode = ASTNode::Block(vec![]);
        loop {
            match block {
                ASTNode::Block(ref mut vec) => {
                    let bs = self.parse_block_statement(is_root)?;
                    if bs.is_none() {
                        break;
                    }
                    vec.push(bs.unwrap());
                },
                _ => todo!(),
            }
            let token = self.lexer.peek_token()?;
            if token.ttype == TokenType::Eof {
                break;
            }
        }
        Ok(block)
    }

    fn parse_expr(&mut self, seperators: &[&'static str]) -> Result<(Expression, Token), Error> {
        let check_precedence_and_update = |op: Op, right_expr: Box<Expression>, working_expr: &Expression| -> Box<Expression> {
            let ret: Box<Expression>;
            let is_first: bool = match working_expr {
                Expression::T(_, _, _, precedence) => {
                    Op::get_precedence(op) <= *precedence
                },
                Expression::Val(..) | Expression::Arr(..) | Expression::F(..) | Expression::Var(..) => {
                    true
                },
                Expression::Undef => unreachable!(),
            };

            if is_first {
                let tmp_expr = Expression::T(Box::new(working_expr.clone()), op, right_expr
                                             , Op::get_precedence(op));
                ret = Box::new(tmp_expr);
            } else {
                assert!(matches!(*working_expr, Expression::T(..)));
                let mut expr = working_expr.clone();
                let tmp_expr = if let Expression::T(_, _, right, _) = &expr {
                    Expression::T(right.clone(), op, right_expr, Op::get_precedence(op))
                } else {
                    unreachable!();
                };

                if let Expression::T(a, b, _, d) = expr {
                    expr = Expression::T(a, b, Box::new(tmp_expr), d);
                }
                    
                ret = Box::new(expr);
            }

            ret
        };

        let mut left_expr = Box::new(Expression::Undef);

        // parse left side
        let subexpr = self.parse_subexpr(seperators)?;
        let mut nleft_expr = subexpr.0;
        
        loop {
            if let Expression::Undef = *nleft_expr {
                return Ok((*left_expr, subexpr.1));
            }
            left_expr = nleft_expr;
            
            // parse operation
            let tk_op = self.lexer.next_token()?;        
            let op = if tk_op.ttype == TokenType::Operator && tk_op.value.len() == 1 {
                Op::from_char(tk_op.value.chars().next().unwrap())?
            } else if tk_op.ttype == TokenType::Eof || (tk_op.ttype == TokenType::Special && seperators.contains(&tk_op.value.as_str())) {
                return Ok((*left_expr, tk_op));
            } else {
                let val = tk_op.value;
                return Err(Error::Syntax(error!(self.lexer, tk_op.pos, "invalid token `{val}`", ErrorLevel::Err)));
            };

            // parse right side
            let subexpr_r = self.parse_subexpr(seperators)?;

            if let Expression::Undef = *subexpr_r.0 {
                return Ok((*left_expr, subexpr.1));
            }
            
            let right_expr = subexpr_r.0;
            nleft_expr = check_precedence_and_update(op, right_expr.clone(), &mut left_expr);
        }
    }

    fn parse_block_statement(&mut self, is_root: bool) -> Result<Option<ASTNode>, Error> {
        let token = self.lexer.next_token()?;
        let val = token.value;

        fn parse_var_dec(parser: &mut Parser, typ: Type) -> Result<Option<ASTNode>, Error> {
            // declare variable
            let next_token = parser.lexer.next_token()?;
            let ntv = next_token.value.clone();
            let ident_pos = next_token.pos;
            
            match next_token.ttype {
                TokenType::Ident => {},
                _ => return Err(Error::Syntax(error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)))
            }
            let ident = ntv;
            
            // check next token
            let next_token = parser.lexer.next_token()?;
                let ntv = next_token.value.clone();
            match next_token.ttype {
                TokenType::Special => {
                        if next_token.value == ";" {
                            // token ended, normal return
                        } else {
                            return Err(Error::Syntax(error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)));
                        }
                },
                TokenType::Operator => {
                    if next_token.value == "=" {
                        // variable initialization, reset position
                        parser.lexer.pos = ident_pos;
                    } else {
                        return Err(Error::Syntax(error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)));
                    }
                },
                _ => return Err(Error::Syntax(error!(parser.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err))),
            }
            
            return Ok(Some(ASTNode::VarDec(typ, ident)));
        }
        
        match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Char | TokenType::String => return Err(Error::Syntax(error!(self.lexer, token.pos, "invalid literal", ErrorLevel::Err))),
            TokenType::Ident => {
                let expr = self.parse_ident_expr(val.clone(), &[";"])?;
                match *expr {
                    Expression::Var(..) => {
                        let var_token = self.lexer.next_token()?;
                        let var_val = var_token.value;
                        
                        if var_token.ttype == TokenType::Operator && var_val == "=" {
                            let expr = self.parse_expr(&[";"])?.0;
                            return Ok(Some(ASTNode::VarInit(val, expr)));
                        } else {
                            return Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{var_val}`", ErrorLevel::Err)));
                        }
                    }
                    Expression::Val(..) => unreachable!(),
                    Expression::T(..) => unreachable!(),
                    Expression::Arr(..) => unreachable!(),
                    Expression::F(name, args) => {
                        // function call: only permitted inside of function
                        if is_root {
                            return Err(Error::Syntax(error!(self.lexer, token.pos, "function calls are not allowed at top level", ErrorLevel::Err)));
                        }
                        
                        // expect semicolon
                        let stoken = self.lexer.next_token()?;
                        let sval = stoken.value;
                        
                        if stoken.ttype == TokenType::Special && sval == ";" {
                            return Ok(Some(ASTNode::FunctionCall(name, args)));
                        } else {
                            return Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{sval}`, expceted semicolon", ErrorLevel::Err)));
                        }
                        
                    },
                    Expression::Undef => unreachable!(),
                    
                }
            },
            TokenType::Eof => return Ok(None),
            TokenType::Special => match val.as_str() {
                "}" => {
                    if is_root {
                        return Err(Error::Syntax(error!(self.lexer, token.pos, "unexprected  token '}}'", ErrorLevel::Err)));
                    }
                    return Ok(None)
                },
                "<-" => {
                    if is_root {
                        return Err(Error::Syntax(error!(self.lexer, token.pos, "returns are not allowed at top level", ErrorLevel::Err)));
                    }
                    // parse expression
                    let expression = self.parse_expr(&[";"])?.0;
                    return Ok(Some(ASTNode::Return(expression)));
                },
                "[" => {
                    // array declaration (e. g. [int, 2])

                    let array_type = self.parse_array()?;

                    return parse_var_dec(self, array_type);
                }
                ";" => {
                    // ignore
                    return self.parse_block_statement(is_root);
                }
                _ => {},
            },
            TokenType::Operator => {},
            TokenType::Type => {
                return parse_var_dec(self, parse_type(val));
            },
            TokenType::Keyword => match val.as_str() {
                "if" => {
                    if is_root {
                        return Err(Error::Syntax(error!(self.lexer, token.pos, "`if` not allowed at top level", ErrorLevel::Err)));
                    }
                    // next token: '('
                    let next_token = self.lexer.next_token()?;
                    if next_token.ttype != TokenType::Special || next_token.value != "(" {
                        let ntv = next_token.value;
                        return Err(Error::Syntax(error!(self.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)));
                    }
                    
                    let predicate = self.parse_expr(&[")"])?.0;

                    // next token: '{'
                    let next_token = self.lexer.next_token()?;
                    if next_token.ttype != TokenType::Special || next_token.value != "{" {
                        let ntv = next_token.value;
                        return Err(Error::Syntax(error!(self.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)));
                    }

                    let block = self.parse_block()?;

                    return Ok(Some(ASTNode::If(predicate, Box::new(block))));
                    
                },
                "func" => {
                    if ! is_root {
                        return Err(Error::Syntax(error!(self.lexer, token.pos, "functions are only allowed at top level", ErrorLevel::Err)));
                    }
                    
                    // parse function name
                    let ident_token = self.lexer.next_token()?;
                    let name = ident_token.value;
                    if ident_token.ttype != TokenType::Ident {
                        return Err(Error::Syntax(error!(self.lexer, ident_token.pos, "unexpected token `{name}`", ErrorLevel::Err)));
                    }

                    // expect '('
                    let next_token = self.lexer.next_token()?;
                    if next_token.ttype != TokenType::Special || next_token.value != "(" {
                        let ntv = next_token.value;
                        return Err(Error::Syntax(error!(self.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)));
                    }
                    // parse args
                    let mut args: Vec<(Type, String)> = vec![];
                    
                    let check = self.lexer.peek_token()?;
                    if check.ttype != TokenType::Special || check.value != ")" {
                        loop {
                            let t = self.parse_type()?;
                            
                            let n = self.lexer.next_token()?;
                            let nval = n.value;
                            if n.ttype != TokenType::Ident {
                                return Err(Error::Syntax(error!(self.lexer, n.pos, "unexpected token `{nval}`, identifier expected", ErrorLevel::Err)));
                            }
                            args.insert(args.len(), (t, nval));

                            let token = self.lexer.next_token()?;
                            let val = token.value;
                            if token.ttype != TokenType::Special {
                                return Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{val}`, `,` or `)` expected", ErrorLevel::Err)));
                            }
                            
                            if val == "," {
                                continue;
                            } else if val == ")" {
                                break;
                            } else {
                                return Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{val}`, `,` or `)` expected", ErrorLevel::Err)));
                            }
                        }
                    } else {
                        self.lexer.next_token()?;
                    }

                    // optional return type (-> type)
                    // else {
                    let mut ret_type = Type::Primitive(PrimitiveType::Void);
                    
                    let mut next_token = self.lexer.next_token()?;
                    if next_token.ttype == TokenType::Special && next_token.value == "->" {
                        // expect return type

                        let tp = self.parse_type()?;
                        ret_type = tp;//parse_type(val);
                        next_token = self.lexer.next_token()?;
                    }
                    
                    
                    // expect {
                    if next_token.ttype != TokenType::Special || next_token.value != "{" {
                        let ntv = next_token.value;
                        return Err(Error::Syntax(error!(self.lexer, next_token.pos, "unexpected token `{ntv}`", ErrorLevel::Err)));
                    }

                    // parse block
                    let block = self.parse_block()?;

                    return Ok(Some(ASTNode::FunctionDecl(name, args, ret_type, Box::new(block))));

//                    let func_expr: Expression = Expression::F(ident, args);
                },
                "alias" => {
                    if ! is_root {
                        return Err(Error::Syntax(error!(self.lexer, token.pos, "type aliases only allowed at top-level", ErrorLevel::Err)));
                    }
                    
                    // expect identifier
                    let next_token = self.lexer.next_token()?;
                    let ident = next_token.value;
                    if next_token.ttype != TokenType::Ident {
                        return Err(Error::Syntax(error!(self.lexer, next_token.pos, "unexpected token `{ident}`, identifier expected", ErrorLevel::Err)));
                    }

                    // expect `=`
                    let next_token = self.lexer.next_token()?;
                    if next_token.ttype != TokenType::Operator || next_token.value != "=" {
                        let ntv = next_token.value;
                        return Err(Error::Syntax(error!(self.lexer, next_token.pos, "unexpected token `{ntv}`, identifier expected", ErrorLevel::Err)));
                    }

                    let typ = self.parse_type()?;

                    return Ok(Some(ASTNode::TypeAlias(ident, typ)));
                    
                },
                _ => {}
            },
            TokenType::Undef => return self.parse_block_statement(is_root),
        }
        Err(Error::Syntax(error!(self.lexer, token.pos, "not implemented", ErrorLevel::Fatal)))
    }

    fn parse_ident_expr(&mut self, ident: String, seperators: &[&'static str]) -> Result<Box<Expression>, Error> {
        let token = self.lexer.peek_token()?;
        let val = token.value;

        match token.ttype {
            TokenType::Int | TokenType::Float | TokenType::Ident | TokenType::Type | TokenType::Keyword | TokenType::String | TokenType::Char => {
                Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{val}`", ErrorLevel::Err)))
            }
            TokenType::Eof => Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected end of file", ErrorLevel::Err))),
            TokenType::Special => match val.as_str() {
                "(" => {
                    self.lexer.next_token()?;
                    // parse args
                    let mut args: Vec<Expression> = vec![];
                    loop {
                        let ret = self.parse_expr(&[")", ","])?;
                        let token: Token = ret.1;
                        if let Expression::Undef = ret.0 {
                            break;
                        }
                        args.insert(args.len(), ret.0);
                        if token.value == ")" {
                            break;
                        }
                    }
                    let func_expr: Expression = Expression::F(ident, args);
                    Ok(Box::new(func_expr))
                },
                a => if seperators.contains(&a) {
                    Ok(Box::new(Expression::Var(ident)))
                } else {
                    Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{val}`", ErrorLevel::Err)))
                },
            },
            TokenType::Operator => {
                Ok(Box::new(Expression::Var(ident)))
            },
            TokenType::Undef => unreachable!(),
        }
    }

    fn parse_subexpr(&mut self, seperators: &[&'static str]) -> Result<(Box<Expression>, Token), Error> {
        let inverse = |right_expr: &mut Box<Expression>| {
            let tmp_expr = right_expr.clone();
            **right_expr = Expression::T(Box::new(Expression::Val(Type::Primitive(PrimitiveType::Int), "-1".into())), Op::Mul, tmp_expr, Op::get_precedence(Op::Mul));
        };
        
        let token = self.lexer.next_token()?;
        let val = token.clone().value;
        match token.ttype {
            TokenType::Type | TokenType::Eof | TokenType::Undef | TokenType::Keyword => {
                Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected token `{val}`", ErrorLevel::Err)))
            },
            TokenType::Int => {
                Ok((Box::new(Expression::Val(Type::Primitive(PrimitiveType::Int), val)), token))
            },
            TokenType::Float => {
                Ok((Box::new(Expression::Val(Type::Primitive(PrimitiveType::Float), val)), token))
            },
            TokenType::Char => {
                Ok((Box::new(Expression::Val(Type::Primitive(PrimitiveType::Char), val)), token))
            },
            TokenType::String => {
                Ok((Box::new(Expression::Val(Type::Array(Box::new(Type::Primitive(PrimitiveType::Char))), val)), token))
            },
          TokenType::Ident => Ok((self.parse_ident_expr(val, seperators)?, token)),
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
                    Ok((Box::new(Expression::Arr(arr)), Token {
                        pos: 0,
                        ttype: TokenType::Undef,
                        value: "".into(),
                    }))
                },
                _ => if seperators.contains(&val.as_str()) {
                    Ok((Box::new(Expression::Undef), token))
                } else {
                    Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected symbol `{val}`", ErrorLevel::Err)))
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
                    Err(Error::Syntax(error!(self.lexer, token.pos, "unexpected operator `{val}`", ErrorLevel::Err)))
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
            return Err(Error::Syntax(error!(self.lexer, next_token.pos, "invalid token `{val}`, `]` expected", ErrorLevel::Err)));
        }

        Ok(array_type)
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let nt = self.lexer.next_token()?;
        if nt.ttype == TokenType::Type {
            Ok(parse_type(nt.value))
        } else if nt.ttype == TokenType::Special && nt.value == "[" {
            self.parse_array()
        } else {
            Ok(Type::Custom(nt.value))
        }
    }
}

fn main() {
    let mut args = args();
    let arg0 = args.next().unwrap_or_else(|| {"<program>".into()});
    
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

    match parser.parse() {
        Ok(a) => eprintln!("AST NODE: \n{:#?}", a),
        Err(e) => match e {
            Error::Lexer(e) | Error::Parse(e) | Error::Syntax(e) => {
                eprintln!("{}", e);
                exit(1);
            },
        },
    }
}

