use crate::util;
use std::{fs, io};
use util::{ErrorLevel, Error, is_valid_type};
use crate::error;

#[derive(Debug, Clone)]
pub struct Lexer {
    pub filename : String,
    source       : Vec<char>,
    pub pos      : usize,
    line         : usize,
    ch           : usize,
    verbose      : usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub pos: usize,
    pub ttype: TokenType,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenType {
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
    "for",
    "break",
    "struct",
    "ref",
    "deref",
    "include",
    "from",
    "typeclass",
    "instance"
];

macro_rules! ret {
    ($a:expr, $verbose:expr) => {
        if ($verbose >= 4) {
            eprintln!("TOKEN: {:?}", $a);
        }
        return $a;
    }
}

impl Lexer {
    pub fn new(filename: String, verbose: usize) -> Result<Self, io::Error> {
        Ok(Lexer {
            source: fs::read_to_string(filename.clone())?.chars().collect(),
            pos: 0,
            filename,
            line: 0,
            ch: 0,
            verbose
        })
    }

    pub fn pos_to_line_char(&mut self, pos: usize) -> (usize, usize) {
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

    pub fn next_token(&mut self) -> Result<Token, Error> {
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
                        ret!(Ok::<Token, Error>(Token {
                            pos: token_pos,
                            ttype,
                            value: val.clone(),
                        }), self.verbose);
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
                        ret!(Ok::<Token, Error>(Token {
                            pos: token_pos,
                            ttype,
                            value: val.clone(),
                        }), self.verbose);
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
                    let mut c = ch;
                    if val.ends_with('\\') {
                        let esc = match ch {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\\' => '\\',
                            _ => {
                                return Err((ErrorLevel::Err,
                                    error!(self, self.pos,
                                           "invalid escape literal: '{val}'")));
                            }
                        };
                        val.pop().unwrap();
                        c = esc;
                    }
                    
                    if c == '"' {
                        self.pos += 1;

                        ret!(Ok::<Token, Error>(Token {
                            pos: token_pos,
                            ttype,
                            value: val.clone(),
                        }), self.verbose);
                    }
                    val.push(c);
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

                            ret!(Ok::<Token, Error>(Token {
                                pos: token_pos,
                                ttype,
                                value: val.clone(),
                            }), self.verbose);
                        }
                    },
                    '"' => {
                        if ttype == TokenType::Undef {
                            ttype = TokenType::String;
                        } else {
                            // check for keywords / types
                            ttype = check_keywords_and_types(val.as_str(),
                                                             ttype);

                            ret!(Ok::<Token, Error>(Token {
                                pos: token_pos,
                                ttype,
                                value: val.clone(),
                            }), self.verbose);
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

                            ret!(Ok::<Token, Error>(Token {
                                pos: token_pos,
                                ttype,
                                value: val.clone(),
                            }), self.verbose);
                        }
                    },
                    '0'..='9'|'.' => {
                        if ttype == TokenType::Float ||
                            ((ttype == TokenType::Int) && ch == '.') {
                                if ttype != TokenType::Float && self.pos + 1 < self.source.len() && ! self.source[self.pos+1].is_numeric() {
                                    ret!(Ok::<Token, Error>(Token {
                                        pos: token_pos,
                                        ttype,
                                        value: val.clone(),
                                    }), self.verbose);
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

                                ret!(Ok::<Token, Error>(Token {
                                    pos: token_pos,
                                    ttype,
                                    value: val.clone(),
                                }), self.verbose);
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

                                ret!(Ok::<Token, Error>(Token {
                                    pos: token_pos,
                                    ttype,
                                    value: val.clone(),
                                }), self.verbose);
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

                            ret!(Ok::<Token, Error>(Token {
                                pos: token_pos,
                                ttype,
                                value: val.clone(),
                            }), self.verbose);
                        }
                    },
                    ' '|'\t'|'\n' => {
                        if ttype != TokenType::Undef {
                            // check for keywords / types
                            ttype = check_keywords_and_types(
                                val.as_str(), ttype);

                            ret!(Ok::<Token, Error>(Token{
                                pos: token_pos,
                                ttype,
                                value: val.clone(),
                            }), self.verbose);
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
                ret!(Ok::<Token, Error>(Token {
                    pos: token_pos,
                    ttype,
                    value: val.clone(),
                }), self.verbose);
            }

            (self.line, self.ch) = self.pos_to_line_char(self.pos);
        }
    }

    pub fn peek_token(&mut self) -> Result<Token, Error> {
        let last_pos = self.pos;
        let token = self.next_token();
        self.pos = last_pos;
        token
    }
}


