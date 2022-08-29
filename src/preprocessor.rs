use crate::{COLOR_RED, COLOR_GREEN, COLOR_RESET, Target};
use std::process::exit;

fn pos_to_line_char(source: Vec<char>, pos: usize) -> (usize, usize) {
    assert!(pos < source.len());
    let mut p: usize = 0;
    let mut ch: usize = 0;
    let mut line: usize = 0;

    while p < pos {
        ch += 1;
        if source[p] == '\n' {
            ch = 0;
            line += 1;
        }
        p += 1;
    }

    (line, ch)
}

macro_rules! parse_str {
    ($file:expr, $i:expr, $dir_start:expr, $filename:expr) => {
        | | -> Option<String> {
            while $file[$i] == ' ' || $file[$i] == '\t' { $i += 1; }
            if $file[$i] != '"' {
                let (l, c) = pos_to_line_char($file.clone(), $dir_start);
                eprintln!("{file}:{line}:{ch}: Invalid character `{}`, `\"` expected",
                          $file[$i],
                          file = $filename,
                          line = l+1,
                          ch = c+1,
                );
                return None;
            }
            $i += 1;
            let str_start = $i;
            while $file[$i] != '"' { $i += 1; }
            let string = $file.iter().skip(str_start).take($i - str_start).collect::<String>();
            Some(string)
        }()
    };
}

#[derive(PartialEq, Eq)]
enum Action {
    Delete,
    Ignore
}

pub fn preprocess(file: Vec<char>, filename: String, tgt: Target) -> (Vec<char>, Vec<String>, Vec<String>) {
    let mut i             : usize = 0;
    let mut to_skip       : Vec<(usize, usize)> = vec![];
    let mut links         : Vec<String> = vec![];
    let mut links_libs    : Vec<String> = vec![];
    let mut action_stack  : Vec<(usize, Action)> = vec![];

    while i < file.len() {
        let ch = file[i];
        if i > 1 {
            if ch == '/' && file[i-1] == '/' {
                i += 1;
                while file[i] != '\n' {i += 1}
                i += 1;
                continue;
            } else if ch == '*' && file[i-1] == '/' {
                i += 1;
                while file[i-1] != '*' && file[i] != '/' {i += 1}
                i += 1;
                continue;
            }
        }
        if ch == '"' {
            i += 1;
            while file[i] != '"' {i += 1;}
            i += 1;
            continue;
        } else if ch == '\'' {
            i += 1;
            while file[i] != '\'' {i += 1}
            i += 1;
            continue;
        } else if ch == '#' {
            let dir_start = i + 1;
            while !file[i].is_whitespace() {i += 1};
            let directive = file.iter().skip(dir_start).take(i - dir_start).collect::<String>();
            while !file[i].is_whitespace() {i += 1};
            to_skip.push((dir_start - 1, i - dir_start + 1));

            match directive.as_str() {
                "link"|"link_lib" => {
                    // expect string
                    let file = match parse_str!(file, i, dir_start, filename) {
                        Some(a) => a,
                        None => continue,
                    };

                    (if directive == *"link" {&mut links} else {&mut links_libs}).push(file);
                },
                "error" => {
                    let msg = match parse_str!(file, i, dir_start, filename) {
                        Some(a) => a,
                        None => continue,
                    };
                    let (l, c) = pos_to_line_char(file.clone(), dir_start);
                    eprintln!("{COLOR_RED}error: {COLOR_GREEN}{file}:{line}:{ch}: {COLOR_RESET}{msg}",
                                   file = filename,
                                   line = l+1,
                                   ch = c+1,
                    );
                    exit(1);
                },
                "target" => {
                    let target_ = match parse_str!(file, i, dir_start, filename) {
                        Some(a) => a,
                        None => continue,
                    };
                    let target = match target_.as_str() {
                        "linux" => Target::Linux,
                        "windows" => Target::Windows,
                        _ => {
                            let (l, c) = pos_to_line_char(file.clone(), dir_start);
                            eprintln!("{COLOR_RED}error: {COLOR_GREEN}{file}:{line}:{ch}: {COLOR_RESET}invalid target {target_}",
                                   file = filename,
                                   line = l+1,
                                   ch = c+1,
                            );
                            exit(1);
                        }
                    };

                    if tgt != target {
                        action_stack.push((i+1, Action::Delete));
                    } else {
                        action_stack.push((i+1, Action::Ignore));
                    }
                },
                "end" => {
                    if action_stack.is_empty() {
                        let (l, c) = pos_to_line_char(file.clone(), dir_start);
                        eprintln!("{COLOR_RED}error: {COLOR_GREEN}{file}:{line}:{ch}: {COLOR_RESET}#end does not end anything",
                                  file = filename,
                                  line = l+1,
                                  ch = c+1,
                        );
                        exit(1);
                    }
                    let (ind, action) = action_stack.pop().unwrap();
                    if action == Action::Delete {
                        for (v, item) in file.iter().enumerate().take(i).skip(ind) {
                            if *item != '\n' {
                                to_skip.push((v, 1));
                            }
                        }
                    }
                }
                _ => {
                    let (l, c) = pos_to_line_char(file.clone(), dir_start);
                    eprintln!("{COLOR_RED}error: {COLOR_GREEN}{file}:{line}:{ch}: {COLOR_RESET}Invalid preprocesser \
                               directive `{directive}`",
                              file = filename,
                              line = l+1,
                              ch = c+1,
                    );
                    exit(1);
                }
            }
        }
        i += 1;
    }

    to_skip.sort();
    let mut new_file = file;
    for (a, l) in to_skip.iter().rev() {
        for i in 0..*l {
            new_file[a+i] = ' ';
        }
    }
    
    (new_file, links, links_libs)
}
