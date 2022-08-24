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

pub fn preprocess(file: Vec<char>, filename: String) -> (Vec<char>, Vec<String>, Vec<String>) {
    let mut i          : usize = 0;
    let mut to_skip    : Vec<(usize, usize)> = vec![];
    let mut links      : Vec<String> = vec![];
    let mut links_libs : Vec<String> = vec![];
    
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
            while file[i] != ' ' {i += 1};
            let directive = file.iter().skip(dir_start).take(i - dir_start).collect::<String>();
            while file[i] != ' ' {i += 1};
            to_skip.push((dir_start, i - dir_start));
            match directive.as_str() {
                "link"|"link_lib" => {
                    // expect string
                    while file[i] == ' ' || file[i] == '\t' { i += 1; }
                    if file[i] != '"' {
                        let (l, c) = pos_to_line_char(file.clone(), dir_start);
                        eprintln!("{file}:{line}:{ch}: Invalid character `{}`, `\"` expected",
                                  file[i],
                                  file = filename,
                                  line = l+1,
                                  ch = c+1,
                        );
                            continue;
                    }
                    i += 1;
                    let str_start = i;
                    while file[i] != '"' { i += 1; }
                    let file = file.iter().skip(str_start).take(i - str_start).collect::<String>();

                    (if directive == "link".to_owned() {&mut links} else {&mut links_libs}).push(file);
                }
                _ => {
                    let (l, c) = pos_to_line_char(file.clone(), dir_start);
                    eprintln!("{file}:{line}:{ch}: Invalid preprocesser directive `{directive}`",
                              file = filename,
                              line = l+1,
                              ch = c+1,
                )}
            }
        }
        i += 1;
    }

    to_skip.sort();
    let mut new_file = file.clone();
    for (a, l) in to_skip.iter().rev() {
        for _ in 0..*l {
            new_file.remove(*a);
        }
    }
    
    (new_file, links, links_libs)
}
