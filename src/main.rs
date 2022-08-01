mod lexer;
mod util;
mod parser;
mod typecheck;
mod intermediate;
mod codegen_x86_64_linux;

use std::{fs, env::args, process::{exit, Command}, collections::HashMap,
          time::SystemTime};

#[allow(unused)]
enum Target {
    Linux,
    Bsd,
    Mac,
    Windows,
    Wasm,
}

fn main() {
    let start_time = SystemTime::now();
    
    let mut args_ = args();
    let arg0 = args_.next().unwrap_or_else(|| {"<program>".into()});

    let mut error: bool = false;

    let vec: Vec<String> = args_.collect();
    let (pos_args, gnu_args, unix_args) = match util::parse_args(vec) {
        Ok(a) => a,
        Err(e) => {
            println!("ERROR during argument parsing: {e}");
            return;
        },
    };

    let target = Target::Linux;

    let arch_result = match target {
        Target::Linux => {
            Ok((codegen_x86_64_linux::intrinsics, codegen_x86_64_linux::generate))
        },
        Target::Bsd => {
            Err("BSD")
        },
        Target::Mac => {
            Err("MacOS")
        },
        Target::Windows => {
            Err("Windows")
        },
        Target::Wasm => {
            Err("WASM")
        },
    };

    let (intrinsics, generate) = match arch_result {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Target `{e}` is not implemented (yet)");
            exit(1);
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

    let mut parser = parser::Parser::new(filename.clone(), verbose).unwrap_or_else(|a| {
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
    let mut aliases: HashMap<String, util::Type> = HashMap::new();
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
        match typecheck::check(ast, parser.lexer.clone(), intrinsics) {
            Err(e) => {
                for a in e.iter() {
                    let (t, v) = a;
                    eprintln!("{}: {}", t, v);
                };
                if e.into_iter().any(|(lvl, _)| lvl == util::ErrorLevel::Err) {
                    checked = false;
                    error = true;
                } else {
                    checked = true;
                    error = false;
                }
            },
            Ok((g, a, functions)) => {
                if ! functions.contains_key(&(None, "main".into())) {
                    eprintln!("error: {}: missing main function", parser.lexer.filename);
                    error = true;
                    checked = false;
                } else {
                    aliases = a;
                    globals = g;
                    checked = true;
                    error = false;
                }
            }
        };
    } else {
        checked = false;
    }

    if checked {
        if let Ok(ast) = a {
            if verbose > 0 {
                println!("[*] generating intermediate represantation");
            }

            let (intermediate, globals) = intermediate::gen(ast.1, &mut HashMap::new(), &globals, aliases, 0, 1, true);
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
