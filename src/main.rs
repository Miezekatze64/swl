// including used modules
mod lexer;
mod util;
mod parser;
mod typecheck;
mod intermediate;
mod optimizer;
mod codegen_x86_64;
mod codegen_x86_64_linux;
mod codegen_x86_64_windows;
mod interpreter;
mod preprocessor;

// including used stdlib function
use std::{fs, env::args, process::{exit, Command}, collections::HashMap,
          time::SystemTime};

/// Enum representing possible target types
// TODO(#2): parse target types (-t/--target)
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Target {
    Linux,
    #[allow(unused)]
    Bsd,
    Mac,
    Windows,
    #[allow(unused)]
    Wasm,
}

pub const COLOR_RED:    &str = "\x1b[31m";
pub const COLOR_GREEN:  &str = "\x1b[32m";
pub const COLOR_YELLOW: &str = "\x1b[33m";
pub const COLOR_RESET:  &str = "\x1b[0m";

/// Returns the usage of the compiler as a string
fn usage(prog_name: String) -> String {
    format!("{COLOR_GREEN}Usage: {prog_name} <file> [OPTIONS]{COLOR_RESET}\n\
             Options:\n\
             \t--verbose   | -v {COLOR_YELLOW}=>{COLOR_RESET} set verbosity level\n\
             \t--output    | -o {COLOR_YELLOW}=>{COLOR_RESET} set output filename\n\
             \t--help      | -h {COLOR_YELLOW}=>{COLOR_RESET} display this help message and exit\n\
             \t--interpret | -i {COLOR_YELLOW}=>{COLOR_RESET} interpret program instead of compiling\n\
             \t--target    | -t {COLOR_YELLOW}=>{COLOR_RESET} set target of compilation (e. g. linux-x86_64)\n\
             ")
}

/// The main function of the compiler
fn main() {
    let start_time = SystemTime::now();
    
    let mut args_ = args();
    let arg0 = args_.next().unwrap_or_else(|| {"<program>".into()});

    let mut error: bool = false;

    // parse arguments
    let vec: Vec<String> = args_.collect();
    let (pos_args, gnu_args, unix_args) = match util::parse_args(vec) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("ERROR during argument parsing: {e}");
            exit(1);
        },
    };

    let t = if gnu_args.contains_key("target") {
        &gnu_args["target"]
    } else if unix_args.contains_key(&'t') {
        &unix_args[&'t']
    } else {
        "linux"
    };

    let target = match t {
        "linux" | "gnu" | "linux-x86_64" | "x86_64-linux-gnu" => Target::Linux,
        "windows" | "win64" | "x86_64-w64" | "windows-x86_64" => Target::Windows,
        "darwin" | "macos" | "darwin-x86_64" | "x86_64-apple-darwin" => Target::Mac,
        _ => {
            eprintln!("Unknown target: {t}");
            exit(1);
        }
    };

    // get architecture-specific functions
    type Intrinsics = fn() -> HashMap<&'static str, &'static str>;
    type Generate   = fn(Vec<intermediate::Inst>, &HashMap<String, usize>,
                                &Vec<String>) -> String;
    let arch_result: Result<(Intrinsics,
                             Generate), &'static str> =
        match target {
            Target::Linux => {
                Ok((codegen_x86_64::intrinsics, codegen_x86_64_linux::generate))
            },
            Target::Bsd => {
                Err("BSD")
            },
            Target::Mac => {
                Err("MacOS")
            },
            Target::Windows => {
                Ok((codegen_x86_64::intrinsics, codegen_x86_64_windows::generate))
            },
            Target::Wasm => {
                Err("WASM")
            },
        };

    // check for errors
    let (intrinsics, generate) = match arch_result {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Target `{e}` is not implemented (yet)");
            exit(1);
        },
    };
    

    // parse arguments
    if gnu_args.contains_key("help") || unix_args.contains_key(&'h') {
        eprintln!("{}", usage(arg0));
        exit(0);
    }
    
    let verbose = if gnu_args.contains_key("verbose") {
        &gnu_args["verbose"]
    } else if unix_args.contains_key(&'v') {
        &unix_args[&'v']
    } else {
        "1"
    }.parse::<usize>().unwrap_or(0);

    let interpret = gnu_args.contains_key("interpret") || unix_args.contains_key(&'i');

    let filename: String = match pos_args.get(0) {
        Some(a) => a.to_string(),
        None => {
            eprintln!("{COLOR_RED}ERROR: <filename> not given{COLOR_RESET}\n{}", usage(arg0));
            exit(1);
        },
    };

    // get directory of program
    let path: String = filename.chars().rev().skip_while(|x| x != &'/').collect::<String>().chars().rev().collect();

    // check for output arg
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

    
    let contents = fs::read_to_string(filename.clone()).expect("File read error: ").chars().collect();
//    let (contents, links, links_libs) = preprocessor::preprocess(contents, filename.clone());
    
    // construct parser
    let mut parser = parser::Parser::new(contents, filename.clone(), verbose, target).unwrap_or_else(|a| {
        eprintln!("Error reading file: {}", a);
        exit(1);
    });

    if verbose > 0 {
        eprintln!("[*] generating AST");
    }

    // parse the file
    let mut a = parser.parse(verbose);
    if let Err(ref e) = a {
        for a in e {
            let (t, v) = a;
            eprintln!("{COLOR_RED}{}: {COLOR_RESET}{}", t, v);
        }
        error = true;
    }

    let checked: bool;
    let mut aliases:   HashMap<String, util::Type> = HashMap::new();
    let mut globals:   HashMap<String, usize> = HashMap::new();

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

        // typecheck the AST
        match typecheck::check(ast, parser.lexer.clone(), intrinsics) {
            Err((e, a, g)) => {
                for a in e.iter() {
                    let (t, v) = a;
                    eprintln!("{COLOR_RED}{}: {COLOR_RESET}{}", t, v);
                };
                if e.into_iter().any(|(lvl, _)| lvl == util::ErrorLevel::Err) {
                    checked = false;
                    error = true;
                } else {
                    checked = true;
                    error = false;
                    aliases = a;
                    globals = g;
                }
            },
            Ok((g, a, functions, ..)) => {
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
        if let Ok(mut ast) = a {
            if verbose > 0 {
                eprintln!("[*] generating intermediate represantation");
            }

            // optimize AST
            optimizer::optimize(&mut ast, &aliases);

            let (intermediate, globals, externs) = intermediate::gen(ast.1, &mut HashMap::new(), &globals, aliases, 0, 1, true);
            if verbose > 2 {
                eprintln!("{:#?}", intermediate);
            }

            if ! interpret {
                // generate assembly
                if verbose > 0 {
                    eprintln!("[*] generating assembly");
                }
                let asm = generate(intermediate, &globals, &externs);
                if verbose > 1 {
                    eprintln!("{}", asm);
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
                
                // call nasm and ld -> ELF file
                if verbose > 0 {
                    eprintln!("[*] CMD: nasm -felf64 {asm_path}");
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
                if target == Target::Windows {
                    outfile.push_str(".exe");
                }

                let mut linked_files = parser.links.clone();
                let linked_libs  = parser.linked_libs.clone();
                
                for lib in linked_libs {
                    // search for lib
                    let lib_str = "-l".to_owned() + &lib;
                    linked_files.push(lib_str);
                }

                let linked_files_str = linked_files.join(" ");


                let ld = match target {
                    Target::Linux => {
                        if verbose > 0 {
                            eprintln!("[*] CMD: ld -o {outfile} {obj_path} {linked_files_str} --dynamic-linker /lib/ld-linux-x86-64.so.2");
                        }

                        Command::new("ld").arg("-o").arg(outfile.clone()).arg(obj_path).args(linked_files).arg("--dynamic-linker").arg("/lib/ld-linux-x86-64.so.2").output().unwrap()
                    },
                    Target::Windows => {
                        let has_mingw = Command::new("x86_64-w64-mingw32-ld").output().is_ok();
                        let linker = if has_mingw { "x86_64-w64-mingw32-ld" } else { "ld" };
                            
                        if verbose > 0 {
                            eprintln!("[*] CMD: {linker} -o {outfile} {obj_path} {linked_files_str} --image-base 0x10000000");
                        }

                        Command::new(linker).arg("-o").arg(outfile.clone()).arg(obj_path).args(linked_files).arg("--dynamic-linker").arg("/lib/ld-linux-x86-64.so.2").arg("--image-base").arg("0x10000000").output().unwrap()
                    }
                    _ => unreachable!("should have already exited")
                };
                
                if ! ld.status.success() {
                    eprintln!("ERROR executing ld:\n{}", std::str::from_utf8(&ld.stderr).unwrap());
                    error = true;
                }
                
                let time = SystemTime::now().duration_since(start_time).expect("not possible: backwards time...").as_secs_f32();
                
                eprintln!("[!] compiled {filename} -> {outfile} in {time}s");
            } else {
                // interpret file
                eprintln!("[*] Starting interpreter\n");
                interpreter::interpret(&intermediate);
            }
        }
    }

    // return 1 on error
    if error {
        exit(1);
    }
}
