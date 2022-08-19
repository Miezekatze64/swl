use std::{process::exit, collections::HashMap, fs::File, io::{stdin, Read, stderr, Write}};
use libc;

use crate::{intermediate::Inst, util::{Type, PrimitiveType, BinaryOp}};

fn find_function(intermediate: &Vec<Inst>, f: &String) -> usize {
    for (i, inst) in intermediate.iter().enumerate() {
        if let Inst::Func(nm, _) = inst {
            if nm == f {
                return i;
            }
        } else if let Inst::Intrinsic(_, nm) = inst {
            if nm == f {
                return i;
            }
        }
    }
    eprintln!("FATAL: function {f} not defined.");
    exit(1);
}

fn find_loop_end(intermediate: &Vec<Inst>, id: usize) -> usize {
    for (i, inst) in intermediate.iter().enumerate() {
        if let Inst::Endwhile(id_) = inst {
            if id == *id_ {
                return i;
            }
        }
    }
    eprintln!("FATAL: loop {id} has no end.");
    exit(1);
}

fn find_if_else_end(intermediate: &Vec<Inst>, id: usize) -> (usize, usize) {
    let mut a = 0;
    for (i, inst) in intermediate.iter().enumerate() {
        if let Inst::Else(id_) = inst {
            if id == *id_ {
                a = i;
                break;
            }
        }
    }
    if a == 0 {
        eprintln!("FATAL: if {id} has no else.");
        exit(1);
    }
    
    let mut b = 0;
    for (i, inst) in intermediate.iter().enumerate() {
        if let Inst::Endif(id_) = inst {
            if id == *id_ {
                b = i;
                break;
            }
        }
    }
    if b == 0 {
        eprintln!("FATAL: if {id} has no end.");
        exit(1);
    }
    (a, b)
}

type Register = u64;

macro_rules! uc {
    ($a:expr) => {
        u64::from_le_bytes($a.to_le_bytes())
    }
}

macro_rules! deref {
    ($val:expr, $globals:expr, $heap:expr, $vars:expr) => {
        match $val & SEGMENT {
            GLOBAL_SEG => {
                let bytes: Vec<u8> = $globals.iter().skip(($val & VALUE) as usize).take(8).map(|x| *x).collect();

                let mut arr: [u8; 8] = [0; 8];
                arr.copy_from_slice(&bytes[..]);
                u64::from_le_bytes(arr)
            },
            HEAP_SEG => {
                let bytes: Vec<u8> = $heap.iter().skip(($val & VALUE) as usize).take(8).map(|x| *x).collect();
                let mut arr: [u8; 8] = [0; 8];
                arr.copy_from_slice(&bytes[..]);
                u64::from_le_bytes(arr)
            },
            VAR_SEG => {
                let bytes: Vec<u8> = $vars.iter().skip(($val & VALUE) as usize).take(8).map(|x| *x).collect();
                let mut arr: [u8; 8] = [0; 8];
                arr.copy_from_slice(&bytes[..]);
                u64::from_le_bytes(arr)
            },
            _ => unimplemented!("SEGMENT: {}, VALUE: {}", $val & SEGMENT, $val)
        }
    }
}

macro_rules! get_str {
    ($init_ptr:expr, $globals:expr, $heap:expr, $vars:expr) => {
        | | -> Vec<u8> {
            let mut ptr = $init_ptr;
            let mut vec = vec![];
            loop {
                let ch = (deref!(ptr, $globals, $heap, $vars) & 0xff) as u8;
                ptr += 1;
                vec.push(ch);
                if ch == 0 {
                    break;
                }
            }
            vec
        }()
    }
}

const HEAP_SEG:   u64 = 0b00 << 62;
const VAR_SEG:    u64 = 0b01 << 62;
const GLOBAL_SEG: u64 = 0b10 << 62;
const CODE_SEG:   u64 = 0b11 << 62;

const SEGMENT: u64 = 0b11 << 62;
const VALUE:   u64 = (0b1 << 62) - 1;

enum FileDescriptor {
    STDIN,
    STDOUT,
    STDERR,
    #[allow(unused)]
    File(File),
}

pub fn interpret(intermediate: &Vec<Inst>) -> ! {
    let mut stack:      Vec<u64>                              = vec![];
    let mut registers:  [Register; 10]                        = [0; 10];
    let mut vars:       Vec<u8>                               = vec![];
    let mut global_idx: HashMap<String, usize>                = HashMap::new();
    let mut globals:    Vec<u8>                               = vec![];
    let mut fds:        Vec<FileDescriptor>                   = vec![];
    let mut loops:      HashMap<usize, (usize, usize)>        = HashMap::new();
    let mut ifs:        HashMap<usize, (usize, usize, usize)> = HashMap::new();

    let mut heap:       Vec<u8>                               = vec![];

    let mut var_offset: usize = 0;
    let mut arr_index: usize = 0;

    let strace: bool = false;

    fds.push(FileDescriptor::STDIN);
    fds.push(FileDescriptor::STDOUT);
    fds.push(FileDescriptor::STDERR);

    // setup args:
    globals.append(&mut 1u64.to_le_bytes().to_vec() /* argc */);
    globals.append(&mut 24u64.to_le_bytes().to_vec() /* argv[0] */);
    globals.append(&mut 0u64.to_le_bytes().to_vec() /* argv END */);
    globals.append(&mut 0u64.to_le_bytes().to_vec() /* envp END */);
    globals.append(&mut vec!['.' as u8, '/' as u8, 'm' as u8, 'a' as u8, 'i' as u8, 'n' as u8, 0, 0] /* argv[0]*/);

    let arg_ptr = 0 | GLOBAL_SEG;
    
    let mut ip = 0;
    loop {
        let inst = &intermediate[ip];
//        println!("{inst:?}");
//        println!("REGS:    {registers:#?}");
//        println!("VARS:    {vars:?}");
        //        println!("GLOBALS: {globals:#?}");
//        println!("STACK: {stack:?}");
        match inst {
            Inst::Func(_nm, offsets) => {
                let mut vec: Vec<(usize, String)> = offsets.iter().map(|(x, (o, _))| (*o, x.clone())).collect();
                vec.sort();
                stack.push(var_offset as u64);
                var_offset = if vars.is_empty() {
                    0
                } else { 
                    vars.len() + 8
                };

                for (index, a) in (0..offsets.len()).enumerate() {
                    let mut sz = offsets[&vec[a].1].1;
                    let mut add_off = 0;
                    while sz > 8 {
                        let idx = offsets[&vec[a].1].0 + add_off + var_offset;
                        if idx+8 >= vars.len() {
                            vars.resize(idx+8, 0);
                        }
                        for (i,ii) in (idx..(idx+8)).enumerate() {
                            vars[ii] = registers[index].to_le_bytes()[i];
                        }
                        sz -= 8;
                        add_off += 8;
                    }
                    let idx = offsets[&vec[a].1].0 + add_off + var_offset;

                    if idx+8 >= vars.len() {
                        vars.resize(idx+8, 0);
                    }
                    for (i,ii) in (idx..(idx+8)).enumerate() {
                        vars[ii] = registers[index].to_le_bytes()[i];
                    }
                }
                ip += 1;
            },
            Inst::VarSet(reg, sz, ind) => {
                let val = registers[*reg];
                let idx = *ind+var_offset;
                
                let inc = if sz < &8 { 8 } else { *sz };

                if idx + inc >= vars.len() {
                    vars.resize(idx + inc, 0);
                }
                for (i,ii) in (idx..(idx+8)).enumerate() {
                    vars[ii] = val.to_le_bytes()[i];
                }
                ip += 1;
            },
            Inst::If(reg, id) => {
                let (els, end) = find_if_else_end(intermediate, *id);
                ifs.insert(*id, (ip, els, end));
                let val = registers[*reg];
                if val == 0 {
                    ip = els;
                }
                ip += 1;
            },
            Inst::Endif(_) => {
                // nothing to do here...
                ip += 1;
            },
            Inst::WhileCheck(reg, id) => {
                let pred = registers[*reg];
                if pred != 0 {
                    ip += 1;
                } else {
                    ip = loops[id].1 + 1;
                }
            },
            Inst::WhileStart(_, id) => {
                let end = find_loop_end(intermediate, *id);
                loops.insert(*id, (ip, end));
                ip += 1;
            },
            Inst::Endwhile(id) => {
                ip = loops[id].0 + 1;
            },
            Inst::Ret(reg) => {
                let val = registers[*reg];
                registers[0] = val;
                var_offset = stack.pop().unwrap() as usize;
                ip = stack.pop().unwrap() as usize;
            },
            Inst::Call(f) => {
                stack.push((ip+1) as u64);
                ip = find_function(intermediate, f);
            },
            Inst::UnOp(reg, _, op) => {
                match op {
                    crate::util::UnaryOp::Not => {
                        registers[*reg] = (registers[*reg] == 0) as u64;
                    },
                }
                ip += 1;
            },
            Inst::BinOp((r1, r2), sz, op) => {
                match op {
                    BinaryOp::Add => {
                        registers[*r1] = registers[*r1] + registers[*r2];
                    },
                    BinaryOp::Sub => {
                        registers[*r1] = registers[*r1] - registers[*r2];
                    },
                    BinaryOp::Mul => {
                        registers[*r1] = registers[*r1] * registers[*r2];
                    },
                    BinaryOp::Div => {
                        registers[*r1] = registers[*r1] / registers[*r2];
                    },
                    BinaryOp::Mod => {
                        registers[*r1] = registers[*r1] % registers[*r2];
                    },
                    BinaryOp::Eq => {
                        let mask = ((1u128 << sz*8) - 1) as u64;
                        registers[*r1] = (registers[*r1] & mask == registers[*r2] & mask) as u64;
                    },
                    BinaryOp::BoolAnd => {
                        registers[*r1] = ((registers[*r1] != 0) && (registers[*r2] != 0)) as u64;
                    },
                    BinaryOp::BoolOr => todo!(),
                    BinaryOp::Less => {
                        registers[*r1] = ((registers[*r1] as i64) < (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::LessEq => {
                        registers[*r1] = ((registers[*r1] as i64) <= (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::Greater => {
                        registers[*r1] = ((registers[*r1] as i64) > (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::GreaterEq => {
                        registers[*r1] = ((registers[*r1] as i64) >= (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::Neq => {
                        registers[*r1] = !((registers[*r1] as i64) <= (registers[*r2] as i64)) as u64;
                    },
                }
                *sz as u64;
                ip += 1;
            },
            Inst::Val(reg, tp, val) => {
                let register: Register = match tp {
                    Type::Primitive(PrimitiveType::Bool)   => if val == "true" {1} else {0},
                    Type::Primitive(PrimitiveType::Char)   => val.bytes().next().unwrap() as u64,
                    Type::Primitive(PrimitiveType::Int)    => uc!(val.parse::<i64>().unwrap()),
                    Type::Primitive(PrimitiveType::Float)  => uc!(val.parse::<f64>().unwrap()),
                    Type::Array(a) => {
                        if **a == Type::Primitive(PrimitiveType::Char) {
                            let index;
                            let len = val.bytes().len();
                            let mut vec: Vec<u8> = len.to_le_bytes().into();
                            if ! global_idx.contains_key(val) {
                                index = globals.len() as u64;
                                
                                vec.append(&mut val.bytes().collect());
                                vec.append(&mut vec![0, 0, 0, 0, 0, 0, 0, 0]);
                                globals.append(&mut vec);
                                global_idx.insert(val.clone(), index as usize);
                            } else {
                                index = global_idx[val] as u64;
                            }
                            index | GLOBAL_SEG
                        } else {
                            unreachable!()
                        }
                    },
                    _ => unreachable!(),
                };

                registers[*reg] = register;
                ip += 1;
                
            },
            Inst::Var(reg, index, _sz, is_ref) => {
                let idx = index + var_offset;
                if *is_ref {
                    registers[*reg] = idx as u64 | VAR_SEG;
                } else {
                    let bytes: Vec<u8> = vars.iter().skip(idx).take(8).map(|x| *x).collect();
                    let mut arr: [u8; 8] = [0; 8];
                    arr.copy_from_slice(&bytes[..]);
                    registers[*reg] = u64::from_le_bytes(arr);
                }
                ip += 1;
            },
            Inst::RetVal(reg) => {
                registers[*reg] = registers[0];
                ip += 1;
            },
            Inst::Intrinsic(nm, _) => {
                stack.push(var_offset as u64);
                match nm.as_str() {
                    "get_args" => {
                        registers[0] = arg_ptr;
                    },
                    "str_to_ptr" => {
                        registers[0] += 8;
                    },
                    "dereference" => {
                        let val = registers[0];
                        registers[0] = deref!(val, globals, heap, vars);
                    },
                    // LINUX x86_64 sycall emulation, to make stdlib work
                    "syscall" => {
                        let id = registers[0];
                        match id {
                            // read(fd, buf, count)
                            0 => {
                                let fd    = registers[1];
                                let buf   = registers[2];
                                let count = registers[3];
                                
                                let mut bytes: Vec<u8> = vec![];

//                                stdout().flush().unwrap();
//                                stderr().flush().unwrap();
                                
                                if count > 0 {
                                    match fds[fd as usize] {
                                        FileDescriptor::STDIN => {
                                            stdin().take(count).read_to_end(&mut bytes).unwrap();
                                            if bytes.len() > 0 {
                                                assert_eq!(count as usize, bytes.len());
                                            }
                                    },
                                    FileDescriptor::STDOUT => {
                                        eprintln!("FATAL: Attempt to read from stdout");
                                        exit(1);
                                    }
                                    FileDescriptor::STDERR => {
                                        eprintln!("FATAL: Attempt to read from stderr");
                                        exit(1);
                                    },
                                    FileDescriptor::File(_) => unimplemented!(),
                                    }
                                }

                                let val = buf & VALUE;
                                match buf & SEGMENT {
                                    GLOBAL_SEG => {
                                        for (i, a) in bytes.iter().enumerate() {
                                            if (val as usize + i) >= globals.len() {
                                                for _ in globals.len() ..= (buf as usize + i) {
                                                    globals.push(0);
                                                }
                                            }
                                            globals[val as usize + i] = *a;
                                        }
                                    },
                                    HEAP_SEG => {
                                        for (i, a) in bytes.iter().enumerate() {
                                            heap[val as usize + i] = *a;
                                        }
                                    },
                                    _ => unimplemented!("SEGMENT: {}", buf & SEGMENT)
                                }

                                registers[0] = bytes.len() as u64;

                                if strace {
                                    eprintln!("read({fd}, {buf}, {count}) -> {}", registers[0]);
                                    stderr().flush().unwrap();
                                }
                            },

                            // write(fd, buf, count)
                            1 => {
                                let fd    = registers[1];
                                let buf   = registers[2];
                                let count = registers[3];

                                let string;
                                
                                match buf & SEGMENT {
                                    GLOBAL_SEG => {
                                        let bytes: Vec<u8> = globals.to_owned().
                                            into_iter().skip((buf & VALUE) as usize).take(count as usize).collect();
                                        string = String::from_utf8(bytes).unwrap();
                                    },
                                    HEAP_SEG => {
                                        let bytes: Vec<u8> = heap.to_owned().
                                            into_iter().skip((buf & VALUE) as usize).take(count as usize).collect();
                                        string = String::from_utf8(bytes).unwrap();
                                    }
                                    _ => unimplemented!("SEGMENT: {}", buf & SEGMENT)
                                }

                                if strace {
                                    eprintln!("write({fd}, {buf}, {count}) -> {}", registers[0]);
                                    stderr().flush().unwrap();
                                }

                                match fds[fd as usize] {
                                    FileDescriptor::STDIN => {
                                        eprintln!("FATAL: Attempt to write to stdin");
                                        exit(1);
                                    },
                                    FileDescriptor::STDOUT => print!("{}", string),
                                    FileDescriptor::STDERR => eprint!("{}", string),
                                    FileDescriptor::File(_) => unimplemented!(),
                                }
                            },
                            // brk(brk)
                            12 => {
                                let val = registers[1];
                                if val == 0 {
                                    registers[0] = heap.len() as u64;
                                } else {
                                    let old_len = heap.len();
                                    heap.resize(val as usize, 0);
                                    registers[0] = (heap.len() - old_len) as u64;
                                }

                                if strace {
                                    eprintln!("brk({val}) -> {}", registers[0]);
                                    stderr().flush().unwrap();
                                }
                            },
                            // nanosleep(rqtp, rmtp)
                            35 => {
                                unsafe {
                                    let a = registers[1];
                                    let b = registers[2];

                                    let ptr = a;
                                    let val1 = deref!(ptr, globals, heap, vars);
                                    
                                    let ptr = a-8;
                                    let val2 = deref!(ptr, globals, heap, vars);

                                    let str1 = libc::malloc(16) as *mut u64;
                                    *str1            = val1;
                                    *str1.offset(1)  = val2;

                                                                        let ptr = a;
                                    let val1 = deref!(ptr, globals, heap, vars);
                                    
                                    let ptr = a-8;
                                    let val2 = deref!(ptr, globals, heap, vars);

                                    let str2 = libc::malloc(16) as *mut u64;
                                    *str2            = val1;
                                    *str2.offset(1)  = val2;
                                    
                                    registers[0] = libc::nanosleep(str1 as *const libc::timespec,
                                                                   str2 as *mut libc::timespec)
                                        as u64;
                                    if strace {
                                        eprintln!("nanosleep({a}, {b}) -> {}", registers[0]);
                                        stderr().flush().unwrap();
                                    }
                                }
                            }
                            // fork()
                            57 => {
                                unsafe {
                                    registers[0] = libc::fork() as u64;
                                    if strace {
                                        eprintln!("fork() -> {}", registers[0]);
                                        stderr().flush().unwrap();
                                    }
                                }
                            },
                            // wait4(pid, stat_addr, options, ru)
                            61 => {
                                unsafe {
                                    let a = registers[1];
                                    let b = registers[2];
                                    let c = registers[3];
                                    let d = registers[4];

//                                    let a = size_of::<String>();

                                    registers[0] = libc::wait4(a as i32,
                                                              b as *mut i32,
                                                              c as i32,
                                                              d as *mut libc::rusage)
                                        as u64;
                                    if strace {
                                        eprintln!("wait4({a}, {b}, {c}, {d}) -> {}", registers[0]);
                                        stderr().flush().unwrap();
                                    }
                                }
                            },
                            // execve(filename, argv, envp)
                            59 => {
                                let a = registers[1]; // char*
                                let b = registers[2];
                                let c = registers[3];

                                let mut fstr = get_str!(a, globals, heap, vars);
                                let len =  fstr.len();
                                
                                let mut ptr = b;
                                let mut args = vec![];
                                loop {
                                    let arg = deref!(ptr, globals, heap, vars);
                                    let string = get_str!(arg, globals, heap, vars);

                                    if string[0] == 'q' as u8 {
                                        exit(42);
                                    }
                                    
                                    args.push(string);
                                    ptr += 8;
                                    if arg == 0 {
                                        break;
                                    }
                                }

                                let argc = args.len();

                                let mut ptr = c;
                                let mut envs = vec![];
                                loop {
                                    let env = deref!(ptr, globals, heap, vars);
                                    let string = get_str!(env, globals, heap, vars);
                                    envs.push(string);
                                    ptr += 8;
                                    if env == 0 {
                                        break;
                                    }
                                }
                                
                                let envc = args.len();

                                unsafe {
                                    let fname_ptr = libc::malloc(len+1) as *mut i8;
                                    libc::strcpy(fname_ptr, fstr.as_mut_ptr() as *mut i8);

                                    let argv = libc::malloc((argc+1) * 8) as *mut *const i8;
                                    for (i, mut a) in args.into_iter().enumerate() {
                                        let arg_ptr = libc::malloc(a.len()) as *mut i8;
                                        libc::strcpy(arg_ptr, a.as_mut_ptr() as *mut i8);
                                        *argv.offset(i as isize) = arg_ptr;
                                    }
                                    *argv.offset(argc as isize) = 0 as *const i8;

                                    let envp = libc::malloc((envc+1) * 8) as *mut *const i8;
                                    for (i, mut a) in envs.into_iter().enumerate() {
                                        let env_ptr = libc::malloc(a.len()) as *mut i8;
                                        libc::strcpy(env_ptr, a.as_mut_ptr() as *mut i8);
                                        *envp.offset(i as isize) = env_ptr;
                                    }
                                    *envp.offset(argc as isize) = 0 as *const i8;
                                    
                                    registers[0] = libc::execve(fname_ptr,
                                                                argv,
                                                                envp)
                                        as u64;

                                    if registers[0] == u64::MAX {
                                        libc::perror("ERROR\0".as_ptr() as *const i8);

                                    }
                                    
                                    if strace {
                                        eprintln!("execve({fname_ptr:?}, {argv:?}, {envp:?}) -> {}", registers[0]);
                                        stderr().flush().unwrap();
                                    }
                                }

                                unimplemented!("END")
                            },
//                            60 => {
//                                
//                            },
                            a => {
                                eprintln!("FATAL: Syscall {a} not implented");
                                exit(1);
                            }
                        }
                    },
                    "convert" => {
                        // only to disable type-checking, nothing to do
                    },
                    "set_ptr" => {
                        //                        "\tmov [rax], rbx\n"
                        let val  = registers[0];
                        let val2 = registers[1];
                        match val & SEGMENT {
                            GLOBAL_SEG => {
                                for (i, a) in val2.to_le_bytes().iter().enumerate() {
                                    globals[(val & VALUE) as usize + i] =  *a;
                                }
                            },
                            HEAP_SEG => {
                                for (i, a) in val2.to_le_bytes().iter().enumerate() {
                                    let ind = (val & VALUE) as usize + i;
                                    heap[ind] = *a;
                                }
                            },
                            a => unimplemented!("SEGMENT: {a}"),
                        }
                    }
                    _ => {
                        eprintln!("FATAL: Intrinsic {nm} not defined");
                        exit(1);
                    }
                }
                var_offset = stack.pop().unwrap() as usize;
                ip = stack.pop().unwrap() as usize;
            },
            Inst::Push(reg) => {
                let v = &registers[*reg];
                stack.push(*v);
                ip += 1;
            },
            Inst::Pop(reg) => {
                let v = &mut registers[*reg];
                *v = stack.pop().unwrap();
                ip += 1;
            },
            Inst::Arr(reg, siz) => {
                let pos = globals.len();
                let sz = if *siz < 8 {8} else {*siz};
                global_idx.insert(format!("arr_{arr_index}"), pos);
                globals.resize(pos + sz + 8 + 1, 0);

                for (i, a) in siz.to_le_bytes().iter().enumerate() {
                    globals[pos + i] = *a;
                }
                
                registers[*reg] = pos as u64 | GLOBAL_SEG;
                arr_index += 1;
                ip += 1;
            },
            Inst::Index(reg0, reg1, sz_vec, is_ref) => {
                if *is_ref {
                    registers[*reg0] = registers[*reg1] + 8 + registers[*reg0];
                } else {
                    let rr = sz_vec.len();
                    registers[*reg1] += 8 + registers[*reg0];
                    registers[rr] = registers[*reg1];
                    
                    let mut offset = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = reg0 + off;
                        let val = registers[rr] + offset as u64;
                        registers[r] = deref!(val, globals, heap, vars);
                        offset += sz;
                    }
                }
                ip += 1;
            },
            Inst::ArraySet(r0, r1, r2, sz_vec) => {
                if sz_vec.len() == 1 {
                    let a = registers[*r2] + 8 + registers[*r1];
                    
                    let val = a & VALUE;
                    match a & SEGMENT {
                        GLOBAL_SEG => {
                            for (i, v) in registers[*r0].to_le_bytes().iter().take(sz_vec[0]).enumerate() {
                                if (val as usize + i) >= globals.len() {
                                    for _ in globals.len() ..= (val as usize + i) {
                                        globals.push(0);
                                    }
                                }
                                globals[val as usize + i] = *v;
                            }
                        },
                        HEAP_SEG => {
                            for (i, v) in registers[*r0].to_le_bytes().iter().take(sz_vec[0]).enumerate() {
                                heap[val as usize + i] = *v;
                            }
                        },
                        a => unimplemented!("SEGMENT: {a}")
                    }
                } else {
                    let a = registers[*r2] + 8 + registers[*r1];
                    
                    let mut offset: usize = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = r0+off+1;
                        
                        let val = (a + offset as u64) & VALUE;
                        match (a + offset as u64) & SEGMENT {
                            GLOBAL_SEG => {
                                for (i, a) in registers[r].to_le_bytes().iter().take(sz_vec[off]).enumerate() {
                                    if (val as usize + i) >= globals.len() {
                                        for _ in globals.len() ..= (val as usize + i) {
                                            globals.push(0);
                                        }
                                    }
                                    globals[val as usize + i] = *a;
                                }
                            },
                            HEAP_SEG => {
                                for (i, a) in registers[r].to_le_bytes().iter().take(sz_vec[off]).enumerate() {
                                    heap[val as usize + i] = *a;
                                }
                            },
                            a => unimplemented!("SEGMENT: {a}")
                        }
                        offset += sz;
                    }
                }
                ip += 1;
            },
            Inst::Global(reg, name, _, off, is_ref) => {
                if *is_ref {
                    unimplemented!()
                } else {
                    if global_idx.contains_key(name) {
                        let bytes: Vec<u8> = globals.iter().skip(global_idx[name] + *off).take(8).map(|x| *x).collect();
                        let mut arr: [u8; 8] = [0; 8];
                        arr.copy_from_slice(&bytes[..]);
                        registers[*reg] = u64::from_le_bytes(arr);
                    } else {
                        registers[*reg] = 0;
                    }
                }
                ip += 1;
            },
            Inst::GlobalSet(reg, nm, _, off) => {
                if ! global_idx.contains_key(nm) {
                    global_idx.insert(nm.clone(), globals.len());
                    globals.append(&mut vec![0, 0, 0, 0, 0, 0, 0, 0]);
                }
                let idx = global_idx[nm];
                let val = registers[*reg];
                for (i, a) in val.to_le_bytes().iter().enumerate() {
                    globals[idx + off + i] = *a;
                }
                ip += 1;
            },
            Inst::Jump(func) => {
                if func == "_end" {
                    // finalize: return registers[0] as exit code
                    exit(registers[0] as i32);
                }
            },
            Inst::Field(reg, off, _) => {
                let val = registers[*reg] + *off as u64;
                registers[*reg] = match val & SEGMENT {
                    GLOBAL_SEG => {
                        let bytes: Vec<u8> = globals.iter().skip((val & VALUE) as usize).take(8).map(|x| *x).collect();
                        let mut arr: [u8; 8] = [0; 8];
                        arr.copy_from_slice(&bytes[..]);
                        u64::from_le_bytes(arr)
                    },
                    HEAP_SEG => {
                        let bytes: Vec<u8> = heap.iter().skip((val & VALUE) as usize).take(8).map(|x| *x).collect();
                        let mut arr: [u8; 8] = [0; 8];
                        arr.copy_from_slice(&bytes[..]);
                        u64::from_le_bytes(arr)
                    },
                    VAR_SEG => {
                        let rval = val - (*off * 2) as u64;
                        let bytes: Vec<u8> = vars.iter().skip((rval & VALUE) as usize).take(8).map(|x| *x).collect();
                        let mut arr: [u8; 8] = [0; 8];
                        arr.copy_from_slice(&bytes[..]);
                        u64::from_le_bytes(arr)
                    },
                    _ => unimplemented!("SEGMENT: {}", val & SEGMENT),
                };
                ip += 1;
            },
            Inst::Deref(_) => todo!(),
            Inst::SetField(reg0, reg1, off, _) => {
                let val = registers[*reg0] + *off as u64;
                match val & SEGMENT {
                    GLOBAL_SEG => {
                        for (i, a) in registers[*reg1].to_le_bytes().iter().enumerate() {
                            if (val as usize + i) >= globals.len() {
                                for _ in globals.len() ..= (val as usize + i) {
                                    globals.push(0);
                                }
                            }
                            globals[val as usize + i] = *a;
                        }
                    },
                    HEAP_SEG => {
                        for (i, a) in registers[*reg1].to_le_bytes().iter().enumerate() {
                            heap[val as usize + i] = *a;
                        }
                    },
                    a => unimplemented!("SEGMENT: {a}")
                }
                ip += 1;
            },
            Inst::Break(id) => {
                ip = loops[id].1 + 1;
            },
            Inst::FuncPtr(reg, f) => {
                registers[*reg] = CODE_SEG | (find_function(intermediate, f) as u64);
                ip += 1;
            },
            Inst::CallReg(reg) => {
                stack.push((ip + 1) as u64);
                let rval = registers[*reg];
                let seg = rval & SEGMENT;
                let ptr = match seg {
                    CODE_SEG => rval & VALUE,
                    _        => {
                        eprintln!("FATAL: invalid segment {seg} for function pointer.");
                        exit(1);
                    }
                };
                ip = ptr as usize;
            },
            Inst::Else(id) => {
                ip = find_if_else_end(intermediate, *id).1;
            },
        }
    }
}

