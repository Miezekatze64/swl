use std::{process::exit, collections::HashMap, fs::File};

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

    let strace: bool = false;

    fds.push(FileDescriptor::STDIN);
    fds.push(FileDescriptor::STDOUT);
    fds.push(FileDescriptor::STDERR);
    
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
                        registers[*r1] = (registers[*r1] == registers[*r2]) as u64;
                    },
                    BinaryOp::BoolAnd => {
                        registers[*r1] = ((registers[*r1] != 0) && (registers[*r2] != 0)) as u64;
                    },
                    BinaryOp::BoolOr => todo!(),
                    BinaryOp::Less => {
                        registers[*r1] = ((registers[*r1] as i64) < (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::LessEq => todo!(),
                    BinaryOp::Greater => {
                        registers[*r1] = ((registers[*r1] as i64) > (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::GreaterEq => {
                        registers[*r1] = ((registers[*r1] as i64) >= (registers[*r2] as i64)) as u64;
                    },
                    BinaryOp::Neq => todo!(),
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
                    "str_to_ptr" => {
                        registers[0] += 8;
                    },
                    "dereference" => {
                        let val = registers[0];
                        registers[0] = match val & SEGMENT {
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
                            _ => unimplemented!("SEGMENT: {}", val & SEGMENT)
                        };
                    },
                    // LINUX x86_64 sycall emulation, to make stdlib work
                    "syscall" => {
                        let id = registers[0];
                        match id {
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
                                }
                            },
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
            Inst::Arr(_, _) => todo!(),
            Inst::Index(reg0, reg1, sz_vec, is_ref) => {
                if *is_ref {
                    registers[*reg0] = registers[*reg1] + 8 + registers[*reg0];
                } else {
                    let rr = sz_vec.len();
                    registers[*reg1] += 8 + registers[*reg0];
                    registers[rr] = registers[1];
                    
                    let mut offset = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = reg0 + off;
                        let val = registers[rr] + offset as u64;
                        registers[r] = match val & SEGMENT {
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
                            _ => unimplemented!("SEGMENT: {}", val & SEGMENT)
                        };
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
            Inst::Break(_) => todo!(),
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
