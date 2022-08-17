use std::{process::exit, mem, collections::HashMap, fs::File};

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

type Register = u64;

macro_rules! uc {
    ($a:expr) => {
        unsafe {mem::transmute($a)}
    }
}

const CODE_SEG:   u64 = 0b00 << 62;
const VAR_SEG:    u64 = 0b01 << 62;
const GLOBAL_SEG: u64 = 0b10 << 62;
const HEAP_SEG:   u64 = 0b11 << 62;

const SEGMENT: u64 = 0b11 << 62;
const VALUE:   u64 = (0b1 << 62) - 1;

enum FileDescriptor {
    STDIN,
    STDOUT,
    STDERR,
    File(File),
}

pub fn interpret(intermediate: &Vec<Inst>) -> ! {
    let mut stack:      Vec<u64>               = vec![];
    let mut registers:  [Register; 10]         = [0; 10];
    let mut vars:       HashMap<usize, u64>    = HashMap::new();
    let mut global_idx: HashMap<String, usize> = HashMap::new();
    let mut globals:    Vec<u8>                = vec![];
    let mut fds:        Vec<FileDescriptor>    = vec![];

    fds.push(FileDescriptor::STDIN);
    fds.push(FileDescriptor::STDOUT);
    fds.push(FileDescriptor::STDERR);
    
    let mut ip = 0;
    loop {
        let inst = &intermediate[ip];
//        println!("{inst:?}");
/*        println!("REGS:    {registers:#?}");
        println!("VARS:    {vars:#?}");
        println!("GLOBALS: {globals:#?}");*/
        match inst {
            Inst::Func(_nm, offsets) => {
                let mut vec: Vec<(usize, String)> = offsets.iter().map(|(x, (o, _))| (*o, x.clone())).collect();
                vec.sort();

                for (index, a) in (0..offsets.len()).enumerate() {
                    let mut sz = offsets[&vec[a].1].1;
                    let mut add_off = 0;
                    while sz > 8 {
                        vars.insert(offsets[&vec[a].1].0 + add_off, registers[index]);
                        sz -= 8;
                        add_off += 8;
                    }
                    vars.insert(offsets[&vec[a].1].0 + add_off, registers[index]);
                }
                ip += 1;
            },
            Inst::VarSet(reg, _sz, ind) => {
                let val = registers[*reg];
                vars.insert(*ind, val);
                ip += 1;
            },
            Inst::If(_, _) => todo!(),
            Inst::Endif(_) => todo!(),
            Inst::WhileCheck(_, _) => todo!(),
            Inst::WhileStart(_, _) => todo!(),
            Inst::Endwhile(_) => todo!(),
            Inst::Ret(reg) => {
                registers[0] = registers[*reg].clone();
                ip = stack.pop().unwrap() as usize;
            },
            Inst::Call(f) => {
                stack.push((ip+1) as u64);
                ip = find_function(intermediate, f);
            },
            Inst::UnOp(_, _, _) => todo!(),
            Inst::BinOp((r1, r2), sz, op) => {
                match op {
                    BinaryOp::Add => {
                        registers[*r1] = registers[*r1] + registers[*r2];
                    },
                    BinaryOp::Sub => todo!(),
                    BinaryOp::Mul => todo!(),
                    BinaryOp::Div => todo!(),
                    BinaryOp::Mod => todo!(),
                    BinaryOp::Eq => todo!(),
                    BinaryOp::BoolAnd => todo!(),
                    BinaryOp::BoolOr => todo!(),
                    BinaryOp::Less => todo!(),
                    BinaryOp::LessEq => todo!(),
                    BinaryOp::Greater => todo!(),
                    BinaryOp::GreaterEq => todo!(),
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
                if *is_ref {
                    unimplemented!();
                } else {
                    registers[*reg] = vars[index];
                }
                ip += 1;
            },
            Inst::RetVal(reg) => {
                registers[*reg] = registers[0];
                ip += 1;
            },
            Inst::Intrinsic(nm, _) => {
                match nm.as_str() {
                    "str_to_ptr" => {
                        registers[0] += 8;
                        ip = stack.pop().unwrap() as usize;
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
                            _ => unimplemented!("SEGMENT: {}", val & SEGMENT)
                        };
                        ip = stack.pop().unwrap() as usize;
                    },
                    // LINUX x86_64 sycall emulation, to make stdlib work
                    "syscall" => {
                        let id = registers[0];
                        match id {
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
                                    _ => unimplemented!("SEGMENT: {}", buf & SEGMENT)
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
                            a => {
                                eprintln!("FATAL: Syscall {a} not implented");
                                exit(1);
                            }
                        }
                        ip = stack.pop().unwrap() as usize;
                    },
                    _ => {
                        eprintln!("FATAL: Intrinsic {nm} not defined");
                        exit(1);
                    }
                }
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
            Inst::Index(_, _, _, _) => todo!(),
            Inst::ArraySet(_, _, _, _) => todo!(),
            Inst::Global(_, _, _, _, _) => todo!(),
            Inst::GlobalSet(_, _, _, _) => todo!(),
            Inst::Jump(func) => {
                if func == "_end" {
                    // finalize: return registers[0] as exit code
                    exit(registers[0] as i32);
                }
            },
            Inst::Field(_, _, _) => todo!(),
            Inst::Deref(_) => todo!(),
            Inst::SetField(_, _, _, _) => todo!(),
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
            Inst::Else(_) => todo!(),
        }
    }
}
