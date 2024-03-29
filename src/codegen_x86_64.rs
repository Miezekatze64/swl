use {crate::{intermediate::Inst,
             util::{Type, PrimitiveType, UnaryOp, BinaryOp}},
     std::{fmt::Write, collections::HashMap}
};

macro_rules! min {
    ($a:expr, $b:expr) => {
        (if $a < $b {$a} else {$b})
    };
}

macro_rules! ceil {
    ($val:expr) => {
        if $val > ($val as i64) as f32 {($val as i64)+1} else {$val as i64}
    };
}

pub fn generate_x86(insts: Vec<Inst>, globals: &HashMap<String, usize>, externs: &Vec<(String, String)>, start: &'static str, exit: &'static str, extern_: fn(String, String) -> String) -> String {

    let datatype = |a: usize| match a {
        0|1 => "byte",
        2 => "word",
        4 => "dword",
        5..=8 => "qword",
        a => panic!("__invalid__({a})")
    };

    let register = |reg| match reg {
        0 => "rax",
        1 => "rbx",
        2 => "rcx",
        3 => "rdx",
        4 => "rdi",
        5 => "rsi",
        6 => "r8",
        7 => "r9",
        8 => "r10",
        a => unimplemented!("index: {a}")
    };

    let register_sz = |reg: usize, sz: usize| match register(reg) {
        "rax" => match sz {
            0|1 => "al",
            2 => "ax",
            4 => "eax",
            5..=8 => "rax",
            _ => "__invalid__"
        },
        "rbx" => match sz {
            0|1 => "bl",
            2 => "bx",
            4 => "ebx",
            5..=8 => "rbx",
            _ => "__invalid__"
        },
        "rcx" => match sz {
            0|1 => "cl",
            2 => "cx",
            4 => "ecx",
            5..=8 => "rcx",
            _ => "__invalid__"
        },
        "rdx" => match sz {
            0|1 => "dl",
            2 => "dx",
            4 => "edx",
            5..=8 => "rdx",
            _ => "__invalid__"
        },
        a => match sz {
            5..=8 => a,
            _ => "__invalid__",
        }
    };

    let mut global_strings: Vec<String> = vec![];
    let mut global_arrays: Vec<usize> = vec![];

    let mut ret: String = "\
    global _start\n\
    section .text\n".into();
    
    for (ext, _) in externs {
        ret.push_str(format!("extern {ext}\n").as_str());
    }
    
    ret.push_str(start);

    let mut intrinsic_labels: Vec<String> = vec![];
    let mut arr_index: usize = 0;

    let mut set_indicies = vec![];

    for a in insts {
        ret.push_str(match a {
            Inst::Func(name, offsets) => {
                let mut string = String::new();

                let mut vec: Vec<(usize, String)> = offsets.iter().
                    map(|(x, (o, _))| (*o, x.clone())).collect();
                vec.sort();
                set_indicies.clear();

                for (index, a) in (0..offsets.len()).enumerate() {
                    let mut sz = offsets[&vec[a].1].1;
                    let mut add_off = 0;
                    let mut rind = index;
                    while sz > 8 {
                        let _ = write!(string, "\tsub rsp, {ind_aligned}\n\tmov \
                                                [rbp-{ind}], {}\n",
                                       register(rind),
                                       ind         = offsets[&vec[a].1].0 - add_off,
                                       ind_aligned = ((offsets[&vec[a].1].0 + add_off) / 16 + 1) * 16,
                        );
                        sz -= 8;
                        rind += 1;
                        add_off += min!(8, sz);
                    }
                    let _ = write!(string, "\tsub rsp, {ind_aligned}\n\tmov \
                                            [rbp-{ind}], {}\n",
                                   register_sz(rind, sz),
                                   ind         = offsets[&vec[a].1].0 - add_off,
                                   ind_aligned = ((offsets[&vec[a].1].0 + add_off)  / 16 + 1) * 16
                    );
                }
                format!(";; FUNCTION DECL {name}\nf_{name}:\n\tpush \
                         rbp\n\tmov rbp, rsp\n{string}")
            },
            Inst::Call(name) => {
                format!(";; FUNCTION CALL {name}\n\tcall f_{name}\n")
            },
            Inst::If(reg, id) => {
                format!(";; IF {id} START\n\tcmp {}, 1\n\tjne \
                         .l2_{id}\n.l1_{id}:\n", register_sz(reg, 1))
            },
            Inst::Else(id) => {
                format!(";; ELSE {id} END\n\tjmp .l3_{id}\n.l2_{id}:\n")
            },
            Inst::Endif(id) => {
                format!(";; IF {id} END\n.l3_{id}:\n")
            },
            Inst::WhileCheck(reg, id) => {
                format!(";; WHILE {id} CHECK\n\tcmp {}, 1\n\tjne \
                         .loop_{id}_end\n", register_sz(reg, 1))
            },
            Inst::WhileStart(_, id) => {
                format!(";; WHILE {id} START\n.loop_{id}:\n")
            }
            Inst::Endwhile(id) => {
                format!(";; WHILE {id} END\n\tjmp .loop_{id}\n.loop_{id}_end\
                         :\n")
            },
            Inst::Break(id) => {
                format!(";; BREAK {id}\n\tjmp .loop_{id}_end\n")
            },
            Inst::Push(reg, sz) => {
                if sz == 8 {
                    format!(";; PUSH\n\tpush {}\n", register(reg))
                } else {
                    format!(";; PUSH \n\tsub rsp, {sz}\n\tmov {} [rsp], {}\n",
                            datatype(sz),
                            register_sz(reg, sz))
                }
            },
            Inst::Pop(reg, sz) => {
                if sz == 8 {
                format!(";; POP\n\tpop {}\n", register(reg))
                } else {
                    format!(";; POP \n\tmov {}, [rsp]\n\tadd rsp, {sz}\n",
                            register_sz(reg, sz))
                }
            },
            Inst::Val(reg, tp, val) => {
                let str_val = match tp {
                    Type::Primitive(a) => {
                        match a {
                            PrimitiveType::Int => val.clone(),
                            PrimitiveType::Long => val.clone(),
                            PrimitiveType::Float => unimplemented!(),
                            PrimitiveType::Char => {
                                format!("{}", val.as_bytes()[0])
                            },
                            PrimitiveType::Void => unreachable!(),
                            PrimitiveType::Bool => {
                                if val == "true" {
                                    "1"
                                } else {
                                    "0"
                                }.into()
                            },
                            PrimitiveType::Unchecked => val.clone(),
                        }
                    },
                    Type::Array(ref bx) => {
                        match **bx {
                            Type::Primitive(PrimitiveType::Char) => {
                                if ! global_strings.contains(&val.clone()) {
                                    global_strings.push(val.clone());
                                }
                                format!("str_{}", val.to_lowercase()
                                        .chars()
                                        .map(|a| format!("{}", a as u64))
                                        .collect::<String>())
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                };

                let register = register(reg);

                format!(";; VALUE {val}\n\tmov {register}, {str_val}\n")
            },
            Inst::Ret(reg) => {
                let register = register(reg);
                if register != "rax" {
                    format!(";; RETURN\n\tmov rax, {register}\
                             \n\tleave\n\tret\n")
                } else {
                    ";; RETURN\n\tleave\n\tret\n".into()
                }
            },
            Inst::UnOp(reg, sz, op) => {
                match op {
                    UnaryOp::Not => format!(";; NOT\n\ttest {r}, {r}\n\tsetz \
                                            {r}\n", r = register_sz(reg, sz)),
                }
            },
            Inst::BinOp(reg, sz, op) => {
                match op {
                    BinaryOp::Add => format!(";; ADD\n\tadd {}, {}\n",
                                            register(reg.0), register(reg.1)),
                    BinaryOp::Sub => format!(";; SUB\n\tsub {}, {}\n",
                                            register(reg.0), register(reg.1)),
                    BinaryOp::Mul => format!(";; MUL\n\timul {}, {}\n",
                                            register(reg.0), register(reg.1)),
                    BinaryOp::Div => format!(";; DIV\n\txor rdx, rdx\n\tmov \
                                             rax, {}\n\tdiv {}\n",
                                            register(reg.0), register(reg.1)),
                    BinaryOp::Eq  => format!(";; EQ\n\tcmp {r0}, {r1}\n\tsete \
                                             al\n\tmov {rr}, al\n",
                                            r0 = register_sz(reg.0, sz),
                                            r1 = register_sz(reg.1, sz),
                                            rr = register_sz(reg.0, 1)),
                    BinaryOp::Neq => format!(";; NEQ\n\tcmp {r0}, {r1}\n\t\
                                             setne al\n\tmov {rr}, al\n",
                                            r0 = register_sz(reg.0, sz),
                                            r1 = register_sz(reg.1, sz),
                                            rr = register_sz(reg.0, 1)),
                    BinaryOp::Less => format!(";; LESS\n\tcmp {r0}, {r1}\n\t\
                                              setl al\n\tmov {rr}, al\n",
                                             r0 = register_sz(reg.0, sz),
                                             r1 = register_sz(reg.1, sz),
                                             rr = register_sz(reg.0, 1)),
                    BinaryOp::Greater => format!(";; GREATER\n\tcmp {r0}, {r1}\
                                                 \n\tsetg al\n\tmov {rr}, al\n",
                                                r0 = register_sz(reg.0, sz),
                                                r1 = register_sz(reg.1, sz),
                                                rr = register_sz(reg.0, 1)),
                    BinaryOp::LessEq => format!(";; LESS OR EQUAL\n\tcmp {r0}, \
                                                {r1}\n\tsetle al\n\tmov {rr}, \
                                                al\n",
                                               r0 = register_sz(reg.0, sz),
                                               r1 = register_sz(reg.1, sz),
                                               rr = register_sz(reg.0, 1)),
                    BinaryOp::GreaterEq => format!(";; LESS\n\tcmp {r0}, {r1}\
                                                   \n\tsetge al\n\tmov {rr}, \
                                                   al\n",
                                                  r0 = register_sz(reg.0, sz),
                                                  r1 = register_sz(reg.1, sz),
                                                  rr = register_sz(reg.0, 1)),
                    BinaryOp::Mod => format!(";; MOD\n\tmov {rax}, {r0}\n\txor \
                                             {rdx}, {rdx}\n\tdiv {r1}\n\tmov {r0},\
                                             {rdx}\n",
                                            rax = register_sz(0, sz),
                                            rdx = register_sz(3, sz),
                                            r0 = register_sz(reg.0, sz),
                                            r1 = register_sz(reg.1, sz)),
                    BinaryOp::BoolAnd  => format!(";; AND\n\tand {r0}, {r1}\n",
                                                 r0 = register_sz(reg.0, sz),
                                                 r1 = register_sz(reg.1, sz)),
                    BinaryOp::BoolOr  => format!(";; AND\n\tor {r0}, {r1}\n",
                                                r0 = register_sz(reg.0, sz),
                                                r1 = register_sz(reg.1, sz)),
                    BinaryOp::BitwiseAnd => format!(";; BITWISE AND\n\tand {r0}, {r1}\n",
                                                   r0 = register_sz(reg.0, sz),
                                                   r1 = register_sz(reg.1, sz)),
                    BinaryOp::BitwiseOr => format!(";; BITWISE OR\n\tor {r0}, {r1}\n",
                                                   r0 = register_sz(reg.0, sz),
                                                   r1 = register_sz(reg.1, sz)),

                }
            },
            Inst::VarSet(reg, sz, index) => {
                if ! set_indicies.contains(&index) {
                    set_indicies.push(index);
                    format!(";; SET VAR {index}\n\tsub rsp, {ind_aligned}\n\tmov {tp} \
                             [rbp-{ind}], {}\n",
                            register_sz(reg, sz),
                            ind_aligned = (index / 16 + 1) * 16, ind = index, tp = datatype(sz))
                } else {
                    format!(";; SET VAR {index}\n\tmov {tp} \
                             [rbp-{ind}], {}\n",
                            register_sz(reg, sz), ind = index, tp = datatype(sz))
                }
            },
            Inst::Var(reg, index, sz, is_ref) => {
                if is_ref {
                    format!(";; GET VAR REF {index}\n\tmov {r}, \
                             rbp\n\tsub {r}, {}\n", index, r = register(reg))
                } else {
                    let mut size = sz;
                    let mut string = String::new();
                    let mut i = 0;
                    while size > 8 {
                        let _ = writeln!(string, "\tmov {}, [rbp-{}]",
                                         register(reg + ceil!(i as f32 / 8.0) as usize), index-i);
                        size -= 8;
                        i += 8;
                    }
                    format!(";; GET VAR {index}\n{string}\tmov {}, [rbp-{}]\n",
                            register_sz(reg + ceil!(i as f32 / 8.0) as usize, size), index-i)
                }
            },
            Inst::CallReg(reg) => {
                format!(";; CALL PTR\n\tcall {}\n", register(reg))
            },
            Inst::Intrinsic(iname, fname) => {
                if ! intrinsic_labels.contains(&iname) {
                    intrinsic_labels.push(iname.clone());
                    format!(";; INTRINSIC {iname}\nf_{fname}:\
                             \n\tpush rbp\n\tmov rbp, rsp\n\tjmp intrinsic_{iname}\nintrinsic_{iname}:\
                             \n{}\n", intrinsics().get(iname.as_str()).unwrap())
                } else {
                    format!(";; INTRINSIC {iname}\nf_{fname}:\
                             \n\tpush rbp\n\tmov rbp, rsp\n\tjmp intrinsic_{iname}\n")
                }
            },
            Inst::RetVal(reg) => {
                if reg != 0 {
                    format!(";; GET RETURN VALUE\n\tmov {}, rax\n",
                            register(reg))
                } else {
                    "".into()
                }
            },
            Inst::Arr(reg, size) => {
                global_arrays.push(size);
                arr_index += 1;
                format!(";; GET ARRAY\n\tmov {r0}, arr_{arr}\n",
                        arr = arr_index -1, r0 = register(reg))
            },
            Inst::Index(reg0, reg1, sz_vec, is_ref) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                if is_ref {
                    format!(";; ARRAY INDEX\n\tadd {r1}, \
                             8\n\tadd {r1}, {r0}\n\tmov {r0}, {r1}\n")
                } else {
                    let mut string: String = String::new();
                    string.push_str(";; ARRAY INDEX\n");
                    let rr = register(sz_vec.len());
                    string.push_str(format!(
                        "\tadd {r1}, 8\n\tadd {r1}, {r0}\n\tmov {rr}, {r1}\n")
                                    .as_str());
                    let mut offset = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = register_sz(reg0 + off, *sz);
                        string.push_str(
                            format!("\tmov {r}, [{rr}+{offset}]\n").as_str());
                        offset += sz;
                    }

                    string
                }
            },
            Inst::ArraySet(reg0, reg1, reg2, sz_vec) => {
                let r0 = register(reg0);
                let r1 = register(reg1);
                let r2 = register(reg2);

                if sz_vec.len() == 1 {
//                    let r0s = register_sz(reg0, sz_vec[0]);
                    let r1s = register_sz(reg1, sz_vec[0]);
                    format!(";; SET ARRAY INDEX\n\tpush {r0}\n\tmov {r0}, \
                             {r2}\n\tadd {r0}, 8\n\tadd {r0}, \
                             {r1}\n\tpop {r1}\n\tmov [{r0}], {r1s}\n")
                } else {
                    let mut string: String = String::new();
                    string.push_str(";; SET ARRAY INDEX\n");
                    for (off, _) in sz_vec.iter().enumerate() {
                        let r = register(reg0+(sz_vec.len() - off-1));
                        string.push_str(format!("\tpush {r}\n").as_str());
                    }

                    string.push_str(format!("\tmov {r0}, {r2}\n\tadd {r0}, \
                                             8\n\tadd {r0}, {r1}\n").as_str());
                    let mut offset = 0;
                    for (off, sz) in sz_vec.iter().enumerate() {
                        let r = register(reg0+off+1);
                        let rs = register_sz(reg0+off+1, *sz);
                        string.push_str(format!("\tpop {r}\n\tmov [{r0}+{}], \
                                                 {rs}\n", offset).as_str());
                        offset += sz;
                    }

                    string
                }
            },
            Inst::Global(reg0, name, sz, off, is_ref) => {
                if is_ref {
                    format!(";; GET GLOBAL REF\n\tmov {r}, global_{name}\n\t\
                             add {r}, {off}\n", r = register_sz(reg0, sz))
                } else {
                    format!(";; GET GLOBAL\n\tmov {}, [global_{name}+{off}]\n",
                            register_sz(reg0, sz))
                }
            },
            Inst::GlobalSet(reg0, name, sz, offset) => {
                format!(";; SET GLOBAL\n\tmov {tp} [global_{name}+{offset}], \
                         {}\n",register_sz(reg0, sz), tp = datatype(sz))
            },
            Inst::Jump(name) => {
                format!(";; JUMP\n\tjmp {name}\n")
            },
            Inst::Field(reg, off, _) => {
                let r0 = register(reg);
                format!(";; GET STRUCT FIELD\n\tadd {r0}, {off}\
                         \n\tmov {r0}, [{r0}]\n")
            },
            Inst::SetField(reg0, reg1, off, sz) => {
                let r0 = register(reg0);
                let r1 = register_sz(reg1, sz);
                format!(";; SET STRUCT FIELD\n\tadd {r0}, \
                         {off}\n\tmov [{r0}], {r1}\n")
            }
            Inst::Deref(reg, sz) => {
                let index = 0;
                let mut size = sz;
                let mut string = String::new();
                let mut i = 0;

                let r = register(reg + size / 8 + 1);
                writeln!(string, "\tmov {r}, {}", register(reg)).unwrap();
                
                while size > 8 {
                    writeln!(string, "\tmov {}, [{r}+{}]",
                             register(reg+i/8),
                             index+i)
                        .unwrap();
                    size -= 8;
                    i += 8;
                }
                format!(";; DEREFERENCE\n{string}\tmov {}, [{r}+{}]\n",
                        register_sz(reg+i/8, size),
                        index+i)
//                format!(";; DEREFERENCE\n\tmov {r}, [{r}]\n", r = register(reg))
            },
            Inst::FuncPtr(reg, func) => {
                format!(";; FUNCTION ADRESS\n\tmov {r}, f_{func}\n",
                        r = register(reg))
            },
            Inst::DerefSet(r1, r2, sz) => {
                let mut size = sz;
                let mut string = String::new();
                let mut i = 0;

                let reg1 = register(r1);
                
                while size > 8 {
                    writeln!(string, "\tmov [{reg1}+{i}], {}",
                             register(r2 + i/8))
                        .unwrap();
                    size -= 8;
                    i += 8;
                }

                format!(";; DEREF SET\n{string}\tmov [{reg1}+{i}], {}\n",
                        register(r2 + i / 8))
            },
            Inst::Extern(ename, name) => {
                extern_(ename, name)
            },

        }.as_str())
    }
    ret.push_str(&("_end:\n;; EXIT\n".to_owned()
                 + exit +"\nsection .data\nARGS:\n\tresb 8\n"));

    for a in global_strings {
        let bytes: String = a.bytes().map(|x| x.to_string())
            .collect::<Vec<String>>().join(", ");
        ret.push_str(format!("str_{}:\n\tdq {}\n\tdb {bytes}\n",
                             a.to_lowercase().chars().
                             map(|a| format!("{}", a as u64))
                             .collect::<String>(), a.len()).as_str());
    }

    for (ind, a) in global_arrays.into_iter().enumerate() {
        ret.push_str(format!("arr_{}:\n\tdq {a}\n\tresb {a}\n", ind).as_str());
    }

    for (val, sz) in globals {
        ret.push_str(format!("global_{val}:\n\tresb {sz}\n").as_str());
    }

    ret
}

pub fn intrinsics() -> HashMap<&'static str, &'static str> {
    [("syscall","\tpush rax\n\tpush rbx\n\tpush rcx\n\tpush \
                 rdx\n\tpop rdx\n\tpop \
                 rsi\n\tpop rdi\n\tpop rax\n\tsyscall\n\tleave\n\tret"),
     ("convert", "\tleave\n\tret\n"),
     ("str_to_ptr", "\tadd rax, 8\n\tleave\n\tret\n"),
     ("dereference", "\tmov rax, [rax]\n\tleave\n\tret\n"),
     ("set_ptr", "\tmov [rax], rbx\n\tleave\n\tret\n"),
     ("get_args", "\tmov rax, [ARGS]\n\tleave\n\tret\n")
    ]
        .iter()
        .cloned()
        .collect()
}
