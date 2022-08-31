use {crate::{intermediate::Inst,codegen_x86_64::generate_x86}, std::collections::HashMap};

fn extern_(ename: String, name: String) -> String {
    format!(";; EXTERN {ename}\n\
             f_{name}:\n\
             \tpush rbp\n\
             \tmov rbp, rsp\n\
             \
             \tpush rax\n\
             \tpush rbx\n\
             \tpush rcx\n\
             \tpush rdx\n\
             \tpush rdi\n\
             \tpush rsi\n\
             \
             \tpop r9\n\
             \tpop r8\n\
             \tpop rcx\n\
             \tpop rdx\n\
             \tpop rsi\n\
             \tpop rdi\n\
             \tcall {ename}\n\
             \tleave\n\
             \tret\n")
}

pub fn generate(insts: Vec<Inst>, globals: &HashMap<String, usize>, externs: &Vec<(String, String)>) -> String {
    generate_x86(insts, globals, externs, "_start:\n\
                                           \tmov [ARGS], rsp\n\
                                           \txor rbp, rbp\n\
                                           \txor rax, rax\n\
                                           ",
                 "\tmov rdi, rax\n\tmov rax, 60\n\tsyscall", extern_)
}
