use {crate::{intermediate::Inst,codegen_x86_64::generate_x86}, std::collections::HashMap};

fn extern_(ename: String, name: String) -> String {
    format!(";; EXTERN {ename}\nf_{name}:\n\
             \tpush rbp\n\
             \tmov rbp, rsp\n\
             \
             \tpush rax\n\
             \tpush rbx\n\
             \tpush rcx\n\
             \tpush rdx\n\
             \
             \tpop r9\n\
             \tpop r8\n\
             \tpop rdx\n\
             \tpop rcx\n\
             \tpush rsp\n\
             \tpush rsi\n\
             \tpush rdi\n\
             \tcall {ename}\n\
             \tpop rdi\n\
             \tpop rsi\n\
             \tpop rsp\n\
             \tleave\n\
             \tret\n")
}

pub fn generate(insts: Vec<Inst>, globals: &HashMap<String, usize>, externs: &Vec<(String, String)>) -> String {
    generate_x86(insts, globals, externs, "_start:\n\
                                           \tmov [ARGS], rsp\n\
                                           \tpush rsp\n\
                                           \tadd rsp, 8\n\
                                           \txor rbp, rbp\n\
                                           \txor rax, rax\n\
                                           ",
                 "\tret\n", extern_)
}
