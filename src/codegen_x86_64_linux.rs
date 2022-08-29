use {crate::{intermediate::Inst,codegen_x86_64::generate_x86}, std::collections::HashMap};

fn extern_(name: String) -> String {
    format!(";; EXTERN {name}\nf_{name}:\n\
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
             \tcall {name}\n\
             \tleave\n\
             \tret\n")
}

pub fn generate(insts: Vec<Inst>, globals: &HashMap<String, usize>, externs: &Vec<String>) -> String {
    generate_x86(insts, globals, externs, "\tmov rdi, rax\n\tmov rax, 60\n\tsyscall", extern_)
}
