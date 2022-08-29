use {crate::{intermediate::Inst,codegen_x86_64::generate_x86}, std::collections::HashMap};

pub fn generate(insts: Vec<Inst>, globals: &HashMap<String, usize>, externs: &Vec<String>) -> String {
    generate_x86(insts, globals, externs, "extern _ExitProcess@4\n\tpush rax\n\tcall _ExitProcess@4\n")
}
