use {std::collections::HashMap, crate::{parser::{ExpressionR, ASTNodeR, Expression}, util::{Op, PrimitiveType, BinaryOp, Type, UnaryOp}}};

macro_rules! ceil {
    ($val:expr) => {
        if $val > ($val as i64) as f32 {($val as i64)+1} else {$val as i64}
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    Func(String, HashMap<String, (usize, usize)>),
    VarSet(usize, usize, usize),
    If(usize, usize),
    Endif(usize),
    WhileCheck(usize, usize),
    WhileStart(usize, usize),
    Endwhile(usize),
    Ret(usize),
    Call(String),
    UnOp(usize, usize, UnaryOp),
    BinOp((usize, usize), usize, BinaryOp),
    Val(usize, Type, String),
    Var(usize, usize, usize, bool),
    RetVal(usize),
    Intrinsic(String, String),
    Push(usize, usize),
    Pop(usize, usize),
    Arr(usize, usize),
    Index(usize, usize, Vec<usize>, bool),
    ArraySet(usize, usize, usize, Vec<usize>),
    Global(usize, String, usize, usize, bool),
    GlobalSet(usize, String, usize, usize),
    Jump(String),
    Field(usize, usize, Type),
    Deref(usize, usize),
    SetField(usize, usize, usize, usize),
    Break(usize),
    FuncPtr(usize, String),
    CallReg(usize),
    Else(usize),
    DerefSet(usize, usize, usize),
    Extern(String, String),
}

fn gen_expr(expr: Expression, index: usize, indicies: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: &HashMap<String, Type>, is_ref: bool) -> Vec<Inst> {
    let mut ret = vec![];
    let Expression(_pos, expr, tp) = expr;
    match expr {
        ExpressionR::UnaryOp(op, fst, _) => {
            ret.push(Inst::Push(index, 8));
            ret.append(&mut gen_expr(*fst, index, indicies, globals, aliases, false));
            ret.push(Inst::Pop(index, 8));
            ret.push(Inst::UnOp(index, tp.unwrap().size(aliases), op));
        },
        ExpressionR::T(fst, op, snd, _) => {
            ret.append(&mut gen_expr(*fst.clone(), index, indicies, globals, aliases, false));
            ret.push(Inst::Push(index, 8));
            ret.append(&mut gen_expr(*snd, index+1, indicies, globals, aliases, false));
            ret.push(Inst::Pop(index, 8));
            if let Op::Binary(op) = op {
                ret.push(Inst::BinOp((index, index+1), fst.2.unwrap().size(aliases), op));
            } else {
                unreachable!();
            }
        },
        ExpressionR::StructLiteral(_, fields) => {
            // convert hashmap to vector
            let mut vector: Vec<String> = fields.iter().map(|(x, _)| x.clone()).collect();
            vector.sort();

            let mut sz = 0;
            for key in vector.iter().rev() {
                ret.append(&mut gen_expr(fields[key].clone(), 0, indicies, globals, aliases, false));
                let size = fields[key].2.as_ref().unwrap().size(aliases);
                ret.push(Inst::Push(0, size));
                sz += size;
            }

            let mut i = sz;
            let mut r = 0;
            while i > 8 {
                ret.push(Inst::Pop(r, 8));
                i -= 8;
                r += 1;
            }
            ret.push(Inst::Pop(r, i));
        },
        ExpressionR::Val(tp, val) => {
            if !is_ref {
                ret.push(Inst::Val(index, tp, val));
            } else {
                ret.push(Inst::Val(index, tp, val));
                let pos = last_ptr(indicies) + 8;
                ret.push(Inst::VarSet(index, 8, pos));
                ret.push(Inst::Var(index, pos, 8, true));
            }
        },
        ExpressionR::Var(name) => {
            if indicies.contains_key(&name) {
                ret.push(Inst::Var(index, indicies.get(&name).unwrap().0, indicies.get(&name).unwrap().1, is_ref));
            } else if globals.contains_key(&name) {
                let size = *globals.get(&name).unwrap();
                if size < 8 || is_ref {
                    ret.push(Inst::Global(index, name, size, 0, is_ref));
                } else {
                    let mut sz = size;
                    let mut ind = 0;
                    while sz > 8 {
                        ret.push(Inst::Global(index+ind/8, name.clone(), 8, ind, is_ref));
                        ind += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::Global(index+ind/8, name, sz, ind, is_ref));
                }
            } else {
                ret.push(Inst::FuncPtr(index, name));
            }
        },
        ExpressionR::F(expr, args) => {
            let mut ind = 0;
            for a in args.clone() {
                let sz = a.2.as_ref().unwrap().size(aliases);
                ret.append(&mut gen_expr(a, ind, indicies, globals, aliases, false));
                for _ in 0 .. ceil!(sz as f32 / 8.0) {
                    ret.push(Inst::Push(ind, 8));
                    ind += 1;
                }
            }
            let find = ind;
            ret.append(&mut gen_expr(*expr, find, indicies, globals, aliases, false));

            for a in args.iter().rev() {
                let sz = a.2.as_ref().unwrap().size(aliases);
                for _ in 0 .. ceil!(sz as f32 / 8.0) {
                    ind -= 1;
                    ret.push(Inst::Pop(ind, 8));
                }
            }

            ret.push(Inst::CallReg(find));

            if is_ref {
                let len = tp.unwrap().size(aliases);
                for i in (0..(len/8)).rev() {
                    indicies.insert("__struct_reserved__".into(), (last_ptr(indicies)+8, 8));
                    ret.push(Inst::VarSet(index+i, 8, last_ptr(indicies)));
                }
                ret.push(Inst::Var(index, last_ptr(indicies), 8, true));
            }
            ret.push(Inst::RetVal(index));
        },
        ExpressionR::MemberFunction(ltype, lexpr, name, args) => {
            ret.append(&mut gen_expr(*lexpr, 0, indicies, globals, aliases, false));
            let mut ind = 1;
            for a in args.clone() {
                ret.append(&mut gen_expr(a, ind, indicies, globals, aliases, false));
                ret.push(Inst::Push(ind, 8));
                ind += 1;
            }

            for _ in args.iter().rev() {
                ind -= 1;
                ret.push(Inst::Pop(ind, 8));
            }
            
            ret.push(Inst::Call(ltype.to_label() + &name));
            if is_ref {
                let len = tp.unwrap().size(aliases);
                for i in (0..(len/8)).rev() {
                    indicies.insert("__struct_reserved__".into(), (last_ptr(indicies)+8, 8));
                    ret.push(Inst::VarSet(index+i, 8, last_ptr(indicies)));
                }
                ret.push(Inst::Var(index, last_ptr(indicies), 8, true));
            }
            ret.push(Inst::RetVal(index));
        },
        ExpressionR::Ref(expr) => {
            ret.append(&mut gen_expr(*expr, index, indicies, globals, aliases, true));
        },
        ExpressionR::Deref(expr) => {
            ret.append(&mut gen_expr(*expr, index, indicies, globals, aliases, false));
            ret.push(Inst::Deref(index, tp.unwrap().size(aliases)));
        }
        ExpressionR::Arr(_) => {
            todo!();
        },
        ExpressionR::Undef => {
            
        },
        ExpressionR::ArrAlloc(tp, sz) => {
            ret.push(Inst::Arr(index, sz * tp.size(aliases)));
        },
        ExpressionR::Index(lexpr, expr, expr_vec) => {
            ret.append(&mut gen_expr(*expr, index, indicies, globals, aliases, false));
            ret.push(Inst::Val(index+1, Type::Primitive(PrimitiveType::Int), tp.unwrap().size(aliases).to_string()));
            ret.push(Inst::BinOp((index, index+1), 8, BinaryOp::Mul));
            ret.push(Inst::Push(index, 8));
            ret.append(&mut gen_expr(*lexpr, index+1, indicies, globals, aliases, false));
            ret.push(Inst::Pop(index, 8));
            ret.push(Inst::Index(index, index+1, expr_vec, is_ref));
        },
        ExpressionR::StructField(expr, field, struct_type, deref) => {
            ret.append(&mut gen_expr(*expr, index, indicies, globals, aliases, !deref));
            
            let mut offset = 0;
            if let Some(Type::Struct(_, fields)) = struct_type {
                // convert hashmap to vector
                for (key, (tp, _)) in fields {
                    if key == field {
                        break;
                    }
                    offset += tp.size(aliases);
                }
            } else {
                unreachable!("should always be a struct (error in type-checking): TYPE = {struct_type:?}")
            }
            ret.push(Inst::Field(index, offset, tp.unwrap().dealias(aliases)));
        }
        ExpressionR::Cast(ltp, expr, rtp_) => {
            assert!(rtp_.is_some());
            let rtp = rtp_.unwrap();

            ret.append(&mut gen_expr(*expr, index, indicies, globals, aliases, false));

            let lsz = ltp.size(aliases);
            let rsz = rtp.size(aliases);

            let bits_mask = 2u128.pow(rsz as u32 * 8) - 1;
            
            if lsz >= rsz {
                ret.push(Inst::Val(index+1, Type::Primitive(PrimitiveType::Int), bits_mask.to_string()));
                ret.push(Inst::BinOp((index, index+1), 8, BinaryOp::BitwiseAnd));
            }
        },
    }
    ret
}


fn last_ptr(offsets: &HashMap<String, (usize, usize)>) -> usize {
    let mut last = 0;
    for el in offsets {
        last = last.max(el.1.0);
    }
    last
}

fn global_init(ast: &ASTNodeR, offsets: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: &HashMap<String, Type>, ret: &mut Vec<Inst>) {
    // check for global initializations
    if let ASTNodeR::Block(a) = ast {
        for inst in a {
            match &inst.1 {
                ASTNodeR::VarInit(ref name, ref expr, tp) => {
                    ret.append(&mut gen_expr(expr.clone(), 0, offsets, globals, &aliases, false));
                    if ! offsets.contains_key(name) {
                        let len = globals.get(name).unwrap();
                        if *len < 8 {
                            ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
                        } else if let Some(Type::Struct(_, fields)) = tp {
                            let mut offset = 0;
                            for (index, (_, vals)) in fields.iter().enumerate() {
                                let sz = vals.0.size(&aliases);
                                ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                                offset += sz;
                            }
                        } else {
                            let mut sz = *len;
                            let mut index = 0;
                            while sz > 8 {
                                ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                                index += 8;
                                sz -= 8;
                            }
                            ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                        }
                    }
                },
                ASTNodeR::VarDecInit(_, tp, ref name, ref expr) => {
                    ret.append(&mut gen_expr(expr.clone(), 0, offsets, globals, &aliases, false));
                    if ! offsets.contains_key(name) {
                        let len = globals.get(name).unwrap();
                        if *len < 8 {
                            ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
                        } else if let Type::Struct(_, fields) = tp.dealias(&aliases) {
                            let mut offset = 0;
                            for (index, (_, vals)) in fields.iter().enumerate() {
                                let sz = vals.0.size(&aliases);
                                ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                                offset += sz;
                            }
                        } else {
                            let mut sz = *len;
                            let mut index = 0;
                            while sz > 8 {
                                ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                                index += 8;
                                sz -= 8;
                            }
                            ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                        }
                    }
                },
                ASTNodeR::Include(_, ast, _) => {
                    global_init(&ast.1, offsets, globals, aliases, ret);
                }
                _ => {}
            }
        }
    }
}

pub fn gen(ast: ASTNodeR, offsets: &mut HashMap<String, (usize, usize)>, globals: &HashMap<String, usize>, aliases: HashMap<String, Type>, loop_idx: usize, index:  usize, is_top_level: bool) -> (Vec<Inst>, HashMap<String, usize>, Vec<(String, String)>) {
    let mut ret     = vec![];
    let mut externs = vec![];

    if is_top_level {

        global_init(&ast, offsets, globals, &aliases, &mut ret);
        
        // call main function
        ret.push(Inst::Call("main".into()));
        ret.push(Inst::Jump("_end".into()));
    }
        
    match ast {
        ASTNodeR::Block(a) => {
            let mut count: usize = index;
            for i in a {
                count += 3;
                let (mut res, _, mut exts) = gen(i.1, offsets, globals, aliases.clone(), loop_idx, count, false);
                externs.append(&mut exts);
                ret.append(&mut res);
            }
        },
        ASTNodeR::VarDec(is_global, tp, ref name) => {
            if ! is_global {
                offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
            }
        },
        ASTNodeR::ArrIndexInit(lexpr, ind, expr, expr_vec) => {
            ret.append(&mut gen_expr(expr, 0, offsets, globals, &aliases, false));
            let index = expr_vec.len();
            ret.append(&mut gen_expr(ind, index, offsets, globals, &aliases, false));
            if let Type::Array(tp) = lexpr.2.as_ref().unwrap().dealias(&aliases) {
                ret.push(Inst::Val(index+1, Type::Primitive(PrimitiveType::Int), tp.size(&aliases).to_string()));
                ret.push(Inst::BinOp((index, index+1), 8, BinaryOp::Mul));
                ret.append(&mut gen_expr(lexpr, index+1, offsets, globals, &aliases, false));
                ret.push(Inst::ArraySet(0, index, index+1, expr_vec));
            } else {
                unreachable!()
            }
        },
        ASTNodeR::VarDecInit(is_global, tp, ref name, expr) => {
            if ! is_global {
                offsets.insert(name.clone(), (last_ptr(offsets) + tp.size(&aliases), tp.size(&aliases)));
            }
            ret.append(&mut gen_expr(expr, 0, offsets, globals, &aliases, false));
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                let size = vals.1;
                if size < 8 {
                    ret.push(Inst::VarSet(0, vals.1, vals.0))
/*                } else if let Type::Struct(_, fields) = tp.dealias(&aliases) {
                    let mut offset = vals.0;
                    for (index, (_, vals)) in fields.iter().enumerate() {
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::VarSet(index, sz, offset));
                        offset -= sz;
                    }
*/              } else {
                    let mut sz = size;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::VarSet(index/8, 8, vals.0 - index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::VarSet(index/8, sz, vals.0 - index));
                }
            } else {
                let len = globals.get(name).unwrap();
                if *len < 8 {
                    ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
/*                } else if let Type::Struct(_, fields) = tp {
                    let mut offset = 0;
                    for (index, (_, vals)) in fields.iter().enumerate() {
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                        offset += sz;
                    }
*/                } else {
                    let mut sz = *len;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                }
            }
        },
        ASTNodeR::VarOp(ref name, op, expr) => {
            ret.append(&mut gen_expr(expr, 1, offsets, globals, &aliases, false));
            
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                ret.push(Inst::Var(0, vals.0, vals.1, false));
                ret.push(Inst::BinOp((0, 1), vals.1, op));
                ret.push(Inst::VarSet(0, vals.1, vals.0))
            } else {
                let len = globals.get(name).unwrap();
                ret.push(Inst::Global(0, name.into(), 8, 0, false));
                ret.push(Inst::BinOp((0, 1), *len, op));
                ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
            }
        },
        ASTNodeR::VarInit(ref name, expr, _) => {
            ret.append(&mut gen_expr(expr, 0, offsets, globals, &aliases, false));
            if offsets.contains_key(name) {
                let vals = offsets.get(name).unwrap();
                let size = vals.1;
                if size < 8 {
                    ret.push(Inst::VarSet(0, vals.1, vals.0));
/*                } else if let Some(Type::Struct(_, fields)) = tp {
                    let mut offset = vals.0;
                    for (index, (_, vals)) in fields.iter().enumerate() {
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::VarSet(index, sz, offset));
                        offset -= sz;
                    }
*/                } else {
                    let mut sz = size;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::VarSet(index/8, 8, vals.0 - index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::VarSet(index/8, sz, vals.0 - index));
                }
            } else {
                let len = globals.get(name).unwrap();
                if *len < 8 {
                    ret.push(Inst::GlobalSet(0, name.clone(), *len, 0))
/*                } else if let Some(Type::Struct(_, fields)) = tp {
                    let mut offset = 0;
                    for (index, (_, vals)) in fields.iter().enumerate() {
                        let sz = vals.0.size(&aliases);
                        ret.push(Inst::GlobalSet(index, name.clone(), sz, offset));
                        offset += sz;
                    }
*/                } else {
                    let mut sz = *len;
                    let mut index = 0;
                    while sz > 8 {
                        ret.push(Inst::GlobalSet(index/8, name.clone(), 8, index));
                        index += 8;
                        sz -= 8;
                    }
                    ret.push(Inst::GlobalSet(index/8, name.clone(), sz, index));
                }
            }
        },
        ASTNodeR::FunctionCall(expr, args) => {
            let mut ind = 0;
            for a in args.clone() {
                let sz = a.2.as_ref().unwrap().size(&aliases);
                ret.append(&mut gen_expr(a, ind, offsets, globals, &aliases, false));
                for _ in 0 .. ceil!(sz as f32 / 8.0) {
                    ret.push(Inst::Push(ind, 8));
                    ind += 1;
                }
            }

            let findex = ind;
            ret.append(&mut gen_expr(*expr, findex, offsets, globals, &aliases, false));
            
            for a in args.iter().rev() {
                let sz = a.2.as_ref().unwrap().size(&aliases);
                for _ in 0 .. ceil!(sz as f32 / 8.0) {
                    ind -= 1;
                    ret.push(Inst::Pop(ind, 8));
                }
            }
//            if offsets.contains_key(&name) {
            ret.push(Inst::CallReg(findex/*offsets[&name].0*/));
//            } else {
//                ret.push(Inst::Call(name));
//            }
        },
        ASTNodeR::MemberFunction(tp, lexpr, name, args) => {
            ret.append(&mut gen_expr(lexpr, 0, offsets, globals, &aliases, false));
            let mut ind = 1;
            for a in args.clone() {
                ret.append(&mut gen_expr(a, ind, offsets, globals, &aliases, false));
                ret.push(Inst::Push(ind, 8));
                ind += 1;
            }

            for _ in args.iter().rev() {
                ind -= 1;
                ret.push(Inst::Pop(ind, 8));
            }
            
            ret.push(Inst::Call(tp.to_label() + &name));
        },
        ASTNodeR::If(expr, block, block2) => {
            ret.append(&mut gen_expr(expr, 0, offsets, globals, &aliases, false));
            ret.push(Inst::If(0, index));
            ret.append(&mut gen(block.1, offsets, globals, aliases.clone(), loop_idx, index * 3, false).0);
            ret.push(Inst::Else(index));
            if let Some(a) = block2 {
                ret.append(&mut gen(a.1, offsets, globals, aliases, loop_idx, index * 5, false).0);
            }
            ret.push(Inst::Endif(index));
        },
        ASTNodeR::While(cond, block) => {
            ret.push(Inst::WhileStart(0, index));
            ret.append(&mut gen_expr(cond, 0, offsets, globals, &aliases, false));
            ret.push(Inst::WhileCheck(0, index));
            ret.append(&mut gen(block.1, offsets, globals, aliases, index, index * 3, false).0);
            ret.push(Inst::Endwhile(index));
        },
        ASTNodeR::Break() => {
            ret.push(Inst::Break(loop_idx));
        }
        ASTNodeR::SetField(lexpr, name, rexpr, struct_type, deref) => {
            if let Some(Type::Struct(_, fields)) = struct_type {
                let map_result = &fields.iter().find(|(x, _)| x == &name).unwrap().1;
                let mut off = 0;
                for (key, (tp, _)) in fields.clone() {
                    if key == name {
                        break;
                    }
                    off += tp.size(&aliases);
                }
                ret.append(&mut gen_expr(lexpr, 0, offsets, globals, &aliases, !deref));
                ret.append(&mut gen_expr(rexpr, 1, offsets, globals, &aliases, false));
                ret.push(Inst::SetField(0, 1, off, map_result.0.size(&aliases)));
            } else {
                unreachable!()
            }
        },
        ASTNodeR::FunctionDecl(member_type, name, a, rt, block) => {
            let mut offsets: HashMap<String, (usize, usize)> = HashMap::new();
            let mut offset = 0;
            for arg in a {
                let sz_ = arg.0.size(&aliases);
                let sz = if sz_ == 0 {1} else {sz_};
                offsets.insert(arg.1, (offset + sz, sz));
                offset += sz;
            }

            if let Some(val) = member_type {
                ret.push(Inst::Func(val.dealias(&aliases).to_label() + &name, offsets.clone()));
            } else {
                ret.push(Inst::Func(name, offsets.clone()));
            }

            ret.append(&mut gen(block.1, &mut offsets, globals, aliases, 0, index * 3, false).0);
            if rt == Type::Primitive(PrimitiveType::Void) {
                ret.push(Inst::Ret(0));
            }
            
        },
        ASTNodeR::Return(expr) => {
            ret.append(&mut gen_expr(expr, 0, offsets, globals, &aliases, false));
            ret.push(Inst::Ret(0));
        },
        ASTNodeR::Intrinsic(iname, fname, _, _) => {
            ret.push(Inst::Intrinsic(iname, fname));
        },
        // ignore
        ASTNodeR::TypeAlias(..) => {},
        ASTNodeR::Struct(..) => {},
        ASTNodeR::Include(_, ast, _) => {
            let (mut res, _, mut exts) = gen(ast.1, offsets, globals, aliases, 0, index, is_top_level);
            externs.append(&mut exts);
            ret.append(&mut res);
        }
        ASTNodeR::TypeClass(..) => {},
        ASTNodeR::Instance(..) => {},
        ASTNodeR::DerefSet(l, r) =>{
            ret.append(&mut gen_expr(l, 0, offsets, globals, &aliases, false));
            ret.append(&mut gen_expr(r.clone(), 1, offsets, globals, &aliases, false));
            ret.push(Inst::DerefSet(0, 1, r.2.unwrap().size(&aliases)));
        },
        ASTNodeR::Extern(ename, name, _, _) => {
            externs.push((ename.clone(), name.clone()));
            ret.push(Inst::Extern(ename, name));
        },
    }
    (ret, globals.clone(), externs)
}
