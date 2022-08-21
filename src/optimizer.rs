use std::collections::HashMap;

use crate::{parser::{ASTNode, ASTNodeR, Expression, ExpressionR}, util::{BinaryOp, PrimitiveType, Type, Op}};

fn optimize_expr(expr: &mut Expression, aliases: &HashMap<String, Type>, const_vars: HashMap<String, (Type, String)>) {
    match &mut expr.1 {
        ExpressionR::T(ea, op_, eb, _) => {            
            optimize_expr(ea, aliases, const_vars.clone());
            optimize_expr(eb, aliases, const_vars.clone());
            if let ExpressionR::Val(ta, va) = &ea.1 {
                if let ExpressionR::Val(tb, vb) = &eb.1 {
                    if let crate::util::Op::Binary(op) = op_ {
                        match op {
                            BinaryOp::Add => {
                                if ta.is_numeric() && tb.is_numeric() {
                                    if ta.is_integral() && tb.is_integral() {
                                        let c = va.parse::<u64>().unwrap() + vb.parse::<u64>().unwrap();
                                        expr.1 = ExpressionR::Val(Op::combine_type(op_, ta, tb, aliases), c.to_string());
                                    }
                                }
                            },
                            BinaryOp::Sub => {
                                if ta.is_numeric() && tb.is_numeric() {
                                    if ta.is_integral() && tb.is_integral() {
                                        let c = va.parse::<u64>().unwrap() - vb.parse::<u64>().unwrap();
                                        expr.1 = ExpressionR::Val(Op::combine_type(op_, ta, tb, aliases), c.to_string());
                                    }
                                }
                            },
                            BinaryOp::Mul => {
                                if ta.is_numeric() && tb.is_numeric() {
                                    if ta.is_integral() && tb.is_integral() {
                                        let c = va.parse::<u64>().unwrap() * vb.parse::<u64>().unwrap();
                                        expr.1 = ExpressionR::Val(Op::combine_type(op_, ta, tb, aliases), c.to_string());
                                    }
                                }
                            },
                            BinaryOp::Div => {
                                if ta.is_numeric() && tb.is_numeric() {
                                    if ta.is_integral() && tb.is_integral() {
                                        let c = va.parse::<u64>().unwrap() / vb.parse::<u64>().unwrap();
                                        expr.1 = ExpressionR::Val(Op::combine_type(op_, ta, tb, aliases), c.to_string());
                                    }
                                }
                            },
                            BinaryOp::Mod => {
                                if ta.is_numeric() && tb.is_numeric() {
                                    if ta.is_integral() && tb.is_integral() {
                                        let c = va.parse::<u64>().unwrap() % vb.parse::<u64>().unwrap();
                                        expr.1 = ExpressionR::Val(Op::combine_type(op_, ta, tb, aliases), c.to_string());
                                    }
                                }
                            },
                            BinaryOp::Eq => {
                                if ta == tb && va == vb {
                                    expr.1 = ExpressionR::Val(Type::Primitive(PrimitiveType::Bool), "true".to_string());
                                }
                            },
                            BinaryOp::BoolAnd => todo!(),
                            BinaryOp::BoolOr => todo!(),
                            BinaryOp::Less => todo!(),
                            BinaryOp::LessEq => todo!(),
                            BinaryOp::Greater => todo!(),
                            BinaryOp::GreaterEq => todo!(),
                            BinaryOp::Neq => todo!(),
                        };
                    }
                }
            }
        },
        ExpressionR::Val(_, _) => {},
        ExpressionR::Var(nm) => {
            if const_vars.contains_key(nm) {
                let var = const_vars[nm].clone();
                expr.1 = ExpressionR::Val(var.0, var.1);
            }
        },
        ExpressionR::F(e1, args) => {
            optimize_expr(e1, aliases, const_vars.clone());
            for e in args {
                optimize_expr(e, aliases, const_vars.clone());
            }

//            if let ExpressionR::Var(name) = e1 {
//                
//            }
            
            // TODO: check for constant / empty function
        },
        ExpressionR::Arr(_) => {},
        ExpressionR::Undef => {},
        ExpressionR::ArrAlloc(_, _) => {},
        ExpressionR::Index(e1, e2, _) => {
            optimize_expr(e1, aliases, const_vars.clone());
            optimize_expr(e2, aliases, const_vars.clone());
        },
        ExpressionR::UnaryOp(_, _, _) => todo!(),
        ExpressionR::StructLiteral(_, _) => {},
        ExpressionR::StructField(e1, _, _) => {
            optimize_expr(e1, aliases, const_vars.clone());
        },
        ExpressionR::Ref(_) => {},
        ExpressionR::Deref(_) => todo!(),
        ExpressionR::MemberFunction(_, e1, _, args) => {
            optimize_expr(e1, aliases, const_vars.clone());
            for e in args {
                optimize_expr(e, aliases, const_vars.clone());
            }
            // TODO: check for constant / empty function
        },
    }
}

fn optimize_block(vec: &mut Vec<ASTNode>, aliases: &HashMap<String, Type>, mut const_vars: HashMap<String, (Type, String)>) {
    let mut to_remove = vec![];
    for (i, a) in vec.iter_mut().enumerate() {
        match &mut a.1 {
            ASTNodeR::Block(inner_vec) => optimize_block(inner_vec, aliases, const_vars.clone()),
            ASTNodeR::VarInit(v, expr, _) | ASTNodeR::VarDecInit(_, _, v, expr) => {
                optimize_expr(expr, aliases, const_vars.clone());
                if let ExpressionR::Val(tp, val) = &expr.1 {
                    const_vars.insert(v.to_owned(), (tp.to_owned(), val.to_owned()));
                } else {
                    const_vars.remove(v);
                }
            },
            ASTNodeR::VarOp(_, op, expr) => {
                optimize_expr(expr, aliases, const_vars.clone());

                if let ExpressionR::Val(tp, val) = &expr.1 {
                    match op {
                        BinaryOp::Add | BinaryOp::Sub => {
                            if tp.is_numeric() && val.parse::<f64>() == Ok(0.0) {
                                to_remove.push(i);
                            }
                        },
                        BinaryOp::Mul |BinaryOp::Div | BinaryOp::Mod => {
                            if tp.is_numeric() && val.parse::<f64>() == Ok(1.0) {
                                to_remove.push(i);
                            }
                        },
                        BinaryOp::BoolAnd => {
                            if *tp == Type::Primitive(PrimitiveType::Bool) && val == "true" {
                                to_remove.push(i);
                            }
                        },
                        BinaryOp::BoolOr => {
                            if *tp == Type::Primitive(PrimitiveType::Bool) && val == "false" {
                                to_remove.push(i);
                            }
                        },
                        _ => {},
                    }
                }
            },
            ASTNodeR::FunctionCall(f, args) => {
                optimize_expr(f.as_mut(), aliases, const_vars.clone());
                for a in args {
                    optimize_expr(a, aliases, const_vars.clone());
                }
                // TODO: check if `f` is guaranteed to be pure
            },
            ASTNodeR::If(exp, _, _) => {
                optimize_expr(exp, aliases, const_vars.clone());
                let b = Type::Primitive(PrimitiveType::Bool);
                if ExpressionR::Val(b.clone(), "true".into()) == exp.1 {
                    println!("Optimizer: TODO: ALWAYS TRUE IF!");
                } else if ExpressionR::Val(b, "false".into()) == exp.1 {
                    println!("Optimizer: TODO: ALWAYS FALSE IF!");
                }
            },
//            ASTNodeR::FunctionDecl(_, _, _, _, _) => {},
//            ASTNodeR::Intrinsic(_, _, _, _) => todo!(),
            ASTNodeR::ArrIndexInit(a, b, c, _) => {
                optimize_expr(a, aliases, const_vars.clone());
                optimize_expr(b, aliases, const_vars.clone());
                optimize_expr(c, aliases, const_vars.clone());
            },
            ASTNodeR::While(expr, _block) => {
                let b = Type::Primitive(PrimitiveType::Bool);
                if ExpressionR::Val(b.clone(), "true".into()) == expr.1 {
                    println!("Optimizer: TODO: CONSTANT TRUE LOOP");
                    // TODO: remove WhileCheck -> replace with direct jump in IR
                } else if ExpressionR::Val(b, "false".into()) == expr.1 {
                    println!("Optimizer: TODO: CONSTANT FALSE LOOP");
                }
            },
            ASTNodeR::SetField(e1, _, e2, _) => {
                optimize_expr(e1, aliases, const_vars.clone());
                optimize_expr(e2, aliases, const_vars.clone());
            },
            ASTNodeR::MemberFunction(_, exp, _, args) => {
                optimize_expr(exp, aliases, const_vars.clone());
                for a in args {
                    optimize_expr(a, aliases, const_vars.clone());
                }
            },
            ASTNodeR::Instance(_, _, fs) => {
                optimize_block(fs, aliases, const_vars.clone());
            },
            ASTNodeR::Return(expr) => {
                optimize_expr(expr, aliases, const_vars.clone());
            },
            ASTNodeR::FunctionDecl(_, _, _, _, block) => {
                // TODO: some checks
                optimize(block, aliases);
            },
            _ => {}
        }
    }

    for a in to_remove {
        vec.remove(a);
    }
}

fn optimize_(ast: &mut ASTNode, aliases: &HashMap<String, Type>, const_vars: HashMap<String, (Type, String)>) {
    if let ASTNode(_, ASTNodeR::Block(vec)) = ast {
        optimize_block(vec, aliases, const_vars);
    } else {
        unreachable!()
    }
}

pub fn optimize(ast: &mut ASTNode, aliases: &HashMap<String, Type>) {
    optimize_(ast, aliases, HashMap::new());
}