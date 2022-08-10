use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

use crate::error_arrow;

use {std::collections::HashMap, crate::{error, lexer::Lexer, parser::{ASTNodeR, ExpressionR, ASTNode, Expression}, util::{Error, ErrorLevel, PrimitiveType, Type, Op}}};

type Globals = HashMap<String, usize>;
type Aliases = HashMap<String, Type>;
type Vars = Aliases;
type Functions = HashMap<(Option<Type>, String), (Vec<Type>, Type)>;

type ListArgs<'a> = (&'a mut ASTNode, &'a mut Functions, &'a Vars, &'a mut HashMap<String, Vec<(String, Vec<Type>, Type)>>, &'a mut Aliases, &'a mut Globals, &'a mut Vec<(ErrorLevel, String)>, fn() -> HashMap<&'static str, &'static str>);

// check
fn typecheck(largs: ListArgs, f: (Option<Type>, String), is_loop: bool, lexer: &mut Lexer) {
    let (node, functions, vars, generic_functions, aliases, globals, errors, intrinsics) = largs;
    if let ASTNode(_, ASTNodeR::Block(ref mut arr)) = node {
        let vars_sub = &mut vars.clone();
        for mut a in arr {
            match a {
                ASTNode(pos, ASTNodeR::FunctionDecl(tp, func, args, ret, block)) => {
                    if func == "main" {
                        let fargs = args.iter().map(|(x, _)| x.clone()).collect();
                        let fret = ret.to_owned();
                        if ! (fargs == vec![] && fret == Type::Primitive(PrimitiveType::Int)) {
                            let arg = Type::Function(fargs, Box::new(fret));
                            errors.push((ErrorLevel::Err, error!(lexer, *pos,
                                                                 "invalid signature of main function:\n\tfound {arg}, expected fn<() -> int>")
                            ));
                        }
                    }
                    
                    let mut has_vars: bool = false;
                    for arg in args.clone() {
                        if let Type::Custom(t) = arg.0.dealias(aliases) {
                            if t.starts_with("_") {
                                has_vars = true;
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                                continue;
                            }
                        }
                        vars_sub.insert(arg.1, arg.0);
                    }
                    let ac = aliases.clone();

                    if ! has_vars {
                        typecheck((block, functions, vars_sub, generic_functions, aliases, globals, errors, intrinsics), (tp.as_ref().map(|x| x.dealias(&ac)), func.clone()), false, lexer);
                    }
                },
                ASTNode(pos, ASTNodeR::FunctionCall(expr, args)) => {
                    let tp = typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    
                    let func_args;
                    if let Type::Function(ref args, _) = tp {
                        func_args = args;
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "`{expr}` is not a function")));
                        continue;
                    }

                    let len_a = func_args.len();
                    let len_b = args.len();
                    if len_a != len_b {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        continue;
                    };

                    let mut has_vars: bool = false;
                    let mut vars: Vec<Type> = vec![];
                    for (index, a) in args.iter_mut().enumerate() {
                        let typa = typecheck_expr(a, functions, generic_functions, vars_sub, aliases, errors, lexer);
                        let typb = func_args.get(index).unwrap().clone();

                        if let Type::Var(_) = typb {
                            has_vars = true;
                            vars.push(typa.clone());
                        }

                        if typa == Type::Invalid || typb == Type::Invalid {
                            break;
                        }

                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible argument type for function call to `{expr}`:\n\texpected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }

                    has_vars as i32;

                    /*if has_vars {
                        if let Type::Var(name) = tp {
                            
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "")));
                        }
                    }*/
                },
                ASTNode(pos, ASTNodeR::MemberFunction(_, lexpr, name, ref mut vec)) => {
                    let left_type = typecheck_expr(lexpr, functions, generic_functions, vars_sub, aliases, errors, lexer).dealias(aliases);
                    if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                        if left_type != Type::Invalid {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to member function `{name}` from type {left_type}")));
                        }
                        continue;
                    }
                    let func_args = &functions.get(&(Some(left_type.clone()), name.clone())).unwrap().0.clone();

                    let len_a = func_args.len();
                    let len_b = vec.len()+1;
                    if len_a != len_b {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        continue;
                    };

                    for (index, a) in vec.iter_mut().enumerate() {
                        let typb = func_args.get(index+1).unwrap().clone();
                        let typa = typecheck_expr(a, functions, generic_functions, vars_sub, aliases, errors, lexer);

                        if typa == Type::Invalid || typb == Type::Invalid {
                            break;
                        }

                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }

                    a.1 = ASTNodeR::MemberFunction(left_type, lexpr.clone(), name.clone(), vec.clone());
                }
                ASTNode(_, ASTNodeR::Block(..)) => {
                    typecheck((a, functions, vars_sub, generic_functions, aliases, globals, errors, intrinsics), f.clone(), false, lexer);
                },
                ASTNode(pos, ASTNodeR::SetField(ref mut expr_, ref mut name, ref mut rexpr, _)) => {
                    let struct_type = typecheck_expr(expr_, functions, generic_functions, vars_sub, aliases, errors, lexer).dealias(aliases);
                    if let Type::Struct(ref struct_name, ref map) = struct_type {
                        if map.contains_key(name) {
                            let field_type = map.get(name).unwrap().0.clone();
                            let rtype = typecheck_expr(rexpr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                            if !field_type.is_compatible(&rtype, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "imcompatible types: expected `{field_type}`, found `{rtype}`")));
                                continue;
                            }
                            a.1 = ASTNodeR::SetField(expr_.clone(), name.clone(), rexpr.clone(), Some(struct_type));
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefinded field `{name}` for type `{struct_name}`")));
                        }
                    }
                }
                ASTNode(pos, ASTNodeR::If(ref mut expr, ref mut block)) => {
                    let bool_type = typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                    }
                    typecheck((block, functions, vars_sub, generic_functions, aliases, globals, errors, intrinsics), f.clone(), is_loop, lexer);
                },
                ASTNode(pos, ASTNodeR::While(ref mut expr, ref mut block)) => {
                    let bool_type = typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                    }
                    typecheck((block, functions, vars_sub, generic_functions, aliases, globals, errors, intrinsics), f.clone(), true, lexer);
                },
                ASTNode(pos, ASTNodeR::Return(ref mut expr)) => {
                    let tp = typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    let ret_type  = &functions.get(&f.clone()).unwrap().1;
                    if ret_type != &Type::Invalid && ! ret_type.is_compatible(&tp, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible type for return value:\n\texpected `{ret_type}`, found `{tp}`")));
                    }
                },
                ASTNode(pos, ASTNodeR::ArrIndexInit(lexpr, ref mut ind, ref mut expr, _)) => {
                    let type_r = &typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    if type_r == &Type::Invalid {
                        continue;
                    }

                    let type_ind = &typecheck_expr(ind, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    if type_ind == &Type::Invalid {
                        continue;
                    }
                    
                    let type_l = typecheck_expr(lexpr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                    if let Type::Array(_) = type_l {} else if let Type::Custom(ref inner) = type_l {
                        if aliases.contains_key(inner) {
                            if let Type::Array(_) = aliases.get(inner).unwrap() {} else {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "{type_l} used in array indexing is not an array type")));
                            }
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "{type_l} used in array indexing is not an array type")));
                        }
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "{type_l} used in array indexing is not an array type")));
                    }
                    
                    if ! type_l.is_compatible(&Type::Array(Box::new(type_r.clone())), aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{type_l}`, found `{type_r}`")));
                    }
                    
                    if ! Type::Primitive(PrimitiveType::Int).is_compatible(type_ind, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: arrays can only be index by integers (found `{type_ind}`)")));
                    }
                    if let Type::Struct(_, map) = type_r {
                        let mut vector_: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                        vector_.sort();
                        let vec: Vec<usize> = vector_.iter().map(|x| map[x].0.size(aliases)).collect();
                        
                        a.1 = ASTNodeR::ArrIndexInit(lexpr.clone(), ind.clone(), expr.clone(), vec);
                    } else {
                        a.1 = ASTNodeR::ArrIndexInit(lexpr.clone(), ind.clone(), expr.clone(), vec![type_r.size(aliases)]);
                    }
                    
                },
                ASTNode(pos, ASTNodeR::VarInit(var, ref mut expr, _)) => {
                    if ! vars_sub.contains_key(var) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{var}`")));
                    } else {
                        let mut type_r = &typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                        let type_l = vars_sub.get(var).unwrap().dealias(aliases);
                        if type_r == &Type::Invalid {
                            type_r = &type_l;
//                            continue;
                        }
                        if ! type_l.is_compatible(type_r, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{type_l}`, found `{type_r}`")));
                        }
                        a.1 = ASTNodeR::VarInit(var.clone(), expr.clone(), Some(type_l));
                    }
                },
                ASTNode(pos, ASTNodeR::VarOp(var, op, ref mut expr)) => {
                    if ! vars_sub.contains_key(var) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{var}`")));
                    } else {
                        let type_r = &typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);
                        let type_l = vars_sub.get(var).unwrap();
                        if ! type_l.is_compatible(type_r, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{type_l}`, found `{type_r}`")));
                        }

                        if Op::combine_type(&Op::Binary(*op), type_l, type_l, aliases) == Type::Invalid {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{type_l}` and `{type_r}` for operation `{op}`")));
                        }
                    }
                },
                ASTNode(pos, ASTNodeR::VarDec(_, tp, name)) => {
                    if let Type::Custom(t) = tp.dealias(aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                        continue;
                    }
                    vars_sub.insert(name.clone(), tp.clone());
                },
                ASTNode(pos, ASTNodeR::VarDecInit(b, tp, name, expr)) => {
                    if let Type::Custom(t) = tp.dealias(aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                        continue;
                    }
                    let mut type_r = &typecheck_expr(expr, functions, generic_functions, vars_sub, aliases, errors, lexer);

                    if type_r == &Type::Invalid {
                        // error has already been set
                        type_r = tp;
//                        continue;
                    }
                    let typ = tp.dealias(aliases);

                    if ! typ.is_compatible(type_r, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible type for variable `{name}`:\n\texpected `{typ}`, found `{type_r}`")));
                    }

                    vars_sub.insert(name.clone(), typ.clone());
                    a.1 = ASTNodeR::VarDecInit(*b, typ.clone(), name.clone(), expr.clone());
                },
                ASTNode(pos, ASTNodeR::Break()) => {
                    if ! is_loop {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "break outside of loop")));
                    }
                },
                ASTNode(_, ASTNodeR::TypeAlias(_, _)) => {},
                ASTNode(_, ASTNodeR::Struct(_, _)) => {},
                ASTNode(_, ASTNodeR::Include(_, ref mut ast, ref lexer_)) => {
                    let (g, a, f) = match check(ast, lexer_.clone(), intrinsics) {
                        Ok(a) => a,
                        Err(vec) => {
                            for e in vec {
                                errors.push(e);
                            }
                            return;
                        },
                    };
                    for (k, v) in g {
                        globals.insert(k, v);
                    }
                    for (k, v) in a {
                        aliases.insert(k, v);
                    }
                    for (k, v) in f {
                        functions.insert(k, v);
                    }
                }
                ASTNode(pos, ASTNodeR::Intrinsic(iname, ..)) => {
                    if ! intrinsics().contains_key(iname.as_str()) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to compiler intrinsic `{iname}`")));
                    }
                },
            }
        };
    }
}

fn check_function_ret_paths(ast: &ASTNodeR, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) {
    match ast {
        ASTNodeR::Block(ref vec) => {
            for a in vec {
                if let ASTNodeR::FunctionDecl(_, _, _, rt, block) = &a.1 {
                    if *rt == Type::Primitive(PrimitiveType::Void) {
                        continue;
                    }
                    let mut rs: bool = false;
                    if let ASTNodeR::Block(blk_vec) = &block.1 {
                        for statement in blk_vec {
                            match &statement.1 {
                                ASTNodeR::If(_, _sub) => {
                                    // TODO: implement here, if else is implemented
                                },
                                ASTNodeR::Return(_) => {
                                    rs = true;
                                },
                                _ => {}
                            }
                        }
                    }
                    if ! rs {
                        errors.push((ErrorLevel::Err, error!(lexer, a.0, "not all paths lead to a return statement")));
                    }
                }
            }
        },
        _ => unreachable!()
    }
}



fn typecheck_expr(expr: &mut Expression, functions: &mut Functions, generic_functions: &mut HashMap<String, Vec<(String, Vec<Type>, Type)>>, vars: &HashMap<String, Type>, aliases: &Aliases, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
    
    let tp = | | -> Type {
        let exp = expr.clone();
        let pos = expr.0;
        match &mut expr.1 {
            ExpressionR::T(left_, op, right_, _) => {
                let left = typecheck_expr(left_, functions, generic_functions, vars, aliases,  errors, lexer);
                let right = typecheck_expr(right_, functions, generic_functions, vars, aliases,  errors, lexer);

                match op.combine_type(&left, &right, aliases) {
                    Type::Invalid => {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `{left}` and `{right}` for operation `{op}`")));
                        Type::Invalid
                    },
                    a => a,
                }
            },
            ExpressionR::Ref(expr) => {
                let tp = typecheck_expr(expr, functions, generic_functions, vars, aliases, errors, lexer);
                Type::Pointer(Box::new(tp))
            },
            ExpressionR::Deref(expr) => {
                let tp = typecheck_expr(expr, functions, generic_functions, vars, aliases, errors, lexer);
                if let Type::Pointer(inner) = tp {
                    *inner
                } else {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "type `{tp}` cannot be dereferenced")));
                    Type::Invalid
                }
            },
            ExpressionR::StructLiteral(name, fields) => {
                let mut fields_: HashMap<String, (Type, usize)> = HashMap::new();
                for (name_, expr) in fields {
                    let tp = typecheck_expr(expr, functions, generic_functions, vars, aliases, errors, lexer);
                    fields_.insert(name_.clone(), (tp.clone(), fields_.len()));
                }
                Type::Struct(name.clone(), fields_)
            },
            ExpressionR::StructField(ref mut expr_, ref mut name, _) => {
                let struct_type = typecheck_expr(expr_, functions, generic_functions, vars, aliases, errors, lexer).dealias(aliases);
                if let Type::Struct(ref struct_name, ref map) = struct_type {
                    if map.contains_key(name) {
                        let ret = map.get(name).unwrap().0.clone();
                        expr.1 = ExpressionR::StructField(expr_.clone(), name.clone(), Some(struct_type.clone()));
                        return ret;
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "undefinded field `{name}` for type `{struct_name}`")));
                    }
                } else {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "type `{struct_type}` is not a struct.")));
                }
                Type::Invalid
            }
            ExpressionR::UnaryOp(op, ref mut left, _) => {
                let left = typecheck_expr(left, functions, generic_functions, vars, aliases, errors, lexer);
                
                match op.with_type(&left, aliases) {
                    Type::Invalid => {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible type `{left}` for operation `{op}`")));
                        Type::Invalid
                    },
                    a => a,
                }
            },
            ExpressionR::Val(tp, _) => {
                tp.clone()
            },
            ExpressionR::Var(name) => {
                if vars.contains_key(name) {
                    vars[name].clone()
                } else if functions.contains_key(&(None, name.clone())) {
                    let f = functions[&(None, name.clone())].clone();
                    Type::Function(f.0, Box::new(f.1))
                } else {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to variable `{name}`")));
                    Type::Invalid
                }
            },
            ExpressionR::F(lexpr, vec) => {
                let tp = typecheck_expr(lexpr, functions, generic_functions, vars, aliases, errors, lexer);

                let func_args;
                let ret_type;
                if let Type::Function(ref args, ref ret) = tp {
                    ret_type = *ret.clone();
                    func_args = args;
                } else {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "`{lexpr}` is not a function")));
                    return Type::Invalid;
                }

                let len_a = func_args.len();
                let len_b = vec.len();
                if len_a > len_b {
                    let start = "missing argument in function call: ";
                    let offset = start.chars().count() + exp.to_string().chars().count() - 1;
                    let tp = &func_args[len_b];
                    let msg = format!("    expected argument of type {tp}");
                    let line = error_arrow!(lexer, pos, offset, msg, 1);

                    errors.push((ErrorLevel::Err, error!(lexer, pos, "{start}{exp}\n{line}")));
                    return ret_type.clone();
                } else if len_a < len_b {
                    let start = "unexpected argument(s) in function call: ";
                    let mut expr_off = 0;
                    for (i, a) in vec.iter().enumerate() {
                        expr_off += a.to_string().chars().count();
                        if i == len_a-1 {
                            break;
                        } else {
                            expr_off += 2;
                        }
                    }
                    let offset = start.chars().count() + lexpr.to_string().chars().count() + 1 + expr_off;
                    let msg = format!("    expected ')', found argument(s)");
                    let line = error_arrow!(lexer, pos, offset, msg, exp.to_string().chars().count() - expr_off - lexpr.to_string().chars().count() - 2);

                    errors.push((ErrorLevel::Err, error!(lexer, pos, "{start}{exp}\n{line}")));
                    return ret_type.clone();
                };

                let orig_vec = vec.clone();

                let mut current_vars: HashMap<String, Type> = HashMap::new();

                let mut new_args: Vec<Type> = vec![];
                for (index, a) in vec.iter_mut().enumerate() {
                    let typa = typecheck_expr(a, functions, generic_functions, vars, aliases, errors, lexer);
                    let mut typb = func_args.get(index).unwrap().clone();

                    if let Type::Var(ref name) = typb {
                        // check for vars
                        if current_vars.contains_key(name) {
                            typb = current_vars[name].clone();
                        } else {
                            if let Type::Var(_) = typa {
                                errors.push((ErrorLevel::Err, error!(lexer, pos, "could not infer type of argument `{a}`")));
                                return ret_type.clone();
                            }
                            current_vars.insert(name.clone(), typa.clone());
                            typb = typa.clone();
                        }
                    }
                    new_args.push(typb.clone());
                    
                    if ! typa.is_compatible(&typb, aliases) {
                        let mut args_pos = 0;
                        for (ind, arg) in orig_vec.iter().enumerate() {
                            if ind == index {
                                break;
                            }
                            args_pos += arg.to_string().chars().count() + 2;
                        }
                        
                        let first = "incompatible types in function call: ";
                        let offset = first.chars().count() + 1 + lexpr.to_string().chars().count() + 1 + args_pos;
                        let msg = format!("    expected `{typb}`, found `{typa}`");
                        let line = error_arrow!(lexer, pos, offset, msg, a.to_string().chars().count());

                        errors.push((ErrorLevel::Err, error!(lexer, pos, "{first}`{exp}`:\n{line}")));
                        break;
                    }
                }

                let ret = if let Type::Var(ref name) = ret_type.clone() {
                    if current_vars.contains_key(name) {
                        current_vars[name].clone()
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "could not infer type of return value")));
                        Type::Invalid
                    }
                } else {
                    ret_type.clone()
                };

                let mut has_var: bool = false;
                let mut new_func_name: String = String::new();
                for tp in func_args.iter() {
                    if let Type::Var(name) = tp {
                        has_var = true;
                        let mut hasher = DefaultHasher::new();
                        current_vars[name].hash(&mut hasher);
                        let hash = hasher.finish();
                        
                        new_func_name.push_str(hash.to_string().as_str());
                        new_func_name.push('_');
                    }
                }
                if let Type::Var(name) = ret_type {
                    has_var = true;
                    let mut hasher = DefaultHasher::new();
                    current_vars[&name].hash(&mut hasher);
                    let hash = hasher.finish();
                    
                    new_func_name.push_str(hash.to_string().as_str());
                    new_func_name.push_str("_");
                }

                if has_var {
                    let func_name: String;
                    if let ExpressionR::Var(ref name) =  lexpr.1 {
                        func_name = name.clone();
                        new_func_name.push_str(name);
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "expression `{lexpr}`, should have a fixed, non-generic type.")));
                        errors.push((ErrorLevel::Note, error!(lexer, pos, "This seems to be a compiler bug, because the type-checker should handle it for you...")));
                        errors.push((ErrorLevel::Note, error!(lexer, pos, "The behaviour may change (or disappear completely) in future versions of the compiler")));
                        return ret;
                    }
                    functions.insert((None, new_func_name.clone()), (new_args.clone(), ret.clone()));
                    if ! generic_functions.contains_key(&func_name) {
                        generic_functions.insert(func_name.clone(), vec![]);
                    }
                    generic_functions.get_mut(&func_name).unwrap().push((new_func_name.clone(), new_args.clone(), ret.clone()));

                    expr.1 = ExpressionR::F(Box::new(Expression(pos,
                                                                ExpressionR::Var(new_func_name),
                                                                Some(Type::Function(new_args, Box::new(ret.clone()))))),
                                            orig_vec);
                }
                ret
            },
            ExpressionR::MemberFunction(_, lexpr, name, ref mut vec) => {
                let left_type = typecheck_expr(lexpr, functions, generic_functions, vars, aliases, errors, lexer).dealias(aliases);
                if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to member function `{name}` from type {left_type}")));
                    if let Type::Struct(_, fields) = left_type {
                        if fields.contains_key(name) {
                            if let Type::Function(..) = fields[name].0 {
                                errors.push((ErrorLevel::Note, error!(lexer, pos, "if you wanted to call the function stored in a struct, write it like this:\n\t({lexpr}.{name})(args..)")));
                            }
                        }
                    }
                    return Type::Invalid;
                }
                
                let func_args = &functions.get(&(Some(left_type.clone()), name.clone())).unwrap().0.clone();

                let len_a = func_args.len();
                let len_b = vec.len()+1;
                if len_a != len_b {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "expected `{len_a}` arguments, got `{len_b}`")));
                    return Type::Invalid;
                };

                
                for (index, a) in vec.iter_mut().enumerate() {
                    let typa = typecheck_expr(a, functions, generic_functions, vars, aliases, errors, lexer);
                    let typb = func_args.get(index+1).unwrap().clone();
                    
                    if ! typa.is_compatible(&typb, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                        break;
                    }
                }

                let n = name.clone();
                expr.1 = ExpressionR::MemberFunction(left_type.clone(), lexpr.clone(), name.clone(), vec.clone());
                
                functions.get(&(Some(left_type.clone()), n)).unwrap().1.clone()
            },
            ExpressionR::Arr(ref mut vec) => {
                let mut last_tp = Type::Invalid;
                for a in vec {
                    let tp = typecheck_expr(a, functions, generic_functions, vars, aliases, errors, lexer);
                    if last_tp == Type::Invalid {
                        last_tp = tp.clone();
                    }
                    if ! last_tp.is_compatible(&tp, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `{last_tp}` and `{tp}` in array literal")));
                        last_tp = Type::Invalid;
                        break;
                    }
                    last_tp = tp;
                }
                Type::Array(Box::new(last_tp))
            },
            ExpressionR::Undef => {
                Type::Primitive(PrimitiveType::Void)
            },
            ExpressionR::ArrAlloc(tp, _) => {
                Type::Array(Box::new(tp.clone()))
            },
            ExpressionR::Index(ref mut ident, ref mut a, _) => {

                let inner_type = typecheck_expr(a, functions, generic_functions, vars, aliases, errors, lexer);
                if ! Type::Primitive(PrimitiveType::Int).is_compatible(&inner_type, aliases) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `int` and `{inner_type}` in array length definition")));
                    Type::Invalid
                } else {
                    let ident_type = typecheck_expr(ident, functions, generic_functions, vars, aliases, errors, lexer);
                    let inner_tp = if let Type::Array(inner) = ident_type {
                        *inner
                    } else if let Type::Custom(ref inner) = ident_type {
                        if aliases.contains_key(inner) {
                            if let Type::Array(ref inner2) = aliases[inner] {
                                *inner2.clone()
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, pos, "`{ident_type}` is not an array type")));
                                Type::Invalid
                            }
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "`{ident_type}` is not an array type")));
                            Type::Invalid
                        }
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "`{ident_type}` is not an array type")));
                        Type::Invalid
                    };
                    if let Type::Struct(_, ref map) = inner_tp.dealias(aliases) {
                        let mut vector_: Vec<String> = map.iter().map(|(x, _)| x.clone()).collect();
                        vector_.sort();
                        let vec: Vec<usize> = vector_.iter().map(|x| map[x].0.size(aliases)).collect();
                        expr.1 = ExpressionR::Index(ident.clone(), a.clone(), vec);
                    } else {
                        expr.1 = ExpressionR::Index(ident.clone(), a.clone(), vec![inner_tp.size(aliases)]);
                    }
                    inner_tp
                }
            }
        }}();
    let typ = tp;//expr, functions, vars, aliases, errors, lexer);
    expr.2 = Some(typ.clone());
    typ
}


pub fn check(ast: &mut ASTNode, mut lexer: Lexer, intrinsics: fn() -> HashMap<&'static str, &'static str>) -> Result<(Globals, Aliases, Functions), Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    
    // collect all aliases + functions
    let mut type_aliases: Aliases = HashMap::new();
    let mut functions: Functions = HashMap::new();
    let mut vars: Vars = HashMap::new();
    let mut globals: Globals = HashMap::new();

    if let ASTNode(_, ASTNodeR::Block(arr)) = ast {
        for a in arr.clone() {
            match a {
                ASTNode(_, ASTNodeR::TypeAlias(alias, typ)) => {
                    type_aliases.insert(alias, typ);
                },
                ASTNode(_, ASTNodeR::Struct(name, fields)) => {
                    type_aliases.insert(name.clone(), Type::Struct(name, fields));
                },
                ASTNode(_, ASTNodeR::Intrinsic(_, fname, args, rt)) => {
                    functions.insert((None, fname), (args.into_iter().enumerate().map(|(_, (a, _))| a).collect(), rt));
                }
                _ => {}
            }
        }
        for a in arr.clone() {
            match a {
                ASTNode(_, ASTNodeR::VarDec(_, tp, name)) => {
                    vars.insert(name.clone(), tp.clone());
                    globals.insert(name.clone(), tp.size(&type_aliases));
                },
                ASTNode(pos, ASTNodeR::FunctionDecl(tp, name, args, ret_type, _)) => {
                    let left_type = tp.clone().map(|x| x.dealias(&type_aliases));

                    if let Some(lt) = left_type.clone() {
                        let first_type = args[0].clone().0;
                        if ! lt.is_compatible(&first_type, &type_aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: first parameter of member function declaration has to be the same type as the type used on. (got `{first_type}`, expected `{lt}`)")));
                        }
                    }

                    if args.iter().any(|(x, _)| if let Type::Custom(a) = x {a.starts_with('_')} else {false}) {
                        println!("here: {name}");
                    }
                    
                    functions.insert((left_type, name.clone()), (args.into_iter().map(|(a, _)| a.clone()).collect(), ret_type.clone()));
                },
                ASTNode(_, ASTNodeR::VarDecInit(_, tp, name, _)) => {
                    vars.insert(name.clone(), tp.dealias(&type_aliases).clone());
                    globals.insert(name.clone(), tp.size(&type_aliases));
                },
                _ => {}
            }
        }
    } else {
        unreachable!();
    }

    let mut generic_functions = HashMap::new();
    typecheck((ast, &mut functions, &vars, &mut generic_functions, &mut type_aliases, &mut globals, &mut errors, intrinsics), (None, "".into()), false, &mut lexer);
    check_function_ret_paths(&ast.1, &mut errors, &mut lexer);

    let root_arr;
    if let ASTNode(_, ASTNodeR::Block(arr)) = ast {
        root_arr = arr;
    } else {
        unreachable!();
    }
    
    for (func, vec) in generic_functions.clone() {
        let mut pos = 0;
        let mut block = Box::new(ASTNode(pos, ASTNodeR::Block(vec![])));
        let mut args = vec![];
        let mut ret = Type::Invalid;
        
        // remove original function declaration from AST
        for (i, a) in root_arr.clone().iter().enumerate() {
            if let ASTNode(pos_, ASTNodeR::FunctionDecl(_, name, args_, ret_, block_)) = a {
                if *name == func {
                    pos = *pos_;
                    block = block_.clone();
                    args = args_.clone();
                    ret = ret_.clone();
                    root_arr.remove(i);
                    break;
                }
            }
        }

        // add generic function declarations to AST
        for a in vec {
            let new_block = block.clone();
            let mut new_aliases = type_aliases.clone();
            
            // add generic function arguments to new_aliases
            for (i, (v, _)) in args.iter().enumerate() {
                if let Type::Var(nm) = v {
                    new_aliases.insert(nm.to_string(), a.1[i].clone());
                }
            }

            if let Type::Var(nm) = ret.clone() {
                new_aliases.insert(nm.to_string(), a.2.clone());
            }

            
            let mut node = ASTNode(pos, ASTNodeR::Block(vec![ASTNode(pos, ASTNodeR::FunctionDecl(None, a.0.clone(), args.iter().enumerate().map(|(i, v)| (a.1[i].clone(), v.1.clone())).collect(), a.2, new_block))]));

            typecheck((&mut  node, &mut functions, &vars, &mut generic_functions, &mut new_aliases, &mut globals, &mut errors, intrinsics), (None, "".into()), false, &mut lexer);

            let new_func;
            if let ASTNode(_, ASTNodeR::Block(here)) = node {
                new_func = here[0].clone();
            } else {
                unreachable!()
            }
            
            root_arr.push(new_func);

            
        }
    }

    // type checking
    if errors.is_empty() {
        Ok((globals, type_aliases, functions))
    } else {
        Err(errors)
    }
}
