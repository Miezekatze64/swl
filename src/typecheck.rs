use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::{DefaultHasher, Entry};

use crate::error_arrow;

use {std::collections::HashMap, crate::{error, lexer::Lexer, parser::{ASTNodeR, ExpressionR, ASTNode, Expression}, util::{Error, ErrorLevel, PrimitiveType, Type, Op}}};

type Globals     = HashMap<String, usize>;
type Aliases     = HashMap<String, Type>;
type TypeClasses = HashMap<String, (String, Functions)>;
type Instances   = HashMap<(String, Type), Vec<ASTNode>>;
type Vars        = Aliases;
type Functions   = HashMap<(Option<Type>, String), (Vec<Type>, Type)>;

type ListArgs<'a> = (&'a mut ASTNode, &'a mut Functions, &'a Vars, &'a mut HashMap<String, Vec<(String, Vec<Type>, Type, Vars)>>, &'a mut TypeClasses, &'a mut Instances, &'a mut Aliases, &'a mut Globals, &'a mut Vec<(ErrorLevel, String)>, fn() -> HashMap<&'static str, &'static str>);

fn hash_vars(tp: Type, current_vars: HashMap<String, Type>, new_func_name: &mut String) {
    match tp {
        Type::Primitive(_)                => {},
        Type::Custom(s)                   => todo!("Custom: {s}"),
        Type::Array(ref a)                => hash_vars(*a.clone(), current_vars, new_func_name),
        Type::Pointer(ref a)              => hash_vars(*a.clone(), current_vars, new_func_name),
        Type::Function(ref args, ref ret) => {
            for arg in args {
                hash_vars(arg.clone(), current_vars.clone(), new_func_name);
            }
            hash_vars(*ret.clone(), current_vars, new_func_name);
        },
        Type::Invalid                     => {},
        Type::Struct(_, _)                => todo!(),
        Type::Var(ref a)                  => {
            let mut hasher = DefaultHasher::new();
            current_vars[a].hash(&mut hasher);
            let hash = hasher.finish();
            
            new_func_name.push_str(hash.to_string().as_str());
            new_func_name.push('_');
        },
        Type::Bounded(ref a, _) => {
            let mut hasher = DefaultHasher::new();
            current_vars[a].hash(&mut hasher);
            let hash = hasher.finish();

            new_func_name.push_str(hash.to_string().as_str());
            new_func_name.push('_');
        },
    }
}

// check
fn typecheck(largs: ListArgs, f: (Option<Type>, String), is_loop: bool, lexer: &mut Lexer, verbose: usize) {
    let (node, functions, vars, generic_functions, type_classes, instances, aliases, globals, errors, intrinsics) = largs;
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

                    let mut vars = false;
                    for arg in args.clone() {
                        if let Type::Custom(t) = arg.0.dealias(aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                            continue;
                        }
                        vars |= has_vars(&arg.0);
                        let arg0 = if let Type::Bounded(n,  _) = arg.0 {
                            Type::Var(n)
                        } else {
                            arg.0
                        };
                        vars_sub.insert(arg.1, arg0);
                    }
                    let ac = aliases.clone();

                    vars |= has_vars(ret);

                    if ! vars {
                        typecheck((block, functions, vars_sub, generic_functions, type_classes, instances, aliases, globals, errors, intrinsics), (tp.as_ref().map(|x| x.dealias(&ac)), func.clone()), false, lexer, verbose);
                    }

                },
                ASTNode(pos, ASTNodeR::FunctionCall(expr, args)) => {
                    let tp = typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    
                    let func_args;
                    let mut ret_type;
                    if let Type::Function(ref args, ref ret) = tp {
                        func_args = args;
                        ret_type = *ret.clone();
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

                    let orig_vec = args.clone();
                    let mut current_vars: HashMap<String, Type> = HashMap::new();
                    let mut new_args: Vec<Type> = vec![];
                    let mut type_class: Option<String> = None;
                    let mut type_class_arg: Option<Type> = None;
                    
                    for (index, a) in args.iter_mut().enumerate() {
                        let typa = typecheck_expr(a, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer).dealias(aliases);
                        let mut typb = func_args.get(index).unwrap().clone();

                        // check for type class
                        if let Type::Bounded(name, ref class) = typb.clone() {
                            if type_class.is_none() {
                                type_class = Some(class.clone());
                                type_class_arg = Some(typa.clone());

                                if ! type_classes.contains_key(class) {
                                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined typeclass `{class}`")));
                                    return;
                                }

                                if !instances.contains_key(&(class.clone(), typa.clone())) {
                                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "type `{typa}` is not a member of `{class}` typeclass")));
                                    return;
                                }
                                
                                typb = typa.clone();
                                current_vars.insert(name, typa.clone());
                            } else if type_class_arg.as_ref().unwrap() != &typb {
                                let mut args_pos = 0;
                                for (ind, arg) in orig_vec.iter().enumerate() {
                                    if ind == index {
                                        break;
                                    }
                                    args_pos += arg.to_string().chars().count() + 2;
                                }
                                
                                let first = "incompatible types in function call: ";
                                let offset = first.chars().count() + 1 + args_pos;
                                let msg = format!("    expected `{typb}`, found `{typa}`");
                                let line = error_arrow!(lexer, *pos, offset, msg, a.to_string().chars().count());
                                
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "{first}`{expr}`:\n{line}")));
                                break;
                            }
                        }
                        
                        if typb == typb.dealias(aliases) {
                            let mut new_errs = infer_types(typa.clone().dealias(aliases), &mut typb, &mut current_vars, lexer, *pos, a.clone(), aliases);
                            if ! new_errs.is_empty() {
                                errors.append(&mut new_errs);
                                return;
                            }
                            
                            new_args.push(typb.clone());
                        }

                        if typa == Type::Invalid || typb == Type::Invalid {
                            break;
                        }

                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible argument type for function call to `{expr}`:\n\texpected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }

                    let mut new_func_name: String = String::new();
                    for tp in func_args.iter() {
                        hash_vars(tp.dealias(aliases), current_vars.clone(), &mut new_func_name);
                    }

                    let has_vars = ! new_func_name.is_empty();

                    if has_vars {
                        let func_name: String;
                        if let ExpressionR::Var(ref name) =  expr.1 {
                            func_name = name.clone();
                            new_func_name.push_str(name);
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "expression `{expr}`, should have a fixed, non-generic type.")));
                            errors.push((ErrorLevel::Note, error!(lexer, *pos, "This seems to be a compiler bug, because the type-checker should handle it for you...")));
                            errors.push((ErrorLevel::Note, error!(lexer, *pos, "The behaviour may change (or disappear completely) in future versions of the compiler")));
                            return;
                        }

                        if let Type::Var(ref name) | Type::Bounded(ref name, _) = ret_type {
                            // check for vars
                            if current_vars.contains_key(name) {
                                ret_type = current_vars[name].clone();
                            } else {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "could not infer return type")));
                                return;
                            }
                        }

                        if let Entry::Vacant(e) = functions.entry((None, new_func_name.clone())) {
                            e.insert((new_args.clone(), ret_type.clone()));
                            if ! generic_functions.contains_key(&func_name) {
                                generic_functions.insert(func_name.clone(), vec![]);
                            }
                            generic_functions.get_mut(&func_name).unwrap().push(
                                (new_func_name.clone(), new_args.clone(),
                                 ret_type.clone(), current_vars.clone()));
                        }
                        
                        a.1 = ASTNodeR::FunctionCall(Box::new(Expression(*pos,
                                                                    ExpressionR::Var(new_func_name),
                                                                    Some(Type::Function(new_args, Box::new(ret_type))))),
                                                args.to_vec());
                    }
                    
                },
                ASTNode(pos, ASTNodeR::MemberFunction(_, lexpr, name, ref mut vec)) => {
                    let left_type = typecheck_expr(lexpr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer).dealias(aliases);
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
                        let typa = typecheck_expr(a, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);

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
                    typecheck((a, functions, vars_sub, generic_functions, type_classes, instances, aliases, globals, errors, intrinsics), f.clone(), false, lexer, verbose);
                },
                ASTNode(pos, ASTNodeR::SetField(ref mut expr_, ref mut name, ref mut rexpr, _, deref)) => {
                    let struct_type_ = typecheck_expr(expr_, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer).dealias(aliases);

                    let struct_type = if *deref {
                        if let Type::Pointer(stu) = struct_type_ {
                            *stu
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "type `{struct_type_}` is not a pointer, but pointer type was expected")));
                            continue;
                        }
                    } else {
                        struct_type_
                    };
                    
                    if let Type::Struct(ref struct_name, ref map) = struct_type {
                        if map.iter().any(|(n, _)| n == name) {
                            let field_type = map.iter().find(|(n, _)| n == name).unwrap().1.0.clone();
                            let rtype = typecheck_expr(rexpr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                            if !field_type.is_compatible(&rtype, aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "imcompatible types: expected `{field_type}`, found `{rtype}`")));
                                continue;
                            }
                            a.1 = ASTNodeR::SetField(expr_.clone(), name.clone(), rexpr.clone(), Some(struct_type), *deref);
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefinded field `{name}` for type `{struct_name}`")));
                        }
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "type `{struct_type}` is not a struct, but struct type was expected")));
                    }
                }
                ASTNode(pos, ASTNodeR::If(ref mut expr, ref mut block, ref mut block2)) => {
                    let bool_type = typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                    }
                    typecheck((block, functions, vars_sub, generic_functions, type_classes, instances, aliases, globals, errors, intrinsics), f.clone(), is_loop, lexer, verbose);
                    if block2.is_some() {
                        typecheck((block2.as_deref_mut().unwrap(), functions, vars_sub, generic_functions, type_classes, instances, aliases, globals, errors, intrinsics), f.clone(), is_loop, lexer, verbose);
                    }
                },
                ASTNode(pos, ASTNodeR::While(ref mut expr, ref mut block)) => {
                    let bool_type = typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                    }
                    typecheck((block, functions, vars_sub, generic_functions, type_classes, instances, aliases, globals, errors, intrinsics), f.clone(), true, lexer, verbose);
                },
                ASTNode(pos, ASTNodeR::Return(ref mut expr)) => {
                    let tp = typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    let ret_type  = &functions.get(&f.clone()).unwrap().1;
                    if ret_type != &Type::Invalid && ! ret_type.is_compatible(&tp, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible type for return value:\n\texpected `{ret_type}`, found `{tp}`")));
                    }
                },
                ASTNode(pos, ASTNodeR::ArrIndexInit(lexpr, ref mut ind, ref mut expr, _)) => {
                    let type_r = &typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    if type_r == &Type::Invalid {
                        continue;
                    }

                    let type_ind = &typecheck_expr(ind, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    if type_ind == &Type::Invalid {
                        continue;
                    }
                    
                    let type_l = typecheck_expr(lexpr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
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
                        let vec: Vec<usize> = vector_.iter().map(|x| map.iter().find(|(n, _)| n == x).unwrap().1.0.size(aliases)).collect();
                        
                        a.1 = ASTNodeR::ArrIndexInit(lexpr.clone(), ind.clone(), expr.clone(), vec);
                    } else {
                        a.1 = ASTNodeR::ArrIndexInit(lexpr.clone(), ind.clone(), expr.clone(), vec![type_r.size(aliases)]);
                    }
                    
                },
                ASTNode(pos, ASTNodeR::VarInit(var, ref mut expr, _)) => {
                    if ! vars_sub.contains_key(var) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to variable `{var}`")));
                    } else {
                        let mut type_r = &typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
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
                        let type_r = &typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
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
                    if let Type::Custom(ref t) = tp.dealias(aliases) {
                        if vars.contains_key(t) {
                            *tp = vars[t].clone();
                        } else {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                            continue;
                        }
                    }
                    let mut type_r = &typecheck_expr(expr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);

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
                ASTNode(pos, ASTNodeR::Intrinsic(iname, ..)) => {
                    if ! intrinsics().contains_key(iname.as_str()) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to compiler intrinsic `{iname}`")));
                    }
                },
                ASTNode(pos, ASTNodeR::TypeClass(_, arg, funcs)) => {
                    for ((from, _nm), (args, ret)) in funcs {
                        if let Some(Type::Custom(a)) = from.clone().map(|x| x.dealias(aliases)) {
                            if a != *arg {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to type `{a}`")));
                            }
                        }
                        for arg_ in args {
                            if let Type::Custom(a) = arg_.dealias(aliases) {
                                if a != *arg {
                                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to type `{a}`")));
                                }
                            }
                        }
                        if let Type::Custom(a) = ret.dealias(aliases) {
                            if a != *arg {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to type `{a}`")));
                            }
                        }
                    }
                },
                ASTNode(_, ASTNodeR::Instance(_, _, _)) => {
                    // nothing here
                },
                ASTNode(pos, ASTNodeR::DerefSet(lexpr, rexpr)) => {
                    let ltype = &typecheck_expr(lexpr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);
                    let rtype = &typecheck_expr(rexpr, functions, generic_functions, (vars_sub, type_classes, instances, aliases), errors, lexer);

                    if let Type::Pointer(inner) = ltype {
                        if ! inner.is_compatible(rtype, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types {ltype} and {rtype}")));
                        }
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "cannot dereference non-pointer type {ltype}")));
                    }
                },
                ASTNode(_, ASTNodeR::Extern(..)) => {},
                ASTNode(_, ASTNodeR::Include(..)) => {},
             }
        };
    } else {
        unreachable!("value is not a block");
    }
}

fn check_ret_path(statement: &ASTNode) -> bool {
    match &statement.1 {
        ASTNodeR::If(_, block, block2) => {
            if block2.is_none() {
                false
            } else {
                check_ret_path(block) && check_ret_path(block2.as_ref().unwrap())
            }
        },
        ASTNodeR::Return(_) => {
            true
        },
        ASTNodeR::Block(blk_vec) => {
            let mut rs: bool = false;
            for statement in blk_vec {
                rs |= check_ret_path(statement);
            }
            rs
        }
        _ => false
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
                    let rs = check_ret_path(block);
                    if ! rs {
                        errors.push((ErrorLevel::Err, error!(lexer, a.0, "not all paths lead to a return statement")));
                    }
                }
            }
        },
        _ => unreachable!()
    }
}

fn infer_types(typa: Type, typb: &mut Type, current_vars: &mut HashMap<String, Type>, lexer: &mut Lexer, pos: usize, expr: Expression, aliases: &Aliases) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    
    match typb {
        Type::Var(ref name) => {
            // check for vars
            if current_vars.contains_key(name) {
                *typb = current_vars[name].clone();
            } else {
                if let Type::Var(_) = typa {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "could not infer type of argument `{expr}`")));
                    return errors;
                }
                current_vars.insert(name.clone(), typa.clone());
                *typb = typa;
            }
        },
        Type::Primitive(_) => {},
        Type::Custom(a) => {
            errors.push((ErrorLevel::Err, error!(lexer, pos, "use of undefined type `{a}`")));
            return errors;
        },
        Type::Array(ref mut inner) => {
            if inner.dealias(aliases) != **inner {
                // nothing to infer
                return errors;
            }
            
            if let Type::Array(ref innera) = typa {
                infer_types(innera.clone().dealias(aliases), inner, current_vars, lexer, pos, expr, aliases);
            } else {
                errors.push((ErrorLevel::Err, error!(lexer, pos, "`{expr}` is not an array, but an array was expected")));
                return errors;
            }
        },
        Type::Pointer(ref mut inner) => {
            if inner.dealias(aliases) != **inner {
                // nothing to infer
                return errors;
            }
            
            if let Type::Pointer(ref innera) = typa {
                infer_types(innera.clone().dealias(aliases), inner, current_vars, lexer, pos, expr, aliases);
            } else {
                errors.push((ErrorLevel::Err, error!(lexer, pos, "`{expr}` is not a pointer, but a pointer was expected")));
                return errors;
            }
        },
        Type::Function(ref mut args, ref mut ret) => {
            if let Type::Function(ref argsa, ref reta) = typa {
                for (i, arg) in args.iter_mut().enumerate() {
                    if arg.dealias(aliases) != *arg {
                        // nothing to infer
                        continue;
                    }
                    
                    infer_types(argsa[i].clone().dealias(aliases), arg, current_vars, lexer, pos, expr.clone(), aliases);
                }
                infer_types(reta.clone().dealias(aliases), ret, current_vars, lexer, pos, expr, aliases);
            } else {
                errors.push((ErrorLevel::Err, error!(lexer, pos, "`{expr}` is not a function, but a function was expected")));
                return errors;
            }
        },
        Type::Invalid => todo!(),
        Type::Struct(_, ref mut args) => {
            if let Type::Struct(_, ref argsa) = typa {
                for (i, (_, (arg, _))) in args.iter_mut().enumerate() {
                    if arg.dealias(aliases) != *arg {
                        // nothing to infer
                        continue;
                    }
                    
                    infer_types(argsa[i].1.0.clone().dealias(aliases), arg, current_vars, lexer, pos, expr.clone(), aliases);
                }
            } else {
                errors.push((ErrorLevel::Err, error!(lexer, pos, "`{expr}` is not a struct, but a struct was expected")));
                return errors;
            }
        },
        Type::Bounded(..) => {},
    }
    errors
}

fn infer_ret(ret_type: Type, current_vars: HashMap<String, Type>) -> Option<Type> {
    match ret_type {
        Type::Primitive(_)     => Some(ret_type),
        Type::Custom(_)        => Some(ret_type),
        Type::Array(ref a)     => Some(Type::Array(Box::new(infer_ret(*a.clone(), current_vars)?))),
        Type::Pointer(ref a)   => Some(Type::Pointer(Box::new(infer_ret(*a.clone(), current_vars)?)),),
        Type::Function(_, _)   => todo!(),
        Type::Invalid          => todo!(),
        Type::Struct(_, _)     => todo!(),
        Type::Var(ref name)    => {
            if current_vars.contains_key(name) {
                Some(current_vars[name].clone())
            } else {
                None
            }
        },
        Type::Bounded(..) => todo!(),
    }
}

type GenericFunc = Vec<(String, Vec<Type>, Type, Vars)>;
type ImmutableArgs<'a> = (/*vars: */&'a HashMap<String, Type>, /*type_classes: */&'a TypeClasses, /*instances: */&'a Instances, /*aliases: */&'a Aliases);
fn typecheck_expr(expr: &mut Expression, functions: &mut Functions, generic_functions: &mut HashMap<String, GenericFunc>, immutable_args: ImmutableArgs, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {

    let (vars, type_classes, instances, aliases) = immutable_args;
    
    let tp = | | -> Type {
        let exp = expr.clone();
        let pos = expr.0;
        match &mut expr.1 {
            ExpressionR::T(left_, op, right_, _) => {
                let left = typecheck_expr(left_, functions, generic_functions, immutable_args, errors, lexer);
                let right = typecheck_expr(right_, functions, generic_functions, immutable_args, errors, lexer);

                match op.combine_type(&left, &right, aliases) {
                    Type::Invalid => {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `{left}` and `{right}` for operation `{op}`")));
                        Type::Invalid
                    },
                    a => a,
                }
            },
            ExpressionR::Ref(expr) => {
                let tp = typecheck_expr(expr, functions, generic_functions, immutable_args,errors, lexer);
                Type::Pointer(Box::new(tp))
            },
            ExpressionR::Deref(expr) => {
                let tp = typecheck_expr(expr, functions, generic_functions, immutable_args,errors, lexer);
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
                    let tp = typecheck_expr(expr, functions, generic_functions, immutable_args,errors, lexer);
                    fields_.insert(name_.clone(), (tp.clone(), fields_.len()));
                }
                Type::Struct(name.clone(), fields_.iter().map(|(a, (b, c))| (a.to_owned(), (b.to_owned(), c.to_owned()))).collect())
            },
            ExpressionR::StructField(ref mut expr_, ref mut name, _, deref) => {
                let struct_type_ = typecheck_expr(expr_, functions, generic_functions, immutable_args,errors, lexer).dealias(aliases);
                let struct_type = if *deref {
                    if let Type::Pointer(stu) = struct_type_ {
                        *stu
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "type `{struct_type_}` is not a pointer, but pointer type was expected")));
                        return Type::Invalid;
                    }
                } else {
                    struct_type_
                };
                
                if let Type::Struct(ref struct_name, ref map) = struct_type {
                    if map.iter().any(|(x, _)| x == name) {
                        let ret = map.iter().find(|(x, _)| x == name).unwrap().1.0.clone();
                        expr.1 = ExpressionR::StructField(expr_.clone(), name.clone(), Some(struct_type.clone()), *deref);
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
                let left = typecheck_expr(left, functions, generic_functions, immutable_args,errors, lexer);
                
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
                let tp = typecheck_expr(lexpr, functions, generic_functions, immutable_args,errors, lexer);
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
                match len_a.cmp(&len_b) {
                    Ordering::Greater => {
                        let start = "missing argument in function call: ";
                        let offset = start.chars().count() + exp.to_string().chars().count() - 1;
                        let tp = &func_args[len_b];
                        let msg = format!("    expected argument of type {tp}");
                        let line = error_arrow!(lexer, pos, offset, msg, 1);
                        
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "{start}{exp}\n{line}")));
                        return ret_type;
                    },
                    Ordering::Less => {
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
                        let msg = "    expected ')', found argument(s)";
                        let line = error_arrow!(lexer, pos, offset, msg, exp.to_string().chars().count() - expr_off - lexpr.to_string().chars().count() - 2);
                        
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "{start}{exp}\n{line}")));
                        return ret_type;
                    },
                    Ordering::Equal => {}
                };

                let orig_vec = vec.clone();
                let mut current_vars: HashMap<String, Type> = HashMap::new();
                let mut new_args: Vec<Type> = vec![];
                let mut type_class: Option<String> = None;
                let mut type_class_arg: Option<Type> = None;
                
                for (index, a) in vec.iter_mut().enumerate() {
                    let typa = typecheck_expr(a, functions, generic_functions, immutable_args, errors, lexer);
                    let mut typb = func_args.get(index).unwrap().clone();

                    // check for type class
                    if let Type::Bounded(name, ref class) = typb.clone() {
                        if type_class.is_none() {
                            type_class = Some(class.clone());
                            type_class_arg = Some(typa.clone());

                            if ! type_classes.contains_key(class) {
                                errors.push((ErrorLevel::Err, error!(lexer, pos, "use of undefined typeclass `{class}`")));
                                return Type::Invalid;
                            }

                            if !instances.contains_key(&(class.clone(), typa.clone())) {
                                errors.push((ErrorLevel::Err, error!(lexer, pos, "type `{typa}` is not a member of `{class}` typeclass")));
                                return Type::Invalid;
                            }

                            typb = typa.clone();
                            current_vars.insert(name, typa.clone());
                        } else if type_class_arg.as_ref().unwrap() != &typb {
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
                    

                    if typb == typb.dealias(aliases) {
                        let mut new_errs = infer_types(typa.clone().dealias(aliases), &mut typb, &mut current_vars, lexer, pos, a.clone(), aliases);
                        if ! new_errs.is_empty() {
                            errors.append(&mut new_errs);
                            return ret_type;
                        }

                        new_args.push(typb.clone());
                    }

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

                let mut ret = match infer_ret(ret_type.clone(), current_vars.clone()) {
                    Some(a) => a,
                    None => {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "could not infer return type")));
                        return Type::Invalid;
                    },
                };

                // check for type class
                if let Type::Bounded(ref name, ref class) = ret.clone() {
                    if type_class.is_none() {
                        type_class = Some(class.clone());
                        type_class_arg = Some(ret_type.clone());
                        if ! type_classes.contains_key(class) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "use of undefined typeclass `{class}`")));
                            return Type::Invalid;
                        }

                        if !instances.contains_key(&(class.clone(), ret.clone())) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "type `{ret}` is not a member of `{class}` typeclass")));
                            return Type::Invalid;
                        }

                        ret = ret_type.clone();
                        current_vars.insert(name.clone(), ret_type.clone());
                    } else if type_class_arg.as_ref().unwrap() != &ret {
                        let mut args_pos = 0;
                        for arg in orig_vec {
                            args_pos += arg.to_string().chars().count() + 2;
                        }
                        
                        let first = "incompatible types in function call: ";
                        let offset = first.chars().count() + 1 + lexpr.to_string().chars().count() + 1 + args_pos;
                        let tca = type_class_arg.unwrap();
                        let msg = format!("    expected `{tca}`, found `{ret}`");
                        let line = error_arrow!(lexer, pos, offset, msg, ret.to_string().chars().count());
                        
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "{first}`{exp}`:\n{line}")));
                        return Type::Invalid;
                    }
                }

                let mut new_func_name: String = String::new();
                let mut has_var: bool = false;
                for tp in func_args.iter() {
                    hash_vars(tp.dealias(aliases), current_vars.clone(), &mut new_func_name);
                    has_var |= has_vars(tp);
                }
                hash_vars(ret_type.dealias(aliases), current_vars.clone(), &mut new_func_name);
                has_var |= has_vars(&ret_type);

                if let Some(ref a) = type_class {
                    // check for instance
                    if ! instances.contains_key(&(a.to_owned(), type_class_arg.as_ref().unwrap().clone())) {
                        let tc = type_class.as_ref().unwrap();
                        let tc_arg = type_class_arg.as_ref().unwrap();
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "instance of type class `{tc}` for type `{tc_arg}` is undefined")));
                    }
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

                    if let Entry::Vacant(e) = functions.entry((None, new_func_name.clone())) {
                        e.insert((new_args.clone(), ret.clone()));
                        if ! generic_functions.contains_key(&func_name) {
                            generic_functions.insert(func_name.clone(), vec![]);
                        }
                        generic_functions.get_mut(&func_name).unwrap().push(
                            (new_func_name.clone(), new_args.clone(),
                             ret.clone(), current_vars.clone()));
                    }

                    expr.1 = ExpressionR::F(Box::new(Expression(pos,
                                                                ExpressionR::Var(new_func_name),
                                                                Some(Type::Function(new_args, Box::new(ret.clone()))))),
                                            vec.to_owned());
                }
                ret
            },
            ExpressionR::MemberFunction(_, lexpr, name, ref mut vec) => {
                let left_type = typecheck_expr(lexpr, functions, generic_functions, immutable_args,errors, lexer).dealias(aliases);

                if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to member function `{name}` from type {left_type}")));
                    if let Type::Struct(_, fields) = left_type {
                        if fields.iter().any(|(x, _)| x == name) {
                            if let Type::Function(..) = fields.iter().find(|(x, _)| x == name).unwrap().1.0 {
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
                    let typa = typecheck_expr(a, functions, generic_functions, immutable_args,errors, lexer);
                    let typb = func_args.get(index+1).unwrap().clone();
                    
                    if ! typa.is_compatible(&typb, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                        break;
                    }
                }

                let n = name.clone();
                expr.1 = ExpressionR::MemberFunction(left_type.clone(), lexpr.clone(), name.clone(), vec.clone());
                
                let ret = functions.get(&(Some(left_type.clone()), n)).unwrap().1.clone();

                ret
            },
            ExpressionR::Arr(ref mut vec) => {
                let mut last_tp = Type::Invalid;
                for a in vec {
                    let tp = typecheck_expr(a, functions, generic_functions, immutable_args,errors, lexer);
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
                let inner_type = typecheck_expr(a, functions, generic_functions, immutable_args,errors, lexer);
                if ! Type::Primitive(PrimitiveType::Int).is_compatible(&inner_type, aliases) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `int` and `{inner_type}` in array length definition")));
                    Type::Invalid
                } else {
                    let ident_type = typecheck_expr(ident, functions, generic_functions, immutable_args,errors, lexer);
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
                        let vec: Vec<usize> = vector_.iter().map(|x| map.iter().find(|(n, _)| n == x).unwrap().1.0.size(aliases)).collect();
                        expr.1 = ExpressionR::Index(ident.clone(), a.clone(), vec);
                    } else {
                        expr.1 = ExpressionR::Index(ident.clone(), a.clone(), vec![inner_tp.size(aliases)]);
                    }
                    inner_tp
                }
            }
            ExpressionR::Cast(ref tp, ref mut rexpr, _) => {
                let rtp = typecheck_expr(rexpr, functions, generic_functions,
                                         immutable_args, errors, lexer);

                let rvalid = match rtp.dealias(aliases) {
                    Type::Primitive(ref primr) => 
                        primr == &PrimitiveType::Float ||
                        primr == &PrimitiveType::Int ||
                        primr == &PrimitiveType::Long ||
                        primr == &PrimitiveType::Char ||
                        primr == &PrimitiveType::Bool,
                    Type::Pointer(_) => true,
                    _ => false
                };

                let lvalid = match tp.dealias(aliases) {
                    Type::Primitive(ref priml) => 
                        priml == &PrimitiveType::Float ||
                        priml == &PrimitiveType::Int ||
                        priml == &PrimitiveType::Long ||
                        priml == &PrimitiveType::Char ||
                        priml == &PrimitiveType::Bool,
                    Type::Pointer(_) => true,
                    _ => false
                };
                
                let allowed = rvalid && lvalid;
                if ! allowed {
                    errors.push((ErrorLevel::Err,
                                 error!(lexer, pos,
                                        "invalid cast {rtp} -> {tp}")
                    ));
                    Type::Invalid
                } else {
                    let rettp = tp.clone();
                    expr.1 = ExpressionR::Cast(tp.clone(), rexpr.clone(), Some(rtp));
                    rettp
                }
            },
        }}();
    let typ = tp;//expr, functions, vars, aliases, errors, lexer);
    expr.2 = Some(typ.clone());
    typ
}

fn add_generic_alias(left: &Type, right: &Type, new_aliases: &mut Aliases) {
    match left {
        Type::Primitive(_) => {},
        Type::Custom(_) => {},
        Type::Array(a) => {
            if let Type::Array(inner) = right {
                add_generic_alias(a, inner, new_aliases);
            } else {
                unreachable!()
            }
        },
        Type::Pointer(a) => {
            if let Type::Pointer(inner) = right {
                add_generic_alias(a, inner, new_aliases);
            } else {
                unreachable!("Type {right} is not a pointer")
            }
        },
        Type::Function(args, ret) => {
            if let Type::Function(inner_args, inner_ret) = right {
                for (i, arg) in args.iter().enumerate() {
                    add_generic_alias(arg, &inner_args[i], new_aliases);
                }
                add_generic_alias(ret, inner_ret, new_aliases);
            } else {
                unreachable!()
            }
        },
        Type::Invalid => {},
        Type::Struct(_, _) => todo!(),
        Type::Var(nm) => {
            new_aliases.insert(nm.to_string(), right.clone());
        },
        Type::Bounded(nm, _cl) => {
            new_aliases.insert(nm.to_string(), right.clone());
        },
    }
}

fn add_generic_aliases(args: Vec<(Type, String)>, new_aliases: &mut Aliases, a: (String, Vec<Type>, Type), ret: Type) {
    // add generic function arguments to new_aliases
    for (i, (v, _)) in args.iter().enumerate() {
        add_generic_alias(v, &a.1[i], new_aliases);
    }

    add_generic_alias(&ret, &a.2, new_aliases);
}


type Ret = (Vec<Error>, Vals);
type GenericFunctions = HashMap<String, GenericFunc>;
type Vals = (Globals, Aliases, Functions, TypeClasses, Instances, GenericFunctions);
pub fn check(ast: &mut ASTNode, mut lexer: Lexer, intrinsics: fn() -> HashMap<&'static str, &'static str>, toplevel: bool, verbose: usize) -> Result<Vals, Ret> {
    let mut errors: Vec<Error> = vec![];

    let mut generic_functions = HashMap::new();
    
    // collect all aliases + functions
    let mut type_aliases: Aliases     = HashMap::new();
    let mut type_classes: TypeClasses = HashMap::new();
    let mut instances:    Instances   = HashMap::new();
    let mut functions:    Functions   = HashMap::new();
    let mut vars:         Vars        = HashMap::new();
    let mut globals:      Globals     = HashMap::new();

    if let ASTNode(_, ASTNodeR::Block(arr)) = ast {
        for a in arr.iter_mut() {
            match a {
                ASTNode(_, ASTNodeR::TypeAlias(alias, typ)) => {
                    type_aliases.insert(alias.clone(), typ.clone());
                },
                ASTNode(_, ASTNodeR::Struct(name, fields)) => {
                    type_aliases.insert(name.clone(), Type::Struct(name.clone(), fields.clone()));
                },
                ASTNode(_, ASTNodeR::Intrinsic(_, fname, args, rt)) => {
                    functions.insert((None, fname.clone()), (args.into_iter().enumerate().map(|(_, (a, _))| a.clone()).collect(), rt.clone()));
                },
                ASTNode(_, ASTNodeR::Extern(_, name, args, rt)) => {
                    functions.insert((None, name.clone()), (args.into_iter().enumerate().map(|(_, (a, _))| a.clone()).collect(), rt.clone()));
                },
                ASTNode(_, ASTNodeR::TypeClass(name, arg, funcs)) => {
                    type_classes.insert(name.clone(), (arg.clone(), funcs.clone()));
                    for (k, v) in funcs {
                        let from = if Some(Type::Custom(arg.clone())) == k.0.clone().map(|x| x.dealias(&type_aliases)) {
                            Some(Type::Bounded(arg.clone(), name.clone()))
                        } else {
                            k.0.clone()
                        };

                        let mut args = vec![];
                        for t in v.0.clone() {
                            args.push(if Type::Custom(arg.clone()) == t.dealias(&type_aliases) {
                                Type::Bounded(arg.clone(), name.clone())
                            } else {
                                t
                            });
                        }

                        let ret = if Type::Custom(arg.clone()) == v.1.dealias(&type_aliases) {
                            Type::Bounded(arg.clone(), name.clone())
                        } else {
                            v.1.clone()
                        };

                        functions.insert((from, k.1.clone()), (args, ret));
                    }
                },
                ASTNode(_, ASTNodeR::Include(_, ref mut ast, lexer_)) => {
                    let (g, a, f, tc, insts, gf) = match check(ast, lexer_.clone(), intrinsics, false, verbose) {
                        Ok(v) => v,
                        Err((vec, v)) => {
                            for e in vec {
                                errors.push(e);
                            }
                            v
                        },
                    };

                    for (k, v) in g {
                        globals.insert(k, v);
                    }
                    for (k, v) in a {
                        type_aliases.insert(k, v);
                    }
                    for (k, v) in f {
                        functions.insert(k, v);
                    }
                    for (k, v) in tc {
                        type_classes.insert(k, v);
                    }
                    for (k, v) in insts {
                        instances.insert(k, v);
                    }
                    for (k, v) in gf {
                        generic_functions.insert(k, v);
                    }
                }
                _ => {}
            }
        }
        for (i, a) in arr.clone().iter().enumerate() {
            match a {
                ASTNode(_, ASTNodeR::VarDec(_, tp, name)) => {
                    vars.insert(name.clone(), tp.clone());
                    globals.insert(name.clone(), tp.size(&type_aliases));
                },
                ASTNode(pos, ASTNodeR::FunctionDecl(ref tp, ref name, ref args, ref ret_type, _)) => {
                    let left_type = tp.clone().map(|x| x.dealias(&type_aliases));

                    if let Some(lt) = left_type.clone() {
                        if args.is_empty() {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "member function should have a `{lt} self` argument as first arg.")));
                        } else {
                            let first_type = args[0].clone().0;
                            if ! lt.is_compatible(&first_type, &type_aliases) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: first parameter of member function declaration has to be the same type as the type used on. (got `{first_type}`, expected `{lt}`)")));
                            }
                        }
                    }

                    if args.iter().any(|(x, _)| if let Type::Var(a) = x {a.starts_with('_')} else {false}) && tp.is_some() {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "cannot use type variables in member function declaration, because of the lack of decidability (consider using bounded typeclasses instead)")));
                    }

                    if functions.contains_key(&(left_type.clone(), name.clone())) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "redifinition of function `{name}`")));
                        // empty current value
                        arr[i] = ASTNode(0, ASTNodeR::Block(vec![]));
                        continue;
                    }

                    functions.insert((left_type, name.clone()), (args.iter().map(|(a, _)| a.clone()).collect(), ret_type.clone()));
                },
                ASTNode(_, ASTNodeR::VarDecInit(_, tp, name, _)) => {
                    vars.insert(name.clone(), tp.dealias(&type_aliases).clone());
                    globals.insert(name.clone(), tp.size(&type_aliases));
                },
                ASTNode(pos, ASTNodeR::Instance(name, arg, funcs)) => {
                    if ! type_classes.contains_key(name) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to typeclass `{name}`")));
                        continue;
                    }
                    let (tp_name, class_funcs) = &type_classes[name];
                    let mut defined = HashMap::new();
                    for func in funcs {
                        if let ASTNode(pos, ASTNodeR::FunctionDecl(fr_tp, fname, args, ret ,_)) = func {
                            let fr = if Some(arg.clone()) == *fr_tp {
                                Some(Type::Custom(tp_name.to_owned()))
                            } else {
                                fr_tp.to_owned()
                            };

                            if ! class_funcs.contains_key(&(fr.clone(), fname.to_owned())) {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "definition of undefined function `{fname}` in typeclass `{name}`")));
                                continue;
                            }
                            let (ref prot_args, ref prot_ret) = class_funcs[&(fr.clone(), fname.to_owned())];
                            for (i, (arg_, _)) in args.iter().enumerate() {
                                if prot_args[i] == Type::Custom(tp_name.clone()) && arg_ == arg {
                                    continue;
                                }
                                if arg_ != &prot_args[i] {
                                    let arg2 = &prot_args[i];
                                    errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{arg}` and `{arg2}` for function `{fname}` in typeclass `{name}`")));
                                    continue;
                                }
                            }
                            if *prot_ret == Type::Custom(tp_name.clone()) && ret == arg {
                                continue;
                            }
                            if ret != prot_ret {
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types `{ret}` and `{prot_ret}` for function `{fname}` in typeclass `{name}`")));
                                continue;
                            }
                            
                            defined.insert(fname.to_owned(), (fr, args, ret));
                        } else {
                            unreachable!()
                        }
                    }
                    
                    if defined.len() != class_funcs.len() {
                        // find undefined functions
                        for a in class_funcs.keys() {
                            if ! defined.contains_key(&a.1) {
                                let nm = &a.1;
                                errors.push((ErrorLevel::Err, error!(lexer, *pos, "no definition of required function `{nm}` of typeclass `{name}`")));
                                continue;
                            }
                        }
                    }

                    if ! errors.is_empty() {
                        continue;
                    }

                    instances.insert((name.to_owned(), arg.to_owned().dealias(&type_aliases)), funcs.to_owned());

                    let mut func_names = HashMap::new();
                    // add instance to funtions
                    for ((fr, fname), (args, ret)) in class_funcs {
                        let mut current_vars: HashMap<String, Type> = HashMap::new();
                        
                        if let Some(Type::Custom(ref name)) = fr {
                            let tp = defined[&(*fname)].0.clone();
                            if let Some(inner) = tp {
                                current_vars.insert(name.clone(), inner);
                            }
                        }
                        for (ind, arg) in args.iter().enumerate() {
                            if let Type::Custom(a) = arg {
                                current_vars.insert(a.clone(), defined[&(*fname)].1[ind].0.clone());
                            }
                        }
                        if let Type::Custom(ref name) = ret {
                            current_vars.insert(name.clone(), defined[&(*fname)].2.clone());
                        }

                        let mut new_func_name = String::new();
                        // hash function name based on Bounded types
                        for a in args {
                            let tp = if let Type::Custom(b) = a.dealias(&type_aliases) {
                                Type::Bounded(b.clone(), b.clone())
                            } else {
                                a.clone()
                            };
                            hash_vars(tp.dealias(&type_aliases), current_vars.clone(), &mut new_func_name);
                        }
                        
                        let tp = if let Type::Custom(b) = ret.dealias(&type_aliases) {
                            Type::Bounded(b.clone(), b.clone())
                        } else {
                            ret.clone()
                        };
                        
                        hash_vars(tp.dealias(&type_aliases), current_vars, &mut new_func_name);

                        new_func_name.push_str(fname);
                        
                        let (ref args, ref ret) = class_funcs[&(fr.clone(), fname.to_owned())];
                        functions.insert((fr.clone(), new_func_name.to_owned()), (args.clone(), ret.clone()));
                        func_names.insert(fname.to_owned(), new_func_name.to_owned());
                    }

                    // add functions to AST
                    for f in funcs {
                        if let ASTNode(pos, ASTNodeR::FunctionDecl(_, fname, _, _, block)) = f {
                            let name = func_names[fname].to_owned();
                            let (fr, args, ret) = defined[fname].to_owned();
                            let new_node = ASTNode(*pos, ASTNodeR::FunctionDecl(fr, name, args.to_owned(), ret.to_owned(), block.clone()));
                            arr.push(new_node);

                        } else {
                            unreachable!()
                        }
                    }
                },
                _ => {}
            }
        }
    } else {
        unreachable!();
    }

    typecheck((ast, &mut functions, &vars, &mut generic_functions, &mut type_classes, &mut instances, &mut type_aliases, &mut globals, &mut errors, intrinsics), (None, "".into()), false, &mut lexer, verbose);
    check_function_ret_paths(&ast.1, &mut errors, &mut lexer);

    let root_arr;
    if let ASTNode(_, ASTNodeR::Block(arr)) = ast {
        root_arr = arr;
    } else {
        unreachable!();
    }

    fn check_unused(root_arr: &mut Vec<ASTNode>, lexer: &mut Lexer, verbose: usize) -> Vec<Error> {
        let mut errors = vec![];
        // check for unused generic functions
        let mut to_remove = vec![];
        for (i, a) in root_arr.iter_mut().enumerate().rev() {
            if let ASTNode(pos, ASTNodeR::FunctionDecl(_, name, args, ret, _)) = a {
                let mut vars = false;
                for a in args {
                    vars |= has_vars(&a.0);
                }
                vars |= has_vars(ret);

                if vars {
                    if verbose > 1 {
                        errors.push((ErrorLevel::Warn, error!(lexer, *pos, "unused generic function {name} will be removed from .asm")));
                    }
                    to_remove.push(i);
                }
            } else if let ASTNode(_, ASTNodeR::Include(_, ast, lexer_)) = a {
                if let ASTNodeR::Block(nvec) = &mut ast.1 {
                    errors.append(
                        &mut check_unused(&mut Box::new(nvec), lexer_, verbose)
                    );
                } else {
                    unreachable!()
                }
            }
        }
        for i in to_remove {
            root_arr.remove(i);
        }
        errors
    }

    if toplevel {
        let mut new_errs = typecheck_generics(&mut generic_functions, root_arr, &type_aliases, &mut functions, &mut type_classes, &mut instances, &mut globals, intrinsics, &mut lexer, verbose);

        errors.append(&mut new_errs);

        errors.append(&mut check_unused(root_arr, &mut lexer, verbose));
    }

    let args = (globals, type_aliases, functions, type_classes, instances, generic_functions);

    // type checking
    if errors.is_empty() {
        Ok(args)
    } else {
        Err((errors, args))
    }
}

fn typecheck_generics(generic_functions: &mut HashMap<String, GenericFunc>,
                      root_arr: &mut Vec<ASTNode>,
                      type_aliases: &Aliases,
                      functions: &mut Functions,
                      type_classes: &mut TypeClasses,
                      instances: &mut Instances,
                      globals: &mut Globals,
                      intrinsics: fn() -> HashMap<&'static str, &'static str>,
                      lexer: &mut Lexer,
                      verbose: usize) -> Vec<Error> {

    fn remove_original(root_arr: &mut Vec<ASTNode>, func: &String, pos: &mut usize,
                       block: &mut Box<ASTNode>,
                       args: &mut Vec<(Type, String)>,
                       ret: &mut Type) -> bool {

        // remove original function declaration from AST
        for (i, a) in root_arr.iter_mut().enumerate().rev() {
            if let ASTNode(pos_, ASTNodeR::FunctionDecl(_, name, args_, ret_, block_)) = a {
                if name == func {
                    *pos = *pos_;
                    *block = block_.clone();
                    *args = args_.clone();
                    *ret = ret_.clone();
                    root_arr.remove(i);
                    return true;
                }
            } else if let ASTNode(_, ASTNodeR::Include(_, ref mut ast, _)) = a {
                if let ASTNodeR::Block(nvec) = &mut ast.1 {
                    if remove_original(&mut Box::new(nvec), func, pos, block, args, ret) {
                        return true;
                    } else {
                        continue;
                    }
                } else {
                    unreachable!()
                }
            }
        }
        false
    }
    
    let mut errors = vec![];
    
    for (func, vec) in generic_functions.clone() {
        let mut pos = 0;
        let mut block = Box::new(ASTNode(pos, ASTNodeR::Block(vec![])));
        let mut args = vec![];
        let mut ret = Type::Invalid;

        if ! remove_original(root_arr, &func, &mut pos, &mut block, &mut args, &mut ret) {
            unreachable!("original definition not found!! <- {func}")
        }

        // add generic function declarations to AST
        for (ref n, ref a, ref r, current_vars) in vec {
            let new_block = block.clone();
            let mut new_aliases = type_aliases.clone();

            add_generic_aliases(args.clone(), &mut new_aliases, (n.to_owned(), a.to_owned(), r.to_owned()), ret.clone());

            let mut node = ASTNode(pos, ASTNodeR::Block(vec![ASTNode(pos, ASTNodeR::FunctionDecl(
                None,
                n.clone(),
                args.iter().enumerate().map(|(i, v)| (a[i].clone(), v.1.clone())).collect(), r.to_owned(),
                new_block)
            )]));

            let mut generics = HashMap::new();

            typecheck((&mut  node, functions, &current_vars, &mut generics, type_classes, instances, &mut new_aliases, globals, &mut errors, intrinsics), (None, "".into()), false, lexer, verbose);

            let mut new_errs = typecheck_generics(&mut generics, root_arr, type_aliases, functions, type_classes, instances, globals, intrinsics, lexer, verbose);

            errors.append(&mut new_errs);

            let new_func;
            if let ASTNode(_, ASTNodeR::Block(here)) = node {
                new_func = here[0].clone();
            } else {
                unreachable!()
            }

            root_arr.push(new_func);
            
        }
    }
    errors
}


fn has_vars(tp: &Type) -> bool {
    match tp {
        Type::Primitive(_)        => false,
        Type::Custom(_)           => false,
        Type::Array(a)            => has_vars(a),
        Type::Pointer(a)          => has_vars(a),
        Type::Function(args, ret) => {
            let mut vars: bool = false;
            for arg in args {
                vars |= has_vars(arg);
            }
            vars |= has_vars(ret);
            vars
        },
        Type::Invalid             => false,
        Type::Struct(_, args)     => {
            let mut vars: bool = false;
            for arg in args {
                vars |= has_vars(&arg.1.0);
            }
            vars
        },
        Type::Var(_)              => true,
        Type::Bounded(..)         => true,
    }
}
