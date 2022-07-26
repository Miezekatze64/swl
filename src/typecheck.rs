use {std::collections::HashMap, crate::{error, lexer::Lexer, parser::{ASTNodeR, ExpressionR, ASTNode, Expression}, util::{Error, ErrorLevel, PrimitiveType, Type, Op}}};

type Globals = HashMap<String, usize>;
type Aliases = HashMap<String, Type>;
type Vars = Aliases;
type Functions = HashMap<(Option<Type>, String), (Vec<Type>, Type)>;

type ListArgs<'a> = (&'a mut ASTNode, &'a mut Functions, &'a Vars, &'a mut Aliases, &'a mut Globals, &'a mut Vec<(ErrorLevel, String)>, fn() -> HashMap<&'static str, &'static str>);

// check
fn typecheck(largs: ListArgs, f: (Option<Type>, String), is_loop: bool, lexer: &mut Lexer) {
    let (node, functions, vars, aliases, globals, errors, intrinsics) = largs;
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
                                                                 "invalid signature of main function, found {arg}, expected fn<() -> int>")
                            ));
                        }
                    }
                    
                    
                    for arg in args.clone() {
                        if let Type::Custom(t) = arg.0.dealias(aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                            continue;
                        }
                        vars_sub.insert(arg.1, arg.0);
                    }
                    let ac = aliases.clone();
                    typecheck((block, functions, vars_sub, aliases, globals, errors, intrinsics), (tp.as_ref().map(|x| x.dealias(&ac)), func.clone()), false, lexer);
                },
                ASTNode(pos, ASTNodeR::FunctionCall(name, args)) => {
                    if ! functions.contains_key(&(None, name.clone())) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to function `{name}`")));
                        continue;
                    }
                    
                    let func_args = &functions.get(&(None, name.clone())).unwrap().0;

                    let len_a = func_args.len();
                    let len_b = args.len();
                    if len_a != len_b {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        continue;
                    };

                    for (index, a) in args.iter_mut().enumerate() {
                        let typa = typecheck_expr(a, functions, vars_sub, aliases, errors, lexer);
                        let typb = func_args.get(index).unwrap().clone();

                        if typa == Type::Invalid || typb == Type::Invalid {
                            break;
                        }

                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }
                },
                ASTNode(pos, ASTNodeR::MemberFunction(lexpr, name, ref mut vec)) => {
                    let left_type = typecheck_expr(lexpr, functions, vars, aliases, errors, lexer).dealias(aliases);
                    if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "undefined reference to member function `{name}` from type {left_type}")));
                        continue;
                    }
                    let func_args = &functions.get(&(Some(left_type), name.clone())).unwrap().0;

                    let len_a = func_args.len();
                    let len_b = vec.len()+1;
                    if len_a != len_b {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "expected `{len_a}` arguments, got `{len_b}`")));
                        continue;
                    };

                    for (index, a) in vec.iter_mut().enumerate() {
                        let typa = typecheck_expr(a, functions, vars_sub, aliases, errors, lexer);
                        let typb = func_args.get(index+1).unwrap().clone();

                        if typa == Type::Invalid || typb == Type::Invalid {
                            break;
                        }

                        if ! typa.is_compatible(&typb, aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                            break;
                        }
                    }
                }
                ASTNode(_, ASTNodeR::Block(..)) => {
                    typecheck((a, functions, vars_sub, aliases, globals, errors, intrinsics), f.clone(), false, lexer);
                },
                ASTNode(pos, ASTNodeR::SetField(ref mut expr_, ref mut name, ref mut rexpr, _)) => {
                    let struct_type = typecheck_expr(expr_, functions, vars_sub, aliases, errors, lexer).dealias(aliases);
                    if let Type::Struct(ref struct_name, ref map) = struct_type {
                        if map.contains_key(name) {
                            let field_type = map.get(name).unwrap().0.clone();
                            let rtype = typecheck_expr(rexpr, functions, vars_sub, aliases, errors, lexer);
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
                    let bool_type = typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
                    if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                    }
                    typecheck((block, functions, vars_sub, aliases, globals, errors, intrinsics), f.clone(), is_loop, lexer);
                },
                ASTNode(pos, ASTNodeR::While(ref mut expr, ref mut block)) => {
                    let bool_type = typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
                    if bool_type != Type::Invalid && ! Type::Primitive(PrimitiveType::Bool).is_compatible(&bool_type, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `bool`, found `{bool_type}`")));
                    }
                    typecheck((block, functions, vars_sub, aliases, globals, errors, intrinsics), f.clone(), true, lexer);
                },
                ASTNode(pos, ASTNodeR::Return(ref mut expr)) => {
                    let tp = typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
                    let ret_type  = &functions.get(&f.clone()).unwrap().1;
                    if ret_type != &Type::Invalid && ! ret_type.is_compatible(&tp, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{ret_type}`, found `{tp}`, because of return type")));
                    }
                },
                ASTNode(pos, ASTNodeR::ArrIndexInit(lexpr, ref mut ind, ref mut expr, _)) => {
                    let type_r = &typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
                    if type_r == &Type::Invalid {
                        continue;
                    }

                    let type_ind = &typecheck_expr(ind, functions, vars_sub, aliases, errors, lexer);
                    if type_ind == &Type::Invalid {
                        continue;
                    }
                    
                    let type_l = typecheck_expr(lexpr, functions, vars_sub, aliases, errors, lexer);
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
                        let mut type_r = &typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
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
                        let type_r = &typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
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
                ASTNode(pos, ASTNodeR::VarDecInit(_, tp, name, expr)) => {
                    if let Type::Custom(t) = tp.dealias(aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "use of undefined type `{t}`")));
                        continue;
                    }
                    let mut type_r = &typecheck_expr(expr, functions, vars_sub, aliases, errors, lexer);
                    if type_r == &Type::Invalid {
                        // error has already been set
                        type_r = tp;
//                        continue;
                    }
                    let typ = tp.dealias(aliases);
                    if ! typ.is_compatible(type_r, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, *pos, "incompatible types: expected `{typ}`, found `{type_r}`")));
                    }
                    vars_sub.insert(name.clone(), typ.clone());
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



fn typecheck_expr(expr: &mut Expression, functions: &Functions, vars: &HashMap<String, Type>, aliases: &Aliases, errors: &mut Vec<(ErrorLevel, String)>, lexer: &mut Lexer) -> Type {
    
    let tp = | | -> Type {
        let pos = expr.0;
        match &mut expr.1 {
            ExpressionR::T(ref mut left, op, ref mut right, _) => {
                let left = typecheck_expr(left, functions, vars, aliases,  errors, lexer);
                let right = typecheck_expr(right, functions, vars, aliases,  errors, lexer);
                
                match op.combine_type(&left, &right, aliases) {
                    Type::Invalid => {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `{left}` and `{right}` for operation `{op}`")));
                        Type::Invalid
                    },
                    a => a,
                }
            },
            ExpressionR::Ref(expr) => {
                let tp = typecheck_expr(expr, functions, vars, aliases, errors, lexer);
                Type::Pointer(Box::new(tp))
            },
            ExpressionR::Deref(expr) => {
                let tp = typecheck_expr(expr, functions, vars, aliases, errors, lexer);
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
                    let tp = typecheck_expr(expr, functions, vars, aliases, errors, lexer);
                    fields_.insert(name_.clone(), (tp.clone(), fields_.len()));
                }
                Type::Struct(name.clone(), fields_)
            },
            ExpressionR::StructField(ref mut expr_, ref mut name, _) => {
                let struct_type = typecheck_expr(expr_, functions, vars, aliases, errors, lexer).dealias(aliases);
                if let Type::Struct(ref struct_name, ref map) = struct_type {
                    if map.contains_key(name) {
                        let ret = map.get(name).unwrap().0.clone();
                        expr.1 = ExpressionR::StructField(expr_.clone(), name.clone(), Some(struct_type.clone()));
                        return ret;
                    } else {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "undefinded field `{name}` for type `{struct_name}`")));
                    }
                }
                Type::Invalid
            }
            ExpressionR::UnaryOp(op, ref mut left, _) => {
                let left = typecheck_expr(left, functions, vars, aliases, errors, lexer);
                
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
                if ! vars.contains_key(name) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to variable `{name}`")));
                    Type::Invalid
                } else {
                    vars.get(name).unwrap().clone()
                }
            },
            ExpressionR::F(name, ref mut vec) => {
                if ! functions.contains_key(&(None, name.clone())) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to function `{name}`")));
                    return Type::Invalid;
                }
                
                let func_args = &functions.get(&(None, name.clone())).unwrap().0;

                let len_a = func_args.len();
                let len_b = vec.len();
                if len_a != len_b {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "expected `{len_a}` arguments, got `{len_b}`")));
                    return functions.get(&(None, name.clone())).unwrap().1.clone();
                };
                
                for (index, a) in vec.iter_mut().enumerate() {
                    let typa = typecheck_expr(a, functions, vars, aliases, errors, lexer);
                    let typb = func_args.get(index).unwrap().clone();
                    
                    if ! typa.is_compatible(&typb, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                        break;
                    }
                }
                
                functions.get(&(None, name.clone())).unwrap().1.clone()
            },
            ExpressionR::MemberFunction(lexpr, name, ref mut vec) => {
                let left_type = typecheck_expr(lexpr, functions, vars, aliases, errors, lexer).dealias(aliases);
                if ! functions.contains_key(&(Some(left_type.clone()), name.clone())) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "undefined reference to member function `{name}` from type {left_type}")));
                    return Type::Invalid;
                }
                
                let func_args = &functions.get(&(Some(left_type.clone()), name.clone())).unwrap().0;

                let len_a = func_args.len();
                let len_b = vec.len()+1;
                if len_a != len_b {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "expected `{len_a}` arguments, got `{len_b}`")));
                    return Type::Invalid;
                };

                
                for (index, a) in vec.iter_mut().enumerate() {
                    let typa = typecheck_expr(a, functions, vars, aliases, errors, lexer);
                    let typb = func_args.get(index+1).unwrap().clone();
                    
                    if ! typa.is_compatible(&typb, aliases) {
                        errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: expected `{typb}`, found `{typa}`")));
                        break;
                    }
                }
                
                functions.get(&(Some(left_type.clone()), name.clone())).unwrap().1.clone()
            },
            ExpressionR::Arr(ref mut vec) => {
                let mut last_tp = Type::Invalid;
                for a in vec {
                    let tp = typecheck_expr(a, functions, vars, aliases, errors, lexer);
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
            ExpressionR::Index(ident, ref mut a, _) => {

                let inner_type = typecheck_expr(a, functions, vars, aliases, errors, lexer);
                if ! Type::Primitive(PrimitiveType::Int).is_compatible(&inner_type, aliases) {
                    errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types `int` and `{inner_type}` in array length definition")));
                    Type::Invalid
                } else {
                    let ident_type = typecheck_expr(ident, functions, vars, aliases, errors, lexer);
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
    
    if let ASTNode(_, ASTNodeR::Block(arr)) = ast.clone() {
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
        for a in arr {
            match a {
                ASTNode(_, ASTNodeR::VarDec(_, tp, name)) => {
                    vars.insert(name.clone(), tp.clone());
                    globals.insert(name, tp.size(&type_aliases));
                },
                ASTNode(pos, ASTNodeR::FunctionDecl(tp, name, args, ret_type, _)) => {
                    let left_type = tp.map(|x| x.dealias(&type_aliases));

                    if let Some(lt) = left_type.clone() {
                        let first_type = args[0].clone().0;
                        if ! lt.is_compatible(&first_type, &type_aliases) {
                            errors.push((ErrorLevel::Err, error!(lexer, pos, "incompatible types: first parameter of member function declaration has to be the same type as the type used on. (got `{first_type}`, expected `{lt}`)")));
                        }
                    }
                    functions.insert((left_type, name), (args.into_iter().map(|(a, _)| a).collect(), ret_type));
                },
                ASTNode(_, ASTNodeR::VarDecInit(_, tp, name, _)) => {
                    vars.insert(name.clone(), tp.clone());
                    globals.insert(name, tp.size(&type_aliases));
                },
                _ => {}
            }
        }
    } else {
        unreachable!();
    }

    typecheck((ast, &mut functions, &vars, &mut type_aliases, &mut globals, &mut errors, intrinsics), (None, "".into()), false, &mut lexer);
    check_function_ret_paths(&ast.1, &mut errors, &mut lexer);

    // type checking
    if errors.is_empty() {
        Ok((globals, type_aliases, functions))
    } else {
        Err(errors)
    }
}
