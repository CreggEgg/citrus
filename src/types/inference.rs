use std::collections::HashMap;

use crate::ast::{File, Literal, TopLevelDeclaration, TypeDeclaration, TypeName, UntypedExpr};

use super::{Type, TypedExpr, TypedFile, TypedLiteral, TypedTopLevelDeclaration};

#[derive(Debug, Clone)]
pub enum TypeError {
    UnknownType(String),
    UndefinedValue(String),
    WrongNumberArgs(usize, usize, String),
    IncorrectArgs(Vec<Type>, Vec<TypedExpr>, String),
    NonFunctionCall(String, Type),
    IncorrectType(Type, Type),
    UnmatchedTypes(Type, Type),
    InconsistentArrayValues(Type, Type),
    AccessOnNonStruct(Type),
    MissingKey(String),
    MismatchedMutation(String, Type, Type),
}

pub fn type_file(ast: File) -> Result<TypedFile, TypeError> {
    let mut types: HashMap<String, Type> = HashMap::new();
    types.insert("int".into(), Type::Int);
    types.insert("float".into(), Type::Float);
    types.insert("bool".into(), Type::Bool);
    types.insert("unit".into(), Type::Unit);
    let mut scope: HashMap<String, Type> = HashMap::new();
    let mut declarations = Vec::new();
    for declaration in ast.declarations {
        match declaration {
            TopLevelDeclaration::Type(type_declaration) => {
                let (name, r#type) = get_type_declaration(&type_declaration, &types)?;
                types.insert(name, r#type);
            }
            TopLevelDeclaration::Binding { public, lhs, rhs } => {
                let expr = if let UntypedExpr::Literal(Literal::Function {
                    args,
                    body,
                    ret_type,
                }) = rhs
                {
                    let literal = Literal::Function {
                        args,
                        body,
                        ret_type,
                    };
                    TypedExpr::Literal(
                        get_literal_type(&literal, &types, &scope)?,
                        type_literal(&literal, &types, &scope, Some(lhs.clone()))?,
                    )
                } else {
                    type_expr(rhs, &types, &scope)?.0
                };
                scope.insert(lhs.clone(), get_type(expr.clone()));
                declarations.push(super::TypedTopLevelDeclaration::Binding {
                    public,
                    lhs,
                    rhs: expr,
                })
            }
            TopLevelDeclaration::Extern(annotated_ident) => {
                let ty = type_from_name(annotated_ident.type_name, &types)?;
                scope.insert(annotated_ident.name.clone(), ty.clone());
                declarations.push(TypedTopLevelDeclaration::Extern(super::AnnotatedIdent {
                    name: annotated_ident.name,
                    r#type: ty,
                }))
            }
        }
    }
    #[cfg(debug_assertions)]
    dbg!(scope);
    Ok(TypedFile { declarations })
}

fn get_type(expr: super::TypedExpr) -> Type {
    match expr {
        TypedExpr::BinaryOp {
            r#type,
            lhs: _,
            op: _,
            rhs: _,
        } => r#type,
        TypedExpr::Literal(r#type, _) => r#type,
        TypedExpr::Ident(r#type, _) => r#type,
        TypedExpr::FunctionCall(r#type, _, _) => r#type,
        TypedExpr::Binding {
            r#type,
            lhs: _,
            rhs: _,
            local: _,
        } => r#type,
        TypedExpr::IfElse {
            r#type,
            condition: _,
            then: _,
            r#else: _,
        } => r#type,
        TypedExpr::UnaryOp {
            r#type,
            op: _,
            target: _,
        } => r#type,
        TypedExpr::Mutate {
            r#type,
            lhs: _,
            rhs: _,
        } => r#type,
        TypedExpr::Access(r#type, _, _) => r#type,
    }
}

fn type_expr(
    expr: crate::ast::UntypedExpr,
    types: &HashMap<String, Type>,
    scope: &HashMap<String, Type>,
) -> Result<(TypedExpr, HashMap<String, Type>), TypeError> {
    match expr {
        crate::ast::UntypedExpr::BinaryOp { lhs, op, rhs } => {
            let lhs = type_expr(*lhs, types, scope)?;
            let rhs = type_expr(*rhs, types, scope)?;
            let shared_type = require_identical(get_type(lhs.0.clone()), get_type(rhs.0.clone()))?;
            Ok((
                TypedExpr::BinaryOp {
                    r#type: match op {
                        crate::ast::BinaryOperator::Add
                        | crate::ast::BinaryOperator::Subtract
                        | crate::ast::BinaryOperator::Multiply
                        | crate::ast::BinaryOperator::Divide
                        // | crate::ast::BinaryOperator::Power
                        | crate::ast::BinaryOperator::Modulo => shared_type,
                        crate::ast::BinaryOperator::Gt
                        | crate::ast::BinaryOperator::Lt
                        | crate::ast::BinaryOperator::Gte
                        | crate::ast::BinaryOperator::Lte
                        | crate::ast::BinaryOperator::Eq
                        | crate::ast::BinaryOperator::And
                        | crate::ast::BinaryOperator::Or => Type::Bool,
                    },
                    lhs: Box::new(lhs.0),
                    op,
                    rhs: Box::new(rhs.0),
                },
                scope.clone(),
            ))
        }
        crate::ast::UntypedExpr::Binding { lhs, rhs, local } => {
            let rhs = if let UntypedExpr::Literal(Literal::Function {
                args,
                body,
                ret_type,
            }) = *rhs
            {
                let literal = Literal::Function {
                    args,
                    body,
                    ret_type,
                };
                TypedExpr::Literal(
                    get_literal_type(&literal, types, scope)?,
                    type_literal(&literal, types, scope, Some(lhs.clone()))?,
                )
            } else {
                type_expr(*rhs, types, scope)?.0
            };
            let mut body_scope = scope.clone();
            body_scope.insert(lhs.clone(), get_type(rhs.clone()));
            Ok((
                TypedExpr::Binding {
                    r#type: get_type(rhs.clone()),
                    lhs: lhs.clone(),
                    rhs: Box::new(rhs),
                    local,
                },
                body_scope,
            ))
        }
        crate::ast::UntypedExpr::Literal(literal) => Ok((
            TypedExpr::Literal(
                get_literal_type(&literal, types, scope)?,
                type_literal(&literal, types, scope, None)?,
            ),
            scope.clone(),
        )),
        crate::ast::UntypedExpr::Ident(ident) => Ok((
            TypedExpr::Ident(
                scope
                    .get(&ident)
                    .ok_or(TypeError::UndefinedValue(ident.clone()))?
                    .clone(),
                ident,
            ),
            scope.clone(),
        )),
        crate::ast::UntypedExpr::FunctionCall(name, args) => {
            let func = scope
                .get(&name)
                .ok_or(TypeError::UndefinedValue(name.clone()))?;
            let args = args
                .iter()
                .map(|arg| type_expr(arg.clone(), types, scope).map(|it| it.0))
                .collect::<Result<Vec<TypedExpr>, TypeError>>()?;
            if let Type::Function(func_args, ret) = func {
                if !func_args.iter().enumerate().all(|(i, arg)| {
                    args.get(i)
                        .map(|arg| get_type(arg.clone()))
                        .ok_or(TypeError::WrongNumberArgs(
                            func_args.len(),
                            args.len(),
                            name.clone(),
                        ))
                        .map(|supplied| supplied == *arg)
                        .unwrap_or(false)
                }) {
                    Err(TypeError::IncorrectArgs(
                        func_args.clone(),
                        args,
                        name.clone(),
                    ))
                } else {
                    // let mut scope = scope.clone();
                    // for arg in func_args {
                    //     scope.insert(arg.name.clone(), arg.r#type.clone());
                    // }
                    Ok((
                        TypedExpr::FunctionCall(*ret.clone(), name.clone(), args),
                        scope.clone(),
                    ))
                }
            } else {
                Err(TypeError::NonFunctionCall(name.clone(), func.clone()))
            }
        }
        crate::ast::UntypedExpr::IfElse {
            condition,
            then,
            r#else,
        } => {
            let then_block_typed = block_type(then, types, scope)?;
            let else_block_typed = block_type(r#else, types, scope)?;
            let r#type = require_identical(then_block_typed.0, else_block_typed.0)?;
            let condition = type_expr(*condition, types, scope)?;
            Ok((
                TypedExpr::IfElse {
                    r#type: r#type.clone(),
                    condition: Box::new(condition.0),
                    then: then_block_typed.1,
                    r#else: else_block_typed.1,
                },
                scope.clone(),
            ))
        }
        crate::ast::UntypedExpr::UnaryOp { op, ref target } => {
            match op {
                // crate::ast::UnaryOperator::Positive => Ok((
                //     TypedExpr::UnaryOp {
                //         r#type: require_type(
                //             type_expr(*target.clone(), types, scope)?.0,
                //             Type::Int,
                //         )?,
                //         op,
                //         target: Box::new(type_expr(*target.clone(), types, scope)?.0),
                //     },
                //     scope.clone(),
                // )),
                crate::ast::UnaryOperator::Negative => Ok((
                    TypedExpr::UnaryOp {
                        r#type: require_type(
                            type_expr(*target.clone(), types, scope)?.0,
                            Type::Int,
                        )?,
                        op,
                        target: Box::new(type_expr(*target.clone(), types, scope)?.0),
                    },
                    scope.clone(),
                )),
                crate::ast::UnaryOperator::Exclave => {
                    let (typed, scope) = type_expr(*target.clone(), types, scope)?;
                    // let r#type = match get_type(typed.clone()) {
                    //     // Type::Int => todo!(),
                    //     // Type::Float => todo!(),
                    //     // Type::Bool => todo!(),
                    //     // Type::Unit => todo!(),
                    //     // Type::Enum(_) => todo!(),
                    //     // Type::Array(_) => todo!(),
                    //     Type::Handle(ret) => Ok(*ret.clone()),
                    //     ty => Err(TypeError::IncorrectType(
                    //         Type::Handle(Box::new(Type::Unit)),
                    //         ty,
                    //     )), // Type::Struct(_) => todo!(),
                    // Type::Handle(_) => todo!(),
                    // };
                    Ok((
                        TypedExpr::UnaryOp {
                            r#type: get_type(typed.clone()),
                            op,
                            target: Box::new(typed),
                        },
                        scope,
                    ))
                }
                crate::ast::UnaryOperator::Not => {
                    let (typed, scope) = type_expr(*target.clone(), types, scope)?;
                    let r#type = get_type(typed.clone());
                    if r#type != Type::Bool {
                        Err(TypeError::IncorrectType(r#type, Type::Bool))
                    } else {
                        Ok((
                            TypedExpr::UnaryOp {
                                r#type,
                                op,
                                target: Box::new(typed),
                            },
                            scope,
                        ))
                    }
                }
            }
        }
        crate::ast::UntypedExpr::Mutate { lhs, rhs } => {
            let rhs = type_expr(*rhs, types, scope)?;
            let mut body_scope = scope.clone();
            body_scope.insert(lhs.clone(), get_type(rhs.0.clone()));
            if let Some(r#type) = scope.get(&lhs) {
                if *r#type != get_type(rhs.0.clone()) {
                    return Err(TypeError::MismatchedMutation(
                        lhs,
                        r#type.clone(),
                        get_type(rhs.0.clone()),
                    ));
                }
            }
            Ok((
                TypedExpr::Mutate {
                    r#type: get_type(rhs.0.clone()),
                    lhs: lhs.clone(),
                    rhs: Box::new(rhs.0),
                },
                body_scope,
            ))
        }
        UntypedExpr::Access(r#struct, field) => {
            let lhs = type_expr(*r#struct, types, scope)?.0;
            let struct_type = get_type(lhs.clone());
            if let Type::Struct(pairs) = struct_type {
                if let Some(value) = pairs.get(&field) {
                    let mut pairs = pairs.iter().collect::<Vec<_>>();
                    pairs.sort_by(|a, b| a.0.cmp(b.0));
                    let idx = pairs.iter().position(|a| *a.0 == field).unwrap();
                    Ok((
                        TypedExpr::Access(value.clone(), Box::new(lhs), idx as i32),
                        scope.clone(),
                    ))
                } else {
                    Err(TypeError::MissingKey(field))
                }
            } else {
                Err(TypeError::AccessOnNonStruct(struct_type))
            }
        }
    }
}

fn require_identical(then_block_typed: Type, else_block_typed: Type) -> Result<Type, TypeError> {
    if then_block_typed == else_block_typed {
        Ok(then_block_typed)
    } else {
        Err(TypeError::UnmatchedTypes(
            then_block_typed,
            else_block_typed,
        ))
    }
}

fn block_type(
    block: Vec<crate::ast::UntypedExpr>,
    types: &HashMap<String, Type>,
    scope: &HashMap<String, Type>,
) -> Result<(Type, Vec<TypedExpr>), TypeError> {
    let mut exprs = Vec::new();
    let mut scope = scope.clone();
    for line in block {
        let new = type_expr(line, types, &scope)?;
        scope = new.1;
        exprs.push(new.0);
    }
    Ok((
        exprs
            .last()
            .map_or(Type::Unit, |expr| get_type(expr.clone())),
        exprs,
    ))
}

fn require_type(target: TypedExpr, required: Type) -> Result<Type, TypeError> {
    let ty = get_type(target.clone());
    if ty == required {
        Ok(required)
    } else {
        Err(TypeError::IncorrectType(required, ty))
    }
}

// fn require_same_type(a: Vec<TypedExpr>, b: Vec<TypedExpr>) -> Result<TypedExpr, TypeError> {
//
// }

fn type_literal(
    literal: &crate::ast::Literal,
    types: &HashMap<String, Type>,
    scope: &HashMap<String, Type>,
    fn_name: Option<String>,
) -> Result<TypedLiteral, TypeError> {
    Ok(match literal {
        crate::ast::Literal::Int(x) => TypedLiteral::Int(*x),
        crate::ast::Literal::Float(x) => TypedLiteral::Float(*x),
        crate::ast::Literal::Bool(x) => TypedLiteral::Bool(*x),
        crate::ast::Literal::Array(x) => {
            let typed = x
                .iter()
                .map(|expr| Ok(type_expr(expr.clone(), types, scope)?.0))
                .collect::<Result<Vec<TypedExpr>, TypeError>>()?;
            TypedLiteral::Array(typed)
        }
        crate::ast::Literal::Struct(x) => {
            let pairs = x
                .iter()
                .map(|(key, value)| Ok((key.clone(), type_expr(value.clone(), types, scope)?.0)))
                .collect::<Result<Vec<(String, TypedExpr)>, _>>()?;
            TypedLiteral::Struct(
                Type::Struct(
                    pairs
                        .iter()
                        .map(|(k, x)| (k.clone(), get_type(x.clone())))
                        .collect(),
                ),
                pairs,
            )
        }
        crate::ast::Literal::String(x) => TypedLiteral::String(x.clone()),
        crate::ast::Literal::Function {
            args,
            body,
            ret_type,
        } => {
            let args = args
                .iter()
                .map(|annotated| {
                    Ok(crate::types::AnnotatedIdent {
                        name: annotated.name.clone(),
                        r#type: type_from_name(annotated.type_name.clone(), types)?,
                    })
                })
                .collect::<Result<Vec<crate::types::AnnotatedIdent>, TypeError>>()?;
            let ret = match ret_type {
                Some(ret) => Some(type_from_name(ret.clone(), types)?),
                None => None,
            };
            let mut body_scope = scope.clone();
            if let Some(name) = fn_name {
                body_scope.insert(
                    name,
                    Type::Function(
                        args.iter().map(|arg| arg.r#type.clone()).collect(),
                        Box::new(ret.clone().unwrap_or(Type::Unit)),
                    ),
                );
            };
            for arg in &args {
                body_scope.insert(arg.name.clone(), arg.r#type.clone());
            }

            let mut typed_body = Vec::new();
            for expr in body {
                let (expr, scope) = type_expr(expr.clone(), types, &body_scope)?;
                body_scope = scope;
                typed_body.push(expr);
            }
            #[cfg(debug_assertions)]
            dbg!(&body_scope);
            TypedLiteral::Function {
                args,
                body: typed_body,
                ret_type: ret, //type_from_name(
                               //ret_type.clone().unwrap_or(TypeName::Named("unit".into())),
                               //types,
                               //),
            }
        }
        Literal::Unit => TypedLiteral::Unit,
    })
}

fn get_literal_type(
    literal: &crate::ast::Literal,
    types: &HashMap<String, Type>,
    scope: &HashMap<String, Type>,
) -> Result<Type, TypeError> {
    match literal {
        crate::ast::Literal::Int(_) => Ok(Type::Int),
        crate::ast::Literal::Float(_) => Ok(Type::Float),
        crate::ast::Literal::Bool(_) => Ok(Type::Bool),
        crate::ast::Literal::Struct(pairs) => Ok(Type::Struct(
            pairs
                .iter()
                .map(|(name, value)| {
                    Ok((
                        name.clone(),
                        get_type(type_expr(value.clone(), types, scope)?.0),
                    ))
                })
                .collect::<Result<HashMap<String, Type>, _>>()?,
        )),
        crate::ast::Literal::String(_) => Ok(Type::Array(Box::new(Type::Int))),
        crate::ast::Literal::Function { args, ret_type, .. } => Ok(Type::Function(
            args.iter()
                .map(|annotation| type_from_name(annotation.type_name.clone(), types))
                .collect::<Result<Vec<Type>, TypeError>>()?,
            Box::new(
                ret_type
                    .as_ref()
                    .map(|t| type_from_name(t.clone(), types))
                    .unwrap_or(Ok(Type::Unit))?, // ret_type
                                                 //     .as_ref()
                                                 //     .map(|t| {
                                                 //         types
                                                 //             .get(t)
                                                 //             .ok_or(TypeError::UnknownType(t.clone()))
                                                 //             .cloned()
                                                 //     })
                                                 //     .unwrap_or(Ok(Type::Unit))?,
            ),
        )),
        Literal::Array(vals) => {
            let typed = vals
                .iter()
                .map(|expr| Ok(get_type(type_expr(expr.clone(), types, scope)?.0)))
                .collect::<Result<Vec<_>, _>>()?;
            let first_type = typed.first().unwrap().clone();
            for r#type in typed {
                if r#type != first_type.clone() {
                    return Err(TypeError::InconsistentArrayValues(first_type, r#type));
                };
            }
            Ok(Type::Array(Box::new(first_type)))
        }
        Literal::Unit => Ok(Type::Unit),
    }
}

fn type_from_name(type_name: TypeName, types: &HashMap<String, Type>) -> Result<Type, TypeError> {
    // types
    //     .get(&type_name)
    //     .ok_or(TypeError::UnknownType(type_name))
    //     .cloned()
    match type_name {
        TypeName::Named(name) => types
            .get(&name)
            .ok_or(TypeError::UnknownType(name))
            .cloned(),
        TypeName::Array(name) => Ok(Type::Array(Box::new(type_from_name(*name, types)?))),
        TypeName::Function(args, ret) => Ok(Type::Function(
            args.iter()
                .map(|annotation| {
                    type_from_name(annotation.clone(), types)
                    // Ok(crate::types::AnnotatedIdent {
                    //     name: annotation.name.clone(),
                    //     r#type: type_from_name(annotation.type_name.clone(), types)?,
                    // })
                })
                .collect::<Result<Vec<Type>, TypeError>>()?,
            Box::new(type_from_name(*ret, types)?),
        )),
    }
}

// fn get_type_name(type_name: TypeName) -> Result<String, TypeError> {
//     if let TypeName::Named(name) = type_name {
//         Ok(name)
//     } else {
//         Err(TypeError::)
//     }
// }

fn get_type_declaration(
    declaration: &TypeDeclaration,
    types: &HashMap<String, Type>,
) -> Result<(String, Type), TypeError> {
    match declaration {
        TypeDeclaration::Struct(name, r#struct) => {
            let mut fields = HashMap::new();
            for field in r#struct {
                fields.insert(
                    field.name.clone(),
                    type_from_name(field.type_name.clone(), types)?, // types
                                                                     //     .get(&field.type_name)
                                                                     //     .ok_or(TypeError::UnknownType(field.type_name.clone()))?
                                                                     //     .clone(),
                );
            }
            Ok((
                name.clone(),
                Type::Struct(
                    fields.clone(), /*, fields.keys().map(|x| x.clone()).collect()*/
                ),
            ))
        }
        TypeDeclaration::Enum(name, r#enum) => Ok((name.clone(), Type::Enum(r#enum.clone()))),
        TypeDeclaration::Alias(name, target_name) => {
            let r#type = type_from_name(target_name.clone(), types)?;
            // let r#type = types
            //     .get(target_name)
            //     .ok_or(TypeError::UnknownType(target_name.clone()))?;
            Ok((name.to_string(), r#type.clone()))
        }
    }
}
