use std::{any::Any, clone, collections::HashMap};

use crate::ast::{AnnotatedIdent, File, TopLevelDeclaration, TypeDeclaration, TypeName};

use super::{Type, TypedExpr, TypedFile, TypedLiteral, TypedTopLevelDeclaration};

#[derive(Debug)]
pub enum TypeError {
    UnknownType(String),
    UndefinedValue(String),
    WrongNumberArgs(usize, usize, String),
    IncorrectArgs(Vec<Type>, Vec<TypedExpr>, String),
    NonFunctionCall(String, Type),
    IncorrectType(Type, Type),
    UnmatchedTypes(Type, Type),
}

pub fn type_file(ast: File) -> Result<TypedFile, TypeError> {
    let mut types: HashMap<String, Type> = HashMap::new();
    types.insert("int".into(), Type::Int);
    types.insert("bool".into(), Type::Bool);
    types.insert("unit".into(), Type::Float);
    let mut scope: HashMap<String, Type> = HashMap::new();
    let mut declarations = Vec::new();
    for declaration in ast.declarations {
        match declaration {
            TopLevelDeclaration::Type(type_declaration) => {
                let (name, r#type) = get_type_declaration(&type_declaration, &types)?;
                types.insert(name, r#type);
            }
            TopLevelDeclaration::Binding { lhs, rhs } => {
                let expr = type_expr(rhs, &types, &scope)?;
                scope.insert(lhs.clone(), get_type(expr.0.clone()));
                declarations.push(super::TypedTopLevelDeclaration::Binding { lhs, rhs: expr.0 })
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
            Ok((
                TypedExpr::BinaryOp {
                    r#type: require_identical(get_type(lhs.0.clone()), get_type(rhs.0.clone()))?,
                    lhs: Box::new(lhs.0),
                    op,
                    rhs: Box::new(rhs.0),
                },
                scope.clone(),
            ))
        }
        crate::ast::UntypedExpr::Literal(literal) => Ok((
            TypedExpr::Literal(
                get_literal_type(&literal, types, scope)?,
                type_literal(&literal, types, scope)?,
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
        crate::ast::UntypedExpr::Binding { lhs, rhs, local } => {
            let rhs = type_expr(*rhs, types, scope)?;
            let mut body_scope = scope.clone();
            body_scope.insert(lhs.clone(), get_type(rhs.0.clone()));
            Ok((
                TypedExpr::Binding {
                    r#type: get_type(rhs.0.clone()),
                    lhs: lhs.clone(),
                    rhs: Box::new(rhs.0),
                    local,
                },
                body_scope,
            ))
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
        crate::ast::UntypedExpr::UnaryOp { op, ref target } => match op {
            crate::ast::UnaryOperator::Positive => Ok((
                TypedExpr::UnaryOp {
                    r#type: require_type(type_expr(*target.clone(), types, scope)?.0, Type::Int)?,
                    op,
                    target: Box::new(type_expr(expr, types, scope)?.0),
                },
                scope.clone(),
            )),
            crate::ast::UnaryOperator::Negative => Ok((
                TypedExpr::UnaryOp {
                    r#type: require_type(type_expr(*target.clone(), types, scope)?.0, Type::Int)?,
                    op,
                    target: Box::new(type_expr(expr, types, scope)?.0),
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
        },
        crate::ast::UntypedExpr::Mutate { lhs, rhs } => {
            let rhs = type_expr(*rhs, types, scope)?;
            let mut body_scope = scope.clone();
            body_scope.insert(lhs.clone(), get_type(rhs.0.clone()));
            Ok((
                TypedExpr::Mutate {
                    r#type: get_type(rhs.0.clone()),
                    lhs: lhs.clone(),
                    rhs: Box::new(rhs.0),
                },
                body_scope,
            ))
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
) -> Result<TypedLiteral, TypeError> {
    Ok(match literal {
        crate::ast::Literal::Int(x) => TypedLiteral::Int(*x),
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
            let mut body_scope = scope.clone();
            for arg in &args {
                body_scope.insert(arg.name.clone(), arg.r#type.clone());
            }

            let mut typed_body = Vec::new();
            for expr in body {
                let (expr, scope) = type_expr(expr.clone(), types, &body_scope)?;
                body_scope = scope;
                typed_body.push(expr);
            }
            TypedLiteral::Function {
                args,
                body: typed_body,
                ret_type: ret_type.clone(),
            }
        }
    })
}

fn get_literal_type(
    literal: &crate::ast::Literal,
    types: &HashMap<String, Type>,
    scope: &HashMap<String, Type>,
) -> Result<Type, TypeError> {
    match literal {
        crate::ast::Literal::Int(_) => Ok(Type::Int),
        crate::ast::Literal::String(val) => Ok(Type::Array(Box::new(Type::Int), val.len() as i64)),
        crate::ast::Literal::Function {
            args,
            body,
            ret_type,
        } => Ok(Type::Function(
            args.iter()
                .map(|annotation| Ok(type_from_name(annotation.type_name.clone(), types)?))
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
        TypeName::Array(name, size) => {
            Ok(Type::Array(Box::new(type_from_name(*name, types)?), size))
        }
        TypeName::Function(args, ret) => Ok(Type::Function(
            args.iter()
                .map(|annotation| {
                    Ok(type_from_name(annotation.clone(), types)?)
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
            Ok((name.clone(), Type::Struct(fields)))
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