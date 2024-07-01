use std::{collections::HashMap, fs::File};

use cranelift::{
    codegen::{
        ir::{types::I64, AbiParam, Function, Signature, UserFuncName},
        isa,
        settings::{self, Configurable},
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::*,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use crate::{
    ast::{Literal, UnaryOperator},
    types::{self, AnnotatedIdent, Type, TypedExpr, TypedLiteral, TypedTopLevelDeclaration},
};

#[derive(Debug)]
pub enum CompileError {
    InvalidGlobal(TypedExpr),
    InvalidExternalType(Type),
}

struct CitrusValue {
    cranelift_value: Value,
    r#type: crate::types::Type,
    storage_location: StorageLocation,
}

enum StorageLocation {
    Stack,
    Heap,
}

pub fn compile(ast: Vec<TypedTopLevelDeclaration>) -> Result<(), CompileError> {
    let mut settings_builder = settings::builder();
    settings_builder.enable("is_pic").unwrap();
    let flags = settings::Flags::new(settings_builder);

    let isa_builder = isa::lookup(Triple::host()).expect("Unsupported architecture");
    let isa = isa_builder.finish(flags).unwrap();
    let call_conv = isa.default_call_conv();

    let obj_builder = ObjectBuilder::new(
        isa.clone(),
        "main",
        cranelift_module::default_libcall_names(),
    );
    let mut obj_module = ObjectModule::new(obj_builder.unwrap());

    let mut ctx = obj_module.make_context();

    // let mut scope = HashMap::new();

    //
    // compile_function(call_conv, I64);

    let mut signature = Signature::new(call_conv);

    signature.returns.push(AbiParam::new(I64));
    let main_fid = obj_module
        .declare_function("main", Linkage::Export, &signature)
        .unwrap();

    let mut main_function = Function::with_name_signature(UserFuncName::user(0, 0), signature);

    let mut main_function_builder_ctx = FunctionBuilderContext::new();
    let mut main_function_builder =
        FunctionBuilder::new(&mut main_function, &mut main_function_builder_ctx);
    let entry = main_function_builder.create_block();

    main_function_builder.switch_to_block(entry);
    // c stuff
    let mut signature = Signature::new(call_conv);

    signature.params.push(AbiParam::new(I64));
    let printfid = obj_module
        .declare_function("printnum", Linkage::Import, &signature)
        .unwrap();
    //--
    let mut signature = Signature::new(call_conv);

    signature.params.push(AbiParam::new(I64));
    signature.returns.push(AbiParam::new(I64));
    let mallocfid = obj_module
        .declare_function("malloc", Linkage::Import, &signature)
        .unwrap();

    //end c stuff
    //
    //
    // let zero = function_builder.ins().iconst(I64, i64::from(0));
    let mut functions = Vec::new();
    for declaration in ast {
        match declaration {
            TypedTopLevelDeclaration::Binding { lhs, rhs } => match rhs {
                TypedExpr::Literal(
                    _,
                    TypedLiteral::Function {
                        args,
                        body,
                        ret_type,
                    },
                ) => {
                    functions.push((args, body, ret_type, false));
                }
                expr => {
                    let val = compile_global(expr, &mut main_function_builder)?;
                    // val.storage_location = StorageLocation::Heap;
                    // match val.r#type {
                    //     types::Type::Int => ,
                    //     types::Type::Float => todo!(),
                    //     types::Type::Bool => todo!(),
                    //     types::Type::Unit => todo!(),
                    //     types::Type::Enum(_) => todo!(),
                    //     types::Type::Array(_) => todo!(),
                    //     types::Type::Function(_, _) => todo!(),
                    //     types::Type::Struct(_) => todo!(),
                    // }
                }
            },
            TypedTopLevelDeclaration::Extern(AnnotatedIdent { name, r#type }) => {
                if let Type::Function(args, ret) = r#type {
                    todo!()
                    // functions.push((args, ));
                } else {
                    return Err(CompileError::InvalidExternalType(r#type));
                }
            }
        };
    }
    let eight = main_function_builder.ins().iconst(I64, i64::from(8));
    let val = main_function_builder.ins().iconst(I64, i64::from(110));

    let malloc = obj_module.declare_func_in_func(mallocfid, main_function_builder.func);
    let ptr = main_function_builder.ins().call(malloc, &[eight]);
    let recv = main_function_builder.inst_results(ptr)[0];

    main_function_builder
        .ins()
        .store(MemFlags::new(), val, recv, 0);
    let num = main_function_builder
        .ins()
        .load(I64, MemFlags::new(), recv, 0);

    let printnum = obj_module.declare_func_in_func(printfid, main_function_builder.func);
    main_function_builder.ins().call(printnum, &[num]);

    main_function_builder.ins().return_(&[val]);
    main_function_builder.seal_block(entry);
    main_function_builder.finalize();

    ctx.func = main_function;
    obj_module.define_function(main_fid, &mut ctx).unwrap();

    ctx.clear();

    let res = obj_module.finish();

    let out = File::create("./main.o").unwrap();

    res.object.write_stream(out).unwrap();
    Ok(())
}

fn compile_global(
    expr: TypedExpr,
    builder: &mut FunctionBuilder,
) -> Result<CitrusValue, CompileError> {
    match expr {
        TypedExpr::FunctionCall(_, _, _)
        | TypedExpr::UnaryOp {
            r#type: _,
            op: UnaryOperator::Exclave,
            target: _,
        }
        | TypedExpr::Binding {
            r#type: _,
            lhs: _,
            rhs: _,
            local: _,
        }
        | TypedExpr::Literal(
            _,
            TypedLiteral::Function {
                args: _,
                body: _,
                ret_type: _,
            },
        )
        | TypedExpr::Mutate {
            r#type: _,
            lhs: _,
            rhs: _,
        }
        | TypedExpr::IfElse {
            r#type: _,
            condition: _,
            then: _,
            r#else: _,
        } => Err(CompileError::InvalidGlobal(expr)),
        TypedExpr::BinaryOp {
            r#type,
            lhs,
            op,
            rhs,
        } => {
            let lhs = compile_global(*lhs, builder)?.cranelift_value;
            let rhs = compile_global(*rhs, builder)?.cranelift_value;
            let val = match op {
                crate::ast::BinaryOperator::Add => builder.ins().iadd(lhs, rhs),
                crate::ast::BinaryOperator::Subtract => builder.ins().isub(lhs, rhs),
                crate::ast::BinaryOperator::Multiply => builder.ins().imul(lhs, rhs),
                crate::ast::BinaryOperator::Divide => builder.ins().sdiv(lhs, rhs),
                crate::ast::BinaryOperator::Power => todo!(),
                // crate::ast::BinaryOperator::Semicolon => todo!(),
                crate::ast::BinaryOperator::Gt => {
                    builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                }

                crate::ast::BinaryOperator::Lt => {
                    builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                }
                crate::ast::BinaryOperator::Gte => {
                    builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                }
                crate::ast::BinaryOperator::Lte => {
                    builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                }
            };
            Ok(CitrusValue {
                cranelift_value: val,
                r#type,
                storage_location: StorageLocation::Heap,
            })
        }
        TypedExpr::Literal(_, _) => todo!(),
        TypedExpr::Ident(_, _) => todo!(),
        TypedExpr::UnaryOp { r#type, op, target } => todo!(),
    }
}

fn compile_function(call_conv: isa::CallConv, ptr_ty: Type) -> Function {
    todo!()
}

fn compile_expr(
    expr: crate::types::TypedExpr,
    mut builder: &mut FunctionBuilder,
) -> Result<CitrusValue, CompileError> {
    match expr {
        crate::types::TypedExpr::BinaryOp {
            r#type,
            lhs,
            op,
            rhs,
        } => {
            let lhs = compile_expr(*lhs, &mut builder)?;
            let rhs = compile_expr(*rhs, &mut builder)?;

            match op {
                crate::ast::BinaryOperator::Add => todo!(),
                crate::ast::BinaryOperator::Subtract => todo!(),
                crate::ast::BinaryOperator::Multiply => todo!(),
                crate::ast::BinaryOperator::Divide => todo!(),
                crate::ast::BinaryOperator::Power => todo!(),
                crate::ast::BinaryOperator::Gt => todo!(),
                crate::ast::BinaryOperator::Lt => todo!(),
                crate::ast::BinaryOperator::Gte => todo!(),
                crate::ast::BinaryOperator::Lte => todo!(),
            }
        }
        crate::types::TypedExpr::Literal(_, _) => todo!(),
        crate::types::TypedExpr::Ident(_, _) => todo!(),
        crate::types::TypedExpr::FunctionCall(_, _, _) => todo!(),
        crate::types::TypedExpr::Binding {
            r#type,
            lhs,
            rhs,
            local,
        } => todo!(),
        crate::types::TypedExpr::IfElse {
            r#type,
            condition,
            then,
            r#else,
        } => todo!(),
        crate::types::TypedExpr::UnaryOp { r#type, op, target } => todo!(),
        crate::types::TypedExpr::Mutate { r#type, lhs, rhs } => todo!(),
    }
}
