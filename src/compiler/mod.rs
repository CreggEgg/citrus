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
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
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
    ExpectedValue(CitrusValue),
    ModuleError(cranelift_module::ModuleError),
}

#[derive(Clone, Debug)]
enum CitrusValue {
    Value {
        cranelift_value: Value,
        r#type: crate::types::Type,
        storage_location: StorageLocation,
    },
    Global {
        data_id: DataId,
        r#type: crate::types::Type,
    },
    Function {
        function: FuncId,
        r#type: crate::types::Type,
    },
}

impl CitrusValue {
    fn value(&self) -> Result<&Value, CompileError> {
        if let CitrusValue::Value {
            cranelift_value,
            r#type: _,
            storage_location: _,
        } = self
        {
            Ok(cranelift_value)
        } else {
            Err(CompileError::ExpectedValue(self.clone()))
        }
    }

    fn r#type(&self) -> &Type {
        match self {
            CitrusValue::Value {
                cranelift_value,
                r#type,
                storage_location,
            } => r#type,
            CitrusValue::Global { data_id, r#type } => r#type,
            CitrusValue::Function { function, r#type } => r#type,
        }
    }
}

#[derive(Clone, Debug)]
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

    signature.params.push(AbiParam::new(I64));
    let freefid = obj_module
        .declare_function("free", Linkage::Import, &signature)
        .unwrap();

    //end c stuff
    //
    //
    let zero = main_function_builder.ins().iconst(I64, i64::from(0));

    let eight = main_function_builder.ins().iconst(I64, i64::from(8));
    let val = main_function_builder.ins().iconst(I64, i64::from(110));

    let malloc = obj_module.declare_func_in_func(mallocfid, main_function_builder.func);
    let free = obj_module.declare_func_in_func(mallocfid, main_function_builder.func);

    // main_function_builder
    //     .ins()
    //     .store(MemFlags::new(), val, recv, 0);
    // let num = main_function_builder
    //     .ins()
    //     .load(I64, MemFlags::new(), recv, 0);

    let printnum = obj_module.declare_func_in_func(printfid, main_function_builder.func);
    // main_function_builder.ins().call(printnum, &[zero]);

    let mut scope = HashMap::new();
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
                    let size = main_function_builder
                        .ins()
                        .iconst(I64, get_size_bytes(val.r#type()));
                    let ret = main_function_builder.ins().call(malloc, &[size]);
                    let ptr = main_function_builder.inst_results(ret)[0];
                    main_function_builder
                        .ins()
                        .store(MemFlags::new(), *val.value()?, ptr, 0);
                    main_function_builder.ins().call(printnum, &[ptr]);
                    let global = obj_module
                        .declare_data(&lhs, Linkage::Local, true, false)
                        .map_err(|er| CompileError::ModuleError(er))?;
                    let mut data = DataDescription::new();
                    data.define_zeroinit(8);
                    let _ = obj_module
                        .define_data(global, &data)
                        .map_err(|er| CompileError::ModuleError(er))?;
                    let global_value_ref =
                        obj_module.declare_data_in_func(global, main_function_builder.func);
                    let global_ptr = main_function_builder
                        .ins()
                        .global_value(I64, global_value_ref);
                    main_function_builder.ins().call(printnum, &[global_ptr]);
                    main_function_builder
                        .ins()
                        .store(MemFlags::new(), ptr, global_ptr, 0);

                    scope.insert(
                        lhs,
                        CitrusValue::Global {
                            data_id: global,
                            r#type: val.r#type().clone(),
                        },
                    );

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
                if let Type::Function(args, ret) = &r#type {
                    let mut signature = Signature::new(call_conv);

                    for _arg in args {
                        signature.params.push(AbiParam::new(I64));
                    }
                    signature.returns.push(AbiParam::new(I64));
                    let fid = obj_module
                        .declare_function(&name, Linkage::Import, &signature)
                        .unwrap();
                    scope.insert(
                        name,
                        CitrusValue::Function {
                            function: fid,
                            r#type,
                        },
                    );
                    // todo!()
                    // functions.push((args, ));
                } else {
                    return Err(CompileError::InvalidExternalType(r#type));
                }
            }
        };
    }
    for (name, value) in scope.iter() {
        if let CitrusValue::Global { data_id, r#type } = value {
            let global_value_ref =
                obj_module.declare_data_in_func(*data_id, main_function_builder.func);
            let global_ptr = main_function_builder
                .ins()
                .global_value(I64, global_value_ref);

            main_function_builder.ins().call(printnum, &[global_ptr]);

            let heap_ptr = main_function_builder
                .ins()
                .load(I64, MemFlags::new(), global_ptr, 0);
            main_function_builder.ins().call(printnum, &[heap_ptr]);

            let value = main_function_builder
                .ins()
                .load(I64, MemFlags::new(), heap_ptr, 0);
            main_function_builder.ins().call(printnum, &[value]);
        }
    }
    main_function_builder.ins().return_(&[zero]);
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
            let lhs = compile_global(*lhs, builder)?.value().cloned()?;
            let rhs = compile_global(*rhs, builder)?.value().cloned()?;
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
            Ok(CitrusValue::Value {
                cranelift_value: val,
                r#type,
                storage_location: StorageLocation::Heap,
            })
        }
        TypedExpr::Literal(r#type, literal) => match literal {
            TypedLiteral::Int(int) => Ok(CitrusValue::Value {
                cranelift_value: builder.ins().iconst(I64, int as i64),
                r#type,
                storage_location: StorageLocation::Heap,
            }),
            TypedLiteral::String(_) => todo!(),
            TypedLiteral::Function {
                args,
                body,
                ret_type,
            } => unreachable!(),
        },
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

fn get_size_bytes(r#type: &Type) -> i64 {
    match r#type {
        Type::Int => 8,
        Type::Float => 8,
        Type::Bool => 1,
        Type::Unit => 0,
        Type::Enum(_) => 8, // right now enums are unable to contain values and so this is just an
        // ordinal value
        Type::Array(ty, len) => get_size_bytes(ty) * len,
        Type::Struct(fields) => fields.values().map(|ty| get_size_bytes(ty)).sum(),
        Type::Function(_, _) => unreachable!(), //the compiler should convert functions to
                                                //cranelift ir functions and not values by this point
    }
}
