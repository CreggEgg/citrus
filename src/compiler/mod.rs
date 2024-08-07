use std::{
    collections::HashMap,
    fs::{self, File},
    ops::Index,
    path::{Path, PathBuf},
    process::Command,
};

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
use cranelift_object::{object::write::Object, ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use crate::{
    ast::{self, UnaryOperator},
    compiler,
    types::{AnnotatedIdent, Type, TypedExpr, TypedLiteral, TypedTopLevelDeclaration},
};

#[derive(Debug)]
pub enum CompileError {
    InvalidGlobal(TypedExpr),
    InvalidExternalType(Type),
    ModuleError(cranelift_module::ModuleError),
    UndefinedValue(String),
    CallOnNonFunction(Type),
    InvalidHeapValue(CitrusValue),
    GccError(String),
}

#[derive(Clone, Debug)]
struct SystemFunctions {
    malloc: FuncId,
    free: FuncId,
    copy: FuncId,
}

#[derive(Clone, Debug)]
pub enum CitrusValue {
    Value {
        cranelift_value: Value,
        r#type: crate::types::Type,
        heap: bool,
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
    fn value(
        &self,
        builder: &mut FunctionBuilder,
        obj_module: &mut ObjectModule,
    ) -> Result<Value, CompileError> {
        //if let CitrusValue::Value {
        //cranelift_value,
        //r#type: _,
        //} = self
        //{
        //Ok(cranelift_value)
        //} else {
        //Err(CompileError::ExpectedValue(self.clone()))
        //}
        match self {
            CitrusValue::Value {
                cranelift_value, ..
            } => Ok(*cranelift_value),
            CitrusValue::Global { data_id, .. } => {
                let global_value_ref = obj_module.declare_data_in_func(*data_id, builder.func);
                let global_ptr = builder.ins().global_value(I64, global_value_ref);
                let value = builder.ins().load(I64, MemFlags::new(), global_ptr, 0);
                Ok(value)
            }
            CitrusValue::Function { .. } => todo!(),
        }
    }

    fn r#type(&self) -> &Type {
        match self {
            CitrusValue::Value { r#type, .. } => r#type,
            CitrusValue::Global { r#type, .. } => r#type,
            CitrusValue::Function { r#type, .. } => r#type,
        }
    }
}

pub fn build_dir() -> PathBuf {
    let out_folder = Path::new("./out");
    let _ = fs::create_dir(out_folder);
    let tmp_folder = Path::new("./tmp");
    let _ = fs::create_dir(tmp_folder);

    let mut core_path = tmp_folder.canonicalize().unwrap();
    core_path.push("core.c");
    fs::write(&core_path, include_str!("../../core.c")).unwrap();

    let citrus_files = fs::read_dir("./")
        .unwrap()
        .into_iter()
        .filter_map(|file| {
            if let Ok(file) = file {
                if file.path().is_file()
                    && file
                        .path()
                        .extension()
                        .map(|ex| ex == "ct")
                        .unwrap_or(false)
                {
                    // && file.path().ends_with(".ct") {
                    Some(file.path())
                } else {
                    None
                }
            } else {
                None
            }
        })
        // .map(|file| file.unwrap().path())
        .collect::<Vec<_>>();

    for file in citrus_files.clone() {
        println!("{}.o", file.to_str().unwrap());
        let mut obj_path = tmp_folder.canonicalize().unwrap();
        obj_path.push(format!("{}.o", file.file_stem().unwrap().to_str().unwrap()));
        let out = File::create(obj_path).unwrap();
        let ast = ast::parser::parse(&fs::read_to_string(file.clone()).unwrap()).unwrap();
        let typed = crate::types::inference::type_file(ast::File { declarations: ast }).unwrap();
        compiler::compile(
            file.file_stem().unwrap().to_str().unwrap().to_string(),
            typed.declarations,
            citrus_files
                .iter()
                .map(|file| file.file_stem().unwrap().to_str().unwrap())
                .collect(),
        )
        .unwrap()
        .write_stream(out)
        .unwrap();
    }
    let out = link(
        citrus_files
            .iter()
            .map(|path| {
                format!(
                    "./tmp/{}",
                    path.file_stem().unwrap().to_str().unwrap().to_string()
                )
            })
            .collect(),
        core_path.to_str().unwrap().to_string(),
    )
    .unwrap();

    #[cfg(debug_assertions)]
    dbg!(Path::new("./").canonicalize().unwrap());
    fs::remove_dir_all("./tmp").unwrap();
    out
}

pub fn compile(
    file_name: String,
    ast: Vec<TypedTopLevelDeclaration>,
    modules: Vec<&str>,
) -> Result<Object<'static>, CompileError> {
    println!("129");
    let mut settings_builder = settings::builder();
    settings_builder.enable("is_pic").unwrap();
    let flags = settings::Flags::new(settings_builder);

    let isa_builder = isa::lookup(Triple::host()).expect("Unsupported architecture");
    let isa = isa_builder.finish(flags).unwrap();
    let call_conv = isa.default_call_conv();

    let obj_builder = ObjectBuilder::new(
        isa.clone(),
        file_name.clone(), //"main",
        cranelift_module::default_libcall_names(),
    );
    let mut obj_module = ObjectModule::new(obj_builder.unwrap());

    let mut ctx = obj_module.make_context();

    // let mut scope = HashMap::new();

    //
    // compile_function(call_conv, I64);

    println!("152");
    let signature = Signature::new(call_conv);

    // signature.returns.push(AbiParam::new(I64));
    let main_fid = obj_module
        .declare_function(
            &if file_name == "main" {
                "main".to_string()
            } else {
                format!("_init_{}", file_name)
            },
            Linkage::Export,
            &signature,
        )
        .unwrap();

    let mut main_function = Function::with_name_signature(UserFuncName::user(0, 0), signature);

    let mut main_function_builder_ctx = FunctionBuilderContext::new();
    let mut main_function_builder =
        FunctionBuilder::new(&mut main_function, &mut main_function_builder_ctx);
    let entry = main_function_builder.create_block();

    main_function_builder.switch_to_block(entry);
    // c stuff
    let mut signature = Signature::new(call_conv);
    println!("170");

    signature.params.push(AbiParam::new(I64));
    //--
    let mut signature = Signature::new(call_conv);

    signature.params.push(AbiParam::new(I64));
    signature.returns.push(AbiParam::new(I64));
    let mallocfid = obj_module
        .declare_function("malloc", Linkage::Import, &signature)
        .unwrap();

    let freefid = obj_module
        .declare_function("free", Linkage::Import, &signature)
        .unwrap();
    signature.params.push(AbiParam::new(I64));
    signature.params.push(AbiParam::new(I64));
    let copyfid = obj_module
        .declare_function("memcpy", Linkage::Import, &signature)
        .unwrap();

    println!("193");
    //end c stuff
    //
    //
    // let zero = main_function_builder.ins().iconst(I64, i64::from(0));

    // let eight = main_function_builder.ins().iconst(I64, i64::from(8));
    // let val = main_function_builder.ins().iconst(I64, i64::from(110));

    // let malloc = obj_module.declare_func_in_func(mallocfid, main_function_builder.func);
    // let free = obj_module.declare_func_in_func(mallocfid, main_function_builder.func);

    // main_function_builder
    //     .ins()
    //     .store(MemFlags::new(), val, recv, 0);
    // let num = main_function_builder
    //     .ins()
    //     .load(I64, MemFlags::new(), recv, 0);

    // main_function_builder.ins().call(printnum, &[zero]);
    let mut scope = HashMap::new();
    let mut functions = Vec::new();
    println!("218");
    for declaration in ast {
        match declaration {
            TypedTopLevelDeclaration::Binding { public, lhs, rhs } => match rhs {
                TypedExpr::Literal(
                    _,
                    TypedLiteral::Function {
                        args,
                        body,
                        ret_type,
                    },
                ) => {
                    functions.push((public, lhs, args, body, ret_type));
                }
                expr => {
                    let val = compile_global(expr, &mut main_function_builder, &mut obj_module)?;
                    // let size = main_function_builder
                    //     .ins()
                    //     .iconst(I64, get_size_bytes(val.r#type()));
                    // let ret = main_function_builder.ins().call(malloc, &[size]);
                    // let ptr = main_function_builder.inst_results(ret)[0];
                    // main_function_builder
                    //     .ins()
                    //     .store(MemFlags::new(), *val.value()?, ptr, 0);
                    // main_function_builder.ins().call(printnum, &[ptr]);
                    let global = obj_module
                        .declare_data(
                            &lhs,
                            if public {
                                Linkage::Export
                            } else {
                                Linkage::Local
                            },
                            true,
                            false,
                        )
                        .map_err(CompileError::ModuleError)?;
                    let mut data = DataDescription::new();
                    data.define_zeroinit(get_size_bytes(val.r#type()) as usize);
                    obj_module
                        .define_data(global, &data)
                        .map_err(CompileError::ModuleError)?;
                    let global_value_ref =
                        obj_module.declare_data_in_func(global, main_function_builder.func);
                    let global_ptr = main_function_builder
                        .ins()
                        .global_value(I64, global_value_ref);
                    let underlying = val.value(&mut main_function_builder, &mut obj_module)?;
                    main_function_builder
                        .ins()
                        .store(MemFlags::new(), underlying, global_ptr, 0);

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
                if let Type::Function(args, _) = &r#type {
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
    // for (name, value) in scope.iter() {
    //     if let CitrusValue::Global { data_id, r#type } = value {
    //         main_function_builder.ins().call(printnum, &[global_ptr]);
    //
    //         // let heap_ptr = main_function_builder
    //         //     .ins()
    //         //     .load(I64, MemFlags::new(), global_ptr, 0);
    //         // main_function_builder.ins().call(printnum, &[heap_ptr]);
    //         //
    //         let global_value_ref =
    //             obj_module.declare_data_in_func(*data_id, main_function_builder.func);
    //         let global_ptr = main_function_builder
    //             .ins()
    //             .global_value(I64, global_value_ref);
    //         let value = main_function_builder
    //             .ins()
    //             .load(I64, MemFlags::new(), global_ptr, 0);
    //         main_function_builder.ins().call(printnum, &[value]);
    //     }
    // }
    let mut main_fn = Option::None;

    for (i, (public, name, args, body, ret)) in functions.iter().enumerate() {
        let mut signature = Signature::new(call_conv);
        for _ in args {
            signature.params.push(AbiParam::new(I64));
        }
        if ret.is_some() {
            signature.returns.push(AbiParam::new(I64));
        }
        let mut name = name.clone();
        if name == "main" {
            name = "_main".to_string();
        };
        let fid = obj_module
            .declare_function(
                &name,
                if *public {
                    Linkage::Export
                } else {
                    Linkage::Local
                },
                &signature,
            )
            .unwrap();
        scope.insert(
            name.clone(),
            CitrusValue::Function {
                function: fid,
                r#type: Type::Function(
                    args.iter().map(|arg| arg.r#type.clone()).collect(),
                    Box::new(ret.clone().unwrap_or(Type::Unit)),
                ),
            },
        );
        if name == "_main" {
            main_fn = Option::Some(fid);
        };

        let mut function =
            Function::with_name_signature(UserFuncName::user(0, (i + 1) as u32), signature);

        let mut function_builder_ctx = FunctionBuilderContext::new();
        let mut function_builder = FunctionBuilder::new(&mut function, &mut function_builder_ctx);
        let entry = function_builder.create_block();

        function_builder.append_block_params_for_function_params(entry);
        let params = function_builder.block_params(entry);
        for (i, arg) in args.iter().enumerate() {
            scope.insert(
                arg.name.clone(),
                CitrusValue::Value {
                    cranelift_value: params[i],
                    r#type: arg.r#type.clone(),
                    heap: false,
                },
            );
        }
        #[cfg(debug_assertions)]
        dbg!(&scope);

        function_builder.switch_to_block(entry);

        let block_return = [compile_block(
            body.clone(),
            &mut function_builder,
            &mut obj_module,
            scope.clone(),
            SystemFunctions {
                malloc: mallocfid,
                free: freefid,
                copy: copyfid,
            },
        )?
        .value(&mut function_builder, &mut obj_module)?];
        function_builder
            .ins()
            .return_(if ret.is_some() { &block_return } else { &[] });

        function_builder.seal_block(entry);
        function_builder.finalize();

        #[cfg(debug_assertions)]
        dbg!(name);
        #[cfg(debug_assertions)]
        dbg!(&function);
        ctx.func = function;
        obj_module.define_function(fid, &mut ctx).unwrap();

        ctx.clear();
    }
    if file_name == "main" {
        let main = obj_module.declare_func_in_func(
            main_fn.expect("No \"main\" function defined"),
            main_function_builder.func,
        );
        for file in modules {
            let signature = Signature::new(call_conv);
            if file != "main" {
                let init = obj_module
                    .declare_function(&format!("_init_{}", file), Linkage::Import, &signature)
                    .unwrap();
                let init = obj_module.declare_func_in_func(init, main_function_builder.func);
                main_function_builder.ins().call(init, &[]);
            }
        }
        main_function_builder.ins().call(main, &[]);
    }
    // let ret = main_function_builder.inst_results(ret)[0];

    main_function_builder.ins().return_(&[]);
    main_function_builder.seal_block(entry);
    main_function_builder.finalize();

    ctx.func = main_function;
    obj_module.define_function(main_fid, &mut ctx).unwrap();

    ctx.clear();

    let res = obj_module.finish();

    Ok(res.object)
}

pub fn link(files: Vec<String>, core_path: String) -> Result<PathBuf, CompileError> {
    let mut path = Path::new("./").canonicalize().unwrap();
    path.push(Path::new("out/main"));
    let mut args: Vec<String> = Vec::new();
    for file in &files {
        let name = format!("{}.o", file);
        args.push(name);
    }
    args.push(core_path);
    args.push("-o".into());
    args.push(path.to_str().unwrap().into());
    let gcc_out = Command::new("gcc")
        .current_dir(Path::new("./").canonicalize().unwrap())
        .args(args)
        .output()
        .expect("Failed to link");
    if !gcc_out.status.success() {
        let stderr = gcc_out.stderr.clone();
        return Err(CompileError::GccError(
            String::from_utf8_lossy(&stderr).into(),
        ));
    }
    Ok(path)
}

fn compile_block(
    body: Vec<TypedExpr>,
    function_builder: &mut FunctionBuilder<'_>,
    obj_module: &mut ObjectModule,
    mut scope: HashMap<String, CitrusValue>,
    functions: SystemFunctions,
) -> Result<CitrusValue, CompileError> {
    let mut ret = None;
    let zero = function_builder.ins().iconst(I64, 0);
    for (i, expr) in body.iter().enumerate() {
        // println!("Compiling expression");
        let (val, newscope) = compile_expr(
            expr.clone(),
            function_builder,
            scope.clone(),
            obj_module,
            functions.clone(),
        )?;
        scope = newscope;

        if i == body.len() - 1 {
            ret = Some(move_to_heap(
                val,
                functions.clone(),
                function_builder,
                obj_module,
            )?);
        }
    }
    let free = obj_module.declare_func_in_func(functions.free, function_builder.func);
    for (name, value) in scope {
        if let CitrusValue::Value {
            cranelift_value,
            heap: true,
            ..
        } = value
        {
            println!("freeing: {name}, {:?}", value);
            function_builder.ins().call(free, &[cranelift_value]);
        }
    }
    Ok(ret.unwrap_or(CitrusValue::Value {
        cranelift_value: zero,
        r#type: Type::Unit,
        heap: false,
    }))
}

fn move_to_heap(
    val: CitrusValue,
    functions: SystemFunctions,
    builder: &mut FunctionBuilder,
    obj_module: &mut ObjectModule,
) -> Result<CitrusValue, CompileError> {
    if let CitrusValue::Value {
        cranelift_value,
        r#type,
        heap,
    } = val
    {
        if !heap {
            let malloc = obj_module.declare_func_in_func(functions.malloc, builder.func);
            let copy = obj_module.declare_func_in_func(functions.copy, builder.func);
            let (heap, value) = match r#type {
                Type::Int | Type::Bool => (false, cranelift_value),
                Type::Array(_) => {
                    let len = builder.ins().load(I64, MemFlags::new(), cranelift_value, 0);
                    let eight = builder.ins().iconst(I64, 8);
                    let len_bytes = builder.ins().imul(len, eight);
                    let size = builder.ins().iadd(len_bytes, eight);
                    let ptr_ins = builder.ins().call(malloc, &[size]);
                    let heap_ptr = builder.inst_results(ptr_ins)[0];

                    builder.ins().call(copy, &[heap_ptr, cranelift_value, size]);

                    (true, heap_ptr)
                }
                Type::Struct(ref pairs) => {
                    let len = builder.ins().iconst(I64, (pairs.len() - 1) as i64);
                    let eight = builder.ins().iconst(I64, 8);
                    let len_bytes = builder.ins().imul(len, eight);
                    let size = builder.ins().iadd(len_bytes, eight);
                    let ptr_ins = builder.ins().call(malloc, &[size]);
                    let heap_ptr = builder.inst_results(ptr_ins)[0];

                    builder.ins().call(copy, &[heap_ptr, cranelift_value, size]);

                    (true, heap_ptr)
                }

                Type::Unit => todo!(),
                Type::Float => todo!(),
                Type::Enum(_) => todo!(),
                Type::Function(_, _) => todo!(),
            };
            Ok(CitrusValue::Value {
                cranelift_value: value,
                r#type,
                heap,
            })
        } else {
            Ok(CitrusValue::Value {
                cranelift_value,
                r#type: r#type.clone(),
                heap: !matches!(r#type, Type::Int | Type::Bool),
            })
        }
    } else {
        Err(CompileError::InvalidHeapValue(val))
    }
}

fn compile_global(
    expr: TypedExpr,
    builder: &mut FunctionBuilder,
    obj_module: &mut ObjectModule,
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
            let lhs = compile_global(*lhs, builder, obj_module)?.value(builder, obj_module)?;
            let rhs = compile_global(*rhs, builder, obj_module)?.value(builder, obj_module)?;
            let val = match op {
                crate::ast::BinaryOperator::Add => builder.ins().iadd(lhs, rhs),
                crate::ast::BinaryOperator::Subtract => builder.ins().isub(lhs, rhs),
                crate::ast::BinaryOperator::Multiply => builder.ins().imul(lhs, rhs),
                crate::ast::BinaryOperator::Divide => builder.ins().sdiv(lhs, rhs),
                crate::ast::BinaryOperator::Modulo => builder.ins().srem(lhs, rhs),
                // crate::ast::BinaryOperator::Power => todo!(),
                // crate::ast::BinaryOperator::Semicolon => todo!(),
                crate::ast::BinaryOperator::Gt => cmp(IntCC::SignedGreaterThan, lhs, rhs, builder),

                crate::ast::BinaryOperator::Lt => cmp(IntCC::SignedLessThan, lhs, rhs, builder),
                crate::ast::BinaryOperator::Gte => {
                    cmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs, builder)
                }
                crate::ast::BinaryOperator::Lte => {
                    cmp(IntCC::SignedLessThanOrEqual, lhs, rhs, builder)
                }
                crate::ast::BinaryOperator::Eq => cmp(IntCC::Equal, lhs, rhs, builder),
                crate::ast::BinaryOperator::And => builder.ins().band(lhs, rhs),
                crate::ast::BinaryOperator::Or => builder.ins().bor(lhs, rhs),
            };
            Ok(CitrusValue::Value {
                cranelift_value: val,
                r#type,
                heap: false,
            })
        }
        TypedExpr::Literal(r#type, literal) => match literal {
            TypedLiteral::Int(int) => Ok(CitrusValue::Value {
                cranelift_value: builder.ins().iconst(I64, int as i64),
                r#type,
                heap: false, // storage_location: StorageLocation::Heap,
            }),

            TypedLiteral::Bool(x) => Ok(CitrusValue::Value {
                cranelift_value: builder.ins().iconst(I64, if x { 1 } else { 0 }),
                r#type,
                heap: false, // storage_location: StorageLocation::Heap,
            }),
            TypedLiteral::String(_) => todo!(),
            TypedLiteral::Function { .. } => unreachable!(),
            TypedLiteral::Array(_) => todo!(),
            TypedLiteral::Struct(_, _, _) => todo!(),
            TypedLiteral::Unit => Ok(CitrusValue::Value {
                cranelift_value: builder.ins().iconst(I64, 0),
                r#type,
                heap: false, // storage_location: StorageLocation::Heap,
            }),
            TypedLiteral::Float(x) => Ok(CitrusValue::Value {
                cranelift_value: {
                    let floating = builder.ins().f64const(x);
                    builder.ins().bitcast(I64, MemFlags::new(), floating)
                },
                r#type,
                heap: false, // storage_location: StorageLocation::Heap,
            }),
        },
        TypedExpr::Ident(_, _) => todo!(),
        TypedExpr::UnaryOp { .. } => todo!(),
        TypedExpr::Access(_, _, _) => todo!(),
    }
}

fn compile_expr(
    expr: crate::types::TypedExpr,
    builder: &mut FunctionBuilder,
    mut scope: HashMap<String, CitrusValue>,
    obj_module: &mut ObjectModule,
    functions: SystemFunctions,
) -> Result<(CitrusValue, HashMap<String, CitrusValue>), CompileError> {
    match expr {
        crate::types::TypedExpr::BinaryOp {
            r#type,
            lhs,
            op,
            rhs,
        } => {
            let lhs = compile_expr(*lhs, builder, scope.clone(), obj_module, functions.clone())?
                .0
                .value(builder, obj_module)?;
            let rhs = compile_expr(*rhs, builder, scope.clone(), obj_module, functions)?
                .0
                .value(builder, obj_module)?;

            Ok((
                match op {
                    crate::ast::BinaryOperator::Add => CitrusValue::Value {
                        cranelift_value: builder.ins().iadd(lhs, rhs),
                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::Subtract => CitrusValue::Value {
                        cranelift_value: builder.ins().isub(lhs, rhs),
                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::Multiply => CitrusValue::Value {
                        cranelift_value: builder.ins().imul(lhs, rhs),
                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },

                    crate::ast::BinaryOperator::Divide => CitrusValue::Value {
                        cranelift_value: builder.ins().sdiv(lhs, rhs),
                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    // crate::ast::BinaryOperator::Power => todo!(),
                    crate::ast::BinaryOperator::Gt => CitrusValue::Value {
                        cranelift_value: cmp(IntCC::SignedGreaterThan, lhs, rhs, builder),
                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::Lt => CitrusValue::Value {
                        cranelift_value: cmp(IntCC::SignedLessThan, lhs, rhs, builder),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::Gte => CitrusValue::Value {
                        cranelift_value: cmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs, builder),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                                     //
                    },

                    crate::ast::BinaryOperator::Lte => CitrusValue::Value {
                        cranelift_value: cmp(IntCC::SignedLessThanOrEqual, lhs, rhs, builder),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::Eq => CitrusValue::Value {
                        cranelift_value: cmp(IntCC::Equal, lhs, rhs, builder),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::And => CitrusValue::Value {
                        cranelift_value: builder.ins().band(lhs, rhs),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                    crate::ast::BinaryOperator::Or => CitrusValue::Value {
                        cranelift_value: builder.ins().bor(lhs, rhs),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                                     //
                    },
                    crate::ast::BinaryOperator::Modulo => CitrusValue::Value {
                        cranelift_value: builder.ins().srem(lhs, rhs),

                        r#type,
                        heap: false, // storage_location: StorageLocation::Stack,
                    },
                },
                scope,
            ))
        }
        crate::types::TypedExpr::Literal(r#type, literal) => Ok((
            match literal {
                TypedLiteral::Int(x) => CitrusValue::Value {
                    cranelift_value: builder.ins().iconst(I64, x as i64),
                    r#type,
                    heap: false, // storage_location: StorageLocation::Stack,
                },
                TypedLiteral::String(string) => {
                    let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        64 * (string.len() as u32 + 1),
                    ));
                    let addr = builder.ins().stack_addr(I64, stack_slot, 0);
                    let len = builder.ins().iconst(I64, string.len() as i64);
                    builder.ins().store(MemFlags::new(), len, addr, 0);

                    for (i, ch) in string.chars().enumerate() {
                        let ch = builder.ins().iconst(I64, ch as i64);
                        builder
                            .ins()
                            .store(MemFlags::new(), ch, addr, (i + 1) as i32 * 64);
                    }
                    CitrusValue::Value {
                        cranelift_value: addr,
                        r#type: Type::Array(Box::new(Type::Int)),
                        heap: false,
                    }
                }
                TypedLiteral::Function { .. } => todo!(),
                TypedLiteral::Bool(value) => CitrusValue::Value {
                    cranelift_value: builder.ins().iconst(I64, if value { 1 } else { 0 }),
                    r#type: Type::Bool,
                    heap: false,
                },
                TypedLiteral::Array(vals) => {
                    let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        64 * (vals.len() as u32 + 1),
                    ));
                    let addr = builder.ins().stack_addr(I64, stack_slot, 0);
                    let len = builder.ins().iconst(I64, vals.len() as i64);
                    builder.ins().store(MemFlags::new(), len, addr, 0);

                    for (i, val) in vals.iter().enumerate() {
                        let val = compile_expr(
                            val.clone(),
                            builder,
                            scope.clone(),
                            obj_module,
                            functions.clone(),
                        )?
                        .0
                        .value(builder, obj_module)?;
                        builder
                            .ins()
                            .store(MemFlags::new(), val, addr, (i + 1) as i32 * 64);
                    }
                    CitrusValue::Value {
                        cranelift_value: addr,
                        r#type: Type::Array(Box::new(Type::Int)),
                        heap: false,
                    }
                }
                TypedLiteral::Struct(r#type, with, base) => {
                    let mut pairs: Vec<(String, CitrusValue)> = Vec::new();
                    if let Some(base) = base {
                        let base = compile_expr(
                            *base,
                            builder,
                            scope.clone(),
                            obj_module,
                            functions.clone(),
                        )?
                        .0;
                        if let CitrusValue::Value {
                            cranelift_value,
                            r#type: Type::Struct(base_pairs),
                            heap,
                        } = base
                        {
                            let mut base_pairs = base_pairs.iter().collect::<Vec<_>>();
                            base_pairs.sort_by(|a, b| a.0.cmp(b.0));
                            for (i, (key, value_type)) in base_pairs.iter().enumerate() {
                                let value_index = builder.ins().iconst(I64, i as i64);
                                let eight = builder.ins().iconst(I64, 8);

                                let value_offset = builder.ins().imul(value_index, eight);
                                let value_addr = builder.ins().iadd(value_offset, cranelift_value);
                                let value = builder.ins().load(I64, MemFlags::new(), value_addr, 0);
                                pairs.push((
                                    key.to_string(),
                                    CitrusValue::Value {
                                        cranelift_value: value,
                                        r#type: value_type.clone().clone(),
                                        heap: false,
                                    },
                                ))
                            }
                        };
                    }
                    for (k, v) in with.clone() {
                        let index = pairs
                            .iter()
                            .enumerate()
                            .find(|(_, (key, _))| **key == k)
                            .map(|(index, _)| index);
                        if let Some(Some(value)) = index.map(|index| pairs.get_mut(index)) {
                            value.1 = compile_expr(
                                v,
                                builder,
                                scope.clone(),
                                obj_module,
                                functions.clone(),
                            )?
                            .0;
                        } else {
                            pairs.push((
                                k.clone(),
                                compile_expr(
                                    v,
                                    builder,
                                    scope.clone(),
                                    obj_module,
                                    functions.clone(),
                                )?
                                .0,
                            ))
                        }
                    }
                    pairs.sort_by(|a, b| a.0.cmp(&b.0));
                    let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        64 * (pairs.len() as u32),
                    ));
                    let addr = builder.ins().stack_addr(I64, stack_slot, 0);

                    for (i, (_, val)) in pairs.iter().enumerate() {
                        let val = /* compile_expr(
                            val.clone(),
                            builder,
                            scope.clone(),
                            obj_module,
                            functions.clone(),
                        )?
                        .0 */
                        val
                        .value(builder, obj_module)?;
                        builder
                            .ins()
                            .store(MemFlags::new(), val, addr, (i) as i32 * 64);
                    }
                    CitrusValue::Value {
                        cranelift_value: addr,
                        r#type,
                        heap: false,
                    }
                }
                TypedLiteral::Unit => CitrusValue::Value {
                    cranelift_value: builder.ins().iconst(I64, 0),
                    r#type,
                    heap: false, // storage_location: StorageLocation::Stack,
                },
                TypedLiteral::Float(x) => CitrusValue::Value {
                    cranelift_value: {
                        let floating = builder.ins().f64const(x);
                        builder.ins().bitcast(I64, MemFlags::new(), floating)
                    },
                    r#type,
                    heap: false,
                },
            },
            scope,
        )),
        crate::types::TypedExpr::Ident(_, name) => scope
            .get(&name)
            .ok_or(CompileError::UndefinedValue(name))
            .cloned()
            .map(|val| (val, scope)),
        crate::types::TypedExpr::FunctionCall(_, name, args) => {
            let err = CompileError::UndefinedValue(name.clone());
            let scope = scope.clone();
            let func = scope/*TODO: get rid of this clone (rn i dont have lsp so id really rather not try to deal with this)*/.get(&name).ok_or(err)?;
            if let CitrusValue::Function { function, r#type } = func {
                let func = obj_module.declare_func_in_func(*function, builder.func);
                let args = args
                    .iter()
                    .map(|expr| {
                        compile_expr(
                            expr.clone(),
                            builder,
                            scope.clone(),
                            obj_module,
                            functions.clone(),
                        )?
                        .0
                        .value(builder, obj_module)
                    })
                    .collect::<Result<Vec<Value>, CompileError>>()?;
                let ret = builder.ins().call(func, &args);
                let ret = builder.inst_results(ret).first();
                Ok(match ret {
                    Some(ret) => {
                        if let Type::Function(_, r#type) = r#type {
                            (
                                CitrusValue::Value {
                                    cranelift_value: *ret,
                                    r#type: *r#type.clone(),
                                    heap: !matches!(**r#type, Type::Int | Type::Bool),
                                },
                                scope,
                            )
                        } else {
                            let unit = builder.ins().iconst(I64, 0);
                            (
                                CitrusValue::Value {
                                    cranelift_value: unit,
                                    r#type: Type::Unit,
                                    heap: false,
                                },
                                scope,
                            )
                        }
                    }
                    None => {
                        let unit = builder.ins().iconst(I64, 0);
                        (
                            CitrusValue::Value {
                                cranelift_value: unit,
                                r#type: Type::Unit,
                                heap: false,
                            },
                            scope,
                        )
                    }
                })
            } else {
                Err(CompileError::CallOnNonFunction(func.r#type().clone()))
            }
        }
        crate::types::TypedExpr::Binding { lhs, rhs, .. } => {
            let rhs = compile_expr(*rhs, builder, scope.clone(), obj_module, functions)?.0;
            scope.insert(lhs, rhs.clone());
            Ok((rhs, scope))
        }
        crate::types::TypedExpr::IfElse {
            r#type,
            condition,
            then,
            r#else,
        } => {
            let condition_value = compile_expr(
                *condition,
                builder,
                scope.clone(),
                obj_module,
                functions.clone(),
            )?
            .0
            .value(builder, obj_module)?;

            let then_block = builder.create_block();
            let else_block = builder.create_block();
            let merge_block = builder.create_block();

            builder.append_block_param(merge_block, I64);

            builder
                .ins()
                .brif(condition_value, then_block, &[], else_block, &[]);
            builder.switch_to_block(then_block);

            builder.seal_block(then_block);

            let then_return =
                compile_block(then, builder, obj_module, scope.clone(), functions.clone())?
                    .value(builder, obj_module)?;
            builder.ins().jump(merge_block, &[then_return]);
            //let else_return = compile_block(r#else, builder, else_block, obj_module, scope.clone())?;
            builder.switch_to_block(else_block);

            builder.seal_block(else_block);

            let r#else_return =
                compile_block(r#else, builder, obj_module, scope.clone(), functions)?
                    .value(builder, obj_module)?;
            builder.ins().jump(merge_block, &[else_return]);

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);

            let phi = builder.block_params(merge_block)[0];

            Ok((
                CitrusValue::Value {
                    cranelift_value: phi,
                    r#type,
                    heap: false,
                },
                scope,
            ))
        }
        crate::types::TypedExpr::UnaryOp { .. } => todo!(),
        crate::types::TypedExpr::Mutate {
            r#type: _,
            lhs,
            rhs,
        } => {
            let (new_value, _) = compile_expr(*rhs, builder, scope.clone(), obj_module, functions)?;
            if let Some(old) = scope.get_mut(&lhs) {
                *old = new_value.clone();
                Ok((new_value, scope))
            } else {
                Err(CompileError::UndefinedValue(lhs))
            }
        }
        TypedExpr::Access(r#type, r#struct, index) => {
            // let pairs =
            let r#struct = compile_expr(*r#struct, builder, scope.clone(), obj_module, functions)?
                .0
                .value(builder, obj_module)?;
            let index = builder.ins().iconst(I64, (index * 8) as i64);
            let ptr = builder.ins().iadd(r#struct, index);
            Ok((
                CitrusValue::Value {
                    cranelift_value: builder.ins().load(I64, MemFlags::new(), ptr, 0),
                    r#type,
                    heap: false,
                },
                scope,
            ))
        }
    }
}

fn get_size_bytes(r#type: &Type) -> i64 {
    match r#type {
        Type::Int => 8,
        Type::Float => 8,
        Type::Bool => 8,
        Type::Unit => 8,
        Type::Enum(_) => 8, // right now enums are unable to contain values and so this is just an
        // ordinal value
        Type::Array(_) => 8,
        Type::Struct(_) => 8, //fields.values().map(|ty| get_size_bytes(ty)).sum(),
        Type::Function(_, _) => unreachable!(), //the compiler should convert functions to
                               //cranelift ir functions and not values by this point
    }
}

fn cmp(cmp: IntCC, lhs: Value, rhs: Value, builder: &mut FunctionBuilder) -> Value {
    let as_i8 = builder.ins().icmp(cmp, lhs, rhs);
    builder.ins().sextend(I64, as_i8)
}
