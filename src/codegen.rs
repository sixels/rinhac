pub mod core;
pub mod runtime;
pub mod traits;
pub mod value;

use std::{cell::RefCell, rc::Rc};

use enumflags2::BitFlags;
use inkwell::values::{AnyValue, BasicMetadataValueEnum, BasicValueEnum, PointerValue};

use crate::{
    ast,
    codegen::value::{Closure, Enum, ValueType},
    compiler::{
        environment::{Capture, Function, ScopeNode, Variable},
        Compiler,
    },
};

use self::{
    core::CoreFunction,
    traits::{Codegen, CodegenValue, DerefValue},
    value::{closure::build_captures_struct, Primitive, Str, Tuple, Value, ValueTypeHint},
};

pub const FIRST_BLOCK_NAME: &str = "start";

impl<'ctx> CodegenValue<'ctx> for ast::Term {
    fn codegen_value(&self, compiler: &mut Compiler<'_, 'ctx>) -> Value<'ctx> {
        match self {
            ast::Term::Int(i) => i.codegen(compiler).into(),
            ast::Term::Bool(b) => b.codegen(compiler).into(),
            ast::Term::Binary(b) => b.codegen(compiler),
            ast::Term::Var(v) => match v.codegen(compiler) {
                Variable::Value(v) => v.build_deref(compiler),
                Variable::Constant(c) => c,
                Variable::Function(_) => todo!(),
            },
            ast::Term::Str(s) => s.codegen(compiler).into(),
            ast::Term::Call(c) => c.codegen(compiler).codegen_value(compiler),
            ast::Term::If(i) => i.codegen(compiler),

            ast::Term::Error(_) => todo!(),
            ast::Term::Function(_) => {
                unimplemented!()
            }
            ast::Term::Let(_) => todo!(),
            ast::Term::Print(p) => p.codegen(compiler).into(),
            ast::Term::First(f) => f.codegen(compiler),
            ast::Term::Second(s) => s.codegen(compiler),
            ast::Term::Tuple(t) => t.codegen(compiler).into(),
        }
    }
}

impl Codegen for ast::Int {
    type R<'ctx> = Primitive<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        Primitive::Int(compiler.context.i32_type().const_int(self.value as _, true))
    }
}

impl Codegen for ast::Bool {
    type R<'ctx> = Primitive<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        Primitive::Bool(
            compiler
                .context
                .bool_type()
                .const_int(self.value as _, false),
        )
    }
}

impl Codegen for ast::Tuple {
    type R<'ctx> = Tuple<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let first = self.first.codegen_value(compiler);
        let second = self.second.codegen_value(compiler);

        Tuple::build_new(compiler, first, second)
    }
}

impl Codegen for ast::First {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let val = self.value.codegen_value(compiler);
        let Value::Tuple(t) = val else {
            panic!("invalid operation")
        };

        t.unwrap_first(compiler)
    }
}

impl Codegen for ast::Second {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let val = self.value.codegen_value(compiler);
        let Value::Tuple(t) = val else {
            panic!("invalid operation")
        };

        t.unwrap_second(compiler)
    }
}

impl Codegen for ast::Str {
    type R<'ctx> = Str<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        if let Some(string) = compiler.strings.get(&self.value).copied() {
            return string;
        }

        let len_value = compiler
            .context
            .i32_type()
            .const_int(self.value.len() as u64, false);

        let global_string = compiler.builder.build_global_string_ptr(&self.value, "str");

        let str = Str::new(global_string.as_pointer_value(), len_value);
        compiler.strings.insert(self.value.clone(), str);

        str
    }
}

impl Codegen for ast::Var {
    type R<'ctx> = Variable<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let var = compiler
            .scope
            .find_variable(&self.text)
            .or_else(|| compiler.scope.find_captured_variable(&self.text))
            .unwrap_or_else(|| panic!("Variable {} not defined", self.text));

        var.clone()
    }
}

impl Codegen for ast::Binary {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let lhs_value = self.lhs.codegen_value(compiler);
        let rhs_value = self.rhs.codegen_value(compiler);

        let result = match (lhs_value, rhs_value) {
            (Value::Primitive(pl), Value::Primitive(pr)) => {
                Primitive::build_arith(compiler, pl, pr, self.op)
                    .unwrap()
                    .into()
            }
            (Value::Primitive(primitive), Value::Boxed(boxed))
            | (Value::Boxed(boxed), Value::Primitive(primitive)) => match (primitive, self.op) {
                (
                    Primitive::Bool(_),
                    ast::BinaryOp::Eq | ast::BinaryOp::Neq | ast::BinaryOp::Or | ast::BinaryOp::And,
                ) => {
                    let Value::Primitive(unboxed) = boxed.unwrap_variant(compiler, ValueTypeHint::Bool) else {unreachable!()};
                    let (pl, pr) = if matches!(lhs_value, Value::Primitive(_)) {
                        (primitive, unboxed)
                    } else {
                        (unboxed, primitive)
                    };
                    Primitive::build_arith(compiler, pl, pr, self.op)
                        .unwrap()
                        .into()
                }
                (Primitive::Bool(_), _) => {
                    panic!("invalid boolean operation: {:?}", self.op)
                }
                (Primitive::Int(_), ast::BinaryOp::And | ast::BinaryOp::Or) => {
                    panic!("invalid number operation: {:?}", self.op)
                }
                (Primitive::Int(_), ast::BinaryOp::Add) => {
                    let result = RefCell::new(Enum::build_new(compiler));

                    let current_block = compiler.builder.get_insert_block().unwrap();
                    let merge_block = compiler
                        .context
                        .insert_basic_block_after(current_block, "matchmerge");

                    boxed.build_runtime_match(
                        compiler,
                        merge_block,
                        &[
                            (ValueTypeHint::Int, &|compiler, value, else_block| {
                                let Value::Primitive(unboxed) = value else {unreachable!()};
                                let (pl, pr) = if matches!(lhs_value, Value::Primitive(_)) {
                                    (primitive, unboxed)
                                } else {
                                    (unboxed, primitive)
                                };

                                if let Ok(op) = Primitive::build_arith(compiler, pl, pr, self.op) {
                                    result.borrow_mut().build_instance(compiler, op.into());
                                } else {
                                    compiler.builder.build_unconditional_branch(else_block);
                                };
                            }),
                            (ValueTypeHint::Str, &|compiler, value, _| {
                                let Value::Str(unboxed) = value else {unreachable!()};
                                let number_fmt = Str::build_fmt_primitive(compiler, primitive);

                                let (l, r) = if matches!(lhs_value, Value::Primitive(_)) {
                                    (number_fmt, unboxed)
                                } else {
                                    (unboxed, number_fmt)
                                };

                                let appended = Str::build_str_append(compiler, l, r);
                                result
                                    .borrow_mut()
                                    .build_instance(compiler, appended.into());
                            }),
                        ],
                    );

                    compiler.builder.position_at_end(merge_block);
                    Value::Boxed(result.into_inner())
                }
                (Primitive::Int(_), _) => {
                    let Value::Primitive(unboxed) = boxed.unwrap_variant(compiler, ValueTypeHint::Int) else {unreachable!()};

                    let (pl, pr) = if matches!(lhs_value, Value::Primitive(_)) {
                        (primitive, unboxed)
                    } else {
                        (unboxed, primitive)
                    };

                    Primitive::build_arith(compiler, pl, pr, self.op)
                        .unwrap()
                        .into()
                }
            },
            (Value::Str(l), Value::Str(r)) => match self.op {
                ast::BinaryOp::Add => Str::build_str_append(compiler, l, r).into(),
                ast::BinaryOp::Eq => Primitive::Bool({
                    let result = compiler.builder.build_call(
                        compiler.core_functions.get(CoreFunction::MemCmp),
                        &[l.ptr.into(), r.ptr.into(), l.len.into()],
                        "",
                    );
                    result.as_any_value_enum().into_int_value()
                })
                .into(),
                ast::BinaryOp::Neq => Primitive::Bool(if l.len != r.len {
                    compiler.context.bool_type().const_int(1 as _, false)
                } else {
                    let result = compiler.builder.build_call(
                        compiler.core_functions.get(CoreFunction::MemCmp),
                        &[l.ptr.into(), r.ptr.into(), l.len.into()],
                        "",
                    );
                    compiler
                        .builder
                        .build_int_neg(result.as_any_value_enum().into_int_value(), "")
                })
                .into(),
                _ => panic!("invalid operation between strings"),
            },
            (Value::Str(string), Value::Primitive(Primitive::Int(number)))
            | (Value::Primitive(Primitive::Int(number)), Value::Str(string))
                if self.op == ast::BinaryOp::Add =>
            {
                let number_fmt = Str::build_fmt_primitive(compiler, Primitive::Int(number));
                let (l, r) = if matches!(lhs_value, Value::Str(_)) {
                    (string, number_fmt)
                } else {
                    (number_fmt, string)
                };
                Str::build_str_append(compiler, l, r).into()
            }

            (Value::Str(string), Value::Boxed(boxed))
            | (Value::Boxed(boxed), Value::Str(string))
                if self.op == ast::BinaryOp::Add =>
            {
                let current_block = compiler.builder.get_insert_block().unwrap();
                let merge_block = compiler
                    .context
                    .insert_basic_block_after(current_block, "matchmerge");

                let result_str = compiler
                    .builder
                    .build_alloca(compiler.context.i8_type().ptr_type(Default::default()), "");
                let result_len = compiler
                    .builder
                    .build_alloca(compiler.context.i32_type(), "");

                {
                    boxed.build_runtime_match(
                        compiler,
                        merge_block,
                        &[
                            (ValueTypeHint::Str, &|compiler, value, _| {
                                let Value::Str(unboxed) = value else {unreachable!()};
                                let (l, r) = if matches!(lhs_value, Value::Str(_)) {
                                    (string, unboxed)
                                } else {
                                    (unboxed, string)
                                };

                                let appended = Str::build_str_append(compiler, l, r);
                                compiler
                                    .builder
                                    .build_memcpy(result_str, 1, appended.ptr, 1, appended.len)
                                    .unwrap();
                                compiler.builder.build_store(result_len, appended.len);
                            }),
                            (ValueTypeHint::Int, &|compiler, value, _| {
                                let Value::Primitive(value) = value else {unreachable!()};
                                let number_fmt = Str::build_fmt_primitive(compiler, value);

                                let (l, r) = if matches!(lhs_value, Value::Str(_)) {
                                    (string, number_fmt)
                                } else {
                                    (number_fmt, string)
                                };

                                let appended = Str::build_str_append(compiler, l, r);
                                compiler
                                    .builder
                                    .build_memcpy(result_str, 1, appended.ptr, 1, appended.len)
                                    .unwrap();
                                compiler.builder.build_store(result_len, appended.len);
                            }),
                        ],
                    );
                    compiler.builder.position_at_end(merge_block);
                }

                Str::new(
                    result_str,
                    compiler
                        .builder
                        .build_load(compiler.context.i32_type(), result_len, "")
                        .into_int_value(),
                )
                .into()
            }
            (Value::Str(string), Value::Boxed(boxed))
            | (Value::Boxed(boxed), Value::Str(string)) => {
                let Value::Str(unboxed) = boxed.unwrap_variant(compiler, ValueTypeHint::Str) else {unreachable!()};
                let (l, r) = if matches!(lhs_value, Value::Str(_)) {
                    (string, unboxed)
                } else {
                    (unboxed, string)
                };
                match self.op {
                    ast::BinaryOp::Eq => Primitive::Bool({
                        let result = compiler.builder.build_call(
                            compiler.core_functions.get(CoreFunction::MemCmp),
                            &[l.ptr.into(), r.ptr.into(), l.len.into()],
                            "",
                        );
                        result.as_any_value_enum().into_int_value()
                    })
                    .into(),
                    ast::BinaryOp::Neq => Primitive::Bool(if l.len != r.len {
                        compiler.context.bool_type().const_int(1 as _, false)
                    } else {
                        let result = compiler.builder.build_call(
                            compiler.core_functions.get(CoreFunction::MemCmp),
                            &[l.ptr.into(), r.ptr.into(), l.len.into()],
                            "",
                        );
                        compiler
                            .builder
                            .build_int_neg(result.as_any_value_enum().into_int_value(), "")
                    })
                    .into(),
                    _ => panic!("invalid operation between strings"),
                }
            }
            (Value::Str(string), Value::Primitive(Primitive::Int(number)))
            | (Value::Primitive(Primitive::Int(number)), Value::Str(string))
                if matches!(self.op, ast::BinaryOp::Add) =>
            {
                let number_fmt = Str::build_fmt_primitive(compiler, Primitive::Int(number));
                let (l, r) = if matches!(lhs_value, Value::Str(_)) {
                    (string, number_fmt)
                } else {
                    (number_fmt, string)
                };
                Str::build_str_append(compiler, l, r).into()
            }
            (Value::Boxed(bl), Value::Boxed(br)) => {
                let result = RefCell::new(Enum::build_new(compiler));

                let current_block = compiler.builder.get_insert_block().unwrap();
                let merge_block = compiler
                    .context
                    .insert_basic_block_after(current_block, "matchmerge");

                bl.build_runtime_match(
                    compiler,
                    merge_block,
                    &[
                        (ValueTypeHint::Bool, &|compiler, l, else_block| {
                            let Value::Primitive(l) = l else {unreachable!()};
                            let Value::Primitive(r) = br.build_runtime_unwrap(compiler, ValueTypeHint::Bool) else {unreachable!()};

                            if let Ok(op) = Primitive::build_arith(compiler, l, r, self.op)
                            {
                                result.borrow_mut().build_instance(compiler, op.into());
                            } else {
                                compiler.builder.build_unconditional_branch(else_block);
                            };
                        }),
                        (ValueTypeHint::Int, &|compiler, l, _| {
                            let Value::Primitive(l) = l else {unreachable!()};

                            br.build_runtime_match(
                                compiler,
                                merge_block,
                                &[
                                    (ValueTypeHint::Int, &|compiler, r, else_block| {
                                        let Value::Primitive(r) = r else {unreachable!()};
                                        if let Ok(op) =
                                            Primitive::build_arith(compiler, l, r, self.op)
                                        {
                                            result.borrow_mut().build_instance(compiler, op.into());
                                        } else {
                                            compiler.builder.build_unconditional_branch(else_block);
                                        }
                                    }),
                                    (ValueTypeHint::Str, &|compiler, r, _| {
                                        let Value::Str(r) = r else {unreachable!()};
                                        let number_fmt = Str::build_fmt_primitive(compiler, l);

                                        let appended =
                                            Str::build_str_append(compiler, number_fmt, r);
                                        result
                                            .borrow_mut()
                                            .build_instance(compiler, appended.into())
                                    }),
                                ],
                            );
                        }),
                        (ValueTypeHint::Str, &|compiler, l, else_block| {
                            let Value::Str(l) = l else {unreachable!()};

                            br.build_runtime_match(
                                compiler,
                                merge_block,
                                &[
                                    (ValueTypeHint::Str, &|compiler, r, _| {
                                        let Value::Str(r) = r else {unreachable!()};
                                        let appended =
                                            Str::build_str_append(compiler, l, r);
                                        result
                                            .borrow_mut()
                                            .build_instance(compiler, appended.into())
                                    }),
                                    (ValueTypeHint::Int, &|compiler, r, _| {
                                        let Value::Primitive(r) = r else {unreachable!()};
                                        let number_fmt = Str::build_fmt_primitive(compiler, r);

                                        let appended =
                                            Str::build_str_append(compiler, l, number_fmt);
                                        result
                                            .borrow_mut()
                                            .build_instance(compiler, appended.into())
                                    }),
                                ],
                            );
                        })
                    ],
                );

                compiler.builder.position_at_end(merge_block);

                Value::Boxed(result.into_inner())
            }
            _ => panic!(
                "invalid operation {:?} between {:?} and {:?} at {:?}",
                self.op, lhs_value, rhs_value, self.location.start
            ),
        };

        result
    }
}

impl Codegen for ast::Print {
    type R<'ctx> = Str<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let param = self.value.codegen_value(compiler);

        let print_str = match param {
            Value::Str(str) => str,
            Value::Primitive(primitive) => {
                let str = Str::build_fmt_primitive(compiler, primitive);
                str
            }
            Value::Closure(_closure) => compiler.get_or_insert_string("<#closure>"),
            Value::Boxed(b) => {
                let current_block = compiler.builder.get_insert_block().unwrap();
                let merge_block = compiler
                    .context
                    .insert_basic_block_after(current_block, "matchmerge");

                let result_str = compiler
                    .builder
                    .build_alloca(compiler.context.i8_type().ptr_type(Default::default()), "");
                let result_len = compiler
                    .builder
                    .build_alloca(compiler.context.i32_type(), "");

                b.build_runtime_match(
                    compiler,
                    merge_block,
                    &[
                        (ValueTypeHint::Bool, &|compiler, value, _| {
                            let Value::Primitive(Primitive::Bool(val)) = value else {unreachable!()};
                            let val_fmt = compiler.builder.build_array_alloca(
                                compiler.context.i8_type(),
                                compiler.context.i32_type().const_int(6, false),
                                "",
                            );
                            let len = compiler.builder.build_call(
                                compiler.core_functions.get(CoreFunction::FmtBool),
                                &[val_fmt.into(), val.into()],
                                "",
                            ).as_any_value_enum().into_int_value();
                            compiler.builder.build_memmove(result_str, 1, val_fmt,1, len).unwrap();
                            compiler.builder.build_store(result_len, len);
                        }),
                        (ValueTypeHint::Int, &|compiler, value, _| {
                            let Value::Primitive(Primitive::Int(val)) = value else {unreachable!()};
                            let val_fmt = compiler.builder.build_array_alloca(
                                compiler.context.i8_type(),
                                compiler.context.i32_type().const_int(6, false),
                                "",
                            );
                            let len = compiler.builder.build_call(
                                compiler.core_functions.get(CoreFunction::FmtInt),
                                &[val_fmt.into(), val.into()],
                                "",
                            ).as_any_value_enum().into_int_value();
                            compiler.builder.build_memmove(result_str, 1, val_fmt,1, len).unwrap();
                            compiler.builder.build_store(result_len, len);
                        }),
                        (ValueTypeHint::Str, &|compiler, value, _| {
                            let Value::Str(val) = value else {unreachable!()};
                            compiler.builder.build_memcpy(result_str, 1, val.ptr,1, val.len).unwrap();
                            compiler.builder.build_store(result_len, val.len);
                        }),
                    ],
                );
                compiler.builder.position_at_end(merge_block);

                Str::new(
                    result_str,
                    compiler
                        .builder
                        .build_load(compiler.context.i32_type(), result_len, "")
                        .into_int_value(),
                )
            }
            Value::Tuple(tup) => {
                let tup_buf = compiler
                    .builder
                    .build_array_malloc(
                        compiler.context.i8_type(),
                        compiler.context.i32_type().const_int(50, false),
                        "",
                    )
                    .unwrap();

                let tup_str_len = compiler
                    .builder
                    .build_indirect_call(
                        compiler
                            .core_functions
                            .get(CoreFunction::FmtTuple)
                            .get_type(),
                        compiler
                            .core_functions
                            .get(CoreFunction::FmtTuple)
                            .as_global_value()
                            .as_pointer_value(),
                        &[tup_buf.into(), tup.ptr.into()],
                        "",
                    )
                    .as_any_value_enum()
                    .into_int_value();

                compiler.builder.build_call(
                    compiler.core_functions.get(CoreFunction::PrintStr),
                    &[tup_buf.into(), tup_str_len.into()],
                    "",
                );

                Str::new(tup_buf, tup_str_len)
            }
        };

        let _ = compiler.builder.build_call(
            compiler.core_functions.get(CoreFunction::PrintStr),
            &[print_str.ptr.into(), print_str.len.into()],
            "print",
        );

        print_str
    }
}

impl Codegen for ast::Call {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let arguments = self
            .arguments
            .iter()
            .map(|arg| {
                let val = arg.codegen_value(compiler);
                (val, matches!(val, Value::Str(_)))
            })
            .collect::<Vec<_>>();

        let closure = if let ast::Term::Var(v) = self.callee.as_ref() {
            let funct_name = v.text.as_ref();
            let Some(funct) = compiler.scope.find_callable(funct_name)  else {
            panic!("function not found")
        };
            let arguments_types = arguments
                .iter()
                .flat_map(|(arg, has_len)| {
                    if *has_len {
                        vec![arg.get_known_type(), ValueType::Int]
                    } else {
                        vec![arg.get_known_type()]
                    }
                })
                .collect::<Vec<_>>();

            let maybe_cl = {
                let f = funct.borrow();
                f.definitions
                    .iter()
                    .find(|(defs, _)| {
                        defs.iter()
                            .zip(arguments_types.iter())
                            .all(|(&a, &b)| a == b)
                    })
                    .map(|(_, cl)| *cl)
            };

            if let Some(closure) = maybe_cl {
                closure
            } else {
                let (mut cl, capt_ty) =
                    { Closure::build_definition(compiler, &funct.borrow(), &arguments) };

                funct
                    .borrow_mut()
                    .definitions
                    .push((arguments_types.clone(), cl));

                cl.build_prepare_function(compiler, &funct.borrow(), Some(&arguments), capt_ty);

                if !cl.funct.verify(false) {
                    cl.funct.print_to_stderr();
                    panic!("invalid function");
                }

                cl
            }
        } else {
            let Value::Closure( closure) = self.callee.as_ref().codegen_value(compiler) else {
                panic!("function is not callable");
            };
            closure
        };

        let mut args = arguments
            .iter()
            .flat_map(|(arg, _)| {
                if let Value::Str(s) = arg {
                    vec![
                        BasicValueEnum::from(arg).into(),
                        BasicValueEnum::IntValue(s.len).into(),
                    ]
                } else {
                    vec![BasicValueEnum::from(arg).into()]
                }
            })
            .collect::<Vec<BasicMetadataValueEnum>>();

        // add sret to arguments
        let sret = Enum::build_new(compiler);
        args.insert(0, sret.ptr.into());

        // add captures to arguments
        if let Some(captures_struct) = closure.captures {
            args.insert(1, captures_struct.into());
        }

        compiler.builder.build_direct_call(closure.funct, &args, "");

        if let Some(t) = closure.returns.exactly_one() {
            return sret.unwrap_variant(compiler, t);
        }
        sret.into()
    }
}

fn build_capture_args<'ctx>(
    compiler: &mut Compiler<'_, 'ctx>,
    funct_ref: &Function<'ctx>,
    captures_ptr: PointerValue<'ctx>,
    captures_struct: inkwell::types::StructType<'ctx>,
) {
    for (n, capture) in funct_ref.captured_variables.iter().enumerate() {
        let value_ptr = match capture {
            Capture::Direct { variable, .. } => variable.get_ptr(compiler),

            Capture::Indirect { function, .. } => {
                let funct_indi_ref = function.borrow();
                let captures_struct_indi =
                    build_captures_struct(&funct_indi_ref, compiler).unwrap();
                let captures_ptr_indi = compiler
                    .builder
                    .build_alloca(captures_struct_indi, "cap_indi");

                build_capture_args(
                    compiler,
                    &funct_indi_ref,
                    captures_ptr_indi,
                    captures_struct_indi,
                );

                captures_ptr_indi
            }
        };

        if n == 0 {
            compiler.builder.build_store(captures_ptr, value_ptr);
        } else {
            let ptr = unsafe {
                compiler.builder.build_in_bounds_gep(
                    captures_struct,
                    captures_ptr,
                    &[
                        compiler.context.i32_type().const_int(0, false),
                        compiler.context.i32_type().const_int(n as _, false),
                    ],
                    "",
                )
            };
            compiler.builder.build_store(ptr, value_ptr);
        };
    }
}

impl Codegen for ast::Let {
    type R<'ctx> = Variable<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let value_term = &self.value;

        let variable = if let ast::Term::Function(f) = value_term.as_ref() {
            let funct = Function::build(compiler, Some(self.name.text.clone()), f);
            Variable::Function(funct)
        } else {
            let value = value_term.codegen_value(compiler);
            Variable::Value(value.build_as_ref(compiler, &self.name.text))
        };
        compiler
            .scope
            .add_variable(&self.name.text, variable.clone());

        variable
    }
}

impl Codegen for ast::If {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let Value::Primitive(Primitive::Bool(condition)) = self.condition.codegen_value(compiler) else {
            panic!("if condition must be boolean")
        };
        // let function = { compiler.scope.borrow().function };

        let current_block = compiler.builder.get_insert_block().unwrap();

        let merge_block = compiler
            .context
            .insert_basic_block_after(current_block, "ifmerge");

        let then_block = compiler.context.prepend_basic_block(merge_block, "then");
        let else_block = compiler.context.prepend_basic_block(merge_block, "else");

        // create an anonymous child for the if block
        let if_scope = {
            let scope = compiler
                .scope
                .create_child(compiler.scope.borrow().block, None);

            scope.borrow_mut().variables = compiler.scope.borrow().variables.clone();
            scope.borrow_mut().captured_variables =
                compiler.scope.borrow().captured_variables.clone();

            scope
        };
        let mut enum_builder = Enum::build_new(compiler);

        let initial_scope = Rc::clone(&compiler.scope);

        compiler
            .builder
            .build_conditional_branch(condition, then_block, else_block);

        let then_scope = {
            let scope = if_scope.create_child(then_block, None);
            scope.borrow_mut().variables = compiler.scope.borrow().variables.clone();
            scope.borrow_mut().captured_variables =
                compiler.scope.borrow().captured_variables.clone();
            scope
        };
        let then_ret = {
            compiler.builder.position_at_end(then_block);
            compiler.scope.clone_from(&then_scope);

            let then_ret = compiler.compile_block(&self.then);
            enum_builder.build_instance(compiler, then_ret);
            then_ret
        };

        let else_scope = {
            let scope = if_scope.create_child(else_block, None);
            scope.borrow_mut().variables = compiler.scope.borrow().variables.clone();
            scope.borrow_mut().captured_variables =
                compiler.scope.borrow().captured_variables.clone();
            scope
        };
        let else_ret = {
            compiler.builder.position_at_end(else_block);
            compiler.scope.clone_from(&else_scope);

            let else_ret = compiler.compile_block(&self.otherwise);
            enum_builder.build_instance(compiler, else_ret);
            else_ret
        };

        if let Some(last_block) = else_block.get_previous_basic_block() {
            if last_block.get_terminator().is_none() {
                compiler.builder.position_at_end(last_block);
                compiler.builder.build_unconditional_branch(merge_block);
            }
        }
        if let Some(last_block) = merge_block.get_previous_basic_block() {
            if last_block.get_terminator().is_none() {
                compiler.builder.position_at_end(last_block);
                compiler.builder.build_unconditional_branch(merge_block);
            }
        }

        if else_block.get_terminator().is_none() {
            compiler.builder.position_at_end(else_block);
            compiler.builder.build_unconditional_branch(merge_block);
        }

        if then_block.get_terminator().is_none() {
            compiler.builder.position_at_end(then_block);
            compiler.builder.build_unconditional_branch(merge_block);
        }

        compiler.builder.position_at_end(merge_block);
        compiler.scope.clone_from(&initial_scope);

        // both types are the same
        let if_ret = if then_ret.get_known_type() == else_ret.get_known_type() {
            let flags = BitFlags::from(then_ret.get_known_type());
            enum_builder.unwrap_variant(compiler, flags.exactly_one().unwrap())
        } else {
            Value::Boxed(enum_builder)
        };
        if_ret
    }
}
