pub mod core;
pub mod traits;
pub mod value;

use std::rc::Rc;

use enumflags2::BitFlags;
use inkwell::values::{AnyValue, BasicMetadataValueEnum, BasicValueEnum, PointerValue};

use crate::{
    ast,
    codegen::value::{Closure, Enum, ValueType},
    compiler::{
        environment::{find_captures, Capture, Function, ScopeNode, Variable},
        Compiler,
    },
};

use self::{
    core::CoreFunction,
    traits::{Codegen, CodegenValue, DerefValue},
    value::{closure::build_captures_struct, Primitive, Str, Value, ValueTypeHint},
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
                Variable::Function(_) => unimplemented!(),
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
            ast::Term::First(_) => todo!(),
            ast::Term::Second(_) => todo!(),
            ast::Term::Tuple(_) => todo!(),
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

        println!("var: {:?}", var);
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
                pl.build_arith(pr, compiler, self.op).into()
            }
            (Value::Str(l), Value::Str(r)) => match self.op {
                ast::BinaryOp::Add => append_str(compiler, l, r).into(),
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
            | (Value::Primitive(Primitive::Bool(number)), Value::Str(string))
            | (Value::Str(string), Value::Primitive(Primitive::Bool(number)))
                if matches!(self.op, ast::BinaryOp::Add) =>
            {
                let number_buf = compiler
                    .builder
                    .build_malloc(compiler.context.i8_type().array_type(24), "")
                    .unwrap();
                let number_len = compiler
                    .builder
                    .build_indirect_call(
                        compiler.core_functions.get(CoreFunction::FmtInt).get_type(),
                        compiler
                            .core_functions
                            .get(CoreFunction::FmtInt)
                            .as_global_value()
                            .as_pointer_value(),
                        &[number_buf.into(), number.into()],
                        "",
                    )
                    .as_any_value_enum()
                    .into_int_value();

                let number_str = Str::new(number_buf, number_len);
                let (l, r) = if matches!(lhs_value, Value::Primitive(_)) {
                    (number_str, string)
                } else {
                    (string, number_str)
                };

                append_str(compiler, l, r).into()
            }
            (Value::Boxed(bl), Value::Boxed(br)) => {
                let result = compiler
                    .builder
                    .build_alloca(compiler.context.i32_type(), "");
                bl.build_runtime_match(
                    compiler,
                    &[(ValueTypeHint::Int, &|compiler, l| {
                        let Value::Primitive(l) = l else {unreachable!()};
                        let r = br.unwrap_variant(compiler, ValueTypeHint::Int);

                        let Value::Primitive(r) = r else {unreachable!()};
                        let op = l.build_arith(r, compiler, self.op);
                        match op {
                            Primitive::Bool(b) => {
                                compiler.builder.build_store(result, b);
                            }
                            Primitive::Int(i) => {
                                compiler.builder.build_store(result, i);
                            }
                        };
                        // // br.build_runtime_match(
                        // //     compiler,
                        // //     &[(ValueTypeHint::Int, &|compiler, r| {
                        // //         let Value::Primitive(r) = r else {unreachable!()};
                        // let op = l.build_arith(r, compiler, self.op);
                        // match op {
                        //     Primitive::Bool(b) => {
                        //         compiler.builder.build_store(result, b);
                        //     }
                        //     Primitive::Int(i) => {
                        //         compiler.builder.build_store(result, i);
                        //     }
                        // };
                        // //     })],
                        // );
                    })],
                );

                Value::Primitive(Primitive::Int(
                    compiler
                        .builder
                        .build_load(compiler.context.i32_type(), result, "")
                        .into_int_value(),
                ))
            }
            _ => panic!(
                "invalid operation between {:?} and {:?}",
                lhs_value, rhs_value,
            ),
        };
        result
    }
}

fn append_str<'ctx>(compiler: &mut Compiler<'_, 'ctx>, l: Str<'ctx>, r: Str<'ctx>) -> Str<'ctx> {
    let size = compiler.builder.build_int_add(l.len, r.len, "");
    let append_buf = compiler
        .builder
        .build_array_alloca(compiler.context.i8_type(), size, "");

    compiler
        .builder
        .build_memcpy(append_buf, 1, l.ptr, 1, l.len)
        .unwrap();
    // safety: we just allocated the right amount of memory
    let new_str_tail = unsafe {
        compiler
            .builder
            .build_gep(compiler.context.i8_type(), append_buf, &[l.len], "")
    };
    compiler
        .builder
        .build_memcpy(new_str_tail, 1, r.ptr, 1, r.len)
        .unwrap();

    Str::new(append_buf, size)
}

impl Codegen for ast::Print {
    type R<'ctx> = Primitive<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let param = self.value.codegen_value(compiler);

        let (funct, params) = match param {
            Value::Str(str) => (CoreFunction::PrintStr, vec![str.ptr.into(), str.len.into()]),
            Value::Primitive(primitive) => match primitive {
                Primitive::Bool(bool) => (CoreFunction::PrintBool, vec![bool.into()]),
                Primitive::Int(int) => (CoreFunction::PrintInt, vec![int.into()]),
            },
            Value::Closure(_closure) => todo!("should print '<#closure>'"),
            Value::Boxed(b) => {
                b.build_runtime_match(
                    compiler,
                    &[
                        (ValueTypeHint::Bool, &|compiler, value| {
                            compiler.builder.build_call(
                                compiler.core_functions.get(CoreFunction::PrintBool),
                                &[value.as_basic_value().into()],
                                "",
                            );
                        }),
                        (ValueTypeHint::Int, &|compiler, value| {
                            compiler.builder.build_call(
                                compiler.core_functions.get(CoreFunction::PrintInt),
                                &[value.as_basic_value().into()],
                                "",
                            );
                        }),
                        (ValueTypeHint::Str, &|compiler, value| {
                            let Value::Str(s) = value else {
                                    unreachable!()
                                };

                            compiler.builder.build_call(
                                compiler.core_functions.get(CoreFunction::PrintStr),
                                &[s.ptr.into(), s.len.into()],
                                "",
                            );
                        }),
                    ],
                );
                return Primitive::Int(compiler.context.i32_type().const_int(0, false));
            }
        };

        let funct = compiler.core_functions.get(funct);
        let _ = compiler.builder.build_call(funct, &params, "print");

        Primitive::Int(compiler.context.i32_type().const_int(0, false))
    }
}

impl Codegen for ast::Call {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let funct_name = match self.callee.as_ref() {
            ast::Term::Var(v) => v.text.as_ref(),
            _ => panic!("invalid function call"),
        };

        let Some(funct) = compiler.scope.find_callable(funct_name)  else {
            panic!("function not found")
        };

        let arguments = self
            .arguments
            .iter()
            .map(|arg| {
                let val = arg.codegen_value(compiler);
                (val, matches!(val, Value::Str(_)))
            })
            .collect::<Vec<_>>();

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
        let closure = if let Some(closure) = maybe_cl {
            closure
        } else {
            let mut cl = { Closure::build_definition(compiler, &funct.borrow(), &arguments) };

            funct
                .borrow_mut()
                .definitions
                .push((arguments_types.clone(), cl));

            cl.build_prepare_function(&funct.borrow(), compiler, &arguments);

            cl
        };
        // let closure = funct
        //     .borrow_mut()
        //     .get_or_insert_definition(&arguments_types, |funct_ref| {});

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
            let captures_ptr = if let Some(environ) = compiler
                .scope
                .get_indirect_captures_environ(&funct.borrow().unique_name())
            {
                compiler
                    .builder
                    .build_load(captures_struct.ptr_type(Default::default()), environ, "")
                    .into_pointer_value()
            } else {
                let captures_ptr = compiler.builder.build_alloca(captures_struct, "cap");
                let funct_ref = funct.borrow();
                build_capture_args(compiler, &funct_ref, captures_ptr, captures_struct);
                captures_ptr
            };

            // let capture_names = &funct_ref.captures;
            // compiler.scope.find_variable(name)
            args.insert(1, captures_ptr.into());
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

        if let ast::Term::Function(f) = value_term.as_ref() {
            let (direct_captures, indirect_captures) = find_captures(f);

            let captured_variables = direct_captures
                .into_iter()
                .map(|dc| {
                    let var = compiler.scope.find_any_variable(dc).unwrap();
                    let var = if let Variable::Value(v) = var {
                        Variable::Value(v.cloned(compiler))
                    } else {
                        var
                    };

                    Capture::direct(dc, var)
                })
                .chain(indirect_captures.into_iter().filter_map(|name| {
                    compiler.scope.find_callable(&name).map(|funct| {
                        let name = {
                            let funct_ref = &funct.borrow();
                            funct_ref.unique_name()
                        };
                        Capture::indirect(name, funct)
                    })
                }))
                .collect::<Vec<_>>();

            let funct = Variable::Function(Function::new(
                self.name.text.clone(),
                f.clone(),
                &compiler.scope,
                captured_variables,
            ));
            compiler.scope.add_variable(&self.name.text, funct.clone());
            return funct;
        }

        let value = value_term.codegen_value(compiler);
        let variable = value.build_as_ref(compiler, &self.name.text);

        compiler
            .scope
            .add_variable(&self.name.text, Variable::Value(variable));

        Variable::Value(variable)
    }
}

impl Codegen for ast::If {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let Value::Primitive(Primitive::Bool(condition)) = self.condition.codegen_value(compiler) else {
            panic!("if condition must be boolean")
        };
        let function = { compiler.scope.borrow().function };

        let then_block = compiler.context.append_basic_block(function.funct, "then");
        let else_block = compiler.context.append_basic_block(function.funct, "else");

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

        let last_block = compiler
            .scope
            .borrow()
            .function
            .funct
            .get_last_basic_block();

        let merge_block = compiler.context.append_basic_block(function.funct, "merge");
        if last_block.unwrap() != else_block {
            compiler.builder.position_at_end(last_block.unwrap());
            compiler.builder.build_unconditional_branch(merge_block);
        };

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
