pub mod enums;
pub mod traits;
pub mod value;

use std::rc::Rc;

use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, PointerValue,
};

use crate::{
    ast,
    codegen::{enums::Enum, value::Closure},
    compiler::{
        environment::{Capture, Function, ScopeNode, Variable},
        Compiler, CoreFunction,
    },
};

use self::{
    traits::{Codegen, CodegenValue, DerefValue},
    value::{closure::build_captures_struct, Primitive, Str, Value},
};

pub const FIRST_BLOCK_NAME: &str = "start";

impl CodegenValue for ast::Term {
    fn codegen_value<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Option<Value<'ctx>> {
        match self {
            ast::Term::Int(i) => Some(i.codegen(compiler).into()),
            ast::Term::Bool(b) => Some(b.codegen(compiler).into()),
            ast::Term::Binary(b) => Some(b.codegen(compiler)),
            ast::Term::Var(v) => match v.codegen(compiler) {
                Variable::Value(v) => Some(v.build_deref(compiler)),
                Variable::Constant(c) => Some(c),
                Variable::Function(_) => None,
            },
            ast::Term::Str(s) => Some(s.codegen(compiler).into()),
            _ => None,
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

        var.clone()
    }
}

impl Codegen for ast::Binary {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let lhs_value = self.lhs.codegen_value(compiler).unwrap();
        let rhs_value = self.rhs.codegen_value(compiler).unwrap();

        let result = match (lhs_value, rhs_value) {
            (Value::Primitive(pl), Value::Primitive(pr)) => match (pl, pr) {
                (Primitive::Int(l), Primitive::Int(r)) => match self.op {
                    ast::BinaryOp::Add => Primitive::Int(compiler.builder.build_int_add(l, r, "")),
                    ast::BinaryOp::Sub => Primitive::Int(compiler.builder.build_int_sub(l, r, "")),
                    ast::BinaryOp::Mul => Primitive::Int(compiler.builder.build_int_mul(l, r, "")),
                    ast::BinaryOp::Div => {
                        Primitive::Int(compiler.builder.build_int_signed_div(l, r, ""))
                    }
                    ast::BinaryOp::Rem => {
                        Primitive::Int(compiler.builder.build_int_signed_rem(l, r, ""))
                    }
                    ast::BinaryOp::Eq => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        l,
                        r,
                        "",
                    )),
                    ast::BinaryOp::Neq => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        l,
                        r,
                        "",
                    )),
                    ast::BinaryOp::Lt => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        l,
                        r,
                        "",
                    )),
                    ast::BinaryOp::Lte => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLE,
                        l,
                        r,
                        "",
                    )),
                    ast::BinaryOp::Gt => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGT,
                        l,
                        r,
                        "",
                    )),
                    ast::BinaryOp::Gte => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGE,
                        l,
                        r,
                        "",
                    )),
                    _ => panic!("invalid operation between numbers"),
                }
                .into(),
                (Primitive::Bool(l), Primitive::Bool(r)) => Primitive::Bool(match self.op {
                    ast::BinaryOp::Eq => {
                        compiler
                            .builder
                            .build_int_compare(inkwell::IntPredicate::EQ, l, r, "")
                    }
                    ast::BinaryOp::Neq => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        l,
                        r,
                        "tmpneq",
                    ),
                    ast::BinaryOp::And => compiler.builder.build_and(l, r, ""),
                    ast::BinaryOp::Or => compiler.builder.build_or(l, r, ""),
                    _ => panic!("invalid operation between booleans"),
                })
                .into(),
                (_l, _r) => panic!("bool and int operations are not allowed"),
            },
            (Value::Str(l), Value::Str(r)) => match self.op {
                ast::BinaryOp::Add => append_str(compiler, l, r).into(),
                ast::BinaryOp::Eq => Primitive::Bool(if l.len != r.len {
                    compiler.context.bool_type().const_int(0 as _, false)
                } else {
                    let result = compiler.builder.build_call(
                        compiler.core_functions[CoreFunction::MemCmp],
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
                        compiler.core_functions[CoreFunction::MemCmp],
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
                if matches!(self.op, ast::BinaryOp::Add) =>
            {
                let number_buf = compiler
                    .builder
                    .build_alloca(compiler.context.i8_type().ptr_type(Default::default()), "");
                let number_len = compiler
                    .builder
                    .build_call(
                        compiler.core_functions[CoreFunction::FmtInt],
                        &[number_buf.into(), number.into()],
                        "",
                    )
                    .as_any_value_enum()
                    .into_int_value();

                let number_str = Str::new(number_buf, number_len);
                let (l, r) = if matches!(lhs_value, Value::Str(_)) {
                    (string, number_str)
                } else {
                    (number_str, string)
                };

                append_str(compiler, l, r).into()
            }
            _ => panic!(
                "invalid operation between {} and {}",
                lhs_value.get_known_type(),
                rhs_value.get_known_type(),
            ),
        };
        result
    }
}

fn append_str<'ctx>(compiler: &mut Compiler<'_, 'ctx>, l: Str<'ctx>, r: Str<'ctx>) -> Str<'ctx> {
    let size = r.len.const_add(l.len);
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
    type R<'ctx> = CallSiteValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let param = match self.value.as_ref() {
            ast::Term::Int(i) => i.codegen(compiler).into(),
            ast::Term::Bool(b) => b.codegen(compiler).into(),
            ast::Term::Str(s) => s.codegen(compiler).into(),
            ast::Term::Binary(b) => b.codegen(compiler),
            ast::Term::Var(v) => match v.codegen(compiler) {
                Variable::Value(v) => v.build_deref(compiler),
                Variable::Constant(c) => c,
                Variable::Function(_) => todo!("can't print functions yet"),
            },
            _ => todo!(),
        };

        let (funct, params) = match param {
            Value::Str(str) => (CoreFunction::PrintStr, vec![str.ptr.into(), str.len.into()]),
            Value::Primitive(primitive) => match primitive {
                Primitive::Bool(bool) => (CoreFunction::PrintBool, vec![bool.into()]),
                Primitive::Int(int) => (CoreFunction::PrintInt, vec![int.into()]),
            },
            Value::Closure(_closure) => todo!("should print '<#closure>'"),
        };

        let funct = &compiler.core_functions[funct];
        let funct_pointer = funct.as_global_value().as_pointer_value();
        let ret =
            compiler
                .builder
                .build_indirect_call(funct.get_type(), funct_pointer, &params, "print");

        ret
    }
}

impl Codegen for ast::Call {
    type R<'ctx> = PointerValue<'ctx>;
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
            .map(|arg| arg.codegen_value(compiler).unwrap())
            .collect::<Vec<_>>();

        let arguments_types = arguments
            .iter()
            .map(|arg| arg.get_type())
            .collect::<Vec<_>>();

        let closure = funct
            .borrow_mut()
            .get_or_insert_definition(&arguments_types, |funct_ref| {
                Closure::build_definition(compiler, funct_ref, &arguments)
            });

        let mut args = arguments
            .iter()
            .map(|arg| BasicValueEnum::from(arg).into())
            .collect::<Vec<BasicMetadataValueEnum>>();

        // add sret to arguments
        let sret = compiler
            .builder
            .build_alloca(Enum::generic_type(compiler), "");
        args.insert(0, sret.into());

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

        sret
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
        let value = match value_term.as_ref() {
            ast::Term::Int(i) => i.codegen(compiler).into(),
            ast::Term::Bool(s) => s.codegen(compiler).into(),
            ast::Term::Str(s) => Value::Str(s.codegen(compiler)),
            ast::Term::Binary(b) => b.codegen(compiler),
            ast::Term::Function(f) => {
                let (direct_captures, indirect_captures) =
                    Function::captured_environment(&compiler.scope, f);

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
                    .chain(indirect_captures.into_keys().map(|name| {
                        let funct = compiler.scope.find_callable(&name).unwrap();
                        let name = {
                            let funct_ref = &funct.borrow();
                            funct_ref.unique_name()
                        };
                        Capture::indirect(name, funct)
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
            ast::Term::Var(v) => match v.codegen(compiler) {
                Variable::Value(v) => v.build_deref(compiler),
                Variable::Constant(c) => c,
                Variable::Function(_) => todo!(),
            },
            _ => Value::Primitive(Primitive::Int(
                compiler.context.i32_type().const_int(0, false),
            )),
        };

        let variable = value.build_variable(compiler, &self.name.text);

        compiler
            .scope
            .add_variable(&self.name.text, Variable::Value(variable));

        Variable::Value(variable)
    }
}

impl Codegen for ast::If {
    type R<'ctx> = ();
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let Value::Primitive(Primitive::Bool(condition)) = self.condition.codegen_value(compiler).unwrap() else {
            panic!("if condition must be a boolean")
        };
        let function = { compiler.scope.borrow().function };

        let then_block = compiler.context.append_basic_block(function.funct, "then");
        let else_block = compiler.context.append_basic_block(function.funct, "else");
        let merge_block = compiler.context.append_basic_block(function.funct, "merge");

        let merge_scope = Rc::clone(&compiler.scope);

        compiler
            .builder
            .build_conditional_branch(condition, then_block, else_block);

        compiler.builder.position_at_end(then_block);
        let then_scope = compiler.scope.create_child(then_block, None);
        compiler.scope.clone_from(&then_scope);

        let _then_ret = compiler.compile(&self.then);
        compiler.builder.build_unconditional_branch(merge_block);

        compiler.builder.position_at_end(else_block);
        let else_scope = compiler.scope.create_child(else_block, None);
        compiler.scope.clone_from(&else_scope);

        let _else_ret = compiler.compile(&self.otherwise);
        compiler.builder.build_unconditional_branch(merge_block);

        compiler.builder.position_at_end(merge_block);
        compiler.scope.clone_from(&merge_scope);
    }
}
