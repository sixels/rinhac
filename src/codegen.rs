pub mod enums;
pub mod traits;
pub mod value;

use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, PointerValue};

use crate::{
    ast,
    codegen::{enums::Enum, value::Closure},
    compiler::{
        environment::{Function, ScopeNode, Variable},
        Compiler, CoreFunction,
    },
};

use self::{
    traits::{Codegen, DerefValue},
    value::{Primitive, Str, Value},
};

pub const FIRST_BLOCK_NAME: &str = "start";

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
        let mut resolve_value = |term: &ast::Term| -> Value<'_> {
            match term {
                ast::Term::Int(i) => i.codegen(compiler).into(),
                ast::Term::Bool(b) => b.codegen(compiler).into(),
                ast::Term::Binary(b) => b.codegen(compiler),
                ast::Term::Var(v) => match v.codegen(compiler) {
                    Variable::Value(v) => v.build_deref(compiler),
                    Variable::Constant(c) => c,
                    Variable::Function(_) => panic!("can't perform operations on functions"),
                },
                ast::Term::Str(s) => s.codegen(compiler).into(),
                _ => unimplemented!(),
            }
        };

        let lhs_value = resolve_value(&self.lhs);
        let rhs_value = resolve_value(&self.rhs);

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
                    _ => panic!("invalid operation between terms"),
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
                    _ => panic!("type bool does not support the \"{:?}\" operation", self.op),
                })
                .into(),
                (_l, _r) => panic!("bool and int operations are not allowed"),
            },
            (Value::Str(l), Value::Str(r)) => match self.op {
                ast::BinaryOp::Add => {
                    let size = l.len.const_add(r.len);
                    let new_str =
                        compiler
                            .builder
                            .build_array_alloca(compiler.context.i8_type(), size, "");

                    compiler
                        .builder
                        .build_memcpy(new_str, 1, l.ptr, 1, l.len)
                        .unwrap();
                    // safety: we just allocated the right amount of memory
                    let new_str_tail = unsafe {
                        compiler.builder.build_gep(
                            compiler.context.i8_type(),
                            new_str,
                            &[l.len],
                            "",
                        )
                    };
                    compiler
                        .builder
                        .build_memcpy(new_str_tail, 1, r.ptr, 1, r.len)
                        .unwrap();

                    Str::new(new_str, size).into()
                }
                _ => todo!(),
            },
            _ => panic!(
                "invalid operation with {:?} and {:?}",
                lhs_value.get_known_type(),
                rhs_value.get_known_type(),
            ),
        };
        result
    }
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

        let mut resolve_value = |term: &ast::Term| -> Value<'_> {
            match term {
                ast::Term::Int(i) => i.codegen(compiler).into(),
                ast::Term::Bool(b) => b.codegen(compiler).into(),
                ast::Term::Binary(b) => b.codegen(compiler),
                ast::Term::Var(v) => match v.codegen(compiler) {
                    Variable::Value(v) => v.build_deref(compiler),
                    Variable::Constant(c) => c,
                    Variable::Function(_) => panic!("can't perform operations on functions"),
                },
                ast::Term::Str(s) => s.codegen(compiler).into(),
                _ => unimplemented!(),
            }
        };

        let arguments = self
            .arguments
            .iter()
            .map(|arg| resolve_value(arg))
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
        if let Some(captures) = closure.captures {
            let funct_ref = funct.borrow();

            let captured = compiler.builder.build_alloca(captures, "cap");

            for (n, (_, s)) in funct_ref.captured_variables.iter().enumerate() {
                let Some(var) =  compiler.scope.find_variable(s).or_else(|| compiler.scope.find_captured_variable(s)) else {
                    panic!("variable {s} not found")
                };

                if n == 0 {
                    compiler
                        .builder
                        .build_store(captured, var.get_ptr(compiler));
                } else {
                    let ptr = unsafe {
                        compiler.builder.build_in_bounds_gep(
                            captures,
                            captured,
                            &[
                                compiler.context.i32_type().const_int(0, false),
                                compiler.context.i32_type().const_int(n as _, false),
                            ],
                            "",
                        )
                    };
                    compiler.builder.build_store(ptr, var.get_ptr(compiler));
                };
            }

            // let capture_names = &funct_ref.captures;
            // compiler.scope.find_variable(name)
            args.insert(1, captured.into());
        }

        compiler.builder.build_direct_call(closure.funct, &args, "");

        sret
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
                let captures = Function::check_captures(&compiler.scope, f);
                let mut captured_variables = Vec::with_capacity(captures.len());
                for symbol in captures {
                    let var = compiler
                        .scope
                        .find_variable(&symbol)
                        .or_else(|| compiler.scope.find_captured_variable(&symbol))
                        .unwrap_or_else(|| panic!("variable {} not defined", symbol));
                    captured_variables.push((var.get_known_type(), String::from(symbol)));
                }

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
            _ => unimplemented!(),
        };

        let variable = value.build_variable(compiler, &self.name.text);

        compiler
            .scope
            .add_variable(&self.name.text, Variable::Value(variable));

        Variable::Value(variable)
    }
}
