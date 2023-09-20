pub mod enums;
pub mod traits;
pub mod value;

use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue};

use crate::{
    ast,
    compiler::{
        environment::{Function, Variable},
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
            .expect("Variable not defined");

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
                    ast::BinaryOp::Add => {
                        Primitive::Int(compiler.builder.build_int_add(l, r, "tmpadd"))
                    }
                    ast::BinaryOp::Sub => {
                        Primitive::Int(compiler.builder.build_int_sub(l, r, "tmpsub"))
                    }
                    ast::BinaryOp::Mul => {
                        Primitive::Int(compiler.builder.build_int_mul(l, r, "tmpmul"))
                    }
                    ast::BinaryOp::Div => {
                        Primitive::Int(compiler.builder.build_int_signed_div(l, r, "tmpdiv"))
                    }
                    ast::BinaryOp::Rem => {
                        Primitive::Int(compiler.builder.build_int_signed_rem(l, r, "tmprem"))
                    }
                    ast::BinaryOp::Eq => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        l,
                        r,
                        "tmpeq",
                    )),
                    ast::BinaryOp::Neq => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        l,
                        r,
                        "tmpneq",
                    )),
                    ast::BinaryOp::Lt => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        l,
                        r,
                        "tmplte",
                    )),
                    ast::BinaryOp::Lte => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLE,
                        l,
                        r,
                        "tmplte",
                    )),
                    ast::BinaryOp::Gt => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGT,
                        l,
                        r,
                        "tmpgte",
                    )),
                    ast::BinaryOp::Gte => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGE,
                        l,
                        r,
                        "tmpgte",
                    )),
                    _ => panic!("invalid operation between terms"),
                }
                .into(),
                (Primitive::Bool(l), Primitive::Bool(r)) => Primitive::Bool(match self.op {
                    ast::BinaryOp::Eq => {
                        compiler
                            .builder
                            .build_int_compare(inkwell::IntPredicate::EQ, l, r, "tmpeq")
                    }
                    ast::BinaryOp::Neq => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        l,
                        r,
                        "tmpneq",
                    ),
                    ast::BinaryOp::And => compiler.builder.build_and(l, r, "tmpand"),
                    ast::BinaryOp::Or => compiler.builder.build_or(l, r, "tmpor"),
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
            _ => todo!(),
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
    type R<'ctx> = ();
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let funct_name = match self.callee.as_ref() {
            ast::Term::Var(v) => v.text.as_ref(),
            _ => panic!("invalid function call"),
        };

        let Some(variable) = compiler.scope.find_variable(funct_name)  else {
            panic!("function not found")
        };

        let Variable::Function(funct) = variable else {
            panic!("variable {} is not callable", funct_name)
        };

        let mut resolve_value = |term: &ast::Term| -> Value<'_> {
            match term {
                ast::Term::Int(i) => i.codegen(compiler).into(),
                ast::Term::Bool(b) => b.codegen(compiler).into(),
                ast::Term::Binary(b) => b.codegen(compiler),
                ast::Term::Var(v) => match v.codegen(compiler) {
                    Variable::Value(v) => v.build_deref(compiler),
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

        let funct_ref = funct.borrow_mut();

        if let Some(monomorphized_definition_index) =
            funct_ref.call_with.iter().position(|params| {
                params
                    .iter()
                    .zip(arguments.iter())
                    .all(|(&a, &b)| a == b.get_type())
            })
        {
            // happy end: we already defined this function for the given argument types
            let closure = funct_ref
                .definition
                .get(monomorphized_definition_index)
                .unwrap();

            let ret = compiler.builder.build_direct_call(
                closure.funct,
                &arguments
                    .iter()
                    .map(|arg| BasicValueEnum::from(arg).into())
                    .collect::<Vec<BasicMetadataValueEnum>>(),
                "",
            );
        }

        todo!()

        // let funct = compiler
        //     .
        //     .get(funct_name)
        //     .expect("Function not defined");

        // if (!funct.called) {
        //     if funct.body.parameters.len() != self.arguments.len() {
        //         panic!("invalid number of arguments");
        //     }
        //     // let mut scope = compiler.scope.current.borrow_mut();
        // }

        // let funct_pointer = funct.as_global_value().as_pointer_value();

        // let params = self
        //     .args
        //     .iter()
        //     .map(|arg| match arg.as_ref() {
        //         ast::Term::Int(i) => i.codegen(compiler).into(),
        //         ast::Term::Bool(b) => b.codegen(compiler).into(),
        //         ast::Term::Str(s) => s.codegen(compiler).into(),
        //         ast::Term::Binary(b) => b.codegen(compiler),
        //         ast::Term::Var(v) => v.codegen(compiler).build_deref(compiler),
        //         _ => todo!(),
        //     })
        //     .collect::<Vec<_>>();

        // compiler
        //     .builder
        //     .build_call(funct_pointer, &params, &self.name.text);
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
                let funct = Variable::Function(Function::new(f.clone()));
                compiler
                    .scope
                    .current
                    .borrow_mut()
                    .variables
                    .insert(self.name.text.clone(), funct.clone());
                return funct.clone();
            }
            _ => unimplemented!(),
        };

        let variable = value.build_variable(compiler, &self.name.text);

        compiler
            .scope
            .current
            .borrow_mut()
            .variables
            .insert(self.name.text.clone(), Variable::Value(variable));

        Variable::Value(variable)
    }
}
