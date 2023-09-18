pub mod traits;
pub mod value;

use std::vec;

use inkwell::values::CallSiteValue;

use crate::{
    ast,
    compiler::{Compiler, CoreFunction},
};

use self::{
    traits::{Codegen, DerefValue},
    value::{Primitive, Str, Value, ValueRef},
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
    type R<'ctx> = ValueRef<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let var = compiler
            .scope
            .current
            .borrow()
            .variables
            .get(&self.text)
            .copied()
            .expect("Variable not defined");
        var
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
                ast::Term::Var(v) => v.codegen(compiler).build_deref(compiler),
                // ast::Term::Str(s) => s.codegen(compiler).into(),
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
            ast::Term::Var(v) => v.codegen(compiler).build_deref(compiler),
            _ => todo!(),
        };

        let (funct, params) = match param {
            Value::Str(str) => (CoreFunction::PrintStr, vec![str.ptr.into(), str.len.into()]),
            Value::Primitive(primitive) => match primitive {
                Primitive::Bool(bool) => (CoreFunction::PrintBool, vec![bool.into()]),
                Primitive::Int(int) => (CoreFunction::PrintInt, vec![int.into()]),
            },
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

impl Codegen for ast::Let {
    type R<'ctx> = ValueRef<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let value_term = &self.value;
        let value = match value_term.as_ref() {
            ast::Term::Int(i) => i.codegen(compiler).into(),
            ast::Term::Bool(s) => s.codegen(compiler).into(),
            ast::Term::Str(s) => Value::Str(s.codegen(compiler)),
            ast::Term::Binary(b) => b.codegen(compiler),
            _ => todo!(),
        };

        let variable = value.build_variable(compiler, &self.name.text);

        compiler
            .scope
            .current
            .borrow_mut()
            .variables
            .insert(self.name.text.clone(), variable);

        variable
    }
}
