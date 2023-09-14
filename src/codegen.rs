use std::vec;

use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum, CallSiteValue, GlobalValue, IntValue};

use crate::{
    ast,
    compiler::{Compiler, RTFunction},
};

pub trait Codegen {
    type R<'ctx>;
    fn codegen<'ctx>(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R<'ctx>;
}

impl Codegen for ast::Int {
    type R<'ctx> = IntValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        compiler.context.i32_type().const_int(self.value as _, true)
    }
}

impl Codegen for ast::Bool {
    type R<'ctx> = IntValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        compiler
            .context
            .bool_type()
            .const_int(self.value as _, false)
    }
}

impl Codegen for ast::Binary {
    type R<'ctx> = AnyValueEnum<'ctx>;
    fn codegen<'ctx>(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let resolve_value = |term: &ast::Term| -> BasicMetadataValueEnum<'_> {
            match term {
                ast::Term::Int(i) => i.codegen(compiler).into(),
                ast::Term::Bool(b) => b.codegen(compiler).into(),
                ast::Term::Binary(b) => match b.codegen(compiler) {
                    AnyValueEnum::IntValue(i) => i.into(),
                    _ => todo!(),
                },
                // ast::Term::Str(s) => s.codegen(compiler).into(),
                _ => unimplemented!(),
            }
        };

        let lhs_value = resolve_value(&self.lhs);
        let rhs_value = resolve_value(&self.rhs);

        let result = match (lhs_value, rhs_value) {
            (BasicMetadataValueEnum::IntValue(l), BasicMetadataValueEnum::IntValue(r)) => {
                match self.op {
                    ast::BinaryOp::Add => compiler.builder.build_int_add(l, r, "tmpadd"),
                    ast::BinaryOp::Sub => compiler.builder.build_int_sub(l, r, "tmpsub"),
                    ast::BinaryOp::Mul => compiler.builder.build_int_mul(l, r, "tmpmul"),
                    ast::BinaryOp::Div => compiler.builder.build_int_signed_div(l, r, "tmpdiv"),
                    ast::BinaryOp::Rem => compiler.builder.build_int_signed_rem(l, r, "tmprem"),
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
                    ast::BinaryOp::Lt => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        l,
                        r,
                        "tmplte",
                    ),
                    ast::BinaryOp::Lte => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLE,
                        l,
                        r,
                        "tmplte",
                    ),
                    ast::BinaryOp::Gt => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGT,
                        l,
                        r,
                        "tmpgte",
                    ),
                    ast::BinaryOp::Gte => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGE,
                        l,
                        r,
                        "tmpgte",
                    ),
                    ast::BinaryOp::And if is_boolean(l) && is_boolean(r) => {
                        compiler.builder.build_and(l, r, "tmpand")
                    }
                    ast::BinaryOp::Or if is_boolean(l) && is_boolean(r) => {
                        compiler.builder.build_or(l, r, "tmpor")
                    }
                    _ => todo!("invalid operation between terms"),
                }
            }

            _ => todo!(),
        };
        result.into()
    }
}

impl Codegen for ast::Str {
    type R<'ctx> = (GlobalValue<'ctx>, IntValue<'ctx>);
    fn codegen<'ctx>(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let len_value = compiler
            .context
            .i32_type()
            .const_int(self.value.len() as u64, false);
        if let Some(string) = compiler.strings.get(&self.value).copied() {
            return (string, len_value);
        }

        let global_name = format!(".str.{}", compiler.strings.len());
        (
            unsafe {
                compiler
                    .builder
                    .build_global_string(&self.value, &global_name)
            },
            len_value,
        )
    }
}

impl Codegen for ast::Print {
    type R<'ctx> = CallSiteValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let (params, funct): (Vec<BasicMetadataValueEnum>, RTFunction) = match self.value.as_ref() {
            ast::Term::Str(s) => (
                {
                    let (string, len) = s.codegen(compiler);
                    vec![string.as_pointer_value().into(), len.into()]
                },
                RTFunction::PrintStr,
            ),
            ast::Term::Int(i) => (vec![i.codegen(compiler).into()], RTFunction::PrintInt),
            ast::Term::Bool(i) => (vec![i.codegen(compiler).into()], RTFunction::PrintBool),
            ast::Term::Binary(b) => {
                let operation = b.codegen(compiler);
                let (result, print_fn) = match operation {
                    AnyValueEnum::IntValue(i) => (
                        vec![i.into()],
                        if is_boolean(i) {
                            RTFunction::PrintBool
                        } else {
                            RTFunction::PrintInt
                        },
                    ),
                    _ => todo!(),
                };

                (result, print_fn)
            }
            _ => unimplemented!(),
        };

        let funct = &compiler.prelude_functions[funct];
        let funct_pointer = funct.as_global_value().as_pointer_value();
        let ret =
            compiler
                .builder
                .build_indirect_call(funct.get_type(), funct_pointer, &params, "print");

        ret
    }
}

fn is_boolean(value: IntValue<'_>) -> bool {
    value.get_type().get_bit_width() == 1
}
