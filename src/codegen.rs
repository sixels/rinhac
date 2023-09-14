use inkwell::values::{
    BasicMetadataValueEnum, CallSiteValue, FunctionValue, GlobalValue, IntValue,
};

use crate::{
    ast,
    compiler::{Compiler, RTFunction},
};

pub trait Codegen {
    type R<'ctx>;
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx>;
}

impl Codegen for ast::Int {
    type R<'ctx> = IntValue<'ctx>;
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx> {
        compiler.context.i32_type().const_int(self.value as _, true)
    }
}

impl Codegen for ast::Str {
    type R<'ctx> = (GlobalValue<'ctx>, IntValue<'ctx>);
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx> {
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
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let (params, funct): (Vec<BasicMetadataValueEnum>, &FunctionValue<'ctx>) =
            match self.value.as_ref() {
                ast::Term::Str(s) => (
                    {
                        let (string, len) = s.codegen(compiler);
                        vec![string.as_pointer_value().into(), len.into()]
                    },
                    &compiler.prelude_functions[RTFunction::PrintStr],
                ),
                ast::Term::Int(i) => (
                    vec![i.codegen(compiler).into()],
                    &compiler.prelude_functions[RTFunction::PrintStr],
                ),
                _ => unimplemented!(),
            };

        let funct_pointer = funct.as_global_value().as_pointer_value();
        let ret =
            compiler
                .builder
                .build_indirect_call(funct.get_type(), funct_pointer, &params, "print");

        ret
    }
}
