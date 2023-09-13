use inkwell::{
    builder,
    values::{
        AnyValue, ArrayValue, BasicMetadataValueEnum, BasicValue, CallSiteValue, FunctionValue,
        GlobalValue, IntValue,
    },
};

use crate::{ast, compiler::Compiler};

pub trait Codegen {
    type R<'ctx>: AnyValue<'ctx>;
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx>;
}

impl Codegen for ast::Int {
    type R<'ctx> = IntValue<'ctx>;
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx> {
        compiler
            .context
            .i32_type()
            .const_int(self.value as _, false)
    }
}

impl Codegen for ast::Str {
    type R<'ctx> = GlobalValue<'ctx>;
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx> {
        if let Some(glob) = compiler.strings.get(&self.value).copied() {
            return glob;
        }

        let global_name = format!(".str.{}", compiler.strings.len());

        unsafe {
            compiler
                .builder
                .build_global_string(&self.value, &global_name)
        }
    }
}

impl Codegen for ast::Print {
    type R<'ctx> = CallSiteValue<'ctx>;
    fn codegen<'a, 'ctx>(&self, compiler: &'a Compiler<'a, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let (value, funct): (BasicMetadataValueEnum, &FunctionValue<'ctx>) =
            match self.value.as_ref() {
                ast::Term::Str(s) => (
                    s.codegen(compiler).as_pointer_value().into(),
                    compiler.prelude_functions.get("print_str").unwrap(),
                ),
                ast::Term::Int(i) => (
                    i.codegen(compiler).into(),
                    compiler.prelude_functions.get("print_int").unwrap(),
                ),
                _ => unimplemented!(),
            };

        let funct_pointer = funct.as_global_value().as_pointer_value();
        let ret = compiler.builder.build_indirect_call(
            funct.get_type(),
            funct_pointer,
            &[value],
            "print",
        );

        ret
    }
}
