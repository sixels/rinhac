use inkwell::{context::Context, types::BasicTypeEnum};

use crate::compiler::Compiler;

use super::value::Value;

pub trait Codegen {
    type R<'ctx>: CodegenValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx>;
}

pub trait DerefValue<'ctx> {
    type R: Into<Value<'ctx>>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R;
}

pub trait CodegenValue<'ctx> {
    fn codegen_value(&self, compiler: &mut Compiler<'_, 'ctx>) -> Value<'ctx>;
}

pub trait AsBasicType<'ctx> {
    fn as_basic_type(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx>;
}

impl<'ctx, T: Into<Value<'ctx>> + Copy> CodegenValue<'ctx> for T {
    fn codegen_value(&self, _: &mut Compiler<'_, 'ctx>) -> Value<'ctx> {
        (*self).into()
        // let a = *self;
        // a.into()
    }
}
