use crate::compiler::Compiler;

use super::value::Value;

pub trait Codegen {
    type R<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx>;
}

pub trait DerefValue<'ctx> {
    type R: Into<Value<'ctx>>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R;
}

pub trait CodegenValue {
    fn codegen_value<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Option<Value<'ctx>>;
}
