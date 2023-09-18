use crate::compiler::Compiler;

pub trait Codegen {
    type R<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx>;
}
