use inkwell::values::BasicMetadataValueEnum;

use crate::compiler::{Compiler, CoreFunction};

pub trait Codegen {
    type R<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx>;
}

pub trait Displayable<'ctx> {
    fn display_params(
        &self,
        compiler: &mut Compiler<'_, 'ctx>,
    ) -> (CoreFunction, Vec<BasicMetadataValueEnum<'ctx>>);
}
