use std::cell::OnceCell;

use enum_map::{Enum, EnumMap};
use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::FunctionType,
    values::FunctionValue,
};

use super::value::tuple::build_generic_tuple_type;

pub struct CoreFunctions<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a inkwell::module::Module<'ctx>,
    functions: EnumMap<CoreFunction, OnceCell<FunctionValue<'ctx>>>,
}

impl<'a, 'ctx> CoreFunctions<'a, 'ctx> {
    pub fn new(context: &'ctx Context, module: &'a Module<'ctx>) -> Self {
        Self {
            context,
            module,
            functions: enum_map::enum_map! {
                CoreFunction::PrintStr => OnceCell::new(),
                CoreFunction::PrintInt => OnceCell::new(),
                CoreFunction::PrintBool => OnceCell::new(),
                CoreFunction::FmtInt => OnceCell::new(),
                CoreFunction::FmtBool => OnceCell::new(),
                CoreFunction::FmtTuple => OnceCell::new(),
                CoreFunction::MemCmp => OnceCell::new(),
                CoreFunction::Panic => OnceCell::new(),
            },
        }
    }

    pub fn get(&self, funct: CoreFunction) -> FunctionValue<'ctx> {
        *self.functions[funct].get_or_init(|| {
            let definition = funct.get_definition(self.context);
            self.module
                .add_function(funct.into(), definition, Some(Linkage::External))
        })
    }
}

#[derive(Debug, Enum, Clone, Copy)]
pub enum CoreFunction {
    PrintStr,
    PrintInt,
    PrintBool,
    FmtInt,
    FmtBool,
    FmtTuple,
    MemCmp,
    Panic,
}

impl CoreFunction {
    pub fn get_definition<'ctx>(&self, context: &'ctx Context) -> FunctionType<'ctx> {
        match self {
            CoreFunction::PrintStr => context.i32_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    context.i32_type().into(),
                ],
                false,
            ),
            CoreFunction::PrintInt => context
                .i32_type()
                .fn_type(&[context.i32_type().into()], false),
            CoreFunction::PrintBool => context
                .i32_type()
                .fn_type(&[context.bool_type().into()], false),
            CoreFunction::FmtInt => context.i32_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    context.i32_type().into(),
                ],
                false,
            ),
            CoreFunction::FmtBool => context.i32_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    context.bool_type().into(),
                ],
                false,
            ),
            CoreFunction::FmtTuple => context.i32_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    build_generic_tuple_type(context)
                        .ptr_type(Default::default())
                        .into(),
                ],
                false,
            ),
            CoreFunction::MemCmp => context.bool_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    context.i8_type().ptr_type(Default::default()).into(),
                    context.i32_type().into(),
                ],
                false,
            ),
            CoreFunction::Panic => context.void_type().fn_type(&[], false),
        }
    }
}

impl From<CoreFunction> for &'static str {
    fn from(funct: CoreFunction) -> Self {
        match funct {
            CoreFunction::PrintStr => "__rinha_print_str",
            CoreFunction::PrintInt => "__rinha_print_int",
            CoreFunction::PrintBool => "__rinha_print_bool",
            CoreFunction::FmtInt => "__rinha_fmt_int",
            CoreFunction::FmtBool => "__rinha_fmt_bool",
            CoreFunction::FmtTuple => "__rinha_fmt_tuple",
            CoreFunction::MemCmp => "__rinha_memcmp",
            CoreFunction::Panic => "__rinha_panic",
        }
    }
}
