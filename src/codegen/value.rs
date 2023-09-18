use inkwell::{
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue},
};

use crate::compiler::{Compiler, CoreFunction};

use super::traits::Displayable;

pub enum ValueKind {
    Int,
    Bool,
    Str(u32),
}

#[derive(Debug, Clone, Copy)]
pub enum Value<'ctx> {
    Primitive(Primitive<'ctx>),
    Str(Str<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum ValueRef<'ctx> {
    Primitive(PrimitiveRef<'ctx>),
    Str(Str<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum Primitive<'ctx> {
    Int(IntValue<'ctx>),
    Bool(IntValue<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveRef<'ctx> {
    Int(PointerValue<'ctx>),
    Bool(PointerValue<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub struct Str<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub len: IntValue<'ctx>,
}

impl<'ctx> Str<'ctx> {
    pub fn new(ptr: PointerValue<'ctx>, len: IntValue<'ctx>) -> Self {
        Self { ptr, len }
    }
}

impl<'ctx> Value<'ctx> {
    pub fn get_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            Value::Primitive(primitive) => match primitive {
                Primitive::Int(i) => i.get_type().into(),
                Primitive::Bool(b) => b.get_type().into(),
            },
            Value::Str(str) => str.ptr.get_type().into(),
        }
    }

    pub fn build_variable(&self, compiler: &Compiler<'_, 'ctx>, name: &str) -> ValueRef<'ctx> {
        let ptr = compiler.builder.build_alloca(self.get_type(), name);
        compiler
            .builder
            .build_store(ptr, BasicValueEnum::from(self));

        match self {
            Self::Primitive(Primitive::Bool(_)) => ValueRef::Primitive(PrimitiveRef::Bool(ptr)),
            Self::Primitive(Primitive::Int(_)) => ValueRef::Primitive(PrimitiveRef::Int(ptr)),
            Self::Str(s) => ValueRef::Str(Str { ptr, len: s.len }),
        }
    }
}

impl<'ctx> ValueRef<'ctx> {
    pub fn build_load(&self, compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        match self {
            Self::Primitive(primitive) => primitive.build_load(compiler).into(),
            Self::Str(str) => Str::new(
                compiler
                    .builder
                    .build_load(str.ptr.get_type(), str.ptr, "tmploadstr")
                    .into_pointer_value(),
                str.len,
            )
            .into(),
        }
    }
}

impl<'ctx> PrimitiveRef<'ctx> {
    pub fn build_load(&self, compiler: &Compiler<'_, 'ctx>) -> Primitive<'ctx> {
        match self {
            Self::Bool(bool_ref) => Primitive::Bool(
                compiler
                    .builder
                    .build_load(compiler.context.bool_type(), *bool_ref, "tmploadbool")
                    .into_int_value(),
            ),
            Self::Int(int_ref) => Primitive::Int(
                compiler
                    .builder
                    .build_load(compiler.context.i32_type(), *int_ref, "tmploadint")
                    .into_int_value(),
            ),
        }
    }
}

impl<'ctx> From<Primitive<'ctx>> for Value<'ctx> {
    fn from(value: Primitive<'ctx>) -> Self {
        match value {
            Primitive::Int(i) => Self::Primitive(Primitive::Int(i)),
            Primitive::Bool(b) => Self::Primitive(Primitive::Bool(b)),
        }
    }
}

impl<'ctx> From<Str<'ctx>> for Value<'ctx> {
    fn from(value: Str<'ctx>) -> Self {
        Self::Str(value)
    }
}

impl<'ctx> From<Primitive<'ctx>> for BasicValueEnum<'ctx> {
    fn from(value: Primitive<'ctx>) -> Self {
        match value {
            Primitive::Int(i) => i.into(),
            Primitive::Bool(b) => b.into(),
        }
    }
}

impl<'ctx> From<&Value<'ctx>> for BasicValueEnum<'ctx> {
    fn from(value: &Value<'ctx>) -> Self {
        match *value {
            Value::Primitive(primitive) => primitive.into(),
            Value::Str(str) => str.ptr.into(),
        }
    }
}

impl<'ctx> Displayable<'ctx> for ValueRef<'ctx> {
    fn display_params(
        &self,
        compiler: &mut Compiler<'_, 'ctx>,
    ) -> (
        crate::compiler::CoreFunction,
        Vec<BasicMetadataValueEnum<'ctx>>,
    ) {
        match self {
            ValueRef::Primitive(primitive) => match primitive {
                PrimitiveRef::Int(ptr) => {
                    let t = compiler.context.i32_type();
                    let p = *ptr;
                    (
                        CoreFunction::PrintInt,
                        vec![compiler.builder.build_load(t, p, "load").into()],
                    )
                }
                PrimitiveRef::Bool(ptr) => {
                    let t = compiler.context.bool_type();
                    let p = *ptr;
                    (
                        CoreFunction::PrintBool,
                        vec![compiler.builder.build_load(t, p, "load").into()],
                    )
                }
            },
            ValueRef::Str(str) => (CoreFunction::PrintStr, vec![str.ptr.into(), str.len.into()]),
        }
    }
}

// impl<'ctx> From<BasicTypeEnum<'ctx>> for Variable<'ctx> {
//     fn from(ptr: PointerValue<'ctx>, ty: BasicTypeEnum) -> Self {
//         match ty {
//             BasicValueEnum::IntValue(i) => {
//                 if i.get_type().get_bit_width() == 1 {
//                     Self::Bool
//                 } else {
//                     Self::Int
//                 }
//             }
//             BasicValueEnum::PointerValue(_) => Self::Str,
//             BasicValueEnum::ArrayValue(_) => Self::Str,
//             ty => panic!("invalid value type: {ty:#?}"),
//         }
//     }
// }
