pub mod closure;

use inkwell::{
    context::Context,
    types::{BasicTypeEnum, PointerType},
    values::{BasicValueEnum, IntValue, PointerValue},
};

use crate::compiler::Compiler;

use super::traits::DerefValue;

pub use closure::Closure;

#[derive(Debug, Clone, Copy)]
pub enum ValueType<'ctx> {
    Int,
    Bool,
    Str(IntValue<'ctx>),
    Closure(PointerType<'ctx>),
}

impl<'ctx> ValueType<'ctx> {
    fn as_basic_type(self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Int => ctx.i32_type().into(),
            Self::Bool => ctx.bool_type().into(),
            Self::Str(_) => ctx.i8_type().ptr_type(Default::default()).into(),
            Self::Closure(t) => t.into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value<'ctx> {
    Primitive(Primitive<'ctx>),
    Str(Str<'ctx>),
    Closure(Closure<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum ValueRef<'ctx> {
    Primitive(PrimitiveRef<'ctx>),
    Str(StrRef<'ctx>),
    Closure(Closure<'ctx>),
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

#[derive(Debug, Clone, Copy)]
pub struct StrRef<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub len: IntValue<'ctx>,
}

impl<'ctx> Value<'ctx> {
    pub fn get_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            Value::Primitive(primitive) => match primitive {
                Primitive::Int(i) => i.get_type().into(),
                Primitive::Bool(b) => b.get_type().into(),
            },
            Value::Str(str) => str.ptr.get_type().into(),
            Value::Closure(closure) => closure
                .funct
                .as_global_value()
                .as_pointer_value()
                .get_type()
                .into(),
        }
    }

    pub fn get_known_type(&self) -> ValueType<'ctx> {
        match self {
            Value::Primitive(primitive) => match primitive {
                Primitive::Int(_) => ValueType::Int,
                Primitive::Bool(_) => ValueType::Bool,
            },
            Value::Str(str) => ValueType::Str(str.len),
            Value::Closure(c) => {
                ValueType::Closure(c.funct.as_global_value().as_pointer_value().get_type())
            }
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
            Self::Str(s) => ValueRef::Str(StrRef { ptr, len: s.len }),
            Self::Closure(closure) => ValueRef::Closure(*closure),
        }
    }
}

impl<'ctx> ValueRef<'ctx> {
    pub fn get_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Primitive(primitive) => match primitive {
                PrimitiveRef::Int(i) => i.get_type().into(),
                PrimitiveRef::Bool(b) => b.get_type().into(),
            },
            Self::Str(str) => str.ptr.get_type().into(),
            Self::Closure(closure) => closure
                .funct
                .as_global_value()
                .as_pointer_value()
                .get_type()
                .into(),
        }
    }

    pub fn get_known_type(&self) -> ValueType<'ctx> {
        match self {
            Self::Primitive(primitive) => match primitive {
                PrimitiveRef::Int(_) => ValueType::Int,
                PrimitiveRef::Bool(_) => ValueType::Bool,
            },
            Self::Str(str) => ValueType::Str(str.len),
            Self::Closure(c) => {
                ValueType::Closure(c.funct.as_global_value().as_pointer_value().get_type())
            }
        }
    }
}

impl<'ctx> DerefValue<'ctx> for ValueRef<'ctx> {
    type R = Value<'ctx>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R {
        match self {
            Self::Primitive(primitive) => primitive.build_deref(compiler).into(),
            Self::Str(str) => Str::new(
                compiler
                    .builder
                    .build_load(str.ptr.get_type(), str.ptr, "")
                    .into_pointer_value(),
                str.len,
            )
            .into(),
            Self::Closure(closure) => closure.build_load(compiler),
        }
    }
}

impl<'ctx> DerefValue<'ctx> for PrimitiveRef<'ctx> {
    type R = Primitive<'ctx>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R {
        match self {
            Self::Bool(bool_ref) => Primitive::Bool(
                compiler
                    .builder
                    .build_load(compiler.context.bool_type(), *bool_ref, "")
                    .into_int_value(),
            ),
            Self::Int(int_ref) => Primitive::Int(
                compiler
                    .builder
                    .build_load(compiler.context.i32_type(), *int_ref, "")
                    .into_int_value(),
            ),
        }
    }
}

impl<'ctx> DerefValue<'ctx> for StrRef<'ctx> {
    type R = Str<'ctx>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R {
        Str::new(
            compiler
                .builder
                .build_load(self.ptr.get_type(), self.ptr, "")
                .into_pointer_value(),
            self.len,
        )
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
            Value::Closure(closure) => closure.funct.as_global_value().as_pointer_value().into(),
        }
    }
}
