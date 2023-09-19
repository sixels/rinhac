use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::compiler::Compiler;

use super::traits::DerefValue;

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

#[derive(Debug, Clone, Copy)]
pub struct StrRef<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub len: IntValue<'ctx>,
}

#[derive(Debug, Clone, Copy)]
pub struct Closure<'ctx> {
    pub funct: FunctionValue<'ctx>,
    pub returns: ReturnType,
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ReturnType(u16);

impl ReturnType {
    pub const RETURN_INT: ReturnType = ReturnType(1 << 0);
    pub const RETURN_BOOL: ReturnType = ReturnType(1 << 1);
    pub const RETURN_STR: ReturnType = ReturnType(1 << 2);
    pub const RETURN_CLOSURE: ReturnType = ReturnType(1 << 3);

    pub fn new() -> Self {
        Self(0)
    }

    pub fn with_int(mut self) -> Self {
        self.0 |= Self::RETURN_INT.0;
        self
    }
    pub fn with_bool(mut self) -> Self {
        self.0 |= Self::RETURN_BOOL.0;
        self
    }
    pub fn with_str(mut self) -> Self {
        self.0 |= Self::RETURN_STR.0;
        self
    }
    pub fn with_closure(mut self) -> Self {
        self.0 |= Self::RETURN_CLOSURE.0;
        self
    }

    pub fn has_int(self) -> bool {
        self.0 & Self::RETURN_INT.0 != 0
    }
    pub fn has_bool(self) -> bool {
        self.0 & Self::RETURN_BOOL.0 != 0
    }
    pub fn has_str(self) -> bool {
        self.0 & Self::RETURN_STR.0 != 0
    }
    pub fn has_closure(self) -> bool {
        self.0 & Self::RETURN_CLOSURE.0 != 0
    }
}

impl<'ctx> Str<'ctx> {
    pub fn new(ptr: PointerValue<'ctx>, len: IntValue<'ctx>) -> Self {
        Self { ptr, len }
    }
}

impl<'ctx> Closure<'ctx> {
    pub fn new(funct: FunctionValue<'ctx>) -> Self {
        Self {
            funct,
            returns: ReturnType::new(),
        }
    }
    pub fn build_load(&self, _compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        todo!()
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
            Value::Closure(closure) => closure
                .funct
                .as_global_value()
                .as_pointer_value()
                .get_type()
                .into(),
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

impl<'ctx> DerefValue<'ctx> for ValueRef<'ctx> {
    type R = Value<'ctx>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R {
        match self {
            Self::Primitive(primitive) => primitive.build_deref(compiler).into(),
            Self::Str(str) => Str::new(
                compiler
                    .builder
                    .build_load(str.ptr.get_type(), str.ptr, "tmploadstr")
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

impl<'ctx> DerefValue<'ctx> for StrRef<'ctx> {
    type R = Str<'ctx>;
    fn build_deref(&self, compiler: &Compiler<'_, 'ctx>) -> Self::R {
        Str::new(
            compiler
                .builder
                .build_load(self.ptr.get_type(), self.ptr, "tmploadstr")
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
