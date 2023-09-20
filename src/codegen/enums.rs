use inkwell::{
    types::{BasicTypeEnum, StructType},
    values::{IntValue, PointerValue},
};

use crate::compiler::Compiler;

use super::value::{Primitive, Value};

pub struct Enum<'ctx>(PointerValue<'ctx>);

impl<'ctx> Enum<'ctx> {
    // pub fn new(flag: IntValue<'ctx>, data: ArrayValue<'ctx>) -> Self {
    //     Self { flag, data }
    // }

    pub fn generic_type(compiler: &Compiler<'_, 'ctx>) -> StructType<'ctx> {
        compiler.context.struct_type(
            &[
                compiler.context.i8_type().into(),
                // pointer size
                compiler.context.i8_type().array_type(23).into(),
            ],
            false,
        )
    }

    pub fn variant_type<V>(compiler: &Compiler<'_, 'ctx>, variant: &V) -> StructType<'ctx>
    where
        V: TypeEnumVariant<'ctx>,
    {
        let data_type = variant.data_type();

        let data_type_size = basic_type_size(data_type);

        let (padding, pad_size) = if data_type_size <= 8 {
            (
                match data_type_size {
                    1 => compiler.context.i8_type(),
                    2 => compiler.context.i16_type(),
                    4 => compiler.context.i32_type(),
                    8 => compiler.context.i64_type(),
                    _ => panic!(),
                },
                1,
            )
        } else if data_type_size % 8 == 0 {
            (compiler.context.i64_type(), data_type_size / 8)
        } else if data_type_size % 4 == 0 {
            (compiler.context.i32_type(), data_type_size / 4)
        } else if data_type_size % 2 == 0 {
            (compiler.context.i16_type(), data_type_size / 2)
        } else {
            (compiler.context.i8_type(), data_type_size)
        };

        compiler
            .context
            .struct_type(&[padding.array_type(pad_size).into(), data_type], false)
    }
}

pub struct TypeEnum<'ctx>(pub Enum<'ctx>);

impl<'ctx> TypeEnum<'ctx> {
    pub fn new(compiler: &Compiler<'_, 'ctx>) -> Self {
        let enum_ptr = compiler
            .builder
            .build_alloca(Enum::generic_type(compiler), "");
        Self(Enum(enum_ptr))
    }

    pub fn createInstance<V: TypeEnumVariant<'ctx>>(
        self,
        compiler: &Compiler<'_, 'ctx>,
        variant: &V,
    ) -> PointerValue<'ctx> {
        let self_ptr = self.get_ptr();
        let data = compiler
            .builder
            .build_struct_gep(Enum::variant_type(compiler, variant), self_ptr, 1, "")
            .unwrap();

        let value = variant.value();
        match value {
            Value::Primitive(Primitive::Bool(b)) => compiler.builder.build_store(data, b),
            Value::Primitive(Primitive::Int(i)) => compiler.builder.build_store(data, i),
            _ => todo!(),
        };

        compiler
            .builder
            .build_store(self_ptr, variant.flag(compiler));

        self_ptr
    }

    pub fn get_ptr(self) -> PointerValue<'ctx> {
        self.0 .0
    }
}

pub trait TypeEnumVariant<'ctx> {
    fn flag(&self, compiler: &Compiler<'_, 'ctx>) -> IntValue<'ctx>;
    fn data_type(&self) -> BasicTypeEnum<'ctx>;
    fn value(&self) -> Value<'ctx>;
}

impl<'ctx> TypeEnumVariant<'ctx> for Primitive<'ctx> {
    fn flag(&self, compiler: &Compiler<'_, 'ctx>) -> IntValue<'ctx> {
        match self {
            Primitive::Bool(_) => compiler.context.i8_type().const_int(0, false),
            Primitive::Int(_) => compiler.context.i8_type().const_int(1, false),
        }
    }

    fn data_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            Primitive::Bool(b) => b.get_type().into(),
            Primitive::Int(i) => i.get_type().into(),
        }
    }
    fn value(&self) -> Value<'ctx> {
        (*self).into()
    }
}

fn basic_type_size(typ: BasicTypeEnum) -> u32 {
    match typ {
        BasicTypeEnum::IntType(ty) => u32::max(1, ty.get_bit_width() / 8),
        BasicTypeEnum::PointerType(_) => std::mem::size_of::<usize>() as u32,
        // BasicTypeEnum::ArrayType(ty) => ty.len() * basic_type_size(ty.get_element_type()),
        // BasicTypeEnum::StructType(ty) => ty.get_field_types().iter().map(|ty| ty.sizeof()).sum(),
        // BasicTypeEnum::VectorType(ty) => panic!("vector type is not supported"),
        _ => unimplemented!(),
    }
}
