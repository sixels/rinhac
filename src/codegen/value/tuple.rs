use inkwell::{
    context::Context,
    types::{BasicTypeEnum, StructType},
    values::{BasicValue, IntValue, PointerValue},
};

use crate::{
    codegen::traits::AsBasicType,
    compiler::{self, Compiler},
};

use super::{Primitive, Value, ValueType, ValueTypeHint};

#[derive(Debug, Clone, Copy)]
pub struct Tuple<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub first_ty: ValueType<'ctx>,
    pub second_ty: ValueType<'ctx>,
}

impl<'ctx> Tuple<'ctx> {
    pub fn build_new(
        compiler: &compiler::Compiler<'_, 'ctx>,
        first: Value<'ctx>,
        second: Value<'ctx>,
    ) -> Self {
        let first_ty = first.get_known_type();
        let second_ty = second.get_known_type();

        let ty = build_tuple_type(
            compiler.context,
            first.get_known_type(),
            second.get_known_type(),
        );
        let gty = build_generic_tuple_type(compiler.context);

        let ptr = compiler.builder.build_alloca(gty, "");

        let (fpad, spad) = (
            pad_size(compiler.context, first_ty),
            pad_size(compiler.context, second_ty),
        );
        let first_tag = compiler.builder.build_struct_gep(gty, ptr, 0, "").unwrap();
        let first_ptr = compiler
            .builder
            .build_struct_gep(ty, ptr, 1 + u32::from(fpad.is_some()), "")
            .unwrap();
        clone_into(compiler, first, first_tag, first_ptr);

        let second_tag = compiler.builder.build_struct_gep(gty, ptr, 2, "").unwrap();
        let second_ptr: PointerValue<'_> = compiler
            .builder
            .build_struct_gep(ty, ptr, 4 + u32::from(fpad.is_some()), "")
            .unwrap();
        clone_into(compiler, second, second_tag, second_ptr);

        Self {
            ptr,
            first_ty,
            second_ty,
        }
    }

    pub fn unwrap_first(self, compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        match self.first_ty {
            ValueType::Int => {
                let num = compiler
                    .builder
                    .build_struct_gep(
                        build_tuple_type(compiler.context, self.first_ty, self.second_ty),
                        self.ptr,
                        2,
                        "",
                    )
                    .unwrap();

                Primitive::Int(
                    compiler
                        .builder
                        .build_load(compiler.context.i32_type(), num, "")
                        .into_int_value(),
                )
                .into()
            }
            ValueType::Bool => {
                let num = compiler
                    .builder
                    .build_struct_gep(
                        build_tuple_type(compiler.context, self.first_ty, self.second_ty),
                        self.ptr,
                        2,
                        "",
                    )
                    .unwrap();

                Primitive::Bool(
                    compiler
                        .builder
                        .build_load(compiler.context.bool_type(), num, "")
                        .into_int_value(),
                )
                .into()
            }
            _ => todo!(),
        }
    }

    pub fn unwrap_second(self, compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        match self.second_ty {
            ValueType::Int => {
                let num = compiler
                    .builder
                    .build_struct_gep(
                        build_tuple_type(compiler.context, self.first_ty, self.second_ty),
                        self.ptr,
                        5,
                        "",
                    )
                    .unwrap();

                Primitive::Int(
                    compiler
                        .builder
                        .build_load(compiler.context.i32_type(), num, "")
                        .into_int_value(),
                )
                .into()
            }
            ValueType::Bool => {
                let num = compiler
                    .builder
                    .build_struct_gep(
                        build_tuple_type(compiler.context, self.first_ty, self.second_ty),
                        self.ptr,
                        5,
                        "",
                    )
                    .unwrap();

                Primitive::Bool(
                    compiler
                        .builder
                        .build_load(compiler.context.bool_type(), num, "")
                        .into_int_value(),
                )
                .into()
            }
            _ => todo!(),
        }
    }
}

pub fn build_tuple_type<'ctx>(
    ctx: &'ctx Context,
    first: ValueType<'ctx>,
    second: ValueType<'ctx>,
) -> StructType<'ctx> {
    let first_ty = if let ValueType::Str(_) = first {
        string_struct(ctx).into()
    } else {
        first.as_basic_type(ctx)
    };
    let second_ty = if let ValueType::Str(_) = second {
        string_struct(ctx).into()
    } else {
        second.as_basic_type(ctx)
    };

    let mut fields = vec![
        ctx.i8_type().into(),
        first_ty,
        ctx.i8_type().into(),
        second_ty,
    ];

    let (fpad, spad) = (pad_size(ctx, first), pad_size(ctx, second));
    if let Some(pad) = fpad {
        fields.insert(1, pad);
    }
    if let Some(pad) = spad {
        fields.insert(3 + usize::from(fpad.is_some()), pad);
    }

    ctx.struct_type(&fields, false)
}

fn pad_size<'ctx>(ctx: &'ctx Context, ty: ValueType<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    match ty {
        ValueType::Any(_) => None,
        ValueType::Tuple(..) => todo!(),
        ValueType::Bool => Some(ctx.i8_type().array_type(22).into()),
        ValueType::Int => Some(ctx.i8_type().array_type(19).into()),
        ValueType::Str(_) => Some(ctx.i8_type().array_type(11).into()),
        ValueType::Closure(_) => todo!(),
    }
}

pub fn build_generic_tuple_type(ctx: &Context) -> StructType {
    ctx.struct_type(
        &[
            ctx.i8_type().into(),
            ctx.i8_type().array_type(23).into(),
            ctx.i8_type().into(),
            ctx.i8_type().array_type(23).into(),
        ],
        false,
    )
}

fn clone_into<'ctx>(
    compiler: &compiler::Compiler<'_, 'ctx>,
    value: Value<'ctx>,
    tag: PointerValue<'ctx>,
    ptr: PointerValue<'ctx>,
) {
    match value {
        Value::Boxed(b) => {
            compiler
                .builder
                .build_memcpy(
                    ptr,
                    8,
                    b.ptr,
                    8,
                    compiler.context.i32_type().const_int(24, false),
                )
                .unwrap();
            let t = compiler
                .builder
                .build_load(compiler.context.i8_type(), b.ptr, "");
            t.as_instruction_value().unwrap().set_alignment(8).unwrap();
            compiler.builder.build_store(tag, t);
        }
        Value::Primitive(p) => {
            compiler.builder.build_store(ptr, p.as_primitive_value());

            let t = match p {
                super::Primitive::Bool(_) => ValueTypeHint::Bool,
                super::Primitive::Int(_) => ValueTypeHint::Int,
            };

            compiler
                .builder
                .build_store(tag, make_tag(compiler.context, t as _));
        }
        Value::Str(s) => {
            let str_ptr = compiler
                .builder
                .build_struct_gep(string_struct(compiler.context), ptr, 0, "")
                .unwrap();
            compiler
                .builder
                .build_memcpy(str_ptr, 1, s.ptr, 1, s.len)
                .unwrap();
            let len_ptr = compiler
                .builder
                .build_struct_gep(string_struct(compiler.context), ptr, 1, "")
                .unwrap();
            compiler.builder.build_store(len_ptr, s.len);

            compiler
                .builder
                .build_store(tag, make_tag(compiler.context, ValueTypeHint::Str as _));
        }
        Value::Closure(_) => todo!(),
        Value::Tuple(_) => todo!(),
    };
}

impl<'ctx> From<Tuple<'ctx>> for Value<'ctx> {
    fn from(val: Tuple<'ctx>) -> Self {
        Value::Tuple(val)
    }
}

#[inline(always)]
fn string_struct(ctx: &Context) -> StructType {
    ctx.struct_type(
        &[
            ctx.i8_type().ptr_type(Default::default()).into(),
            ctx.i32_type().into(),
        ],
        false,
    )
}

#[inline(always)]
fn make_tag(ctx: &Context, tag: u8) -> IntValue {
    ctx.i8_type().const_int(tag as _, false)
}
