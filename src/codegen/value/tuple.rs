use inkwell::{
    context::Context,
    types::StructType,
    values::{BasicValue, PointerValue},
};

use crate::{
    codegen::traits::AsBasicType,
    compiler::{self, Compiler},
};

use super::{Primitive, Str, Value, ValueType};

#[derive(Debug, Clone)]
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
            &first.get_known_type(),
            &second.get_known_type(),
        );

        let ptr = compiler.builder.build_alloca(ty, "");

        let first_ptr = compiler.builder.build_struct_gep(ty, ptr, 0, "").unwrap();
        store_value_into(compiler, first, first_ptr);

        let second_ptr: PointerValue<'_> =
            compiler.builder.build_struct_gep(ty, ptr, 1, "").unwrap();
        store_value_into(compiler, second, second_ptr);

        Self {
            ptr,
            first_ty,
            second_ty,
        }
    }

    pub fn unwrap_first(&self, compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        self.unwrap(compiler, true)
    }

    pub fn unwrap_second(&self, compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        self.unwrap(compiler, false)
    }

    fn unwrap(&self, compiler: &Compiler<'_, 'ctx>, first: bool) -> Value<'ctx> {
        let (ty, index) = if first {
            (self.first_ty.clone(), 0)
        } else {
            (self.second_ty.clone(), 1)
        };

        let tup_ty = build_tuple_type(compiler.context, &self.first_ty, &self.second_ty);
        match ty {
            ValueType::Int => {
                let num = compiler
                    .builder
                    .build_struct_gep(tup_ty, self.ptr, index, "")
                    .unwrap();

                let val = compiler
                    .builder
                    .build_load(compiler.context.i32_type(), num, "");

                Primitive::Int(val.into_int_value()).into()
            }
            ValueType::Bool => {
                let num = compiler
                    .builder
                    .build_struct_gep(tup_ty, self.ptr, index, "")
                    .unwrap();

                Primitive::Bool(
                    compiler
                        .builder
                        .build_load(compiler.context.bool_type(), num, "")
                        .into_int_value(),
                )
                .into()
            }
            ValueType::Str(_) => {
                let data = compiler
                    .builder
                    .build_struct_gep(tup_ty, self.ptr, index, "")
                    .unwrap();

                let str_data = compiler
                    .builder
                    .build_struct_gep(string_struct(compiler.context), data, 0, "")
                    .unwrap();

                let str_ptr = compiler
                    .builder
                    .build_load(
                        compiler.context.i8_type().ptr_type(Default::default()),
                        str_data,
                        "",
                    )
                    .into_pointer_value();

                let len_ptr = compiler
                    .builder
                    .build_struct_gep(string_struct(compiler.context), data, 1, "")
                    .unwrap();
                let len = compiler
                    .builder
                    .build_load(compiler.context.i32_type(), len_ptr, "")
                    .into_int_value();

                let str_clone = compiler.builder.build_array_alloca(
                    compiler.context.i8_type().ptr_type(Default::default()),
                    len,
                    "",
                );
                compiler
                    .builder
                    .build_memcpy(str_clone, 1, str_ptr, 1, len)
                    .unwrap();

                Value::Str(crate::codegen::value::Str { ptr: str_ptr, len })
            }
            _ => unimplemented!(),
        }
    }

    pub fn fmt(compiler: &Compiler<'_, 'ctx>, tup: &Tuple<'ctx>) -> Str<'ctx> {
        let first_value = tup.unwrap_first(compiler);
        let first_fmt = fmt_value(compiler, first_value);
        let second_value = tup.unwrap_second(compiler);
        let second_fmt = fmt_value(compiler, second_value);

        let len = {
            let len = compiler
                .builder
                .build_int_add(first_fmt.len, second_fmt.len, "");
            compiler
                .builder
                .build_int_add(len, compiler.context.i32_type().const_int(4, false), "")
        };
        let buf = compiler
            .builder
            .build_array_alloca(compiler.context.i8_type(), len, "");

        let i8ty = compiler.context.i8_type();
        let i32ty = compiler.context.i32_type();

        // write "("
        compiler.builder.build_store(buf, i8ty.const_int(40, false));

        // write first elem
        unsafe {
            let f = compiler
                .builder
                .build_gep(i8ty, buf, &[i32ty.const_int(1, false)], "");
            compiler
                .builder
                .build_memcpy(f, 1, first_fmt.ptr, 1, first_fmt.len)
                .unwrap();
        }
        // write ", "
        unsafe {
            let index =
                compiler
                    .builder
                    .build_int_add(first_fmt.len, i32ty.const_int(1, false), "");
            let c = compiler.builder.build_gep(i8ty, buf, &[index], "");
            compiler.builder.build_store(c, i8ty.const_int(44, false));

            let index =
                compiler
                    .builder
                    .build_int_add(first_fmt.len, i32ty.const_int(2, false), "");
            let c = compiler.builder.build_gep(i8ty, buf, &[index], "");
            compiler.builder.build_store(c, i8ty.const_int(32, false));
        }

        // write second elem
        unsafe {
            let index =
                compiler
                    .builder
                    .build_int_add(first_fmt.len, i32ty.const_int(3, false), "");
            let s = compiler.builder.build_gep(i8ty, buf, &[index], "");
            compiler
                .builder
                .build_memcpy(s, 1, second_fmt.ptr, 1, second_fmt.len)
                .unwrap();
        }

        // write ")"
        unsafe {
            let index = compiler
                .builder
                .build_int_sub(len, i32ty.const_int(1, false), "");
            let c = compiler.builder.build_gep(i8ty, buf, &[index], "");
            compiler.builder.build_store(c, i8ty.const_int(41, false));
        }

        Str::new(buf, len)
    }
}

fn fmt_value<'ctx>(compiler: &Compiler<'_, 'ctx>, value: Value<'ctx>) -> Str<'ctx> {
    match value {
        Value::Primitive(prim) => Str::build_fmt_primitive(compiler, prim),
        Value::Str(s) => s,
        _ => todo!(),
    }
}

pub fn build_tuple_type<'ctx>(
    ctx: &'ctx Context,
    first: &ValueType<'ctx>,
    second: &ValueType<'ctx>,
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

    let fields = [first_ty, second_ty];
    ctx.struct_type(&fields, false)
}

pub fn store_value_into<'ctx>(
    compiler: &compiler::Compiler<'_, 'ctx>,
    value: Value<'ctx>,
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
        }
        Value::Primitive(p) => {
            compiler.builder.build_store(ptr, p.as_primitive_value());
        }
        Value::Str(s) => {
            let str_data = compiler
                .builder
                .build_struct_gep(string_struct(compiler.context), ptr, 0, "")
                .unwrap();
            compiler.builder.build_store(str_data, s.ptr);
            let len_ptr = compiler
                .builder
                .build_struct_gep(string_struct(compiler.context), ptr, 1, "")
                .unwrap();
            compiler.builder.build_store(len_ptr, s.len);
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
