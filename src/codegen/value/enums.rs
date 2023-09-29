use enumflags2::BitFlags;
use inkwell::{
    basic_block::BasicBlock,
    context::Context,
    types::{BasicTypeEnum, IntType, StructType},
    values::{BasicValue, PointerValue},
};

use crate::{
    codegen::{core::CoreFunction, traits::AsBasicType},
    compiler::Compiler,
};

use super::{tuple::build_tuple_type, Primitive, Str, Value, ValueType, ValueTypeHint};

#[derive(Debug, Clone, Copy)]
pub struct Enum<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub type_hint: BitFlags<ValueTypeHint>,
}

type RuntimeMatchCallback<'a, 'ctx> =
    &'a dyn Fn(&'a Compiler<'a, 'ctx>, Value<'ctx>, BasicBlock<'ctx>);

impl<'ctx> Enum<'ctx> {
    pub fn build_new(compiler: &Compiler<'_, 'ctx>) -> Self {
        let enum_ptr = compiler
            .builder
            .build_alloca(Enum::generic_type(compiler.context), "");
        Self {
            ptr: enum_ptr,
            type_hint: BitFlags::empty(),
        }
    }

    pub fn from_ptr(ptr: PointerValue<'ctx>) -> Self {
        Self {
            ptr,
            type_hint: BitFlags::empty(),
        }
    }

    pub fn generic_type(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                // todo: update with tuple size
                context.i8_type().array_type(24).into(),
            ],
            false,
        )
    }

    pub fn variant_type(
        compiler: &Compiler<'_, 'ctx>,
        padding: IntType<'ctx>,
        pad_size: u32,
        data_type: BasicTypeEnum<'_>,
    ) -> StructType<'ctx> {
        compiler
            .context
            .struct_type(&[padding.array_type(pad_size).into(), data_type], false)
    }

    pub fn build_instance(&mut self, compiler: &Compiler<'_, 'ctx>, value: &Value<'ctx>) {
        let value_type = value.get_known_type();
        let (tag, pad, data_type) = match &value_type {
            ValueType::Any(a) => {
                compiler
                    .builder
                    .build_memcpy(
                        self.ptr,
                        8,
                        a.ptr,
                        8,
                        compiler.context.i32_type().const_int(24, false),
                    )
                    .unwrap();
                self.type_hint = a.type_hint;

                return;
            }
            ValueType::Bool => (
                ValueTypeHint::Bool,
                (compiler.context.i8_type(), 1),
                value.get_type(compiler.context),
            ),
            ValueType::Int => (
                ValueTypeHint::Int,
                (compiler.context.i32_type(), 1),
                value.get_type(compiler.context),
            ),
            ValueType::Str(_) => (
                ValueTypeHint::Str,
                (compiler.context.i64_type(), 1),
                compiler
                    .context
                    .struct_type(
                        &[
                            compiler
                                .context
                                .i8_type()
                                .ptr_type(Default::default())
                                .into(),
                            compiler.context.i32_type().into(),
                        ],
                        false,
                    )
                    .into(),
            ),
            ValueType::Closure(_) => (
                ValueTypeHint::Closure,
                (compiler.context.i32_type(), 1),
                compiler.context.i32_type().into(),
            ),
            ValueType::Tuple(t) => (
                ValueTypeHint::Tuple,
                (compiler.context.i8_type(), 10),
                compiler
                    .context
                    .struct_type(
                        &[
                            compiler.context.i8_type().into(),
                            compiler.context.i8_type().into(),
                            build_tuple_type(compiler.context, &t.0, &t.1).into(),
                        ],
                        false,
                    )
                    .into(),
            ),
        };

        let value_type_hints: BitFlags<ValueTypeHint> = value_type.into();
        self.type_hint |= value_type_hints;

        let data = compiler
            .builder
            .build_struct_gep(
                Enum::variant_type(compiler, pad.0, pad.1, data_type),
                self.ptr,
                1,
                "",
            )
            .unwrap();

        match value {
            Value::Str(s) => {
                let str_data = compiler
                    .builder
                    .build_struct_gep(
                        compiler.context.struct_type(
                            &[
                                compiler
                                    .context
                                    .i8_type()
                                    .ptr_type(Default::default())
                                    .into(),
                                compiler.context.i32_type().into(),
                            ],
                            false,
                        ),
                        data,
                        0,
                        "",
                    )
                    .unwrap();
                compiler.builder.build_store(str_data, s.ptr);

                let len_ptr = compiler
                    .builder
                    .build_struct_gep(
                        compiler.context.struct_type(
                            &[
                                compiler
                                    .context
                                    .i8_type()
                                    .ptr_type(Default::default())
                                    .into(),
                                compiler.context.i32_type().into(),
                            ],
                            false,
                        ),
                        data,
                        1,
                        "",
                    )
                    .unwrap();
                compiler.builder.build_store(len_ptr, s.len);
            }
            Value::Tuple(_t) => {
                todo!()
            }
            _ => {
                compiler
                    .builder
                    .build_store(data, value.as_basic_value())
                    .set_alignment(8)
                    .unwrap();
            }
        };

        compiler
            .builder
            .build_store(
                self.ptr,
                compiler.context.i8_type().const_int(tag as u8 as _, false),
            )
            .set_alignment(8)
            .unwrap();
    }

    pub fn unwrap_variant(
        &self,
        compiler: &Compiler<'_, 'ctx>,
        variant: ValueTypeHint,
    ) -> Value<'ctx> {
        let (pad, data_type) = match variant {
            ValueTypeHint::Bool => (
                (compiler.context.i8_type(), 1),
                compiler.context.bool_type().into(),
            ),
            ValueTypeHint::Int => (
                (compiler.context.i32_type(), 1),
                compiler.context.i32_type().into(),
            ),
            ValueTypeHint::Str => (
                (compiler.context.i64_type(), 1),
                compiler
                    .context
                    .struct_type(
                        &[
                            compiler
                                .context
                                .i8_type()
                                .ptr_type(Default::default())
                                .into(),
                            compiler.context.i32_type().into(),
                        ],
                        false,
                    )
                    .into(),
            ),
            ValueTypeHint::Closure => (
                (compiler.context.i32_type(), 1),
                compiler.context.i32_type().into(),
            ),
            ValueTypeHint::Tuple => todo!(),
        };

        let data = compiler
            .builder
            .build_struct_gep(
                Enum::variant_type(compiler, pad.0, pad.1, data_type),
                self.ptr,
                1,
                "",
            )
            .unwrap();

        match variant {
            ValueTypeHint::Int => Primitive::Int(
                compiler
                    .builder
                    .build_load(variant.as_basic_type(compiler.context), data, "")
                    .into_int_value(),
            )
            .into(),
            ValueTypeHint::Bool => Primitive::Bool(
                compiler
                    .builder
                    .build_load(variant.as_basic_type(compiler.context), data, "")
                    .into_int_value(),
            )
            .into(),
            ValueTypeHint::Str => {
                let str_data = compiler
                    .builder
                    .build_struct_gep(
                        compiler.context.struct_type(
                            &[
                                compiler
                                    .context
                                    .i8_type()
                                    .ptr_type(Default::default())
                                    .into(),
                                compiler.context.i32_type().into(),
                            ],
                            false,
                        ),
                        data,
                        0,
                        "",
                    )
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
                    .build_struct_gep(
                        compiler.context.struct_type(
                            &[
                                compiler
                                    .context
                                    .i8_type()
                                    .ptr_type(Default::default())
                                    .into(),
                                compiler.context.i32_type().into(),
                            ],
                            false,
                        ),
                        data,
                        1,
                        "",
                    )
                    .unwrap();
                let len = compiler
                    .builder
                    .build_load(compiler.context.i32_type(), len_ptr, "")
                    .into_int_value();

                let str_clone =
                    compiler
                        .builder
                        .build_array_alloca(compiler.context.i8_type(), len, "");
                compiler
                    .builder
                    .build_memcpy(str_clone, 1, str_ptr, 1, len)
                    .unwrap();

                Str {
                    ptr: str_clone,
                    len,
                }
                .into()
            }
            ValueTypeHint::Closure => todo!(),
            //  Value::Closure(
            // compiler
            //     .builder
            //     .build_load(variant.as_basic_type(compiler.context), data, "")
            //     .into_int_value(),
            // ),
            ValueTypeHint::Tuple => todo!(),
        }
    }

    pub fn build_runtime_unwrap(
        &self,
        compiler: &Compiler<'_, 'ctx>,
        expected: ValueTypeHint,
    ) -> Value<'ctx> {
        let tag = {
            let tag = compiler
                .builder
                .build_load(compiler.context.i8_type(), self.ptr, "")
                .into_int_value();
            tag.as_instruction_value()
                .unwrap()
                .set_alignment(8)
                .unwrap();
            compiler
                .builder
                .build_int_z_extend(tag, compiler.context.i64_type(), "")
        };

        let expected_tag = compiler
            .context
            .i64_type()
            .const_int(expected as u8 as _, false);

        let current_block = compiler.builder.get_insert_block().unwrap();
        let else_block = compiler
            .context
            .insert_basic_block_after(current_block, "bb");
        let then_block = compiler
            .context
            .insert_basic_block_after(current_block, "bb");

        compiler.builder.build_conditional_branch(
            compiler
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, tag, expected_tag, ""),
            then_block,
            else_block,
        );

        compiler.builder.position_at_end(else_block);

        compiler
            .builder
            .build_call(compiler.core_functions.get(CoreFunction::Panic), &[], "");
        compiler.builder.build_unreachable();

        compiler.builder.position_at_end(then_block);
        self.unwrap_variant(compiler, expected)
    }

    pub fn build_runtime_match<'a>(
        &self,
        compiler: &'a Compiler<'a, 'ctx>,
        merge_block: BasicBlock<'ctx>,
        match_map: &[(ValueTypeHint, RuntimeMatchCallback<'a, 'ctx>)],
    ) {
        let scope = compiler.scope.borrow();

        // create blocks for each match
        let blocks = match_map
            .iter()
            .map(|(expected, _)| {
                let expected_tag = compiler
                    .context
                    .i64_type()
                    .const_int(*expected as u8 as _, false);

                (
                    expected_tag,
                    compiler.context.prepend_basic_block(merge_block, "bb"),
                )
            })
            .collect::<Vec<_>>();
        // create a fallback block
        let else_block = compiler.context.prepend_basic_block(merge_block, "else");

        // get the tag to match against
        let tag = {
            let tag = compiler
                .builder
                .build_load(compiler.context.i8_type(), self.ptr, "")
                .into_int_value();
            tag.as_instruction_value()
                .unwrap()
                .set_alignment(8)
                .unwrap();
            compiler
                .builder
                .build_int_z_extend(tag, compiler.context.i64_type(), "")
        };

        // create the switch instruction
        compiler.builder.build_switch(tag, else_block, &blocks);

        drop(scope);

        // build the blocks
        for ((check_tag, codegen), (_, block)) in match_map.iter().zip(blocks.into_iter()) {
            compiler.builder.position_at_end(block);
            let value = self.unwrap_variant(compiler, *check_tag);
            codegen(compiler, value, else_block);
            if block.get_terminator().is_none() {
                compiler.builder.build_unconditional_branch(merge_block);
            }
        }

        compiler.builder.position_at_end(else_block);
        compiler
            .builder
            .build_call(compiler.core_functions.get(CoreFunction::Panic), &[], "");
        compiler.builder.build_unreachable();

        // compiler.builder.position_at_end(merge_block);
    }
}

impl<'ctx> From<Enum<'ctx>> for Value<'ctx> {
    fn from(val: Enum<'ctx>) -> Self {
        Value::Boxed(val)
    }
}
