pub mod closure;
pub mod enums;
pub mod tuple;

use std::fmt::Display;

use enumflags2::{bitflags, BitFlags};
use inkwell::{
    context::Context,
    types::{BasicTypeEnum, PointerType},
    values::{AnyValue, BasicValueEnum, IntValue, PointerValue},
};

use crate::{ast, compiler::Compiler};

use super::{
    core::CoreFunction,
    traits::{AsBasicType, DerefValue},
};

use self::tuple::{build_tuple_type, store_value_into};
pub use self::{closure::Closure, enums::Enum, tuple::Tuple};

#[derive(Debug, Clone, Copy)]
pub enum ValueType<'ctx> {
    Int,
    Bool,
    Str(IntValue<'ctx>),
    Closure(PointerType<'ctx>),
    Any(Enum<'ctx>),
    Tuple(ValueTypeHint, ValueTypeHint),
}

impl<'ctx> From<ValueType<'ctx>> for BitFlags<ValueTypeHint> {
    fn from(val: ValueType<'ctx>) -> Self {
        match val {
            ValueType::Int => ValueTypeHint::Int.into(),
            ValueType::Bool => ValueTypeHint::Bool.into(),
            ValueType::Str(_) => ValueTypeHint::Str.into(),
            ValueType::Closure(_) => ValueTypeHint::Closure.into(),
            ValueType::Any(a) => a.type_hint,
            ValueType::Tuple(a, b) => ValueTypeHint::Tuple.into(),
        }
    }
}

impl<'ctx> PartialEq for ValueType<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(_), Self::Str(_)) => true,
            (Self::Closure(_), Self::Closure(_)) => true,
            (Self::Int, Self::Int) | (Self::Bool, Self::Bool) => true,
            (Self::Any(a), Self::Any(b)) => a.type_hint == b.type_hint,

            _ => false,
        }
    }
}

#[bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ValueTypeHint {
    Int = 1 << 0,
    Bool = 1 << 1,
    Str = 1 << 2,
    Closure = 1 << 3,
    Tuple = 1 << 4,
}

impl ValueTypeHint {
    pub fn any() -> enumflags2::BitFlags<ValueTypeHint> {
        Self::Int | Self::Bool | Self::Str
    }
}

impl<'ctx> AsBasicType<'ctx> for ValueType<'ctx> {
    fn as_basic_type(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match *self {
            Self::Int => ctx.i32_type().into(),
            Self::Bool => ctx.bool_type().into(),
            Self::Str(_) => ctx.i8_type().ptr_type(Default::default()).into(),
            Self::Closure(t) => t.into(),
            Self::Any(_) => Enum::generic_type(ctx).ptr_type(Default::default()).into(),
            Self::Tuple(a, b) => todo!(),
        }
    }
}
impl<'ctx> AsBasicType<'ctx> for ValueTypeHint {
    fn as_basic_type(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Int => ctx.i32_type().into(),
            Self::Bool => ctx.bool_type().into(),
            Self::Str => ctx.i8_type().ptr_type(Default::default()).into(),
            Self::Closure => Enum::generic_type(ctx).ptr_type(Default::default()).into(),
            Self::Tuple => panic!("can't get basic type from tuple"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value<'ctx> {
    Primitive(Primitive<'ctx>),
    Str(Str<'ctx>),
    Closure(Closure<'ctx>),
    Boxed(Enum<'ctx>),
    Tuple(Tuple<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum ValueRef<'ctx> {
    Primitive(PrimitiveRef<'ctx>),
    Str(StrRef<'ctx>),
    Closure(Closure<'ctx>),
    Boxed(Enum<'ctx>),
    Tuple(Tuple<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum Primitive<'ctx> {
    Int(IntValue<'ctx>),
    Bool(IntValue<'ctx>),
}

impl<'ctx> Primitive<'ctx> {
    pub fn build_arith(
        compiler: &Compiler<'_, 'ctx>,
        lhs: Primitive<'ctx>,
        rhs: Primitive<'ctx>,
        op: ast::BinaryOp,
    ) -> Result<Self, &'static str> {
        Ok(match (lhs, rhs) {
            (Primitive::Int(l), Primitive::Int(r)) => match op {
                ast::BinaryOp::Add => Primitive::Int(compiler.builder.build_int_add(l, r, "")),
                ast::BinaryOp::Sub => Primitive::Int(compiler.builder.build_int_sub(l, r, "")),
                ast::BinaryOp::Mul => Primitive::Int(compiler.builder.build_int_mul(l, r, "")),
                ast::BinaryOp::Div => {
                    Primitive::Int(compiler.builder.build_int_signed_div(l, r, ""))
                }
                ast::BinaryOp::Rem => {
                    Primitive::Int(compiler.builder.build_int_signed_rem(l, r, ""))
                }
                ast::BinaryOp::Eq => Primitive::Bool(compiler.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    l,
                    r,
                    "",
                )),
                ast::BinaryOp::Neq => Primitive::Bool(compiler.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    l,
                    r,
                    "",
                )),
                ast::BinaryOp::Lt => Primitive::Bool(compiler.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    l,
                    r,
                    "",
                )),
                ast::BinaryOp::Lte => Primitive::Bool(compiler.builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    l,
                    r,
                    "",
                )),
                ast::BinaryOp::Gt => Primitive::Bool(compiler.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    l,
                    r,
                    "",
                )),
                ast::BinaryOp::Gte => Primitive::Bool(compiler.builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    l,
                    r,
                    "",
                )),
                _ => return Err("invalid operation between numbers"),
            },
            (Primitive::Bool(l), Primitive::Bool(r)) => Primitive::Bool(match op {
                ast::BinaryOp::Eq => {
                    compiler
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, l, r, "")
                }
                ast::BinaryOp::Neq => {
                    compiler
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, l, r, "tmpneq")
                }
                ast::BinaryOp::And => compiler.builder.build_and(l, r, ""),
                ast::BinaryOp::Or => compiler.builder.build_or(l, r, ""),
                _ => {
                    return Err("invalid operation between booleans");
                }
            }),
            (_l, _r) => return Err("bool and int operations are not allowed"),
        })
    }

    pub fn as_primitive_value(&self) -> BasicValueEnum<'ctx> {
        match *self {
            Self::Int(i) => i.into(),
            Self::Bool(b) => b.into(),
        }
    }
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

    pub fn build_str_append(compiler: &Compiler<'_, 'ctx>, lhs: Str<'ctx>, rhs: Str<'ctx>) -> Self {
        let size = compiler.builder.build_int_add(lhs.len, rhs.len, "");
        let append_buf = compiler
            .builder
            .build_array_alloca(compiler.context.i8_type(), size, "");
        compiler
            .builder
            .build_memset(append_buf, 1, compiler.context.i8_type().const_zero(), size)
            .unwrap();

        compiler
            .builder
            .build_memcpy(append_buf, 1, lhs.ptr, 1, lhs.len)
            .unwrap();
        // safety: we just allocated the right amount of memory
        let new_str_tail = unsafe {
            compiler
                .builder
                .build_gep(compiler.context.i8_type(), append_buf, &[lhs.len], "")
        };
        compiler
            .builder
            .build_memcpy(new_str_tail, 1, rhs.ptr, 1, rhs.len)
            .unwrap();

        Str::new(append_buf, size)
    }

    pub fn build_fmt_primitive(compiler: &Compiler<'_, 'ctx>, value: Primitive<'ctx>) -> Self {
        let number_buf = compiler.builder.build_array_alloca(
            compiler.context.i8_type(),
            compiler.context.i32_type().const_int(
                if matches!(value, Primitive::Int(_)) {
                    24
                } else {
                    6
                },
                false,
            ),
            "",
        );

        let number_len = compiler
            .builder
            .build_indirect_call(
                compiler.core_functions.get(CoreFunction::FmtInt).get_type(),
                compiler
                    .core_functions
                    .get(CoreFunction::FmtInt)
                    .as_global_value()
                    .as_pointer_value(),
                &[number_buf.into(), value.as_primitive_value().into()],
                "",
            )
            .as_any_value_enum()
            .into_int_value();

        Str::new(number_buf, number_len)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StrRef<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub len: IntValue<'ctx>,
}

impl<'ctx> Value<'ctx> {
    pub fn as_basic_value(&self) -> BasicValueEnum<'ctx> {
        match *self {
            Self::Primitive(p) => p.as_primitive_value(),
            Self::Str(str) => str.ptr.into(),
            Self::Closure(closure) => closure.funct.as_global_value().as_pointer_value().into(),
            Self::Boxed(_) => panic!("use boxed unwrap method to get the value"),
            Self::Tuple(_) => todo!(),
        }
    }

    pub fn get_type(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
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
            Value::Boxed(_) => Enum::generic_type(ctx).into(),
            Value::Tuple(t) => build_tuple_type(ctx, t.first_ty, t.second_ty).into(),
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
            Value::Boxed(a) => ValueType::Any(*a),
            Value::Tuple(tup) => todo!(),
        }
    }

    pub fn build_as_ref(&self, compiler: &Compiler<'_, 'ctx>, name: &str) -> ValueRef<'ctx> {
        let ty = self.get_type(compiler.context);
        let ptr: PointerValue<'_> = compiler.builder.build_alloca(ty, name);

        match self {
            Self::Boxed(b) => {
                // we need to copy the underlying data, not the pointer
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
            }
            Self::Tuple(t) => {
                let first_ptr = compiler.builder.build_struct_gep(ty, ptr, 0, "").unwrap();
                store_value_into(compiler, t.unwrap_first(compiler), first_ptr);

                let second_ptr = compiler.builder.build_struct_gep(ty, ptr, 1, "").unwrap();
                store_value_into(compiler, t.unwrap_second(compiler), second_ptr);

                // compiler
                //     .builder
                //     .build_memcpy(
                //         ptr,
                //         8,
                //         t.ptr,
                //         8,
                //         compiler.context.i32_type().const_int(24, false),
                //     )
                //     .unwrap();
            }
            _ => {
                compiler
                    .builder
                    .build_store(ptr, BasicValueEnum::from(self));
            }
        }

        match self {
            Self::Primitive(Primitive::Bool(_)) => ValueRef::Primitive(PrimitiveRef::Bool(ptr)),
            Self::Primitive(Primitive::Int(_)) => ValueRef::Primitive(PrimitiveRef::Int(ptr)),
            Self::Str(s) => ValueRef::Str(StrRef { ptr, len: s.len }),
            Self::Closure(closure) => ValueRef::Closure(*closure),
            Self::Boxed(b) => ValueRef::Boxed(Enum {
                ptr,
                type_hint: b.type_hint,
            }),
            Self::Tuple(t) => ValueRef::Tuple(Tuple {
                ptr,
                first_ty: t.first_ty,
                second_ty: t.second_ty,
            }),
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
            Self::Boxed(b) => b.ptr.get_type().into(),
            Self::Tuple(t) => panic!("can't get basic type from tuple"),
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
            Self::Boxed(b) => ValueType::Any(*b),
            Self::Tuple(t) => todo!(),
        }
    }

    pub fn cloned(&self, compiler: &Compiler<'_, 'ctx>) -> Self {
        self.build_deref(compiler).build_as_ref(compiler, "_")
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
            Self::Closure(closure) => Value::Closure(*closure),
            Self::Boxed(boxed) => Value::Boxed(*boxed),
            Self::Tuple(tup) => Value::Tuple(*tup),
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
            Value::Boxed(boxed) => boxed.ptr.into(),
            Value::Tuple(tup) => tup.ptr.into(),
        }
    }
}

impl<'ctx> Display for ValueType<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Int => write!(f, "int"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Str(_) => write!(f, "str"),
            ValueType::Closure(_) => write!(f, "closure"),
            ValueType::Any(_) => write!(f, "any"),
            ValueType::Tuple(a, b) => write!(f, "{}&{}", a, b),
        }
    }
}
impl Display for ValueTypeHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueTypeHint::Int => write!(f, "int"),
            ValueTypeHint::Bool => write!(f, "bool"),
            ValueTypeHint::Str => write!(f, "str"),
            ValueTypeHint::Closure => write!(f, "closure"),
            ValueTypeHint::Tuple => write!(f, "tuple"),
        }
    }
}
