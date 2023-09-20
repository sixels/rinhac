use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::{
    ast,
    compiler::{
        environment::{ScopeNode, ScopeRc, Variable},
        Compiler,
    },
};

use super::traits::{Codegen, DerefValue};

pub enum ValueType {
    Int,
    Bool,
    Str,
    Closure,
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

#[derive(Debug, Clone, Copy)]
pub struct Closure<'ctx> {
    pub funct: FunctionValue<'ctx>,
    // pub returns: ReturnType<'ctx>,
}

impl<'ctx> Closure<'ctx> {
    pub fn new(funct: FunctionValue<'ctx>) -> Self {
        Self {
            funct,
            // returns: ReturnType::new(),
        }
    }
    pub fn build_load(&self, _compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        todo!()
    }

    pub(crate) fn build_definition(
        &self,
        compiler: &mut Compiler<'_, 'ctx>,
        params: Vec<(Value<'ctx>, String)>,
        body: &ast::Term,
    ) {
        // put parameters in current scope
        let params = self
            .funct
            .get_param_iter()
            .skip(1)
            .zip(params.into_iter())
            .map(|(param, (val, name))| {
                (
                    name,
                    match val {
                        Value::Primitive(Primitive::Int(_)) => {
                            Variable::Constant(Primitive::Int(param.into_int_value()).into())
                        }
                        Value::Primitive(Primitive::Bool(_)) => {
                            Variable::Constant(Primitive::Bool(param.into_int_value()).into())
                        }
                        Value::Str(str) => Variable::Constant(Value::Str(Str::new(
                            param.into_pointer_value(),
                            str.len,
                        ))),
                        Value::Closure(_) => todo!(),
                    },
                )
            });

        for (name, param) in params.into_iter() {
            compiler.scope.add_variable(name, param);
        }

        let mut next = Some(body);
        while let Some(term) = next {
            next = match term {
                ast::Term::Print(print) => {
                    print.codegen(compiler);
                    None
                }
                ast::Term::Let(binding) => {
                    binding.codegen(compiler);
                    Some(&binding.next)
                }

                ast::Term::Call(call) => {
                    call.codegen(compiler);
                    None
                }

                // ignore top-level values for now
                ast::Term::Var(..)
                | ast::Term::Tuple(..)
                | ast::Term::Binary(..)
                | ast::Term::Bool(..)
                | ast::Term::Int(..)
                | ast::Term::Str(..) => None,
                _ => todo!(),
            };
        }

        compiler.builder.build_return(None);
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
