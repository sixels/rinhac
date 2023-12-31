use std::rc::Rc;

use enumflags2::BitFlags;
use inkwell::{
    attributes::Attribute,
    module::Linkage,
    types::{BasicMetadataTypeEnum, StructType},
    values::{FunctionValue, PointerValue},
};

use crate::{
    codegen::{build_capture_args, traits::AsBasicType, FIRST_BLOCK_NAME},
    compiler::{
        environment::{Capture, Function, ScopeNode, Variable},
        Compiler,
    },
};

use super::{enums::Enum, Primitive, Str, Tuple, Value, ValueType, ValueTypeHint};

#[derive(Debug, Clone, Copy)]
pub struct Closure<'ctx> {
    pub funct: FunctionValue<'ctx>,
    pub captures: Option<PointerValue<'ctx>>,
    pub returns: BitFlags<ValueTypeHint>,
}

impl<'ctx> Closure<'ctx> {
    pub fn new(funct: FunctionValue<'ctx>, captures: Option<PointerValue<'ctx>>) -> Self {
        Self {
            funct,
            captures,
            returns: BitFlags::empty(),
        }
    }

    pub(crate) fn build_definition(
        compiler: &mut Compiler<'_, 'ctx>,
        function: &Function<'ctx>,
        arguments: &[(Value<'ctx>, bool)],
    ) -> (Self, Option<StructType<'ctx>>) {
        // get function name based on parameters types
        let params_known_types = arguments
            .iter()
            .zip(function.body.parameters.iter())
            .map(|((param, has_len), ast_param)| {
                (
                    (param.get_known_type(), has_len),
                    ast_param.text.to_string(),
                )
            })
            .collect::<Vec<_>>();
        let funct_name_monomorphized = if arguments.is_empty() {
            function.unique_name()
        } else {
            function.monomorph_name(
                params_known_types
                    .iter()
                    .map(|((p, l), _)| (p.clone(), **l)),
            )
        };

        // get llvm param metadatas
        let mut param_metadata = params_known_types
            .iter()
            .flat_map(|((t, &has_len), _)| {
                if has_len {
                    vec![
                        BasicMetadataTypeEnum::from(t.as_basic_type(compiler.context)),
                        compiler.context.i32_type().into(),
                    ]
                } else {
                    vec![BasicMetadataTypeEnum::from(
                        t.as_basic_type(compiler.context),
                    )]
                }
            })
            .collect::<Vec<_>>();

        // insert sret on params
        param_metadata.insert(
            0,
            Enum::generic_type(compiler.context)
                .ptr_type(Default::default())
                .into(),
        );

        // insert captures on params
        let captures_type = build_captures_struct(function, compiler);
        let captures_ptr = if let Some(captures_struct) = captures_type {
            param_metadata.insert(1, captures_struct.ptr_type(Default::default()).into());
            // extract captures
            let captures_ptr = if let Some(environ) = compiler
                .scope
                .get_indirect_captures_environ(&function.unique_name())
            {
                compiler
                    .builder
                    .build_load(captures_struct.ptr_type(Default::default()), environ, "")
                    .into_pointer_value()
            } else {
                let captures_ptr = compiler.builder.build_alloca(captures_struct, "cap");
                build_capture_args(compiler, function, captures_ptr, captures_struct);
                captures_ptr
            };

            Some(captures_ptr)
        } else {
            None
        };

        // define the function
        let signature = compiler.context.void_type().fn_type(&param_metadata, false);

        let prototype = compiler.module.add_function(
            &funct_name_monomorphized,
            signature,
            Some(Linkage::Internal),
        );
        // set sret attribute
        prototype.add_attribute(
            inkwell::attributes::AttributeLoc::Param(0),
            compiler.context.create_type_attribute(
                Attribute::get_named_enum_kind_id("sret"),
                Enum::generic_type(compiler.context).into(),
            ),
        );

        compiler
            .context
            .append_basic_block(prototype, FIRST_BLOCK_NAME);

        (Closure::new(prototype, captures_ptr), captures_type)
    }

    pub(crate) fn build_anonymous(
        compiler: &mut Compiler<'_, 'ctx>,
        function: &Function<'ctx>,
    ) -> (Self, Option<StructType<'ctx>>) {
        // // get llvm param metadatas
        let mut param_metadata = vec![
            Enum::generic_type(compiler.context)
                .ptr_type(Default::default())
                .into(),
            compiler
                .context
                .i8_type()
                .ptr_type(Default::default())
                .ptr_type(Default::default())
                .into(),
        ];

        // insert captures on params
        let captures_type = build_captures_struct(function, compiler);
        let captures_ptr = if let Some(captures_struct) = captures_type {
            param_metadata.insert(1, captures_struct.ptr_type(Default::default()).into());
            // extract captures
            let captures_ptr = if let Some(environ) = compiler
                .scope
                .get_indirect_captures_environ(&function.unique_name())
            {
                compiler
                    .builder
                    .build_load(captures_struct.ptr_type(Default::default()), environ, "")
                    .into_pointer_value()
            } else {
                let captures_ptr = compiler.builder.build_alloca(captures_struct, "cap");
                build_capture_args(compiler, function, captures_ptr, captures_struct);
                captures_ptr
            };

            Some(captures_ptr)
        } else {
            None
        };

        // define the function
        let signature = compiler.context.void_type().fn_type(&param_metadata, true);

        let prototype =
            compiler
                .module
                .add_function(&function.name, signature, Some(Linkage::Internal));

        // set sret attribute
        prototype.add_attribute(
            inkwell::attributes::AttributeLoc::Param(0),
            compiler.context.create_type_attribute(
                Attribute::get_named_enum_kind_id("sret"),
                Enum::generic_type(compiler.context).into(),
            ),
        );

        compiler
            .context
            .append_basic_block(prototype, FIRST_BLOCK_NAME);

        (Closure::new(prototype, captures_ptr), captures_type)
    }

    pub fn build_prepare_function(
        &mut self,
        compiler: &mut Compiler<'_, 'ctx>,
        function: &Function<'ctx>,
        arguments: Option<&[(Value<'ctx>, bool)]>,
        captures_type: Option<StructType<'ctx>>,
    ) {
        let params_known_types = arguments.map(|args| {
            args.iter()
                .zip(function.body.parameters.iter())
                .map(|((param, _), ast_param)| (param.get_known_type(), ast_param.text.to_string()))
                .collect::<Vec<_>>()
        });

        let fn_block = self.funct.get_first_basic_block().unwrap();
        let closure_scope = function
            .definition_scope
            .create_child(fn_block, Some(*self));

        let call_scope = Rc::clone(&compiler.scope);
        let call_block = compiler.builder.get_insert_block().unwrap();

        // enter scope
        compiler.scope.clone_from(&closure_scope);
        compiler
            .builder
            .position_at_end(closure_scope.borrow().block);
        // build function
        self.build_function(compiler, function, params_known_types, captures_type);
        // leave scope
        compiler.scope.clone_from(&call_scope);
        compiler.builder.position_at_end(call_block);
    }

    fn build_function(
        &mut self,
        compiler: &mut Compiler<'_, 'ctx>,
        function: &Function<'ctx>,
        funct_params: Option<Vec<(ValueType<'ctx>, String)>>,
        captures_struct: Option<StructType<'ctx>>,
    ) {
        let mut params =
            Vec::with_capacity(funct_params.as_ref().map(|p| p.len() + 2).unwrap_or(2));

        let skip_params = 1 + usize::from(self.captures.is_some());
        if let Some(funct_params) = funct_params {
            let mut i = skip_params as u32;
            for (ty, name) in funct_params.into_iter() {
                let param = self.funct.get_nth_param(i).unwrap();

                params.push((
                    name,
                    match ty {
                        ValueType::Int => {
                            Variable::Constant(Primitive::Int(param.into_int_value()).into())
                        }
                        ValueType::Bool => {
                            Variable::Constant(Primitive::Bool(param.into_int_value()).into())
                        }
                        ValueType::Str(_) => {
                            i += 1;
                            Variable::Constant(Value::Str(Str::new(
                                param.into_pointer_value(),
                                self.funct.get_nth_param((i) as _).unwrap().into_int_value(),
                            )))
                        }
                        ValueType::Any(a) => Variable::Value(super::ValueRef::Boxed(Enum {
                            ptr: param.into_pointer_value(),
                            type_hint: a.type_hint,
                        })),
                        ValueType::Tuple(t) => Variable::Value(super::ValueRef::Tuple(Tuple {
                            ptr: param.into_pointer_value(),
                            first_ty: t.0,
                            second_ty: t.1,
                        })),
                        ValueType::Closure(_) => todo!(),
                    },
                ));
                i += 1;
            }
        } else {
            for (param, name) in self
                .funct
                .get_param_iter()
                .skip(skip_params)
                .zip(function.body.parameters.iter().map(|a| &a.text))
            {
                params.push((
                    name.clone(),
                    Variable::Value(super::ValueRef::Boxed(Enum::from_ptr(
                        param.into_pointer_value(),
                    ))),
                ));
            }
        };

        if self.captures.is_some() {
            let captures_ref = self.funct.get_nth_param(1).unwrap().into_pointer_value();

            for (n, capture) in function.captured_variables.iter().enumerate() {
                let load_capture = if n == 0 {
                    captures_ref
                } else {
                    let ptr = unsafe {
                        compiler.builder.build_in_bounds_gep(
                            captures_struct.unwrap(),
                            captures_ref,
                            &[
                                compiler.context.i32_type().const_int(0, false),
                                compiler.context.i32_type().const_int(n as _, false),
                            ],
                            "",
                        )
                    };

                    ptr
                };

                match capture {
                    Capture::Direct { symbol, variable } => {
                        let known_type = variable.get_known_type();
                        let load_capture = compiler
                            .builder
                            .build_load(
                                match known_type {
                                    ValueType::Int => {
                                        compiler.context.i32_type().ptr_type(Default::default())
                                    }
                                    ValueType::Bool => {
                                        compiler.context.bool_type().ptr_type(Default::default())
                                    }
                                    ValueType::Str(_) => {
                                        compiler.context.i8_type().ptr_type(Default::default())
                                    }
                                    ValueType::Closure(_) => {
                                        todo!("closures won't be captured yet")
                                    }
                                    ValueType::Any(_) => Enum::generic_type(compiler.context)
                                        .ptr_type(Default::default()),
                                    ValueType::Tuple(_) => todo!(),
                                },
                                load_capture,
                                "",
                            )
                            .into_pointer_value();

                        let variable = match known_type {
                            ValueType::Int => Variable::Value(super::ValueRef::Primitive(
                                super::PrimitiveRef::Int(load_capture),
                            )),
                            ValueType::Bool => Variable::Value(super::ValueRef::Primitive(
                                super::PrimitiveRef::Bool(load_capture),
                            )),
                            ValueType::Str(len) => {
                                Variable::Value(super::ValueRef::Str(super::StrRef {
                                    ptr: load_capture,
                                    len,
                                }))
                            }
                            ValueType::Any(a) => Variable::Value(super::ValueRef::Boxed(Enum {
                                ptr: load_capture,
                                type_hint: a.type_hint,
                            })),
                            _ => todo!(),
                        };

                        compiler
                            .scope
                            .add_captured_variable(symbol.clone(), variable);
                    }
                    Capture::Indirect {
                        symbol: cl_symbol, ..
                    } => {
                        let mut scope = compiler.scope.borrow_mut();
                        scope
                            .indirect_captured_environment
                            .insert(cl_symbol.clone(), load_capture);
                    }
                }
            }
        }

        for (name, param) in params.into_iter() {
            compiler.scope.add_variable(name, param);
        }

        let mut sret = Enum::from_ptr(self.funct.get_nth_param(0).unwrap().into_pointer_value());
        let ret_value = compiler.compile_block(function.body.value.as_ref());

        sret.build_instance(compiler, &ret_value);
        self.returns = sret.type_hint;

        compiler.builder.build_return(None);
    }
}

pub fn build_captures_struct<'ctx>(
    function: &Function<'ctx>,
    compiler: &mut Compiler<'_, 'ctx>,
) -> Option<StructType<'ctx>> {
    if !function.captured_variables.is_empty() {
        // generate struct `{ ptr, ptr, ... }`
        let capture_fields =
            std::iter::repeat(compiler.context.i8_type().ptr_type(Default::default()))
                .take(function.captured_variables.len())
                .map(|t| t.into())
                .collect::<Vec<_>>();
        let captures = compiler.context.struct_type(&capture_fields, false);

        Some(captures)
    } else {
        None
    }
}
