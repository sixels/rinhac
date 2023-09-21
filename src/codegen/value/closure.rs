use std::rc::Rc;

use inkwell::{
    attributes::Attribute,
    module::Linkage,
    types::{BasicMetadataTypeEnum, PointerType},
    values::FunctionValue,
};

use crate::{
    ast,
    codegen::{enums::Enum, FIRST_BLOCK_NAME},
    compiler::{
        environment::{Function, ScopeNode, Variable},
        Compiler,
    },
};

use super::{Primitive, Str, Value, ValueType};

#[derive(Debug, Clone, Copy)]
pub struct Closure<'ctx> {
    pub funct: FunctionValue<'ctx>,
    pub captures: Option<PointerType<'ctx>>,
    // pub returns: ReturnType<'ctx>,
}

impl<'ctx> Closure<'ctx> {
    pub fn new(funct: FunctionValue<'ctx>, captures: Option<PointerType<'ctx>>) -> Self {
        Self {
            funct,
            captures,
            // returns: ReturnType::new(),
        }
    }

    pub fn build_load(&self, _compiler: &Compiler<'_, 'ctx>) -> Value<'ctx> {
        todo!()
    }

    pub(crate) fn build_definition(
        compiler: &mut Compiler<'_, 'ctx>,
        function: &Function<'ctx>,
        arguments: &[Value<'ctx>],
    ) -> Self {
        // get function name based on parameters types
        let params_known_types = arguments
            .iter()
            .zip(function.body.parameters.iter())
            .map(|(param, ast_param)| (param.get_known_type(), ast_param.text.to_string()))
            .collect::<Vec<_>>();
        let funct_name_monomorphized = if arguments.is_empty() {
            function.name.clone()
        } else {
            function.monomorph_name(params_known_types.iter().map(|(p, _)| p).copied())
        };

        // get llvm param metadatas
        let mut param_metadata = params_known_types
            .iter()
            .map(|(t, _)| BasicMetadataTypeEnum::from(t.as_basic_type(compiler.context)))
            .collect::<Vec<_>>();

        // insert sret on params
        param_metadata.insert(
            0,
            Enum::generic_type(compiler)
                .ptr_type(Default::default())
                .into(),
        );

        // insert captures on params
        let captures_type = if !function.captured_variables.is_empty() {
            // generate struct `{ ptr, ptr, ... }`
            let capture_fields =
                std::iter::repeat(compiler.context.i8_type().ptr_type(Default::default()))
                    .take(function.captured_variables.len())
                    .map(|t| t.into())
                    .collect::<Vec<_>>();
            let captures = compiler
                .context
                .struct_type(&capture_fields, false)
                .ptr_type(Default::default());

            param_metadata.insert(1, captures.into());

            Some(captures)
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
                Enum::generic_type(compiler).into(),
            ),
        );

        let fn_block = compiler
            .context
            .append_basic_block(prototype, FIRST_BLOCK_NAME);

        let closure = Closure::new(prototype, captures_type);

        // function declaration

        let closure_scope = function
            .definition_scope
            .create_child(fn_block, Some(closure));
        let call_scope = Rc::clone(&compiler.scope);

        // let mut captured_values = Vec::with_capacity(function.captured_variables.len());
        // for (var, symbol) in function.captured_variables.iter() {
        //     // captured_values.push()
        //     match var {
        //         Variable::Value(v) => {
        //             captured_values.push((v.get_known_type(), symbol.clone()));
        //         }
        //         Variable::Constant(c) => {
        //             compiler.scope.add_variable(
        //                 symbol.clone(),
        //                 Variable::Value(c.build_variable(compiler, symbol)),
        //             );
        //             captured_values.push((c.get_known_type(), symbol.clone()));
        //         }
        //         _ => panic!("can't capture yet"),
        //     }
        // }

        // enter scope
        compiler.scope.clone_from(&closure_scope);
        compiler
            .builder
            .position_at_end(closure_scope.borrow().block);
        // build function
        closure.build_function(
            compiler,
            params_known_types,
            &function.captured_variables,
            &function.body.value,
        );
        // leave scope
        compiler.scope.clone_from(&call_scope);
        compiler.builder.position_at_end(call_scope.borrow().block);

        closure
    }

    pub(crate) fn build_function(
        &self,
        compiler: &mut Compiler<'_, 'ctx>,
        params: Vec<(ValueType<'ctx>, String)>,
        captured_values: &[(ValueType<'ctx>, String)],
        body: &ast::Term,
    ) {
        // put parameters in current scope
        let params = self
            .funct
            .get_param_iter()
            .skip(1 + usize::from(self.captures.is_some()))
            .zip(params.into_iter())
            .map(|(param, (val, name))| {
                (
                    name,
                    match val {
                        ValueType::Int => {
                            Variable::Constant(Primitive::Int(param.into_int_value()).into())
                        }
                        ValueType::Bool => {
                            Variable::Constant(Primitive::Bool(param.into_int_value()).into())
                        }
                        ValueType::Str(len) => Variable::Constant(Value::Str(Str::new(
                            param.into_pointer_value(),
                            len,
                        ))),
                        ValueType::Closure(_) => todo!(),
                    },
                )
            });

        // todo: load captures and put them in current scope
        if self.captures.is_some() {
            let captures_ref = self.funct.get_nth_param(1).unwrap().into_pointer_value();

            let first_capture = compiler
                .builder
                .build_load(
                    match captured_values[0].0 {
                        ValueType::Int => compiler.context.i32_type().ptr_type(Default::default()),
                        ValueType::Bool => {
                            compiler.context.bool_type().ptr_type(Default::default())
                        }
                        ValueType::Str(_) => {
                            compiler.context.i8_type().ptr_type(Default::default())
                        }
                        _ => todo!(),
                    },
                    captures_ref,
                    "",
                )
                .into_pointer_value();

            let variable = match captured_values[0].0 {
                ValueType::Int => Variable::Value(super::ValueRef::Primitive(
                    super::PrimitiveRef::Int(first_capture),
                )),
                ValueType::Bool => Variable::Value(super::ValueRef::Primitive(
                    super::PrimitiveRef::Bool(first_capture),
                )),
                ValueType::Str(len) => Variable::Value(super::ValueRef::Str(super::StrRef {
                    ptr: first_capture,
                    len,
                })),
                _ => todo!(),
            };

            println!("capturing: {}", captured_values[0].1);
            compiler
                .scope
                .add_captured_variable(captured_values[0].1.clone(), variable);

            // self.captures.
            // let captures = compiler
            //     .builder
            //     .build_load(captures, "captures")
            //     .into_pointer_value();
            // captures
        }

        for (name, param) in params.into_iter() {
            compiler.scope.add_variable(name, param);
        }

        let mut next = Some(body);
        while let Some(term) = next {
            next = compiler.compile(term);
        }

        compiler.builder.build_return(None);
    }
}
