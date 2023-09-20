pub mod enums;
pub mod traits;
pub mod value;

use std::rc::Rc;

use inkwell::{
    attributes::Attribute,
    module::Linkage,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, PointerValue},
};

use crate::{
    ast,
    codegen::{enums::Enum, value::Closure},
    compiler::{
        environment::{Function, ScopeNode, Variable},
        Compiler, CoreFunction,
    },
};

use self::{
    traits::{Codegen, DerefValue},
    value::{Primitive, Str, Value},
};

pub const FIRST_BLOCK_NAME: &str = "start";

impl Codegen for ast::Int {
    type R<'ctx> = Primitive<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        Primitive::Int(compiler.context.i32_type().const_int(self.value as _, true))
    }
}

impl Codegen for ast::Bool {
    type R<'ctx> = Primitive<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        Primitive::Bool(
            compiler
                .context
                .bool_type()
                .const_int(self.value as _, false),
        )
    }
}

impl Codegen for ast::Str {
    type R<'ctx> = Str<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        if let Some(string) = compiler.strings.get(&self.value).copied() {
            return string;
        }

        let len_value = compiler
            .context
            .i32_type()
            .const_int(self.value.len() as u64, false);

        let global_string = compiler.builder.build_global_string_ptr(&self.value, "str");

        let str = Str::new(global_string.as_pointer_value(), len_value);
        compiler.strings.insert(self.value.clone(), str);

        str
    }
}

impl Codegen for ast::Var {
    type R<'ctx> = Variable<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let var = compiler
            .scope
            .find_variable(&self.text)
            .unwrap_or_else(|| panic!("Variable {} not defined", self.text));

        var.clone()
    }
}

impl Codegen for ast::Binary {
    type R<'ctx> = Value<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let mut resolve_value = |term: &ast::Term| -> Value<'_> {
            match term {
                ast::Term::Int(i) => i.codegen(compiler).into(),
                ast::Term::Bool(b) => b.codegen(compiler).into(),
                ast::Term::Binary(b) => b.codegen(compiler),
                ast::Term::Var(v) => match v.codegen(compiler) {
                    Variable::Value(v) => v.build_deref(compiler),
                    Variable::Constant(c) => c,
                    Variable::Function(_) => panic!("can't perform operations on functions"),
                },
                ast::Term::Str(s) => s.codegen(compiler).into(),
                _ => unimplemented!(),
            }
        };

        let lhs_value = resolve_value(&self.lhs);
        let rhs_value = resolve_value(&self.rhs);

        let result = match (lhs_value, rhs_value) {
            (Value::Primitive(pl), Value::Primitive(pr)) => match (pl, pr) {
                (Primitive::Int(l), Primitive::Int(r)) => match self.op {
                    ast::BinaryOp::Add => {
                        Primitive::Int(compiler.builder.build_int_add(l, r, "tmpadd"))
                    }
                    ast::BinaryOp::Sub => {
                        Primitive::Int(compiler.builder.build_int_sub(l, r, "tmpsub"))
                    }
                    ast::BinaryOp::Mul => {
                        Primitive::Int(compiler.builder.build_int_mul(l, r, "tmpmul"))
                    }
                    ast::BinaryOp::Div => {
                        Primitive::Int(compiler.builder.build_int_signed_div(l, r, "tmpdiv"))
                    }
                    ast::BinaryOp::Rem => {
                        Primitive::Int(compiler.builder.build_int_signed_rem(l, r, "tmprem"))
                    }
                    ast::BinaryOp::Eq => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        l,
                        r,
                        "tmpeq",
                    )),
                    ast::BinaryOp::Neq => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        l,
                        r,
                        "tmpneq",
                    )),
                    ast::BinaryOp::Lt => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        l,
                        r,
                        "tmplte",
                    )),
                    ast::BinaryOp::Lte => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SLE,
                        l,
                        r,
                        "tmplte",
                    )),
                    ast::BinaryOp::Gt => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGT,
                        l,
                        r,
                        "tmpgte",
                    )),
                    ast::BinaryOp::Gte => Primitive::Bool(compiler.builder.build_int_compare(
                        inkwell::IntPredicate::SGE,
                        l,
                        r,
                        "tmpgte",
                    )),
                    _ => panic!("invalid operation between terms"),
                }
                .into(),
                (Primitive::Bool(l), Primitive::Bool(r)) => Primitive::Bool(match self.op {
                    ast::BinaryOp::Eq => {
                        compiler
                            .builder
                            .build_int_compare(inkwell::IntPredicate::EQ, l, r, "tmpeq")
                    }
                    ast::BinaryOp::Neq => compiler.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        l,
                        r,
                        "tmpneq",
                    ),
                    ast::BinaryOp::And => compiler.builder.build_and(l, r, "tmpand"),
                    ast::BinaryOp::Or => compiler.builder.build_or(l, r, "tmpor"),
                    _ => panic!("type bool does not support the \"{:?}\" operation", self.op),
                })
                .into(),
                (_l, _r) => panic!("bool and int operations are not allowed"),
            },
            (Value::Str(l), Value::Str(r)) => match self.op {
                ast::BinaryOp::Add => {
                    let size = l.len.const_add(r.len);
                    let new_str =
                        compiler
                            .builder
                            .build_array_alloca(compiler.context.i8_type(), size, "");

                    compiler
                        .builder
                        .build_memcpy(new_str, 1, l.ptr, 1, l.len)
                        .unwrap();
                    // safety: we just allocated the right amount of memory
                    let new_str_tail = unsafe {
                        compiler.builder.build_gep(
                            compiler.context.i8_type(),
                            new_str,
                            &[l.len],
                            "",
                        )
                    };
                    compiler
                        .builder
                        .build_memcpy(new_str_tail, 1, r.ptr, 1, r.len)
                        .unwrap();

                    Str::new(new_str, size).into()
                }
                _ => todo!(),
            },
            _ => todo!(),
        };
        result
    }
}

impl Codegen for ast::Print {
    type R<'ctx> = CallSiteValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        // Target::initialize_all(&InitializationConfig::default());
        let param = match self.value.as_ref() {
            ast::Term::Int(i) => i.codegen(compiler).into(),
            ast::Term::Bool(b) => b.codegen(compiler).into(),
            ast::Term::Str(s) => s.codegen(compiler).into(),
            ast::Term::Binary(b) => b.codegen(compiler),
            ast::Term::Var(v) => match v.codegen(compiler) {
                Variable::Value(v) => v.build_deref(compiler),
                Variable::Constant(c) => c,
                Variable::Function(_) => todo!("can't print functions yet"),
            },
            _ => todo!(),
        };

        let (funct, params) = match param {
            Value::Str(str) => (CoreFunction::PrintStr, vec![str.ptr.into(), str.len.into()]),
            Value::Primitive(primitive) => match primitive {
                Primitive::Bool(bool) => (CoreFunction::PrintBool, vec![bool.into()]),
                Primitive::Int(int) => (CoreFunction::PrintInt, vec![int.into()]),
            },
            Value::Closure(_closure) => todo!("should print '<#closure>'"),
        };

        let funct = &compiler.core_functions[funct];
        let funct_pointer = funct.as_global_value().as_pointer_value();
        let ret =
            compiler
                .builder
                .build_indirect_call(funct.get_type(), funct_pointer, &params, "print");

        ret
    }
}

impl Codegen for ast::Call {
    type R<'ctx> = PointerValue<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let funct_name = match self.callee.as_ref() {
            ast::Term::Var(v) => v.text.as_ref(),
            _ => panic!("invalid function call"),
        };

        let Some(variable) = compiler.scope.find_variable(funct_name)  else {
            panic!("function not found")
        };

        let Variable::Function(funct) = variable else {
            panic!("variable {} is not callable", funct_name)
        };

        let mut resolve_value = |term: &ast::Term| -> Value<'_> {
            match term {
                ast::Term::Int(i) => i.codegen(compiler).into(),
                ast::Term::Bool(b) => b.codegen(compiler).into(),
                ast::Term::Binary(b) => b.codegen(compiler),
                ast::Term::Var(v) => match v.codegen(compiler) {
                    Variable::Value(v) => v.build_deref(compiler),
                    Variable::Constant(c) => c,
                    Variable::Function(_) => panic!("can't perform operations on functions"),
                },
                ast::Term::Str(s) => s.codegen(compiler).into(),
                _ => unimplemented!(),
            }
        };

        let arguments = self
            .arguments
            .iter()
            .map(|arg| resolve_value(arg))
            .collect::<Vec<_>>();

        let arguments_types = arguments
            .iter()
            .map(|arg| arg.get_type())
            .collect::<Vec<_>>();

        let closure = funct
            .borrow_mut()
            .get_or_insert_definition(&arguments_types, |funct_ref| {
                // bad end: we need to create a new function

                let param_names = funct_ref
                    .body
                    .parameters
                    .iter()
                    .map(|p| p.text.clone())
                    .collect::<Vec<_>>();

                let mut param_types = arguments
                    .iter()
                    .map(|param| BasicMetadataTypeEnum::from(param.get_type()))
                    .collect::<Vec<_>>();

                let funct_name_monomorphized = if arguments.len() == 1 {
                    String::from(funct_name)
                } else {
                    format!(
                        "{}_{}",
                        funct_name,
                        param_types
                            .iter()
                            .map(|pt| pt.to_string())
                            .collect::<Vec<_>>()
                            .join("_")
                    )
                };

                param_types.insert(
                    0,
                    Enum::generic_type(compiler)
                        .ptr_type(Default::default())
                        .into(),
                );

                let signature = compiler.context.void_type().fn_type(&param_types, false);
                let prototype = compiler.module.add_function(
                    &funct_name_monomorphized,
                    signature,
                    Some(Linkage::Internal),
                );

                prototype.add_attribute(
                    inkwell::attributes::AttributeLoc::Param(0),
                    compiler.context.create_type_attribute(
                        Attribute::get_named_enum_kind_id("sret"),
                        Enum::generic_type(compiler).into(),
                    ),
                );
                let closure = Closure::new(prototype);

                let fn_block = compiler
                    .context
                    .append_basic_block(prototype, FIRST_BLOCK_NAME);

                let param_values = closure
                    .funct
                    .get_param_iter()
                    .skip(1)
                    .zip(arguments.iter())
                    .map(|(param, val)| match val {
                        Value::Primitive(Primitive::Int(_)) => {
                            Value::Primitive(Primitive::Int(param.into_int_value()))
                        }
                        Value::Primitive(Primitive::Bool(_)) => {
                            Value::Primitive(Primitive::Bool(param.into_int_value()))
                        }
                        Value::Str(str) => {
                            Value::Str(Str::new(param.into_pointer_value(), str.len))
                        }
                        Value::Closure(_) => todo!(),
                    })
                    .zip(param_names)
                    .collect::<Vec<(Value, String)>>();

                let closure_scope = funct_ref
                    .definition_scope
                    .create_child(fn_block, Some(closure));
                let call_scope = Rc::clone(&compiler.scope);

                compiler.scope.clone_from(&closure_scope);
                compiler
                    .builder
                    .position_at_end(closure_scope.borrow().block);

                let function_body = &funct_ref.body.value;
                closure.build_definition(compiler, param_values, function_body);

                compiler.scope.clone_from(&call_scope);
                compiler.builder.position_at_end(call_scope.borrow().block);

                closure
            });

        // happy end: we already defined this function for the given argument types
        let mut args = arguments
            .iter()
            .map(|arg| BasicValueEnum::from(arg).into())
            .collect::<Vec<BasicMetadataValueEnum>>();

        let sret = compiler
            .builder
            .build_alloca(Enum::generic_type(compiler), "");
        args.insert(0, sret.into());

        compiler.builder.build_direct_call(closure.funct, &args, "");

        sret
    }
}

impl Codegen for ast::Let {
    type R<'ctx> = Variable<'ctx>;
    fn codegen<'ctx>(&self, compiler: &mut Compiler<'_, 'ctx>) -> Self::R<'ctx> {
        let value_term = &self.value;
        let value = match value_term.as_ref() {
            ast::Term::Int(i) => i.codegen(compiler).into(),
            ast::Term::Bool(s) => s.codegen(compiler).into(),
            ast::Term::Str(s) => Value::Str(s.codegen(compiler)),
            ast::Term::Binary(b) => b.codegen(compiler),
            ast::Term::Function(f) => {
                let funct = Variable::Function(Function::new(f.clone(), &compiler.scope));
                compiler.scope.add_variable(&self.name.text, funct.clone());
                return funct;
            }
            _ => unimplemented!(),
        };

        let variable = value.build_variable(compiler, &self.name.text);

        compiler
            .scope
            .add_variable(&self.name.text, Variable::Value(variable));

        Variable::Value(variable)
    }
}
