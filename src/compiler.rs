pub mod environment;

use std::{collections::HashMap, path::Path};

use enum_map::{Enum, EnumMap};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{InitializationConfig, Target, TargetTriple},
    values::FunctionValue,
    AddressSpace,
};

use crate::{
    ast,
    codegen::{traits::Codegen, value::Str, FIRST_BLOCK_NAME},
};

use self::environment::{Scope, Scopes};

pub struct Rinhac {}

impl Rinhac {
    pub fn compile(ast: ast::File) {
        let context = Context::create();
        let module = context.create_module(&ast.name);

        let triple = TargetTriple::create(current_platform::CURRENT_PLATFORM);
        module.set_triple(&triple);

        let builder = context.create_builder();
        let core = Self::define_core_functions(&context, &module);

        let mut compiler = Compiler::new(&context, &module, core, builder);

        let mut next = Some(&ast.expression);
        while let Some(next_term) = next {
            next = compiler.compile(next_term);
        }

        compiler.finalize();

        println!("GENERATED IR:\n{}", module.print_to_string().to_string(),)
    }

    pub fn define_core_functions<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> EnumMap<CoreFunction, FunctionValue<'ctx>> {
        let print_str_type = context.i32_type().fn_type(
            &[
                context.i8_type().ptr_type(AddressSpace::from(0)).into(),
                context.i32_type().into(),
            ],
            false,
        );
        let print_str_prototype = module.add_function(
            CoreFunction::PrintStr.into(),
            print_str_type,
            Some(Linkage::External),
        );

        let print_int_type = context
            .i32_type()
            .fn_type(&[context.i32_type().into()], false);
        let print_int_prototype = module.add_function(
            CoreFunction::PrintInt.into(),
            print_int_type,
            Some(Linkage::External),
        );

        let print_bool_type = context
            .i32_type()
            .fn_type(&[context.bool_type().into()], false);
        let print_bool_prototype = module.add_function(
            CoreFunction::PrintBool.into(),
            print_bool_type,
            Some(Linkage::External),
        );

        enum_map::enum_map! {
            CoreFunction::PrintStr => print_str_prototype,
            CoreFunction::PrintInt => print_int_prototype,
            CoreFunction::PrintBool => print_bool_prototype,
        }
    }
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub entry_function: FunctionValue<'ctx>,
    pub function: Option<FunctionValue<'ctx>>,
    pub core_functions: EnumMap<CoreFunction, FunctionValue<'ctx>>,
    pub strings: HashMap<String, Str<'ctx>>,
    pub scope: Scopes<'ctx>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        core_functions: EnumMap<CoreFunction, FunctionValue<'ctx>>,
        builder: Builder<'ctx>,
    ) -> Self {
        builder.clear_insertion_position();
        let main_type = context.void_type().fn_type(&[], false);
        let main_prototype = module.add_function("main", main_type, Some(Linkage::External));

        let entry_block = context.append_basic_block(main_prototype, FIRST_BLOCK_NAME);
        builder.position_at_end(entry_block);

        Self {
            context,
            module,
            builder,
            entry_function: main_prototype,
            function: None,
            core_functions,
            strings: HashMap::new(),
            scope: Scopes::new(Scope::new("main", entry_block, None)),
        }
    }

    pub fn compile<'t>(&mut self, term: &'t ast::Term) -> Option<&'t ast::Term> {
        match term {
            ast::Term::Print(print) => {
                print.codegen(self);
                None
            }
            ast::Term::Let(binding) => {
                binding.codegen(self);
                Some(&binding.next)
            }
            // ignore top-level values for now
            ast::Term::Var(..)
            | ast::Term::Tuple(..)
            | ast::Term::Binary(..)
            | ast::Term::Bool(..)
            | ast::Term::Int(..)
            | ast::Term::Str(..) => None,
            _ => todo!(),
        }
    }

    pub fn finalize(&self) {
        self.builder
            .position_at_end(self.entry_function.get_last_basic_block().unwrap());
        self.builder.build_return(None);

        if !self.entry_function.verify(true) {
            // todo: error handling
            // self.entry_function.print_to_stderr();
            panic!("compilation failed: invalid entry-block code");
        }

        Target::initialize_all(&InitializationConfig::default());

        let target = Target::from_triple(&self.module.get_triple()).unwrap();

        let tm = target
            .create_target_machine(
                &self.module.get_triple(),
                "generic",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Medium,
            )
            .unwrap();
        tm.write_to_file(
            self.module,
            inkwell::targets::FileType::Object,
            Path::new("output.o"),
        )
        .unwrap();
    }
}

#[derive(Debug, Enum)]
pub enum CoreFunction {
    PrintStr,
    PrintInt,
    PrintBool,
}

impl From<CoreFunction> for &'static str {
    fn from(funct: CoreFunction) -> Self {
        match funct {
            CoreFunction::PrintStr => "__rinha_print_str",
            CoreFunction::PrintInt => "__rinha_print_int",
            CoreFunction::PrintBool => "__rinha_print_bool",
        }
    }
}
