use std::{collections::HashMap, path::Path};

use enum_map::{Enum, EnumMap};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{InitializationConfig, Target, TargetTriple},
    values::{FunctionValue, GlobalValue},
    AddressSpace,
};

use crate::{ast, codegen::Codegen};

pub struct Rinhac {}

impl Rinhac {
    pub fn compile(ast: ast::File) {
        let context = Context::create();
        let module = context.create_module(&ast.name);

        let triple = TargetTriple::create(current_platform::CURRENT_PLATFORM);
        module.set_triple(&triple);

        let prelude = Self::define_prelude_functions(&context, &module);
        let compiler = Compiler::new(&context, &module, prelude);
        compiler.compile(&ast.expression);
        compiler.finalize();

        println!("GENERATED IR:\n{}", module.print_to_string().to_string(),)
    }

    pub fn define_prelude_functions<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> EnumMap<RTFunction, FunctionValue<'ctx>> {
        let print_str_type = context.i32_type().fn_type(
            &[
                context.i8_type().ptr_type(AddressSpace::from(0)).into(),
                context.i32_type().into(),
            ],
            false,
        );
        let print_str_prototype = module.add_function(
            RTFunction::PrintStr.into(),
            print_str_type,
            Some(Linkage::External),
        );

        let print_int_type = context
            .i32_type()
            .fn_type(&[context.i32_type().into()], false);
        let print_int_prototype = module.add_function(
            RTFunction::PrintInt.into(),
            print_int_type,
            Some(Linkage::External),
        );

        let print_bool_type = context
            .i32_type()
            .fn_type(&[context.bool_type().into()], false);
        let print_bool_prototype = module.add_function(
            RTFunction::PrintBool.into(),
            print_bool_type,
            Some(Linkage::External),
        );

        enum_map::enum_map! {
            RTFunction::PrintStr => print_str_prototype,
            RTFunction::PrintInt => print_int_prototype,
            RTFunction::PrintBool => print_bool_prototype,
        }
    }
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub entry_function: FunctionValue<'ctx>,
    pub function: Option<FunctionValue<'ctx>>,
    pub prelude_functions: EnumMap<RTFunction, FunctionValue<'ctx>>,
    pub strings: HashMap<String, GlobalValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        prelude_functions: EnumMap<RTFunction, FunctionValue<'ctx>>,
    ) -> Self {
        let builder = context.create_builder();

        let main_type = context.void_type().fn_type(&[], false);
        let main_prototype = module.add_function("main", main_type, Some(Linkage::External));

        let entry_block = context.append_basic_block(main_prototype, "entry");
        builder.position_at_end(entry_block);

        Self {
            context,
            module,
            builder,
            entry_function: main_prototype,
            function: None,
            prelude_functions,
            strings: HashMap::new(),
        }
    }

    pub fn compile(&self, term: &ast::Term) {
        match term {
            ast::Term::Print(print) => print.codegen(self),
            _ => todo!(),
        };
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
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
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
pub enum RTFunction {
    PrintStr,
    PrintInt,
    PrintBool,
}

impl From<RTFunction> for &'static str {
    fn from(rt: RTFunction) -> Self {
        match rt {
            RTFunction::PrintStr => "__rinha_rt_print_str",
            RTFunction::PrintInt => "__rinha_rt_print_int",
            RTFunction::PrintBool => "__rinha_rt_print_bool",
        }
    }
}
