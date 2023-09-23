pub mod environment;

use std::{collections::HashMap, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{InitializationConfig, Target, TargetTriple},
    values::FunctionValue,
};

use crate::{
    ast,
    codegen::{
        core::CoreFunctions,
        traits::Codegen,
        value::{Closure, Str},
        FIRST_BLOCK_NAME,
    },
};

use self::environment::{Scope, ScopeRc};

pub struct Rinhac {}

impl Rinhac {
    pub fn compile(ast: ast::File) {
        let context = Context::create();
        let module = context.create_module(&ast.name);

        let triple = TargetTriple::create(current_platform::CURRENT_PLATFORM);
        module.set_triple(&triple);

        let builder = context.create_builder();

        let mut compiler = Compiler::new(&context, &module, builder);

        let mut next = Some(&ast.expression);
        while let Some(next_term) = next {
            next = compiler.compile(next_term);
        }

        compiler.finalize();

        println!("GENERATED IR:\n{}", module.print_to_string().to_string(),)
    }
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub entry_function: FunctionValue<'ctx>,
    pub core_functions: CoreFunctions<'a, 'ctx>,
    pub strings: HashMap<String, Str<'ctx>>,
    pub scope: ScopeRc<'ctx>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(context: &'ctx Context, module: &'a Module<'ctx>, builder: Builder<'ctx>) -> Self {
        builder.clear_insertion_position();
        let main_type = context.void_type().fn_type(&[], false);
        let main_prototype = module.add_function("main", main_type, Some(Linkage::External));

        let entry_block = context.append_basic_block(main_prototype, FIRST_BLOCK_NAME);
        builder.position_at_end(entry_block);

        let scope = Scope::new(entry_block, Closure::new(main_prototype, None), None);
        Self {
            context,
            module,
            builder,
            entry_function: main_prototype,
            core_functions: CoreFunctions::new(context, module),
            strings: HashMap::new(),
            scope,
        }
    }

    pub fn compile<'t>(&mut self, term: &'t ast::Term) -> Option<&'t ast::Term> {
        println!("compiling: {:?}\n\n", term);

        match term {
            ast::Term::Print(print) => {
                print.codegen(self);
                None
            }
            ast::Term::Let(binding) => {
                binding.codegen(self);
                Some(&binding.next)
            }

            ast::Term::Call(call) => {
                call.codegen(self);
                None
            }

            ast::Term::If(conditional) => {
                conditional.codegen(self);
                None
            }

            // ignore top-level values for now
            ast::Term::Var(_)
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
            eprintln!("----------IR--------");
            self.module.print_to_stderr();
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

    // pub fn enter_scope(&mut self, scope: &ScopeNode<'ctx>) {
    //     self.scope.enter(scope);
    //     self.builder.position_at_end(scope.borrow().block);
    // }

    // pub fn leave_scope(&mut self) {
    //     self.scope.leave();
    //     self.builder.position_at_end(self.scope.current_block());
    // }
}
