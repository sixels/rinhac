use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::atomic::AtomicBool};

use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum};

use crate::{
    ast,
    codegen::value::{Closure, Value, ValueRef},
};

pub type ScopeRc<'ctx> = Rc<RefCell<Scope<'ctx>>>;

pub trait ScopeNode<'ctx> {
    fn create_child(&self, block: BasicBlock<'ctx>, function: Option<Closure<'ctx>>) -> Self;
    fn find_variable(&self, name: &str) -> Option<Variable<'ctx>>;
    fn add_variable(&self, name: impl Into<String>, value: Variable<'ctx>);
}

#[derive(Debug)]
pub struct Scope<'ctx> {
    pub name: String,
    pub block: BasicBlock<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,
    pub function: Closure<'ctx>,

    pub parent: Option<ScopeRc<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new(
        block: BasicBlock<'ctx>,
        function: Closure<'ctx>,
        parent: Option<ScopeRc<'ctx>>,
    ) -> Rc<RefCell<Self>> {
        let name = format!(
            "{}-{}",
            function.funct.get_name().to_string_lossy(),
            block.get_name().to_string_lossy()
        );
        Rc::new(RefCell::new(Self {
            name,
            block,
            variables: HashMap::new(),
            function,
            parent,
        }))
    }
}

impl<'ctx> ScopeNode<'ctx> for ScopeRc<'ctx> {
    fn create_child(&self, block: BasicBlock<'ctx>, function: Option<Closure<'ctx>>) -> Self {
        let this = self.borrow();
        Rc::new(RefCell::new(Scope {
            name: format!("{}-{}", this.name, block.get_name().to_string_lossy()),
            block,
            variables: HashMap::new(),
            function: function.unwrap_or(this.function),
            parent: Some(Rc::clone(self)),
        }))
    }

    fn find_variable(&self, name: &str) -> Option<Variable<'ctx>> {
        let mut depth = 0;
        let mut scope = Rc::clone(self);
        loop {
            let parent = {
                let scope_ref = scope.borrow();

                if let Some(variable) = scope_ref.variables.get(name).cloned() {
                    // capture variable
                    if depth > 0 {
                        self.borrow_mut()
                            .variables
                            .insert(String::from(name), variable.clone());
                    }
                    return Some(variable);
                }

                if let Some(parent) = &scope_ref.parent {
                    Rc::clone(parent)
                } else {
                    break;
                }
            };

            depth += 1;
            scope = parent;
        }
        None
    }

    fn add_variable(&self, name: impl Into<String>, value: Variable<'ctx>) {
        self.borrow_mut().variables.insert(name.into(), value);
    }
}

#[derive(Debug, Clone)]
pub enum Variable<'ctx> {
    Function(Rc<RefCell<Function<'ctx>>>),
    Value(ValueRef<'ctx>),
    Constant(Value<'ctx>),
}

#[derive(Debug)]
pub struct Function<'ctx> {
    pub body: ast::Function,
    pub definitions: Vec<(Vec<BasicTypeEnum<'ctx>>, Closure<'ctx>)>,
    pub called: AtomicBool,
    pub definition_scope: ScopeRc<'ctx>,
}

impl<'ctx> Function<'ctx> {
    pub fn new(body: ast::Function, definition_scope: &ScopeRc<'ctx>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            body,
            definitions: Vec::new(),
            called: false.into(),
            definition_scope: Rc::clone(definition_scope),
        }))
    }

    pub fn get_or_insert_definition(
        &mut self,
        params: &[BasicTypeEnum<'ctx>],
        define: impl FnOnce(&Self) -> Closure<'ctx>,
    ) -> Closure<'ctx> {
        if let Some(def) = self.definitions.iter().find(|(defined_params, _)| {
            defined_params
                .iter()
                .zip(params.iter())
                .all(|(&a, &b)| a == b)
        }) {
            def.1
        } else {
            let def = define(self);
            self.definitions.push((Vec::from(params), def));
            def
        }
    }
}

impl<'ctx> Drop for Scope<'ctx> {
    fn drop(&mut self) {
        println!("Dropping scope: {}", self.name);
    }
}

#[derive(Debug)]
pub struct FunctionDefinition<'ctx> {
    pub param_types: Vec<BasicTypeEnum<'ctx>>,
    pub closure: Closure<'ctx>,
    pub scope: ScopeRc<'ctx>,
}

impl<'ctx> FunctionDefinition<'ctx> {
    pub fn new(param_types: Vec<BasicTypeEnum<'ctx>>, closure: Closure<'ctx>) -> Self {
        Self {
            param_types,
            closure,
            scope: Scope::new(
                closure.funct.get_first_basic_block().unwrap(),
                closure,
                None,
            ),
        }
    }
}
