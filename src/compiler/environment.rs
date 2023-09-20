use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum};

use crate::{
    ast,
    codegen::value::{Closure, ValueRef},
};

pub struct Scopes<'ctx> {
    entry: ScopeNode<'ctx>,
    pub current: ScopeNode<'ctx>,
}
impl<'ctx> Scopes<'ctx> {
    pub fn new(initial: Scope<'ctx>) -> Self {
        let entry_scope = Rc::new(RefCell::new(initial));
        Self {
            current: Rc::clone(&entry_scope),
            entry: entry_scope,
        }
    }

    pub fn enter(&mut self, block: BasicBlock<'ctx>) -> Rc<RefCell<Scope<'ctx>>> {
        let name = block.get_name().to_string_lossy().to_string();
        let new_scope = Rc::new(RefCell::new(Scope {
            name: name.clone(),
            block,
            variables: HashMap::new(),

            parent: Some(Rc::clone(&self.entry)),
            children: Vec::new(),
        }));

        self.entry.borrow_mut().children.push(Rc::clone(&new_scope));
        self.current = Rc::clone(&new_scope);

        new_scope
    }

    pub fn leave(&mut self) {
        let parent = {
            let mut scope = self.current.borrow_mut();

            let prev_scope = if let Some(node) = scope.parent.take() {
                node
            } else {
                Rc::clone(&self.entry)
            };

            prev_scope
                .borrow_mut()
                .children
                .retain(|child| !Rc::ptr_eq(child, &self.current));

            prev_scope
        };
        self.current = parent;
    }

    pub fn find_variable(&self, name: &str) -> Option<Variable<'ctx>> {
        let mut scope = Rc::clone(&self.current);
        loop {
            let parent = {
                let scope_ref = scope.borrow();

                if let Some(variable) = scope_ref.variables.get(name) {
                    return Some(variable.clone());
                }

                if let Some(parent) = &scope_ref.parent {
                    Rc::clone(parent)
                } else {
                    break;
                }
            };
            scope = parent;
        }
        None
    }
}

impl<'ctx> Drop for Scope<'ctx> {
    fn drop(&mut self) {
        println!("Dropping scope: {}", self.name);
    }
}

pub type ScopeNode<'ctx> = Rc<RefCell<Scope<'ctx>>>;

#[derive(Debug)]
pub struct Scope<'ctx> {
    pub name: String,
    pub block: BasicBlock<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,

    parent: Option<ScopeNode<'ctx>>,
    children: Vec<ScopeNode<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new(
        name: impl Into<String>,
        block: BasicBlock<'ctx>,
        parent: Option<ScopeNode<'ctx>>,
    ) -> Self {
        Self {
            name: name.into(),
            block,
            variables: HashMap::new(),

            parent,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Variable<'ctx> {
    Function(Rc<RefCell<Function<'ctx>>>),
    Value(ValueRef<'ctx>),
}

#[derive(Debug)]
pub struct Function<'ctx> {
    pub body: ast::Function,
    pub call_with: Vec<Vec<BasicTypeEnum<'ctx>>>,
    pub definition: Vec<Closure<'ctx>>,
    pub called: bool,
}

impl<'ctx> Function<'ctx> {
    pub fn new(body: ast::Function) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            body,
            call_with: Vec::new(),
            definition: Vec::new(),
            called: false,
        }))
    }
}
