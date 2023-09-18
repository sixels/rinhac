use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::basic_block::BasicBlock;

use crate::codegen::value::ValueRef;

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
        let mut scope = self.current.borrow_mut();

        let parent;
        if let Some(node) = scope.parent.take() {
            parent = node
        } else {
            parent = Rc::clone(&self.entry)
        };

        parent
            .borrow_mut()
            .children
            .retain(|child| !Rc::ptr_eq(child, &self.current));

        self.current.swap(&parent);
    }
}

impl<'ctx> Drop for Scope<'ctx> {
    fn drop(&mut self) {
        println!("Dropping scope: {}", self.name);
    }
}

pub type ScopeNode<'ctx> = Rc<RefCell<Scope<'ctx>>>;

pub struct Scope<'ctx> {
    pub name: String,
    pub block: BasicBlock<'ctx>,
    pub variables: HashMap<String, ValueRef<'ctx>>,

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
