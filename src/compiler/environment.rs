use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::atomic::AtomicBool,
};

use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum, values::PointerValue};

use crate::{
    ast,
    codegen::value::{Closure, PrimitiveRef, Value, ValueRef, ValueType},
};

use super::Compiler;

pub type ScopeRc<'ctx> = Rc<RefCell<Scope<'ctx>>>;

pub trait ScopeNode<'ctx> {
    fn create_child(&self, block: BasicBlock<'ctx>, function: Option<Closure<'ctx>>) -> Self;
    fn find_callable(&self, name: &str) -> Option<Rc<RefCell<Function<'ctx>>>>;
    fn find_variable(&self, name: &str) -> Option<Variable<'ctx>>;
    fn add_variable(&self, name: impl Into<String>, value: Variable<'ctx>);
    fn find_captured_variable(&self, name: &str) -> Option<Variable<'ctx>>;
    fn add_captured_variable(&self, name: impl Into<String>, value: Variable<'ctx>);
}

#[derive(Debug)]
pub struct Scope<'ctx> {
    pub name: String,
    pub block: BasicBlock<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,
    pub captured_variables: HashMap<String, Variable<'ctx>>,
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
            captured_variables: HashMap::new(),
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
            captured_variables: HashMap::new(),
            function: function.unwrap_or(this.function),
            parent: Some(Rc::clone(self)),
        }))
    }

    fn find_callable(&self, name: &str) -> Option<Rc<RefCell<Function<'ctx>>>> {
        let mut current = Some(Rc::clone(self));
        while let Some(scope) = current.clone() {
            let scope_ref = scope.borrow();
            if let Some(var) = scope_ref.variables.get(name) {
                if let Variable::Function(f) = var {
                    return Some(Rc::clone(f));
                } else {
                    // variable is not callable
                    return None;
                }
            }

            current.clone_from(&scope_ref.parent);
        }
        None
    }

    fn find_variable(&self, name: &str) -> Option<Variable<'ctx>> {
        self.borrow().variables.get(name).cloned()
    }

    fn find_captured_variable(&self, name: &str) -> Option<Variable<'ctx>> {
        self.borrow().captured_variables.get(name).cloned()
    }

    fn add_variable(&self, name: impl Into<String>, value: Variable<'ctx>) {
        self.borrow_mut().variables.insert(name.into(), value);
    }

    fn add_captured_variable(&self, name: impl Into<String>, value: Variable<'ctx>) {
        self.borrow_mut()
            .captured_variables
            .insert(name.into(), value);
    }
}

#[derive(Debug, Clone)]
pub enum Variable<'ctx> {
    Function(Rc<RefCell<Function<'ctx>>>),
    Value(ValueRef<'ctx>),
    Constant(Value<'ctx>),
}

impl<'ctx> Variable<'ctx> {
    pub fn get_ptr(&self, compiler: &Compiler<'_, 'ctx>) -> PointerValue<'ctx> {
        match *self {
            Variable::Value(v) => match v {
                ValueRef::Primitive(PrimitiveRef::Bool(b)) => b,
                ValueRef::Primitive(PrimitiveRef::Int(i)) => i,
                ValueRef::Str(s) => s.ptr,
                ValueRef::Closure(c) => c.funct.as_global_value().as_pointer_value(),
            },
            Variable::Constant(c) => {
                let v = c.build_variable(compiler, "");
                match v {
                    ValueRef::Primitive(PrimitiveRef::Bool(b)) => b,
                    ValueRef::Primitive(PrimitiveRef::Int(i)) => i,
                    ValueRef::Str(s) => s.ptr,
                    ValueRef::Closure(c) => c.funct.as_global_value().as_pointer_value(),
                }
            }
            _ => todo!(),
        }
    }

    pub fn get_known_type(&self) -> ValueType<'ctx> {
        match self {
            Variable::Value(v) => match v {
                ValueRef::Primitive(PrimitiveRef::Bool(_)) => ValueType::Bool,
                ValueRef::Primitive(PrimitiveRef::Int(_)) => ValueType::Int,
                ValueRef::Str(s) => ValueType::Str(s.len),
                ValueRef::Closure(c) => {
                    ValueType::Closure(c.funct.as_global_value().as_pointer_value().get_type())
                }
            },
            Variable::Constant(c) => c.get_known_type(),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct Function<'ctx> {
    pub name: String,
    pub body: ast::Function,
    pub definitions: Vec<(Vec<BasicTypeEnum<'ctx>>, Closure<'ctx>)>,
    pub called: AtomicBool,
    pub definition_scope: ScopeRc<'ctx>,
    pub captured_variables: Vec<(ValueType<'ctx>, String)>,
}

impl<'ctx> Function<'ctx> {
    pub fn new(
        name: String,
        body: ast::Function,
        definition_scope: &ScopeRc<'ctx>,
        captured_variables: Vec<(ValueType<'ctx>, String)>,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            name,
            body,
            definitions: Vec::new(),
            called: false.into(),
            definition_scope: Rc::clone(definition_scope),
            captured_variables,
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

    pub(crate) fn check_captures<'a>(
        scope: &ScopeRc<'ctx>,
        fn_ast: &'a ast::Function,
    ) -> HashSet<Cow<'a, str>> {
        let mut environ = fn_ast
            .parameters
            .iter()
            .map(|param| param.text.as_str().into())
            .collect::<HashSet<Cow<'a, str>>>();

        let mut capture = HashSet::new();

        Self::check_captures_with_env(scope, &fn_ast.value, &mut environ, &mut capture);
        capture
    }

    fn check_captures_with_env<'a>(
        scope: &ScopeRc<'ctx>,
        term: &'a ast::Term,
        environ: &mut HashSet<Cow<'a, str>>,
        capture: &mut HashSet<Cow<'a, str>>,
    ) {
        let mut current = Some(term);
        while let Some(term) = current {
            current = match term {
                ast::Term::Let(l) => {
                    Self::check_captures_with_env(scope, &l.value, environ, capture);
                    environ.insert(l.name.text.as_str().into());
                    Some(&l.next)
                }
                ast::Term::Var(v) => {
                    if !environ.contains(v.text.as_str()) {
                        capture.insert(v.text.as_str().into());
                    }
                    None
                }
                ast::Term::Call(c) => {
                    if let ast::Term::Var(v) = c.callee.as_ref() {
                        if let Some(callable) = scope.find_callable(&v.text) {
                            let callable_ref = callable.borrow();
                            for (_, symbol) in callable_ref.captured_variables.iter() {
                                if !environ.contains(symbol.as_str()) {
                                    capture.insert(Cow::Owned(symbol.clone()));
                                }
                            }
                        }
                    }
                    // scope.find_callable(c.callee)
                    for arg in c.arguments.iter() {
                        Self::check_captures_with_env(scope, arg, environ, capture)
                    }
                    None
                }
                ast::Term::Binary(b) => {
                    Self::check_captures_with_env(scope, &b.lhs, environ, capture);
                    Self::check_captures_with_env(scope, &b.rhs, environ, capture);
                    None
                }
                ast::Term::If(i) => {
                    Self::check_captures_with_env(scope, &i.then, environ, capture);
                    Self::check_captures_with_env(scope, &i.otherwise, environ, capture);
                    None
                }
                ast::Term::First(f) => {
                    Self::check_captures_with_env(scope, &f.value, environ, capture);
                    None
                }
                ast::Term::Second(s) => {
                    Self::check_captures_with_env(scope, &s.value, environ, capture);
                    None
                }
                ast::Term::Print(p) => {
                    Self::check_captures_with_env(scope, &p.value, environ, capture);
                    None
                }
                ast::Term::Tuple(t) => {
                    Self::check_captures_with_env(scope, &t.first, environ, capture);
                    Self::check_captures_with_env(scope, &t.second, environ, capture);
                    None
                }
                ast::Term::Function(a) => {
                    for param in a.parameters.iter() {
                        environ.insert(param.text.as_str().into());
                    }
                    Self::check_captures_with_env(scope, &a.value, environ, capture);
                    None
                }
                _ => None,
            }
        }
    }

    pub fn monomorph_name(&self, params: impl Iterator<Item = ValueType<'ctx>>) -> String {
        format!(
            "{}_{}",
            self.name,
            params
                .map(|p| match p {
                    ValueType::Int => "i",
                    ValueType::Bool => "b",
                    ValueType::Str(_) => "s",
                    ValueType::Closure(_) => "c",
                })
                .collect::<String>()
        )
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
