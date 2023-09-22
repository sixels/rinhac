use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
};

use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum, values::PointerValue};

use crate::{
    ast,
    codegen::value::{Closure, PrimitiveRef, Value, ValueRef, ValueType},
};

use super::Compiler;

pub type ScopeRc<'ctx> = Rc<RefCell<Scope<'ctx>>>;

#[derive(Debug)]
pub struct IndirectVariable<'ctx> {
    pub symbol: String,
    pub known_type: ValueType<'ctx>,
    pub ptr: PointerValue<'ctx>,
}

pub trait ScopeNode<'ctx> {
    fn create_child(&self, block: BasicBlock<'ctx>, function: Option<Closure<'ctx>>) -> Self;
    fn find_callable(&self, name: &str) -> Option<Rc<RefCell<Function<'ctx>>>>;
    fn find_variable(&self, name: &str) -> Option<Variable<'ctx>>;
    fn add_variable(&self, name: impl Into<String>, value: Variable<'ctx>);
    fn find_captured_variable(&self, name: &str) -> Option<Variable<'ctx>>;
    fn add_captured_variable(&self, name: impl Into<String>, value: Variable<'ctx>);
    fn find_any_variable(&self, name: &str) -> Result<Variable<'ctx>, String> {
        self.find_variable(name)
            .or_else(|| self.find_captured_variable(name))
            .ok_or_else(|| format!("Variable \"{}\" not found", name))
    }
    fn get_indirect_captures_environ(&self, name: &str) -> Option<PointerValue<'ctx>>;
    fn add_indirect_captures(&self, capture: &Capture<'ctx>);
}

#[derive(Debug)]
pub struct Scope<'ctx> {
    pub name: String,
    pub block: BasicBlock<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,
    pub captured_variables: HashMap<String, Variable<'ctx>>,
    pub function: Closure<'ctx>,
    pub parent: Option<ScopeRc<'ctx>>,
    pub indirect_captured_environment: BTreeMap<String, PointerValue<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new(
        block: BasicBlock<'ctx>,
        function: Closure<'ctx>,
        parent: Option<ScopeRc<'ctx>>,
    ) -> Rc<RefCell<Self>> {
        let name = format!("{}", function.funct.get_name().to_string_lossy(),);
        Rc::new(RefCell::new(Self {
            name,
            block,
            variables: HashMap::new(),
            captured_variables: HashMap::new(),
            function,
            parent,
            indirect_captured_environment: BTreeMap::new(),
        }))
    }
}

impl<'ctx> ScopeNode<'ctx> for ScopeRc<'ctx> {
    fn create_child(&self, block: BasicBlock<'ctx>, function: Option<Closure<'ctx>>) -> Self {
        let block_name = function
            .map(|f| f.funct.get_name().to_string_lossy().into_owned())
            .unwrap_or_else(|| block.get_name().to_string_lossy().into_owned());

        Rc::new(RefCell::new(Scope {
            name: block_name,
            block,
            variables: HashMap::new(),
            captured_variables: HashMap::new(),
            function: function.unwrap_or_else(|| self.borrow().function),
            parent: Some(Rc::clone(self)),
            indirect_captured_environment: BTreeMap::new(),
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

    fn get_indirect_captures_environ(&self, name: &str) -> Option<PointerValue<'ctx>> {
        self.borrow()
            .indirect_captured_environment
            .get(name)
            .copied()
    }
    fn add_indirect_captures(&self, _capture: &Capture<'ctx>) {
        todo!()
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
    pub definition_scope: ScopeRc<'ctx>,
    pub captured_variables: Vec<Capture<'ctx>>,
}

impl<'ctx> Function<'ctx> {
    pub fn new(
        name: String,
        body: ast::Function,
        definition_scope: &ScopeRc<'ctx>,
        captured_variables: Vec<Capture<'ctx>>,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            name,
            body,
            definitions: Vec::new(),
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

    pub(crate) fn captured_environment<'a>(
        scope: &ScopeRc<'ctx>,
        fn_ast: &'a ast::Function,
    ) -> (HashSet<&'a str>, BTreeMap<String, Vec<Capture<'ctx>>>) {
        let environ = fn_ast
            .parameters
            .iter()
            .map(|param| param.text.as_str())
            .collect::<HashSet<&'a str>>();

        let mut direct_captures = HashSet::new();
        let mut indirect_captures = BTreeMap::new();
        let mut captures = (&mut direct_captures, &mut indirect_captures);

        Self::check_captures_with_env(&mut captures, scope, &environ, &fn_ast.value);
        (direct_captures, indirect_captures)
    }

    fn check_captures_with_env<'a>(
        captures: &mut (
            &mut HashSet<&'a str>,
            &mut BTreeMap<String, Vec<Capture<'ctx>>>,
        ),
        scope: &ScopeRc<'ctx>,
        parent_env: &HashSet<&'a str>,
        term: &'a ast::Term,
    ) -> HashSet<&'a str> {
        let mut environ = parent_env.clone();

        let mut current = Some(term);
        while let Some(term) = current {
            current = match term {
                ast::Term::Let(l) => {
                    Self::check_captures_with_env(captures, scope, &environ, &l.value);
                    environ.insert(l.name.text.as_str());
                    Some(&l.next)
                }
                ast::Term::Var(v) => {
                    if !environ.contains(v.text.as_str()) {
                        captures.0.insert(v.text.as_str());
                    }
                    None
                }
                ast::Term::Call(c) => {
                    if let ast::Term::Var(v) = c.callee.as_ref() {
                        if !captures.0.contains(v.text.as_str()) {
                            if let Some(callable) = scope.find_callable(&v.text) {
                                let callable_ref = callable.borrow();

                                captures.1.insert(
                                    v.text.clone(),
                                    callable_ref.captured_variables.clone(),
                                );
                            }
                        }
                    }

                    for arg in c.arguments.iter() {
                        Self::check_captures_with_env(captures, scope, &environ, arg);
                    }
                    None
                }
                ast::Term::Binary(b) => {
                    Self::check_captures_with_env(captures, scope, &environ, &b.lhs);
                    Self::check_captures_with_env(captures, scope, &environ, &b.rhs);
                    None
                }
                ast::Term::If(i) => {
                    Self::check_captures_with_env(captures, scope, &environ, &i.then);
                    Self::check_captures_with_env(captures, scope, &environ, &i.otherwise);
                    None
                }
                ast::Term::First(f) => {
                    Self::check_captures_with_env(captures, scope, &environ, &f.value);
                    None
                }
                ast::Term::Second(s) => {
                    Self::check_captures_with_env(captures, scope, &environ, &s.value);
                    None
                }
                ast::Term::Print(p) => {
                    Self::check_captures_with_env(captures, scope, &environ, &p.value);
                    None
                }
                ast::Term::Tuple(t) => {
                    Self::check_captures_with_env(captures, scope, &environ, &t.first);
                    Self::check_captures_with_env(captures, scope, &environ, &t.second);
                    None
                }
                ast::Term::Function(a) => {
                    let mut fn_env = a
                        .parameters
                        .iter()
                        .map(|param| param.text.as_str())
                        .collect::<HashSet<&'a str>>();
                    fn_env.extend(&environ);
                    Self::check_captures_with_env(captures, scope, &fn_env, &a.value);
                    None
                }
                _ => None,
            }
        }

        environ
    }

    pub fn monomorph_name(&self, params: impl Iterator<Item = ValueType<'ctx>>) -> String {
        format!(
            "{}${}",
            self.unique_name(),
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

    pub fn unique_name(&self) -> String {
        format!("{}_{}", self.definition_scope.borrow().name, self.name)
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

#[derive(Debug)]
pub struct CapturedVarMetadata<'ctx> {
    pub symbol: String,
    pub known_type: ValueType<'ctx>,
}

impl<'ctx> Drop for Scope<'ctx> {
    fn drop(&mut self) {
        println!("Dropping scope: {}", self.name);
    }
}

#[derive(Debug, Clone)]
pub enum Capture<'ctx> {
    Direct {
        symbol: String,
        known_type: ValueType<'ctx>,
    },
    Indirect {
        symbol: String,
        function: Rc<RefCell<Function<'ctx>>>,
    },
}
impl<'ctx> Capture<'ctx> {
    pub fn direct(symbol: impl Into<String>, known_type: ValueType<'ctx>) -> Self {
        Self::Direct {
            symbol: symbol.into(),
            known_type,
        }
    }
    pub fn indirect(symbol: String, function: Rc<RefCell<Function<'ctx>>>) -> Self {
        Self::Indirect { symbol, function }
    }
}
