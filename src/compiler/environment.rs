use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
};

use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum, values::PointerValue};
use rand::distributions::{Alphanumeric, DistString};

use crate::{
    ast,
    codegen::{
        traits::{CodegenValue, DerefValue},
        value::{Closure, PrimitiveRef, Value, ValueRef, ValueType},
    },
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
        match self.clone() {
            Variable::Value(v) => match v {
                ValueRef::Primitive(PrimitiveRef::Bool(b)) => b,
                ValueRef::Primitive(PrimitiveRef::Int(i)) => i,
                ValueRef::Str(s) => s.ptr,
                ValueRef::Boxed(b) => b.ptr,
                ValueRef::Closure(_) => panic!("closure cannot be used as a pointer"),
                ValueRef::Tuple(_) => panic!(""),
            },
            Variable::Constant(c) => {
                let c_ref = c.build_as_ref(compiler, "");
                match c_ref {
                    ValueRef::Primitive(PrimitiveRef::Bool(b)) => b,
                    ValueRef::Primitive(PrimitiveRef::Int(i)) => i,
                    ValueRef::Str(s) => s.ptr,
                    ValueRef::Boxed(b) => b.ptr,
                    ValueRef::Closure(_) => panic!("A"),
                    ValueRef::Tuple(_) => panic!(""),
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
                ValueRef::Closure(cl) => {
                    ValueType::Closure(cl.funct.as_global_value().as_pointer_value().get_type())
                }
                ValueRef::Boxed(b) => ValueType::Any(*b),
                ValueRef::Tuple(_) => panic!(),
            },
            Variable::Constant(c) => c.get_known_type(),
            _ => todo!(),
        }
    }
}

impl<'ctx> CodegenValue<'ctx> for Variable<'ctx> {
    fn codegen_value(&self, compiler: &mut Compiler<'_, 'ctx>) -> Value<'ctx> {
        match self {
            Variable::Value(v) => v.build_deref(compiler),
            Variable::Constant(c) => c.clone(),
            // returns 0 for now
            Variable::Function(_) => Value::Primitive(crate::codegen::value::Primitive::Int(
                compiler.context.i32_type().const_int(0, false),
            )),
            // closure.build_deref(compiler)
            // }
        }
    }
}

#[derive(Debug)]
pub struct Function<'ctx> {
    pub name: String,
    pub body: ast::Function,
    pub definitions: Vec<(Vec<ValueType<'ctx>>, Closure<'ctx>)>,
    pub definition_scope: ScopeRc<'ctx>,
    pub captured_variables: Vec<Capture<'ctx>>,
    is_anonymous: bool,
}

impl<'ctx> Function<'ctx> {
    pub fn build(
        compiler: &Compiler<'_, 'ctx>,
        name: Option<String>,
        definition: &ast::Function,
    ) -> Rc<RefCell<Self>> {
        let is_anonymous = name.is_none();
        let name = name.unwrap_or_else(|| {
            let mut rng = rand::thread_rng();
            Alphanumeric.sample_string(&mut rng, 10)
        });
        let (direct_captures, indirect_captures) = find_captures(definition);

        let captured_variables = direct_captures
            .into_iter()
            .map(|dc| {
                let var = compiler.scope.find_any_variable(dc).unwrap();
                let var = if let Variable::Value(v) = var {
                    Variable::Value(v.cloned(compiler))
                } else {
                    var
                };

                Capture::direct(dc, var)
            })
            .chain(indirect_captures.into_iter().filter_map(|name| {
                compiler.scope.find_callable(&name).map(|funct| {
                    let name = {
                        let funct_ref = &funct.borrow();
                        funct_ref.unique_name()
                    };
                    Capture::indirect(name, funct)
                })
            }))
            .collect::<Vec<_>>();

        Self::new(
            name,
            definition.clone(),
            &compiler.scope,
            captured_variables,
            is_anonymous,
        )
    }

    pub fn new(
        name: String,
        body: ast::Function,
        definition_scope: &ScopeRc<'ctx>,
        captured_variables: Vec<Capture<'ctx>>,
        is_anonymous: bool,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            name,
            body,
            definitions: Vec::new(),
            definition_scope: Rc::clone(definition_scope),
            captured_variables,
            is_anonymous,
        }))
    }

    pub fn get_or_insert_definition(
        &mut self,
        params: &[ValueType<'ctx>],
        define: impl FnOnce(&Self) -> Closure<'ctx>,
    ) -> Closure<'ctx> {
        if let Some(def) = self.definitions.iter().find(|(defined_params, _)| {
            defined_params
                .iter()
                .zip(params.iter())
                .all(|(a, b)| a == b)
        }) {
            def.1
        } else {
            let def = define(self);
            self.definitions.push((Vec::from(params), def));
            def
        }
    }

    pub fn monomorph_name(&self, params: impl Iterator<Item = (ValueType<'ctx>, bool)>) -> String {
        format!(
            "{}.{}",
            self.unique_name(),
            params.map(|p| format!("{}", p.0)).collect::<String>()
        )
    }

    pub fn unique_name(&self) -> String {
        format!("{}_{}", self.definition_scope.borrow().name, self.name)
    }
}

pub fn find_captures<'a>(fn_ast: &'a ast::Function) -> (HashSet<&'a str>, HashSet<String>) {
    let environ = fn_ast
        .parameters
        .iter()
        .map(|param| param.text.as_str())
        .collect::<HashSet<&'a str>>();

    let mut direct_captures = HashSet::new();
    let mut indirect_captures = HashSet::new();
    let mut captures = (&mut direct_captures, &mut indirect_captures);

    find_captures_with_env(&mut captures, &environ, &fn_ast.value);
    (direct_captures, indirect_captures)
}

fn find_captures_with_env<'a>(
    captures: &mut (&mut HashSet<&'a str>, &mut HashSet<String>),
    parent_env: &HashSet<&'a str>,
    term: &'a ast::Term,
) -> HashSet<&'a str> {
    let mut environ = parent_env.clone();

    let mut current = Some(term);
    while let Some(term) = current {
        current = match term {
            ast::Term::Let(l) => {
                find_captures_with_env(captures, &environ, &l.value);
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
                        captures.1.insert(v.text.clone());
                    }
                }

                for arg in c.arguments.iter() {
                    find_captures_with_env(captures, &environ, arg);
                }
                None
            }
            ast::Term::Binary(b) => {
                find_captures_with_env(captures, &environ, &b.lhs);
                find_captures_with_env(captures, &environ, &b.rhs);
                None
            }
            ast::Term::If(i) => {
                find_captures_with_env(captures, &environ, &i.then);
                find_captures_with_env(captures, &environ, &i.otherwise);
                None
            }
            ast::Term::First(f) => {
                find_captures_with_env(captures, &environ, &f.value);
                None
            }
            ast::Term::Second(s) => {
                find_captures_with_env(captures, &environ, &s.value);
                None
            }
            ast::Term::Print(p) => {
                find_captures_with_env(captures, &environ, &p.value);
                None
            }
            ast::Term::Tuple(t) => {
                find_captures_with_env(captures, &environ, &t.first);
                find_captures_with_env(captures, &environ, &t.second);
                None
            }
            ast::Term::Function(a) => {
                let mut fn_env = a
                    .parameters
                    .iter()
                    .map(|param| param.text.as_str())
                    .collect::<HashSet<&'a str>>();
                fn_env.extend(&environ);
                find_captures_with_env(captures, &fn_env, &a.value);
                None
            }
            _ => None,
        }
    }

    environ
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

impl<'ctx> Drop for Scope<'ctx> {
    fn drop(&mut self) {
        // println!("Dropping scope: {}", self.name);
    }
}

#[derive(Debug, Clone)]
pub enum Capture<'ctx> {
    Direct {
        symbol: String,
        variable: Variable<'ctx>,
    },
    Indirect {
        symbol: String,
        function: Rc<RefCell<Function<'ctx>>>,
    },
}
impl<'ctx> Capture<'ctx> {
    pub fn direct(symbol: impl Into<String>, variable: Variable<'ctx>) -> Self {
        Self::Direct {
            symbol: symbol.into(),
            variable,
        }
    }
    pub fn indirect(symbol: String, function: Rc<RefCell<Function<'ctx>>>) -> Self {
        Self::Indirect { symbol, function }
    }
}
