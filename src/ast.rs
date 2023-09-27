//! Copy-pasted and adapted from https://github.com/aripiprazole/rinha-de-compiler/blob/main/src/ast.rs

use serde::Deserialize;
use std::{fmt::Debug, rc::Rc};

/// File definition, it contains all the statements,
/// the module name, and a base location for it as anchor
/// for the statements.
#[derive(Debug, Clone, Deserialize)]
pub struct File {
    pub name: String,
    pub expression: Term,
    pub location: Location,
}

impl<T: Element> Element for Rc<T> {
    fn location(&self) -> &Location {
        self.as_ref().location()
    }
}

impl<T: Element> Element for Box<T> {
    fn location(&self) -> &Location {
        self.as_ref().location()
    }
}

#[derive(Default, Hash, PartialEq, Eq, Clone, Deserialize)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub filename: String,
}

impl Location {
    /// Creates a new instance of [`Location`].
    pub fn new(start: usize, end: usize, filename: &str) -> Self {
        Self {
            start,
            end,
            filename: filename.into(),
        }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location")
    }
}

/// An element. It can be a declaration, or a term.
pub trait Element {
    fn location(&self) -> &Location;
}

/// Error node, it does contains an error.
#[derive(Debug, Clone, Deserialize)]
pub struct Error {
    /// The error message.
    pub message: String,

    /// The original text that originated the error.
    pub full_text: String,

    /// The location of the error.
    pub location: Location,
}

impl Element for Error {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct If {
    pub condition: Box<Term>,
    pub then: Box<Term>,
    pub otherwise: Box<Term>,
    pub location: Location,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Let {
    pub name: Var,
    pub value: Box<Term>,
    pub next: Box<Term>,
    pub location: Location,
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone, Deserialize)]
pub struct Str {
    pub value: String,

    /// The location of the source in the source code.
    pub location: Location,
}

impl Element for Str {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Default, Debug, Clone, Deserialize)]
pub struct Bool {
    pub value: bool,
    pub location: Location,
}

impl Element for Bool {
    fn location(&self) -> &Location {
        &self.location
    }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone, Deserialize)]
pub struct Int {
    /// The value of the integer.
    pub value: i32,

    /// The location of the integer in the source code.
    pub location: Location,
}

impl Element for Int {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, // Add
    Sub, // Subtract
    Mul, // Multiply
    Div, // Divide
    Rem, // Rem
    Eq,  // Equal
    Neq, // Not equal
    Lt,  // Less than
    Gt,  // Greater than
    Lte, // Less than or equal to
    Gte, // Greater than or equal to
    And, // And
    Or,  // Or
}

#[derive(Debug, Clone, Deserialize)]
pub struct Binary {
    pub lhs: Box<Term>,
    pub op: BinaryOp,
    pub rhs: Box<Term>,
    pub location: Location,
}

impl Element for Binary {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Call {
    pub callee: Box<Term>,
    pub arguments: Vec<Term>,
    pub location: Location,
}

impl Element for Call {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Function {
    pub parameters: Vec<Var>,
    pub value: Box<Term>,
    pub location: Location,
}

impl Element for Function {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Print {
    pub value: Box<Term>,
    pub location: Location,
}

impl Element for Print {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct First {
    pub value: Box<Term>,
    pub location: Location,
}

impl Element for First {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Second {
    pub value: Box<Term>,
    pub location: Location,
}

impl Element for Second {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Tuple {
    pub first: Box<Term>,
    pub second: Box<Term>,
    pub location: Location,
}

impl Element for Tuple {
    fn location(&self) -> &Location {
        &self.location
    }
}

/// A name reference or definition in the source code.
///
/// It's useful to know the location of the name in the source code
/// and the name itself to be resolved later.
#[derive(Debug, Clone, Deserialize)]
pub struct Var {
    pub text: String,
    pub location: Location,
}

impl Element for Var {
    fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum Term {
    Error(Error),
    Int(Int),
    Str(Str),
    Call(Call),
    Binary(Binary),
    Function(Function),
    Let(Let),
    If(If),
    Print(Print),
    First(First),
    Second(Second),
    Bool(Bool),
    Tuple(Tuple),
    Var(Var),
}

impl Element for Term {
    fn location(&self) -> &Location {
        match self {
            Term::Error(arg0) => &arg0.location,
            Term::Int(arg0) => &arg0.location,
            Term::Str(arg0) => &arg0.location,
            Term::Function(arg0) => &arg0.location,
            Term::Call(arg0) => arg0.location(),
            Term::Var(arg0) => arg0.location(),
            Term::Binary(arg0) => &arg0.location,
            Term::Print(arg0) => &arg0.location,
            Term::First(arg0) => &arg0.location,
            Term::Second(arg0) => &arg0.location,
            Term::Let(arg0) => &arg0.location,
            Term::If(arg0) => &arg0.location,
            Term::Bool(arg0) => &arg0.location,
            Term::Tuple(arg0) => arg0.location(),
        }
    }
}
