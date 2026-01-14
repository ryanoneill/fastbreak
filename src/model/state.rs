//! Runtime state representation
//!
//! This module defines the runtime state used for evaluating expressions,
//! checking invariants, and running scenarios.

use crate::semantic::Type;
use indexmap::IndexMap;
use smol_str::SmolStr;
use std::fmt;
use std::sync::Arc;

/// A runtime value
///
/// Note: Function values cannot be meaningfully compared.
#[derive(Debug, Clone)]
pub enum Value {
    /// Integer value
    Int(i64),
    /// Float value
    Float(f64),
    /// String value
    String(Arc<str>),
    /// Boolean value
    Bool(bool),
    /// Unit value
    Unit,
    /// A struct instance
    Struct {
        /// Type name
        ty: SmolStr,
        /// Field values
        fields: IndexMap<SmolStr, Value>,
    },
    /// An enum variant
    Enum {
        /// Type name
        ty: SmolStr,
        /// Variant name
        variant: SmolStr,
        /// Variant fields (if any)
        fields: IndexMap<SmolStr, Value>,
    },
    /// A set of values (stored as Vec for simplicity)
    Set(Vec<Value>),
    /// A list of values
    List(Vec<Value>),
    /// A map of key-value pairs (stored as Vec of pairs for simplicity)
    Map(Vec<(Value, Value)>),
    /// An optional value
    Option(Option<Box<Value>>),
    /// A result value
    Result(Result<Box<Value>, Box<Value>>),
    /// A tuple of values
    Tuple(Vec<Value>),
    /// A function/closure (stored as AST for now)
    Function(Arc<crate::ast::Expr>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (
                Value::Struct {
                    ty: ty1,
                    fields: f1,
                },
                Value::Struct {
                    ty: ty2,
                    fields: f2,
                },
            ) => ty1 == ty2 && f1 == f2,
            (
                Value::Enum {
                    ty: ty1,
                    variant: v1,
                    fields: f1,
                },
                Value::Enum {
                    ty: ty2,
                    variant: v2,
                    fields: f2,
                },
            ) => ty1 == ty2 && v1 == v2 && f1 == f2,
            (Value::Set(a), Value::Set(b))
            | (Value::List(a), Value::List(b))
            | (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Option(a), Value::Option(b)) => a == b,
            (Value::Result(a), Value::Result(b)) => a == b,
            // Functions are never equal, and mismatched types are not equal
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Value {
    /// Create a new integer value
    #[must_use]
    pub const fn int(n: i64) -> Self {
        Value::Int(n)
    }

    /// Create a new string value
    #[must_use]
    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Value::String(s.into())
    }

    /// Create a new boolean value
    #[must_use]
    pub const fn bool(b: bool) -> Self {
        Value::Bool(b)
    }

    /// Create a new empty set
    #[must_use]
    pub fn empty_set() -> Self {
        Value::Set(Vec::new())
    }

    /// Create a new empty list
    #[must_use]
    pub fn empty_list() -> Self {
        Value::List(Vec::new())
    }

    /// Create a new empty map
    #[must_use]
    pub fn empty_map() -> Self {
        Value::Map(Vec::new())
    }

    /// Create a Some value
    #[must_use]
    pub fn some(value: Value) -> Self {
        Value::Option(Some(Box::new(value)))
    }

    /// Create a None value
    #[must_use]
    pub fn none() -> Self {
        Value::Option(None)
    }

    /// Create an Ok value
    #[must_use]
    pub fn ok(value: Value) -> Self {
        Value::Result(Ok(Box::new(value)))
    }

    /// Create an Err value
    #[must_use]
    pub fn err(value: Value) -> Self {
        Value::Result(Err(Box::new(value)))
    }

    /// Check if this value is truthy (for boolean contexts)
    #[must_use]
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Option(opt) => opt.is_some(),
            Value::Result(res) => res.is_ok(),
            _ => true,
        }
    }

    /// Get the type of this value
    #[must_use]
    pub fn value_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::Unit => Type::Unit,
            Value::Struct { .. }
            | Value::Enum { .. }
            | Value::Set(_)
            | Value::List(_)
            | Value::Map(_)
            | Value::Option(_)
            | Value::Result(_)
            | Value::Tuple(_)
            | Value::Function(_) => Type::Unknown,
        }
    }

    /// Check if this value is contained in a set
    #[must_use]
    pub fn is_in_set(&self, set: &[Value]) -> bool {
        set.contains(self)
    }

    /// Check if this value is a key in a map
    #[must_use]
    pub fn is_in_map(&self, map: &[(Value, Value)]) -> bool {
        map.iter().any(|(k, _)| k == self)
    }

    /// Get a value from a map by key
    #[must_use]
    pub fn get_from_map<'a>(&self, map: &'a [(Value, Value)]) -> Option<&'a Value> {
        map.iter().find(|(k, _)| k == self).map(|(_, v)| v)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Unit => write!(f, "()"),
            Value::Struct { ty, fields } => {
                write!(f, "{ty} {{")?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, " {name}: {value}")?;
                }
                write!(f, " }}")
            }
            Value::Enum {
                ty: _,
                variant,
                fields,
            } => {
                write!(f, "{variant}")?;
                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, (_, value)) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{value}")?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Value::Set(items) => {
                write!(f, "{{")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "}}")
            }
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            Value::Map(pairs) => {
                write!(f, "{{")?;
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}: {v}")?;
                }
                write!(f, "}}")
            }
            Value::Option(opt) => match opt {
                Some(v) => write!(f, "Some({v})"),
                None => write!(f, "None"),
            },
            Value::Result(res) => match res {
                Ok(v) => write!(f, "Ok({v})"),
                Err(e) => write!(f, "Err({e})"),
            },
            Value::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Function(_) => write!(f, "<function>"),
        }
    }
}

/// Runtime state environment
#[derive(Debug, Clone)]
pub struct Environment {
    /// Variable bindings (scoped)
    scopes: Vec<IndexMap<SmolStr, Value>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    /// Create a new empty environment
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![IndexMap::new()],
        }
    }

    /// Enter a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(IndexMap::new());
    }

    /// Leave the current scope
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a variable in the current scope
    pub fn define(&mut self, name: impl Into<SmolStr>, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.into(), value);
        }
    }

    /// Look up a variable
    #[must_use]
    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Update a variable (searches all scopes)
    pub fn set(&mut self, name: &str, value: Value) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(SmolStr::new(name), value);
                return true;
            }
        }
        false
    }

    /// Get all variables in the current scope
    ///
    /// # Panics
    ///
    /// Never panics - the environment always maintains at least one scope.
    #[must_use]
    pub fn current_scope(&self) -> &IndexMap<SmolStr, Value> {
        self.scopes
            .last()
            .expect("Environment always has at least one scope")
    }
}

/// A snapshot of the system state
#[derive(Debug, Clone)]
pub struct StateSnapshot {
    /// State variable values
    pub variables: IndexMap<SmolStr, Value>,
    /// The state name this snapshot is for
    pub state_name: Option<SmolStr>,
}

impl StateSnapshot {
    /// Create a new empty state snapshot
    #[must_use]
    pub fn new() -> Self {
        Self {
            variables: IndexMap::new(),
            state_name: None,
        }
    }

    /// Create a state snapshot for a named state
    #[must_use]
    pub fn for_state(name: impl Into<SmolStr>) -> Self {
        Self {
            variables: IndexMap::new(),
            state_name: Some(name.into()),
        }
    }

    /// Set a state variable
    pub fn set(&mut self, name: impl Into<SmolStr>, value: Value) {
        self.variables.insert(name.into(), value);
    }

    /// Get a state variable
    #[must_use]
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }
}

impl Default for StateSnapshot {
    fn default() -> Self {
        Self::new()
    }
}

/// A trace of state transitions
#[derive(Debug, Clone)]
pub struct Trace {
    /// Sequence of states
    pub states: Vec<StateSnapshot>,
    /// Actions that caused each transition
    pub actions: Vec<SmolStr>,
}

impl Default for Trace {
    fn default() -> Self {
        Self::new()
    }
}

impl Trace {
    /// Create a new empty trace
    #[must_use]
    pub fn new() -> Self {
        Self {
            states: Vec::new(),
            actions: Vec::new(),
        }
    }

    /// Add an initial state
    pub fn add_initial(&mut self, state: StateSnapshot) {
        self.states.push(state);
    }

    /// Add a transition
    pub fn add_transition(&mut self, action: impl Into<SmolStr>, new_state: StateSnapshot) {
        self.actions.push(action.into());
        self.states.push(new_state);
    }

    /// Get the current state
    #[must_use]
    pub fn current(&self) -> Option<&StateSnapshot> {
        self.states.last()
    }

    /// Get the previous state
    #[must_use]
    pub fn previous(&self) -> Option<&StateSnapshot> {
        if self.states.len() > 1 {
            self.states.get(self.states.len() - 2)
        } else {
            None
        }
    }

    /// Get the number of transitions
    #[must_use]
    pub fn transition_count(&self) -> usize {
        self.actions.len()
    }
}
