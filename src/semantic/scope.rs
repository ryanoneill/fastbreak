//! Symbol tables and scope management

use super::types::{ActionInfo, EnumInfo, RelationInfo, StateInfo, StructInfo, Type, TypeId};
use crate::Span;
use indexmap::IndexMap;
use smol_str::SmolStr;

/// A symbol in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Symbol name
    pub name: SmolStr,
    /// Symbol kind
    pub kind: SymbolKind,
    /// Definition span
    pub span: Span,
}

impl Symbol {
    /// Create a new symbol
    #[must_use]
    pub fn new(name: impl Into<SmolStr>, kind: SymbolKind, span: Span) -> Self {
        Self {
            name: name.into(),
            kind,
            span,
        }
    }
}

/// The kind of a symbol
#[derive(Debug, Clone)]
pub enum SymbolKind {
    /// A type definition
    Type(TypeId),
    /// An enum definition
    Enum(TypeId),
    /// A state definition
    State(SmolStr),
    /// An action definition
    Action(SmolStr),
    /// A relation definition
    Relation(SmolStr),
    /// A local variable
    Variable(Type),
    /// A type parameter
    TypeParam(SmolStr),
    /// A function parameter
    Parameter(Type),
    /// A field
    Field(Type),
}

/// A scope containing symbols
#[derive(Debug)]
pub struct Scope {
    /// Symbols in this scope
    symbols: IndexMap<SmolStr, Symbol>,
    /// Parent scope index (None for global scope)
    parent: Option<usize>,
    /// Scope kind
    pub kind: ScopeKind,
}

impl Scope {
    /// Create a new scope
    #[must_use]
    pub fn new(kind: ScopeKind, parent: Option<usize>) -> Self {
        Self {
            symbols: IndexMap::new(),
            parent,
            kind,
        }
    }

    /// Define a symbol in this scope
    pub fn define(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    /// Look up a symbol in this scope only (not parents)
    #[must_use]
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    /// Check if a symbol exists in this scope only
    #[must_use]
    pub fn contains(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Get the parent scope index
    #[must_use]
    pub fn parent(&self) -> Option<usize> {
        self.parent
    }
}

/// The kind of scope
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Global/module scope
    Global,
    /// Type definition scope (for type parameters)
    TypeDef,
    /// State definition scope
    State,
    /// Action scope
    Action,
    /// Invariant scope
    Invariant,
    /// Scenario scope
    Scenario,
    /// Given clause scope
    Given,
    /// When clause scope
    When,
    /// Then clause scope
    Then,
    /// Property scope
    Property,
    /// Block/expression scope
    Block,
    /// Lambda scope
    Lambda,
    /// Quantifier scope (forall/exists)
    Quantifier,
    /// Match arm scope
    Match,
}

/// The symbol table managing all scopes
#[derive(Debug)]
pub struct SymbolTable {
    /// All scopes
    scopes: Vec<Scope>,
    /// Current scope index
    current: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    /// Create a new symbol table with a global scope
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new(ScopeKind::Global, None)],
            current: 0,
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self, kind: ScopeKind) {
        let parent = self.current;
        self.scopes.push(Scope::new(kind, Some(parent)));
        self.current = self.scopes.len() - 1;
    }

    /// Leave the current scope
    pub fn leave_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current].parent {
            self.current = parent;
        }
    }

    /// Define a symbol in the current scope
    pub fn define(&mut self, symbol: Symbol) {
        self.scopes[self.current].define(symbol);
    }

    /// Look up a symbol, searching through parent scopes
    #[must_use]
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut scope_idx = self.current;
        loop {
            if let Some(symbol) = self.scopes[scope_idx].get(name) {
                return Some(symbol);
            }
            match self.scopes[scope_idx].parent {
                Some(parent) => scope_idx = parent,
                None => return None,
            }
        }
    }

    /// Look up a symbol in the current scope only
    #[must_use]
    pub fn lookup_local(&self, name: &str) -> Option<&Symbol> {
        self.scopes[self.current].get(name)
    }

    /// Check if a symbol exists in the current scope only
    #[must_use]
    pub fn is_defined_locally(&self, name: &str) -> bool {
        self.scopes[self.current].contains(name)
    }

    /// Get the current scope kind
    #[must_use]
    pub fn current_scope_kind(&self) -> ScopeKind {
        self.scopes[self.current].kind
    }

    /// Get the current scope
    #[must_use]
    pub fn current_scope(&self) -> &Scope {
        &self.scopes[self.current]
    }
}

/// Registry of all type information
#[derive(Debug, Default)]
pub struct TypeRegistry {
    /// Struct definitions
    structs: IndexMap<SmolStr, StructInfo>,
    /// Enum definitions
    enums: IndexMap<SmolStr, EnumInfo>,
    /// State definitions
    states: IndexMap<SmolStr, StateInfo>,
    /// Action definitions
    actions: IndexMap<SmolStr, ActionInfo>,
    /// Relation definitions
    relations: IndexMap<SmolStr, RelationInfo>,
    /// Next type index
    next_type_index: usize,
}

impl TypeRegistry {
    /// Create a new type registry
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a struct type
    pub fn register_struct(&mut self, info: StructInfo) {
        self.structs.insert(info.id.name.clone(), info);
    }

    /// Register an enum type
    pub fn register_enum(&mut self, info: EnumInfo) {
        self.enums.insert(info.id.name.clone(), info);
    }

    /// Register a state
    pub fn register_state(&mut self, info: StateInfo) {
        self.states.insert(info.name.clone(), info);
    }

    /// Register an action
    pub fn register_action(&mut self, info: ActionInfo) {
        self.actions.insert(info.name.clone(), info);
    }

    /// Register a relation
    pub fn register_relation(&mut self, info: RelationInfo) {
        self.relations.insert(info.name.clone(), info);
    }

    /// Get struct info
    #[must_use]
    pub fn get_struct(&self, name: &str) -> Option<&StructInfo> {
        self.structs.get(name)
    }

    /// Get mutable struct info
    pub fn get_struct_mut(&mut self, name: &str) -> Option<&mut StructInfo> {
        self.structs.get_mut(name)
    }

    /// Get enum info
    #[must_use]
    pub fn get_enum(&self, name: &str) -> Option<&EnumInfo> {
        self.enums.get(name)
    }

    /// Get mutable enum info
    pub fn get_enum_mut(&mut self, name: &str) -> Option<&mut EnumInfo> {
        self.enums.get_mut(name)
    }

    /// Get state info
    #[must_use]
    pub fn get_state(&self, name: &str) -> Option<&StateInfo> {
        self.states.get(name)
    }

    /// Get action info
    #[must_use]
    pub fn get_action(&self, name: &str) -> Option<&ActionInfo> {
        self.actions.get(name)
    }

    /// Get relation info
    #[must_use]
    pub fn get_relation(&self, name: &str) -> Option<&RelationInfo> {
        self.relations.get(name)
    }

    /// Allocate a new type ID
    pub fn alloc_type_id(&mut self, name: impl Into<SmolStr>) -> TypeId {
        let id = TypeId::new(name, self.next_type_index);
        self.next_type_index += 1;
        id
    }

    /// Get all struct names
    pub fn struct_names(&self) -> impl Iterator<Item = &SmolStr> {
        self.structs.keys()
    }

    /// Get all enum names
    pub fn enum_names(&self) -> impl Iterator<Item = &SmolStr> {
        self.enums.keys()
    }

    /// Get all state names
    pub fn state_names(&self) -> impl Iterator<Item = &SmolStr> {
        self.states.keys()
    }

    /// Get all action names
    pub fn action_names(&self) -> impl Iterator<Item = &SmolStr> {
        self.actions.keys()
    }

    /// Iterate over all structs
    pub fn structs(&self) -> impl Iterator<Item = &StructInfo> {
        self.structs.values()
    }

    /// Iterate over all enums
    pub fn enums(&self) -> impl Iterator<Item = &EnumInfo> {
        self.enums.values()
    }
}
