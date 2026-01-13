//! Semantic analysis for Fastbreak specifications
//!
//! This module performs:
//! - Name resolution (resolving references to definitions)
//! - Type checking (verifying type consistency)
//! - Validation (checking semantic constraints)
//! - Module tracking (for multi-file specifications)

mod error;
mod modules;
mod scope;
mod types;

pub use error::{Diagnostics, SemanticError, SemanticResult};
pub use modules::{ImportedItem, ModuleInfo, ModuleRegistry, QualifiedName, ResolvedImport};
pub use scope::{ScopeKind, Symbol, SymbolKind, SymbolTable, TypeRegistry};
pub use types::{
    ActionInfo, EnumInfo, RelationInfo, StateInfo, StructInfo, Type, TypeId, TypeVarId,
    VariantInfo,
};

use crate::ast::{
    Action, Alternative, Attribute, AttributeArg, BinaryOp, BuiltInType, EnumDef, Expr, ExprKind,
    Literal, Pattern, PatternKind, Quality, QualityValue, QuantBindingKind, Relation, Scenario,
    Specification, StateBlock, TypeDef, TypeRef, TypeRefKind, UnaryOp,
};
use crate::Span;
use indexmap::IndexMap;
use smol_str::SmolStr;
use std::sync::Arc;

/// Semantic analyzer for Fastbreak specifications
#[derive(Debug)]
pub struct Analyzer {
    /// Symbol table
    pub symbols: SymbolTable,
    /// Type registry
    pub types: TypeRegistry,
    /// Collected diagnostics
    pub diagnostics: Diagnostics,
    /// Registry of @id values to their spans (for duplicate detection)
    id_registry: IndexMap<SmolStr, Span>,
    /// The type of `self` during refinement predicate analysis
    refinement_subject: Option<Type>,
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl Analyzer {
    /// Create a new analyzer
    #[must_use]
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            types: TypeRegistry::new(),
            diagnostics: Diagnostics::new(),
            id_registry: IndexMap::new(),
            refinement_subject: None,
        }
    }

    /// Analyze a specification
    pub fn analyze(&mut self, spec: &Specification) {
        // Phase 1: Register all type definitions (forward declarations)
        self.register_types(spec);

        // Phase 2: Resolve type bodies (fields, variants)
        self.resolve_type_bodies(spec);

        // Phase 3: Register and resolve states, actions, relations
        self.resolve_definitions(spec);

        // Phase 4: Check scenarios, properties, and qualities
        self.check_scenarios(spec);
        self.check_properties(spec);
        self.check_qualities(spec);

        // Phase 5: Validate attributes (IDs, references)
        self.validate_all_attributes(spec);
    }

    /// Check if analysis succeeded (no errors)
    #[must_use]
    pub fn succeeded(&self) -> bool {
        !self.diagnostics.has_errors()
    }

    /// Check if there are any errors
    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Get all errors
    #[must_use]
    pub fn errors(&self) -> &[SemanticError] {
        self.diagnostics.errors()
    }

    // ========== Phase 1: Register Types ==========

    fn register_types(&mut self, spec: &Specification) {
        // Register struct types
        for type_def in &spec.types {
            let name = &type_def.name.name;
            if self.symbols.is_defined_locally(name) {
                if let Some(existing) = self.symbols.lookup(name) {
                    self.diagnostics.error(SemanticError::duplicate(
                        "type",
                        name.as_str(),
                        type_def.name.span,
                        existing.span,
                    ));
                }
            } else {
                let type_id = self.types.alloc_type_id(name.clone());
                self.symbols.define(Symbol::new(
                    name.clone(),
                    SymbolKind::Type(type_id.clone()),
                    type_def.name.span,
                ));
                self.types.register_struct(StructInfo::new(type_id));
            }
        }

        // Register enum types
        for enum_def in &spec.enums {
            let name = &enum_def.name.name;
            if self.symbols.is_defined_locally(name) {
                if let Some(existing) = self.symbols.lookup(name) {
                    self.diagnostics.error(SemanticError::duplicate(
                        "enum",
                        name.as_str(),
                        enum_def.name.span,
                        existing.span,
                    ));
                }
            } else {
                let type_id = self.types.alloc_type_id(name.clone());
                self.symbols.define(Symbol::new(
                    name.clone(),
                    SymbolKind::Enum(type_id.clone()),
                    enum_def.name.span,
                ));
                self.types.register_enum(EnumInfo::new(type_id));
            }
        }
    }

    // ========== Phase 2: Resolve Type Bodies ==========

    fn resolve_type_bodies(&mut self, spec: &Specification) {
        // Resolve struct fields
        for type_def in &spec.types {
            self.resolve_struct_body(type_def);
        }

        // Resolve enum variants
        for enum_def in &spec.enums {
            self.resolve_enum_body(enum_def);
        }
    }

    fn resolve_struct_body(&mut self, type_def: &TypeDef) {
        let name = &type_def.name.name;

        // Enter type scope for type parameters
        self.symbols.enter_scope(ScopeKind::TypeDef);

        // Register type parameters
        let type_params: Vec<SmolStr> = type_def
            .type_params
            .iter()
            .map(|p| {
                self.symbols.define(Symbol::new(
                    p.name.clone(),
                    SymbolKind::TypeParam(p.name.clone()),
                    p.span,
                ));
                p.name.clone()
            })
            .collect();

        // Resolve fields
        let mut fields = indexmap::IndexMap::new();
        for field in &type_def.fields {
            let field_type = self.resolve_type(&field.ty);
            fields.insert(field.name.name.clone(), field_type);
        }

        self.symbols.leave_scope();

        // Update struct info
        if let Some(info) = self.types.get_struct_mut(name) {
            info.type_params = type_params;
            info.fields = fields;
        }
    }

    fn resolve_enum_body(&mut self, enum_def: &EnumDef) {
        let name = &enum_def.name.name;

        // Enter type scope for type parameters
        self.symbols.enter_scope(ScopeKind::TypeDef);

        // Register type parameters
        let type_params: Vec<SmolStr> = enum_def
            .type_params
            .iter()
            .map(|p| {
                self.symbols.define(Symbol::new(
                    p.name.clone(),
                    SymbolKind::TypeParam(p.name.clone()),
                    p.span,
                ));
                p.name.clone()
            })
            .collect();

        // Resolve variants
        let mut variants = indexmap::IndexMap::new();
        for variant in &enum_def.variants {
            let variant_info = if variant.fields.is_empty() {
                VariantInfo::unit(variant.name.name.clone())
            } else {
                // Check if tuple variant (numeric field names) or struct variant
                let is_tuple = variant
                    .fields
                    .first()
                    .is_some_and(|f| f.name.name.parse::<usize>().is_ok());

                if is_tuple {
                    let types: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| self.resolve_type(&f.ty))
                        .collect();
                    VariantInfo::tuple(variant.name.name.clone(), types)
                } else {
                    let mut fields = indexmap::IndexMap::new();
                    for field in &variant.fields {
                        fields.insert(field.name.name.clone(), self.resolve_type(&field.ty));
                    }
                    VariantInfo {
                        name: variant.name.name.clone(),
                        fields,
                        is_tuple: false,
                    }
                }
            };
            variants.insert(variant.name.name.clone(), variant_info);
        }

        self.symbols.leave_scope();

        // Update enum info
        if let Some(info) = self.types.get_enum_mut(name) {
            info.type_params = type_params;
            info.variants = variants;
        }
    }

    // ========== Phase 3: Resolve Definitions ==========

    fn resolve_definitions(&mut self, spec: &Specification) {
        // Resolve relations
        for relation in &spec.relations {
            self.resolve_relation(relation);
        }

        // Resolve states
        for state in &spec.states {
            self.resolve_state(state);
        }

        // Resolve actions
        for action in &spec.actions {
            self.resolve_action(action);
        }
    }

    fn resolve_relation(&mut self, relation: &Relation) {
        let name = &relation.name.name;

        if self.symbols.is_defined_locally(name) {
            if let Some(existing) = self.symbols.lookup(name) {
                self.diagnostics.error(SemanticError::duplicate(
                    "relation",
                    name.as_str(),
                    relation.name.span,
                    existing.span,
                ));
                return;
            }
        }

        let source = self.resolve_type(&relation.source);
        let target = self.resolve_type(&relation.target);

        self.symbols.define(Symbol::new(
            name.clone(),
            SymbolKind::Relation(name.clone()),
            relation.name.span,
        ));

        self.types
            .register_relation(RelationInfo::new(name.clone(), source, target));
    }

    fn resolve_state(&mut self, state: &StateBlock) {
        let name = &state.name.name;

        if self.symbols.is_defined_locally(name) {
            if let Some(existing) = self.symbols.lookup(name) {
                self.diagnostics.error(SemanticError::duplicate(
                    "state",
                    name.as_str(),
                    state.name.span,
                    existing.span,
                ));
                return;
            }
        }

        self.symbols.define(Symbol::new(
            name.clone(),
            SymbolKind::State(name.clone()),
            state.name.span,
        ));

        let mut state_info = StateInfo::new(name.clone());

        // Enter state scope
        self.symbols.enter_scope(ScopeKind::State);

        // Resolve fields
        for field in &state.fields {
            let field_type = self.resolve_type(&field.ty);
            state_info.fields.insert(field.name.name.clone(), field_type.clone());

            self.symbols.define(Symbol::new(
                field.name.name.clone(),
                SymbolKind::Field(field_type),
                field.name.span,
            ));
        }

        // Check invariants
        for invariant in &state.invariants {
            self.symbols.enter_scope(ScopeKind::Invariant);
            let inv_type = self.check_expr(&invariant.expr);
            self.expect_type(&inv_type, &Type::Bool, invariant.expr.span);
            self.symbols.leave_scope();
        }

        self.symbols.leave_scope();

        self.types.register_state(state_info);
    }

    fn resolve_action(&mut self, action: &Action) {
        let name = &action.name.name;

        if self.symbols.is_defined_locally(name) {
            if let Some(existing) = self.symbols.lookup(name) {
                self.diagnostics.error(SemanticError::duplicate(
                    "action",
                    name.as_str(),
                    action.name.span,
                    existing.span,
                ));
                return;
            }
        }

        let return_type = action
            .return_type
            .as_ref()
            .map_or(Type::Unit, |t| self.resolve_type(t));

        self.symbols.define(Symbol::new(
            name.clone(),
            SymbolKind::Action(name.clone()),
            action.name.span,
        ));

        let mut action_info = ActionInfo::new(name.clone(), return_type.clone());

        // Enter action scope
        self.symbols.enter_scope(ScopeKind::Action);

        // Resolve parameters
        for param in &action.params {
            let param_type = self.resolve_type(&param.ty);
            action_info.params.insert(param.name.name.clone(), param_type.clone());

            self.symbols.define(Symbol::new(
                param.name.name.clone(),
                SymbolKind::Parameter(param_type),
                param.name.span,
            ));
        }

        // Define 'result' variable for postconditions
        self.symbols.define(Symbol::new(
            SmolStr::new("result"),
            SymbolKind::Variable(return_type),
            action.name.span,
        ));

        // Check contracts
        for contract in &action.contracts {
            let contract_type = self.check_expr(&contract.expr);
            self.expect_type(&contract_type, &Type::Bool, contract.expr.span);
        }

        self.symbols.leave_scope();

        self.types.register_action(action_info);
    }

    // ========== Phase 4: Check Scenarios and Properties ==========

    fn check_scenarios(&mut self, spec: &Specification) {
        for scenario in &spec.scenarios {
            self.check_scenario(scenario);
        }
    }

    fn check_scenario(&mut self, scenario: &Scenario) {
        self.symbols.enter_scope(ScopeKind::Scenario);

        // Check given clause
        self.symbols.enter_scope(ScopeKind::Given);
        for binding in &scenario.given.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }
        self.symbols.leave_scope();

        // Check when clause (inherits from given)
        self.symbols.enter_scope(ScopeKind::When);
        // Re-add given bindings
        for binding in &scenario.given.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }
        for binding in &scenario.when.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }
        self.symbols.leave_scope();

        // Check then clause
        self.symbols.enter_scope(ScopeKind::Then);
        // Re-add all bindings
        for binding in &scenario.given.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }
        for binding in &scenario.when.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }
        for assertion in &scenario.then.assertions {
            let assertion_type = self.check_expr(&assertion.expr);
            self.expect_type(&assertion_type, &Type::Bool, assertion.expr.span);
        }
        self.symbols.leave_scope();

        // Check alternative flows
        for alternative in &scenario.alternatives {
            self.check_alternative(alternative, scenario);
        }

        self.symbols.leave_scope();
    }

    fn check_alternative(&mut self, alt: &Alternative, scenario: &Scenario) {
        self.symbols.enter_scope(ScopeKind::Given);

        // Re-add base given bindings
        for binding in &scenario.given.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }

        // Check condition (if present, must be Bool)
        if let Some(ref cond) = alt.condition {
            let cond_type = self.check_expr(cond);
            self.expect_type(&cond_type, &Type::Bool, cond.span);
        }

        // Add additional given bindings (if present)
        if let Some(ref given) = alt.given {
            for binding in &given.bindings {
                let value_type = self.check_expr(&binding.value);
                self.symbols.define(Symbol::new(
                    binding.name.name.clone(),
                    SymbolKind::Variable(value_type),
                    binding.name.span,
                ));
            }
        }

        // Use alternative's when or base when
        let when = alt.when.as_ref().unwrap_or(&scenario.when);
        for binding in &when.bindings {
            let value_type = self.check_expr(&binding.value);
            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(value_type),
                binding.name.span,
            ));
        }

        // Check then assertions
        for assertion in &alt.then.assertions {
            let assertion_type = self.check_expr(&assertion.expr);
            self.expect_type(&assertion_type, &Type::Bool, assertion.expr.span);
        }

        self.symbols.leave_scope();
    }

    fn check_properties(&mut self, spec: &Specification) {
        for property in &spec.properties {
            self.symbols.enter_scope(ScopeKind::Property);
            let prop_type = self.check_expr(&property.expr);
            self.expect_type(&prop_type, &Type::Bool, property.expr.span);
            self.symbols.leave_scope();
        }
    }

    fn check_qualities(&mut self, spec: &Specification) {
        for quality in &spec.qualities {
            self.check_quality(quality);
        }
    }

    fn check_quality(&mut self, quality: &Quality) {
        // Check if target value is an expression that needs type checking
        if let QualityValue::Expr(expr) = &quality.target.value {
            self.symbols.enter_scope(ScopeKind::Property);
            // Expression targets should evaluate to a comparable type
            let _expr_type = self.check_expr(expr);
            self.symbols.leave_scope();
        }

        // Check additional properties that might contain expressions
        for prop in &quality.properties {
            if let crate::ast::QualityPropertyValue::Target(target) = &prop.value {
                if let QualityValue::Expr(expr) = &target.value {
                    self.symbols.enter_scope(ScopeKind::Property);
                    let _expr_type = self.check_expr(expr);
                    self.symbols.leave_scope();
                }
            }
        }
    }

    // ========== Phase 5: Attribute Validation ==========

    fn validate_all_attributes(&mut self, spec: &Specification) {
        // Validate attributes on all constructs
        for type_def in &spec.types {
            self.validate_attributes(&type_def.attributes);
        }
        for enum_def in &spec.enums {
            self.validate_attributes(&enum_def.attributes);
        }
        for relation in &spec.relations {
            self.validate_attributes(&relation.attributes);
        }
        for state in &spec.states {
            self.validate_attributes(&state.attributes);
            for invariant in &state.invariants {
                self.validate_attributes(&invariant.attributes);
            }
        }
        for action in &spec.actions {
            self.validate_attributes(&action.attributes);
        }
        for scenario in &spec.scenarios {
            self.validate_attributes(&scenario.attributes);
        }
        for property in &spec.properties {
            self.validate_attributes(&property.attributes);
        }
        for quality in &spec.qualities {
            self.validate_attributes(&quality.attributes);
        }
    }

    fn validate_attributes(&mut self, attrs: &[Attribute]) {
        for attr in attrs {
            // Check for @id and validate uniqueness
            if attr.name.as_str() == "id" {
                if let Some(arg) = attr.args.first() {
                    let id_value = Self::extract_id_value(arg);
                    if let Some(existing_span) = self.id_registry.get(&id_value) {
                        self.diagnostics.error(SemanticError::DuplicateId {
                            id: id_value.to_string(),
                            span: attr.span,
                            original_span: *existing_span,
                        });
                    } else {
                        self.id_registry.insert(id_value, attr.span);
                    }
                }
            }
        }
    }

    fn extract_id_value(arg: &AttributeArg) -> SmolStr {
        match arg {
            AttributeArg::String(s, _) => s.clone(),
            AttributeArg::Ident(ident) => ident.name.clone(),
            AttributeArg::Int(n, _) => SmolStr::new(n.to_string()),
        }
    }

    // ========== Type Resolution ==========

    fn resolve_type(&mut self, type_ref: &TypeRef) -> Type {
        match &type_ref.kind {
            TypeRefKind::Named(path) => {
                let name = path.name().map_or("", |n| n.as_str());

                // Look up the type
                if let Some(symbol) = self.symbols.lookup(name) {
                    match &symbol.kind {
                        SymbolKind::Type(id) => Type::Struct(id.clone()),
                        SymbolKind::Enum(id) => Type::Enum(id.clone()),
                        SymbolKind::TypeParam(_) => {
                            // For now, treat type params as unknown
                            Type::Unknown
                        }
                        _ => {
                            self.diagnostics.error(SemanticError::InvalidType {
                                message: format!("`{name}` is not a type"),
                                span: type_ref.span,
                            });
                            Type::Error
                        }
                    }
                } else {
                    self.diagnostics.error(SemanticError::undefined(
                        "type",
                        name,
                        type_ref.span,
                    ));
                    Type::Error
                }
            }
            TypeRefKind::BuiltIn(builtin) => match builtin {
                BuiltInType::Int => Type::Int,
                BuiltInType::String => Type::String,
                BuiltInType::Bool => Type::Bool,
                BuiltInType::Set => {
                    self.diagnostics.error(SemanticError::ArityMismatch {
                        kind: "Set",
                        expected: 1,
                        found: 0,
                        span: type_ref.span,
                    });
                    Type::Error
                }
                BuiltInType::List => {
                    self.diagnostics.error(SemanticError::ArityMismatch {
                        kind: "List",
                        expected: 1,
                        found: 0,
                        span: type_ref.span,
                    });
                    Type::Error
                }
                BuiltInType::Map => {
                    self.diagnostics.error(SemanticError::ArityMismatch {
                        kind: "Map",
                        expected: 2,
                        found: 0,
                        span: type_ref.span,
                    });
                    Type::Error
                }
                BuiltInType::Option => {
                    self.diagnostics.error(SemanticError::ArityMismatch {
                        kind: "Option",
                        expected: 1,
                        found: 0,
                        span: type_ref.span,
                    });
                    Type::Error
                }
                BuiltInType::Result => {
                    self.diagnostics.error(SemanticError::ArityMismatch {
                        kind: "Result",
                        expected: 2,
                        found: 0,
                        span: type_ref.span,
                    });
                    Type::Error
                }
            },
            TypeRefKind::Generic { base, args } => {
                self.resolve_generic_type(base, args, type_ref.span)
            }
            TypeRefKind::Function { params, ret } => {
                let param_types: Vec<Type> = params.iter().map(|p| self.resolve_type(p)).collect();
                let ret_type = self.resolve_type(ret);
                Type::Function {
                    params: param_types,
                    ret: Arc::new(ret_type),
                }
            }
            TypeRefKind::Tuple(types) => {
                let element_types: Vec<Type> = types.iter().map(|t| self.resolve_type(t)).collect();
                Type::Tuple(element_types)
            }
            TypeRefKind::Unit => Type::Unit,
        }
    }

    fn resolve_generic_type(
        &mut self,
        base: &TypeRef,
        args: &[crate::ast::GenericArg],
        span: Span,
    ) -> Type {
        // Handle built-in generic types
        if let TypeRefKind::BuiltIn(builtin) = &base.kind {
            match builtin {
                BuiltInType::Set => {
                    if args.len() != 1 {
                        self.diagnostics.error(SemanticError::arity_mismatch(
                            "Set",
                            1,
                            args.len(),
                            span,
                        ));
                        return Type::Error;
                    }
                    let elem = self.resolve_type(&args[0].ty);
                    return Type::Set(Arc::new(elem));
                }
                BuiltInType::List => {
                    if args.len() != 1 {
                        self.diagnostics.error(SemanticError::arity_mismatch(
                            "List",
                            1,
                            args.len(),
                            span,
                        ));
                        return Type::Error;
                    }
                    let elem = self.resolve_type(&args[0].ty);
                    return Type::List(Arc::new(elem));
                }
                BuiltInType::Map => {
                    if args.len() != 2 {
                        self.diagnostics.error(SemanticError::arity_mismatch(
                            "Map",
                            2,
                            args.len(),
                            span,
                        ));
                        return Type::Error;
                    }
                    let key = self.resolve_type(&args[0].ty);
                    let value = self.resolve_type(&args[1].ty);
                    return Type::Map(Arc::new(key), Arc::new(value));
                }
                BuiltInType::Option => {
                    if args.len() != 1 {
                        self.diagnostics.error(SemanticError::arity_mismatch(
                            "Option",
                            1,
                            args.len(),
                            span,
                        ));
                        return Type::Error;
                    }
                    let elem = self.resolve_type(&args[0].ty);
                    return Type::Option(Arc::new(elem));
                }
                BuiltInType::Result => {
                    if args.len() != 2 {
                        self.diagnostics.error(SemanticError::arity_mismatch(
                            "Result",
                            2,
                            args.len(),
                            span,
                        ));
                        return Type::Error;
                    }
                    let ok = self.resolve_type(&args[0].ty);
                    let err = self.resolve_type(&args[1].ty);
                    return Type::Result(Arc::new(ok), Arc::new(err));
                }
                _ => {
                    self.diagnostics.error(SemanticError::InvalidType {
                        message: format!("`{builtin}` does not take type arguments"),
                        span,
                    });
                    return Type::Error;
                }
            }
        }

        // For user-defined generic types, resolve the base and ignore args for now
        // (full generic instantiation would require more infrastructure)
        self.resolve_type(base)
    }

    // ========== Type Checking ==========

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Literal(lit) => Self::check_literal(lit),
            ExprKind::Var(path) => self.check_var(path, expr.span),
            ExprKind::Prime(inner) | ExprKind::Old(inner) => self.check_expr(inner),
            ExprKind::Binary { left, op, right } => {
                self.check_binary(left, *op, right, expr.span)
            }
            ExprKind::Unary { op, expr: inner } => self.check_unary(*op, inner, expr.span),
            ExprKind::Field { base, field } => self.check_field(base, field, expr.span),
            ExprKind::Index { base, index } => self.check_index(base, index, expr.span),
            ExprKind::Call { func, args } => self.check_call(func, args, expr.span),
            ExprKind::MethodCall {
                receiver,
                method,
                args,
            } => self.check_method_call(receiver, method, args, expr.span),
            ExprKind::Forall { bindings, filter, body }
            | ExprKind::Exists { bindings, filter, body } => {
                self.check_quantifier(bindings, filter.as_deref(), body, expr.span)
            }
            ExprKind::Implies { antecedent, consequent } => {
                let ant_type = self.check_expr(antecedent);
                self.expect_type(&ant_type, &Type::Bool, antecedent.span);
                let cons_type = self.check_expr(consequent);
                self.expect_type(&cons_type, &Type::Bool, consequent.span);
                Type::Bool
            }
            ExprKind::Set(elements) => self.check_set(elements, expr.span),
            ExprKind::List(elements) => self.check_list(elements, expr.span),
            ExprKind::Map(pairs) => self.check_map(pairs, expr.span),
            ExprKind::Tuple(elements) => {
                let types: Vec<Type> = elements.iter().map(|e| self.check_expr(e)).collect();
                Type::Tuple(types)
            }
            ExprKind::Struct { ty, fields } => self.check_struct_literal(ty, fields, expr.span),
            ExprKind::Lambda { params, body } => self.check_lambda(params, body, expr.span),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.check_if(condition, then_branch, else_branch.as_deref(), expr.span),
            ExprKind::Match { expr: scrutinee, arms } => {
                self.check_match(scrutinee, arms, expr.span)
            }
            ExprKind::Let { name, ty, value, body } => {
                self.check_let(name, ty.as_ref(), value, body, expr.span)
            }
            ExprKind::Is { expr: inner, ty: _ } => {
                self.check_expr(inner);
                // `is` expressions always return Bool (type path validation is done during parsing)
                Type::Bool
            }
            ExprKind::Block(exprs) => {
                let mut result = Type::Unit;
                for e in exprs {
                    result = self.check_expr(e);
                }
                result
            }
            ExprKind::Range { start, end } => {
                let start_type = self.check_expr(start);
                self.expect_type(&start_type, &Type::Int, start.span);
                let end_type = self.check_expr(end);
                self.expect_type(&end_type, &Type::Int, end.span);
                Type::List(Arc::new(Type::Int))
            }
            ExprKind::Result => {
                // Look up result variable
                if let Some(symbol) = self.symbols.lookup("result") {
                    if let SymbolKind::Variable(ty) = &symbol.kind {
                        return ty.clone();
                    }
                }
                self.diagnostics.error(SemanticError::undefined(
                    "variable",
                    "result",
                    expr.span,
                ));
                Type::Error
            }
            ExprKind::SelfRef => {
                // `self` is only valid in refinement context
                if let Some(ref subject) = self.refinement_subject {
                    subject.clone()
                } else {
                    self.diagnostics.error(SemanticError::InvalidOperation {
                        message: "`self` can only be used in refinement predicates".to_string(),
                        span: expr.span,
                    });
                    Type::Error
                }
            }
        }
    }

    fn check_literal(lit: &Literal) -> Type {
        match lit {
            Literal::Int(_) => Type::Int,
            Literal::String(_) => Type::String,
            Literal::Bool(_) => Type::Bool,
            Literal::Unit => Type::Unit,
        }
    }

    fn check_var(&mut self, path: &crate::ast::Path, span: Span) -> Type {
        let name = path.name().map_or("", |n| n.as_str());

        // Special cases for Result/Option variants
        if name == "Ok" || name == "Err" || name == "Some" || name == "None" {
            return Type::Unknown; // These need context to determine type
        }

        if let Some(symbol) = self.symbols.lookup(name) {
            match &symbol.kind {
                SymbolKind::Variable(ty) | SymbolKind::Parameter(ty) | SymbolKind::Field(ty) => {
                    ty.clone()
                }
                SymbolKind::Action(action_name) => {
                    if let Some(info) = self.types.get_action(action_name) {
                        let params: Vec<Type> = info.params.values().cloned().collect();
                        Type::Function {
                            params,
                            ret: Arc::new(info.return_type.clone()),
                        }
                    } else {
                        Type::Error
                    }
                }
                SymbolKind::State(state_name) => {
                    // Return a struct-like type for state access
                    if self.types.get_state(state_name).is_some() {
                        // For simplicity, treat state as an opaque type for now
                        Type::Unknown
                    } else {
                        Type::Error
                    }
                }
                _ => Type::Unknown,
            }
        } else {
            self.diagnostics
                .error(SemanticError::undefined("variable", name, span));
            Type::Error
        }
    }

    fn check_binary(&mut self, left: &Expr, op: BinaryOp, right: &Expr, span: Span) -> Type {
        let left_type = self.check_expr(left);
        let right_type = self.check_expr(right);

        match op {
            // Arithmetic operators require Int
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                self.expect_type(&left_type, &Type::Int, left.span);
                self.expect_type(&right_type, &Type::Int, right.span);
                Type::Int
            }
            // Comparison operators
            BinaryOp::Eq | BinaryOp::NotEq => {
                // Types should match
                if !Self::types_compatible(&left_type, &right_type) {
                    self.diagnostics.error(SemanticError::type_mismatch(
                        left_type.to_string(),
                        right_type.to_string(),
                        span,
                    ));
                }
                Type::Bool
            }
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                self.expect_type(&left_type, &Type::Int, left.span);
                self.expect_type(&right_type, &Type::Int, right.span);
                Type::Bool
            }
            // Logical operators require Bool
            BinaryOp::And | BinaryOp::Or => {
                self.expect_type(&left_type, &Type::Bool, left.span);
                self.expect_type(&right_type, &Type::Bool, right.span);
                Type::Bool
            }
            // Set membership
            BinaryOp::In => {
                // Right should be a collection containing left's type
                Type::Bool
            }
            // Set operations
            BinaryOp::Union | BinaryOp::Intersect | BinaryOp::Difference => {
                // Both should be sets of the same type
                left_type
            }
        }
    }

    fn check_unary(&mut self, op: UnaryOp, expr: &Expr, _span: Span) -> Type {
        let expr_type = self.check_expr(expr);

        match op {
            UnaryOp::Not => {
                self.expect_type(&expr_type, &Type::Bool, expr.span);
                Type::Bool
            }
            UnaryOp::Neg => {
                self.expect_type(&expr_type, &Type::Int, expr.span);
                Type::Int
            }
        }
    }

    fn check_field(
        &mut self,
        base: &Expr,
        field: &crate::ast::Ident,
        span: Span,
    ) -> Type {
        let base_type = self.check_expr(base);

        match &base_type {
            Type::Struct(id) => {
                if let Some(info) = self.types.get_struct(&id.name) {
                    if let Some(field_type) = info.field(&field.name) {
                        return field_type.clone();
                    }
                }
                self.diagnostics.error(SemanticError::undefined(
                    "field",
                    field.as_str(),
                    span,
                ));
                Type::Error
            }
            Type::Unknown | Type::Error => Type::Unknown,
            _ => {
                self.diagnostics.error(SemanticError::InvalidOperation {
                    message: format!("cannot access field on type `{base_type}`"),
                    span,
                });
                Type::Error
            }
        }
    }

    fn check_index(&mut self, base: &Expr, index: &Expr, span: Span) -> Type {
        let base_type = self.check_expr(base);
        let index_type = self.check_expr(index);

        match &base_type {
            Type::List(elem) => {
                self.expect_type(&index_type, &Type::Int, index.span);
                (**elem).clone()
            }
            Type::Map(key, value) => {
                if !Self::types_compatible(&index_type, key) {
                    self.diagnostics.error(SemanticError::type_mismatch(
                        key.to_string(),
                        index_type.to_string(),
                        index.span,
                    ));
                }
                (**value).clone()
            }
            Type::Unknown | Type::Error => Type::Unknown,
            _ => {
                self.diagnostics.error(SemanticError::InvalidOperation {
                    message: format!("cannot index type `{base_type}`"),
                    span,
                });
                Type::Error
            }
        }
    }

    fn check_call(&mut self, func: &Expr, args: &[Expr], span: Span) -> Type {
        let func_type = self.check_expr(func);
        let arg_types: Vec<Type> = args.iter().map(|a| self.check_expr(a)).collect();

        match func_type {
            Type::Function { params, ret } => {
                if params.len() != arg_types.len() {
                    self.diagnostics.error(SemanticError::arity_mismatch(
                        "function",
                        params.len(),
                        arg_types.len(),
                        span,
                    ));
                }
                (*ret).clone()
            }
            Type::Unknown | Type::Error => Type::Unknown,
            _ => {
                self.diagnostics.error(SemanticError::InvalidOperation {
                    message: format!("cannot call type `{func_type}`"),
                    span,
                });
                Type::Error
            }
        }
    }

    fn check_method_call(
        &mut self,
        receiver: &Expr,
        method: &crate::ast::Ident,
        args: &[Expr],
        _span: Span,
    ) -> Type {
        let receiver_type = self.check_expr(receiver);
        let _arg_types: Vec<Type> = args.iter().map(|a| self.check_expr(a)).collect();

        // Handle common collection methods
        let method_name = method.as_str();
        match method_name {
            "len" => Type::Int,
            "is_empty" | "contains" => Type::Bool,
            "first" | "last" => {
                if let Some(elem) = receiver_type.element_type() {
                    Type::Option(Arc::new(elem.clone()))
                } else {
                    Type::Unknown
                }
            }
            "map" | "filter" => receiver_type,
            "keys" | "values" => {
                if let Type::Map(k, v) = &receiver_type {
                    if method_name == "keys" {
                        Type::Set(k.clone())
                    } else {
                        Type::List(v.clone())
                    }
                } else {
                    Type::Unknown
                }
            }
            "unwrap" => {
                match &receiver_type {
                    Type::Option(inner) => (**inner).clone(),
                    Type::Result(ok, _) => (**ok).clone(),
                    _ => Type::Unknown,
                }
            }
            _ => Type::Unknown,
        }
    }

    fn check_quantifier(
        &mut self,
        bindings: &[crate::ast::QuantBinding],
        filter: Option<&Expr>,
        body: &Expr,
        _span: Span,
    ) -> Type {
        self.symbols.enter_scope(ScopeKind::Quantifier);

        for binding in bindings {
            let elem_type = match &binding.kind {
                QuantBindingKind::InCollection(collection) => {
                    let collection_type = self.check_expr(collection);
                    collection_type.element_type().cloned().unwrap_or(Type::Unknown)
                }
                QuantBindingKind::Typed(type_ref) => self.resolve_type(type_ref),
            };

            self.symbols.define(Symbol::new(
                binding.name.name.clone(),
                SymbolKind::Variable(elem_type),
                binding.name.span,
            ));
        }

        if let Some(filter_expr) = filter {
            let filter_type = self.check_expr(filter_expr);
            self.expect_type(&filter_type, &Type::Bool, filter_expr.span);
        }

        let body_type = self.check_expr(body);
        self.expect_type(&body_type, &Type::Bool, body.span);

        self.symbols.leave_scope();
        Type::Bool
    }

    fn check_set(&mut self, elements: &[Expr], _span: Span) -> Type {
        if elements.is_empty() {
            return Type::Set(Arc::new(Type::Unknown));
        }

        let first_type = self.check_expr(&elements[0]);
        for elem in &elements[1..] {
            let elem_type = self.check_expr(elem);
            if !Self::types_compatible(&first_type, &elem_type) {
                self.diagnostics.error(SemanticError::type_mismatch(
                    first_type.to_string(),
                    elem_type.to_string(),
                    elem.span,
                ));
            }
        }

        Type::Set(Arc::new(first_type))
    }

    fn check_list(&mut self, elements: &[Expr], _span: Span) -> Type {
        if elements.is_empty() {
            return Type::List(Arc::new(Type::Unknown));
        }

        let first_type = self.check_expr(&elements[0]);
        for elem in &elements[1..] {
            let elem_type = self.check_expr(elem);
            if !Self::types_compatible(&first_type, &elem_type) {
                self.diagnostics.error(SemanticError::type_mismatch(
                    first_type.to_string(),
                    elem_type.to_string(),
                    elem.span,
                ));
            }
        }

        Type::List(Arc::new(first_type))
    }

    fn check_map(&mut self, pairs: &[(Expr, Expr)], _span: Span) -> Type {
        if pairs.is_empty() {
            return Type::Map(Arc::new(Type::Unknown), Arc::new(Type::Unknown));
        }

        let (first_key, first_value) = &pairs[0];
        let key_type = self.check_expr(first_key);
        let value_type = self.check_expr(first_value);

        for (key, value) in &pairs[1..] {
            let k_type = self.check_expr(key);
            let v_type = self.check_expr(value);
            if !Self::types_compatible(&key_type, &k_type) {
                self.diagnostics.error(SemanticError::type_mismatch(
                    key_type.to_string(),
                    k_type.to_string(),
                    key.span,
                ));
            }
            if !Self::types_compatible(&value_type, &v_type) {
                self.diagnostics.error(SemanticError::type_mismatch(
                    value_type.to_string(),
                    v_type.to_string(),
                    value.span,
                ));
            }
        }

        Type::Map(Arc::new(key_type), Arc::new(value_type))
    }

    fn check_struct_literal(
        &mut self,
        ty: &crate::ast::Path,
        fields: &[crate::ast::FieldInit],
        span: Span,
    ) -> Type {
        let type_name = ty.name().map_or("", |n| n.as_str());

        // First, look up the type and gather the info we need (cloning to avoid borrow issues)
        let struct_lookup = self.symbols.lookup(type_name).and_then(|symbol| {
            if let SymbolKind::Type(id) = &symbol.kind {
                self.types.get_struct(&id.name).map(|info| {
                    (id.clone(), info.fields.clone())
                })
            } else {
                None
            }
        });

        if let Some((type_id, struct_fields)) = struct_lookup {
            // Now check fields without holding borrows
            for field in fields {
                let field_name = field.name.as_str();
                if let Some(expected_type) = struct_fields.get(field_name) {
                    let value_type = self.check_expr(&field.value);
                    if !Self::types_compatible(expected_type, &value_type) {
                        self.diagnostics.error(SemanticError::type_mismatch(
                            expected_type.to_string(),
                            value_type.to_string(),
                            field.value.span,
                        ));
                    }
                } else {
                    self.diagnostics.error(SemanticError::undefined(
                        "field",
                        field_name,
                        field.name.span,
                    ));
                }
            }
            return Type::Struct(type_id);
        }

        self.diagnostics
            .error(SemanticError::undefined("type", type_name, span));
        Type::Error
    }

    fn check_lambda(
        &mut self,
        params: &[crate::ast::LambdaParam],
        body: &Expr,
        _span: Span,
    ) -> Type {
        self.symbols.enter_scope(ScopeKind::Lambda);

        let mut param_types = Vec::new();
        for param in params {
            let param_type = param
                .ty
                .as_ref()
                .map_or(Type::Unknown, |t| self.resolve_type(t));
            param_types.push(param_type.clone());

            self.symbols.define(Symbol::new(
                param.name.name.clone(),
                SymbolKind::Parameter(param_type),
                param.name.span,
            ));
        }

        let return_type = self.check_expr(body);
        self.symbols.leave_scope();

        Type::Function {
            params: param_types,
            ret: Arc::new(return_type),
        }
    }

    fn check_if(
        &mut self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
        _span: Span,
    ) -> Type {
        let cond_type = self.check_expr(condition);
        self.expect_type(&cond_type, &Type::Bool, condition.span);

        let then_type = self.check_expr(then_branch);

        if let Some(else_expr) = else_branch {
            let else_type = self.check_expr(else_expr);
            if !Self::types_compatible(&then_type, &else_type) {
                // Types don't match, but we'll use the then type
            }
            then_type
        } else {
            Type::Unit
        }
    }

    fn check_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[crate::ast::MatchArm],
        _span: Span,
    ) -> Type {
        let scrutinee_type = self.check_expr(scrutinee);

        let mut result_type = Type::Unknown;
        for arm in arms {
            self.symbols.enter_scope(ScopeKind::Match);
            self.check_pattern(&arm.pattern, &scrutinee_type);

            if let Some(guard) = &arm.guard {
                let guard_type = self.check_expr(guard);
                self.expect_type(&guard_type, &Type::Bool, guard.span);
            }

            let body_type = self.check_expr(&arm.body);
            if matches!(result_type, Type::Unknown) {
                result_type = body_type;
            }

            self.symbols.leave_scope();
        }

        result_type
    }

    fn check_let(
        &mut self,
        name: &crate::ast::Ident,
        ty: Option<&TypeRef>,
        value: &Expr,
        body: &Expr,
        _span: Span,
    ) -> Type {
        let value_type = self.check_expr(value);

        let var_type = if let Some(type_ref) = ty {
            let declared_type = self.resolve_type(type_ref);
            if !Self::types_compatible(&declared_type, &value_type) {
                self.diagnostics.error(SemanticError::type_mismatch(
                    declared_type.to_string(),
                    value_type.to_string(),
                    value.span,
                ));
            }
            declared_type
        } else {
            value_type
        };

        self.symbols.enter_scope(ScopeKind::Block);
        self.symbols.define(Symbol::new(
            name.name.clone(),
            SymbolKind::Variable(var_type),
            name.span,
        ));

        let result = self.check_expr(body);
        self.symbols.leave_scope();
        result
    }

    fn check_pattern(&mut self, pattern: &Pattern, expected_type: &Type) {
        match &pattern.kind {
            PatternKind::Wildcard | PatternKind::Literal(_) => {}
            PatternKind::Binding(ident) => {
                self.symbols.define(Symbol::new(
                    ident.name.clone(),
                    SymbolKind::Variable(expected_type.clone()),
                    ident.span,
                ));
            }
            PatternKind::Tuple(patterns) => {
                if let Type::Tuple(types) = expected_type {
                    for (pattern, ty) in patterns.iter().zip(types.iter()) {
                        self.check_pattern(pattern, ty);
                    }
                }
            }
            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    if let Some(inner_pattern) = &field.pattern {
                        self.check_pattern(inner_pattern, &Type::Unknown);
                    } else {
                        self.symbols.define(Symbol::new(
                            field.name.name.clone(),
                            SymbolKind::Variable(Type::Unknown),
                            field.name.span,
                        ));
                    }
                }
            }
            PatternKind::Variant { patterns, .. } => {
                for pattern in patterns {
                    self.check_pattern(pattern, &Type::Unknown);
                }
            }
            PatternKind::Or(patterns) => {
                for pattern in patterns {
                    self.check_pattern(pattern, expected_type);
                }
            }
        }
    }

    // ========== Helpers ==========

    fn expect_type(&mut self, actual: &Type, expected: &Type, span: Span) {
        if !Self::types_compatible(actual, expected) {
            self.diagnostics.error(SemanticError::type_mismatch(
                expected.to_string(),
                actual.to_string(),
                span,
            ));
        }
    }

    fn types_compatible(a: &Type, b: &Type) -> bool {
        // Unknown and Error types are compatible with everything (for error recovery)
        if matches!(a, Type::Unknown | Type::Error) || matches!(b, Type::Unknown | Type::Error) {
            return true;
        }

        // Check structural compatibility with nested Unknown handling
        match (a, b) {
            (Type::List(a_elem), Type::List(b_elem))
            | (Type::Set(a_elem), Type::Set(b_elem)) => Self::types_compatible(a_elem, b_elem),
            (Type::Option(a_inner), Type::Option(b_inner)) => {
                Self::types_compatible(a_inner, b_inner)
            }
            (Type::Map(a_key, a_val), Type::Map(b_key, b_val)) => {
                Self::types_compatible(a_key, b_key) && Self::types_compatible(a_val, b_val)
            }
            (Type::Result(a_ok, a_err), Type::Result(b_ok, b_err)) => {
                Self::types_compatible(a_ok, b_ok) && Self::types_compatible(a_err, b_err)
            }
            (Type::Tuple(a_elems), Type::Tuple(b_elems)) => {
                a_elems.len() == b_elems.len()
                    && a_elems
                        .iter()
                        .zip(b_elems.iter())
                        .all(|(a, b)| Self::types_compatible(a, b))
            }
            _ => a == b,
        }
    }
}

/// Analyze a specification
///
/// # Returns
///
/// Returns the analyzer with results and any diagnostics
#[must_use]
pub fn analyze(spec: &Specification) -> Analyzer {
    let mut analyzer = Analyzer::new();
    analyzer.analyze(spec);
    analyzer
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn analyze_source(source: &str) -> Analyzer {
        let spec = parse(source).expect("parse error");
        analyze(&spec)
    }

    #[test]
    fn test_analyze_empty() {
        let analyzer = analyze_source("");
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_type_def() {
        let analyzer = analyze_source("type User { id: Int, name: String }");
        assert!(analyzer.succeeded());
        assert!(analyzer.types.get_struct("User").is_some());
    }

    #[test]
    fn test_analyze_enum_def() {
        let analyzer = analyze_source("enum Status { Active, Inactive }");
        assert!(analyzer.succeeded());
        assert!(analyzer.types.get_enum("Status").is_some());
    }

    #[test]
    fn test_analyze_duplicate_type() {
        let analyzer = analyze_source("type Foo { x: Int } type Foo { y: String }");
        assert!(!analyzer.succeeded());
        assert_eq!(analyzer.diagnostics.len(), 1);
    }

    #[test]
    fn test_analyze_undefined_type() {
        let analyzer = analyze_source("type Foo { x: Bar }");
        assert!(!analyzer.succeeded());
    }

    #[test]
    fn test_analyze_state() {
        let analyzer = analyze_source(
            r#"
            type User { id: Int }
            state AppState {
                users: Set<User>,
                count: Int
            }
            "#,
        );
        assert!(analyzer.succeeded());
        assert!(analyzer.types.get_state("AppState").is_some());
    }

    #[test]
    fn test_analyze_action() {
        let analyzer = analyze_source(
            r#"
            action greet(name: String) -> String
            "#,
        );
        assert!(analyzer.succeeded());
        assert!(analyzer.types.get_action("greet").is_some());
    }

    #[test]
    fn test_analyze_invariant_type() {
        let analyzer = analyze_source(
            r#"
            state Test {
                x: Int,
                invariant "positive" { x > 0 }
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_scenario() {
        let analyzer = analyze_source(
            r#"
            scenario "test" {
                given { x = 1 }
                when { y = x + 1 }
                then { y == 2 }
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_scenario_with_alternative() {
        let analyzer = analyze_source(
            r#"
            scenario "test with alternatives" {
                given { x = 1 }
                when { y = x + 1 }
                then { y > 0 }

                alt "negative input" when { x < 0 } {
                    then { x == 0 }
                }

                alt "with extra given" {
                    given { z = 5 }
                    then { z > x }
                }
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_alternative_inherits_base_given() {
        // x from base given should be accessible in alternative
        let analyzer = analyze_source(
            r#"
            scenario "test" {
                given { x = 1 }
                when { y = x + 1 }
                then { y > 0 }

                alt "uses x" when { x > 0 } {
                    then { x == 1 }
                }
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_generic_types() {
        let analyzer = analyze_source(
            r#"
            type User { id: Int }
            state Test {
                users: Set<User>,
                names: List<String>,
                lookup: Map<Int, String>
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_attributes_valid() {
        let analyzer = analyze_source(
            r#"
            @id(TYPE001)
            @rationale("Test type")
            type User { id: Int }

            @id(STATE001)
            state AppState {
                users: Set<User>
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_duplicate_id() {
        let analyzer = analyze_source(
            r#"
            @id(DUPLICATE)
            type User { id: Int }

            @id(DUPLICATE)
            type Admin { id: Int }
            "#,
        );
        assert!(!analyzer.succeeded());
        // Should have the duplicate ID error plus potentially the duplicate type error
        assert!(analyzer.diagnostics.errors().iter().any(|e| {
            matches!(e, SemanticError::DuplicateId { id, .. } if id == "DUPLICATE")
        }));
    }

    #[test]
    fn test_analyze_unique_ids() {
        let analyzer = analyze_source(
            r#"
            @id(TYPE001)
            type User { id: Int }

            @id(TYPE002)
            type Admin { id: Int }

            @id(ACTION001)
            action create() -> User
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_self_outside_refinement() {
        // `self` used outside of a refinement context should error
        let analyzer = analyze_source(
            r#"
            state Test {
                x: Int,
                invariant "bad" { self > 0 }
            }
            "#,
        );
        assert!(!analyzer.succeeded());
        assert!(analyzer.diagnostics.errors().iter().any(|e| {
            matches!(e, SemanticError::InvalidOperation { message, .. } if message.contains("self"))
        }));
    }

    #[test]
    fn test_analyze_type_alias_simple() {
        // Type alias without refinement - should parse but we don't validate it yet
        let analyzer = analyze_source("type PositiveInt = Int");
        // For now, we just test that it doesn't crash
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_quality() {
        let analyzer = analyze_source(
            r#"
            quality performance "API response time" {
                metric: latency,
                target: < 200ms,
            }
            "#,
        );
        assert!(analyzer.succeeded());
    }

    #[test]
    fn test_analyze_quality_with_duplicate_id() {
        let analyzer = analyze_source(
            r#"
            @id("NFR-001")
            quality performance "API response time" {
                metric: latency,
                target: < 200ms,
            }

            @id("NFR-001")
            quality reliability "Uptime" {
                metric: uptime,
                target: >= 99%,
            }
            "#,
        );
        assert!(!analyzer.succeeded());
        assert!(analyzer.diagnostics.errors().iter().any(|e| {
            matches!(e, SemanticError::DuplicateId { id, .. } if id == "NFR-001")
        }));
    }
}
