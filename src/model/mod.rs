//! Compiled specification model
//!
//! This module provides the compiled representation of a Fastbreak specification
//! after semantic analysis. It includes:
//!
//! - `CompiledSpec`: The fully resolved specification ready for code generation
//! - `Checker`: Property and invariant checking infrastructure
//! - Runtime state representation for evaluation

mod checker;
mod spec;
mod state;

pub use checker::{CheckResult, Checker, Counterexample};
pub use spec::{
    CompiledAction, CompiledAlternative, CompiledAssignment, CompiledAssertion, CompiledAttribute,
    CompiledAttributeArg, CompiledContract, CompiledEnum, CompiledGiven, CompiledInvariant,
    CompiledProperty, CompiledQuality, CompiledQualityProperty, CompiledRelation, CompiledScenario,
    CompiledSpec, CompiledState, CompiledStruct, CompiledThen, CompiledTypeAlias, CompiledVariant,
    CompiledWhen, CompiledWhenAction, Import, QualityCategory, RelationProperty, TemporalOp,
};
pub use state::{Environment, StateSnapshot, Trace, Value};

use crate::ast::{self, Module, Specification};
use crate::semantic::{Analyzer, TypeRegistry};
use smol_str::SmolStr;
use std::sync::Arc;

/// Build a compiled specification from an analyzed AST
///
/// This transforms the AST and semantic information into a fully
/// resolved model ready for code generation and verification.
#[must_use]
pub fn compile(spec: &Specification, analyzer: &Analyzer) -> CompiledSpec {
    let builder = SpecBuilder::new(analyzer);
    builder.build(spec)
}

/// Builder for constructing a compiled specification
struct SpecBuilder<'a> {
    /// Reference to the semantic analyzer results
    analyzer: &'a Analyzer,
    /// The specification being built
    spec: CompiledSpec,
}

impl<'a> SpecBuilder<'a> {
    /// Create a new spec builder
    fn new(analyzer: &'a Analyzer) -> Self {
        Self {
            analyzer,
            spec: CompiledSpec::new(),
        }
    }

    /// Convert AST attributes to compiled attributes
    fn compile_attributes(attrs: &[ast::Attribute]) -> Vec<spec::CompiledAttribute> {
        attrs.iter().map(spec::CompiledAttribute::from_ast).collect()
    }

    /// Build the compiled specification
    fn build(mut self, ast: &Specification) -> CompiledSpec {
        // Set module name
        self.spec.module = ast.module.as_ref().map(Module::name);

        // Build imports
        self.build_imports(&ast.imports);

        // Build types
        self.build_types(&ast.types, &self.analyzer.types);
        self.build_type_aliases(&ast.type_aliases);
        self.build_enums(&ast.enums, &self.analyzer.types);

        // Build states
        self.build_states(&ast.states);

        // Build actions
        self.build_actions(&ast.actions);

        // Build relations
        self.build_relations(&ast.relations);

        // Build scenarios
        self.build_scenarios(&ast.scenarios);

        // Build properties
        self.build_properties(&ast.properties);

        // Build qualities (NFRs)
        self.build_qualities(&ast.qualities);

        self.spec
    }

    fn build_imports(&mut self, imports: &[ast::Import]) {
        for import in imports {
            let path: Vec<SmolStr> = import.path.segments.iter().map(|s| s.name.clone()).collect();

            let items: Vec<SmolStr> = import
                .items
                .as_ref()
                .map(|items| items.iter().map(|i| i.name.name.clone()).collect())
                .unwrap_or_default();

            self.spec.imports.push(Import {
                path,
                items,
                span: import.span,
            });
        }
    }

    fn build_types(&mut self, types: &[ast::TypeDef], registry: &TypeRegistry) {
        for type_def in types {
            let name = &type_def.name.name;
            if let Some(info) = registry.get_struct(name) {
                let attrs = Self::compile_attributes(&type_def.attributes);
                let refinement = type_def.refinement.as_ref().map(|e| Arc::new(e.clone()));
                let compiled = CompiledStruct::from_info(info, refinement, attrs, type_def.span);
                self.spec.structs.insert(name.clone(), compiled);
            }
        }
    }

    fn build_type_aliases(&mut self, aliases: &[ast::TypeAlias]) {
        for alias in aliases {
            let attrs = Self::compile_attributes(&alias.attributes);
            let refinement = alias.refinement.as_ref().map(|e| Arc::new(e.clone()));

            // Resolve the base type
            let base = self.resolve_type_ref(&alias.base);

            let compiled = spec::CompiledTypeAlias {
                name: alias.name.name.clone(),
                type_params: alias.type_params.iter().map(|i| i.name.clone()).collect(),
                base,
                refinement,
                attributes: attrs,
                doc: None,
                span: alias.span,
            };
            self.spec.type_aliases.insert(alias.name.name.clone(), compiled);
        }
    }

    fn resolve_type_ref(&self, type_ref: &ast::TypeRef) -> crate::semantic::Type {
        use crate::semantic::{Type, TypeId};

        match &type_ref.kind {
            ast::TypeRefKind::Named(path) => {
                if let Some(ident) = path.name() {
                    let name = &ident.name;
                    // Check for primitive types first
                    match name.as_str() {
                        "Int" => Type::Int,
                        "String" => Type::String,
                        "Bool" => Type::Bool,
                        _ => {
                            // Try to look up as struct
                            if let Some(info) = self.analyzer.types.get_struct(name) {
                                Type::Struct(info.id.clone())
                            // Try to look up as enum
                            } else if let Some(info) = self.analyzer.types.get_enum(name) {
                                Type::Enum(info.id.clone())
                            // Type alias or not yet resolved - create placeholder TypeId
                            } else {
                                // For type aliases or unresolved types, create a struct TypeId
                                // The semantic analysis has already validated the type
                                Type::Struct(TypeId::new(name.clone(), 0))
                            }
                        }
                    }
                } else {
                    Type::Unknown
                }
            }
            ast::TypeRefKind::BuiltIn(builtin) => match builtin {
                ast::BuiltInType::Int => Type::Int,
                ast::BuiltInType::String => Type::String,
                ast::BuiltInType::Bool => Type::Bool,
                ast::BuiltInType::Set => Type::Set(Arc::new(Type::Unknown)),
                ast::BuiltInType::Map => Type::Map(Arc::new(Type::Unknown), Arc::new(Type::Unknown)),
                ast::BuiltInType::List => Type::List(Arc::new(Type::Unknown)),
                ast::BuiltInType::Option => Type::Option(Arc::new(Type::Unknown)),
                ast::BuiltInType::Result => {
                    Type::Result(Arc::new(Type::Unknown), Arc::new(Type::Unknown))
                }
            },
            ast::TypeRefKind::Generic { base, args } => {
                let base_type = self.resolve_type_ref(base);
                let resolved_args: Vec<Type> =
                    args.iter().map(|arg| self.resolve_type_ref(&arg.ty)).collect();

                match &base_type {
                    Type::Set(_) if !resolved_args.is_empty() => {
                        Type::Set(Arc::new(resolved_args[0].clone()))
                    }
                    Type::List(_) if !resolved_args.is_empty() => {
                        Type::List(Arc::new(resolved_args[0].clone()))
                    }
                    Type::Option(_) if !resolved_args.is_empty() => {
                        Type::Option(Arc::new(resolved_args[0].clone()))
                    }
                    Type::Map(_, _) if resolved_args.len() >= 2 => Type::Map(
                        Arc::new(resolved_args[0].clone()),
                        Arc::new(resolved_args[1].clone()),
                    ),
                    Type::Result(_, _) if resolved_args.len() >= 2 => Type::Result(
                        Arc::new(resolved_args[0].clone()),
                        Arc::new(resolved_args[1].clone()),
                    ),
                    _ => base_type,
                }
            }
            ast::TypeRefKind::Tuple(types) => {
                let resolved: Vec<Type> = types.iter().map(|t| self.resolve_type_ref(t)).collect();
                Type::Tuple(resolved)
            }
            ast::TypeRefKind::Unit => Type::Unit,
            ast::TypeRefKind::Function { params, ret } => {
                let resolved_params: Vec<Type> =
                    params.iter().map(|p| self.resolve_type_ref(p)).collect();
                let resolved_ret = self.resolve_type_ref(ret);
                Type::Function {
                    params: resolved_params,
                    ret: Arc::new(resolved_ret),
                }
            }
        }
    }

    fn build_enums(&mut self, enums: &[ast::EnumDef], registry: &TypeRegistry) {
        for enum_def in enums {
            let name = &enum_def.name.name;
            if let Some(info) = registry.get_enum(name) {
                let attrs = Self::compile_attributes(&enum_def.attributes);
                let compiled = CompiledEnum::from_info(info, attrs, enum_def.span);
                self.spec.enums.insert(name.clone(), compiled);
            }
        }
    }

    fn build_states(&mut self, states: &[ast::StateBlock]) {
        for state in states {
            let name = &state.name.name;

            if let Some(info) = self.analyzer.types.get_state(name) {
                let attrs = Self::compile_attributes(&state.attributes);
                let mut compiled = CompiledState::from_info(info, attrs, state.span);

                // Build invariants
                for invariant in &state.invariants {
                    compiled.invariants.push(CompiledInvariant {
                        name: invariant.description.as_ref().map(ToString::to_string),
                        expr: Arc::new(invariant.expr.clone()),
                        span: invariant.span,
                    });
                }

                self.spec.states.insert(name.clone(), compiled);
            }
        }
    }

    fn build_actions(&mut self, actions: &[ast::Action]) {
        for action in actions {
            let name = &action.name.name;

            if let Some(info) = self.analyzer.types.get_action(name) {
                let attrs = Self::compile_attributes(&action.attributes);
                let mut compiled = CompiledAction::from_info(info, attrs, action.span);

                // Build contracts from action.contracts
                for contract in &action.contracts {
                    let compiled_contract = CompiledContract {
                        expr: Arc::new(contract.expr.clone()),
                        span: contract.span,
                    };
                    match contract.kind {
                        ast::ContractKind::Requires => compiled.requires.push(compiled_contract),
                        ast::ContractKind::Ensures => compiled.ensures.push(compiled_contract),
                    }
                }

                self.spec.actions.insert(name.clone(), compiled);
            }
        }
    }

    fn build_relations(&mut self, relations: &[ast::Relation]) {
        for relation in relations {
            let name = &relation.name.name;

            if let Some(info) = self.analyzer.types.get_relation(name) {
                let attrs = Self::compile_attributes(&relation.attributes);
                let mut compiled = CompiledRelation::from_info(info, attrs, relation.span);

                // Build relation properties from constraints
                for constraint in &relation.constraints {
                    let property = match constraint {
                        ast::RelationConstraint::Symmetric => RelationProperty::Symmetric,
                        ast::RelationConstraint::Reflexive => RelationProperty::Reflexive,
                        ast::RelationConstraint::Irreflexive => RelationProperty::Irreflexive,
                        ast::RelationConstraint::Transitive => RelationProperty::Transitive,
                        ast::RelationConstraint::Antisymmetric => RelationProperty::Antisymmetric,
                    };
                    compiled.properties.push(property);
                }

                self.spec.relations.insert(name.clone(), compiled);
            }
        }
    }

    fn build_scenarios(&mut self, scenarios: &[ast::Scenario]) {
        for scenario in scenarios {
            let alternatives = scenario
                .alternatives
                .iter()
                .map(Self::build_alternative)
                .collect();

            let compiled = CompiledScenario {
                name: scenario.description.to_string(),
                given: vec![Self::build_given(&scenario.given)],
                when: vec![Self::build_when(&scenario.when)],
                then: vec![Self::build_then(&scenario.then)],
                alternatives,
                attributes: Self::compile_attributes(&scenario.attributes),
                doc: None,
                span: scenario.span,
            };

            self.spec.scenarios.push(compiled);
        }
    }

    fn build_alternative(alt: &ast::Alternative) -> CompiledAlternative {
        CompiledAlternative {
            name: alt.name.to_string(),
            condition: alt.condition.as_ref().map(|e| Arc::new(e.clone())),
            given: alt.given.as_ref().map(Self::build_given),
            when: alt.when.as_ref().map(Self::build_when),
            then: Self::build_then(&alt.then),
            attributes: Self::compile_attributes(&alt.attributes),
            span: alt.span,
        }
    }

    fn build_given(given: &ast::GivenClause) -> CompiledGiven {
        let assignments = given
            .bindings
            .iter()
            .map(|b| CompiledAssignment {
                name: b.name.to_dotted_string(),
                value: Arc::new(b.value.clone()),
                span: b.span,
            })
            .collect();

        CompiledGiven {
            assignments,
            span: given.span,
        }
    }

    fn build_when(when: &ast::WhenClause) -> CompiledWhen {
        let actions = when
            .bindings
            .iter()
            .map(|b| CompiledWhenAction {
                binding: Some(b.name.to_dotted_string()),
                expr: Arc::new(b.value.clone()),
                span: b.span,
            })
            .collect();

        CompiledWhen {
            actions,
            span: when.span,
        }
    }

    fn build_then(then: &ast::ThenClause) -> CompiledThen {
        let assertions = then
            .assertions
            .iter()
            .map(|a| CompiledAssertion {
                expr: Arc::new(a.expr.clone()),
                span: a.span,
            })
            .collect();

        CompiledThen {
            assertions,
            span: then.span,
        }
    }

    fn build_properties(&mut self, properties: &[ast::Property]) {
        for property in properties {
            let temporal = match &property.temporal_op {
                Some(ast::TemporalOp::Always) => Some(TemporalOp::Always),
                Some(ast::TemporalOp::Eventually) => Some(TemporalOp::Eventually),
                None => None,
            };

            let compiled = CompiledProperty {
                name: property.description.to_string(),
                expr: Arc::new(property.expr.clone()),
                temporal,
                attributes: Self::compile_attributes(&property.attributes),
                doc: None,
                span: property.span,
            };

            self.spec.properties.push(compiled);
        }
    }

    fn build_qualities(&mut self, qualities: &[ast::Quality]) {
        for quality in qualities {
            let category = Self::convert_quality_category(quality.category);
            let target = Self::format_quality_target(&quality.target);

            // Format scale
            let scale = quality.scale.map(|s| s.to_string());

            // Format constraint
            let constraint = quality.constraint.map(|c| c.to_string());

            // Format applies_to
            let applies_to = quality
                .applies_to
                .as_ref()
                .map(|a| format!("{} {}", a.kind, a.name.name));

            // Format measurement
            let measurement = quality.measurement.map(|m| m.to_string());

            // Format under_load
            let under_load = quality.under_load.as_ref().map(|load| {
                let mut parts = Vec::new();
                if let Some(users) = load.concurrent_users {
                    parts.push(format!("concurrent_users: {users}"));
                }
                if let Some(conns) = load.concurrent_connections {
                    parts.push(format!("concurrent_connections: {conns}"));
                }
                if let Some(rps) = load.requests_per_second {
                    parts.push(format!("requests_per_second: {rps}"));
                }
                if let Some((size, unit)) = &load.payload_size {
                    parts.push(format!("payload_size: {size}{unit}"));
                }
                if let Some((dur, unit)) = &load.duration {
                    parts.push(format!("duration: {dur}{unit}"));
                }
                parts.join(", ")
            });

            // Format verified_by
            let verified_by = quality
                .verified_by
                .iter()
                .map(|v| format!("{} \"{}\"", v.kind, v.name))
                .collect();

            // Format additional properties
            let properties = quality
                .properties
                .iter()
                .map(|p| spec::CompiledQualityProperty {
                    name: p.name.name.clone(),
                    value: Self::format_quality_property_value(&p.value),
                })
                .collect();

            let compiled = spec::CompiledQuality {
                category,
                description: quality.description.clone(),
                metric: quality.metric.name.clone(),
                target,
                scale,
                constraint,
                applies_to,
                measurement,
                under_load,
                verified_by,
                properties,
                attributes: Self::compile_attributes(&quality.attributes),
                span: quality.span,
            };

            self.spec.qualities.push(compiled);
        }
    }

    fn convert_quality_category(cat: ast::QualityCategory) -> spec::QualityCategory {
        match cat {
            ast::QualityCategory::Performance => spec::QualityCategory::Performance,
            ast::QualityCategory::Reliability => spec::QualityCategory::Reliability,
            ast::QualityCategory::Security => spec::QualityCategory::Security,
            ast::QualityCategory::Usability => spec::QualityCategory::Usability,
            ast::QualityCategory::Scalability => spec::QualityCategory::Scalability,
            ast::QualityCategory::Maintainability => spec::QualityCategory::Maintainability,
        }
    }

    fn format_quality_target(target: &ast::QualityTarget) -> String {
        let op = match target.op {
            ast::QualityOp::Lt => "<",
            ast::QualityOp::LtEq => "<=",
            ast::QualityOp::Gt => ">",
            ast::QualityOp::GtEq => ">=",
            ast::QualityOp::Eq => "==",
        };
        let value = Self::format_quality_value(&target.value);
        format!("{op} {value}")
    }

    fn format_quality_value(value: &ast::QualityValue) -> String {
        match value {
            ast::QualityValue::Int(n) => n.to_string(),
            ast::QualityValue::Percentage(p) => format!("{p}%"),
            ast::QualityValue::Duration(n, unit) => format!("{n}{unit}"),
            ast::QualityValue::Size(n, unit) => format!("{n}{unit}"),
            ast::QualityValue::Rate(n, unit) => format!("{n}{unit}"),
            ast::QualityValue::Expr(_) => "<expr>".to_string(),
        }
    }

    fn format_quality_property_value(value: &ast::QualityPropertyValue) -> String {
        match value {
            ast::QualityPropertyValue::Ident(ident) => ident.name.to_string(),
            ast::QualityPropertyValue::Target(target) => Self::format_quality_target(target),
            ast::QualityPropertyValue::AppliesTo(applies_to) => {
                format!("{} {}", applies_to.kind, applies_to.name.name)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::semantic;

    #[test]
    fn test_compile_empty() {
        let spec = parse("").unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert!(compiled.is_empty());
    }

    #[test]
    fn test_compile_type_def() {
        let source = r#"
            type User {
                id: Int,
                name: String,
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.structs.len(), 1);
        let user = compiled.structs.get("User").unwrap();
        assert_eq!(user.name.as_str(), "User");
        assert_eq!(user.fields.len(), 2);
    }

    #[test]
    fn test_compile_enum_def() {
        let source = r#"
            enum Status {
                Active,
                Inactive,
                Pending(String),
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.enums.len(), 1);
        let status = compiled.enums.get("Status").unwrap();
        assert_eq!(status.variants.len(), 3);
        assert!(status.variants.get("Active").unwrap().is_unit());
        assert!(!status.variants.get("Pending").unwrap().is_unit());
    }

    #[test]
    fn test_compile_state() {
        let source = r#"
            state Counter {
                count: Int,

                invariant "count is non-negative" {
                    count >= 0
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.states.len(), 1);
        let counter = compiled.states.get("Counter").unwrap();
        assert_eq!(counter.fields.len(), 1);
        assert_eq!(counter.invariants.len(), 1);
        assert_eq!(
            counter.invariants[0].name.as_deref(),
            Some("count is non-negative")
        );
    }

    #[test]
    fn test_compile_action() {
        let source = r#"
            action increment(amount: Int) -> Int
                requires { amount > 0 }
                ensures { result == old(count) + amount }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.actions.len(), 1);
        let action = compiled.actions.get("increment").unwrap();
        assert_eq!(action.params.len(), 1);
        assert_eq!(action.requires.len(), 1);
        assert_eq!(action.ensures.len(), 1);
    }

    #[test]
    fn test_compile_scenario() {
        let source = r#"
            scenario "simple test" {
                given {
                    x = 1
                }
                when {
                    y = x + 1
                }
                then {
                    y == 2
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.scenarios.len(), 1);
        let scenario = &compiled.scenarios[0];
        assert_eq!(scenario.name, "simple test");
        assert_eq!(scenario.given.len(), 1);
        assert_eq!(scenario.when.len(), 1);
        assert_eq!(scenario.then.len(), 1);
    }

    #[test]
    fn test_compile_property() {
        let source = r#"
            property "always positive" {
                always {
                    count > 0
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.properties.len(), 1);
        let property = &compiled.properties[0];
        assert_eq!(property.name, "always positive");
        assert_eq!(property.temporal, Some(TemporalOp::Always));
    }

    #[test]
    fn test_checker_invariant_pass() {
        let source = r#"
            state Counter {
                count: Int,

                invariant "count is non-negative" {
                    count >= 0
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let checker = Checker::new(&compiled);
        let mut state = StateSnapshot::for_state("Counter");
        state.set("count", Value::Int(5));

        let results = checker.check_invariants(&state);
        assert_eq!(results.len(), 1);
        assert!(results[0].passed);
    }

    #[test]
    fn test_checker_invariant_fail() {
        let source = r#"
            state Counter {
                count: Int,

                invariant "count is non-negative" {
                    count >= 0
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let checker = Checker::new(&compiled);
        let mut state = StateSnapshot::for_state("Counter");
        state.set("count", Value::Int(-1));

        let results = checker.check_invariants(&state);
        assert_eq!(results.len(), 1);
        assert!(!results[0].passed);
    }

    #[test]
    fn test_checker_always_property() {
        let source = r#"
            property "always positive" {
                always {
                    x > 0
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let checker = Checker::new(&compiled);

        // Create a trace with all positive values
        let mut trace = Trace::new();
        let mut state1 = StateSnapshot::new();
        state1.set("x", Value::Int(1));
        trace.add_initial(state1);

        let mut state2 = StateSnapshot::new();
        state2.set("x", Value::Int(5));
        trace.add_transition("action1", state2);

        let results = checker.check_properties(&trace);
        assert_eq!(results.len(), 1);
        assert!(results[0].passed);
    }

    #[test]
    fn test_compile_type_alias() {
        let source = r#"
            type PositiveInt = Int where self > 0
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.type_aliases.len(), 1);
        let alias = compiled.type_aliases.get("PositiveInt").unwrap();
        assert_eq!(alias.name.as_str(), "PositiveInt");
        assert!(alias.refinement.is_some());
    }

    #[test]
    fn test_compile_type_alias_no_refinement() {
        let source = r#"
            type UserId = Int
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.type_aliases.len(), 1);
        let alias = compiled.type_aliases.get("UserId").unwrap();
        assert_eq!(alias.name.as_str(), "UserId");
        assert!(alias.refinement.is_none());
    }

    #[test]
    fn test_compile_struct_with_refinement() {
        let source = r#"
            type Point {
                x: Int,
                y: Int,
            } where self.x >= 0 and self.y >= 0
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        assert_eq!(compiled.structs.len(), 1);
        let point = compiled.structs.get("Point").unwrap();
        assert_eq!(point.fields.len(), 2);
        assert!(point.refinement.is_some());
    }
}
