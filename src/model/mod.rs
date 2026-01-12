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
    CompiledAction, CompiledAssignment, CompiledAssertion, CompiledAttribute, CompiledAttributeArg,
    CompiledContract, CompiledEnum, CompiledGiven, CompiledInvariant, CompiledProperty,
    CompiledRelation, CompiledScenario, CompiledSpec, CompiledState, CompiledStruct, CompiledThen,
    CompiledVariant, CompiledWhen, CompiledWhenAction, Import, RelationProperty, TemporalOp,
};
pub use state::{Environment, StateSnapshot, Trace, Value};

use crate::ast::{self, Specification};
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
        self.spec.module = ast.module.as_ref().map(|m| m.name.name.clone());

        // Build imports
        self.build_imports(&ast.imports);

        // Build types
        self.build_types(&ast.types, &self.analyzer.types);
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
                let compiled = CompiledStruct::from_info(info, attrs, type_def.span);
                self.spec.structs.insert(name.clone(), compiled);
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
            let compiled = CompiledScenario {
                name: scenario.description.to_string(),
                given: vec![Self::build_given(&scenario.given)],
                when: vec![Self::build_when(&scenario.when)],
                then: vec![Self::build_then(&scenario.then)],
                attributes: Self::compile_attributes(&scenario.attributes),
                doc: None,
                span: scenario.span,
            };

            self.spec.scenarios.push(compiled);
        }
    }

    fn build_given(given: &ast::GivenClause) -> CompiledGiven {
        let assignments = given
            .bindings
            .iter()
            .map(|b| CompiledAssignment {
                name: b.name.name.clone(),
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
                binding: Some(b.name.name.clone()),
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
}
