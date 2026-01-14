//! Mermaid diagram generator
//!
//! Generates Mermaid diagrams from a compiled specification:
//! - Entity Relationship Diagrams (ERDs)
//! - State diagrams
//! - Sequence diagrams from scenarios

use crate::model::{CompiledSpec, RelationProperty};
use crate::semantic::Type;
use std::fmt::Write;

/// Type of Mermaid diagram to generate
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagramType {
    /// Entity Relationship Diagram
    Erd,
    /// State diagram
    State,
    /// Sequence diagram from scenarios
    Sequence,
}

/// Mermaid diagram generator
pub struct MermaidGenerator<'a> {
    spec: &'a CompiledSpec,
}

impl<'a> MermaidGenerator<'a> {
    /// Create a new Mermaid generator for a specification
    #[must_use]
    pub const fn new(spec: &'a CompiledSpec) -> Self {
        Self { spec }
    }

    /// Generate a diagram of the specified type
    #[must_use]
    pub fn generate(&self, diagram_type: DiagramType) -> String {
        match diagram_type {
            DiagramType::Erd => self.generate_erd(),
            DiagramType::State => self.generate_state_diagram(),
            DiagramType::Sequence => self.generate_sequence_diagram(),
        }
    }

    /// Generate all diagram types
    #[must_use]
    pub fn generate_all(&self) -> AllDiagrams {
        AllDiagrams {
            erd: self.generate_erd(),
            state: self.generate_state_diagram(),
            sequence: self.generate_sequence_diagram(),
        }
    }

    /// Generate an Entity Relationship Diagram
    fn generate_erd(&self) -> String {
        let mut output = String::new();
        writeln!(output, "erDiagram").unwrap();

        // Generate entities from structs
        for (name, struct_def) in &self.spec.structs {
            writeln!(output, "    {name} {{").unwrap();
            for (field_name, field_type) in &struct_def.fields {
                let type_str = Self::format_type_for_erd(field_type);
                writeln!(output, "        {type_str} {field_name}").unwrap();
            }
            writeln!(output, "    }}").unwrap();
        }

        // Generate entities from states
        for (name, state_def) in &self.spec.states {
            writeln!(output, "    {name} {{").unwrap();
            for (field_name, field_type) in &state_def.fields {
                let type_str = Self::format_type_for_erd(field_type);
                writeln!(output, "        {type_str} {field_name}").unwrap();
            }
            writeln!(output, "    }}").unwrap();
        }

        // Generate relationships from relations
        for (name, relation_def) in &self.spec.relations {
            let source = Self::extract_type_name(&relation_def.source);
            let target = Self::extract_type_name(&relation_def.target);
            let cardinality = Self::relation_cardinality(&relation_def.properties);
            writeln!(output, "    {source} {cardinality} {target} : \"{name}\"").unwrap();
        }

        // Generate relationships from struct fields that reference other types
        for (struct_name, struct_def) in &self.spec.structs {
            for (field_name, field_type) in &struct_def.fields {
                if let Some((target, cardinality)) = Self::extract_relationship(field_type, self.spec) {
                    writeln!(
                        output,
                        "    {struct_name} {cardinality} {target} : \"{field_name}\""
                    )
                    .unwrap();
                }
            }
        }

        output
    }

    /// Generate a State diagram
    fn generate_state_diagram(&self) -> String {
        let mut output = String::new();
        writeln!(output, "stateDiagram-v2").unwrap();

        if self.spec.states.is_empty() && self.spec.actions.is_empty() {
            writeln!(output, "    [*] --> Empty").unwrap();
            writeln!(output, "    Empty --> [*]").unwrap();
            return output;
        }

        // For each state, show it as a composite state with its invariants
        for (name, state_def) in &self.spec.states {
            if state_def.invariants.is_empty() {
                writeln!(output, "    state {name}").unwrap();
            } else {
                writeln!(output, "    state {name} {{").unwrap();
                for invariant in &state_def.invariants {
                    let inv_name = invariant
                        .name
                        .as_deref()
                        .unwrap_or("invariant")
                        .replace('"', "'");
                    writeln!(output, "        {name}: {inv_name}").unwrap();
                }
                writeln!(output, "    }}").unwrap();
            }
        }

        // Show actions as transitions
        // If we have states, connect actions to/from them
        // Otherwise, show actions as self-transitions on a generic state
        if self.spec.states.is_empty() {
            writeln!(output, "    state System").unwrap();
            for action_name in self.spec.actions.keys() {
                writeln!(output, "    System --> System : {action_name}()").unwrap();
            }
        } else {
            // Connect actions to the first state (simplified)
            // In a real implementation, we'd analyze which actions affect which states
            if let Some(state_name) = self.spec.states.keys().next() {
                writeln!(output, "    [*] --> {state_name}").unwrap();
                for action_name in self.spec.actions.keys() {
                    writeln!(output, "    {state_name} --> {state_name} : {action_name}()").unwrap();
                }
            }
        }

        output
    }

    /// Generate a Sequence diagram from scenarios
    fn generate_sequence_diagram(&self) -> String {
        let mut output = String::new();
        writeln!(output, "sequenceDiagram").unwrap();

        if self.spec.scenarios.is_empty() {
            writeln!(output, "    participant System").unwrap();
            writeln!(output, "    Note over System: No scenarios defined").unwrap();
            return output;
        }

        // Define participants
        writeln!(output, "    participant Test").unwrap();
        writeln!(output, "    participant System").unwrap();

        // Generate sequence for each scenario
        for scenario in &self.spec.scenarios {
            let scenario_name = scenario.name.replace('"', "'");
            writeln!(output, "    Note over Test,System: {scenario_name}").unwrap();

            // Given phase
            for given in &scenario.given {
                for assignment in &given.assignments {
                    writeln!(output, "    Test->>System: Set {} = ...", assignment.name).unwrap();
                }
            }

            // When phase
            for when in &scenario.when {
                for action in &when.actions {
                    if let Some(binding) = &action.binding {
                        writeln!(output, "    Test->>System: Execute action").unwrap();
                        writeln!(output, "    System-->>Test: {binding} = result").unwrap();
                    } else {
                        writeln!(output, "    Test->>System: Execute action").unwrap();
                    }
                }
            }

            // Then phase
            for then in &scenario.then {
                let count = then.assertions.len();
                writeln!(output, "    Test->>Test: Verify {count} assertion(s)").unwrap();
            }
        }

        output
    }

    /// Format a type for ERD display
    fn format_type_for_erd(ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::String => "string".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Struct(id) | Type::Enum(id) => id.name.to_string(),
            Type::Set(_) => "set".to_string(),
            Type::List(_) => "list".to_string(),
            Type::Map(_, _) => "map".to_string(),
            Type::Option(inner) => format!("option_{}", Self::format_type_for_erd(inner)),
            Type::Result(_, _) => "result".to_string(),
            Type::Tuple(_) => "tuple".to_string(),
            Type::Function { .. } => "function".to_string(),
            Type::Var(id) => format!("T{}", id.0),
            Type::Unknown => "unknown".to_string(),
            Type::Error => "error".to_string(),
        }
    }

    /// Extract the base type name from a type
    fn extract_type_name(ty: &Type) -> String {
        match ty {
            Type::Struct(id) | Type::Enum(id) => id.name.to_string(),
            Type::Set(inner) | Type::List(inner) | Type::Option(inner) => {
                Self::extract_type_name(inner)
            }
            _ => "Entity".to_string(),
        }
    }

    /// Determine ERD cardinality from relation properties
    fn relation_cardinality(properties: &[RelationProperty]) -> &'static str {
        let is_symmetric = properties.contains(&RelationProperty::Symmetric);

        if is_symmetric {
            "}o--o{" // many-to-many
        } else {
            "||--o{" // one-to-many
        }
    }

    /// Extract relationship info from a field type
    fn extract_relationship(ty: &Type, spec: &CompiledSpec) -> Option<(String, &'static str)> {
        match ty {
            Type::Struct(id) | Type::Enum(id) => {
                // Check if this is a known type
                let name = &id.name;
                if spec.structs.contains_key(name) || spec.states.contains_key(name) {
                    Some((name.to_string(), "||--||")) // one-to-one
                } else {
                    None
                }
            }
            Type::Set(inner) | Type::List(inner) => {
                if let Type::Struct(id) | Type::Enum(id) = inner.as_ref() {
                    let name = &id.name;
                    if spec.structs.contains_key(name) || spec.states.contains_key(name) {
                        Some((name.to_string(), "||--o{")) // one-to-many
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Type::Option(inner) => {
                if let Type::Struct(id) | Type::Enum(id) = inner.as_ref() {
                    let name = &id.name;
                    if spec.structs.contains_key(name) || spec.states.contains_key(name) {
                        Some((name.to_string(), "|o--||")) // zero-or-one-to-one
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Type::Map(_, value) => {
                if let Type::Struct(id) | Type::Enum(id) = value.as_ref() {
                    let name = &id.name;
                    if spec.structs.contains_key(name) || spec.states.contains_key(name) {
                        Some((name.to_string(), "||--o{")) // one-to-many
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// All diagram types generated at once
#[derive(Debug, Clone)]
pub struct AllDiagrams {
    /// Entity Relationship Diagram
    pub erd: String,
    /// State diagram
    pub state: String,
    /// Sequence diagram
    pub sequence: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::compile;
    use crate::parser::parse;
    use crate::semantic;

    #[test]
    fn test_generate_empty_erd() {
        let spec = parse("").unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::Erd);

        assert!(output.contains("erDiagram"));
    }

    #[test]
    fn test_generate_erd_with_type() {
        let source = r#"
            type User {
                id: Int,
                name: String,
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::Erd);

        assert!(output.contains("erDiagram"));
        assert!(output.contains("User {"));
        assert!(output.contains("int id"));
        assert!(output.contains("string name"));
    }

    #[test]
    fn test_generate_erd_with_state() {
        let source = r#"
            state Counter {
                count: Int,
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::Erd);

        assert!(output.contains("erDiagram"));
        assert!(output.contains("Counter {"));
        assert!(output.contains("int count"));
    }

    #[test]
    fn test_generate_empty_state_diagram() {
        let spec = parse("").unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::State);

        assert!(output.contains("stateDiagram-v2"));
        assert!(output.contains("[*] --> Empty"));
    }

    #[test]
    fn test_generate_state_diagram_with_action() {
        let source = r#"
            action increment(amount: Int) -> Int
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::State);

        assert!(output.contains("stateDiagram-v2"));
        assert!(output.contains("System --> System : increment()"));
    }

    #[test]
    fn test_generate_state_diagram_with_state_and_action() {
        let source = r#"
            state Counter {
                count: Int,
            }

            action increment(amount: Int) -> Int
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::State);

        assert!(output.contains("stateDiagram-v2"));
        assert!(output.contains("state Counter"));
        assert!(output.contains("[*] --> Counter"));
        assert!(output.contains("Counter --> Counter : increment()"));
    }

    #[test]
    fn test_generate_state_diagram_with_invariant() {
        let source = r#"
            state Counter {
                count: Int,

                invariant "count is positive" {
                    count > 0
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::State);

        assert!(output.contains("stateDiagram-v2"));
        assert!(output.contains("state Counter {"));
        assert!(output.contains("Counter: count is positive"));
    }

    #[test]
    fn test_generate_empty_sequence() {
        let spec = parse("").unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::Sequence);

        assert!(output.contains("sequenceDiagram"));
        assert!(output.contains("participant System"));
        assert!(output.contains("No scenarios defined"));
    }

    #[test]
    fn test_generate_sequence_with_scenario() {
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

        let generator = MermaidGenerator::new(&compiled);
        let output = generator.generate(DiagramType::Sequence);

        assert!(output.contains("sequenceDiagram"));
        assert!(output.contains("participant Test"));
        assert!(output.contains("participant System"));
        assert!(output.contains("Note over Test,System: simple test"));
        assert!(output.contains("Test->>System: Set x = ..."));
        assert!(output.contains("System-->>Test: y = result"));
        assert!(output.contains("Verify 1 assertion(s)"));
    }

    #[test]
    fn test_generate_all() {
        let source = r#"
            type User {
                id: Int,
            }

            state System {
                users: Set<User>,
            }

            action addUser(id: Int) -> Bool

            scenario "add user" {
                given {
                    users = {}
                }
                when {
                    result = addUser(1)
                }
                then {
                    result == true
                }
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MermaidGenerator::new(&compiled);
        let diagrams = generator.generate_all();

        assert!(diagrams.erd.contains("erDiagram"));
        assert!(diagrams.state.contains("stateDiagram-v2"));
        assert!(diagrams.sequence.contains("sequenceDiagram"));
    }
}
