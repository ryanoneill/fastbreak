//! Markdown documentation generator
//!
//! Generates comprehensive Markdown documentation from a compiled specification.

use crate::model::{
    CompiledAction, CompiledEnum, CompiledProperty, CompiledRelation, CompiledScenario,
    CompiledSpec, CompiledState, CompiledStruct, RelationProperty, TemporalOp,
};
use crate::semantic::Type;
use std::fmt::Write;

/// Markdown documentation generator
pub struct MarkdownGenerator<'a> {
    spec: &'a CompiledSpec,
    output: String,
}

impl<'a> MarkdownGenerator<'a> {
    /// Create a new Markdown generator for a specification
    #[must_use]
    pub fn new(spec: &'a CompiledSpec) -> Self {
        Self {
            spec,
            output: String::new(),
        }
    }

    /// Generate complete Markdown documentation
    #[must_use]
    pub fn generate(mut self) -> String {
        self.generate_header();
        self.generate_table_of_contents();
        self.generate_types_section();
        self.generate_enums_section();
        self.generate_states_section();
        self.generate_actions_section();
        self.generate_relations_section();
        self.generate_scenarios_section();
        self.generate_properties_section();
        self.output
    }

    fn generate_header(&mut self) {
        let module_name = self
            .spec
            .module
            .as_ref()
            .map_or("Specification", |m| m.as_str());

        writeln!(self.output, "# {module_name}\n").unwrap();

        if !self.spec.imports.is_empty() {
            writeln!(self.output, "## Imports\n").unwrap();
            for import in &self.spec.imports {
                let path = import.path.join("::");
                if import.items.is_empty() {
                    writeln!(self.output, "- `{path}`").unwrap();
                } else {
                    let items = import.items.join(", ");
                    writeln!(self.output, "- `{path}` ({items})").unwrap();
                }
            }
            writeln!(self.output).unwrap();
        }
    }

    fn generate_table_of_contents(&mut self) {
        writeln!(self.output, "## Table of Contents\n").unwrap();

        if !self.spec.structs.is_empty() {
            writeln!(self.output, "- [Types](#types)").unwrap();
        }
        if !self.spec.enums.is_empty() {
            writeln!(self.output, "- [Enums](#enums)").unwrap();
        }
        if !self.spec.states.is_empty() {
            writeln!(self.output, "- [States](#states)").unwrap();
        }
        if !self.spec.actions.is_empty() {
            writeln!(self.output, "- [Actions](#actions)").unwrap();
        }
        if !self.spec.relations.is_empty() {
            writeln!(self.output, "- [Relations](#relations)").unwrap();
        }
        if !self.spec.scenarios.is_empty() {
            writeln!(self.output, "- [Scenarios](#scenarios)").unwrap();
        }
        if !self.spec.properties.is_empty() {
            writeln!(self.output, "- [Properties](#properties)").unwrap();
        }
        writeln!(self.output).unwrap();
    }

    fn generate_types_section(&mut self) {
        if self.spec.structs.is_empty() {
            return;
        }

        writeln!(self.output, "## Types\n").unwrap();

        for (name, struct_def) in &self.spec.structs {
            self.generate_struct(name, struct_def);
        }
    }

    fn generate_struct(&mut self, name: &str, struct_def: &CompiledStruct) {
        // Header with type parameters
        if struct_def.type_params.is_empty() {
            writeln!(self.output, "### `{name}`\n").unwrap();
        } else {
            let params = struct_def.type_params.join(", ");
            writeln!(self.output, "### `{name}<{params}>`\n").unwrap();
        }

        // Documentation
        if let Some(doc) = &struct_def.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Fields table
        if !struct_def.fields.is_empty() {
            writeln!(self.output, "| Field | Type |").unwrap();
            writeln!(self.output, "|-------|------|").unwrap();
            for (field_name, field_type) in &struct_def.fields {
                let type_str = Self::format_type(field_type);
                writeln!(self.output, "| `{field_name}` | `{type_str}` |").unwrap();
            }
            writeln!(self.output).unwrap();
        }
    }

    fn generate_enums_section(&mut self) {
        if self.spec.enums.is_empty() {
            return;
        }

        writeln!(self.output, "## Enums\n").unwrap();

        for (name, enum_def) in &self.spec.enums {
            self.generate_enum(name, enum_def);
        }
    }

    fn generate_enum(&mut self, name: &str, enum_def: &CompiledEnum) {
        // Header with type parameters
        if enum_def.type_params.is_empty() {
            writeln!(self.output, "### `{name}`\n").unwrap();
        } else {
            let params = enum_def.type_params.join(", ");
            writeln!(self.output, "### `{name}<{params}>`\n").unwrap();
        }

        // Documentation
        if let Some(doc) = &enum_def.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Variants
        writeln!(self.output, "**Variants:**\n").unwrap();
        for (variant_name, variant) in &enum_def.variants {
            if variant.is_unit() {
                writeln!(self.output, "- `{variant_name}`").unwrap();
            } else if variant.is_tuple {
                let fields: Vec<String> = variant
                    .fields
                    .values()
                    .map(Self::format_type)
                    .collect();
                let fields_str = fields.join(", ");
                writeln!(self.output, "- `{variant_name}({fields_str})`").unwrap();
            } else {
                let fields: Vec<String> = variant
                    .fields
                    .iter()
                    .map(|(n, t)| format!("{}: {}", n, Self::format_type(t)))
                    .collect();
                let fields_str = fields.join(", ");
                writeln!(self.output, "- `{variant_name} {{ {fields_str} }}`").unwrap();
            }
        }
        writeln!(self.output).unwrap();
    }

    fn generate_states_section(&mut self) {
        if self.spec.states.is_empty() {
            return;
        }

        writeln!(self.output, "## States\n").unwrap();

        for (name, state_def) in &self.spec.states {
            self.generate_state(name, state_def);
        }
    }

    fn generate_state(&mut self, name: &str, state_def: &CompiledState) {
        writeln!(self.output, "### `{name}`\n").unwrap();

        // Documentation
        if let Some(doc) = &state_def.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Fields table
        if !state_def.fields.is_empty() {
            writeln!(self.output, "**State Variables:**\n").unwrap();
            writeln!(self.output, "| Variable | Type |").unwrap();
            writeln!(self.output, "|----------|------|").unwrap();
            for (field_name, field_type) in &state_def.fields {
                let type_str = Self::format_type(field_type);
                writeln!(self.output, "| `{field_name}` | `{type_str}` |").unwrap();
            }
            writeln!(self.output).unwrap();
        }

        // Invariants
        if !state_def.invariants.is_empty() {
            writeln!(self.output, "**Invariants:**\n").unwrap();
            for invariant in &state_def.invariants {
                let inv_name = invariant
                    .name
                    .as_deref()
                    .unwrap_or("unnamed");
                writeln!(self.output, "- {inv_name}").unwrap();
            }
            writeln!(self.output).unwrap();
        }
    }

    fn generate_actions_section(&mut self) {
        if self.spec.actions.is_empty() {
            return;
        }

        writeln!(self.output, "## Actions\n").unwrap();

        for (name, action_def) in &self.spec.actions {
            self.generate_action(name, action_def);
        }
    }

    fn generate_action(&mut self, name: &str, action_def: &CompiledAction) {
        // Signature
        let params: Vec<String> = action_def
            .params
            .iter()
            .map(|(n, t)| format!("{}: {}", n, Self::format_type(t)))
            .collect();
        let params_str = params.join(", ");
        let return_type = Self::format_type(&action_def.return_type);

        writeln!(self.output, "### `{name}({params_str}) -> {return_type}`\n").unwrap();

        // Documentation
        if let Some(doc) = &action_def.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Parameters table
        if !action_def.params.is_empty() {
            writeln!(self.output, "**Parameters:**\n").unwrap();
            writeln!(self.output, "| Name | Type |").unwrap();
            writeln!(self.output, "|------|------|").unwrap();
            for (param_name, param_type) in &action_def.params {
                let type_str = Self::format_type(param_type);
                writeln!(self.output, "| `{param_name}` | `{type_str}` |").unwrap();
            }
            writeln!(self.output).unwrap();
        }

        // Contracts
        if !action_def.requires.is_empty() {
            writeln!(self.output, "**Preconditions:** {}\n", action_def.requires.len()).unwrap();
        }
        if !action_def.ensures.is_empty() {
            writeln!(self.output, "**Postconditions:** {}\n", action_def.ensures.len()).unwrap();
        }
    }

    fn generate_relations_section(&mut self) {
        if self.spec.relations.is_empty() {
            return;
        }

        writeln!(self.output, "## Relations\n").unwrap();

        for (name, relation_def) in &self.spec.relations {
            self.generate_relation(name, relation_def);
        }
    }

    fn generate_relation(&mut self, name: &str, relation_def: &CompiledRelation) {
        let source = Self::format_type(&relation_def.source);
        let target = Self::format_type(&relation_def.target);

        writeln!(self.output, "### `{name}: {source} -> {target}`\n").unwrap();

        // Documentation
        if let Some(doc) = &relation_def.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Properties
        if !relation_def.properties.is_empty() {
            writeln!(self.output, "**Properties:**\n").unwrap();
            for prop in &relation_def.properties {
                let prop_name = match prop {
                    RelationProperty::Symmetric => "Symmetric",
                    RelationProperty::Reflexive => "Reflexive",
                    RelationProperty::Irreflexive => "Irreflexive",
                    RelationProperty::Transitive => "Transitive",
                    RelationProperty::Antisymmetric => "Antisymmetric",
                };
                writeln!(self.output, "- {prop_name}").unwrap();
            }
            writeln!(self.output).unwrap();
        }
    }

    fn generate_scenarios_section(&mut self) {
        if self.spec.scenarios.is_empty() {
            return;
        }

        writeln!(self.output, "## Scenarios\n").unwrap();

        for (i, scenario) in self.spec.scenarios.iter().enumerate() {
            self.generate_scenario(i + 1, scenario);
        }
    }

    fn generate_scenario(&mut self, index: usize, scenario: &CompiledScenario) {
        writeln!(self.output, "### Scenario {}: {}\n", index, scenario.name).unwrap();

        // Documentation
        if let Some(doc) = &scenario.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Given
        if !scenario.given.is_empty() {
            writeln!(self.output, "**Given:**").unwrap();
            for given in &scenario.given {
                for assignment in &given.assignments {
                    writeln!(self.output, "- `{}` is initialized", assignment.name).unwrap();
                }
            }
            writeln!(self.output).unwrap();
        }

        // When
        if !scenario.when.is_empty() {
            writeln!(self.output, "**When:**").unwrap();
            for when in &scenario.when {
                for action in &when.actions {
                    if let Some(binding) = &action.binding {
                        writeln!(self.output, "- `{binding}` = action result").unwrap();
                    } else {
                        writeln!(self.output, "- Action executed").unwrap();
                    }
                }
            }
            writeln!(self.output).unwrap();
        }

        // Then
        if !scenario.then.is_empty() {
            writeln!(self.output, "**Then:**").unwrap();
            for then in &scenario.then {
                writeln!(self.output, "- {} assertion(s) verified", then.assertions.len()).unwrap();
            }
            writeln!(self.output).unwrap();
        }
    }

    fn generate_properties_section(&mut self) {
        if self.spec.properties.is_empty() {
            return;
        }

        writeln!(self.output, "## Properties\n").unwrap();

        for (i, property) in self.spec.properties.iter().enumerate() {
            self.generate_property(i + 1, property);
        }
    }

    fn generate_property(&mut self, index: usize, property: &CompiledProperty) {
        writeln!(self.output, "### Property {}: {}\n", index, property.name).unwrap();

        // Documentation
        if let Some(doc) = &property.doc {
            writeln!(self.output, "{doc}\n").unwrap();
        }

        // Temporal operator
        if let Some(temporal) = &property.temporal {
            let op_name = match temporal {
                TemporalOp::Always => "Always (invariant)",
                TemporalOp::Eventually => "Eventually (liveness)",
                TemporalOp::Until => "Until",
                TemporalOp::Next => "Next state",
            };
            writeln!(self.output, "**Type:** {op_name}\n").unwrap();
        }
    }

    fn format_type(ty: &Type) -> String {
        match ty {
            Type::Int => "Int".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Unit => "()".to_string(),
            Type::Struct(id) | Type::Enum(id) => id.name.to_string(),
            Type::Set(inner) => format!("Set<{}>", Self::format_type(inner)),
            Type::List(inner) => format!("List<{}>", Self::format_type(inner)),
            Type::Map(k, v) => format!("Map<{}, {}>", Self::format_type(k), Self::format_type(v)),
            Type::Option(inner) => format!("Option<{}>", Self::format_type(inner)),
            Type::Result(ok, err) => {
                format!("Result<{}, {}>", Self::format_type(ok), Self::format_type(err))
            }
            Type::Tuple(items) => {
                let items_str: Vec<String> = items.iter().map(Self::format_type).collect();
                format!("({})", items_str.join(", "))
            }
            Type::Function { params, ret } => {
                let params_str: Vec<String> = params.iter().map(Self::format_type).collect();
                format!("({}) -> {}", params_str.join(", "), Self::format_type(ret))
            }
            Type::Var(id) => format!("'{}", id.0),
            Type::Unknown => "?".to_string(),
            Type::Error => "<error>".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::compile;
    use crate::parser::parse;
    use crate::semantic;

    #[test]
    fn test_generate_empty() {
        let spec = parse("").unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("# Specification"));
        assert!(output.contains("## Table of Contents"));
    }

    #[test]
    fn test_generate_with_module() {
        let source = "module auth";
        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("# auth"));
    }

    #[test]
    fn test_generate_type_def() {
        let source = r#"
            type User {
                id: Int,
                name: String,
            }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("## Types"));
        assert!(output.contains("### `User`"));
        assert!(output.contains("| `id` | `Int` |"));
        assert!(output.contains("| `name` | `String` |"));
    }

    #[test]
    fn test_generate_enum_def() {
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

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("## Enums"));
        assert!(output.contains("### `Status`"));
        assert!(output.contains("- `Active`"));
        assert!(output.contains("- `Inactive`"));
        assert!(output.contains("- `Pending(String)`"));
    }

    #[test]
    fn test_generate_state() {
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

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("## States"));
        assert!(output.contains("### `Counter`"));
        assert!(output.contains("| `count` | `Int` |"));
        assert!(output.contains("**Invariants:**"));
        assert!(output.contains("- count is non-negative"));
    }

    #[test]
    fn test_generate_action() {
        let source = r#"
            action increment(amount: Int) -> Int
                requires { amount > 0 }
                ensures { result > 0 }
        "#;

        let spec = parse(source).unwrap();
        let analyzer = semantic::analyze(&spec);
        let compiled = compile(&spec, &analyzer);

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("## Actions"));
        assert!(output.contains("### `increment(amount: Int) -> Int`"));
        assert!(output.contains("| `amount` | `Int` |"));
        assert!(output.contains("**Preconditions:** 1"));
        assert!(output.contains("**Postconditions:** 1"));
    }

    #[test]
    fn test_generate_scenario() {
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

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("## Scenarios"));
        assert!(output.contains("### Scenario 1: simple test"));
        assert!(output.contains("**Given:**"));
        assert!(output.contains("**When:**"));
        assert!(output.contains("**Then:**"));
    }

    #[test]
    fn test_generate_property() {
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

        let generator = MarkdownGenerator::new(&compiled);
        let output = generator.generate();

        assert!(output.contains("## Properties"));
        assert!(output.contains("### Property 1: always positive"));
        assert!(output.contains("**Type:** Always (invariant)"));
    }
}
