# Phase 10: Non-Functional Requirements (Quality Attributes)

## Overview

Add first-class support for non-functional requirements (NFRs) with quantifiable fit criteria. This enables specifying performance, reliability, security, and other quality attributes alongside functional specifications.

### Goals

1. Define measurable quality requirements
2. Bind NFRs to specific constructs (types, actions, states)
3. Generate quality requirements documentation
4. Establish foundation for monitoring/verification

### Non-Goals (for this phase)

- Automated performance testing
- Runtime monitoring integration
- SLA management

---

## Language Design

### Syntax

New top-level `quality` construct:

```fbs
quality performance "API response time" {
    metric: latency,
    scale: p95,
    target: < 200ms,
    constraint: < 500ms,
    applies_to: action create_user,
}

quality reliability "System availability" {
    metric: uptime,
    target: >= 99.9%,
    measurement: monthly,
}

quality security "Password entropy" {
    metric: entropy,
    target: >= 60 bits,
    applies_to: type Password,
}

quality scalability "Concurrent users" {
    metric: concurrent_connections,
    target: >= 10000,
    under_load: normal_traffic,
}
```

### Quality Categories

```fbs
// Performance
quality performance "..." { ... }

// Reliability
quality reliability "..." { ... }

// Security
quality security "..." { ... }

// Usability (less quantifiable, but can specify)
quality usability "..." { ... }

// Scalability
quality scalability "..." { ... }

// Maintainability
quality maintainability "..." { ... }

// Portability (custom category)
quality portability "..." { ... }

// Custom category
quality custom "..." { ... }
```

### Quality Block Structure

```fbs
quality <category> "<name>" {
    // Required: what we're measuring
    metric: <identifier>,

    // Optional: how we measure it
    scale: <identifier>,           // p50, p95, p99, mean, max, min

    // Required: desired value
    target: <comparison_expr>,     // >= 99.9%, < 200ms

    // Optional: unacceptable value
    constraint: <comparison_expr>, // < 500ms (fail if exceeded)

    // Optional: measurement period
    measurement: <identifier>,     // per_request, hourly, daily, monthly

    // Optional: condition under which this applies
    under_load: <identifier>,      // normal, peak, stress

    // Optional: what this applies to
    applies_to: <target>,          // action X, type Y, state Z

    // Optional: how to verify
    verified_by: <identifier>,     // test name or monitoring system
}
```

### Value Literals with Units

New literal syntax for values with units:

```fbs
// Duration
200ms      // milliseconds
5s         // seconds
2m         // minutes
1h         // hours

// Percentage
99.9%
50%

// Data size
1kb, 1mb, 1gb, 1tb

// Rate
1000/s     // per second (throughput)
100/m      // per minute

// Information
60bits     // entropy

// Plain numbers with custom units
10000 connections
1000 requests
```

### Full Examples

```fbs
module api

// Types
type User { ... }
type Password = String where self.len() >= 8

// Actions
action create_user(email: Email, password: Password) -> Result<User, Error>
action login(email: Email, password: String) -> Result<Session, Error>
action list_users(page: Int) -> List<User>

// Performance requirements
quality performance "User creation response time" {
    metric: latency,
    scale: p95,
    target: < 200ms,
    constraint: < 1s,
    applies_to: action create_user,
    measurement: per_request,
}

quality performance "Login response time" {
    metric: latency,
    scale: p99,
    target: < 100ms,
    applies_to: action login,
}

quality performance "List users throughput" {
    metric: throughput,
    target: >= 1000/s,
    applies_to: action list_users,
    under_load: peak,
}

// Reliability requirements
quality reliability "API availability" {
    metric: uptime,
    target: >= 99.9%,
    measurement: monthly,
    applies_to: state ApiSystem,
}

quality reliability "Data durability" {
    metric: durability,
    target: >= 99.999999999%,  // 11 nines
    applies_to: state UserDatabase,
}

// Security requirements
quality security "Password strength" {
    metric: entropy,
    target: >= 60bits,
    applies_to: type Password,
}

quality security "Session timeout" {
    metric: session_duration,
    constraint: < 24h,
    applies_to: type Session,
}

quality security "Failed login lockout" {
    metric: max_attempts,
    constraint: <= 5,
    measurement: per_hour,
    applies_to: action login,
}

// Scalability requirements
quality scalability "Concurrent users" {
    metric: concurrent_connections,
    target: >= 10000,
    under_load: peak,
}

quality scalability "Database connections" {
    metric: connection_pool_size,
    target: >= 100,
    constraint: <= 500,
}

// Maintainability
quality maintainability "Code coverage" {
    metric: test_coverage,
    target: >= 80%,
}

quality maintainability "Documentation coverage" {
    metric: doc_coverage,
    target: >= 90%,
    applies_to: action *,  // all actions
}
```

---

## AST Changes

### New: QualityRequirement

```rust
// src/ast/quality.rs

/// A quality/non-functional requirement
#[derive(Debug, Clone, PartialEq)]
pub struct QualityRequirement {
    /// Category (performance, reliability, security, etc.)
    pub category: QualityCategory,
    /// Descriptive name
    pub name: SmolStr,
    /// The metric being measured
    pub metric: Ident,
    /// Scale/aggregation (p95, mean, max, etc.)
    pub scale: Option<Ident>,
    /// Target value (the goal)
    pub target: QualityExpr,
    /// Constraint value (unacceptable threshold)
    pub constraint: Option<QualityExpr>,
    /// Measurement period
    pub measurement: Option<Ident>,
    /// Load condition
    pub under_load: Option<Ident>,
    /// What this applies to
    pub applies_to: Option<QualityTarget>,
    /// Verification method
    pub verified_by: Option<Ident>,
    /// Attributes
    pub attributes: Vec<Attribute>,
    /// Span
    pub span: Span,
}

/// Quality requirement category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QualityCategory {
    Performance,
    Reliability,
    Security,
    Usability,
    Scalability,
    Maintainability,
    Custom,
}

/// Target of a quality requirement
#[derive(Debug, Clone, PartialEq)]
pub enum QualityTarget {
    /// Applies to an action: `action create_user`
    Action(Path),
    /// Applies to a type: `type Password`
    Type(Path),
    /// Applies to a state: `state System`
    State(Path),
    /// Applies to all of a kind: `action *`
    All(QualityTargetKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QualityTargetKind {
    Action,
    Type,
    State,
}

/// A quality expression (comparison with value)
#[derive(Debug, Clone, PartialEq)]
pub struct QualityExpr {
    /// Comparison operator
    pub op: ComparisonOp,
    /// Value with optional unit
    pub value: QualityValue,
    /// Span
    pub span: Span,
}

/// A value with optional unit
#[derive(Debug, Clone, PartialEq)]
pub struct QualityValue {
    /// Numeric value
    pub number: f64,
    /// Unit (ms, s, %, bits, etc.)
    pub unit: Option<SmolStr>,
    /// Span
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    Lt,      // <
    Le,      // <=
    Gt,      // >
    Ge,      // >=
    Eq,      // ==
}
```

### Modified: Specification

```rust
pub struct Specification {
    // ... existing fields
    pub qualities: Vec<QualityRequirement>,  // NEW
}
```

---

## Lexer Changes

### New Tokens

```rust
// Category keywords
#[token("quality")]
Quality,

#[token("performance")]
Performance,

#[token("reliability")]
Reliability,

#[token("security")]
Security,

#[token("usability")]
Usability,

#[token("scalability")]
Scalability,

#[token("maintainability")]
Maintainability,

// Quality block keywords
#[token("metric")]
Metric,

#[token("scale")]
Scale,

#[token("target")]
Target,

#[token("constraint")]
Constraint,

#[token("measurement")]
Measurement,

#[token("under_load")]
UnderLoad,

#[token("applies_to")]
AppliesTo,

// Value with unit (regex-based)
#[regex(r"[0-9]+(\.[0-9]+)?(ms|s|m|h|%|bits|kb|mb|gb|/s|/m)", parse_value_with_unit)]
ValueWithUnit((f64, SmolStr)),
```

---

## Parser Changes

### New: parse_quality_requirement

```rust
fn parse_quality_requirement(&mut self) -> ParseResult<QualityRequirement> {
    let attributes = self.parse_attributes()?;
    let start = self.expect(&Token::Quality)?;

    let category = self.parse_quality_category()?;
    let name = self.parse_string()?;

    self.expect(&Token::LBrace)?;

    let mut metric = None;
    let mut scale = None;
    let mut target = None;
    let mut constraint = None;
    let mut measurement = None;
    let mut under_load = None;
    let mut applies_to = None;
    let mut verified_by = None;

    while !self.check(&Token::RBrace) {
        let field_name = self.parse_ident()?;
        self.expect(&Token::Colon)?;

        match field_name.as_str() {
            "metric" => metric = Some(self.parse_ident()?),
            "scale" => scale = Some(self.parse_ident()?),
            "target" => target = Some(self.parse_quality_expr()?),
            "constraint" => constraint = Some(self.parse_quality_expr()?),
            "measurement" => measurement = Some(self.parse_ident()?),
            "under_load" => under_load = Some(self.parse_ident()?),
            "applies_to" => applies_to = Some(self.parse_quality_target()?),
            "verified_by" => verified_by = Some(self.parse_ident()?),
            _ => return Err(self.error_unexpected("quality field")),
        }

        // Optional comma
        self.check_and_consume(&Token::Comma);
    }

    let end = self.expect(&Token::RBrace)?;

    // Validate required fields
    let metric = metric.ok_or_else(|| self.error_missing("metric"))?;
    let target = target.ok_or_else(|| self.error_missing("target"))?;

    Ok(QualityRequirement {
        category,
        name,
        metric,
        scale,
        target,
        constraint,
        measurement,
        under_load,
        applies_to,
        verified_by,
        attributes,
        span: start.merge(end),
    })
}

fn parse_quality_category(&mut self) -> ParseResult<QualityCategory> {
    match self.current_token() {
        Token::Performance => { self.advance(); Ok(QualityCategory::Performance) }
        Token::Reliability => { self.advance(); Ok(QualityCategory::Reliability) }
        Token::Security => { self.advance(); Ok(QualityCategory::Security) }
        Token::Usability => { self.advance(); Ok(QualityCategory::Usability) }
        Token::Scalability => { self.advance(); Ok(QualityCategory::Scalability) }
        Token::Maintainability => { self.advance(); Ok(QualityCategory::Maintainability) }
        Token::Ident(_) => { self.advance(); Ok(QualityCategory::Custom) }
        _ => Err(self.error_unexpected("quality category")),
    }
}

fn parse_quality_expr(&mut self) -> ParseResult<QualityExpr> {
    let start = self.current_span();

    let op = match self.current_token() {
        Token::Lt => { self.advance(); ComparisonOp::Lt }
        Token::Le => { self.advance(); ComparisonOp::Le }
        Token::Gt => { self.advance(); ComparisonOp::Gt }
        Token::Ge => { self.advance(); ComparisonOp::Ge }
        Token::EqEq => { self.advance(); ComparisonOp::Eq }
        _ => return Err(self.error_unexpected("comparison operator")),
    };

    let value = self.parse_quality_value()?;

    Ok(QualityExpr {
        op,
        value,
        span: start.merge(value.span),
    })
}

fn parse_quality_value(&mut self) -> ParseResult<QualityValue> {
    let span = self.current_span();

    match self.current_token() {
        Token::ValueWithUnit((num, unit)) => {
            let number = *num;
            let unit = Some(unit.clone());
            self.advance();
            Ok(QualityValue { number, unit, span })
        }
        Token::IntLit(n) => {
            let number = *n as f64;
            self.advance();

            // Check for following unit identifier
            let unit = if self.check_ident() {
                let u = self.parse_ident()?;
                Some(u.name)
            } else {
                None
            };

            Ok(QualityValue { number, unit, span })
        }
        Token::FloatLit(n) => {
            let number = *n;
            self.advance();
            let unit = if self.check_ident() {
                Some(self.parse_ident()?.name)
            } else {
                None
            };
            Ok(QualityValue { number, unit, span })
        }
        _ => Err(self.error_unexpected("numeric value")),
    }
}

fn parse_quality_target(&mut self) -> ParseResult<QualityTarget> {
    let kind = match self.current_token() {
        Token::Action => { self.advance(); QualityTargetKind::Action }
        Token::Type => { self.advance(); QualityTargetKind::Type }
        Token::State => { self.advance(); QualityTargetKind::State }
        _ => return Err(self.error_unexpected("action, type, or state")),
    };

    if self.check(&Token::Star) {
        self.advance();
        Ok(QualityTarget::All(kind))
    } else {
        let path = self.parse_path()?;
        match kind {
            QualityTargetKind::Action => Ok(QualityTarget::Action(path)),
            QualityTargetKind::Type => Ok(QualityTarget::Type(path)),
            QualityTargetKind::State => Ok(QualityTarget::State(path)),
        }
    }
}
```

---

## Semantic Analysis Changes

### Quality Validation

```rust
fn analyze_quality(&mut self, quality: &QualityRequirement) {
    self.analyze_attributes(&quality.attributes, AttributeContext::Quality);

    // Validate applies_to references exist
    if let Some(ref target) = quality.applies_to {
        match target {
            QualityTarget::Action(path) => {
                if !self.actions.contains_key(&path.to_string()) {
                    self.error(SemanticError::UnknownAction {
                        name: path.to_string().into(),
                        span: quality.span,
                    });
                }
            }
            QualityTarget::Type(path) => {
                if !self.types.contains_key(&path.to_string()) {
                    self.error(SemanticError::UnknownType {
                        name: path.to_string().into(),
                        span: quality.span,
                    });
                }
            }
            QualityTarget::State(path) => {
                if !self.states.contains_key(&path.to_string()) {
                    self.error(SemanticError::UnknownState {
                        name: path.to_string().into(),
                        span: quality.span,
                    });
                }
            }
            QualityTarget::All(_) => { /* valid */ }
        }
    }

    // Validate target vs constraint consistency
    if let Some(ref constraint) = quality.constraint {
        if !self.constraints_consistent(&quality.target, constraint) {
            self.warning(SemanticWarning::InconsistentQualityConstraints {
                name: quality.name.clone(),
                span: quality.span,
            });
        }
    }

    // Validate known metrics (optional, can be extended)
    let known_metrics = ["latency", "throughput", "uptime", "durability",
                         "entropy", "coverage", "connections"];
    if !known_metrics.contains(&quality.metric.as_str()) {
        // Just a warning - custom metrics are allowed
        self.info(SemanticInfo::CustomMetric {
            metric: quality.metric.name.clone(),
            span: quality.metric.span,
        });
    }
}

fn constraints_consistent(&self, target: &QualityExpr, constraint: &QualityExpr) -> bool {
    // For < or <=: constraint should be >= target
    // For > or >=: constraint should be <= target
    // This is a simplified check
    match (target.op, constraint.op) {
        (ComparisonOp::Lt, ComparisonOp::Lt) |
        (ComparisonOp::Le, ComparisonOp::Lt) |
        (ComparisonOp::Le, ComparisonOp::Le) => {
            target.value.number <= constraint.value.number
        }
        (ComparisonOp::Gt, ComparisonOp::Gt) |
        (ComparisonOp::Ge, ComparisonOp::Gt) |
        (ComparisonOp::Ge, ComparisonOp::Ge) => {
            target.value.number >= constraint.value.number
        }
        _ => true // Can't easily validate mixed comparisons
    }
}
```

---

## Model Changes

### New: CompiledQuality

```rust
pub struct CompiledQuality {
    pub category: QualityCategory,
    pub name: SmolStr,
    pub metric: SmolStr,
    pub scale: Option<SmolStr>,
    pub target: CompiledQualityExpr,
    pub constraint: Option<CompiledQualityExpr>,
    pub measurement: Option<SmolStr>,
    pub under_load: Option<SmolStr>,
    pub applies_to: Option<CompiledQualityTarget>,
    pub verified_by: Option<SmolStr>,
    pub attributes: Vec<CompiledAttribute>,
}

pub struct CompiledQualityExpr {
    pub op: ComparisonOp,
    pub value: f64,
    pub unit: Option<SmolStr>,
}

pub enum CompiledQualityTarget {
    Action(SmolStr),
    Type(SmolStr),
    State(SmolStr),
    AllActions,
    AllTypes,
    AllStates,
}
```

### Modified: CompiledSpec

```rust
pub struct CompiledSpec {
    // ... existing fields
    pub qualities: Vec<CompiledQuality>,  // NEW
}
```

---

## Codegen Changes

### Markdown Generator

```rust
fn generate_quality_requirements(&self) -> String {
    let mut output = String::from("## Quality Requirements\n\n");

    // Group by category
    let by_category = self.group_qualities_by_category();

    for (category, qualities) in by_category {
        output.push_str(&format!("### {}\n\n", format_category(category)));

        output.push_str("| Requirement | Metric | Target | Constraint | Applies To |\n");
        output.push_str("|-------------|--------|--------|------------|------------|\n");

        for q in qualities {
            output.push_str(&format!(
                "| {} | {} | {} | {} | {} |\n",
                q.name,
                format_metric(&q.metric, &q.scale),
                format_quality_expr(&q.target),
                q.constraint.as_ref().map(format_quality_expr).unwrap_or_default(),
                q.applies_to.as_ref().map(format_target).unwrap_or_default(),
            ));
        }

        output.push_str("\n");
    }

    output
}

fn format_quality_expr(expr: &CompiledQualityExpr) -> String {
    let op_str = match expr.op {
        ComparisonOp::Lt => "<",
        ComparisonOp::Le => "≤",
        ComparisonOp::Gt => ">",
        ComparisonOp::Ge => "≥",
        ComparisonOp::Eq => "=",
    };

    let unit = expr.unit.as_deref().unwrap_or("");
    format!("{} {}{}", op_str, expr.value, unit)
}
```

### New: Quality Summary Generator

```rust
fn generate_quality_summary(&self) -> String {
    let mut output = String::from("## Quality Summary\n\n");

    let performance = self.qualities.iter()
        .filter(|q| q.category == QualityCategory::Performance)
        .count();
    let reliability = self.qualities.iter()
        .filter(|q| q.category == QualityCategory::Reliability)
        .count();
    // ... etc

    output.push_str("| Category | Count |\n");
    output.push_str("|----------|-------|\n");
    output.push_str(&format!("| Performance | {} |\n", performance));
    output.push_str(&format!("| Reliability | {} |\n", reliability));
    // ... etc

    output
}
```

---

## CLI Changes

### Updated Build Output

```
fb build

Checking myproject...
  5 definition(s)
  2 scenario(s)
  0 property(ies)
  8 quality requirement(s)    <-- NEW

No errors found.
```

### New: `fb quality` Command

```rust
/// List and analyze quality requirements
Quality {
    /// Path to manifest or source file
    #[arg(default_value = ".")]
    path: PathBuf,
    /// Filter by category
    #[arg(short, long)]
    category: Option<String>,
    /// Show only requirements applying to specific target
    #[arg(short, long)]
    target: Option<String>,
}
```

---

## Testing Strategy

### Parser Tests

```rust
#[test]
fn test_parse_quality_basic() {
    let spec = parse(r#"
        quality performance "response time" {
            metric: latency,
            target: < 200ms,
        }
    "#).unwrap();

    assert_eq!(spec.qualities.len(), 1);
    let q = &spec.qualities[0];
    assert_eq!(q.category, QualityCategory::Performance);
    assert_eq!(q.metric.as_str(), "latency");
}

#[test]
fn test_parse_quality_full() {
    let spec = parse(r#"
        quality reliability "uptime" {
            metric: uptime,
            scale: monthly,
            target: >= 99.9%,
            constraint: >= 99%,
            measurement: monthly,
            applies_to: state System,
            verified_by: uptime_monitor,
        }
    "#).unwrap();

    let q = &spec.qualities[0];
    assert!(q.scale.is_some());
    assert!(q.constraint.is_some());
    assert!(q.applies_to.is_some());
}

#[test]
fn test_parse_value_with_unit() {
    let spec = parse(r#"
        quality performance "fast" {
            metric: latency,
            target: < 100ms,
        }
    "#).unwrap();

    let target = &spec.qualities[0].target;
    assert_eq!(target.value.number, 100.0);
    assert_eq!(target.value.unit.as_deref(), Some("ms"));
}
```

### Semantic Tests

```rust
#[test]
fn test_quality_unknown_action() {
    let result = analyze(r#"
        quality performance "x" {
            metric: latency,
            target: < 100ms,
            applies_to: action nonexistent,
        }
    "#);
    assert!(result.has_errors());
}

#[test]
fn test_quality_inconsistent_constraints() {
    let result = analyze(r#"
        quality performance "x" {
            metric: latency,
            target: < 100ms,
            constraint: < 50ms,  // stricter than target - warning
        }
    "#);
    // Should produce warning
}
```

---

## Implementation Order

### Step 1: AST (1 unit)
- Define `QualityRequirement` and related types
- Add `qualities` to `Specification`

### Step 2: Lexer (0.5 unit)
- Add category keywords
- Add quality field keywords
- Add value-with-unit parsing

### Step 3: Parser (1.5 units)
- Implement `parse_quality_requirement`
- Handle all quality block fields
- Parse value literals with units
- Write parser tests

### Step 4: Semantic Analysis (1 unit)
- Validate applies_to references
- Check constraint consistency
- Handle unknown metrics gracefully
- Write semantic tests

### Step 5: Model (0.5 unit)
- Add `CompiledQuality`
- Update `CompiledSpec`

### Step 6: Codegen (1 unit)
- Generate quality requirements table
- Add to Markdown output
- Write codegen tests

### Step 7: CLI (0.5 unit)
- Update build output
- Add `fb quality` command

---

## Future Work

1. **SLA Generation**: Export as SLA document
2. **Monitoring Integration**: Generate Prometheus/Grafana configs
3. **Test Generation**: Create performance test scaffolds
4. **Trend Analysis**: Track quality metrics over spec versions
