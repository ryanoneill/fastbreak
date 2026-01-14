//! Property and invariant checking
//!
//! This module provides infrastructure for checking invariants and properties
//! against state snapshots and traces.

use super::spec::{CompiledInvariant, CompiledProperty, CompiledSpec, TemporalOp};
use super::state::{Environment, StateSnapshot, Trace, Value};
use crate::ast::{BinaryOp, Expr, ExprKind, Literal, QuantBindingKind, UnaryOp};
use crate::Span;
use std::sync::Arc;

/// Result of checking an invariant or property
#[derive(Debug, Clone)]
pub struct CheckResult {
    /// Whether the check passed
    pub passed: bool,
    /// Name of what was checked
    pub name: String,
    /// Description of the result
    pub message: String,
    /// Source span for error reporting
    pub span: Option<Span>,
    /// Counter-example if check failed
    pub counterexample: Option<Counterexample>,
}

impl CheckResult {
    /// Create a passing result
    #[must_use]
    pub fn pass(name: impl Into<String>) -> Self {
        Self {
            passed: true,
            name: name.into(),
            message: String::new(),
            span: None,
            counterexample: None,
        }
    }

    /// Create a failing result
    #[must_use]
    pub fn fail(name: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            passed: false,
            name: name.into(),
            message: message.into(),
            span: None,
            counterexample: None,
        }
    }

    /// Add a span to the result
    #[must_use]
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    /// Add a counterexample to the result
    #[must_use]
    pub fn with_counterexample(mut self, cx: Counterexample) -> Self {
        self.counterexample = Some(cx);
        self
    }
}

/// A counterexample showing why a check failed
#[derive(Debug, Clone)]
pub struct Counterexample {
    /// The state that caused the failure
    pub state: StateSnapshot,
    /// Optional trace leading to this state
    pub trace: Option<Trace>,
    /// Description of the failure
    pub description: String,
}

/// Checker for invariants and properties
pub struct Checker<'a> {
    /// The compiled specification
    spec: &'a CompiledSpec,
}

impl<'a> Checker<'a> {
    /// Create a new checker for a specification
    #[must_use]
    pub const fn new(spec: &'a CompiledSpec) -> Self {
        Self { spec }
    }

    /// Check all invariants against a state snapshot
    #[must_use]
    pub fn check_invariants(&self, state: &StateSnapshot) -> Vec<CheckResult> {
        let mut results = Vec::new();

        for compiled_state in self.spec.states.values() {
            for invariant in &compiled_state.invariants {
                let result = self.check_invariant(invariant, state);
                results.push(result);
            }
        }

        results
    }

    /// Check a single invariant against a state
    #[must_use]
    pub fn check_invariant(
        &self,
        invariant: &CompiledInvariant,
        state: &StateSnapshot,
    ) -> CheckResult {
        let name = invariant
            .name
            .clone()
            .unwrap_or_else(|| "unnamed invariant".to_string());

        let mut env = Environment::new();

        // Bind state variables to environment
        for (var_name, value) in &state.variables {
            env.define(var_name.clone(), value.clone());
        }

        match self.evaluate(&invariant.expr, &env, state) {
            Ok(Value::Bool(true)) => CheckResult::pass(&name),
            Ok(Value::Bool(false)) => {
                CheckResult::fail(&name, "Invariant violated").with_span(invariant.span)
            }
            Ok(other) => CheckResult::fail(
                &name,
                format!("Invariant expression returned non-boolean: {other}"),
            )
            .with_span(invariant.span),
            Err(e) => CheckResult::fail(&name, format!("Error evaluating invariant: {e}"))
                .with_span(invariant.span),
        }
    }

    /// Check all properties against a trace
    #[must_use]
    pub fn check_properties(&self, trace: &Trace) -> Vec<CheckResult> {
        self.spec
            .properties
            .iter()
            .map(|prop| self.check_property(prop, trace))
            .collect()
    }

    /// Check a single property against a trace
    #[must_use]
    pub fn check_property(&self, property: &CompiledProperty, trace: &Trace) -> CheckResult {
        match property.temporal {
            Some(TemporalOp::Always) => self.check_always(property, trace),
            Some(TemporalOp::Eventually) => self.check_eventually(property, trace),
            Some(TemporalOp::Next) => self.check_next(property, trace),
            Some(TemporalOp::Until) => {
                // Until requires special handling with two expressions
                CheckResult::fail(&property.name, "Until operator not yet implemented")
            }
            None => {
                // Non-temporal property - check against current state
                if let Some(state) = trace.current() {
                    let mut env = Environment::new();
                    for (var_name, value) in &state.variables {
                        env.define(var_name.clone(), value.clone());
                    }

                    match self.evaluate(&property.expr, &env, state) {
                        Ok(Value::Bool(true)) => CheckResult::pass(&property.name),
                        Ok(Value::Bool(false)) => {
                            CheckResult::fail(&property.name, "Property violated")
                        }
                        Ok(_) => CheckResult::fail(
                            &property.name,
                            "Property expression returned non-boolean",
                        ),
                        Err(e) => {
                            CheckResult::fail(&property.name, format!("Evaluation error: {e}"))
                        }
                    }
                } else {
                    CheckResult::fail(&property.name, "No state to check property against")
                }
            }
        }
    }

    /// Check that a property holds in all states
    fn check_always(&self, property: &CompiledProperty, trace: &Trace) -> CheckResult {
        for (i, state) in trace.states.iter().enumerate() {
            let mut env = Environment::new();
            for (var_name, value) in &state.variables {
                env.define(var_name.clone(), value.clone());
            }

            match self.evaluate(&property.expr, &env, state) {
                Ok(Value::Bool(true)) => {}
                Ok(Value::Bool(false)) => {
                    return CheckResult::fail(
                        &property.name,
                        format!("Property violated in state {i}"),
                    )
                    .with_counterexample(Counterexample {
                        state: state.clone(),
                        trace: Some(trace.clone()),
                        description: format!("Property failed at step {i}"),
                    });
                }
                Ok(_) => {
                    return CheckResult::fail(
                        &property.name,
                        "Property expression returned non-boolean",
                    );
                }
                Err(e) => {
                    return CheckResult::fail(&property.name, format!("Evaluation error: {e}"));
                }
            }
        }
        CheckResult::pass(&property.name)
    }

    /// Check that a property holds in at least one state
    fn check_eventually(&self, property: &CompiledProperty, trace: &Trace) -> CheckResult {
        for state in &trace.states {
            let mut env = Environment::new();
            for (var_name, value) in &state.variables {
                env.define(var_name.clone(), value.clone());
            }

            match self.evaluate(&property.expr, &env, state) {
                Ok(Value::Bool(true)) => return CheckResult::pass(&property.name),
                Ok(Value::Bool(false)) => {}
                Ok(_) => {
                    return CheckResult::fail(
                        &property.name,
                        "Property expression returned non-boolean",
                    );
                }
                Err(e) => {
                    return CheckResult::fail(&property.name, format!("Evaluation error: {e}"));
                }
            }
        }
        CheckResult::fail(&property.name, "Property never became true")
    }

    /// Check that a property holds in the next state
    fn check_next(&self, property: &CompiledProperty, trace: &Trace) -> CheckResult {
        if trace.states.len() < 2 {
            return CheckResult::fail(&property.name, "Not enough states for next operator");
        }

        let next_state = &trace.states[1];
        let mut env = Environment::new();
        for (var_name, value) in &next_state.variables {
            env.define(var_name.clone(), value.clone());
        }

        match self.evaluate(&property.expr, &env, next_state) {
            Ok(Value::Bool(true)) => CheckResult::pass(&property.name),
            Ok(Value::Bool(false)) => {
                CheckResult::fail(&property.name, "Property violated in next state")
            }
            Ok(_) => CheckResult::fail(
                &property.name,
                "Property expression returned non-boolean",
            ),
            Err(e) => CheckResult::fail(&property.name, format!("Evaluation error: {e}")),
        }
    }

    /// Evaluate an expression in the given environment
    #[allow(clippy::too_many_lines, clippy::self_only_used_in_recursion)]
    fn evaluate(
        &self,
        expr: &Expr,
        env: &Environment,
        state: &StateSnapshot,
    ) -> Result<Value, String> {
        match &expr.kind {
            ExprKind::Literal(lit) => Ok(Self::eval_literal(lit)),
            ExprKind::Var(path) => {
                let name = path.name().map_or("", |id| id.name.as_str());
                env.get(name)
                    .or_else(|| state.get(name))
                    .cloned()
                    .ok_or_else(|| format!("Undefined variable: {name}"))
            }
            ExprKind::Binary { left, op, right } => {
                let lhs = self.evaluate(left, env, state)?;
                let rhs = self.evaluate(right, env, state)?;
                Self::eval_binary(*op, lhs, rhs)
            }
            ExprKind::Unary { op, expr: inner } => {
                let value = self.evaluate(inner, env, state)?;
                Self::eval_unary(*op, &value)
            }
            ExprKind::Field { base, field } => {
                let base_value = self.evaluate(base, env, state)?;
                Self::eval_field(base_value, &field.name)
            }
            ExprKind::Index { base, index } => {
                let base_value = self.evaluate(base, env, state)?;
                let index_value = self.evaluate(index, env, state)?;
                Self::eval_index(base_value, &index_value)
            }
            ExprKind::Call { func, args } => {
                // For now, just evaluate the function expression
                let func_name = if let ExprKind::Var(path) = &func.kind {
                    path.name().map_or("unknown", |id| id.name.as_str())
                } else {
                    "unknown"
                };

                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|a| self.evaluate(a, env, state))
                    .collect::<Result<_, _>>()?;

                Self::eval_builtin(func_name, &arg_values)
            }
            ExprKind::MethodCall {
                receiver,
                method,
                args,
            } => {
                let recv_value = self.evaluate(receiver, env, state)?;
                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|a| self.evaluate(a, env, state))
                    .collect::<Result<_, _>>()?;

                Self::eval_method(recv_value, &method.name, &arg_values)
            }
            ExprKind::Forall {
                bindings,
                filter,
                body,
            } => {
                // For each binding, iterate over the collection and check the body
                if bindings.is_empty() {
                    return Ok(Value::Bool(true));
                }

                let binding = &bindings[0];

                // Get the collection from the binding
                let collection = match &binding.kind {
                    QuantBindingKind::InCollection(coll_expr) => {
                        self.evaluate(coll_expr, env, state)?
                    }
                    QuantBindingKind::Typed(_) => {
                        // Typed quantifiers can't be evaluated at runtime
                        // (we can't enumerate all values of a type)
                        return Err(
                            "typed quantifiers cannot be evaluated at runtime".to_string(),
                        );
                    }
                };

                let items = match &collection {
                    Value::Set(s) => s.clone(),
                    Value::List(l) => l.clone(),
                    _ => return Err("forall requires a collection".to_string()),
                };

                for item in items {
                    let mut inner_env = env.clone();
                    inner_env.push_scope();
                    inner_env.define(binding.name.name.clone(), item);

                    // Apply filter if present
                    if let Some(filter_expr) = filter {
                        match self.evaluate(filter_expr, &inner_env, state)? {
                            Value::Bool(false) => continue,
                            Value::Bool(true) => {}
                            _ => return Err("filter must return boolean".to_string()),
                        }
                    }

                    match self.evaluate(body, &inner_env, state)? {
                        Value::Bool(true) => {}
                        Value::Bool(false) => return Ok(Value::Bool(false)),
                        _ => return Err("forall body must return boolean".to_string()),
                    }
                }

                Ok(Value::Bool(true))
            }
            ExprKind::Exists {
                bindings,
                filter,
                body,
            } => {
                if bindings.is_empty() {
                    return Ok(Value::Bool(false));
                }

                let binding = &bindings[0];

                // Get the collection from the binding
                let collection = match &binding.kind {
                    QuantBindingKind::InCollection(coll_expr) => {
                        self.evaluate(coll_expr, env, state)?
                    }
                    QuantBindingKind::Typed(_) => {
                        // Typed quantifiers can't be evaluated at runtime
                        // (we can't enumerate all values of a type)
                        return Err(
                            "typed quantifiers cannot be evaluated at runtime".to_string(),
                        );
                    }
                };

                let items = match &collection {
                    Value::Set(s) => s.clone(),
                    Value::List(l) => l.clone(),
                    _ => return Err("exists requires a collection".to_string()),
                };

                for item in items {
                    let mut inner_env = env.clone();
                    inner_env.push_scope();
                    inner_env.define(binding.name.name.clone(), item);

                    // Apply filter if present
                    if let Some(filter_expr) = filter {
                        match self.evaluate(filter_expr, &inner_env, state)? {
                            Value::Bool(false) => continue,
                            Value::Bool(true) => {}
                            _ => return Err("filter must return boolean".to_string()),
                        }
                    }

                    match self.evaluate(body, &inner_env, state)? {
                        Value::Bool(true) => return Ok(Value::Bool(true)),
                        Value::Bool(false) => {}
                        _ => return Err("exists body must return boolean".to_string()),
                    }
                }

                Ok(Value::Bool(false))
            }
            ExprKind::Implies {
                antecedent,
                consequent,
            } => {
                let ant = self.evaluate(antecedent, env, state)?;
                match ant {
                    Value::Bool(false) => Ok(Value::Bool(true)),
                    Value::Bool(true) => self.evaluate(consequent, env, state),
                    _ => Err("implies antecedent must be boolean".to_string()),
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(condition, env, state)?;
                match cond {
                    Value::Bool(true) => self.evaluate(then_branch, env, state),
                    Value::Bool(false) => {
                        if let Some(else_expr) = else_branch {
                            self.evaluate(else_expr, env, state)
                        } else {
                            Ok(Value::Unit)
                        }
                    }
                    _ => Err("if condition must be boolean".to_string()),
                }
            }
            ExprKind::Set(elements) => {
                let values: Vec<Value> = elements
                    .iter()
                    .map(|e| self.evaluate(e, env, state))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Set(values.into_iter().collect()))
            }
            ExprKind::List(elements) => {
                let values: Vec<Value> = elements
                    .iter()
                    .map(|e| self.evaluate(e, env, state))
                    .collect::<Result<_, _>>()?;
                Ok(Value::List(values))
            }
            ExprKind::Tuple(elements) => {
                let values: Vec<Value> = elements
                    .iter()
                    .map(|e| self.evaluate(e, env, state))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Tuple(values))
            }
            ExprKind::Prime(inner) => {
                // Prime refers to the "next" state value - for now, just evaluate as-is
                self.evaluate(inner, env, state)
            }
            ExprKind::Old(inner) => {
                // Old refers to the "previous" state value - for now, just evaluate as-is
                self.evaluate(inner, env, state)
            }
            ExprKind::Result => env
                .get("result")
                .cloned()
                .ok_or_else(|| "result not defined".to_string()),
            ExprKind::Block(exprs) => {
                let mut result = Value::Unit;
                for e in exprs {
                    result = self.evaluate(e, env, state)?;
                }
                Ok(result)
            }
            _ => Err(format!("Unsupported expression kind: {:?}", expr.kind)),
        }
    }

    fn eval_literal(lit: &Literal) -> Value {
        match lit {
            Literal::Int(n) => Value::Int(*n),
            Literal::Float(n) => Value::Float(*n),
            Literal::String(s) => Value::String(Arc::from(s.as_str())),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Unit => Value::Unit,
        }
    }

    fn eval_binary(op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, String> {
        match op {
            BinaryOp::Add => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                _ => Err("+ requires integers".to_string()),
            },
            BinaryOp::Sub => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                _ => Err("- requires integers".to_string()),
            },
            BinaryOp::Mul => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                _ => Err("* requires integers".to_string()),
            },
            BinaryOp::Div => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) if b != 0 => Ok(Value::Int(a / b)),
                (Value::Int(_), Value::Int(0)) => Err("division by zero".to_string()),
                _ => Err("/ requires integers".to_string()),
            },
            BinaryOp::Mod => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) if b != 0 => Ok(Value::Int(a % b)),
                (Value::Int(_), Value::Int(0)) => Err("modulo by zero".to_string()),
                _ => Err("% requires integers".to_string()),
            },
            BinaryOp::Eq => Ok(Value::Bool(lhs == rhs)),
            BinaryOp::NotEq => Ok(Value::Bool(lhs != rhs)),
            BinaryOp::Lt => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                _ => Err("< requires integers".to_string()),
            },
            BinaryOp::LtEq => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                _ => Err("<= requires integers".to_string()),
            },
            BinaryOp::Gt => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                _ => Err("> requires integers".to_string()),
            },
            BinaryOp::GtEq => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                _ => Err(">= requires integers".to_string()),
            },
            BinaryOp::And => match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                _ => Err("&& requires booleans".to_string()),
            },
            BinaryOp::Or => match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                _ => Err("|| requires booleans".to_string()),
            },
            BinaryOp::In => match rhs {
                Value::Set(set) => Ok(Value::Bool(set.contains(&lhs))),
                Value::List(list) => Ok(Value::Bool(list.contains(&lhs))),
                Value::Map(map) => Ok(Value::Bool(lhs.is_in_map(&map))),
                _ => Err("in requires a collection".to_string()),
            },
            BinaryOp::Union => match (lhs, rhs) {
                (Value::Set(mut a), Value::Set(b)) => {
                    for v in b {
                        if !a.contains(&v) {
                            a.push(v);
                        }
                    }
                    Ok(Value::Set(a))
                }
                _ => Err("union requires sets".to_string()),
            },
            BinaryOp::Intersect => match (lhs, rhs) {
                (Value::Set(a), Value::Set(b)) => {
                    let result = a.into_iter().filter(|v| b.contains(v)).collect();
                    Ok(Value::Set(result))
                }
                _ => Err("intersect requires sets".to_string()),
            },
            BinaryOp::Difference => match (lhs, rhs) {
                (Value::Set(a), Value::Set(b)) => {
                    let result = a.into_iter().filter(|v| !b.contains(v)).collect();
                    Ok(Value::Set(result))
                }
                _ => Err("difference requires sets".to_string()),
            },
        }
    }

    fn eval_unary(op: UnaryOp, value: &Value) -> Result<Value, String> {
        match op {
            UnaryOp::Not => match value {
                Value::Bool(b) => Ok(Value::Bool(!*b)),
                _ => Err("not requires a boolean".to_string()),
            },
            UnaryOp::Neg => match value {
                Value::Int(n) => Ok(Value::Int(-*n)),
                _ => Err("- requires an integer".to_string()),
            },
        }
    }

    fn eval_field(base: Value, field: &str) -> Result<Value, String> {
        match base {
            Value::Struct { fields, .. } => fields
                .get(field)
                .cloned()
                .ok_or_else(|| format!("No field {field}")),
            Value::Tuple(items) => {
                let idx: usize = field
                    .parse()
                    .map_err(|_| format!("Invalid tuple index: {field}"))?;
                items
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| format!("Tuple index {idx} out of bounds"))
            }
            _ => Err(format!("Cannot access field on {base:?}")),
        }
    }

    fn eval_index(base: Value, index: &Value) -> Result<Value, String> {
        match base {
            Value::List(items) => {
                if let Value::Int(i) = index {
                    let idx = usize::try_from(*i).map_err(|_| "Invalid index")?;
                    items
                        .get(idx)
                        .cloned()
                        .ok_or_else(|| format!("Index {idx} out of bounds"))
                } else {
                    Err("List index must be an integer".to_string())
                }
            }
            Value::Map(map) => index
                .get_from_map(&map)
                .cloned()
                .ok_or_else(|| format!("Key not found: {index}")),
            _ => Err(format!("Cannot index {base:?}")),
        }
    }

    fn eval_builtin(name: &str, args: &[Value]) -> Result<Value, String> {
        match name {
            "len" => {
                if args.len() != 1 {
                    return Err("len takes 1 argument".to_string());
                }
                match &args[0] {
                    Value::Set(s) => Ok(Value::Int(i64::try_from(s.len()).unwrap_or(i64::MAX))),
                    Value::List(l) => Ok(Value::Int(i64::try_from(l.len()).unwrap_or(i64::MAX))),
                    Value::Map(m) => Ok(Value::Int(i64::try_from(m.len()).unwrap_or(i64::MAX))),
                    Value::String(s) => Ok(Value::Int(i64::try_from(s.len()).unwrap_or(i64::MAX))),
                    _ => Err("len requires a collection".to_string()),
                }
            }
            _ => Err(format!("Unknown function: {name}")),
        }
    }

    fn eval_method(receiver: Value, method: &str, args: &[Value]) -> Result<Value, String> {
        match method {
            "len" => match receiver {
                Value::Set(s) => Ok(Value::Int(i64::try_from(s.len()).unwrap_or(i64::MAX))),
                Value::List(l) => Ok(Value::Int(i64::try_from(l.len()).unwrap_or(i64::MAX))),
                Value::Map(m) => Ok(Value::Int(i64::try_from(m.len()).unwrap_or(i64::MAX))),
                Value::String(s) => Ok(Value::Int(i64::try_from(s.len()).unwrap_or(i64::MAX))),
                _ => Err("len requires a collection".to_string()),
            },
            "is_empty" => match receiver {
                Value::Set(s) => Ok(Value::Bool(s.is_empty())),
                Value::List(l) => Ok(Value::Bool(l.is_empty())),
                Value::Map(m) => Ok(Value::Bool(m.is_empty())),
                _ => Err("is_empty requires a collection".to_string()),
            },
            "contains" => {
                if args.len() != 1 {
                    return Err("contains takes 1 argument".to_string());
                }
                match receiver {
                    Value::Set(s) => Ok(Value::Bool(s.contains(&args[0]))),
                    Value::List(l) => Ok(Value::Bool(l.contains(&args[0]))),
                    Value::Map(m) => Ok(Value::Bool(args[0].is_in_map(&m))),
                    _ => Err("contains requires a collection".to_string()),
                }
            }
            "map" | "filter" | "first" | "last" => {
                // These would need more sophisticated evaluation
                Err(format!("Method {method} not yet fully implemented"))
            }
            "unwrap" => match receiver {
                Value::Option(Some(v)) | Value::Result(Ok(v)) => Ok(*v),
                Value::Option(None) => Err("unwrap called on None".to_string()),
                Value::Result(Err(_)) => Err("unwrap called on Err".to_string()),
                _ => Err("unwrap requires Option or Result".to_string()),
            },
            "is_some" => match receiver {
                Value::Option(opt) => Ok(Value::Bool(opt.is_some())),
                _ => Err("is_some requires Option".to_string()),
            },
            "is_none" => match receiver {
                Value::Option(opt) => Ok(Value::Bool(opt.is_none())),
                _ => Err("is_none requires Option".to_string()),
            },
            "is_ok" => match receiver {
                Value::Result(res) => Ok(Value::Bool(res.is_ok())),
                _ => Err("is_ok requires Result".to_string()),
            },
            "is_err" => match receiver {
                Value::Result(res) => Ok(Value::Bool(res.is_err())),
                _ => Err("is_err requires Result".to_string()),
            },
            _ => Err(format!("Unknown method: {method}")),
        }
    }
}
