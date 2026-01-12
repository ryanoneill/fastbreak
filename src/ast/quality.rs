//! Quality requirement AST nodes (Non-Functional Requirements)

use super::{Attribute, Expr, Ident};
use crate::Span;
use smol_str::SmolStr;

/// A quality requirement definition
///
/// ```fbs
/// quality performance "API response time" {
///     metric: latency,
///     target: < 200ms,
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Quality {
    /// Attributes (e.g., `@id(NFR-001)`)
    pub attributes: Vec<Attribute>,
    /// Quality category
    pub category: QualityCategory,
    /// Description/name of the quality requirement
    pub description: SmolStr,
    /// Metric being measured
    pub metric: Ident,
    /// Target value expression
    pub target: QualityTarget,
    /// Additional properties
    pub properties: Vec<QualityProperty>,
    /// Span of the entire quality block
    pub span: Span,
}

/// Category of quality requirement
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QualityCategory {
    /// Performance requirements (latency, throughput)
    Performance,
    /// Reliability requirements (uptime, MTBF)
    Reliability,
    /// Security requirements (encryption, authentication)
    Security,
    /// Usability requirements (accessibility, learnability)
    Usability,
    /// Scalability requirements (capacity, elasticity)
    Scalability,
    /// Maintainability requirements (testability, modularity)
    Maintainability,
}

impl std::fmt::Display for QualityCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QualityCategory::Performance => write!(f, "performance"),
            QualityCategory::Reliability => write!(f, "reliability"),
            QualityCategory::Security => write!(f, "security"),
            QualityCategory::Usability => write!(f, "usability"),
            QualityCategory::Scalability => write!(f, "scalability"),
            QualityCategory::Maintainability => write!(f, "maintainability"),
        }
    }
}

/// A quality target expression
#[derive(Debug, Clone, PartialEq)]
pub struct QualityTarget {
    /// Comparison operator
    pub op: QualityOp,
    /// Target value
    pub value: QualityValue,
    /// Span of the target
    pub span: Span,
}

/// Comparison operator for quality targets
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QualityOp {
    /// Less than
    Lt,
    /// Less than or equal
    LtEq,
    /// Greater than
    Gt,
    /// Greater than or equal
    GtEq,
    /// Equal
    Eq,
}

impl std::fmt::Display for QualityOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QualityOp::Lt => write!(f, "<"),
            QualityOp::LtEq => write!(f, "<="),
            QualityOp::Gt => write!(f, ">"),
            QualityOp::GtEq => write!(f, ">="),
            QualityOp::Eq => write!(f, "=="),
        }
    }
}

/// A quality value with optional unit
#[derive(Debug, Clone, PartialEq)]
pub enum QualityValue {
    /// Plain integer
    Int(i64),
    /// Percentage (e.g., 99.9%)
    Percentage(f64),
    /// Duration in milliseconds
    Duration(i64, DurationUnit),
    /// Data size in bytes
    Size(i64, SizeUnit),
    /// Rate (e.g., 1000/s)
    Rate(i64, RateUnit),
    /// Reference to an expression
    Expr(Expr),
}

/// Duration unit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DurationUnit {
    /// Milliseconds
    Ms,
    /// Seconds
    S,
    /// Minutes
    M,
    /// Hours
    H,
}

impl std::fmt::Display for DurationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DurationUnit::Ms => write!(f, "ms"),
            DurationUnit::S => write!(f, "s"),
            DurationUnit::M => write!(f, "m"),
            DurationUnit::H => write!(f, "h"),
        }
    }
}

/// Size unit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizeUnit {
    /// Kilobytes
    Kb,
    /// Megabytes
    Mb,
    /// Gigabytes
    Gb,
    /// Terabytes
    Tb,
}

impl std::fmt::Display for SizeUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SizeUnit::Kb => write!(f, "kb"),
            SizeUnit::Mb => write!(f, "mb"),
            SizeUnit::Gb => write!(f, "gb"),
            SizeUnit::Tb => write!(f, "tb"),
        }
    }
}

/// Rate unit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RateUnit {
    /// Per second
    PerSecond,
    /// Per minute
    PerMinute,
    /// Per hour
    PerHour,
}

impl std::fmt::Display for RateUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RateUnit::PerSecond => write!(f, "/s"),
            RateUnit::PerMinute => write!(f, "/m"),
            RateUnit::PerHour => write!(f, "/h"),
        }
    }
}

/// Additional property in a quality block
#[derive(Debug, Clone, PartialEq)]
pub struct QualityProperty {
    /// Property name (e.g., "scale", "constraint", "`applies_to`")
    pub name: Ident,
    /// Property value
    pub value: QualityPropertyValue,
    /// Span of the property
    pub span: Span,
}

/// Value of a quality property
#[derive(Debug, Clone, PartialEq)]
pub enum QualityPropertyValue {
    /// Identifier value (e.g., scale: p95)
    Ident(Ident),
    /// Target value (e.g., constraint: < 500ms)
    Target(QualityTarget),
    /// Applies to reference (e.g., `applies_to`: action `create_user`)
    AppliesTo(AppliesTo),
}

/// Reference to what a quality applies to
#[derive(Debug, Clone, PartialEq)]
pub struct AppliesTo {
    /// Kind of target
    pub kind: AppliesToKind,
    /// Name of the target
    pub name: Ident,
    /// Span
    pub span: Span,
}

/// Kind of `applies_to` target
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppliesToKind {
    /// Applies to an action
    Action,
    /// Applies to a type
    Type,
    /// Applies to a state
    State,
}

impl std::fmt::Display for AppliesToKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AppliesToKind::Action => write!(f, "action"),
            AppliesToKind::Type => write!(f, "type"),
            AppliesToKind::State => write!(f, "state"),
        }
    }
}
