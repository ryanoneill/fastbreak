//! Quality requirement AST nodes (Non-Functional Requirements)

use super::{Attribute, Expr, Ident};
use crate::Span;
use smol_str::SmolStr;

/// A quality requirement definition
///
/// ```fbrk
/// quality performance "API response time" {
///     metric: latency,
///     scale: p99,
///     target: < 100ms,
///     constraint: hard,
///     applies_to: action register,
///     measurement: per_request,
///     under_load: {
///         concurrent_users: 1000,
///         requests_per_second: 500,
///     },
///     verified_by: [
///         test "load_test_api",
///         monitor "datadog_latency",
///     ],
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
    /// Statistical scale (e.g., p99, mean)
    pub scale: Option<Scale>,
    /// Target value expression
    pub target: QualityTarget,
    /// Hard constraint vs soft target
    pub constraint: Option<Constraint>,
    /// What this quality applies to
    pub applies_to: Option<AppliesTo>,
    /// Measurement period
    pub measurement: Option<MeasurementPeriod>,
    /// Load conditions for the measurement
    pub under_load: Option<LoadConditions>,
    /// Verification methods
    pub verified_by: Vec<VerificationMethod>,
    /// Additional properties (for extensibility)
    pub properties: Vec<QualityProperty>,
    /// Span of the entire quality block
    pub span: Span,
}

/// Statistical scale for measurements
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scale {
    /// Arithmetic mean
    Mean,
    /// Median (50th percentile)
    Median,
    /// 50th percentile
    P50,
    /// 90th percentile
    P90,
    /// 95th percentile
    P95,
    /// 99th percentile
    P99,
    /// 99.9th percentile
    P999,
    /// Maximum value
    Max,
    /// Minimum value
    Min,
}

impl std::fmt::Display for Scale {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Scale::Mean => write!(f, "mean"),
            Scale::Median => write!(f, "median"),
            Scale::P50 => write!(f, "p50"),
            Scale::P90 => write!(f, "p90"),
            Scale::P95 => write!(f, "p95"),
            Scale::P99 => write!(f, "p99"),
            Scale::P999 => write!(f, "p999"),
            Scale::Max => write!(f, "max"),
            Scale::Min => write!(f, "min"),
        }
    }
}

/// Whether a quality target is a hard constraint or soft target
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Constraint {
    /// Must be met - failure is unacceptable
    Hard,
    /// Goal/target - best effort
    Soft,
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Hard => write!(f, "hard"),
            Constraint::Soft => write!(f, "soft"),
        }
    }
}

/// Measurement period for quality metrics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeasurementPeriod {
    /// Measured per individual request
    PerRequest,
    /// Measured per second
    PerSecond,
    /// Measured per minute
    PerMinute,
    /// Measured hourly
    Hourly,
    /// Measured daily
    Daily,
    /// Measured weekly
    Weekly,
    /// Measured monthly
    Monthly,
}

impl std::fmt::Display for MeasurementPeriod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MeasurementPeriod::PerRequest => write!(f, "per_request"),
            MeasurementPeriod::PerSecond => write!(f, "per_second"),
            MeasurementPeriod::PerMinute => write!(f, "per_minute"),
            MeasurementPeriod::Hourly => write!(f, "hourly"),
            MeasurementPeriod::Daily => write!(f, "daily"),
            MeasurementPeriod::Weekly => write!(f, "weekly"),
            MeasurementPeriod::Monthly => write!(f, "monthly"),
        }
    }
}

/// Load conditions under which the quality is measured
#[derive(Debug, Clone, PartialEq)]
pub struct LoadConditions {
    /// Number of concurrent users
    pub concurrent_users: Option<i64>,
    /// Number of concurrent connections
    pub concurrent_connections: Option<i64>,
    /// Requests per second
    pub requests_per_second: Option<i64>,
    /// Payload size
    pub payload_size: Option<(i64, SizeUnit)>,
    /// Duration of sustained load
    pub duration: Option<(i64, DurationUnit)>,
    /// Span
    pub span: Span,
}

/// Verification method for a quality requirement
#[derive(Debug, Clone, PartialEq)]
pub struct VerificationMethod {
    /// Kind of verification
    pub kind: VerificationKind,
    /// Name/identifier of the verification
    pub name: SmolStr,
    /// Span
    pub span: Span,
}

/// Kind of verification method
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerificationKind {
    /// Automated test
    Test,
    /// Monitoring/observability
    Monitor,
    /// Performance benchmark
    Benchmark,
    /// Manual audit process
    Audit,
}

impl std::fmt::Display for VerificationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerificationKind::Test => write!(f, "test"),
            VerificationKind::Monitor => write!(f, "monitor"),
            VerificationKind::Benchmark => write!(f, "benchmark"),
            VerificationKind::Audit => write!(f, "audit"),
        }
    }
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
