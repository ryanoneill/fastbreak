//! Expression AST nodes

use super::{Ident, Path, TypeRef};
use crate::Span;
use smol_str::SmolStr;

/// An expression with source location
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    /// The expression kind
    pub kind: ExprKind,
    /// Source location
    pub span: Span,
}

impl Expr {
    /// Create a new expression
    #[must_use]
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Expression kinds
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// A literal value
    Literal(Literal),

    /// A variable or path reference: `x`, `user.email`
    Var(Path),

    /// A primed variable (next state): `users'`
    Prime(Box<Expr>),

    /// Binary operation: `a + b`, `a and b`
    Binary {
        /// Left operand
        left: Box<Expr>,
        /// Operator
        op: BinaryOp,
        /// Right operand
        right: Box<Expr>,
    },

    /// Unary operation: `not x`, `-x`
    Unary {
        /// Operator
        op: UnaryOp,
        /// Operand
        expr: Box<Expr>,
    },

    /// Field access: `user.email`
    Field {
        /// Base expression
        base: Box<Expr>,
        /// Field name
        field: Ident,
    },

    /// Index access: `users[0]`, `sessions[sid]`
    Index {
        /// Base expression
        base: Box<Expr>,
        /// Index expression
        index: Box<Expr>,
    },

    /// Function/method call: `users.len()`, `hash(password)`
    Call {
        /// Function/method expression
        func: Box<Expr>,
        /// Arguments
        args: Vec<Expr>,
    },

    /// Method call with receiver: `users.map(u => u.id)`
    MethodCall {
        /// Receiver expression
        receiver: Box<Expr>,
        /// Method name
        method: Ident,
        /// Arguments
        args: Vec<Expr>,
    },

    /// Universal quantifier: `forall x in xs => pred(x)`
    Forall {
        /// Bound variables
        bindings: Vec<QuantBinding>,
        /// Optional filter
        filter: Option<Box<Expr>>,
        /// Body expression
        body: Box<Expr>,
    },

    /// Existential quantifier: `exists x in xs => pred(x)`
    Exists {
        /// Bound variables
        bindings: Vec<QuantBinding>,
        /// Optional filter
        filter: Option<Box<Expr>>,
        /// Body expression
        body: Box<Expr>,
    },

    /// Implication: `a implies b`
    Implies {
        /// Antecedent
        antecedent: Box<Expr>,
        /// Consequent
        consequent: Box<Expr>,
    },

    /// Set literal: `{1, 2, 3}` or `{}`
    Set(Vec<Expr>),

    /// List literal: `[1, 2, 3]`
    List(Vec<Expr>),

    /// Map literal: `{a: 1, b: 2}`
    Map(Vec<(Expr, Expr)>),

    /// Tuple: `(a, b)`
    Tuple(Vec<Expr>),

    /// Struct/record literal: `User { id: 1, name: "Alice" }`
    Struct {
        /// Type name
        ty: Path,
        /// Field values
        fields: Vec<FieldInit>,
    },

    /// Lambda expression: `x => x + 1`, `(a, b) => a + b`
    Lambda {
        /// Parameters
        params: Vec<LambdaParam>,
        /// Body expression
        body: Box<Expr>,
    },

    /// If expression: `if cond { a } else { b }`
    If {
        /// Condition
        condition: Box<Expr>,
        /// Then branch
        then_branch: Box<Expr>,
        /// Else branch (optional for statements, required for expressions)
        else_branch: Option<Box<Expr>>,
    },

    /// Match expression
    Match {
        /// Expression to match
        expr: Box<Expr>,
        /// Match arms
        arms: Vec<MatchArm>,
    },

    /// Let binding: `let x = 1 in x + 1`
    Let {
        /// Variable name
        name: Ident,
        /// Optional type annotation
        ty: Option<TypeRef>,
        /// Value expression
        value: Box<Expr>,
        /// Body expression
        body: Box<Expr>,
    },

    /// Type check: `x is Ok`
    Is {
        /// Expression to check
        expr: Box<Expr>,
        /// Type or variant to check against
        ty: Path,
    },

    /// Block expression: `{ stmt1; stmt2; expr }`
    Block(Vec<Expr>),

    /// Range: `1..10`
    Range {
        /// Start (inclusive)
        start: Box<Expr>,
        /// End (exclusive)
        end: Box<Expr>,
    },

    /// Old value (in postconditions): `old(users)`
    Old(Box<Expr>),

    /// Result value (in postconditions)
    Result,

    /// Self reference (in refinement predicates): `self`
    SelfRef,
}

/// A literal value
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal
    Int(i64),
    /// String literal
    String(SmolStr),
    /// Boolean literal
    Bool(bool),
    /// Unit/empty tuple
    Unit,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Arithmetic
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    // Comparison
    /// `==`
    Eq,
    /// `!=`
    NotEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,

    // Logical
    /// `and` or `&&`
    And,
    /// `or` or `||`
    Or,

    // Set operations
    /// `in` - membership test
    In,
    /// `union`
    Union,
    /// `intersect`
    Intersect,
    /// `difference`
    Difference,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::LtEq => write!(f, "<="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::GtEq => write!(f, ">="),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::In => write!(f, "in"),
            BinaryOp::Union => write!(f, "union"),
            BinaryOp::Intersect => write!(f, "intersect"),
            BinaryOp::Difference => write!(f, "difference"),
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `not` or `!`
    Not,
    /// `-` (negation)
    Neg,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "not"),
            UnaryOp::Neg => write!(f, "-"),
        }
    }
}

/// The kind of quantifier binding
#[derive(Debug, Clone, PartialEq)]
pub enum QuantBindingKind {
    /// Binding over a collection: `x in collection`
    InCollection(Expr),
    /// Typed binding: `x: Type` (quantifies over all values of the type)
    Typed(TypeRef),
}

/// A binding in a quantifier
#[derive(Debug, Clone, PartialEq)]
pub struct QuantBinding {
    /// Variable name
    pub name: Ident,
    /// The binding kind (collection or typed)
    pub kind: QuantBindingKind,
    /// Span of the binding
    pub span: Span,
}

/// Field initialization in a struct literal
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    /// Field name
    pub name: Ident,
    /// Value expression
    pub value: Expr,
    /// Span of the initialization
    pub span: Span,
}

/// Lambda parameter
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaParam {
    /// Parameter name
    pub name: Ident,
    /// Optional type annotation
    pub ty: Option<TypeRef>,
}

/// A match arm
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    /// Pattern to match
    pub pattern: Pattern,
    /// Optional guard
    pub guard: Option<Expr>,
    /// Body expression
    pub body: Expr,
    /// Span of the arm
    pub span: Span,
}

/// A pattern for matching
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    /// Pattern kind
    pub kind: PatternKind,
    /// Span of the pattern
    pub span: Span,
}

/// Pattern kinds
#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    /// Wildcard pattern: `_`
    Wildcard,

    /// Variable binding: `x`
    Binding(Ident),

    /// Literal pattern: `42`, `"hello"`
    Literal(Literal),

    /// Tuple pattern: `(a, b)`
    Tuple(Vec<Pattern>),

    /// Struct pattern: `User { id, name }`
    Struct {
        /// Type name
        ty: Path,
        /// Field patterns
        fields: Vec<FieldPattern>,
        /// Rest pattern (`..`)
        rest: bool,
    },

    /// Enum variant pattern: `Ok(value)`, `None`
    Variant {
        /// Path to variant
        path: Path,
        /// Inner patterns (if any)
        patterns: Vec<Pattern>,
    },

    /// Or pattern: `A | B`
    Or(Vec<Pattern>),
}

/// A field pattern in a struct pattern
#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    /// Field name
    pub name: Ident,
    /// Pattern for the field (if different from name)
    pub pattern: Option<Pattern>,
    /// Span
    pub span: Span,
}
