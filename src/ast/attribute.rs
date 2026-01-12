//! Attribute AST nodes for metadata and traceability

use super::Ident;
use crate::Span;
use smol_str::SmolStr;

/// An attribute annotation: `@name` or `@name(args)`
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    /// Attribute name
    pub name: Ident,
    /// Arguments (if any)
    pub args: Vec<AttributeArg>,
    /// Span of the entire attribute
    pub span: Span,
}

/// An attribute argument
#[derive(Debug, Clone, PartialEq)]
pub enum AttributeArg {
    /// String literal: `"explanation"`
    String(SmolStr, Span),
    /// Identifier: `security_team`
    Ident(Ident),
    /// Integer literal: `100`
    Int(i64, Span),
}

impl AttributeArg {
    /// Get the span of this argument
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            AttributeArg::Ident(ident) => ident.span,
            AttributeArg::String(_, span) | AttributeArg::Int(_, span) => *span,
        }
    }
}

impl std::fmt::Display for AttributeArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeArg::String(s, _) => write!(f, "\"{s}\""),
            AttributeArg::Ident(ident) => write!(f, "{ident}"),
            AttributeArg::Int(n, _) => write!(f, "{n}"),
        }
    }
}

impl std::fmt::Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.name)?;
        if !self.args.is_empty() {
            write!(f, "(")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{arg}")?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}
