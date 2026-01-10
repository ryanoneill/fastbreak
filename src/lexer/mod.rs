//! Lexical analysis for Fastbreak specifications
//!
//! This module provides tokenization of `.fbs` source files using the logos crate.

mod token;

use logos::Logos;
pub use token::Token;

use crate::Span;

/// A token with its source location
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    /// The token
    pub token: Token,
    /// Source location
    pub span: Span,
}

impl SpannedToken {
    /// Create a new spanned token
    #[must_use]
    pub const fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
}

/// Lexer for Fastbreak source code
pub struct Lexer<'src> {
    inner: logos::Lexer<'src, Token>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer for the given source
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        Self {
            inner: Token::lexer(source),
        }
    }

    /// Get the remaining source text
    #[must_use]
    pub fn remainder(&self) -> &'src str {
        self.inner.remainder()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<SpannedToken, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.inner.next()?;
        let span = Span::from(self.inner.span());

        Some(match result {
            Ok(token) => Ok(SpannedToken::new(token, span)),
            Err(()) => Err(LexError { span }),
        })
    }
}

/// Error during lexical analysis
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    /// Location of the error
    pub span: Span,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "unexpected character at position {}..{}",
            self.span.start, self.span.end
        )
    }
}

impl std::error::Error for LexError {}

/// Tokenize source code into a vector of tokens
///
/// # Errors
///
/// Returns an error if the source contains invalid tokens
pub fn tokenize(source: &str) -> Result<Vec<SpannedToken>, LexError> {
    Lexer::new(source).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let source = "module auth";
        let tokens: Vec<_> = Lexer::new(source)
            .filter_map(Result::ok)
            .map(|st| st.token)
            .collect();

        assert_eq!(tokens, vec![Token::Module, Token::Ident("auth".into())]);
    }

    #[test]
    fn test_lexer_type_definition() {
        let source = "type User { id: UserId, }";
        let tokens: Vec<_> = Lexer::new(source)
            .filter_map(Result::ok)
            .map(|st| st.token)
            .collect();

        assert_eq!(
            tokens,
            vec![
                Token::Type,
                Token::Ident("User".into()),
                Token::LBrace,
                Token::Ident("id".into()),
                Token::Colon,
                Token::Ident("UserId".into()),
                Token::Comma,
                Token::RBrace,
            ]
        );
    }
}
