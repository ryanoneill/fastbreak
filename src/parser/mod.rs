//! Parser for Fastbreak specifications
//!
//! This module implements a recursive descent parser that transforms
//! a stream of tokens into an AST.

mod error;

pub use error::{ParseError, ParseResult};

use crate::ast::{
    Action, ActionParam, Assertion, Attribute, AttributeArg, BinaryOp, Binding, BuiltInType,
    Contract, ContractKind, EnumDef, EnumVariant, Expr, ExprKind, Field, FieldInit, FieldPattern,
    GenericArg, GivenClause, Ident, Import, ImportItem, Invariant, LambdaParam, Literal, MatchArm,
    Module, Path, Pattern, PatternKind, Property, QuantBinding, Relation, RelationConstraint,
    Scenario, Specification, StateBlock, StateField, TemporalOp, ThenClause, TypeDef, TypeRef,
    TypeRefKind, UnaryOp, WhenClause,
};
use crate::lexer::{Lexer, SpannedToken, Token};
use crate::Span;

/// Parser for Fastbreak specifications
pub struct Parser<'src> {
    /// Token stream
    tokens: Vec<SpannedToken>,
    /// Current position in the token stream
    pos: usize,
    /// Source code (for error reporting)
    source: &'src str,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source
    ///
    /// # Errors
    ///
    /// Returns an error if lexing fails
    pub fn new(source: &'src str) -> ParseResult<Self> {
        let mut tokens = Vec::new();
        for result in Lexer::new(source) {
            match result {
                Ok(token) => tokens.push(token),
                Err(lex_error) => {
                    return Err(ParseError::InvalidToken { span: lex_error.span });
                }
            }
        }
        Ok(Self {
            tokens,
            pos: 0,
            source,
        })
    }

    /// Parse a complete specification
    ///
    /// # Errors
    ///
    /// Returns an error if parsing fails
    pub fn parse(&mut self) -> ParseResult<Specification> {
        let mut spec = Specification::new();

        // Parse module declaration if present
        if self.check(&Token::Module) {
            spec.module = Some(self.parse_module()?);
        }

        // Parse imports
        while self.check(&Token::Use) {
            spec.imports.push(self.parse_import()?);
        }

        // Parse top-level items (with optional preceding attributes)
        while !self.is_at_end() {
            // Parse any attributes before the item
            let attributes = self.parse_attributes()?;

            match self.peek() {
                Some(Token::Type) => spec.types.push(self.parse_type_def_with_attrs(attributes)?),
                Some(Token::Enum) => spec.enums.push(self.parse_enum_def_with_attrs(attributes)?),
                Some(Token::Relation) => {
                    spec.relations.push(self.parse_relation_with_attrs(attributes)?);
                }
                Some(Token::State) => {
                    spec.states.push(self.parse_state_block_with_attrs(attributes)?);
                }
                Some(Token::Action) => spec.actions.push(self.parse_action_with_attrs(attributes)?),
                Some(Token::Scenario) => {
                    spec.scenarios.push(self.parse_scenario_with_attrs(attributes)?);
                }
                Some(Token::Property) => {
                    spec.properties.push(self.parse_property_with_attrs(attributes)?);
                }
                Some(_) => {
                    let (token, span) = self.advance()?;
                    return Err(ParseError::unexpected(
                        "type, enum, state, action, scenario, or property",
                        &token,
                        span,
                    ));
                }
                None => break,
            }
        }

        Ok(spec)
    }

    // ========== Attributes ==========

    /// Parse zero or more attributes: `@id(REQ-001) @rationale("...")`
    fn parse_attributes(&mut self) -> ParseResult<Vec<Attribute>> {
        let mut attributes = Vec::new();
        while self.check(&Token::At) {
            attributes.push(self.parse_attribute()?);
        }
        Ok(attributes)
    }

    /// Parse a single attribute: `@name` or `@name(args)`
    fn parse_attribute(&mut self) -> ParseResult<Attribute> {
        let start = self.expect(&Token::At)?;
        let name = self.parse_ident()?;

        let args = if self.check(&Token::LParen) {
            self.advance()?;
            let args = self.parse_attribute_args()?;
            self.expect(&Token::RParen)?;
            args
        } else {
            Vec::new()
        };

        let end_span = if args.is_empty() {
            name.span
        } else {
            self.previous_span()
        };

        Ok(Attribute {
            name,
            args,
            span: start.merge(end_span),
        })
    }

    /// Parse attribute arguments: `"string", ident, 123`
    fn parse_attribute_args(&mut self) -> ParseResult<Vec<AttributeArg>> {
        let mut args = Vec::new();

        while !self.check(&Token::RParen) {
            let arg = match self.peek() {
                Some(Token::String(s)) => {
                    let s = s.clone();
                    let span = self.current_span();
                    self.advance()?;
                    AttributeArg::String(s, span)
                }
                Some(Token::Integer(n)) => {
                    let n = *n;
                    let span = self.current_span();
                    self.advance()?;
                    AttributeArg::Int(n, span)
                }
                Some(Token::Ident(_)) => {
                    let ident = self.parse_ident()?;
                    AttributeArg::Ident(ident)
                }
                Some(token) => {
                    let token = token.clone();
                    let span = self.current_span();
                    return Err(ParseError::unexpected(
                        "attribute argument (string, identifier, or integer)",
                        &token,
                        span,
                    ));
                }
                None => return Err(ParseError::unexpected_eof("attribute argument", self.eof_span())),
            };
            args.push(arg);

            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }

        Ok(args)
    }

    // ========== Module and Imports ==========

    fn parse_module(&mut self) -> ParseResult<Module> {
        let start = self.expect(&Token::Module)?;
        let name = self.parse_ident()?;
        let span = start.merge(name.span);
        Ok(Module { name, span })
    }

    fn parse_import(&mut self) -> ParseResult<Import> {
        let start = self.expect(&Token::Use)?;
        let path = self.parse_path()?;

        let items = if self.check(&Token::ColonColon) && self.check_ahead(1, &Token::LBrace) {
            self.advance()?; // ::
            self.advance()?; // {
            let items = self.parse_comma_separated(Self::parse_import_item, &Token::RBrace)?;
            self.expect(&Token::RBrace)?;
            Some(items)
        } else {
            None
        };

        let end_span = items
            .as_ref()
            .and_then(|i| i.last())
            .map_or(path.span, |item| item.name.span);

        Ok(Import {
            path,
            items,
            span: start.merge(end_span),
        })
    }

    fn parse_import_item(&mut self) -> ParseResult<ImportItem> {
        let name = self.parse_ident()?;
        let alias = if self.check_ident("as") {
            self.advance()?;
            Some(self.parse_ident()?)
        } else {
            None
        };
        Ok(ImportItem { name, alias })
    }

    // ========== Type Definitions ==========

    fn parse_type_def_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<TypeDef> {
        let start = self.expect(&Token::Type)?;
        let name = self.parse_ident()?;

        // Optional type parameters
        let type_params = if self.check(&Token::LAngle) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };

        self.expect(&Token::LBrace)?;
        let fields = self.parse_comma_separated(Self::parse_field, &Token::RBrace)?;
        let end = self.expect(&Token::RBrace)?;

        Ok(TypeDef {
            attributes,
            name,
            type_params,
            fields,
            span: start.merge(end),
        })
    }

    fn parse_field(&mut self) -> ParseResult<Field> {
        let name = self.parse_ident()?;
        self.expect(&Token::Colon)?;
        let ty = self.parse_type_ref()?;
        let span = name.span.merge(ty.span);
        Ok(Field { name, ty, span })
    }

    fn parse_type_params(&mut self) -> ParseResult<Vec<Ident>> {
        self.expect(&Token::LAngle)?;
        let params = self.parse_comma_separated(Self::parse_ident, &Token::RAngle)?;
        self.expect(&Token::RAngle)?;
        Ok(params)
    }

    fn parse_enum_def_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<EnumDef> {
        let start = self.expect(&Token::Enum)?;
        let name = self.parse_ident()?;

        let type_params = if self.check(&Token::LAngle) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };

        self.expect(&Token::LBrace)?;
        let variants = self.parse_comma_separated(Self::parse_enum_variant, &Token::RBrace)?;
        let end = self.expect(&Token::RBrace)?;

        Ok(EnumDef {
            attributes,
            name,
            type_params,
            variants,
            span: start.merge(end),
        })
    }

    fn parse_enum_variant(&mut self) -> ParseResult<EnumVariant> {
        let name = self.parse_ident()?;

        let fields = if self.check(&Token::LBrace) {
            self.advance()?;
            let fields = self.parse_comma_separated(Self::parse_field, &Token::RBrace)?;
            self.expect(&Token::RBrace)?;
            fields
        } else if self.check(&Token::LParen) {
            self.advance()?;
            let mut fields = Vec::new();
            let mut idx = 0;
            while !self.check(&Token::RParen) {
                let ty = self.parse_type_ref()?;
                let field_span = ty.span;
                fields.push(Field {
                    name: Ident::new(format!("{idx}"), field_span),
                    ty,
                    span: field_span,
                });
                idx += 1;
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;
            fields
        } else {
            Vec::new()
        };

        let span = if fields.is_empty() {
            name.span
        } else {
            name.span.merge(fields.last().unwrap().span)
        };

        Ok(EnumVariant { name, fields, span })
    }

    // ========== Type References ==========

    fn parse_type_ref(&mut self) -> ParseResult<TypeRef> {
        let base = self.parse_primary_type()?;

        // Check for generic arguments
        if self.check(&Token::LAngle) {
            self.parse_generic_type(base)
        } else {
            Ok(base)
        }
    }

    fn parse_primary_type(&mut self) -> ParseResult<TypeRef> {
        match self.peek() {
            Some(Token::LParen) => self.parse_tuple_or_unit_type(),
            Some(Token::SetType) => self.parse_builtin_type(BuiltInType::Set),
            Some(Token::MapType) => self.parse_builtin_type(BuiltInType::Map),
            Some(Token::ListType) => self.parse_builtin_type(BuiltInType::List),
            Some(Token::OptionType) => self.parse_builtin_type(BuiltInType::Option),
            Some(Token::ResultType) => self.parse_builtin_type(BuiltInType::Result),
            Some(Token::StringType) => self.parse_builtin_type(BuiltInType::String),
            Some(Token::IntType) => self.parse_builtin_type(BuiltInType::Int),
            Some(Token::BoolType) => self.parse_builtin_type(BuiltInType::Bool),
            Some(Token::Ident(_)) => {
                let path = self.parse_path()?;
                let span = path.span;
                Ok(TypeRef {
                    kind: TypeRefKind::Named(path),
                    span,
                })
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::expected_type(&token, span))
            }
            None => Err(ParseError::unexpected_eof("type", self.eof_span())),
        }
    }

    fn parse_builtin_type(&mut self, builtin: BuiltInType) -> ParseResult<TypeRef> {
        let (_, span) = self.advance()?;
        Ok(TypeRef {
            kind: TypeRefKind::BuiltIn(builtin),
            span,
        })
    }

    fn parse_tuple_or_unit_type(&mut self) -> ParseResult<TypeRef> {
        let start = self.expect(&Token::LParen)?;

        if self.check(&Token::RParen) {
            let end = self.expect(&Token::RParen)?;
            return Ok(TypeRef {
                kind: TypeRefKind::Unit,
                span: start.merge(end),
            });
        }

        let first = self.parse_type_ref()?;

        // Check for function type: (A, B) -> C
        if self.check(&Token::Comma) || self.check(&Token::RParen) {
            let mut types = vec![first];
            while self.check(&Token::Comma) {
                self.advance()?;
                if self.check(&Token::RParen) {
                    break;
                }
                types.push(self.parse_type_ref()?);
            }
            let end = self.expect(&Token::RParen)?;

            if self.check(&Token::Arrow) {
                self.advance()?;
                let ret = self.parse_type_ref()?;
                let span = start.merge(ret.span);
                return Ok(TypeRef {
                    kind: TypeRefKind::Function {
                        params: types,
                        ret: Box::new(ret),
                    },
                    span,
                });
            }

            if types.len() == 1 {
                // Just parenthesized type
                return Ok(types.into_iter().next().unwrap());
            }

            return Ok(TypeRef {
                kind: TypeRefKind::Tuple(types),
                span: start.merge(end),
            });
        }

        let end = self.expect(&Token::RParen)?;
        Ok(TypeRef {
            kind: TypeRefKind::Tuple(vec![first]),
            span: start.merge(end),
        })
    }

    fn parse_generic_type(&mut self, base: TypeRef) -> ParseResult<TypeRef> {
        let base_span = base.span;
        self.expect(&Token::LAngle)?;
        let args = self.parse_comma_separated(Self::parse_generic_arg, &Token::RAngle)?;
        let end = self.expect(&Token::RAngle)?;

        Ok(TypeRef {
            kind: TypeRefKind::Generic {
                base: Box::new(base),
                args,
            },
            span: base_span.merge(end),
        })
    }

    fn parse_generic_arg(&mut self) -> ParseResult<GenericArg> {
        let ty = self.parse_type_ref()?;
        Ok(GenericArg { ty })
    }

    // ========== Relations ==========

    fn parse_relation_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<Relation> {
        let start = self.expect(&Token::Relation)?;
        let name = self.parse_ident()?;
        self.expect(&Token::Colon)?;
        let source = self.parse_type_ref()?;
        self.expect(&Token::Arrow)?;
        let target = self.parse_type_ref()?;

        let constraints = if self.check(&Token::LBrace) {
            self.advance()?;
            let mut constraints = Vec::new();
            while !self.check(&Token::RBrace) {
                constraints.push(self.parse_relation_constraint()?);
            }
            self.expect(&Token::RBrace)?;
            constraints
        } else {
            Vec::new()
        };

        let end_span = constraints
            .last()
            .map_or(target.span, |_| self.previous_span());

        Ok(Relation {
            attributes,
            name,
            source,
            target,
            constraints,
            span: start.merge(end_span),
        })
    }

    fn parse_relation_constraint(&mut self) -> ParseResult<RelationConstraint> {
        match self.peek() {
            Some(Token::Symmetric) => {
                self.advance()?;
                Ok(RelationConstraint::Symmetric)
            }
            Some(Token::Reflexive) => {
                self.advance()?;
                Ok(RelationConstraint::Reflexive)
            }
            Some(Token::Irreflexive) => {
                self.advance()?;
                Ok(RelationConstraint::Irreflexive)
            }
            Some(Token::Transitive) => {
                self.advance()?;
                Ok(RelationConstraint::Transitive)
            }
            Some(Token::Antisymmetric) => {
                self.advance()?;
                Ok(RelationConstraint::Antisymmetric)
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::unexpected("relation constraint", &token, span))
            }
            None => Err(ParseError::unexpected_eof(
                "relation constraint",
                self.eof_span(),
            )),
        }
    }

    // ========== State Blocks ==========

    fn parse_state_block_with_attrs(
        &mut self,
        attributes: Vec<Attribute>,
    ) -> ParseResult<StateBlock> {
        let start = self.expect(&Token::State)?;
        let name = self.parse_ident()?;
        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        let mut invariants = Vec::new();

        while !self.check(&Token::RBrace) {
            // Parse attributes for invariants
            let inv_attrs = self.parse_attributes()?;

            if self.check(&Token::Invariant) {
                invariants.push(self.parse_invariant_with_attrs(inv_attrs)?);
            } else if !inv_attrs.is_empty() {
                // Attributes before a non-invariant item - unexpected
                let (token, span) = self.advance()?;
                return Err(ParseError::unexpected(
                    "invariant after attributes",
                    &token,
                    span,
                ));
            } else {
                fields.push(self.parse_state_field()?);
                // Allow optional comma after field
                self.check(&Token::Comma).then(|| self.advance());
            }
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(StateBlock {
            attributes,
            name,
            fields,
            invariants,
            span: start.merge(end),
        })
    }

    fn parse_state_field(&mut self) -> ParseResult<StateField> {
        let name = self.parse_ident()?;
        self.expect(&Token::Colon)?;
        let ty = self.parse_type_ref()?;

        let init = if self.check(&Token::Eq) {
            self.advance()?;
            Some(self.parse_expr()?)
        } else {
            None
        };

        let span = name.span.merge(init.as_ref().map_or(ty.span, |e| e.span));
        Ok(StateField {
            name,
            ty,
            init,
            span,
        })
    }

    fn parse_invariant_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<Invariant> {
        let start = self.expect(&Token::Invariant)?;

        let description = if let Some(Token::String(s)) = self.peek() {
            let s = s.clone();
            self.advance()?;
            Some(s)
        } else {
            None
        };

        self.expect(&Token::LBrace)?;
        let expr = self.parse_expr()?;
        let end = self.expect(&Token::RBrace)?;

        Ok(Invariant {
            attributes,
            description,
            expr,
            span: start.merge(end),
        })
    }

    // ========== Actions ==========

    fn parse_action_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<Action> {
        let start = self.expect(&Token::Action)?;
        let name = self.parse_ident()?;

        self.expect(&Token::LParen)?;
        let params = self.parse_comma_separated(Self::parse_action_param, &Token::RParen)?;
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::Arrow) {
            self.advance()?;
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        let mut contracts = Vec::new();
        while self.check(&Token::Requires) || self.check(&Token::Ensures) {
            contracts.push(self.parse_contract()?);
        }

        let body = if self.check(&Token::LBrace) {
            self.advance()?;
            let expr = self.parse_expr()?;
            self.expect(&Token::RBrace)?;
            Some(expr)
        } else {
            None
        };

        let end_span = body
            .as_ref()
            .map(|e| e.span)
            .or_else(|| contracts.last().map(|c| c.span))
            .or_else(|| return_type.as_ref().map(|t| t.span))
            .unwrap_or(name.span);

        Ok(Action {
            attributes,
            name,
            params,
            return_type,
            contracts,
            body,
            span: start.merge(end_span),
        })
    }

    fn parse_action_param(&mut self) -> ParseResult<ActionParam> {
        let name = self.parse_ident()?;
        self.expect(&Token::Colon)?;
        let ty = self.parse_type_ref()?;
        let span = name.span.merge(ty.span);
        Ok(ActionParam { name, ty, span })
    }

    fn parse_contract(&mut self) -> ParseResult<Contract> {
        let (kind, start) = if self.check(&Token::Requires) {
            (ContractKind::Requires, self.expect(&Token::Requires)?)
        } else {
            (ContractKind::Ensures, self.expect(&Token::Ensures)?)
        };

        self.expect(&Token::LBrace)?;
        let expr = self.parse_expr()?;
        let end = self.expect(&Token::RBrace)?;

        Ok(Contract {
            kind,
            expr,
            span: start.merge(end),
        })
    }

    // ========== Scenarios ==========

    fn parse_scenario_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<Scenario> {
        let start = self.expect(&Token::Scenario)?;

        let description = match self.peek() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance()?;
                s
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                return Err(ParseError::unexpected("scenario description string", &token, span));
            }
            None => return Err(ParseError::unexpected_eof("scenario description", self.eof_span())),
        };

        self.expect(&Token::LBrace)?;

        let given = self.parse_given_clause()?;
        let when = self.parse_when_clause()?;
        let then = self.parse_then_clause()?;

        let end = self.expect(&Token::RBrace)?;

        Ok(Scenario {
            attributes,
            description,
            given,
            when,
            then,
            span: start.merge(end),
        })
    }

    fn parse_given_clause(&mut self) -> ParseResult<GivenClause> {
        let start = self.expect(&Token::Given)?;
        self.expect(&Token::LBrace)?;

        let mut bindings = Vec::new();
        while !self.check(&Token::RBrace) {
            bindings.push(self.parse_binding()?);
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(GivenClause {
            bindings,
            span: start.merge(end),
        })
    }

    fn parse_when_clause(&mut self) -> ParseResult<WhenClause> {
        let start = self.expect(&Token::When)?;
        self.expect(&Token::LBrace)?;

        let mut bindings = Vec::new();
        while !self.check(&Token::RBrace) {
            bindings.push(self.parse_binding()?);
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(WhenClause {
            bindings,
            span: start.merge(end),
        })
    }

    fn parse_then_clause(&mut self) -> ParseResult<ThenClause> {
        let start = self.expect(&Token::Then)?;
        self.expect(&Token::LBrace)?;

        let mut assertions = Vec::new();
        while !self.check(&Token::RBrace) {
            assertions.push(self.parse_assertion()?);
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(ThenClause {
            assertions,
            span: start.merge(end),
        })
    }

    fn parse_binding(&mut self) -> ParseResult<Binding> {
        let name = self.parse_ident()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        let span = name.span.merge(value.span);
        Ok(Binding { name, value, span })
    }

    fn parse_assertion(&mut self) -> ParseResult<Assertion> {
        let expr = self.parse_expr()?;
        let span = expr.span;
        Ok(Assertion {
            expr,
            description: None,
            span,
        })
    }

    // ========== Properties ==========

    fn parse_property_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<Property> {
        let start = self.expect(&Token::Property)?;

        let description = match self.peek() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance()?;
                s
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                return Err(ParseError::unexpected("property description string", &token, span));
            }
            None => return Err(ParseError::unexpected_eof("property description", self.eof_span())),
        };

        self.expect(&Token::LBrace)?;

        let temporal_op = if self.check(&Token::Always) {
            self.advance()?;
            Some(TemporalOp::Always)
        } else if self.check(&Token::Eventually) {
            self.advance()?;
            Some(TemporalOp::Eventually)
        } else {
            None
        };

        // If temporal op, expect another block
        let expr = if temporal_op.is_some() {
            self.expect(&Token::LBrace)?;
            let e = self.parse_expr()?;
            self.expect(&Token::RBrace)?;
            e
        } else {
            self.parse_expr()?
        };

        let end = self.expect(&Token::RBrace)?;

        Ok(Property {
            attributes,
            description,
            temporal_op,
            expr,
            span: start.merge(end),
        })
    }

    // ========== Expressions ==========

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_implies_expr()
    }

    fn parse_implies_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_or_expr()?;

        while self.check(&Token::Implies) {
            self.advance()?;
            let right = self.parse_or_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Implies {
                    antecedent: Box::new(left),
                    consequent: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_and_expr()?;

        while self.check(&Token::Or) || self.check(&Token::OrOr) {
            self.advance()?;
            let right = self.parse_and_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Or,
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_comparison_expr()?;

        while self.check(&Token::And) || self.check(&Token::AndAnd) {
            self.advance()?;
            let right = self.parse_comparison_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Binary {
                    left: Box::new(left),
                    op: BinaryOp::And,
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_comparison_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_in_expr()?;

        loop {
            let op = match self.peek() {
                Some(Token::EqEq) => BinaryOp::Eq,
                Some(Token::NotEq) => BinaryOp::NotEq,
                Some(Token::LAngle) => BinaryOp::Lt,
                Some(Token::LtEq) => BinaryOp::LtEq,
                Some(Token::RAngle) => BinaryOp::Gt,
                Some(Token::GtEq) => BinaryOp::GtEq,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_in_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }

        // Handle `is` type check
        if self.check(&Token::Is) {
            self.advance()?;
            let ty = self.parse_type_path()?;
            let span = left.span.merge(ty.span);
            left = Expr::new(
                ExprKind::Is {
                    expr: Box::new(left),
                    ty,
                },
                span,
            );
        }

        Ok(left)
    }

    /// Parse a path that can be used as a type in `is` checks.
    /// Handles special variant tokens like Ok, Err, Some, None.
    fn parse_type_path(&mut self) -> ParseResult<Path> {
        match self.peek() {
            Some(Token::Ok) => {
                let (_, span) = self.advance()?;
                Ok(Path::simple(Ident::new("Ok", span)))
            }
            Some(Token::Err) => {
                let (_, span) = self.advance()?;
                Ok(Path::simple(Ident::new("Err", span)))
            }
            Some(Token::Some) => {
                let (_, span) = self.advance()?;
                Ok(Path::simple(Ident::new("Some", span)))
            }
            Some(Token::None) => {
                let (_, span) = self.advance()?;
                Ok(Path::simple(Ident::new("None", span)))
            }
            _ => self.parse_path(),
        }
    }

    fn parse_in_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_additive_expr()?;

        while self.check(&Token::In) {
            self.advance()?;
            let right = self.parse_additive_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Binary {
                    left: Box::new(left),
                    op: BinaryOp::In,
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_additive_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_multiplicative_expr()?;

        loop {
            let op = match self.peek() {
                Some(Token::Plus) => BinaryOp::Add,
                Some(Token::Minus) => BinaryOp::Sub,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_multiplicative_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_unary_expr()?;

        loop {
            let op = match self.peek() {
                Some(Token::Star) => BinaryOp::Mul,
                Some(Token::Slash) => BinaryOp::Div,
                Some(Token::Percent) => BinaryOp::Mod,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_unary_expr()?;
            let span = left.span.merge(right.span);
            left = Expr::new(
                ExprKind::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            Some(Token::Not | Token::Bang) => {
                let (_, start) = self.advance()?;
                let expr = self.parse_unary_expr()?;
                let span = start.merge(expr.span);
                Ok(Expr::new(
                    ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                    span,
                ))
            }
            Some(Token::Minus) => {
                let (_, start) = self.advance()?;
                let expr = self.parse_unary_expr()?;
                let span = start.merge(expr.span);
                Ok(Expr::new(
                    ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(expr),
                    },
                    span,
                ))
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            if self.check(&Token::Dot) {
                self.advance()?;
                let field = self.parse_ident()?;

                // Check for method call
                if self.check(&Token::LParen) {
                    self.advance()?;
                    let args = self.parse_comma_separated(Self::parse_expr, &Token::RParen)?;
                    let end = self.expect(&Token::RParen)?;
                    let span = expr.span.merge(end);
                    expr = Expr::new(
                        ExprKind::MethodCall {
                            receiver: Box::new(expr),
                            method: field,
                            args,
                        },
                        span,
                    );
                } else {
                    let span = expr.span.merge(field.span);
                    expr = Expr::new(
                        ExprKind::Field {
                            base: Box::new(expr),
                            field,
                        },
                        span,
                    );
                }
            } else if self.check(&Token::LBracket) {
                self.advance()?;
                let index = self.parse_expr()?;
                let end = self.expect(&Token::RBracket)?;
                let span = expr.span.merge(end);
                expr = Expr::new(
                    ExprKind::Index {
                        base: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                );
            } else if self.check(&Token::LParen) {
                self.advance()?;
                let args = self.parse_comma_separated(Self::parse_expr, &Token::RParen)?;
                let end = self.expect(&Token::RParen)?;
                let span = expr.span.merge(end);
                expr = Expr::new(
                    ExprKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                    span,
                );
            } else if self.check(&Token::Prime) {
                let (_, end) = self.advance()?;
                let span = expr.span.merge(end);
                expr = Expr::new(ExprKind::Prime(Box::new(expr)), span);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_primary_expr(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            // Literals
            Some(Token::Integer(n)) => {
                let n = *n;
                let (_, span) = self.advance()?;
                Ok(Expr::new(ExprKind::Literal(Literal::Int(n)), span))
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                let (_, span) = self.advance()?;
                Ok(Expr::new(ExprKind::Literal(Literal::String(s)), span))
            }
            Some(Token::True) => {
                let (_, span) = self.advance()?;
                Ok(Expr::new(ExprKind::Literal(Literal::Bool(true)), span))
            }
            Some(Token::False) => {
                let (_, span) = self.advance()?;
                Ok(Expr::new(ExprKind::Literal(Literal::Bool(false)), span))
            }

            // Quantifiers
            Some(Token::Forall) => self.parse_forall(),
            Some(Token::Exists) => self.parse_exists(),

            // Match expression
            Some(Token::Match) => self.parse_match_expr(),

            // If expression
            Some(Token::If) => self.parse_if_expr(),

            // Let expression
            Some(Token::Let) => self.parse_let_expr(),

            // Result keyword
            Some(Token::Ident(s)) if s.as_str() == "result" => {
                let (_, span) = self.advance()?;
                Ok(Expr::new(ExprKind::Result, span))
            }

            // Old expression
            Some(Token::Ident(s)) if s.as_str() == "old" => {
                let (_, start) = self.advance()?;
                self.expect(&Token::LParen)?;
                let inner = self.parse_expr()?;
                let end = self.expect(&Token::RParen)?;
                Ok(Expr::new(ExprKind::Old(Box::new(inner)), start.merge(end)))
            }

            // Set literal or block
            Some(Token::LBrace) => self.parse_set_or_map_literal(),

            // List literal
            Some(Token::LBracket) => self.parse_list_literal(),

            // Tuple or parenthesized expression or lambda
            Some(Token::LParen) => self.parse_paren_expr(),

            // Identifier or path
            Some(Token::Ident(_)) => {
                let path = self.parse_path()?;

                // Check for struct literal
                if self.check(&Token::LBrace) {
                    self.parse_struct_literal(path)
                } else {
                    let span = path.span;
                    Ok(Expr::new(ExprKind::Var(path), span))
                }
            }

            // Result variants
            Some(Token::Ok) => {
                let (_, span) = self.advance()?;
                let path = Path::simple(Ident::new("Ok", span));
                if self.check(&Token::LParen) {
                    self.advance()?;
                    let inner = self.parse_expr()?;
                    let end = self.expect(&Token::RParen)?;
                    Ok(Expr::new(
                        ExprKind::Call {
                            func: Box::new(Expr::new(ExprKind::Var(path), span)),
                            args: vec![inner],
                        },
                        span.merge(end),
                    ))
                } else {
                    Ok(Expr::new(ExprKind::Var(path), span))
                }
            }
            Some(Token::Err) => {
                let (_, span) = self.advance()?;
                let path = Path::simple(Ident::new("Err", span));
                if self.check(&Token::LParen) {
                    self.advance()?;
                    let inner = self.parse_expr()?;
                    let end = self.expect(&Token::RParen)?;
                    Ok(Expr::new(
                        ExprKind::Call {
                            func: Box::new(Expr::new(ExprKind::Var(path), span)),
                            args: vec![inner],
                        },
                        span.merge(end),
                    ))
                } else {
                    Ok(Expr::new(ExprKind::Var(path), span))
                }
            }
            Some(Token::Some) => {
                let (_, span) = self.advance()?;
                let path = Path::simple(Ident::new("Some", span));
                if self.check(&Token::LParen) {
                    self.advance()?;
                    let inner = self.parse_expr()?;
                    let end = self.expect(&Token::RParen)?;
                    Ok(Expr::new(
                        ExprKind::Call {
                            func: Box::new(Expr::new(ExprKind::Var(path), span)),
                            args: vec![inner],
                        },
                        span.merge(end),
                    ))
                } else {
                    Ok(Expr::new(ExprKind::Var(path), span))
                }
            }
            Some(Token::None) => {
                let (_, span) = self.advance()?;
                let path = Path::simple(Ident::new("None", span));
                Ok(Expr::new(ExprKind::Var(path), span))
            }

            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::expected_expr(&token, span))
            }
            None => Err(ParseError::unexpected_eof("expression", self.eof_span())),
        }
    }

    fn parse_forall(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::Forall)?;
        let bindings = self.parse_quant_bindings()?;

        let filter = if self.check(&Token::Where) {
            self.advance()?;
            Some(Box::new(self.parse_comparison_expr()?))
        } else {
            None
        };

        self.expect(&Token::FatArrow)?;
        let body = self.parse_expr()?;
        let span = start.merge(body.span);

        Ok(Expr::new(
            ExprKind::Forall {
                bindings,
                filter,
                body: Box::new(body),
            },
            span,
        ))
    }

    fn parse_exists(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::Exists)?;
        let bindings = self.parse_quant_bindings()?;

        let filter = if self.check(&Token::Where) {
            self.advance()?;
            Some(Box::new(self.parse_comparison_expr()?))
        } else {
            None
        };

        // Body is optional - if no `=>`, body defaults to `true`
        let (body, end_span) = if self.check(&Token::FatArrow) {
            self.advance()?;
            let body = self.parse_expr()?;
            let span = body.span;
            (body, span)
        } else {
            // Default body is `true` - exists is just a boolean check
            let end = filter
                .as_ref()
                .map(|f| f.span)
                .or_else(|| bindings.last().map(|b| b.span))
                .unwrap_or(start);
            (Expr::new(ExprKind::Literal(Literal::Bool(true)), end), end)
        };

        Ok(Expr::new(
            ExprKind::Exists {
                bindings,
                filter,
                body: Box::new(body),
            },
            start.merge(end_span),
        ))
    }

    fn parse_quant_bindings(&mut self) -> ParseResult<Vec<QuantBinding>> {
        let mut bindings = Vec::new();

        // Parse variable names (can be comma-separated before `in`)
        let mut names = vec![self.parse_ident()?];
        while self.check(&Token::Comma) {
            self.advance()?;
            // Stop if we hit `in` keyword next (it's an identifier check)
            if self.check(&Token::In) {
                break;
            }
            names.push(self.parse_ident()?);
        }

        self.expect(&Token::In)?;
        let collection = self.parse_postfix_expr()?;

        // Create a binding for each variable, all sharing the same collection
        for name in names {
            let span = name.span.merge(collection.span);
            bindings.push(QuantBinding {
                name,
                collection: collection.clone(),
                span,
            });
        }

        Ok(bindings)
    }

    fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect(&Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(&Token::RBrace) {
            arms.push(self.parse_match_arm()?);
            // Optional comma between arms
            self.check(&Token::Comma).then(|| self.advance());
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(Expr::new(
            ExprKind::Match {
                expr: Box::new(scrutinee),
                arms,
            },
            start.merge(end),
        ))
    }

    fn parse_match_arm(&mut self) -> ParseResult<MatchArm> {
        let pattern = self.parse_pattern()?;

        let guard = if self.check(&Token::If) {
            self.advance()?;
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(&Token::FatArrow)?;
        let body = self.parse_expr()?;
        let span = pattern.span.merge(body.span);

        Ok(MatchArm {
            pattern,
            guard,
            body,
            span,
        })
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        self.parse_or_pattern()
    }

    fn parse_or_pattern(&mut self) -> ParseResult<Pattern> {
        let mut left = self.parse_primary_pattern()?;

        while self.check(&Token::Pipe) {
            self.advance()?;
            let right = self.parse_primary_pattern()?;
            let span = left.span.merge(right.span);

            let patterns = match left.kind {
                PatternKind::Or(mut pats) => {
                    pats.push(right);
                    pats
                }
                _ => vec![left, right],
            };

            left = Pattern {
                kind: PatternKind::Or(patterns),
                span,
            };
        }

        Ok(left)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_primary_pattern(&mut self) -> ParseResult<Pattern> {
        match self.peek() {
            Some(Token::Underscore) => {
                let (_, span) = self.advance()?;
                Ok(Pattern {
                    kind: PatternKind::Wildcard,
                    span,
                })
            }
            Some(Token::Integer(n)) => {
                let n = *n;
                let (_, span) = self.advance()?;
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Int(n)),
                    span,
                })
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                let (_, span) = self.advance()?;
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::String(s)),
                    span,
                })
            }
            Some(Token::True) => {
                let (_, span) = self.advance()?;
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Bool(true)),
                    span,
                })
            }
            Some(Token::False) => {
                let (_, span) = self.advance()?;
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Bool(false)),
                    span,
                })
            }
            Some(Token::LParen) => {
                let start = self.expect(&Token::LParen)?;
                if self.check(&Token::RParen) {
                    let end = self.expect(&Token::RParen)?;
                    return Ok(Pattern {
                        kind: PatternKind::Literal(Literal::Unit),
                        span: start.merge(end),
                    });
                }

                let patterns = self.parse_comma_separated(Self::parse_pattern, &Token::RParen)?;
                let end = self.expect(&Token::RParen)?;

                if patterns.len() == 1 {
                    Ok(patterns.into_iter().next().unwrap())
                } else {
                    Ok(Pattern {
                        kind: PatternKind::Tuple(patterns),
                        span: start.merge(end),
                    })
                }
            }
            Some(Token::Ok | Token::Err | Token::Some | Token::None) => {
                self.parse_variant_pattern()
            }
            Some(Token::Ident(_)) => {
                let path = self.parse_path()?;

                if self.check(&Token::LBrace) {
                    // Struct pattern
                    self.parse_struct_pattern(path)
                } else if self.check(&Token::LParen) {
                    // Variant with args
                    self.advance()?;
                    let patterns =
                        self.parse_comma_separated(Self::parse_pattern, &Token::RParen)?;
                    let end = self.expect(&Token::RParen)?;
                    let span = path.span.merge(end);
                    Ok(Pattern {
                        kind: PatternKind::Variant { path, patterns },
                        span,
                    })
                } else if path.is_simple() {
                    // Simple binding
                    let ident = path.segments.into_iter().next().unwrap();
                    Ok(Pattern {
                        kind: PatternKind::Binding(ident.clone()),
                        span: ident.span,
                    })
                } else {
                    // Path pattern (variant without args)
                    let span = path.span;
                    Ok(Pattern {
                        kind: PatternKind::Variant {
                            path,
                            patterns: Vec::new(),
                        },
                        span,
                    })
                }
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::ExpectedPattern {
                    found: token.to_string(),
                    span,
                })
            }
            None => Err(ParseError::unexpected_eof("pattern", self.eof_span())),
        }
    }

    fn parse_variant_pattern(&mut self) -> ParseResult<Pattern> {
        let (token, span) = self.advance()?;
        let name = match &token {
            Token::Ok => "Ok",
            Token::Err => "Err",
            Token::Some => "Some",
            Token::None => "None",
            _ => unreachable!(),
        };

        let path = Path::simple(Ident::new(name, span));

        if self.check(&Token::LParen) {
            self.advance()?;
            let patterns = self.parse_comma_separated(Self::parse_pattern, &Token::RParen)?;
            let end = self.expect(&Token::RParen)?;
            Ok(Pattern {
                kind: PatternKind::Variant { path, patterns },
                span: span.merge(end),
            })
        } else {
            Ok(Pattern {
                kind: PatternKind::Variant {
                    path,
                    patterns: Vec::new(),
                },
                span,
            })
        }
    }

    fn parse_struct_pattern(&mut self, path: Path) -> ParseResult<Pattern> {
        let path_span = path.span;
        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        let mut rest = false;

        while !self.check(&Token::RBrace) {
            if self.check(&Token::DotDot) {
                self.advance()?;
                rest = true;
                break;
            }

            let name = self.parse_ident()?;
            let pattern = if self.check(&Token::Colon) {
                self.advance()?;
                Some(self.parse_pattern()?)
            } else {
                None
            };

            let span = pattern.as_ref().map_or(name.span, |p| name.span.merge(p.span));
            fields.push(FieldPattern {
                name,
                pattern,
                span,
            });

            if !self.check(&Token::RBrace) && !self.check(&Token::DotDot) {
                self.expect(&Token::Comma)?;
            }
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(Pattern {
            kind: PatternKind::Struct { ty: path, fields, rest },
            span: path_span.merge(end),
        })
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::If)?;
        let condition = self.parse_expr()?;
        self.expect(&Token::LBrace)?;
        let then_branch = self.parse_expr()?;
        self.expect(&Token::RBrace)?;

        let else_branch = if self.check(&Token::Else) {
            self.advance()?;
            if self.check(&Token::If) {
                Some(Box::new(self.parse_if_expr()?))
            } else {
                self.expect(&Token::LBrace)?;
                let expr = self.parse_expr()?;
                self.expect(&Token::RBrace)?;
                Some(Box::new(expr))
            }
        } else {
            None
        };

        let end_span = else_branch
            .as_ref()
            .map_or(then_branch.span, |e| e.span);

        Ok(Expr::new(
            ExprKind::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
            },
            start.merge(end_span),
        ))
    }

    fn parse_let_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::Let)?;
        let name = self.parse_ident()?;

        let ty = if self.check(&Token::Colon) {
            self.advance()?;
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        self.expect(&Token::In)?;
        let body = self.parse_expr()?;
        let span = start.merge(body.span);

        Ok(Expr::new(
            ExprKind::Let {
                name,
                ty,
                value: Box::new(value),
                body: Box::new(body),
            },
            span,
        ))
    }

    fn parse_set_or_map_literal(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::LBrace)?;

        if self.check(&Token::RBrace) {
            let end = self.expect(&Token::RBrace)?;
            return Ok(Expr::new(ExprKind::Set(Vec::new()), start.merge(end)));
        }

        let first = self.parse_expr()?;

        // Check for map literal (key: value)
        if self.check(&Token::Colon) {
            self.advance()?;
            let first_value = self.parse_expr()?;
            let mut pairs = vec![(first, first_value)];

            while self.check(&Token::Comma) {
                self.advance()?;
                if self.check(&Token::RBrace) {
                    break;
                }
                let key = self.parse_expr()?;
                self.expect(&Token::Colon)?;
                let value = self.parse_expr()?;
                pairs.push((key, value));
            }

            let end = self.expect(&Token::RBrace)?;
            return Ok(Expr::new(ExprKind::Map(pairs), start.merge(end)));
        }

        // Set literal
        let mut elements = vec![first];
        while self.check(&Token::Comma) {
            self.advance()?;
            if self.check(&Token::RBrace) {
                break;
            }
            elements.push(self.parse_expr()?);
        }

        let end = self.expect(&Token::RBrace)?;
        Ok(Expr::new(ExprKind::Set(elements), start.merge(end)))
    }

    fn parse_list_literal(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::LBracket)?;
        let elements = self.parse_comma_separated(Self::parse_expr, &Token::RBracket)?;
        let end = self.expect(&Token::RBracket)?;
        Ok(Expr::new(ExprKind::List(elements), start.merge(end)))
    }

    fn parse_paren_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::LParen)?;

        if self.check(&Token::RParen) {
            let end = self.expect(&Token::RParen)?;
            return Ok(Expr::new(
                ExprKind::Literal(Literal::Unit),
                start.merge(end),
            ));
        }

        // Check for lambda: (x, y) => expr or (x) => expr
        // We need to look ahead to see if this is a lambda
        if self.is_lambda_start() {
            return self.parse_lambda(start);
        }

        let first = self.parse_expr()?;

        if self.check(&Token::Comma) {
            // Tuple
            let mut elements = vec![first];
            while self.check(&Token::Comma) {
                self.advance()?;
                if self.check(&Token::RParen) {
                    break;
                }
                elements.push(self.parse_expr()?);
            }
            let end = self.expect(&Token::RParen)?;

            // Check for lambda after tuple params
            if self.check(&Token::FatArrow) {
                return self.parse_lambda_with_params(elements, start);
            }

            Ok(Expr::new(ExprKind::Tuple(elements), start.merge(end)))
        } else {
            self.expect(&Token::RParen)?;

            // Check for lambda: (x) => expr
            if self.check(&Token::FatArrow) {
                return self.parse_lambda_with_params(vec![first], start);
            }

            // Just a parenthesized expression
            Ok(first)
        }
    }

    fn is_lambda_start(&self) -> bool {
        // Simple heuristic: look for pattern like `ident)` or `ident,` or `ident:`
        // This isn't perfect but works for common cases
        if let Some(Token::Ident(_)) = self.peek() {
            if let Some(token) = self.peek_ahead(1) {
                matches!(token, Token::RParen | Token::Comma | Token::Colon)
            } else {
                false
            }
        } else {
            false
        }
    }

    fn parse_lambda(&mut self, start: Span) -> ParseResult<Expr> {
        let mut params = Vec::new();

        while !self.check(&Token::RParen) {
            let name = self.parse_ident()?;
            let ty = if self.check(&Token::Colon) {
                self.advance()?;
                Some(self.parse_type_ref()?)
            } else {
                None
            };
            params.push(LambdaParam { name, ty });

            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }

        self.expect(&Token::RParen)?;
        self.expect(&Token::FatArrow)?;
        let body = self.parse_expr()?;
        let span = start.merge(body.span);

        Ok(Expr::new(
            ExprKind::Lambda {
                params,
                body: Box::new(body),
            },
            span,
        ))
    }

    fn parse_lambda_with_params(&mut self, param_exprs: Vec<Expr>, start: Span) -> ParseResult<Expr> {
        self.expect(&Token::FatArrow)?;

        // Convert expressions to lambda params
        let mut params = Vec::new();
        for expr in param_exprs {
            match expr.kind {
                ExprKind::Var(path) if path.is_simple() => {
                    let ident = path.segments.into_iter().next().unwrap();
                    params.push(LambdaParam { name: ident, ty: None });
                }
                _ => {
                    return Err(ParseError::ExpectedIdent {
                        found: "expression".to_string(),
                        span: expr.span,
                    });
                }
            }
        }

        let body = self.parse_expr()?;
        let span = start.merge(body.span);

        Ok(Expr::new(
            ExprKind::Lambda {
                params,
                body: Box::new(body),
            },
            span,
        ))
    }

    fn parse_struct_literal(&mut self, path: Path) -> ParseResult<Expr> {
        let path_span = path.span;
        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(&Token::RBrace) {
            let name = self.parse_ident()?;
            self.expect(&Token::Colon)?;
            let value = self.parse_expr()?;
            let span = name.span.merge(value.span);
            fields.push(FieldInit { name, value, span });

            if !self.check(&Token::RBrace) {
                self.expect(&Token::Comma)?;
            }
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(Expr::new(
            ExprKind::Struct { ty: path, fields },
            path_span.merge(end),
        ))
    }

    // ========== Helpers ==========

    fn parse_ident(&mut self) -> ParseResult<Ident> {
        match self.peek() {
            Some(Token::Ident(name)) => {
                let name = name.clone();
                let (_, span) = self.advance()?;
                Ok(Ident::new(name, span))
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::expected_ident(&token, span))
            }
            None => Err(ParseError::unexpected_eof("identifier", self.eof_span())),
        }
    }

    fn parse_path(&mut self) -> ParseResult<Path> {
        let first = self.parse_ident()?;
        let mut segments = vec![first];

        while self.check(&Token::ColonColon) {
            self.advance()?;
            segments.push(self.parse_ident()?);
        }

        let span = segments
            .first()
            .unwrap()
            .span
            .merge(segments.last().unwrap().span);
        Ok(Path::new(segments, span))
    }

    fn parse_comma_separated<T, F>(&mut self, mut parse_fn: F, terminator: &Token) -> ParseResult<Vec<T>>
    where
        F: FnMut(&mut Self) -> ParseResult<T>,
    {
        let mut items = Vec::new();

        while !self.check(terminator) {
            items.push(parse_fn(self)?);
            if !self.check(terminator) {
                // Allow trailing comma
                if self.check(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
        }

        Ok(items)
    }

    fn check(&self, expected: &Token) -> bool {
        self.peek()
            .is_some_and(|t| std::mem::discriminant(t) == std::mem::discriminant(expected))
    }

    fn check_ahead(&self, n: usize, expected: &Token) -> bool {
        self.peek_ahead(n)
            .is_some_and(|t| std::mem::discriminant(t) == std::mem::discriminant(expected))
    }

    fn check_ident(&self, name: &str) -> bool {
        matches!(self.peek(), Some(Token::Ident(s)) if s.as_str() == name)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|st| &st.token)
    }

    fn peek_ahead(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n).map(|st| &st.token)
    }

    fn advance(&mut self) -> ParseResult<(Token, Span)> {
        if let Some(st) = self.tokens.get(self.pos) {
            self.pos += 1;
            Ok((st.token.clone(), st.span))
        } else {
            Err(ParseError::unexpected_eof("token", self.eof_span()))
        }
    }

    fn expect(&mut self, expected: &Token) -> ParseResult<Span> {
        if self.check(expected) {
            let (_, span) = self.advance()?;
            Ok(span)
        } else if let Some(token) = self.peek() {
            let token = token.clone();
            let span = self.current_span();
            Err(ParseError::unexpected(expected.to_string(), &token, span))
        } else {
            Err(ParseError::unexpected_eof(expected.to_string(), self.eof_span()))
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map_or_else(|| self.eof_span(), |st| st.span)
    }

    fn previous_span(&self) -> Span {
        self.tokens
            .get(self.pos.saturating_sub(1))
            .map_or_else(|| self.eof_span(), |st| st.span)
    }

    fn eof_span(&self) -> Span {
        Span::new(self.source.len(), self.source.len())
    }
}

/// Parse a Fastbreak specification from source code
///
/// # Errors
///
/// Returns an error if parsing fails
pub fn parse(source: &str) -> ParseResult<Specification> {
    Parser::new(source)?.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty() {
        let spec = parse("").unwrap();
        assert!(spec.is_empty());
    }

    #[test]
    fn test_parse_module() {
        let spec = parse("module auth").unwrap();
        assert_eq!(spec.module.as_ref().unwrap().name.as_str(), "auth");
    }

    #[test]
    fn test_parse_type_def() {
        let spec = parse("type User { id: Int, name: String }").unwrap();
        assert_eq!(spec.types.len(), 1);
        assert_eq!(spec.types[0].name.as_str(), "User");
        assert_eq!(spec.types[0].fields.len(), 2);
    }

    #[test]
    fn test_parse_enum_def() {
        let spec = parse("enum Status { Active, Inactive, Pending }").unwrap();
        assert_eq!(spec.enums.len(), 1);
        assert_eq!(spec.enums[0].name.as_str(), "Status");
        assert_eq!(spec.enums[0].variants.len(), 3);
    }

    #[test]
    fn test_parse_state_block() {
        let spec = parse(
            r#"
            state AppState {
                users: Set<User>,
                count: Int
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.states.len(), 1);
        assert_eq!(spec.states[0].name.as_str(), "AppState");
        assert_eq!(spec.states[0].fields.len(), 2);
    }

    #[test]
    fn test_parse_action() {
        let spec = parse(
            r#"
            action register(email: String) -> Result<User, Error>
                requires { not exists u in users where u.email == email }
                ensures { result is Ok implies users'.len() > users.len() }
            "#,
        )
        .unwrap();
        assert_eq!(spec.actions.len(), 1);
        assert_eq!(spec.actions[0].name.as_str(), "register");
        assert_eq!(spec.actions[0].contracts.len(), 2);
    }

    #[test]
    fn test_parse_scenario() {
        let spec = parse(
            r#"
            scenario "User registers successfully" {
                given {
                    users = {}
                }
                when {
                    result = register("test@example.com")
                }
                then {
                    result is Ok
                    users.len() == 1
                }
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.scenarios.len(), 1);
        assert_eq!(
            spec.scenarios[0].description.as_str(),
            "User registers successfully"
        );
    }

    #[test]
    fn test_parse_property() {
        let spec = parse(
            r#"
            property "Users have unique emails" {
                always {
                    forall u1, u2 in users where u1 != u2 => u1.email != u2.email
                }
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.properties.len(), 1);
        assert_eq!(spec.properties[0].temporal_op, Some(TemporalOp::Always));
    }

    #[test]
    fn test_parse_expressions() {
        // Test various expression forms
        let spec = parse(
            r#"
            state Test {
                x: Int,

                invariant "test" {
                    x + 1 == 2 and x * 2 < 10 or not (x == 0)
                }
            }
            "#,
        )
        .unwrap();
        assert!(!spec.states[0].invariants.is_empty());
    }

    #[test]
    fn test_parse_forall() {
        let spec = parse(
            r#"
            property "all positive" {
                forall x in numbers => x > 0
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.properties.len(), 1);
    }

    #[test]
    fn test_parse_match() {
        let spec = parse(
            r#"
            state Test {
                result: Result<Int, String>,

                invariant "match test" {
                    match result {
                        Ok(n) => n > 0,
                        Err(_) => true
                    }
                }
            }
            "#,
        )
        .unwrap();
        assert!(!spec.states[0].invariants.is_empty());
    }

    #[test]
    fn test_parse_lambda() {
        let spec = parse(
            r#"
            state Test {
                nums: List<Int>,

                invariant "mapped" {
                    nums.map((x) => x + 1).len() == nums.len()
                }
            }
            "#,
        )
        .unwrap();
        assert!(!spec.states[0].invariants.is_empty());
    }

    #[test]
    fn test_parse_attribute_simple() {
        let spec = parse(
            r#"
            @id(TYPE001)
            type User {
                id: Int
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.types.len(), 1);
        assert_eq!(spec.types[0].attributes.len(), 1);
        assert_eq!(spec.types[0].attributes[0].name.as_str(), "id");
    }

    #[test]
    fn test_parse_attribute_with_string() {
        let spec = parse(
            r#"
            @rationale("Users must be uniquely identifiable")
            type User {
                id: Int
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.types[0].attributes.len(), 1);
        assert_eq!(spec.types[0].attributes[0].name.as_str(), "rationale");
        assert_eq!(spec.types[0].attributes[0].args.len(), 1);
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let spec = parse(
            r#"
            @id(REQ001)
            @stakeholder(security_team)
            @rationale("Security requirement")
            action authenticate(user: String) -> Result<Token, Error>
                requires { true }
            "#,
        )
        .unwrap();
        assert_eq!(spec.actions.len(), 1);
        assert_eq!(spec.actions[0].attributes.len(), 3);
        assert_eq!(spec.actions[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.actions[0].attributes[1].name.as_str(), "stakeholder");
        assert_eq!(spec.actions[0].attributes[2].name.as_str(), "rationale");
    }

    #[test]
    fn test_parse_attribute_on_all_constructs() {
        let spec = parse(
            r#"
            @id(TYPE001)
            type User { id: Int }

            @id(ENUM001)
            enum Status { Active, Inactive }

            @id(REL001)
            relation friends: User -> Set<User>

            @id(STATE001)
            state AppState {
                users: Set<User>

                @id(INV001)
                invariant "non-empty" { users.len() > 0 }
            }

            @id(ACTION001)
            action create() -> User
                requires { true }

            @id(SCENARIO001)
            scenario "test" {
                given { x = 1 }
                when { y = 2 }
                then { x + y == 3 }
            }

            @id(PROP001)
            property "always true" {
                always { true }
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.types[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.enums[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.relations[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.states[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.states[0].invariants[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.actions[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.scenarios[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.properties[0].attributes[0].name.as_str(), "id");
    }

    #[test]
    fn test_parse_attribute_no_args() {
        let spec = parse(
            r#"
            @deprecated
            type OldUser { id: Int }
            "#,
        )
        .unwrap();
        assert_eq!(spec.types[0].attributes[0].name.as_str(), "deprecated");
        assert!(spec.types[0].attributes[0].args.is_empty());
    }
}
