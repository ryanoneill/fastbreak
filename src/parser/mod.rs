//! Parser for Fastbreak specifications
//!
//! This module implements a recursive descent parser that transforms
//! a stream of tokens into an AST.

mod error;

pub use error::{ParseError, ParseResult};

use crate::ast::{
    Action, ActionParam, Alternative, AppliesTo, AppliesToKind, Assertion, Attribute, AttributeArg,
    BinaryOp, Binding, BuiltInType, Constraint, Contract, ContractKind, DurationUnit, EnumDef,
    EnumVariant, Expr, ExprKind, Field, FieldInit, FieldPattern, GenericArg, GivenClause, Ident,
    Import, ImportItem, Invariant, LambdaParam, Literal, LoadConditions, MatchArm, MeasurementPeriod,
    Module, Path, Pattern, PatternKind, Property, Quality, QualityCategory, QualityOp,
    QualityProperty, QualityPropertyValue, QualityTarget, QualityValue, QuantBinding,
    QuantBindingKind, RateUnit, Relation, RelationConstraint, Scale, Scenario, SizeUnit,
    Specification, StateBlock, StateField, TemporalOp, ThenClause, TypeAlias, TypeDef, TypeRef,
    TypeRefKind, UnaryOp, VerificationKind, VerificationMethod, WhenClause,
};

/// Represents either a type definition or a type alias
enum TypeOrAlias {
    Type(TypeDef),
    Alias(TypeAlias),
}
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
    /// Whether struct literals are allowed in the current expression context.
    /// This is set to false when parsing match scrutinees to avoid ambiguity
    /// with match arms (e.g., `match x { ... }` where `{ ... }` is the match body).
    allow_struct_literal: bool,
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
            allow_struct_literal: true,
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
        if self.check_keyword_ident("module") {
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
                Some(Token::Type) => {
                    // Could be a struct (type Foo { ... }) or alias (type Foo = ...)
                    match self.parse_type_or_alias_with_attrs(attributes)? {
                        TypeOrAlias::Type(t) => spec.types.push(t),
                        TypeOrAlias::Alias(a) => spec.type_aliases.push(a),
                    }
                }
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
                Some(Token::Quality) => {
                    spec.qualities.push(self.parse_quality_with_attrs(attributes)?);
                }
                Some(_) => {
                    let (token, span) = self.advance()?;
                    return Err(ParseError::unexpected(
                        "type, enum, state, action, scenario, property, or quality",
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
        let start = self.expect_keyword_ident("module")?;
        let path = self.parse_dotted_path()?;
        let span = start.merge(path.span);
        Ok(Module { path, span })
    }

    /// Parse a dot-separated path like `abc.def.ghi`
    fn parse_dotted_path(&mut self) -> ParseResult<Path> {
        let first = self.parse_ident()?;
        let mut segments = vec![first];

        while self.check(&Token::Dot) {
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

    /// Parse either a type definition or type alias
    fn parse_type_or_alias_with_attrs(
        &mut self,
        attributes: Vec<Attribute>,
    ) -> ParseResult<TypeOrAlias> {
        let start = self.expect(&Token::Type)?;
        let name = self.parse_ident()?;

        // Optional type parameters
        let type_params = if self.check(&Token::LAngle) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };

        // Check for '=' (type alias) vs '{' (struct definition)
        if self.check(&Token::Eq) {
            self.advance()?;
            let base = self.parse_type_ref()?;

            // Optional refinement clause
            let (refinement, end) = if self.check(&Token::Where) {
                self.advance()?;
                let pred = self.parse_expr()?;
                let span = pred.span;
                (Some(pred), span)
            } else {
                (None, base.span)
            };

            Ok(TypeOrAlias::Alias(TypeAlias {
                attributes,
                name,
                type_params,
                base,
                refinement,
                span: start.merge(end),
            }))
        } else {
            // Struct definition
            self.expect(&Token::LBrace)?;
            let fields = self.parse_comma_separated(Self::parse_field, &Token::RBrace)?;
            let end_brace = self.expect(&Token::RBrace)?;

            // Optional refinement clause: `where <predicate>`
            let (refinement, end) = if self.check(&Token::Where) {
                self.advance()?;
                let pred = self.parse_expr()?;
                let span = pred.span;
                (Some(pred), span)
            } else {
                (None, end_brace)
            };

            Ok(TypeOrAlias::Type(TypeDef {
                attributes,
                name,
                type_params,
                fields,
                refinement,
                span: start.merge(end),
            }))
        }
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
            Some(Token::Ident(name)) => {
                // Check if identifier is a built-in type
                let builtin = match name.as_str() {
                    "Set" => Some(BuiltInType::Set),
                    "Map" => Some(BuiltInType::Map),
                    "List" => Some(BuiltInType::List),
                    "Option" => Some(BuiltInType::Option),
                    "Result" => Some(BuiltInType::Result),
                    "String" => Some(BuiltInType::String),
                    "Int" => Some(BuiltInType::Int),
                    "Bool" => Some(BuiltInType::Bool),
                    _ => None,
                };

                if let Some(builtin_type) = builtin {
                    self.parse_builtin_type(builtin_type)
                } else {
                    let path = self.parse_path()?;
                    let span = path.span;
                    Ok(TypeRef {
                        kind: TypeRefKind::Named(path),
                        span,
                    })
                }
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

        // Parse alternative flows
        let mut alternatives = Vec::new();
        while self.check(&Token::Alt) || self.check(&Token::At) {
            // Handle attributes before alt
            let alt_attrs = if self.check(&Token::At) {
                self.parse_attributes()?
            } else {
                Vec::new()
            };

            if self.check(&Token::Alt) {
                alternatives.push(self.parse_alternative_with_attrs(alt_attrs)?);
            } else if !alt_attrs.is_empty() {
                // We have attributes but not an alt - that's an error
                let span = self.current_span();
                return Err(ParseError::unexpected(
                    "alt",
                    &self.peek().cloned().unwrap_or(Token::RBrace),
                    span,
                ));
            }
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(Scenario {
            attributes,
            description,
            given,
            when,
            then,
            alternatives,
            span: start.merge(end),
        })
    }

    fn parse_given_clause(&mut self) -> ParseResult<GivenClause> {
        let start = self.expect_keyword_ident("given")?;
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
        let start = self.expect_keyword_ident("when")?;
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
        let start = self.expect_keyword_ident("then")?;
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

    fn parse_alternative_with_attrs(
        &mut self,
        attributes: Vec<Attribute>,
    ) -> ParseResult<Alternative> {
        let start = self.expect(&Token::Alt)?;

        // Parse the alternative name (required string)
        let name = match self.peek() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance()?;
                s
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                return Err(ParseError::unexpected(
                    "alternative description string",
                    &token,
                    span,
                ));
            }
            None => {
                return Err(ParseError::unexpected_eof(
                    "alternative description",
                    self.eof_span(),
                ))
            }
        };

        // Optional condition: `when { expr }`
        let condition = if self.check_keyword_ident("when") {
            self.advance()?; // consume 'when'
            self.expect(&Token::LBrace)?;
            let expr = self.parse_expr()?;
            self.expect(&Token::RBrace)?;
            Some(expr)
        } else {
            None
        };

        self.expect(&Token::LBrace)?;

        // Optional given clause (extends base)
        let given = if self.check_keyword_ident("given") {
            Some(self.parse_given_clause()?)
        } else {
            None
        };

        // Optional when clause (replaces base)
        let when = if self.check_keyword_ident("when") {
            Some(self.parse_when_clause()?)
        } else {
            None
        };

        // Required then clause
        let then = self.parse_then_clause()?;

        let end = self.expect(&Token::RBrace)?;

        Ok(Alternative {
            attributes,
            name,
            condition,
            given,
            when,
            then,
            span: start.merge(end),
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

    // ========== Quality Requirements ==========

    fn parse_quality_with_attrs(&mut self, attributes: Vec<Attribute>) -> ParseResult<Quality> {
        let start = self.expect(&Token::Quality)?;

        // Parse category
        let category = self.parse_quality_category()?;

        // Parse description string
        let description = match self.peek() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance()?;
                s
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                return Err(ParseError::unexpected(
                    "quality description string",
                    &token,
                    span,
                ));
            }
            None => {
                return Err(ParseError::unexpected_eof(
                    "quality description",
                    self.eof_span(),
                ))
            }
        };

        self.expect(&Token::LBrace)?;

        // All properties are optional and can appear in any order
        let mut metric: Option<Ident> = None;
        let mut scale: Option<Scale> = None;
        let mut target: Option<QualityTarget> = None;
        let mut constraint: Option<Constraint> = None;
        let mut applies_to: Option<AppliesTo> = None;
        let mut measurement: Option<MeasurementPeriod> = None;
        let mut under_load: Option<LoadConditions> = None;
        let mut verified_by: Vec<VerificationMethod> = Vec::new();
        let mut properties: Vec<QualityProperty> = Vec::new();

        while !self.check(&Token::RBrace) {
            let prop_name = self.parse_ident()?;
            self.expect(&Token::Colon)?;

            match prop_name.as_str() {
                "metric" => {
                    metric = Some(self.parse_ident()?);
                }
                "scale" => {
                    scale = Some(self.parse_scale()?);
                }
                "target" => {
                    target = Some(self.parse_quality_target()?);
                }
                "constraint" => {
                    constraint = Some(self.parse_constraint()?);
                }
                "applies_to" => {
                    applies_to = Some(self.parse_applies_to()?);
                }
                "measurement" => {
                    measurement = Some(self.parse_measurement_period()?);
                }
                "under_load" => {
                    under_load = Some(self.parse_load_conditions()?);
                }
                "verified_by" => {
                    verified_by = self.parse_verification_methods()?;
                }
                _ => {
                    // Unknown property - store as generic property
                    let value = if self.check(&Token::LAngle)
                        || self.check(&Token::LtEq)
                        || self.check(&Token::RAngle)
                        || self.check(&Token::GtEq)
                        || self.check(&Token::EqEq)
                    {
                        let t = self.parse_quality_target()?;
                        QualityPropertyValue::Target(t)
                    } else {
                        let ident = self.parse_ident()?;
                        QualityPropertyValue::Ident(ident)
                    };
                    properties.push(QualityProperty {
                        name: prop_name.clone(),
                        value,
                        span: prop_name.span,
                    });
                }
            }

            // Optional comma
            if self.check(&Token::Comma) {
                self.advance()?;
            }
        }

        let end = self.expect(&Token::RBrace)?;

        // Metric and target are required
        let metric = metric.ok_or_else(|| {
            ParseError::unexpected("metric property in quality block", &Token::RBrace, end)
        })?;
        let target = target.ok_or_else(|| {
            ParseError::unexpected("target property in quality block", &Token::RBrace, end)
        })?;

        Ok(Quality {
            attributes,
            category,
            description,
            metric,
            scale,
            target,
            constraint,
            applies_to,
            measurement,
            under_load,
            verified_by,
            properties,
            span: start.merge(end),
        })
    }

    fn parse_scale(&mut self) -> ParseResult<Scale> {
        let ident = self.parse_ident()?;
        match ident.as_str() {
            "mean" => Ok(Scale::Mean),
            "median" => Ok(Scale::Median),
            "p50" => Ok(Scale::P50),
            "p90" => Ok(Scale::P90),
            "p95" => Ok(Scale::P95),
            "p99" => Ok(Scale::P99),
            "p999" => Ok(Scale::P999),
            "max" => Ok(Scale::Max),
            "min" => Ok(Scale::Min),
            _ => Err(ParseError::unexpected(
                "scale (mean, median, p50, p90, p95, p99, p999, max, min)",
                &Token::Ident(ident.name),
                ident.span,
            )),
        }
    }

    fn parse_constraint(&mut self) -> ParseResult<Constraint> {
        let ident = self.parse_ident()?;
        match ident.as_str() {
            "hard" => Ok(Constraint::Hard),
            "soft" => Ok(Constraint::Soft),
            _ => Err(ParseError::unexpected(
                "constraint (hard, soft)",
                &Token::Ident(ident.name),
                ident.span,
            )),
        }
    }

    fn parse_applies_to(&mut self) -> ParseResult<AppliesTo> {
        let start = self.current_span();

        // Parse kind - these are keywords, not identifiers
        let kind = if self.check(&Token::Action) {
            self.advance()?;
            AppliesToKind::Action
        } else if self.check(&Token::State) {
            self.advance()?;
            AppliesToKind::State
        } else if self.check(&Token::Type) {
            self.advance()?;
            AppliesToKind::Type
        } else {
            let found = self.peek().cloned().unwrap_or(Token::RBrace);
            return Err(ParseError::unexpected(
                "applies_to kind (action, state, type)",
                &found,
                self.current_span(),
            ));
        };

        let name = self.parse_ident()?;
        let span = start.merge(name.span);
        Ok(AppliesTo { kind, name, span })
    }

    fn parse_measurement_period(&mut self) -> ParseResult<MeasurementPeriod> {
        let ident = self.parse_ident()?;
        match ident.as_str() {
            "per_request" => Ok(MeasurementPeriod::PerRequest),
            "per_second" => Ok(MeasurementPeriod::PerSecond),
            "per_minute" => Ok(MeasurementPeriod::PerMinute),
            "hourly" => Ok(MeasurementPeriod::Hourly),
            "daily" => Ok(MeasurementPeriod::Daily),
            "weekly" => Ok(MeasurementPeriod::Weekly),
            "monthly" => Ok(MeasurementPeriod::Monthly),
            _ => Err(ParseError::unexpected(
                "measurement period (per_request, per_second, per_minute, hourly, daily, weekly, monthly)",
                &Token::Ident(ident.name),
                ident.span,
            )),
        }
    }

    fn parse_load_conditions(&mut self) -> ParseResult<LoadConditions> {
        let start = self.expect(&Token::LBrace)?;

        let mut concurrent_users: Option<i64> = None;
        let mut concurrent_connections: Option<i64> = None;
        let mut requests_per_second: Option<i64> = None;
        let mut payload_size: Option<(i64, SizeUnit)> = None;
        let mut duration: Option<(i64, DurationUnit)> = None;

        while !self.check(&Token::RBrace) {
            let prop_name = self.parse_ident()?;
            self.expect(&Token::Colon)?;

            match prop_name.as_str() {
                "concurrent_users" => {
                    concurrent_users = Some(self.parse_integer()?);
                }
                "concurrent_connections" => {
                    concurrent_connections = Some(self.parse_integer()?);
                }
                "requests_per_second" => {
                    requests_per_second = Some(self.parse_integer()?);
                }
                "payload_size" => {
                    let (value, unit) = self.parse_size_value()?;
                    payload_size = Some((value, unit));
                }
                "duration" => {
                    let (value, unit) = self.parse_duration_value()?;
                    duration = Some((value, unit));
                }
                _ => {
                    // Skip unknown properties
                    self.parse_integer().ok();
                }
            }

            // Optional comma
            if self.check(&Token::Comma) {
                self.advance()?;
            }
        }

        let end = self.expect(&Token::RBrace)?;

        Ok(LoadConditions {
            concurrent_users,
            concurrent_connections,
            requests_per_second,
            payload_size,
            duration,
            span: start.merge(end),
        })
    }

    fn parse_integer(&mut self) -> ParseResult<i64> {
        match self.peek() {
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance()?;
                Ok(n)
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::unexpected("integer", &token, span))
            }
            None => Err(ParseError::unexpected_eof("integer", self.eof_span())),
        }
    }

    fn parse_size_value(&mut self) -> ParseResult<(i64, SizeUnit)> {
        let n = self.parse_integer()?;
        let unit_ident = self.parse_ident()?;
        let unit = match unit_ident.as_str().to_lowercase().as_str() {
            "kb" => SizeUnit::Kb,
            "mb" => SizeUnit::Mb,
            "gb" => SizeUnit::Gb,
            "tb" => SizeUnit::Tb,
            _ => {
                return Err(ParseError::unexpected(
                    "size unit (KB, MB, GB, TB)",
                    &Token::Ident(unit_ident.name),
                    unit_ident.span,
                ))
            }
        };
        Ok((n, unit))
    }

    fn parse_duration_value(&mut self) -> ParseResult<(i64, DurationUnit)> {
        let n = self.parse_integer()?;
        let unit_ident = self.parse_ident()?;
        let unit = match unit_ident.as_str() {
            "ms" => DurationUnit::Ms,
            "s" => DurationUnit::S,
            "m" => DurationUnit::M,
            "h" => DurationUnit::H,
            _ => {
                return Err(ParseError::unexpected(
                    "duration unit (ms, s, m, h)",
                    &Token::Ident(unit_ident.name),
                    unit_ident.span,
                ))
            }
        };
        Ok((n, unit))
    }

    fn parse_verification_methods(&mut self) -> ParseResult<Vec<VerificationMethod>> {
        self.expect(&Token::LBracket)?;

        let mut methods = Vec::new();

        while !self.check(&Token::RBracket) {
            let method = self.parse_verification_method()?;
            methods.push(method);

            // Optional comma
            if self.check(&Token::Comma) {
                self.advance()?;
            }
        }

        self.expect(&Token::RBracket)?;
        Ok(methods)
    }

    fn parse_verification_method(&mut self) -> ParseResult<VerificationMethod> {
        let start = self.current_span();
        let kind_ident = self.parse_ident()?;
        let kind = match kind_ident.as_str() {
            "test" => VerificationKind::Test,
            "monitor" => VerificationKind::Monitor,
            "benchmark" => VerificationKind::Benchmark,
            "audit" => VerificationKind::Audit,
            _ => {
                return Err(ParseError::unexpected(
                    "verification kind (test, monitor, benchmark, audit)",
                    &Token::Ident(kind_ident.name),
                    kind_ident.span,
                ))
            }
        };

        // Parse the name as a string
        let name = match self.peek() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance()?;
                s
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                return Err(ParseError::unexpected("verification name string", &token, span));
            }
            None => {
                return Err(ParseError::unexpected_eof(
                    "verification name",
                    self.eof_span(),
                ))
            }
        };

        let end = self.current_span();

        Ok(VerificationMethod {
            kind,
            name,
            span: start.merge(end),
        })
    }

    fn parse_quality_category(&mut self) -> ParseResult<QualityCategory> {
        match self.peek() {
            Some(Token::Ident(name)) => {
                let category = match name.as_str() {
                    "performance" => QualityCategory::Performance,
                    "reliability" => QualityCategory::Reliability,
                    "security" => QualityCategory::Security,
                    "usability" => QualityCategory::Usability,
                    "scalability" => QualityCategory::Scalability,
                    "maintainability" => QualityCategory::Maintainability,
                    _ => {
                        let token = Token::Ident(name.clone());
                        let span = self.current_span();
                        return Err(ParseError::unexpected("quality category", &token, span));
                    }
                };
                self.advance()?;
                Ok(category)
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                Err(ParseError::unexpected("quality category", &token, span))
            }
            None => Err(ParseError::unexpected_eof(
                "quality category",
                self.eof_span(),
            )),
        }
    }

    fn parse_quality_target(&mut self) -> ParseResult<QualityTarget> {
        let start = self.current_span();

        // Parse comparison operator
        let op = match self.peek() {
            Some(Token::LAngle) => {
                self.advance()?;
                QualityOp::Lt
            }
            Some(Token::LtEq) => {
                self.advance()?;
                QualityOp::LtEq
            }
            Some(Token::RAngle) => {
                self.advance()?;
                QualityOp::Gt
            }
            Some(Token::GtEq) => {
                self.advance()?;
                QualityOp::GtEq
            }
            Some(Token::EqEq) => {
                self.advance()?;
                QualityOp::Eq
            }
            Some(token) => {
                let token = token.clone();
                let span = self.current_span();
                return Err(ParseError::unexpected(
                    "comparison operator (<, <=, >, >=, ==)",
                    &token,
                    span,
                ));
            }
            None => {
                return Err(ParseError::unexpected_eof(
                    "comparison operator",
                    self.eof_span(),
                ))
            }
        };

        // Parse value
        let value = self.parse_quality_value()?;

        let end = self.current_span();

        Ok(QualityTarget {
            op,
            value,
            span: start.merge(end),
        })
    }

    fn parse_quality_value(&mut self) -> ParseResult<QualityValue> {
        match self.peek() {
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance()?;

                // Check for unit suffix
                match self.peek() {
                    Some(Token::Ident(unit)) if unit.as_str() == "ms" => {
                        self.advance()?;
                        Ok(QualityValue::Duration(n, DurationUnit::Ms))
                    }
                    Some(Token::Ident(unit)) if unit.as_str() == "s" => {
                        self.advance()?;
                        Ok(QualityValue::Duration(n, DurationUnit::S))
                    }
                    Some(Token::Ident(unit)) if unit.as_str() == "m" => {
                        self.advance()?;
                        Ok(QualityValue::Duration(n, DurationUnit::M))
                    }
                    Some(Token::Ident(unit)) if unit.as_str() == "h" => {
                        self.advance()?;
                        Ok(QualityValue::Duration(n, DurationUnit::H))
                    }
                    Some(Token::Percent) => {
                        self.advance()?;
                        #[allow(clippy::cast_precision_loss)]
                        Ok(QualityValue::Percentage(n as f64))
                    }
                    Some(Token::Slash) => {
                        self.advance()?;
                        // Expect rate unit: s, m, or h
                        let unit = match self.peek() {
                            Some(Token::Ident(u)) if u.as_str() == "s" => {
                                self.advance()?;
                                RateUnit::PerSecond
                            }
                            Some(Token::Ident(u)) if u.as_str() == "m" => {
                                self.advance()?;
                                RateUnit::PerMinute
                            }
                            Some(Token::Ident(u)) if u.as_str() == "h" => {
                                self.advance()?;
                                RateUnit::PerHour
                            }
                            Some(token) => {
                                let token = token.clone();
                                return Err(ParseError::expected_ident(
                                    &token,
                                    self.current_span(),
                                ));
                            }
                            None => {
                                return Err(ParseError::unexpected_eof(
                                    "rate unit",
                                    self.eof_span(),
                                ));
                            }
                        };
                        Ok(QualityValue::Rate(n, unit))
                    }
                    _ => Ok(QualityValue::Int(n)),
                }
            }
            Some(_) => {
                // Fall back to expression parsing
                let expr = self.parse_expr()?;
                Ok(QualityValue::Expr(expr))
            }
            None => Err(ParseError::unexpected_eof("quality value", self.eof_span())),
        }
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

            // Self reference (for refinement predicates)
            Some(Token::SelfKw) => {
                let (_, span) = self.advance()?;
                Ok(Expr::new(ExprKind::SelfRef, span))
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

                // Check for struct literal (only if allowed in this context)
                if self.allow_struct_literal && self.check(&Token::LBrace) {
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
        let mut all_bindings = Vec::new();

        loop {
            // Parse one binding group (either typed or collection with possibly multiple names)
            let binding_group = self.parse_binding_group()?;
            all_bindings.extend(binding_group);

            // If we see a comma, there's another binding group
            if self.check(&Token::Comma) {
                self.advance()?; // consume comma
                // Continue parsing next binding group
            } else {
                break;
            }
        }

        Ok(all_bindings)
    }

    fn parse_binding_group(&mut self) -> ParseResult<Vec<QuantBinding>> {
        // A binding group is either:
        // 1. `name: Type` - single typed binding
        // 2. `name1, name2, ... in collection` - multiple names sharing one collection

        let first_name = self.parse_ident()?;

        if self.check(&Token::Colon) {
            // Typed binding: `name: Type`
            self.advance()?; // consume ':'
            let type_ref = self.parse_type_ref()?;
            let span = first_name.span.merge(type_ref.span);
            Ok(vec![QuantBinding {
                name: first_name,
                kind: QuantBindingKind::Typed(type_ref),
                span,
            }])
        } else {
            // Collection binding: `name1, name2, ... in collection`
            let mut names = vec![first_name];

            // Collect additional names before 'in'
            while self.check(&Token::Comma) {
                self.advance()?; // consume comma
                // If next is 'in', we've collected all names
                if self.check(&Token::In) {
                    break;
                }
                names.push(self.parse_ident()?);
            }

            self.expect(&Token::In)?;
            let collection = self.parse_postfix_expr()?;

            // Create a binding for each name, all sharing the same collection
            let mut bindings = Vec::new();
            for name in names {
                let span = name.span.merge(collection.span);
                bindings.push(QuantBinding {
                    name,
                    kind: QuantBindingKind::InCollection(collection.clone()),
                    span,
                });
            }
            Ok(bindings)
        }
    }

    fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::Match)?;

        // Disable struct literals when parsing the scrutinee to avoid ambiguity
        // with the match body. E.g., `match x { ... }` should parse `x` as the
        // scrutinee, not `x { ... }` as a struct literal.
        let saved_allow_struct_literal = self.allow_struct_literal;
        self.allow_struct_literal = false;
        let scrutinee = self.parse_expr()?;
        self.allow_struct_literal = saved_allow_struct_literal;

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

        // Check for block body: `=> { ... }`
        let body = if self.check(&Token::LBrace) {
            self.parse_block_expr()?
        } else {
            self.parse_expr()?
        };
        let span = pattern.span.merge(body.span);

        Ok(MatchArm {
            pattern,
            guard,
            body,
            span,
        })
    }

    fn parse_block_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect(&Token::LBrace)?;

        let mut exprs = Vec::new();

        while !self.check(&Token::RBrace) {
            let expr = self.parse_expr()?;
            exprs.push(expr);

            // Consume optional semicolon between expressions
            if self.check(&Token::Semicolon) {
                self.advance()?;
            } else if !self.check(&Token::RBrace) {
                // If no semicolon and not at end, it's an error
                break;
            }
        }

        let end = self.expect(&Token::RBrace)?;

        // If only one expression, return it directly (unwrap single-element block)
        if exprs.len() == 1 {
            let mut expr = exprs.pop().unwrap();
            expr.span = start.merge(end);
            Ok(expr)
        } else {
            Ok(Expr::new(ExprKind::Block(exprs), start.merge(end)))
        }
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
        // Parse value with restricted precedence to stop before `in` keyword
        // (since `in` is both a binary operator and let-expression keyword)
        let value = self.parse_additive_expr()?;
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

        // Only consume `::` if followed by an identifier (not `{` for import groups)
        while self.check(&Token::ColonColon) && !self.check_ahead(1, &Token::LBrace) {
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

    /// Expect an identifier with a specific name (for contextual keywords)
    fn expect_keyword_ident(&mut self, expected: &str) -> ParseResult<Span> {
        if self.check_keyword_ident(expected) {
            let (_, span) = self.advance()?;
            return Ok(span);
        }
        if let Some(token) = self.peek() {
            let token = token.clone();
            let span = self.current_span();
            Err(ParseError::unexpected(expected, &token, span))
        } else {
            Err(ParseError::unexpected_eof(expected, self.eof_span()))
        }
    }

    /// Check if current token is an identifier with a specific name
    fn check_keyword_ident(&self, expected: &str) -> bool {
        matches!(self.peek(), Some(Token::Ident(name)) if name.as_str() == expected)
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
        assert_eq!(spec.module.as_ref().unwrap().name(), "auth");
    }

    #[test]
    fn test_parse_dotted_module() {
        let spec = parse("module abc.def.ghi").unwrap();
        let module = spec.module.as_ref().unwrap();
        assert_eq!(module.name(), "abc.def.ghi");
        assert_eq!(module.path.segments.len(), 3);
        assert_eq!(module.path.segments[0].as_str(), "abc");
        assert_eq!(module.path.segments[1].as_str(), "def");
        assert_eq!(module.path.segments[2].as_str(), "ghi");
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
    fn test_parse_scenario_with_alternative() {
        let spec = parse(
            r#"
            scenario "User registration" {
                given {
                    users = {}
                }
                when {
                    result = register("test@example.com")
                }
                then {
                    result is Ok
                }

                alt "email already exists" when { exists u in users where u.email == email } {
                    then {
                        result is Err
                    }
                }
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.scenarios.len(), 1);
        assert_eq!(spec.scenarios[0].alternatives.len(), 1);
        assert_eq!(
            spec.scenarios[0].alternatives[0].name.as_str(),
            "email already exists"
        );
        assert!(spec.scenarios[0].alternatives[0].condition.is_some());
    }

    #[test]
    fn test_parse_alternative_with_different_when() {
        let spec = parse(
            r#"
            scenario "test" {
                given { x = 1 }
                when { result = perform(x) }
                then { result is Ok }

                alt "different action" {
                    when { result = other_perform(x) }
                    then { result is Ok }
                }
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.scenarios[0].alternatives.len(), 1);
        assert!(spec.scenarios[0].alternatives[0].when.is_some());
        assert!(spec.scenarios[0].alternatives[0].condition.is_none());
    }

    #[test]
    fn test_parse_alternative_with_additional_given() {
        let spec = parse(
            r#"
            scenario "test" {
                given { x = 1 }
                when { result = perform(x) }
                then { result is Ok }

                alt "with extra setup" {
                    given { y = 2 }
                    then { result.value == y }
                }
            }
            "#,
        )
        .unwrap();
        assert!(spec.scenarios[0].alternatives[0].given.is_some());
        assert!(spec.scenarios[0].alternatives[0].when.is_none());
    }

    #[test]
    fn test_parse_multiple_alternatives() {
        let spec = parse(
            r#"
            scenario "withdraw funds" {
                given { balance = 100 }
                when { result = withdraw(50) }
                then { balance' == 50 }

                alt "insufficient funds" when { amount > balance } {
                    then { result is Err }
                }

                alt "negative amount" when { amount < 0 } {
                    then { result is Err }
                }

                alt "account frozen" {
                    given { status = Frozen }
                    then { result is Err }
                }
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.scenarios[0].alternatives.len(), 3);
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
    fn test_parse_match_with_block() {
        let spec = parse(
            r#"
            state Test {
                result: Result<Int, String>,

                invariant "match with block" {
                    match result {
                        Ok(n) => {
                            let doubled = n * 2 in doubled > 0
                        },
                        Err(_) => { true }
                    }
                }
            }
            "#,
        )
        .unwrap();
        assert!(!spec.states[0].invariants.is_empty());
    }

    #[test]
    fn test_parse_match_with_multiline_block() {
        let spec = parse(
            r#"
            state Test {
                x: Int,

                invariant "match with multi-expression block" {
                    match x {
                        0 => { false },
                        1 => { true },
                        _ => {
                            x > 0;
                            x < 100
                        }
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

    #[test]
    fn test_parse_type_alias_simple() {
        let spec = parse("type PositiveInt = Int").unwrap();
        assert_eq!(spec.type_aliases.len(), 1);
        assert_eq!(spec.type_aliases[0].name.as_str(), "PositiveInt");
        assert!(spec.type_aliases[0].refinement.is_none());
    }

    #[test]
    fn test_parse_type_alias_with_refinement() {
        let spec = parse("type PositiveInt = Int where self > 0").unwrap();
        assert_eq!(spec.type_aliases.len(), 1);
        assert_eq!(spec.type_aliases[0].name.as_str(), "PositiveInt");
        assert!(spec.type_aliases[0].refinement.is_some());
    }

    #[test]
    fn test_parse_type_alias_generic() {
        let spec = parse("type NonEmptyList<T> = List<T> where self.len() > 0").unwrap();
        assert_eq!(spec.type_aliases.len(), 1);
        assert_eq!(spec.type_aliases[0].name.as_str(), "NonEmptyList");
        assert_eq!(spec.type_aliases[0].type_params.len(), 1);
        assert!(spec.type_aliases[0].refinement.is_some());
    }

    #[test]
    fn test_parse_struct_with_refinement() {
        let spec = parse(
            r#"
            type DateRange {
                start: Int,
                end: Int,
            } where self.start <= self.end
            "#,
        )
        .unwrap();
        assert_eq!(spec.types.len(), 1);
        assert_eq!(spec.types[0].name.as_str(), "DateRange");
        assert!(spec.types[0].refinement.is_some());
    }

    #[test]
    fn test_parse_self_keyword() {
        let spec = parse("type Age = Int where self >= 0 and self < 150").unwrap();
        assert_eq!(spec.type_aliases.len(), 1);
        let refinement = spec.type_aliases[0].refinement.as_ref().unwrap();
        // The refinement should be a binary 'and' expression
        assert!(matches!(refinement.kind, ExprKind::Binary { .. }));
    }

    #[test]
    fn test_parse_type_alias_with_attributes() {
        let spec = parse(
            r#"
            @id(TYPE001)
            type Email = String where self.len() > 0
            "#,
        )
        .unwrap();
        assert_eq!(spec.type_aliases.len(), 1);
        assert_eq!(spec.type_aliases[0].attributes.len(), 1);
        assert_eq!(spec.type_aliases[0].attributes[0].name.as_str(), "id");
    }

    #[test]
    fn test_parse_quality_performance() {
        let spec = parse(
            r#"
            quality performance "API response time" {
                metric: latency,
                target: < 200ms,
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.qualities.len(), 1);
        assert_eq!(
            spec.qualities[0].category,
            crate::ast::QualityCategory::Performance
        );
        assert_eq!(
            spec.qualities[0].description.as_str(),
            "API response time"
        );
        assert_eq!(spec.qualities[0].metric.as_str(), "latency");
        assert_eq!(spec.qualities[0].target.op, crate::ast::QualityOp::Lt);
    }

    #[test]
    fn test_parse_quality_reliability() {
        let spec = parse(
            r#"
            quality reliability "System uptime" {
                metric: uptime,
                target: >= 99%,
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.qualities.len(), 1);
        assert_eq!(
            spec.qualities[0].category,
            crate::ast::QualityCategory::Reliability
        );
        assert_eq!(spec.qualities[0].target.op, crate::ast::QualityOp::GtEq);
        match &spec.qualities[0].target.value {
            crate::ast::QualityValue::Percentage(p) => {
                assert!((*p - 99.0).abs() < 0.01);
            }
            _ => panic!("Expected percentage value"),
        }
    }

    #[test]
    fn test_parse_quality_with_attributes() {
        let spec = parse(
            r#"
            @id("NFR-001")
            @priority(high)
            quality security "Data encryption" {
                metric: encryption_level,
                target: == aes256,
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.qualities.len(), 1);
        assert_eq!(spec.qualities[0].attributes.len(), 2);
        assert_eq!(spec.qualities[0].attributes[0].name.as_str(), "id");
        assert_eq!(spec.qualities[0].attributes[1].name.as_str(), "priority");
    }

    #[test]
    fn test_parse_quality_with_properties() {
        let spec = parse(
            r#"
            quality performance "Request throughput" {
                metric: throughput,
                target: >= 1000/s,
                scale: p99,
            }
            "#,
        )
        .unwrap();
        assert_eq!(spec.qualities.len(), 1);
        // scale is now a dedicated field, not a property
        assert_eq!(spec.qualities[0].scale, Some(crate::ast::Scale::P99));
    }

    #[test]
    fn test_parse_quality_all_categories() {
        let spec = parse(
            r#"
            quality performance "perf" { metric: m, target: > 1, }
            quality reliability "rel" { metric: m, target: > 1, }
            quality security "sec" { metric: m, target: > 1, }
            quality usability "usa" { metric: m, target: > 1, }
            quality scalability "sca" { metric: m, target: > 1, }
            quality maintainability "mai" { metric: m, target: > 1, }
            "#,
        )
        .unwrap();
        assert_eq!(spec.qualities.len(), 6);
        assert_eq!(
            spec.qualities[0].category,
            crate::ast::QualityCategory::Performance
        );
        assert_eq!(
            spec.qualities[1].category,
            crate::ast::QualityCategory::Reliability
        );
        assert_eq!(
            spec.qualities[2].category,
            crate::ast::QualityCategory::Security
        );
        assert_eq!(
            spec.qualities[3].category,
            crate::ast::QualityCategory::Usability
        );
        assert_eq!(
            spec.qualities[4].category,
            crate::ast::QualityCategory::Scalability
        );
        assert_eq!(
            spec.qualities[5].category,
            crate::ast::QualityCategory::Maintainability
        );
    }

    #[test]
    fn test_parse_quality_duration_units() {
        let spec = parse(
            r#"
            quality performance "fast" { metric: latency, target: < 100ms, }
            quality performance "slow" { metric: timeout, target: < 30s, }
            "#,
        )
        .unwrap();
        assert_eq!(spec.qualities.len(), 2);
        match &spec.qualities[0].target.value {
            crate::ast::QualityValue::Duration(100, crate::ast::DurationUnit::Ms) => {}
            _ => panic!("Expected 100ms duration"),
        }
        match &spec.qualities[1].target.value {
            crate::ast::QualityValue::Duration(30, crate::ast::DurationUnit::S) => {}
            _ => panic!("Expected 30s duration"),
        }
    }

    #[test]
    fn test_parse_quality_enhanced_fields() {
        let spec = parse(
            r#"
            quality performance "API response time" {
                metric: latency,
                scale: p99,
                target: < 100ms,
                constraint: hard,
                applies_to: action register,
                measurement: per_request,
                under_load: {
                    concurrent_users: 1000,
                    requests_per_second: 500,
                },
                verified_by: [
                    test "load_test_api",
                    monitor "datadog_latency",
                ],
            }
            "#,
        )
        .unwrap();

        assert_eq!(spec.qualities.len(), 1);
        let q = &spec.qualities[0];

        // Check metric
        assert_eq!(q.metric.as_str(), "latency");

        // Check scale
        assert_eq!(q.scale, Some(crate::ast::Scale::P99));

        // Check target
        assert_eq!(q.target.op, crate::ast::QualityOp::Lt);
        match &q.target.value {
            crate::ast::QualityValue::Duration(100, crate::ast::DurationUnit::Ms) => {}
            _ => panic!("Expected 100ms duration"),
        }

        // Check constraint
        assert_eq!(q.constraint, Some(crate::ast::Constraint::Hard));

        // Check applies_to
        let applies_to = q.applies_to.as_ref().expect("Expected applies_to");
        assert_eq!(applies_to.kind, crate::ast::AppliesToKind::Action);
        assert_eq!(applies_to.name.as_str(), "register");

        // Check measurement
        assert_eq!(
            q.measurement,
            Some(crate::ast::MeasurementPeriod::PerRequest)
        );

        // Check under_load
        let load = q.under_load.as_ref().expect("Expected under_load");
        assert_eq!(load.concurrent_users, Some(1000));
        assert_eq!(load.requests_per_second, Some(500));

        // Check verified_by
        assert_eq!(q.verified_by.len(), 2);
        assert_eq!(q.verified_by[0].kind, crate::ast::VerificationKind::Test);
        assert_eq!(q.verified_by[0].name.as_str(), "load_test_api");
        assert_eq!(q.verified_by[1].kind, crate::ast::VerificationKind::Monitor);
        assert_eq!(q.verified_by[1].name.as_str(), "datadog_latency");
    }

    #[test]
    fn test_parse_quality_scale_variants() {
        let spec = parse(
            r#"
            quality performance "p50" { metric: m, target: > 1, scale: p50, }
            quality performance "p90" { metric: m, target: > 1, scale: p90, }
            quality performance "p95" { metric: m, target: > 1, scale: p95, }
            quality performance "p99" { metric: m, target: > 1, scale: p99, }
            quality performance "p999" { metric: m, target: > 1, scale: p999, }
            quality performance "mean" { metric: m, target: > 1, scale: mean, }
            quality performance "median" { metric: m, target: > 1, scale: median, }
            quality performance "max" { metric: m, target: > 1, scale: max, }
            quality performance "min" { metric: m, target: > 1, scale: min, }
            "#,
        )
        .unwrap();

        assert_eq!(spec.qualities.len(), 9);
        assert_eq!(spec.qualities[0].scale, Some(crate::ast::Scale::P50));
        assert_eq!(spec.qualities[1].scale, Some(crate::ast::Scale::P90));
        assert_eq!(spec.qualities[2].scale, Some(crate::ast::Scale::P95));
        assert_eq!(spec.qualities[3].scale, Some(crate::ast::Scale::P99));
        assert_eq!(spec.qualities[4].scale, Some(crate::ast::Scale::P999));
        assert_eq!(spec.qualities[5].scale, Some(crate::ast::Scale::Mean));
        assert_eq!(spec.qualities[6].scale, Some(crate::ast::Scale::Median));
        assert_eq!(spec.qualities[7].scale, Some(crate::ast::Scale::Max));
        assert_eq!(spec.qualities[8].scale, Some(crate::ast::Scale::Min));
    }

    #[test]
    fn test_parse_quality_measurement_periods() {
        let spec = parse(
            r#"
            quality reliability "req" { metric: m, target: > 1, measurement: per_request, }
            quality reliability "sec" { metric: m, target: > 1, measurement: per_second, }
            quality reliability "min" { metric: m, target: > 1, measurement: per_minute, }
            quality reliability "hr" { metric: m, target: > 1, measurement: hourly, }
            quality reliability "day" { metric: m, target: > 1, measurement: daily, }
            quality reliability "wk" { metric: m, target: > 1, measurement: weekly, }
            quality reliability "mo" { metric: m, target: > 1, measurement: monthly, }
            "#,
        )
        .unwrap();

        use crate::ast::MeasurementPeriod::*;
        assert_eq!(spec.qualities.len(), 7);
        assert_eq!(spec.qualities[0].measurement, Some(PerRequest));
        assert_eq!(spec.qualities[1].measurement, Some(PerSecond));
        assert_eq!(spec.qualities[2].measurement, Some(PerMinute));
        assert_eq!(spec.qualities[3].measurement, Some(Hourly));
        assert_eq!(spec.qualities[4].measurement, Some(Daily));
        assert_eq!(spec.qualities[5].measurement, Some(Weekly));
        assert_eq!(spec.qualities[6].measurement, Some(Monthly));
    }

    #[test]
    fn test_parse_quality_verification_kinds() {
        let spec = parse(
            r#"
            quality performance "v" {
                metric: m,
                target: > 1,
                verified_by: [
                    test "test_name",
                    monitor "monitor_name",
                    benchmark "bench_name",
                    audit "audit_name",
                ],
            }
            "#,
        )
        .unwrap();

        let q = &spec.qualities[0];
        assert_eq!(q.verified_by.len(), 4);

        use crate::ast::VerificationKind::*;
        assert_eq!(q.verified_by[0].kind, Test);
        assert_eq!(q.verified_by[0].name.as_str(), "test_name");
        assert_eq!(q.verified_by[1].kind, Monitor);
        assert_eq!(q.verified_by[1].name.as_str(), "monitor_name");
        assert_eq!(q.verified_by[2].kind, Benchmark);
        assert_eq!(q.verified_by[2].name.as_str(), "bench_name");
        assert_eq!(q.verified_by[3].kind, Audit);
        assert_eq!(q.verified_by[3].name.as_str(), "audit_name");
    }

    #[test]
    fn test_parse_quality_under_load_all_fields() {
        let spec = parse(
            r#"
            quality performance "load" {
                metric: m,
                target: > 1,
                under_load: {
                    concurrent_users: 1000,
                    concurrent_connections: 500,
                    requests_per_second: 10000,
                    payload_size: 1 MB,
                    duration: 5 m,
                },
            }
            "#,
        )
        .unwrap();

        let load = spec.qualities[0]
            .under_load
            .as_ref()
            .expect("Expected under_load");
        assert_eq!(load.concurrent_users, Some(1000));
        assert_eq!(load.concurrent_connections, Some(500));
        assert_eq!(load.requests_per_second, Some(10000));
        assert_eq!(
            load.payload_size,
            Some((1, crate::ast::SizeUnit::Mb))
        );
        assert_eq!(
            load.duration,
            Some((5, crate::ast::DurationUnit::M))
        );
    }
}
