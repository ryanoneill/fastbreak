//! Token definitions for Fastbreak

use logos::Logos;
use smol_str::SmolStr;

/// Tokens in the Fastbreak language
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum Token {
    // ========== Keywords ==========
    /// `use` keyword for imports
    #[token("use")]
    Use,

    /// `type` keyword for type definitions
    #[token("type")]
    Type,

    /// `enum` keyword for enum definitions
    #[token("enum")]
    Enum,

    /// `relation` keyword for relations
    #[token("relation")]
    Relation,

    /// `state` keyword for state definitions
    #[token("state")]
    State,

    /// `action` keyword for action definitions
    #[token("action")]
    Action,

    /// `invariant` keyword
    #[token("invariant")]
    Invariant,

    /// `requires` keyword for preconditions
    #[token("requires")]
    Requires,

    /// `ensures` keyword for postconditions
    #[token("ensures")]
    Ensures,

    /// `scenario` keyword
    #[token("scenario")]
    Scenario,

    /// `alt` keyword for alternative flows
    #[token("alt")]
    Alt,

    /// `quality` keyword for non-functional requirements
    #[token("quality")]
    Quality,

    /// `property` keyword
    #[token("property")]
    Property,

    /// `always` keyword for temporal properties
    #[token("always")]
    Always,

    /// `eventually` keyword for temporal properties
    #[token("eventually")]
    Eventually,

    /// `forall` quantifier
    #[token("forall")]
    Forall,

    /// `exists` quantifier
    #[token("exists")]
    Exists,

    /// `in` keyword
    #[token("in")]
    In,

    /// `where` keyword
    #[token("where")]
    Where,

    /// `match` keyword
    #[token("match")]
    Match,

    /// `if` keyword
    #[token("if")]
    If,

    /// `else` keyword
    #[token("else")]
    Else,

    /// `let` keyword
    #[token("let")]
    Let,

    /// `fn` keyword for functions
    #[token("fn")]
    Fn,

    /// `return` keyword
    #[token("return")]
    Return,

    /// `self` keyword for refinement predicates
    #[token("self")]
    SelfKw,

    // ========== Logical operators ==========
    /// `and` logical operator
    #[token("and")]
    And,

    /// `or` logical operator
    #[token("or")]
    Or,

    /// `not` logical operator
    #[token("not")]
    Not,

    /// `implies` logical operator
    #[token("implies")]
    Implies,

    // ========== Boolean literals ==========
    /// `true` literal
    #[token("true")]
    True,

    /// `false` literal
    #[token("false")]
    False,

    // ========== Relation constraints ==========
    /// `symmetric` constraint
    #[token("symmetric")]
    Symmetric,

    /// `reflexive` constraint
    #[token("reflexive")]
    Reflexive,

    /// `irreflexive` constraint
    #[token("irreflexive")]
    Irreflexive,

    /// `transitive` constraint
    #[token("transitive")]
    Transitive,

    /// `antisymmetric` constraint
    #[token("antisymmetric")]
    Antisymmetric,

    // ========== Type keywords ==========
    /// `is` for type checking
    #[token("is")]
    Is,

    /// `Ok` result variant
    #[token("Ok")]
    Ok,

    /// `Err` result variant
    #[token("Err")]
    Err,

    /// `Some` option variant
    #[token("Some")]
    Some,

    /// `None` option variant
    #[token("None")]
    None,

    // ========== Punctuation ==========
    /// `{`
    #[token("{")]
    LBrace,

    /// `}`
    #[token("}")]
    RBrace,

    /// `(`
    #[token("(")]
    LParen,

    /// `)`
    #[token(")")]
    RParen,

    /// `[`
    #[token("[")]
    LBracket,

    /// `]`
    #[token("]")]
    RBracket,

    /// `<`
    #[token("<")]
    LAngle,

    /// `>`
    #[token(">")]
    RAngle,

    /// `,`
    #[token(",")]
    Comma,

    /// `:`
    #[token(":")]
    Colon,

    /// `::`
    #[token("::")]
    ColonColon,

    /// `;`
    #[token(";")]
    Semicolon,

    /// `.`
    #[token(".")]
    Dot,

    /// `..`
    #[token("..")]
    DotDot,

    /// `->`
    #[token("->")]
    Arrow,

    /// `=>`
    #[token("=>")]
    FatArrow,

    /// `|`
    #[token("|")]
    Pipe,

    /// `_` (wildcard pattern)
    #[token("_", priority = 3)]
    Underscore,

    /// `'` for primed variables (next state)
    #[token("'")]
    Prime,

    /// `@` for attributes
    #[token("@")]
    At,

    // ========== Operators ==========
    /// `=`
    #[token("=")]
    Eq,

    /// `==`
    #[token("==")]
    EqEq,

    /// `!=`
    #[token("!=")]
    NotEq,

    /// `<=`
    #[token("<=")]
    LtEq,

    /// `>=`
    #[token(">=")]
    GtEq,

    /// `+`
    #[token("+")]
    Plus,

    /// `-`
    #[token("-")]
    Minus,

    /// `*`
    #[token("*")]
    Star,

    /// `/`
    #[token("/")]
    Slash,

    /// `%`
    #[token("%")]
    Percent,

    /// `&&`
    #[token("&&")]
    AndAnd,

    /// `||`
    #[token("||")]
    OrOr,

    /// `!`
    #[token("!")]
    Bang,

    // ========== Literals ==========
    /// Float literal (must come before Integer for proper priority)
    /// Matches: 3.14, 99.99, 0.5
    #[regex(r"[0-9]+\.[0-9]+", priority = 3, callback = |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    /// Integer literal (non-negative; negation handled by parser)
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    /// String literal
    #[regex(r#""[^"]*""#, |lex| {
        let s = lex.slice();
        SmolStr::new(&s[1..s.len()-1])
    })]
    String(SmolStr),

    /// Identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| SmolStr::new(lex.slice()))]
    Ident(SmolStr),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Use => write!(f, "use"),
            Token::Type => write!(f, "type"),
            Token::Enum => write!(f, "enum"),
            Token::Relation => write!(f, "relation"),
            Token::State => write!(f, "state"),
            Token::Action => write!(f, "action"),
            Token::Invariant => write!(f, "invariant"),
            Token::Requires => write!(f, "requires"),
            Token::Ensures => write!(f, "ensures"),
            Token::Scenario => write!(f, "scenario"),
            Token::Alt => write!(f, "alt"),
            Token::Quality => write!(f, "quality"),
            Token::Property => write!(f, "property"),
            Token::Always => write!(f, "always"),
            Token::Eventually => write!(f, "eventually"),
            Token::Forall => write!(f, "forall"),
            Token::Exists => write!(f, "exists"),
            Token::In => write!(f, "in"),
            Token::Where => write!(f, "where"),
            Token::Match => write!(f, "match"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::SelfKw => write!(f, "self"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::Implies => write!(f, "implies"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Symmetric => write!(f, "symmetric"),
            Token::Reflexive => write!(f, "reflexive"),
            Token::Irreflexive => write!(f, "irreflexive"),
            Token::Transitive => write!(f, "transitive"),
            Token::Antisymmetric => write!(f, "antisymmetric"),
            Token::Is => write!(f, "is"),
            Token::Ok => write!(f, "Ok"),
            Token::Err => write!(f, "Err"),
            Token::Some => write!(f, "Some"),
            Token::None => write!(f, "None"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::ColonColon => write!(f, "::"),
            Token::Semicolon => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::Pipe => write!(f, "|"),
            Token::Underscore => write!(f, "_"),
            Token::Prime => write!(f, "'"),
            Token::At => write!(f, "@"),
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::AndAnd => write!(f, "&&"),
            Token::OrOr => write!(f, "||"),
            Token::Bang => write!(f, "!"),
            Token::Float(n) => write!(f, "{n}"),
            Token::Integer(n) => write!(f, "{n}"),
            Token::String(s) => write!(f, "\"{s}\""),
            Token::Ident(s) => write!(f, "{s}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<Token> {
        Token::lexer(source).filter_map(Result::ok).collect()
    }

    #[test]
    fn test_keywords() {
        // 'module' is now contextual (identifier)
        assert_eq!(lex("module"), vec![Token::Ident("module".into())]);
        assert_eq!(lex("use"), vec![Token::Use]);
        assert_eq!(lex("type"), vec![Token::Type]);
        assert_eq!(lex("enum"), vec![Token::Enum]);
        assert_eq!(lex("state"), vec![Token::State]);
        assert_eq!(lex("action"), vec![Token::Action]);
        assert_eq!(lex("scenario"), vec![Token::Scenario]);
        assert_eq!(lex("property"), vec![Token::Property]);
    }

    #[test]
    fn test_quantifiers() {
        assert_eq!(lex("forall"), vec![Token::Forall]);
        assert_eq!(lex("exists"), vec![Token::Exists]);
        assert_eq!(lex("in"), vec![Token::In]);
        assert_eq!(lex("where"), vec![Token::Where]);
    }

    #[test]
    fn test_logical_operators() {
        assert_eq!(lex("and"), vec![Token::And]);
        assert_eq!(lex("or"), vec![Token::Or]);
        assert_eq!(lex("not"), vec![Token::Not]);
        assert_eq!(lex("implies"), vec![Token::Implies]);
    }

    #[test]
    fn test_comparison_operators() {
        assert_eq!(lex("=="), vec![Token::EqEq]);
        assert_eq!(lex("!="), vec![Token::NotEq]);
        assert_eq!(lex("<="), vec![Token::LtEq]);
        assert_eq!(lex(">="), vec![Token::GtEq]);
        assert_eq!(lex("<"), vec![Token::LAngle]);
        assert_eq!(lex(">"), vec![Token::RAngle]);
    }

    #[test]
    fn test_arrows() {
        assert_eq!(lex("->"), vec![Token::Arrow]);
        assert_eq!(lex("=>"), vec![Token::FatArrow]);
    }

    #[test]
    fn test_prime() {
        assert_eq!(
            lex("users'"),
            vec![Token::Ident("users".into()), Token::Prime]
        );
    }

    #[test]
    fn test_integers() {
        assert_eq!(lex("42"), vec![Token::Integer(42)]);
        // Negative integers are lexed as Minus followed by Integer
        assert_eq!(lex("-17"), vec![Token::Minus, Token::Integer(17)]);
        assert_eq!(lex("0"), vec![Token::Integer(0)]);
    }

    #[test]
    fn test_floats() {
        assert_eq!(lex("3.14"), vec![Token::Float(3.14)]);
        assert_eq!(lex("99.99"), vec![Token::Float(99.99)]);
        assert_eq!(lex("0.5"), vec![Token::Float(0.5)]);
        // Negative floats are lexed as Minus followed by Float
        assert_eq!(lex("-3.14"), vec![Token::Minus, Token::Float(3.14)]);
        // Integer followed by dot and identifier should NOT be a float
        assert_eq!(
            lex("99.name"),
            vec![Token::Integer(99), Token::Dot, Token::Ident("name".into())]
        );
    }

    #[test]
    fn test_percent() {
        assert_eq!(lex("%"), vec![Token::Percent]);
        // Percentage value: 99.99%
        assert_eq!(
            lex("99.99%"),
            vec![Token::Float(99.99), Token::Percent]
        );
        // Integer percentage: 100%
        assert_eq!(lex("100%"), vec![Token::Integer(100), Token::Percent]);
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            lex(r#""hello world""#),
            vec![Token::String("hello world".into())]
        );
        assert_eq!(lex(r#""""#), vec![Token::String("".into())]);
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(lex("foo"), vec![Token::Ident("foo".into())]);
        assert_eq!(lex("FooBar"), vec![Token::Ident("FooBar".into())]);
        assert_eq!(lex("foo_bar"), vec![Token::Ident("foo_bar".into())]);
        assert_eq!(lex("foo123"), vec![Token::Ident("foo123".into())]);
        assert_eq!(lex("_private"), vec![Token::Ident("_private".into())]);
    }

    #[test]
    fn test_type_definition() {
        let tokens = lex("type User { id: UserId, email: Email }");
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
                Token::Ident("email".into()),
                Token::Colon,
                Token::Ident("Email".into()),
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_generic_types() {
        // Built-in types are now lexed as identifiers
        let tokens = lex("Set<User>");
        assert_eq!(
            tokens,
            vec![
                Token::Ident("Set".into()),
                Token::LAngle,
                Token::Ident("User".into()),
                Token::RAngle,
            ]
        );

        let tokens = lex("Map<String, Int>");
        assert_eq!(
            tokens,
            vec![
                Token::Ident("Map".into()),
                Token::LAngle,
                Token::Ident("String".into()),
                Token::Comma,
                Token::Ident("Int".into()),
                Token::RAngle,
            ]
        );
    }

    #[test]
    fn test_invariant() {
        let tokens = lex(r#"invariant "unique emails" { forall u in users => true }"#);
        assert_eq!(
            tokens,
            vec![
                Token::Invariant,
                Token::String("unique emails".into()),
                Token::LBrace,
                Token::Forall,
                Token::Ident("u".into()),
                Token::In,
                Token::Ident("users".into()),
                Token::FatArrow,
                Token::True,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_scenario_keywords() {
        // given, when, then are now contextual (identifiers)
        let tokens = lex("scenario given when then alt");
        assert_eq!(
            tokens,
            vec![
                Token::Scenario,
                Token::Ident("given".into()),
                Token::Ident("when".into()),
                Token::Ident("then".into()),
                Token::Alt,
            ]
        );
    }

    #[test]
    fn test_quality_keywords() {
        // Only 'quality' is a keyword; category names are now identifiers
        let tokens = lex("quality performance reliability security scalability");
        assert_eq!(
            tokens,
            vec![
                Token::Quality,
                Token::Ident("performance".into()),
                Token::Ident("reliability".into()),
                Token::Ident("security".into()),
                Token::Ident("scalability".into()),
            ]
        );
        // metric and target are now identifiers (contextual in quality blocks)
        let tokens = lex("metric target");
        assert_eq!(
            tokens,
            vec![Token::Ident("metric".into()), Token::Ident("target".into())]
        );
    }

    #[test]
    fn test_comments_are_skipped() {
        let tokens = lex("type // this is a comment\nUser");
        assert_eq!(tokens, vec![Token::Type, Token::Ident("User".into())]);
    }

    #[test]
    fn test_whitespace_is_skipped() {
        let tokens = lex("type   \n\t  User");
        assert_eq!(tokens, vec![Token::Type, Token::Ident("User".into())]);
    }

    #[test]
    fn test_path_syntax() {
        let tokens = lex("common::types::Email");
        assert_eq!(
            tokens,
            vec![
                Token::Ident("common".into()),
                Token::ColonColon,
                Token::Ident("types".into()),
                Token::ColonColon,
                Token::Ident("Email".into()),
            ]
        );
    }

    #[test]
    fn test_action_signature() {
        let tokens = lex("action register(email: Email) -> Result<User, Error>");
        assert_eq!(
            tokens,
            vec![
                Token::Action,
                Token::Ident("register".into()),
                Token::LParen,
                Token::Ident("email".into()),
                Token::Colon,
                Token::Ident("Email".into()),
                Token::RParen,
                Token::Arrow,
                Token::Ident("Result".into()),
                Token::LAngle,
                Token::Ident("User".into()),
                Token::Comma,
                Token::Ident("Error".into()),
                Token::RAngle,
            ]
        );
    }

    #[test]
    fn test_attribute() {
        let tokens = lex("@id(REQ-001)");
        assert_eq!(
            tokens,
            vec![
                Token::At,
                Token::Ident("id".into()),
                Token::LParen,
                Token::Ident("REQ".into()),
                Token::Minus,
                Token::Integer(1),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_attribute_with_string() {
        let tokens = lex(r#"@rationale("test reason")"#);
        assert_eq!(
            tokens,
            vec![
                Token::At,
                Token::Ident("rationale".into()),
                Token::LParen,
                Token::String("test reason".into()),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_self_keyword() {
        assert_eq!(lex("self"), vec![Token::SelfKw]);
        // Self in refinement context
        let tokens = lex("where self > 0");
        assert_eq!(
            tokens,
            vec![
                Token::Where,
                Token::SelfKw,
                Token::RAngle,
                Token::Integer(0),
            ]
        );
    }
}
