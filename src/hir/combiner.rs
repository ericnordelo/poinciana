//! The implementation of a high-level intermediate representation (HIR)
//! combiner.
use std::{collections::HashSet, mem, result};

use thiserror::Error;

use super::{Hir, Root};
use crate::span::Span;

type Result<T> = result::Result<T, Error>;

/// An error that occurred while combining HIRs.
#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub struct Error {
    /// The kind of error.
    #[source]
    kind: ErrorKind,
    /// The original text that the parser generated the error from. Every
    /// span in an error is a valid range into this string.
    text: String,
    /// The span of this error.
    span: Span,
}

impl Error {
    /// Return the type of this error.
    #[must_use]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// The original text string in which this error occurred.
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Return the span at which this error occurred.
    #[must_use]
    pub fn span(&self) -> &Span {
        &self.span
    }
}

type Identifier = String;
type Index = usize;

/// The type of an error that occurred while combining HIRs.
#[derive(Error, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {}

/// A high-level intermediate representation (HIR) combiner.
///
/// It takes a vector of HIRs and combines them into a single HIR
/// by appending the function nodes to the root contract node.
pub struct Combiner;

impl Default for Combiner {
    fn default() -> Self {
        Self::new()
    }
}

impl Combiner {
    /// Creates a new combiner.
    #[must_use]
    pub fn new() -> Self {
        Combiner {}
    }

    /// Combines the translated HIRs into a single HIR. HIRs are merged by
    /// iterating over each HIR and merging their children into the contract
    /// definition of the first HIR, while verifying the contract identifiers
    /// match and filtering out duplicate modifiers.
    pub fn combine(self, text: &str, hirs: Vec<Hir>) -> Result<Hir> {
        CombinerI::new(text).combine(hirs)
    }
}

struct CombinerI<'t> {
    /// The input text.
    text: &'t str,
}

impl<'t> CombinerI<'t> {
    /// Creates a new combiner.
    fn new(text: &'t str) -> Self {
        CombinerI { text }
    }

    /// Create a new error with the given span and error type.
    fn error(&self, span: Span, kind: ErrorKind) -> Error {
        Error {
            kind,
            text: self.text.to_owned(),
            span,
        }
    }

    /// Internal implementation of `Combiner::combine`.
    fn combine(&self, hirs: Vec<Hir>) -> Result<Hir> {
        // For `.tree` files with a single root, we don't need to do any work.
        if hirs.len() == 1 {
            return Ok(hirs[0].clone());
        }

        let combined_root = &mut Root::default();
        let mut unique_modifiers = HashSet::new();

        for (_, hir) in hirs.into_iter().enumerate() {
            let Hir::Root(r) = hir else {
                unreachable!();
            };

            let children = update_children(r.children, &mut unique_modifiers);
            combined_root.children.extend(children);
        }

        Ok(Hir::Root(mem::take(combined_root)))
    }
}

/// Filter modifiers.
fn update_children(children: Vec<Hir>, unique_modifiers: &mut HashSet<String>) -> Vec<Hir> {
    children
        .into_iter()
        .filter_map(|c| collect_modifier(c, unique_modifiers))
        .collect()
}

fn collect_modifier(child: Hir, unique_modifiers: &mut HashSet<String>) -> Option<Hir> {
    let Hir::FunctionDefinition(test_or_modifier) = child else {
        return Some(child);
    };

    // If child is of type `FunctionDefinition` with the same identifier
    // as a child of another `ContractDefinition` of ty `Modifier`, then
    // they are duplicates.
    if unique_modifiers.contains(&test_or_modifier.identifier) {
        return None;
    }

    unique_modifiers.insert(test_or_modifier.identifier.clone());
    Some(Hir::FunctionDefinition(test_or_modifier.clone()))
}

#[cfg(test)]
mod tests {
    use anyhow::{Error, Result};
    use pretty_assertions::assert_eq;

    use crate::{
        config::Config,
        hir::{self, Hir},
        scaffold::modifiers,
        span::{Position, Span},
        syntax::tree::{parser::Parser, tokenizer::Tokenizer},
    };

    fn translate(text: &str) -> Result<Hir> {
        let tokens = Tokenizer::new().tokenize(&text)?;
        let ast = Parser::new().parse(&text, &tokens)?;
        let mut discoverer = modifiers::ModifierDiscoverer::new();
        let modifiers = discoverer.discover(&ast);

        let cfg: Config = Default::default();
        Ok(hir::translator::Translator::new().translate(&ast, modifiers, &cfg))
    }

    fn combine(text: &str, hirs: Vec<Hir>) -> Result<Hir, Error> {
        Ok(crate::hir::combiner::Combiner::new().combine(text, hirs)?)
    }

    fn root(children: Vec<Hir>) -> Hir {
        Hir::Root(hir::Root { children })
    }

    fn function(
        identifier: String,
        ty: hir::FunctionTy,
        span: Span,
        modifiers: Option<Vec<String>>,
        children: Option<Vec<Hir>>,
    ) -> Hir {
        Hir::FunctionDefinition(hir::FunctionDefinition {
            identifier,
            ty,
            span,
            modifiers,
            children,
        })
    }

    fn comment(lexeme: String) -> Hir {
        Hir::Comment(hir::Comment { lexeme })
    }

    #[test]
    fn errors_when_root_contract_identifier_is_missing() {
        let trees = vec![
            "::orphanedFunction\n└── when something bad happens\n   └── it should revert",
            "Contract::function\n└── when something bad happens\n   └── it should revert",
        ];
        let hirs = trees.iter().map(|tree| translate(tree).unwrap()).collect();
        let text = trees.join("\n\n");
        let result = combine(&text, hirs);

        assert!(result.is_err());
    }

    #[test]
    fn errors_when_contract_names_mismatch() {
        let trees = vec![
            "Contract::function\n└── when something bad happens\n   └── it should revert",
            "::orphanedFunction\n└── when something bad happens\n   └── it should revert",
        ];
        let hirs = trees.iter().map(|tree| translate(tree).unwrap()).collect();

        let expected = r"•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
bulloak error: contract name missing at tree root #2";

        let text = trees.join("\n\n");
        match combine(&text, hirs) {
            Err(e) => assert_eq!(e.to_string(), expected),
            _ => unreachable!("expected an error"),
        }
    }

    #[test]
    fn skips_non_function_children() {
        let trees = vec![
            "Contract::function1\n└── when something bad happens\n    └── it should revert",
            "Contract::function2\n└── when something shit happens\n    └── it should revert",
        ];
        let mut hirs: Vec<_> = trees.iter().map(|tree| translate(tree).unwrap()).collect();

        // Append a comment HIR to the hirs.
        hirs.push(root(vec![comment("this is a random comment".to_owned())]));

        let text = trees.join("\n\n");
        let children = match combine(&text, hirs).unwrap() {
            Hir::Root(root) => root.children,
            _ => unreachable!(),
        };

        assert_eq!(
            children,
            vec![
                function(
                    "test_Function1RevertWhen_SomethingBadHappens".to_owned(),
                    hir::FunctionTy::Test,
                    Span::new(Position::new(20, 2, 1), Position::new(86, 3, 24)),
                    None,
                    Some(vec![comment("it should revert".to_owned()),])
                ),
                function(
                    "test_Function2RevertWhen_SomethingShitHappens".to_owned(),
                    hir::FunctionTy::Test,
                    Span::new(Position::new(20, 2, 1), Position::new(87, 3, 24)),
                    None,
                    Some(vec![comment("it should revert".to_owned()),])
                ),
            ]
        );
    }

    #[test]
    fn dedups_cross_root_modifiers() {
        let trees = vec![
            "Contract::function1\n└── when something bad happens\n    └── given something else happens\n        └── it should revert",
            "Contract::function2\n└── when something bad happens\n    └── given the caller is 0x1337\n        └── it should revert",
        ];
        let mut hirs: Vec<_> = trees.iter().map(|tree| translate(tree).unwrap()).collect();

        // Append a comment HIR to the hirs.
        hirs.push(root(vec![comment("this is a random comment".to_owned())]));

        let text = trees.join("\n\n");
        let children = match combine(&text, hirs).unwrap() {
            Hir::Root(root) => root.children,
            _ => unreachable!(),
        };

        assert_eq!(
            children,
            vec![
                function(
                    "whenSomethingBadHappens".to_owned(),
                    hir::FunctionTy::Modifier,
                    Span::new(Position::new(20, 2, 1), Position::new(133, 4, 28)),
                    None,
                    None
                ),
                function(
                    "test_Function1RevertGiven_SomethingElseHappens".to_owned(),
                    hir::FunctionTy::Test,
                    Span::new(Position::new(61, 3, 5), Position::new(133, 4, 28)),
                    Some(vec!["whenSomethingBadHappens".to_owned()]),
                    Some(vec![comment("it should revert".to_owned()),])
                ),
                function(
                    "test_Function2RevertGiven_TheCallerIs0x1337".to_owned(),
                    hir::FunctionTy::Test,
                    Span::new(Position::new(61, 3, 5), Position::new(131, 4, 28)),
                    Some(vec!["whenSomethingBadHappens".to_owned()]),
                    Some(vec![comment("it should revert".to_owned()),])
                ),
            ]
        );
    }
}
