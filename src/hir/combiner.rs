//! The implementation of a high-level intermediate representation (HIR)
//! combiner.
use std::{collections::HashSet, fmt, mem, result};

use thiserror::Error;

use super::{Hir, Root, Target};
use bulloak_syntax::{utils::lower_first_letter, FrontendError, Span};

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

impl FrontendError<ErrorKind> for Error {
    /// Return the type of this error.
    fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// The original text string in which this error occurred.
    fn text(&self) -> &str {
        &self.text
    }

    /// Return the span at which this error occurred.
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_error(f)
    }
}

type Identifier = String;
type Index = usize;

/// The type of an error that occurred while combining HIRs.
#[derive(Error, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    /// This happens when the module name in the identifier of one HIR does
    /// not match the module name in the identifier of another HIR.
    #[error("module name mismatch: expected '{expected}', found '{actual}'")]
    ModuleNameMismatch {
        /// The name found in the current tree being analyzed.
        actual: Identifier,
        /// The name expected to be found during analysis.
        expected: Identifier,
    },

    /// No module name was found in one of the tree roots.
    #[error("module name missing at tree root #{0}")]
    ModuleNameMissing(Index),

    /// A `::` was missing in
    /// one of the tree roots.
    #[error("separator missing at tree root #{0}. Expected to find `::` between the module name and the function name when multiple roots exist")]
    SeparatorMissing(Index),
}

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
    /// iterating over each HIR and merging their children into the root
    /// definition of the first HIR, while filtering out duplicate modifiers.
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

        let target = &mut Target::default();
        let mut unique_modifiers = HashSet::new();

        for (idx, hir) in hirs.into_iter().enumerate() {
            let Hir::Root(r) = hir else {
                unreachable!();
            };

            for child in r.children {
                let Hir::Target(module) = child else {
                    // For now we ignore everything that isn't a target.
                    continue;
                };

                // ModuleName::function_name -> (ModuleName, function_name)
                //
                // Errors if `::` isn't present.
                let (module_name, function_name) = module
                    .identifier
                    .split_once("::")
                    .ok_or(self.error(Span::default(), ErrorKind::SeparatorMissing(idx + 1)))?;

                if module_name.trim().is_empty() {
                    return Err(self.error(Span::default(), ErrorKind::ModuleNameMissing(idx + 1)));
                }

                // If the accumulated identifier is empty, we're on the first
                // module.
                if target.identifier.is_empty() {
                    // Add modifiers to the list of added modifiers and prefix
                    // test names.
                    let children = module
                        .children
                        .into_iter()
                        .map(|c| prefix_test(c, function_name))
                        .filter_map(|c| collect_modifier(c, &mut unique_modifiers))
                        .collect();
                    let first_module = Target {
                        identifier: module_name.to_owned(),
                        children,
                    };
                    *target = first_module;
                    continue;
                }

                // If the current module name doesn't match, we error.
                if module_name != target.identifier {
                    return Err(self.error(
                        Span::default(),
                        ErrorKind::ModuleNameMismatch {
                            actual: module_name.to_owned(),
                            expected: target.identifier.clone(),
                        },
                    ));
                }

                let children =
                    update_children(module.children, function_name, &mut unique_modifiers);
                target.children.extend(children);
            }
        }

        let combined_root = Hir::Root(Root {
            children: vec![Hir::Target(mem::take(target))],
        });
        Ok(combined_root)
    }
}

/// Prefix function names and filter modifiers.
fn update_children(
    children: Vec<Hir>,
    function_identifier: &str,
    unique_modifiers: &mut HashSet<String>,
) -> Vec<Hir> {
    children
        .into_iter()
        .map(|c| prefix_test(c, function_identifier))
        .filter_map(|c| collect_modifier(c, unique_modifiers))
        .collect()
}

fn prefix_test(child: Hir, prefix: &str) -> Hir {
    let Hir::FunctionDefinition(mut test_or_modifier) = child else {
        return child;
    };
    if test_or_modifier.is_test() {
        test_or_modifier.identifier = prefix_test_with(&test_or_modifier.identifier, prefix);
    }
    Hir::FunctionDefinition(test_or_modifier)
}

/// Prefix the suffix of a test name.
fn prefix_test_with(test_name: &str, prefix: &str) -> String {
    let fn_name = lower_first_letter(prefix);
    let test_suffix = test_name.trim_start_matches("test_");
    format!("test_{fn_name}_{test_suffix}")
}

fn collect_modifier(child: Hir, unique_modifiers: &mut HashSet<String>) -> Option<Hir> {
    let Hir::FunctionDefinition(test_or_modifier) = child else {
        return Some(child);
    };

    // If child is of type `FunctionDefinition` with the same identifier
    // as a child of another `Target` of ty `Modifier`, then
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
    use bulloak_syntax::{parse_one, Position, Span};
    use pretty_assertions::assert_eq;

    use crate::{
        config::Config,
        hir::{self, Hir},
        scaffold::modifiers,
    };

    fn translate(text: &str) -> Result<Hir> {
        let ast = parse_one(&text)?;
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

    fn target(identifier: &str, children: Vec<Hir>) -> Hir {
        Hir::Target(hir::Target {
            identifier: identifier.to_owned(),
            children,
        })
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
            "::orphaned_function\n└── when something bad happens\n   └── it should revert",
            "Contract::function\n└── when something bad happens\n   └── it should revert",
        ];
        let hirs = trees.iter().map(|tree| translate(tree).unwrap()).collect();
        let text = trees.join("\n\n");
        let result = combine(&text, hirs);

        let expected = r"•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
bulloak error: module name missing at tree root #1";
        assert!(result.is_err_and(|e| { e.to_string() == expected }));
    }

    #[test]
    fn errors_when_root_contract_identifier_is_missing_second() {
        let trees = vec![
            "Contract::function\n└── when something bad happens\n   └── it should revert",
            "::orphaned_function\n└── when something bad happens\n   └── it should revert",
        ];
        let hirs = trees.iter().map(|tree| translate(tree).unwrap()).collect();
        let text = trees.join("\n\n");
        let result = combine(&text, hirs);

        let expected = r"•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
bulloak error: module name missing at tree root #2";
        assert!(result.is_err_and(|e| { e.to_string() == expected }));
    }

    #[test]
    fn errors_when_module_names_mismatch() {
        let trees = vec![
            "Contract::function\n└── when something bad happens\n   └── it should revert",
            "Different::orphaned_function\n└── when something bad happens\n   └── it should revert",
        ];
        let hirs = trees.iter().map(|tree| translate(tree).unwrap()).collect();
        let text = trees.join("\n\n");
        let result = combine(&text, hirs);

        let expected = r"•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
bulloak error: module name mismatch: expected 'Contract', found 'Different'";
        assert!(result.is_err_and(|e| { e.to_string() == expected }));
    }

    #[test]
    fn skips_non_target_children() {
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
            vec![target(
                "Contract",
                vec![
                    function(
                        "test_function1_panic_when_something_bad_happens".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(20, 2, 1), Position::new(86, 3, 24)),
                        None,
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                    function(
                        "test_function2_panic_when_something_shit_happens".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(20, 2, 1), Position::new(87, 3, 24)),
                        None,
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                ]
            )]
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
            vec![target(
                "Contract",
                vec![
                    function(
                        "when_something_bad_happens".to_owned(),
                        hir::FunctionTy::Modifier,
                        Span::new(Position::new(20, 2, 1), Position::new(133, 4, 28)),
                        None,
                        None
                    ),
                    function(
                        "test_function1_panic_given_something_else_happens".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(61, 3, 5), Position::new(133, 4, 28)),
                        Some(vec!["when_something_bad_happens".to_owned()]),
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                    function(
                        "test_function2_panic_given_the_caller_is_0x1337".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(61, 3, 5), Position::new(131, 4, 28)),
                        Some(vec!["when_something_bad_happens".to_owned()]),
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                ]
            )]
        );
    }
}
