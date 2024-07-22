//! The implementation of a translator between a poinciana tree AST and a
//! high-level intermediate representation (HIR) -- AST -> HIR.
use crate::{
    config::Config,
    hir::{self, Hir},
};
use bulloak_syntax::{
    utils::{lower_first_letter, sanitize},
    Action, Ast, Condition, Description, Root, Visitor,
};
use indexmap::IndexMap;

/// A translator between a poinciana tree abstract syntax tree (AST)
/// and a high-level intermediate representation (HIR) -- AST -> HIR.
///
/// It visits an AST in depth-first order an generates a HIR
/// as a result.
#[derive(Default)]
pub struct Translator;

impl Translator {
    /// Create a new translator.
    #[must_use]
    pub const fn new() -> Self {
        Self {}
    }

    /// Translate an AST to a HIR.
    ///
    /// This function is the entry point of the translator.
    #[must_use]
    pub fn translate(&self, ast: &Ast, modifiers: &IndexMap<String, String>, cfg: &Config) -> Hir {
        TranslatorI::new(modifiers, cfg).translate(ast)
    }
}

/// The internal implementation of the Translator.
struct TranslatorI<'a> {
    /// A stack of modifiers that will be applied to the
    /// currently visited function.
    ///
    /// This stack is updated as the translator traverses the AST.
    /// When the translator finishes traversing a condition, it
    /// pops the last modifier from the stack, since it won't
    /// be applied to the next function. The rest of the modifiers
    /// might be applied in case there are more sibling actions or
    /// conditions.
    modifier_stack: Vec<&'a str>,
    /// A map of condition titles to their corresponding modifiers.
    ///
    /// This map is used to retrieve a modifier given a condition title
    /// to improve performance. Otherwise each title would be converted
    /// to a modifier every time it is used.
    modifiers: &'a IndexMap<String, String>,
}

impl<'a> TranslatorI<'a> {
    /// Creates a new internal translator.
    fn new(modifiers: &'a IndexMap<String, String>, _cfg: &Config) -> Self {
        Self {
            modifier_stack: Vec::new(),
            modifiers,
        }
    }

    /// Concrete implementation of the translation from AST to HIR.
    fn translate(&mut self, ast: &Ast) -> Hir {
        let mut hirs = match ast {
            Ast::Root(ref root) => self.visit_root(root).unwrap(),
            _ => unreachable!(),
        };

        // The result of translating is a Vec<Hir> where the only member
        // is a Root HIR node.
        std::mem::take(&mut hirs[0])
    }
}

impl<'a> Visitor for TranslatorI<'a> {
    type Error = ();
    type Output = Vec<Hir>;

    fn visit_root(&mut self, root: &Root) -> Result<Self::Output, Self::Error> {
        let mut target_children = Vec::new();

        for ast in &root.children {
            match ast {
                // Root or ActionDescription nodes cannot be children of a root
                // node. This must be handled in a previous
                // pass.
                Ast::Root(_) | Ast::ActionDescription(_) => {
                    unreachable!()
                }
                // Found a top-level action. This corresponds to a function.
                Ast::Action(action) => {
                    let words = action.title.split_whitespace();
                    let words = words.skip(1); // Removes "it" from the test name.

                    // Map an iterator over the words of an action to the test
                    // name.
                    //
                    // Example: [do, stuff] -> _do_stuff
                    let test_name =
                        words.fold(String::with_capacity(action.title.len()), |mut acc, w| {
                            acc.reserve(w.len() + 1);
                            acc.push_str(&format!("_{}", w));
                            acc
                        });

                    // We need to sanitize here and not in a previous compiler
                    // phase because we want to emit the action as is in a
                    // comment.
                    let test_name = sanitize(&test_name);
                    let test_name = format!("test{test_name}");

                    let hirs = self.visit_action(action)?;

                    let hir = Hir::FunctionDefinition(hir::FunctionDefinition {
                        identifier: test_name,
                        ty: hir::FunctionTy::Test,
                        span: action.span,
                        modifiers: None,
                        children: Some(hirs),
                    });
                    target_children.push(hir);
                }
                Ast::Condition(condition) => {
                    target_children.append(&mut self.visit_condition(condition)?);
                }
            }
        }

        let root_children = vec![Hir::Target(hir::Target {
            identifier: root.contract_name.clone(),
            children: target_children,
        })];

        Ok(vec![Hir::Root(hir::Root {
            children: root_children,
        })])
    }

    fn visit_condition(&mut self, condition: &Condition) -> Result<Self::Output, Self::Error> {
        let mut children = Vec::new();

        let action_count = condition
            .children
            .iter()
            .filter(|child| Ast::is_action(child))
            .count();
        // If this condition only has actions as children, then we don't
        // generate a modifier for it, since it would only be used in
        // the emitted function.
        if condition.children.len() != action_count {
            if let Some(modifier) = self.modifiers.get(&condition.title) {
                self.modifier_stack.push(modifier);
                // Add a modifier node.
                let hir = Hir::FunctionDefinition(hir::FunctionDefinition {
                    identifier: modifier.clone(),
                    ty: hir::FunctionTy::Modifier,
                    span: condition.span,
                    modifiers: None,
                    children: None,
                });
                children.push(hir);
            };
        }

        // We first visit all actions in order to keep the functions
        // in the same order that they appear in the source .tree text.
        let mut actions = Vec::new();
        for action in &condition.children {
            if let Ast::Action(action) = action {
                actions.append(&mut self.visit_action(action)?);
            }
        }

        // Add this condition's function definition if it has children actions.
        if !actions.is_empty() {
            // If the only action is `it should revert`, we slightly change the
            // function name to reflect this.
            let is_panic = actions.first().is_some_and(|action| {
                if let hir::Hir::Comment(comment) = action {
                    let sanitized_lexeme = sanitize(&comment.lexeme.trim().to_lowercase());
                    sanitized_lexeme == "it should revert"
                } else {
                    false
                }
            });

            let mut words = condition.title.split_whitespace();
            // It is fine to unwrap because conditions have at least one word in
            // them.
            let keyword = lower_first_letter(words.next().unwrap());

            let function_name = if is_panic {
                // Map an iterator over the words of a condition to the test
                // name.
                //
                // Example: [when, something, happens] -> _when_something_happens
                let test_name = words.fold(
                    String::with_capacity(condition.title.len() - keyword.len()),
                    |mut acc, w| {
                        if !acc.is_empty() {
                            acc.reserve(w.len() + 1);
                            acc.push_str(&format!("_{}", w));
                        } else {
                            acc.reserve(w.len());
                            acc.push_str(&w);
                        }
                        acc
                    },
                );

                // The structure for a function name when it is a panic is:
                //
                // test_panic_[KEYWORD]_name
                //
                // where `KEYWORD` is the starting word of the condition.
                format!("test_panic_{keyword}_{test_name}")
            } else {
                // Map an iterator over the words of a condition to the test
                // name.
                //
                // Example: [when, something, happens] -> WhenSomethingHappens
                let test_name = words.fold(keyword, |mut acc, w| {
                    acc.reserve(w.len() + 1);
                    acc.push_str(&format!("_{}", w));
                    acc
                });

                format!("test_{test_name}")
            };

            let modifiers = if self.modifier_stack.is_empty() {
                None
            } else {
                Some(self.modifier_stack.iter().map(|&m| m.to_owned()).collect())
            };

            let hir = Hir::FunctionDefinition(hir::FunctionDefinition {
                identifier: function_name,
                ty: hir::FunctionTy::Test,
                span: condition.span,
                modifiers,
                children: Some(actions),
            });
            children.push(hir);
        }

        // Then we recursively visit all child conditions.
        for condition in &condition.children {
            if let Ast::Condition(condition) = condition {
                children.append(&mut self.visit_condition(condition)?);
            }
        }

        if condition.children.len() != action_count {
            self.modifier_stack.pop();
        }

        Ok(children)
    }

    fn visit_action(&mut self, action: &Action) -> Result<Self::Output, Self::Error> {
        let mut descriptions = vec![];
        for description in &action.children {
            if let Ast::ActionDescription(description) = description {
                descriptions.append(&mut self.visit_description(description)?);
            }
        }

        Ok(std::iter::once(hir::Hir::Comment(hir::Comment {
            lexeme: action.title.clone(),
        }))
        .chain(descriptions)
        .collect())
    }

    fn visit_description(
        &mut self,
        description: &Description,
    ) -> Result<Self::Output, Self::Error> {
        Ok(vec![hir::Hir::Comment(hir::Comment {
            lexeme: description.text.clone(),
        })])
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use bulloak_syntax::{parse_one, Position, Span};
    use pretty_assertions::assert_eq;

    use crate::{
        config::Config,
        hir::{self, Hir},
        scaffold::modifiers,
    };

    fn translate(text: &str) -> Result<hir::Hir> {
        let ast = parse_one(&text)?;
        let mut discoverer = modifiers::ModifierDiscoverer::new();
        let modifiers = discoverer.discover(&ast);

        let cfg: Config = Default::default();
        Ok(hir::translator::Translator::new().translate(&ast, modifiers, &cfg))
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
    fn one_child() {
        let file_contents = "FooTest\n└── when something bad happens\n   └── it should revert";
        assert_eq!(
            translate(file_contents).unwrap(),
            root(vec![target(
                "FooTest",
                vec![function(
                    "test_panic_when_something_bad_happens".to_owned(),
                    hir::FunctionTy::Test,
                    Span::new(Position::new(9, 2, 1), Position::new(74, 3, 23)),
                    None,
                    Some(vec![comment("it should revert".to_owned()),])
                ),]
            )])
        );
    }

    #[test]
    fn two_children() {
        let file_contents = r"FooBarTheBestTest
├── when stuff called
│  └── it should revert
└── given not stuff called
   └── it should revert";
        assert_eq!(
            translate(file_contents).unwrap(),
            root(vec![target(
                "FooBarTheBestTest",
                vec![
                    function(
                        "test_panic_when_stuff_called".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(18, 2, 1), Position::new(76, 3, 23)),
                        None,
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                    function(
                        "test_panic_given_not_stuff_called".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(78, 4, 1), Position::new(139, 5, 23)),
                        None,
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                ]
            )],)
        );
    }

    #[test]
    fn action_with_sibling_condition() -> Result<()> {
        let file_contents = String::from(
            r"
FooTest
└── when stuff called
    ├── It should do stuff.
    ├── when a called
    │   └── it should revert
    ├── It should do more.
    └── when b called
        └── it should not revert",
        );

        assert_eq!(
            translate(&file_contents)?,
            root(vec![target(
                "FooTest",
                vec![
                    function(
                        "when_stuff_called".to_owned(),
                        hir::FunctionTy::Modifier,
                        Span::new(Position::new(9, 3, 1), Position::new(234, 9, 32)),
                        None,
                        None
                    ),
                    function(
                        "test_when_stuff_called".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(9, 3, 1), Position::new(234, 9, 32)),
                        Some(vec!["when_stuff_called".to_owned()]),
                        Some(vec![
                            comment("It should do stuff.".to_owned()),
                            comment("It should do more.".to_owned()),
                        ])
                    ),
                    function(
                        "test_panic_when_a_called".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(75, 5, 5), Position::new(134, 6, 28)),
                        Some(vec!["when_stuff_called".to_owned()]),
                        Some(vec![comment("it should revert".to_owned()),])
                    ),
                    function(
                        "test_when_b_called".to_owned(),
                        hir::FunctionTy::Test,
                        Span::new(Position::new(173, 8, 5), Position::new(234, 9, 32)),
                        Some(vec!["when_stuff_called".to_owned()]),
                        Some(vec![comment("it should not revert".to_owned()),])
                    ),
                ]
            )],)
        );

        Ok(())
    }
}
