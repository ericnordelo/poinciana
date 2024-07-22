//! Defines a Cairo code emitter from a HIR.

use std::result;

use crate::{
    config::Config,
    hir::{self, visitor::Visitor, Hir},
};

/// Solidity code emitter.
///
/// This struct holds the state of the emitter. It is not
/// tied to a specific HIR.
pub struct Emitter {
    /// The indentation level of the emitted code.
    indent: usize,
}

impl Emitter {
    /// Create a new emitter with the given configuration.
    #[must_use]
    pub fn new(cfg: &Config) -> Self {
        let cfg = cfg.scaffold();
        Self { indent: cfg.indent }
    }

    /// Emit Solidity code from the given HIR.
    #[must_use]
    pub fn emit(self, hir: &hir::Hir) -> String {
        EmitterI::new(self).emit(hir)
    }

    /// Return the indentation string. i.e. the string that is used
    /// to indent the emitted code.
    fn indent(&self) -> String {
        " ".repeat(self.indent)
    }
}

/// The internal implementation of the Solidity code emitter.
///
/// This emitter generates skeleton contracts and tests functions
/// inside that contract described in the input .tree file.
struct EmitterI {
    /// The emitter state.
    emitter: Emitter,
}

impl EmitterI {
    /// Create a new emitter with the given emitter state and modifier map.
    fn new(emitter: Emitter) -> Self {
        Self { emitter }
    }

    /// Emit Cairo code from the given HIR.
    ///
    /// This function is the entry point of the emitter. It is fine to unwrap
    /// here since emitting can't fail, otherwise a previous phase of the
    /// compiler is buggy.
    fn emit(&mut self, hir: &hir::Hir) -> String {
        match hir {
            Hir::Root(ref inner) => self.visit_root(inner).unwrap(),
            Hir::Target(ref inner) => self.visit_target(inner).unwrap(),
            Hir::FunctionDefinition(ref inner) => self.visit_function(inner).unwrap(),
            Hir::Comment(ref inner) => self.visit_comment(inner).unwrap(),
        }
    }

    /// Emit a modifier.
    ///
    /// A modifier is a helper function not anottated as test.
    fn emit_modifier(&self, modifier: &str) -> String {
        let mut emitted = String::new();
        let indentation = self.emitter.indent();
        emitted.push_str(&format!("{indentation}fn {modifier}() {{\n"));
        emitted.push_str(&format!("{}// code\n", indentation.repeat(2)));
        emitted.push_str(&format!("{indentation}}}\n"));
        emitted.push('\n');

        emitted
    }

    /// Emit a test function's definition.
    fn emit_fn_definition(&self, function: &hir::FunctionDefinition) -> String {
        let mut emitted = String::new();

        let fn_indentation = self.emitter.indent();

        emitted.push_str("#[test]");
        emitted.push_str(format!("{}fn {}() {{\n", fn_indentation, function.identifier).as_str());

        emitted
    }

    /// Emit a test function's modifiers.
    fn emit_fn_modifiers(&self, function: &hir::FunctionDefinition) -> String {
        let mut emitted = String::new();

        let fn_indentation = self.emitter.indent();
        let fn_body_indentation = fn_indentation.repeat(2);

        // Emit the modifiers that should be applied to this function.
        if let Some(ref modifiers) = function.modifiers {
            for modifier in modifiers {
                emitted.push_str(format!("{fn_body_indentation}{modifier}();\n").as_str());
            }
            emitted.push('\n');
        }

        emitted
    }
}

/// The visitor implementation for the emitter.
///
/// Note that the visitor is infallible because previous
/// passes ensure that the HIR is valid. In case an error
/// is found, it should be added to a previous pass.
impl Visitor for EmitterI {
    type CommentOutput = String;
    type Error = ();
    type FunctionDefinitionOutput = String;
    type RootOutput = String;
    type TargetOutput = String;

    fn visit_root(&mut self, root: &hir::Root) -> result::Result<Self::RootOutput, Self::Error> {
        let mut emitted = String::new();
        emitted.push_str("/// Generated by poinciana using BTT\n\n");

        for hir in &root.children {
            let result = match hir {
                Hir::Target(target) => self.visit_target(target)?,
                _ => unreachable!(),
            };

            emitted.push_str(&result);
        }

        Ok(emitted)
    }

    fn visit_target(
        &mut self,
        target: &hir::Target,
    ) -> result::Result<Self::TargetOutput, Self::Error> {
        let mut emitted = String::new();

        for hir in &target.children {
            if let Hir::FunctionDefinition(function) = hir {
                emitted.push_str(&self.visit_function(function)?);
            }
        }

        // Remove the last char, which is the extra '\n' from
        // emitting functions.
        emitted.pop();

        Ok(emitted)
    }

    fn visit_function(
        &mut self,
        function: &hir::FunctionDefinition,
    ) -> result::Result<Self::FunctionDefinitionOutput, Self::Error> {
        let mut emitted = String::new();

        if matches!(function.ty, hir::FunctionTy::Modifier) {
            emitted.push_str(&self.emit_modifier(&function.identifier));
        } else {
            let fn_definition = self.emit_fn_definition(function);
            let fn_modifiers = self.emit_fn_modifiers(function);
            emitted.push_str(&fn_definition);
            emitted.push_str(&fn_modifiers);

            if let Some(ref children) = function.children {
                for child in children {
                    if let Hir::Comment(comment) = child {
                        emitted.push_str(&self.visit_comment(comment)?);
                    }
                }
            }

            emitted.push_str("\npanic!(\"NOT IMPLEMENTED\");\n");

            let indentation = self.emitter.indent();
            emitted.push_str(format!("{indentation}}}\n\n").as_str());
        }

        Ok(emitted)
    }

    fn visit_comment(
        &mut self,
        comment: &hir::Comment,
    ) -> result::Result<Self::CommentOutput, Self::Error> {
        let mut emitted = String::new();
        let indentation = self.emitter.indent().repeat(2);
        emitted.push_str(format!("{indentation}// {}\n", comment.lexeme).as_str());

        Ok(emitted)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use pretty_assertions::assert_eq;

    use crate::{config::Config, hir::translate, scaffold::emitter};

    fn scaffold(text: &str) -> Result<String> {
        let cfg = Config::default();
        let hir = translate(text, &cfg)?;
        Ok(emitter::Emitter::new(&cfg).emit(&hir))
    }

    #[test]
    fn one_child() -> Result<()> {
        let file_contents =
            String::from("FileTest\n└── when something bad happens\n   └── it should not revert");

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FileTest {
  function test_WhenSomethingBadHappens() external {
    // it should not revert
  }
}"
        );

        // Test that "it should revert" actions change the test name.
        let file_contents =
            String::from("FileTest\n└── when something bad happens\n   └── it should revert");

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FileTest {
  function test_RevertWhen_SomethingBadHappens() external {
    // it should revert
  }
}"
        );

        Ok(())
    }

    #[test]
    fn actions_without_conditions() -> Result<()> {
        let file_contents = String::from("FileTest\n├── it should do st-ff\n└── It never reverts.");

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FileTest {
  function test_ShouldDoSt_ff() external {
    // it should do st-ff
  }

  function test_NeverReverts() external {
    // It never reverts.
  }
}"
        );

        let file_contents = String::from(
            "FileTest
├── it should do stuff
└── when something happens
    └── it should revert",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FileTest {
  function test_ShouldDoStuff() external {
    // it should do stuff
  }

  function test_RevertWhen_SomethingHappens() external {
    // it should revert
  }
}"
        );

        let file_contents = String::from(
            "FileTest
├── it should do stuff
├── when something happens
│   └── it should revert
└── it does everything",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FileTest {
  function test_ShouldDoStuff() external {
    // it should do stuff
  }

  function test_RevertWhen_SomethingHappens() external {
    // it should revert
  }

  function test_DoesEverything() external {
    // it does everything
  }
}"
        );

        Ok(())
    }

    #[test]
    fn unsanitized_input() -> Result<()> {
        let file_contents =
            String::from("Fi-eTest\n└── when something bad happens\n   └── it should not revert");

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract Fi_eTest {
  function test_WhenSomethingBadHappens() external {
    // it should not revert
  }
}"
        );

        Ok(())
    }

    #[test]
    fn indentation() -> Result<()> {
        let file_contents =
            String::from("FileTest\n└── when something bad happens\n   └── it should not revert");

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FileTest {
  function test_WhenSomethingBadHappens() external {
    // it should not revert
  }
}"
        );

        Ok(())
    }

    #[test]
    fn two_children() -> Result<()> {
        let file_contents = String::from(
            r"TwoChildren_Test
├── when stuff called
│  └── it should revert
└── when not stuff called
   └── it should revert",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract TwoChildren_Test {
  function test_RevertWhen_StuffCalled() external {
    // it should revert
  }

  function test_RevertWhen_NotStuffCalled() external {
    // it should revert
  }
}"
        );

        Ok(())
    }

    #[test]
    fn action_with_sibling_condition() -> Result<()> {
        let file_contents = String::from(
            r"
Foo_Test
└── when stuff called
    ├── It should do stuff.
    ├── when a called
    │   └── it should revert
    ├── It should do more.
    └── when b called
        └── it should not revert",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract Foo_Test {
  modifier whenStuffCalled() {
    _;
  }

  function test_WhenStuffCalled()
    external
    whenStuffCalled
  {
    // It should do stuff.
    // It should do more.
  }

  function test_RevertWhen_ACalled()
    external
    whenStuffCalled
  {
    // it should revert
  }

  function test_WhenBCalled()
    external
    whenStuffCalled
  {
    // it should not revert
  }
}"
        );

        Ok(())
    }

    #[test]
    fn action_recollection() -> Result<()> {
        let file_contents = String::from(
            r"ActionsTest
└── when stuff called
   ├── it should do stuff
   ├── it should be cool
   └── it might break
",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract ActionsTest {
  function test_WhenStuffCalled() external {
    // it should do stuff
    // it should be cool
    // it might break
  }
}"
        );

        Ok(())
    }

    #[test]
    fn first_action_revert_emits_revert_when() -> Result<()> {
        let file_contents = String::from(
            r"ActionsTest
└── when stuff called
   ├── it should revert
   ├── it should be cool
   └── it might break
",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract ActionsTest {
  function test_RevertWhen_StuffCalled() external {
    // it should revert
    // it should be cool
    // it might break
  }
}"
        );

        Ok(())
    }

    #[test]
    fn weird_it_should_revert_emits_revert_when() -> Result<()> {
        let file_contents = String::from(
            r"ActionsTest
└── when stuff called
   └── IT sHould RevERT. 
",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract ActionsTest {
  function test_RevertWhen_StuffCalled() external {
    // IT sHould RevERT.
  }
}"
        );

        Ok(())
    }

    #[test]
    fn non_firt_child_it_should_revert_doesnt_emit_revert_when() -> Result<()> {
        let file_contents = String::from(
            r"ActionsTest
└── when stuff called
   ├── it should be cool
   ├── it should revert
   └── it might break
",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract ActionsTest {
  function test_WhenStuffCalled() external {
    // it should be cool
    // it should revert
    // it might break
  }
}"
        );

        Ok(())
    }

    #[test]
    fn action_descriptions() -> Result<()> {
        let file_contents = String::from(
            r"DescriptionsTest
└── when something bad happens
   └── it should try to revert
      ├── some stuff happened
      │  └── and that stuff
      └── was very _bad_",
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract DescriptionsTest {
  function test_WhenSomethingBadHappens() external {
    // it should try to revert
    //    some stuff happened
    //       and that stuff
    //    was very _bad_
  }
}"
        );

        Ok(())
    }

    #[test]
    fn deep_tree() -> Result<()> {
        let file_contents = String::from(
            r#"DeepTest
├── when stuff called
│  └── it should revert
└── when not stuff called
   ├── when the deposit amount is zero
   │  └── it should revert
   └── when the deposit amount is not zero
      ├── when the number count is zero
      │  └── it should revert
      ├── when the asset is not a contract
      │  └── it should revert
      └── given the asset is a contract
          ├── when the asset misses the ERC-20 return value
          │  ├── it should create the child
          │  ├── it should perform the ERC-20 transfers
          │  └── it should emit a {MultipleChildren} event
          └── when the asset does not miss the ERC-20 return value
              ├── it should create the child
              └── it should emit a {MultipleChildren} event
                 ├── - Because the deposit should not be 0.
                 ├── - The number count is > 0.
                 └── - Events should be emitted."#,
        );

        assert_eq!(
            &scaffold(&file_contents)?,
            r"// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract DeepTest {
  function test_RevertWhen_StuffCalled() external {
    // it should revert
  }

  modifier whenNotStuffCalled() {
    _;
  }

  function test_RevertWhen_TheDepositAmountIsZero()
    external
    whenNotStuffCalled
  {
    // it should revert
  }

  modifier whenTheDepositAmountIsNotZero() {
    _;
  }

  function test_RevertWhen_TheNumberCountIsZero()
    external
    whenNotStuffCalled
    whenTheDepositAmountIsNotZero
  {
    // it should revert
  }

  function test_RevertWhen_TheAssetIsNotAContract()
    external
    whenNotStuffCalled
    whenTheDepositAmountIsNotZero
  {
    // it should revert
  }

  modifier givenTheAssetIsAContract() {
    _;
  }

  function test_WhenTheAssetMissesTheERC_20ReturnValue()
    external
    whenNotStuffCalled
    whenTheDepositAmountIsNotZero
    givenTheAssetIsAContract
  {
    // it should create the child
    // it should perform the ERC-20 transfers
    // it should emit a {MultipleChildren} event
  }

  function test_WhenTheAssetDoesNotMissTheERC_20ReturnValue()
    external
    whenNotStuffCalled
    whenTheDepositAmountIsNotZero
    givenTheAssetIsAContract
  {
    // it should create the child
    // it should emit a {MultipleChildren} event
    //    - Because the deposit should not be 0.
    //    - The number count is > 0.
    //    - Events should be emitted.
  }
}"
        );

        Ok(())
    }
}
