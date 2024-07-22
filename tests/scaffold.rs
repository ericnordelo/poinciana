use std::{env, fs};

use common::{cmd, get_binary_path};
use owo_colors::OwoColorize;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn scaffolds_trees() {
    let cwd = env::current_dir().unwrap();
    let binary_path = get_binary_path();
    let tests_path = cwd.join("tests").join("scaffold");
    let trees = [
        "basic.tree",
        "complex.tree",
        "multiple_roots.tree",
        "removes_invalid_title_chars.tree",
        "hash_pair.tree",
        "revert_when.tree",
        "spurious_comments.tree",
    ];

    for tree_name in trees {
        let tree_path = tests_path.join(tree_name);
        let output = cmd(&binary_path, "scaffold", &tree_path, &[]);
        let actual = String::from_utf8(output.stdout).unwrap();

        let mut output_file = tree_path.clone();
        output_file.set_extension("t.cairo");
        let expected = fs::read_to_string(output_file).unwrap();

        // We trim here because we don't care about ending newlines.
        assert_eq!(expected.trim(), actual.trim());
    }
}

#[test]
fn errors_when_tree_is_empty() {
    let cwd = env::current_dir().unwrap();
    let binary_path = get_binary_path();
    let tests_path = cwd.join("tests").join("scaffold");
    let trees = ["empty.tree"];

    for tree_name in trees {
        let tree_path = tests_path.join(tree_name);
        let output = cmd(&binary_path, "scaffold", &tree_path, &[]);
        let actual = String::from_utf8(output.stderr).unwrap();

        assert!(actual.contains("found an empty tree"));
    }
}

#[test]
fn errors_when_condition_appears_multiple_times() {
    let cwd = env::current_dir().unwrap();
    let binary_path = get_binary_path();
    let tests_path = cwd.join("tests").join("scaffold");
    let trees = ["duplicated_condition.tree", "duplicated_top_action.tree"];

    for tree_name in trees {
        let tree_path = tests_path.join(tree_name);
        let output = cmd(&binary_path, "scaffold", &tree_path, &[]);
        let actual = String::from_utf8(output.stderr).unwrap();

        assert!(actual.contains("found an identifier more than once"));
    }
}

#[test]
fn errors_when_root_module_is_missing_multiple_roots() {
    let cwd = env::current_dir().unwrap();
    let binary_path = get_binary_path();
    let tests_path = cwd.join("tests").join("scaffold");
    let trees = ["module_name_missing_multiple_roots.tree"];

    for tree_name in trees {
        let tree_path = tests_path.join(tree_name);
        let output = cmd(&binary_path, "scaffold", &tree_path, &[]);
        let actual = String::from_utf8(output.stderr).unwrap();

        assert!(actual.contains("module name missing at tree root #1"));
    }
}
