use bulloak_syntax::utils::lower_first_letter;
use itertools::Itertools;

/// Converts a sentence to snake case.
///
/// The conversion is done by lowering the first letter of each word
/// in the title and replacing the spaces for underscores. For example, the sentence
/// `when only owner` is converted to the `when_only_owner` string.
pub(crate) fn to_snake_case(sentence: &str) -> String {
    sentence
        .split_whitespace()
        .map(|s| lower_first_letter(s))
        .join("_")
}
