use std::fs;
use mux_compiler::source::Source;

#[test]
fn test_file_output_matches_cat() {
    let file_path = "../test_scripts/full_test.mux";
    let expected = fs::read_to_string(file_path)
        .expect("Failed to read test file");
    let mut src = Source::new(file_path)
        .expect("Failed to open file in Source");

    let mut actual = String::new();
    while let Some(ch) = src.next_char() {
        actual.push(ch);
    }
    assert_eq!(expected, actual);
}
