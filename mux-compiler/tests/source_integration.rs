use insta::assert_snapshot;
use mux_compiler::source::Source;
use std::fs;

#[test]
fn test_file_output_matches_cat() {
    let test_dir = "../test_scripts";

    for entry in fs::read_dir(test_dir).expect("Failed to read test_scripts directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("mux") {
            let file_path = path.to_str().unwrap();

            let expected = fs::read_to_string(file_path)
                .unwrap_or_else(|_| panic!("Failed to read test file: {}", file_path));

            let mut src = Source::new(file_path)
                .unwrap_or_else(|_| panic!("Failed to open file in Source: {}", file_path));

            let mut actual = String::new();
            while let Some(ch) = src.next_char() {
                actual.push(ch);
            }

            assert_eq!(expected, actual, "File mismatch in {}", file_path);
            assert_snapshot!(format!("{:#?}", expected));
            // TODO: need to test the full tokens as well with the positions
        }
    }
}
