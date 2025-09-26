use tahuc_diagnostics::reporter::DiagnosticReporter;
use tahuc_lexer::Lexer;
use tahuc_span::FileId;

use crate::Parser;

fn create_parser(input: &'_ str) -> Parser<'_> {
    let reporter = Box::leak(Box::new(DiagnosticReporter::new()));
    let mut lexer = Lexer::new(input.to_string(), FileId(0), reporter);
    let lexer_result = lexer.tokenize();
    Parser::new(FileId(0), lexer_result, reporter)
}

#[cfg(test)]
mod function_test {
    use super::*;

    #[test]
    fn test_fn_missing_name() {
        let input = r#"fn () {}"#;

        let mut parser = create_parser(input);
        let result = parser.parse();
        println!("{:?}", result);

        assert!(result.has_errors);
    }

    #[test]
    fn test_fn_missing_left_paren() {
        let input = r#"fn main ) {}"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }

    #[test]
    fn test_fn_missing_right_paren() {
        let input = r#"fn main( {}"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }

    #[test]
    fn test_fn_missing_return_type() {
        let input = r#"fn main() {}"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(!result.has_errors);
    }

    #[test]
    fn test_fn_missing_left_brace() {
        let input = r#"fn main() int }"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }

    #[test]
    fn test_fn_missing_right_brace() {
        let input = r#"fn main() int {"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }

    #[test]
    fn test_fn_missing_block() {
        let input = r#"fn main() int"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }
}

#[cfg(test)]
mod variable_test {
    use super::*;

    #[test]
    fn test_var_missing_name() {
        let input = r#"var = 10;"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }

    #[test]
    fn test_var_missing_colon() {
        let input = r#"var a = 10;"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(!result.has_errors);
    }

    #[test]
    fn test_var_missing_assign() {
        let input = r#"var a: int;"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(!result.has_errors);
    }

    #[test]
    fn test_var_missing_semicolon() {
        let input = r#"var a: int = 10"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }

    #[test]
    fn test_var_missing_type() {
        let input = r#"var a: = 10;"#;

        let mut parser = create_parser(input);
        let result = parser.parse();

        assert!(result.has_errors);
    }
}
