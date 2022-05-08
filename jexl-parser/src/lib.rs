/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

pub mod ast;
#[rustfmt::skip]
#[allow(clippy::all)]
mod parser;

pub use lalrpop_util::lexer::Token;
pub use lalrpop_util::ParseError;

// pub use crate::parser::Token;

pub struct Parser {}

impl Parser {
    pub fn parse(input: &str) -> Result<ast::Expression, ParseError<usize, Token, &str>> {
        Ok(*parser::ExpressionParser::new().parse(input)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, ExpressionTransform, OpCode};

    #[test]
    fn literal() {
        assert_eq!(Parser::parse("1"), Ok(Expression::Number(1.0)));
        assert_eq!(
            Parser::parse(r#""1""#),
            Ok(Expression::String("1".to_owned()))
        );
        assert_eq!(
            Parser::parse(r#"/\d/"#),
            Ok(Expression::Regex(r#"\d"#.to_owned()))
        );
    }

    #[test]
    fn binary_expression() {
        assert_eq!(
            Parser::parse("1+2"),
            Ok(Expression::BinaryOperation {
                operation: OpCode::Add,
                left: Box::new(Expression::Number(1.0)),
                right: Box::new(Expression::Number(2.0)),
            }),
        );
    }

    #[test]
    fn binary_expression_whitespace() {
        assert_eq!(Parser::parse("1  +     2 "), Parser::parse("1+2"),);
    }

    #[test]
    fn transform_simple_no_args() {
        let exp = "'T_T'|lower";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::Transform {
                name: "lower".to_string(),
                subject: Box::new(Expression::String("T_T".to_string())),
                args: None
            }
        );
    }

    #[test]
    fn transform_multiple_args() {
        let exp = "'John Doe'|split(' ')";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::Transform {
                name: "split".to_string(),
                subject: Box::new(Expression::String("John Doe".to_string())),
                args: Some(vec![Box::new(Expression::String(" ".to_string()))])
            }
        );
    }

    #[test]
    fn trasform_way_too_many_args() {
        let exp = "123456|math(12, 35, 100, 31, 90)";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::Transform {
                name: "math".to_string(),
                subject: Box::new(Expression::Number(123_456f64)),
                args: Some(vec![
                    Box::new(Expression::Number(12f64)),
                    Box::new(Expression::Number(35f64)),
                    Box::new(Expression::Number(100f64)),
                    Box::new(Expression::Number(31f64)),
                    Box::new(Expression::Number(90f64)),
                ])
            }
        );
    }

    #[test]
    fn test_index_op_ident() {
        let exp = "foo[0]";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::IndexOperation {
                subject: Box::new(Expression::Identifier("foo".to_string())),
                index: Box::new(Expression::Number(0f64))
            }
        );
    }

    #[test]
    fn test_index_op_array_literal() {
        let exp = "[1, 2, 3][0]";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::IndexOperation {
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64)),
                    Box::new(Expression::Number(3f64)),
                ])),
                index: Box::new(Expression::Number(0f64))
            }
        );
    }

    #[test]
    fn test_dot_op_ident() {
        let exp = "foo.bar";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::DotOperation {
                subject: Box::new(Expression::Identifier("foo".to_string())),
                ident: "bar".to_string()
            }
        );
    }

    #[test]
    fn test_dot_op_object_literal() {
        let exp = "{'foo': 1}.foo";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::DotOperation {
                subject: Box::new(Expression::Object(vec![(
                    "foo".to_string(),
                    Box::new(Expression::Number(1f64))
                )])),
                ident: "foo".to_string()
            }
        );
    }

    #[test]
    fn test_map_operation() {
        let exp = r#"[1, 2] | map(this | foo)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::ExpressionTransform {
                name: ExpressionTransform::Map,
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64))
                ])),
                expression: Box::new(Expression::Transform {
                    name: "foo".to_string(),
                    args: None,
                    subject: Box::new(Expression::Identifier("this".to_string()))
                }),
                args: None
            }
        );
    }

    #[test]
    fn test_map_transform_operation() {
        let exp = r#"[1, 2] | mapTransform(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::MapTransform {
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64))
                ])),
                name: "operation".to_string(),
                args: None,
            }
        );
    }

    #[test]
    fn test_filter_operation() {
        let exp = r#"[1, 2] | filter(this | foo)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::ExpressionTransform {
                name: ExpressionTransform::Filter,
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64))
                ])),
                expression: Box::new(Expression::Transform {
                    name: "foo".to_string(),
                    args: None,
                    subject: Box::new(Expression::Identifier("this".to_string()))
                }),
                args: None
            }
        );
        let exp = r#"[1, 2] | filter(this | foo(3))"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::ExpressionTransform {
                name: ast::ExpressionTransform::Filter,
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64))
                ])),
                expression: Box::new(Expression::Transform {
                    name: "foo".to_string(),
                    args: Some(vec![Box::new(Expression::Number(3f64))]),
                    subject: Box::new(Expression::Identifier("this".to_string()))
                }),
                args: None,
            }
        );
    }

    #[test]
    fn test_filter_transform_operation() {
        let exp = r#"[1, 2] | filterTransform(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::FilterTransform {
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64))
                ])),
                name: "operation".to_string(),
                args: None,
            }
        );
    }

    #[test]
    fn test_reduce_operation() {
        let exp = r#"[1, 2] | reduce([], acc + 23)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::ReduceExpression {
                subject: Box::new(Expression::Array(vec![
                    Box::new(Expression::Number(1f64)),
                    Box::new(Expression::Number(2f64))
                ])),
                init: Box::new(Expression::Array(vec![])),
                expression: Box::new(Expression::BinaryOperation {
                    operation: OpCode::Add,
                    left: Box::new(Expression::Identifier("acc".to_string())),
                    right: Box::new(Expression::Number(23f64)),
                })
            }
        );
    }

    #[test]
    fn test_date() {
        let exp = r#"date("1988-03-29", "YYYY-MM-DD")"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::Date {
                date: Box::new(Expression::String("1988-03-29".to_string())),
                format: Box::new(Expression::String("YYYY-MM-DD".to_string())),
            }
        );
        let exp = r#"date(test, "YYYY-MM-DD")"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::Date {
                date: Box::new(Expression::Identifier("test".to_string())),
                format: Box::new(Expression::String("YYYY-MM-DD".to_string())),
            }
        );
        let exp = r#"date((test | toDate("YYYY-MM-DD")), "YYYY-MM-DD")"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::Date {
                date: Box::new(Expression::Transform {
                    name: "toDate".to_string(),
                    subject: Box::new(Expression::Identifier("test".to_string())),
                    args: Some(vec![Box::new(Expression::String("YYYY-MM-DD".to_string()))])
                }),
                format: Box::new(Expression::String("YYYY-MM-DD".to_string())),
            }
        );
    }

    #[test]
    fn test_regex_matches() {
        let exp = r#" "Hello" ~ /\w+/ "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::BinaryOperation {
                operation: OpCode::Matches,
                left: Box::new(Expression::String("Hello".to_string())),
                right: Box::new(Expression::Regex("\\w+".to_string())),
            }
        );
    }

    #[test]
    fn test_regex_captures() {
        let exp = r#" "Hello" @ /\w+/ "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::BinaryOperation {
                operation: OpCode::Capture,
                left: Box::new(Expression::String("Hello".to_string())),
                right: Box::new(Expression::Regex("\\w+".to_string())),
            }
        );
        let exp = r#" ("Hello" @ /\w+/)[0] "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::IndexOperation {
                subject: Box::new(Expression::BinaryOperation {
                    operation: OpCode::Capture,
                    left: Box::new(Expression::String("Hello".to_string())),
                    right: Box::new(Expression::Regex("\\w+".to_string())),
                }),
                index: Box::new(Expression::Number(0f64)),
            }
        );
    }

    #[test]
    fn test_regex_multiple_captures() {
        let exp = r#" "Hello" @+ /\w+/ "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::BinaryOperation {
                operation: OpCode::CaptureMultiple,
                left: Box::new(Expression::String("Hello".to_string())),
                right: Box::new(Expression::Regex("\\w+".to_string())),
            }
        );
    }

    #[test]
    fn test_comments() {
        let exp = r#"
        # test
        1 + 2 # aussi test
        "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression::BinaryOperation {
                operation: OpCode::Add,
                left: Box::new(Expression::Number(1f64)),
                right: Box::new(Expression::Number(2f64)),
            }
        );
    }
}
