/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

pub mod ast;
pub mod lexer;
#[rustfmt::skip]
#[allow(clippy::all)]
mod parser;

pub use crate::lexer::{LexError, Token};
pub use lalrpop_util::ParseError;

pub struct Parser {}

impl Parser {
    pub fn parse(input: &str) -> Result<ast::Expression, ParseError<usize, Token<'_>, LexError>> {
        let lexer = lexer::Lexer::new(input);
        Ok(*parser::ExpressionParser::new().parse(lexer)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Expression, ExpressionTransform, OpCode, UnCode};

    #[test]
    fn literal() {
        assert_eq!(
            Parser::parse("1"),
            Ok(Expression {
                expression: Expr::Number(1.0),
                location: (0, 1)
            })
        );
        assert_eq!(
            Parser::parse(r#""1""#),
            Ok(Expression {
                expression: Expr::String("1".to_owned()),
                location: (0, 3)
            })
        );
        assert_eq!(
            Parser::parse(r#"/\d/"#),
            Ok(Expression {
                expression: Expr::Regex("\\d".to_owned(), "".to_owned()),
                location: (0, 4)
            })
        );
        assert_eq!(
            Parser::parse("null"),
            Ok(Expression {
                expression: Expr::Null,
                location: (0, 4)
            })
        );
    }

    #[test]
    fn binary_expression() {
        assert_eq!(
            Parser::parse("1+2"),
            Ok(Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Add,
                    left: Box::new(Expression {
                        expression: Expr::Number(1.0),
                        location: (0, 1)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Number(2.0),
                        location: (2, 3)
                    })
                },
                location: (0, 3)
            })
        );
    }

    #[test]
    fn binary_expression_whitespace() {
        assert_eq!(
            Parser::parse("1  +     2 "),
            Ok(Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Add,
                    left: Box::new(Expression {
                        expression: Expr::Number(1.0),
                        location: (0, 1)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Number(2.0),
                        location: (9, 10)
                    })
                },
                location: (0, 10)
            })
        );
    }

    #[test]
    fn transform_simple_no_args() {
        let exp = "'T_T'|lower";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Transform {
                    name: "lower".to_string(),
                    subject: Box::new(Expression {
                        expression: Expr::String("T_T".to_string()),
                        location: (0, 5)
                    }),
                    args: None
                },
                location: (0, 11)
            }
        );
    }

    #[test]
    fn transform_multiple_args() {
        let exp = "'John Doe'|split(' ')";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Transform {
                    name: "split".to_string(),
                    subject: Box::new(Expression {
                        expression: Expr::String("John Doe".to_string()),
                        location: (0, 10)
                    }),
                    args: Some(vec![Box::new(Expression {
                        expression: Expr::String(" ".to_string()),
                        location: (17, 20)
                    })])
                },
                location: (0, 21)
            }
        );
    }

    #[test]
    fn transform_way_too_many_args() {
        let exp = "123456|math(12, 35, 100, 31, 90)";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Transform {
                    name: "math".to_string(),
                    subject: Box::new(Expression {
                        expression: Expr::Number(123456.0),
                        location: (0, 6)
                    }),
                    args: Some(vec![
                        Box::new(Expression {
                            expression: Expr::Number(12.0),
                            location: (12, 14)
                        }),
                        Box::new(Expression {
                            expression: Expr::Number(35.0),
                            location: (16, 18)
                        }),
                        Box::new(Expression {
                            expression: Expr::Number(100.0),
                            location: (20, 23)
                        }),
                        Box::new(Expression {
                            expression: Expr::Number(31.0),
                            location: (25, 27)
                        }),
                        Box::new(Expression {
                            expression: Expr::Number(90.0),
                            location: (29, 31)
                        })
                    ])
                },
                location: (0, 32)
            }
        );
    }

    #[test]
    fn test_index_op_ident() {
        let exp = "foo[0]";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::IndexOperation {
                    subject: Box::new(Expression {
                        expression: Expr::Identifier("foo".to_string()),
                        location: (0, 3)
                    }),
                    index: Box::new(Expression {
                        expression: Expr::Number(0.0),
                        location: (4, 5)
                    }),
                    is_filter: false
                },
                location: (0, 6)
            }
        );
    }

    #[test]
    fn test_index_op_array_literal() {
        let exp = "[1, 2, 3][0]";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::IndexOperation {
                    subject: Box::new(Expression {
                        expression: Expr::Array(vec![
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (1, 2)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(3.0),
                                location: (7, 8)
                            })
                        ]),
                        location: (0, 9)
                    }),
                    index: Box::new(Expression {
                        expression: Expr::Number(0.0),
                        location: (10, 11)
                    }),
                    is_filter: false
                },
                location: (0, 12)
            }
        );
    }

    #[test]
    fn test_dot_op_ident() {
        let exp = "foo.bar";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::DotOperation {
                    subject: Box::new(Expression {
                        expression: Expr::Identifier("foo".to_string()),
                        location: (0, 3)
                    }),
                    ident: "bar".to_string()
                },
                location: (0, 7)
            }
        );
    }

    #[test]
    fn test_dot_op_object_literal() {
        let exp = "{'foo': 1}.foo";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::DotOperation {
                    subject: Box::new(Expression {
                        expression: Expr::Object(vec![(
                            "foo".to_string(),
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (8, 9)
                            })
                        )]),
                        location: (0, 10)
                    }),
                    ident: "foo".to_string()
                },
                location: (0, 14)
            }
        );
    }

    #[test]
    fn test_map_operation() {
        let exp = r#"[1, 2] | map(this | foo)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::ExpressionTransform {
                    name: ExpressionTransform::Map,
                    subject: Box::new(Expression {
                        expression: Expr::Array(vec![
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (1, 2)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            })
                        ]),
                        location: (0, 6)
                    }),
                    expression: Box::new(Expression {
                        expression: Expr::Transform {
                            name: "foo".to_string(),
                            subject: Box::new(Expression {
                                expression: Expr::Identifier("this".to_string()),
                                location: (13, 17)
                            }),
                            args: None
                        },
                        location: (13, 23)
                    }),
                    args: None
                },
                location: (0, 24)
            }
        );
    }

    #[test]
    fn test_map_shorthand_parses_as_map_transform() {
        let exp = r#"[1, 2] | map(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        // Ensure the expression is parsed as a MapTransform with the expected name
        assert!(matches!(parsed.expression, Expr::MapTransform { .. }));
        if let Expr::MapTransform {
            name,
            subject,
            args,
        } = parsed.expression
        {
            assert_eq!(name, "operation".to_string());
            if let Expr::Array(items) = subject.expression {
                assert_eq!(items.len(), 2);
            } else {
                panic!("expected array subject");
            }
            assert!(args.is_none());
        } else {
            panic!("expected MapTransform expression");
        }
    }

    #[test]
    fn test_map_with_this_property_is_normal_map() {
        let exp = r#"[1, 2] | map(this.test)"#;
        let parsed = Parser::parse(exp).unwrap();
        // Should be an expression-based Map (not MapTransform shorthand)
        assert!(matches!(
            parsed.expression,
            Expr::ExpressionTransform {
                name: ExpressionTransform::Map,
                ..
            }
        ));
        if let Expr::ExpressionTransform { expression, .. } = parsed.expression {
            // inner expression should be a DotOperation on `this.test`
            let inner = *expression;
            if let Expr::DotOperation { subject, ident } = inner.expression {
                assert!(matches!(subject.expression, Expr::Identifier(ref s) if s == "this"));
                assert_eq!(ident, "test");
            } else {
                panic!("expected DotOperation as map expression");
            }
        } else {
            panic!("expected ExpressionTransform::Map");
        }
    }

    #[test]
    fn test_map_with_this_property_is_normal_map_2() {
        let exp = r#"[1, 2] | map(yolo + this.test)"#;
        let parsed = Parser::parse(exp).unwrap();
        // Should be an expression-based Map (not MapTransform shorthand)
        assert!(matches!(
            parsed.expression,
            Expr::ExpressionTransform {
                name: ExpressionTransform::Map,
                ..
            }
        ));
        if let Expr::ExpressionTransform { expression, .. } = parsed.expression {
            // inner expression should be a BinaryOperation on `yolo + this.test`
            let inner = *expression;
            if let Expr::BinaryOperation {
                left,
                operation,
                right,
            } = inner.expression
            {
                assert_eq!(operation, OpCode::Add);
                assert!(matches!(left.expression, Expr::Identifier(ref s) if s == "yolo"));
                if let Expr::DotOperation { subject, ident } = right.expression {
                    assert!(matches!(subject.expression, Expr::Identifier(ref s) if s == "this"));
                    assert_eq!(ident, "test");
                } else {
                    panic!("expected DotOperation as right side of map expression");
                }
            } else {
                panic!("expected BinaryOperation as map expression");
            }
        } else {
            panic!("expected ExpressionTransform::Map");
        }
    }

    #[test]
    fn test_filter_shorthand_parses_as_filter_transform() {
        let exp = r#"[1, 2] | filter(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        // Ensure the expression is parsed as a FilterTransform with the expected name
        assert!(matches!(parsed.expression, Expr::FilterTransform { .. }));
        if let Expr::FilterTransform {
            name,
            subject,
            args,
        } = parsed.expression
        {
            assert_eq!(name, "operation".to_string());
            if let Expr::Array(items) = subject.expression {
                assert_eq!(items.len(), 2);
            } else {
                panic!("expected array subject");
            }
            assert!(args.is_none());
        } else {
            panic!("expected FilterTransform expression");
        }
    }

    #[test]
    fn test_filter_with_this_property_is_normal_filter() {
        let exp = r#"[1, 2] | filter(this.test)"#;
        let parsed = Parser::parse(exp).unwrap();
        // Should be an expression-based Filter (not FilterTransform shorthand)
        assert!(matches!(
            parsed.expression,
            Expr::ExpressionTransform {
                name: ExpressionTransform::Filter,
                ..
            }
        ));
        if let Expr::ExpressionTransform { expression, .. } = parsed.expression {
            // inner expression should be a DotOperation on `this.test`
            let inner = *expression;
            if let Expr::DotOperation { subject, ident } = inner.expression {
                assert!(matches!(subject.expression, Expr::Identifier(ref s) if s == "this"));
                assert_eq!(ident, "test");
            } else {
                panic!("expected DotOperation as filter expression");
            }
        } else {
            panic!("expected ExpressionTransform::Filter");
        }
    }

    #[test]
    fn test_sortby_shorthand_parses_as_sortby_transform() {
        let exp = r#"[1, 2] | sortBy(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::SortByTransform { .. }));
        if let Expr::SortByTransform {
            name,
            subject,
            args,
        } = parsed.expression
        {
            assert_eq!(name, "operation".to_string());
            if let Expr::Array(items) = subject.expression {
                assert_eq!(items.len(), 2);
            } else {
                panic!("expected array subject");
            }
            assert!(args.is_none());
        } else {
            panic!("expected SortByTransform");
        }
    }

    #[test]
    fn test_sortby_with_this_property_is_normal_sortby() {
        let exp = r#"[1, 2] | sortBy(this.test)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(
            parsed.expression,
            Expr::ExpressionTransform {
                name: ExpressionTransform::SortBy,
                ..
            }
        ));
    }

    #[test]
    fn test_any_shorthand_parses_as_any_transform() {
        let exp = r#"[1, 2] | any(isOdd)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::AnyTransform { .. }));
    }

    #[test]
    fn test_any_with_this_property_is_normal_any() {
        let exp = r#"[1, 2] | any(this.test)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(
            parsed.expression,
            Expr::ExpressionTransform {
                name: ExpressionTransform::Any,
                ..
            }
        ));
    }

    #[test]
    fn test_all_shorthand_parses_as_all_transform() {
        let exp = r#"[1, 2] | all(isPos)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::AllTransform { .. }));
    }

    #[test]
    fn test_find_shorthand_parses_as_find_transform() {
        let exp = r#"[1, 2] | find(predicate)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::FindTransform { .. }));
    }

    #[test]
    fn test_findindex_shorthand_parses_as_findindex_transform() {
        let exp = r#"[1, 2] | findIndex(predicate)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::FindIndexTransform { .. }));
    }

    // #[test]
    fn test_map_transform_operation_2() {
        let exp = r#"[1, 2] | map(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::MapTransform {
                    name: "operation".to_string(),
                    subject: Box::new(Expression {
                        expression: Expr::Array(vec![
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (1, 2)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            })
                        ]),
                        location: (0, 6)
                    }),
                    args: None
                },
                location: (0, 32)
            }
        );
    }

    #[test]
    fn test_filter_operation() {
        let exp = r#"[1, 2] | filter(this | foo)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::ExpressionTransform {
                    name: ExpressionTransform::Filter,
                    subject: Box::new(Expression {
                        expression: Expr::Array(vec![
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (1, 2)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            })
                        ]),
                        location: (0, 6)
                    }),
                    expression: Box::new(Expression {
                        expression: Expr::Transform {
                            name: "foo".to_string(),
                            subject: Box::new(Expression {
                                expression: Expr::Identifier("this".to_string()),
                                location: (16, 20)
                            }),
                            args: None
                        },
                        location: (16, 26)
                    }),
                    args: None
                },
                location: (0, 27)
            }
        );
        let exp = r#"[1, 2] | filter(this | foo(3))"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::ExpressionTransform {
                    name: ExpressionTransform::Filter,
                    subject: Box::new(Expression {
                        expression: Expr::Array(vec![
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (1, 2)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            })
                        ]),
                        location: (0, 6)
                    }),
                    expression: Box::new(Expression {
                        expression: Expr::Transform {
                            name: "foo".to_string(),
                            subject: Box::new(Expression {
                                expression: Expr::Identifier("this".to_string()),
                                location: (16, 20)
                            }),
                            args: Some(vec![Box::new(Expression {
                                expression: Expr::Number(3.0),
                                location: (27, 28)
                            })])
                        },
                        location: (16, 29)
                    }),
                    args: None
                },
                location: (0, 30)
            }
        );
    }

    #[test]
    fn test_reduce_operation() {
        let exp = r#"[1, 2] | reduce([], acc + 23)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::ReduceExpression {
                    subject: Box::new(Expression {
                        expression: Expr::Array(vec![
                            Box::new(Expression {
                                expression: Expr::Number(1.0),
                                location: (1, 2)
                            }),
                            Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            })
                        ]),
                        location: (0, 6)
                    }),
                    expression: Box::new(Expression {
                        expression: Expr::BinaryOperation {
                            left: Box::new(Expression {
                                expression: Expr::Identifier("acc".to_string()),
                                location: (20, 23)
                            }),
                            operation: OpCode::Add,
                            right: Box::new(Expression {
                                expression: Expr::Number(23.0),
                                location: (26, 28)
                            })
                        },
                        location: (20, 28)
                    }),
                    init: Box::new(Expression {
                        expression: Expr::Array(vec![]),
                        location: (16, 18)
                    }),
                },
                location: (0, 29)
            }
        );
    }

    #[test]
    fn test_regex_matches() {
        let exp = r#" "Hello" ~ /\w+/ "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Matches,
                    left: Box::new(Expression {
                        expression: Expr::String("Hello".to_string()),
                        location: (1, 8)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Regex("\\w+".to_string(), "".to_string()),
                        location: (11, 16)
                    })
                },
                location: (1, 16)
            }
        );
    }

    #[test]
    fn test_regex_captures() {
        let exp = r#" "Hello" @ /\w+/ "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Capture,
                    left: Box::new(Expression {
                        expression: Expr::String("Hello".to_string()),
                        location: (1, 8)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Regex("\\w+".to_string(), "".to_string()),
                        location: (11, 16)
                    })
                },
                location: (1, 16)
            }
        );
        let exp = r#" ("Hello" @ /\w+/)[0] "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::IndexOperation {
                    subject: Box::new(Expression {
                        expression: Expr::BinaryOperation {
                            operation: OpCode::Capture,
                            left: Box::new(Expression {
                                expression: Expr::String("Hello".to_string()),
                                location: (2, 9)
                            }),
                            right: Box::new(Expression {
                                expression: Expr::Regex("\\w+".to_string(), "".to_string()),
                                location: (12, 17)
                            })
                        },
                        location: (2, 17)
                    }),
                    index: Box::new(Expression {
                        expression: Expr::Number(0.0),
                        location: (19, 20)
                    }),
                    is_filter: false
                },
                location: (1, 21)
            }
        );
    }

    #[test]
    fn test_regex_multiple_captures() {
        let exp = r#" "Hello" @+ /\w+/ "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::CaptureMultiple,
                    left: Box::new(Expression {
                        expression: Expr::String("Hello".to_string()),
                        location: (1, 8)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Regex("\\w+".to_string(), "".to_string()),
                        location: (12, 17)
                    })
                },
                location: (1, 17)
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
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Add,
                    left: Box::new(Expression {
                        expression: Expr::Number(1.0),
                        location: (24, 25)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Number(2.0),
                        location: (28, 29)
                    })
                },
                location: (24, 29)
            }
        );
    }

    #[test]
    fn test_filter_item_property() {
        // Test simple filter item property
        let exp = r#"arr[.age >= 30]"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::IndexOperation { .. }));

        if let Expr::IndexOperation { index, .. } = parsed.expression {
            // The index should be a binary operation containing FilterItemProperty
            if let Expr::BinaryOperation { left, .. } = index.expression {
                assert!(matches!(left.expression, Expr::FilterItemProperty(_)));
                if let Expr::FilterItemProperty(prop) = left.expression {
                    assert_eq!(prop, "age");
                }
            }
        }
    }

    #[test]
    fn test_filter_complex_expression() {
        // Test complex filter with AND
        let exp = r#"arr[.age >= 30 && .age < 40]"#;
        let parsed = Parser::parse(exp).unwrap();
        // Verify full AST structure:
        match parsed.expression {
            Expr::IndexOperation {
                subject,
                index,
                is_filter,
            } => {
                assert!(is_filter, "index should be marked as a filter");

                // Subject should be identifier `arr`
                match subject.expression {
                    Expr::Identifier(ref id) => assert_eq!(id, "arr"),
                    _ => panic!("expected subject to be identifier 'arr'"),
                }

                // Index should be a binary AND expression: (.age >= 30) && (.age < 40)
                match index.expression {
                    Expr::BinaryOperation {
                        operation,
                        left,
                        right,
                    } => {
                        assert_eq!(operation, OpCode::And);

                        // Left side: .age >= 30
                        match left.expression {
                            Expr::BinaryOperation {
                                operation: op_l,
                                left: lleft,
                                right: lright,
                            } => {
                                assert_eq!(op_l, OpCode::GreaterEqual);
                                assert!(
                                    matches!(lleft.expression, Expr::FilterItemProperty(ref p) if p == "age")
                                );
                                assert!(matches!(lright.expression, Expr::Number(n) if n == 30.0));
                            }
                            _ => panic!(
                                "expected left side of AND to be BinaryOperation (age >= 30)"
                            ),
                        }

                        // Right side: .age < 40
                        match right.expression {
                            Expr::BinaryOperation {
                                operation: op_r,
                                left: rleft,
                                right: rright,
                            } => {
                                assert_eq!(op_r, OpCode::Less);
                                assert!(
                                    matches!(rleft.expression, Expr::FilterItemProperty(ref p) if p == "age")
                                );
                                assert!(matches!(rright.expression, Expr::Number(n) if n == 40.0));
                            }
                            _ => panic!(
                                "expected right side of AND to be BinaryOperation (age < 40)"
                            ),
                        }
                    }
                    _ => panic!("expected index to be BinaryOperation (AND)"),
                }
            }
            _ => panic!("expected top-level IndexOperation"),
        }
    }

    #[test]
    fn test_filter_with_context_var() {
        // Test filter referencing context variable
        let exp = r#"arr[.age >= retireAge]"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::IndexOperation { .. }));

        if let Expr::IndexOperation { index, .. } = parsed.expression
            && let Expr::BinaryOperation { right, .. } = index.expression
        {
            // Right side should be an identifier (context variable)
            assert!(matches!(right.expression, Expr::Identifier(_)));
        }
    }

    #[test]
    fn test_filter_nested_property() {
        // Test filter with nested property access
        let exp = r#"arr[.address.city == 'NYC']"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::IndexOperation { .. }));

        if let Expr::IndexOperation { index, .. } = parsed.expression
            && let Expr::BinaryOperation { left, .. } = index.expression
        {
            // Left side should be DotOperation on FilterItemProperty
            assert!(matches!(left.expression, Expr::DotOperation { .. }));
            if let Expr::DotOperation { subject, ident } = &left.expression {
                assert!(matches!(subject.expression, Expr::FilterItemProperty(_)));
                assert_eq!(ident, "city");
            }
        }
    }

    #[test]
    fn test_filter_chained() {
        // Test chained filters
        let exp = r#"arr[.age >= 30][.age < 40]"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::IndexOperation { .. }));

        // Outer IndexOperation
        if let Expr::IndexOperation { subject, .. } = parsed.expression {
            // Inner should also be IndexOperation
            assert!(matches!(subject.expression, Expr::IndexOperation { .. }));
        }
    }

    #[test]
    fn test_filter_with_expression() {
        // Test filter with expression in condition
        let exp = r#"arr[.name == 'Jo' + 'hn']"#;
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(
            parsed.expression,
            Expr::IndexOperation {
                is_filter: true,
                ..
            }
        ));
    }

    #[test]
    fn test_regular_index_is_not_filter() {
        // Plain numeric / string indexing must NOT be marked as a filter.
        let exp = "arr[0]";
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(
            parsed.expression,
            Expr::IndexOperation {
                is_filter: false,
                ..
            }
        ));
    }

    #[test]
    fn test_nested_filter_index_marks_outer_as_filter() {
        // Known limitation: `arr[arr2[.x == 1]]` — the outer IndexOperation is
        // marked is_filter=true because contains_filter_property propagates the
        // inner node's is_filter upward.  The evaluator therefore treats the outer
        // `[...]` as a filter predicate, which in practice returns an empty array
        // since an array result is never truthy as a predicate element.
        // This matches the pre-refactor behaviour and is documented here so the
        // semantics are explicit.
        let exp = "arr[arr2[.x == 1]]";
        let parsed = Parser::parse(exp).unwrap();
        // Outer is incorrectly marked as filter — see above note.
        assert!(matches!(
            parsed.expression,
            Expr::IndexOperation {
                is_filter: true,
                ..
            }
        ));
        // Inner is correctly marked as filter.
        if let Expr::IndexOperation { index, .. } = parsed.expression {
            assert!(matches!(
                index.expression,
                Expr::IndexOperation {
                    is_filter: true,
                    ..
                }
            ));
        }
    }

    #[test]
    fn test_leading_dot_creates_filter_property() {
        // Test that leading dot creates FilterItemProperty
        let exp = ".age";
        let parsed = Parser::parse(exp).unwrap();
        assert!(matches!(parsed.expression, Expr::FilterItemProperty(_)));

        if let Expr::FilterItemProperty(prop) = parsed.expression {
            assert_eq!(prop, "age");
        }
    }

    #[test]
    fn test_precedence() {
        let exp = r#"customer.name && customer.age > 25 ? "old" : "young""#;
        println!("Testing: {}", exp);
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Conditional {
                    left: Box::new(Expression {
                        expression: Expr::BinaryOperation {
                            operation: OpCode::And,
                            left: Box::new(Expression {
                                expression: Expr::DotOperation {
                                    subject: Box::new(Expression {
                                        expression: Expr::Identifier("customer".to_string()),
                                        location: (0, 8)
                                    }),
                                    ident: "name".to_string()
                                },
                                location: (0, 13)
                            }),
                            right: Box::new(Expression {
                                expression: Expr::BinaryOperation {
                                    operation: OpCode::Greater,
                                    left: Box::new(Expression {
                                        expression: Expr::DotOperation {
                                            subject: Box::new(Expression {
                                                expression: Expr::Identifier(
                                                    "customer".to_string()
                                                ),
                                                location: (17, 25)
                                            }),
                                            ident: "age".to_string()
                                        },
                                        location: (17, 29)
                                    }),
                                    right: Box::new(Expression {
                                        expression: Expr::Number(25.0),
                                        location: (32, 34)
                                    })
                                },
                                location: (17, 34)
                            })
                        },
                        location: (0, 34)
                    }),
                    truthy: Box::new(Expression {
                        expression: Expr::String("old".to_string()),
                        location: (37, 42)
                    }),
                    falsy: Box::new(Expression {
                        expression: Expr::String("young".to_string()),
                        location: (45, 52)
                    })
                },
                location: (0, 52)
            }
        )
    }

    #[test]
    fn test_and_or_precedence() {
        let exp = r#"a || b && c"#;
        let parsed = Parser::parse(exp).unwrap();
        // Expected: a || (b && c)
        assert_eq!(
            parsed.expression,
            Expr::BinaryOperation {
                operation: OpCode::Or,
                left: Box::new(Expression {
                    expression: Expr::Identifier("a".into()),
                    location: (0, 1)
                }),
                right: Box::new(Expression {
                    expression: Expr::BinaryOperation {
                        operation: OpCode::And,
                        left: Box::new(Expression {
                            expression: Expr::Identifier("b".into()),
                            location: (5, 6)
                        }),
                        right: Box::new(Expression {
                            expression: Expr::Identifier("c".into()),
                            location: (10, 11)
                        }),
                    },
                    location: (5, 11)
                }),
            }
        );
    }

    #[test]
    fn test_ternary_right_assoc() {
        let exp = r#"a ? b : c ? d : e"#;
        let parsed = Parser::parse(exp).unwrap();
        // Expected: a ? b : (c ? d : e)
        if let Expr::Conditional {
            left: _,
            truthy,
            falsy,
        } = &parsed.expression
        {
            assert_eq!(truthy.expression, Expr::Identifier("b".into()));
            if let Expr::Conditional {
                left: _,
                truthy: t2,
                falsy: f2,
            } = &falsy.expression
            {
                assert_eq!(t2.expression, Expr::Identifier("d".into()));
                assert_eq!(f2.expression, Expr::Identifier("e".into()));
            } else {
                panic!("Expected nested conditional");
            }
        } else {
            panic!("Expected conditional at top level");
        }
    }

    // #[test]
    // fn test_arithmetic_precedence() {
    //     let exp = r#"1 + 2 * 3 - 4 / 2"#;
    //     let parsed = Parser::parse(exp).unwrap();
    //     // Expected: 1 + (2*3) - (4/2)
    //     if let Expr::BinaryOperation {
    //         left,
    //         right,
    //         operation,
    //     } = &parsed.expression
    //     {
    //         assert_eq!(*operation, OpCode::Subtract);
    //         if let Expr::BinaryOperation {
    //             left: l1,
    //             right: r1,
    //             operation: op1,
    //         } = &**left
    //         {
    //             assert_eq!(*op1, OpCode::Add);
    //             // 1 + (2*3)
    //         }
    //         if let Expr::BinaryOperation {
    //             left: l2,
    //             right: r2,
    //             operation: op2,
    //         } = &**right
    //         {
    //             assert_eq!(*op2, OpCode::Divide);
    //             // 4 / 2
    //         }
    //     }
    // }

    #[test]
    fn test_arithmetic_precedence() {
        let exp = r#"1 + 2 * 3 - 4 / 2"#;
        let parsed = Parser::parse(exp).unwrap();

        // Top-level: (1 + 2*3) - (4/2)
        if let Expr::BinaryOperation {
            left: left1,
            right: right1,
            operation: op1,
        } = &parsed.expression
        {
            assert_eq!(*op1, OpCode::Subtract);

            // Left side of top-level: 1 + 2*3
            if let Expr::BinaryOperation {
                left: l2,
                right: r2,
                operation: op2,
            } = &left1.expression
            {
                assert_eq!(*op2, OpCode::Add);

                // l2 should be 1
                if let Expr::Number(n1) = l2.expression {
                    assert_eq!(n1, 1.0);
                } else {
                    panic!("Expected left number 1");
                }

                // r2 should be 2*3
                if let Expr::BinaryOperation {
                    left: l3,
                    right: r3,
                    operation: op3,
                } = &r2.expression
                {
                    assert_eq!(*op3, OpCode::Multiply);
                    if let Expr::Number(n2) = l3.expression {
                        assert_eq!(n2, 2.0);
                    } else {
                        panic!("Expected number 2");
                    }
                    if let Expr::Number(n3) = r3.expression {
                        assert_eq!(n3, 3.0);
                    } else {
                        panic!("Expected number 3");
                    }
                } else {
                    panic!("Expected multiplication on right side of addition");
                }
            } else {
                panic!("Expected addition on left side of subtraction");
            }

            // Right side of top-level: 4/2
            if let Expr::BinaryOperation {
                left: l4,
                right: r4,
                operation: op4,
            } = &right1.expression
            {
                assert_eq!(*op4, OpCode::Divide);
                if let Expr::Number(n4) = l4.expression {
                    assert_eq!(n4, 4.0);
                } else {
                    panic!("Expected number 4");
                }
                if let Expr::Number(n5) = r4.expression {
                    assert_eq!(n5, 2.0);
                } else {
                    panic!("Expected number 2");
                }
            } else {
                panic!("Expected division on right side of subtraction");
            }
        } else {
            panic!("Expected top-level subtraction");
        }
    }

    #[test]
    fn test_power() {
        let exp = "2^3";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Exponent,
                    left: Box::new(Expression {
                        expression: Expr::Number(2.0),
                        location: (0, 1)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Number(3.0),
                        location: (2, 3)
                    })
                },
                location: (0, 3)
            }
        );
    }

    #[test]
    fn test_modulus() {
        let exp = "2%3";
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Modulus,
                    left: Box::new(Expression {
                        expression: Expr::Number(2.0),
                        location: (0, 1)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Number(3.0),
                        location: (2, 3)
                    })
                },
                location: (0, 3)
            }
        );
    }

    // #[test]
    // TODO NBE
    fn test_unary_precedence() {
        let exp = r#"-2^2 "#;
        let parsed = Parser::parse(exp).unwrap();

        // expected: -(2^2)
        // result : -4
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::UnaryOperation {
                    operation: UnCode::Minus,
                    right: Box::new(Expression {
                        expression: Expr::BinaryOperation {
                            operation: OpCode::Exponent,
                            left: Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (1, 2)
                            }),
                            right: Box::new(Expression {
                                expression: Expr::Number(2.0),
                                location: (4, 5)
                            }),
                        },
                        location: (1, 5)
                    })
                },
                location: (0, 5)
            }
        );
    }

    // #[test]
    // TODO NBE
    fn test_unary_precedence_complex() {
        let exp = r#"-2^3 * 4"#;
        let parsed = Parser::parse(exp).unwrap();

        // expected: (-(2^3)) * 4

        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::Multiply,
                    left: Box::new(Expression {
                        expression: Expr::UnaryOperation {
                            operation: UnCode::Minus,
                            right: Box::new(Expression {
                                expression: Expr::BinaryOperation {
                                    operation: OpCode::Exponent,
                                    left: Box::new(Expression {
                                        expression: Expr::Number(2.0),
                                        location: (1, 2)
                                    }),
                                    right: Box::new(Expression {
                                        expression: Expr::Number(3.0),
                                        location: (3, 4)
                                    }),
                                },
                                location: (1, 4)
                            })
                        },
                        location: (0, 4)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Number(4.0),
                        location: (7, 8)
                    })
                },
                location: (0, 8)
            }
        );
    }

    #[test]
    fn test_parentheses() {
        let exp = r#"(1 + 2) * 3"#;
        let parsed = Parser::parse(exp).unwrap();
        // Expected: (1+2) then multiply by 3
        if let Expr::BinaryOperation {
            left,
            right: _,
            operation,
        } = &parsed.expression
        {
            assert_eq!(*operation, OpCode::Multiply);
            if let Expr::BinaryOperation { operation: op2, .. } = &left.expression {
                assert_eq!(*op2, OpCode::Add);
            } else {
                panic!("Expected addition inside parentheses");
            }
        }
    }

    #[test]
    // customer.email && (customer.email | lowercase)
    fn test_transform_operation() {
        let exp = r#" customer.email && customer.email | lowercase "#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::BinaryOperation {
                    operation: OpCode::And,
                    left: Box::new(Expression {
                        expression: Expr::DotOperation {
                            subject: Box::new(Expression {
                                expression: Expr::Identifier("customer".to_string()),
                                location: (1, 9)
                            }),
                            ident: "email".to_string()
                        },
                        location: (1, 15)
                    }),
                    right: Box::new(Expression {
                        expression: Expr::Transform {
                            name: "lowercase".to_string(),
                            subject: Box::new(Expression {
                                expression: Expr::DotOperation {
                                    subject: Box::new(Expression {
                                        expression: Expr::Identifier("customer".to_string()),
                                        location: (19, 27)
                                    }),
                                    ident: "email".to_string()
                                },
                                location: (19, 33)
                            }),
                            args: None
                        },
                        location: (19, 45)
                    })
                },
                location: (1, 45)
            }
        );
    }
}
