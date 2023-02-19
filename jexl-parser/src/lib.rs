/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

pub mod ast;
#[rustfmt::skip]
#[allow(clippy::all)]
mod parser;

pub use lalrpop_util::lexer::Token;
pub use lalrpop_util::ParseError;

pub struct Parser {}

impl Parser {
    pub fn parse(input: &str) -> Result<ast::Expression, ParseError<usize, Token, &str>> {
        Ok(*parser::ExpressionParser::new().parse(input)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Expression, ExpressionTransform, OpCode};

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
                expression: Expr::Regex("\\d".to_owned()),
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
                    })
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
                    })
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
    fn test_map_transform_operation() {
        let exp = r#"[1, 2] | mapTransform(operation)"#;
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
    fn test_filter_transform_operation() {
        let exp = r#"[1, 2] | filterTransform(operation)"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::FilterTransform {
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
                location: (0, 35)
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
    fn test_date() {
        let exp = r#"date("1988-03-29", "YYYY-MM-DD")"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Date {
                    date: Box::new(Expression {
                        expression: Expr::String("1988-03-29".to_string()),
                        location: (5, 17)
                    }),
                    format: Box::new(Expression {
                        expression: Expr::String("YYYY-MM-DD".to_string()),
                        location: (19, 31)
                    })
                },
                location: (0, 32)
            }
        );
        let exp = r#"date(test, "YYYY-MM-DD")"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Date {
                    date: Box::new(Expression {
                        expression: Expr::Identifier("test".to_string()),
                        location: (5, 9)
                    }),
                    format: Box::new(Expression {
                        expression: Expr::String("YYYY-MM-DD".to_string()),
                        location: (11, 23)
                    })
                },
                location: (0, 24)
            }
        );
        let exp = r#"date((test | toDate("YYYY-MM-DD")), "YYYY-MM-DD")"#;
        let parsed = Parser::parse(exp).unwrap();
        assert_eq!(
            parsed,
            Expression {
                expression: Expr::Date {
                    date: Box::new(Expression {
                        expression: Expr::Transform {
                            name: "toDate".to_string(),
                            subject: Box::new(Expression {
                                expression: Expr::Identifier("test".to_string()),
                                location: (6, 10)
                            }),
                            args: Some(vec![Box::new(Expression {
                                expression: Expr::String("YYYY-MM-DD".to_string()),
                                location: (20, 32)
                            })])
                        },
                        location: (6, 33)
                    }),
                    format: Box::new(Expression {
                        expression: Expr::String("YYYY-MM-DD".to_string()),
                        location: (36, 48)
                    })
                },
                location: (0, 49)
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
                        expression: Expr::Regex("\\w+".to_string()),
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
                        expression: Expr::Regex("\\w+".to_string()),
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
                                expression: Expr::Regex("\\w+".to_string()),
                                location: (12, 17)
                            })
                        },
                        location: (2, 17)
                    }),
                    index: Box::new(Expression {
                        expression: Expr::Number(0.0),
                        location: (19, 20)
                    })
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
                        expression: Expr::Regex("\\w+".to_string()),
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
}
