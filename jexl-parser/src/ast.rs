/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

pub type Location = (usize, usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub expression: Expr,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Box<Expression>>),
    Object(Vec<(String, Box<Expression>)>),
    Identifier(String),
    Regex(String),
    UnaryOperation {
        operation: UnCode,
        right: Box<Expression>,
    },
    BinaryOperation {
        operation: OpCode,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Transform {
        name: String,
        subject: Box<Expression>,
        args: Option<Vec<Box<Expression>>>,
    },
    DotOperation {
        subject: Box<Expression>,
        ident: String,
    },
    IndexOperation {
        subject: Box<Expression>,
        index: Box<Expression>,
    },
    Conditional {
        left: Box<Expression>,
        truthy: Box<Expression>,
        falsy: Box<Expression>,
    },
    Filter {
        ident: String,
        op: OpCode,
        right: Box<Expression>,
    },
    MapTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    ExpressionTransform {
        name: ExpressionTransform,
        subject: Box<Expression>,
        expression: Box<Expression>,
        args: Option<f64>,
    },
    FilterTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    ReduceExpression {
        subject: Box<Expression>,
        init: Box<Expression>,
        expression: Box<Expression>,
    },
    Now,
    NowUtc,
    Date {
        date: Box<Expression>,
        format: Box<Expression>,
    },
    DateTime {
        datetime: Box<Expression>,
        format: Box<Expression>,
    },
    Duration {
        duration: Box<Expression>,
        duration_type: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OpCode {
    Add,
    Subtract,
    Multiply,
    Divide,
    FloorDivide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Modulus,
    Exponent,
    In,
    Matches,
    Capture,
    CaptureMultiple,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ExpressionTransform {
    Map,
    Filter,
    SortBy,
    Any,
    All,
    Find,
    FindIndex,
    Apply,
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OpCode::Add => "Add",
                OpCode::Subtract => "Subtract",
                OpCode::Multiply => "Multiply",
                OpCode::Divide => "Divide",
                OpCode::FloorDivide => "Floor division",
                OpCode::Less => "Less than",
                OpCode::LessEqual => "Less than or equal to",
                OpCode::Greater => "Greater than",
                OpCode::GreaterEqual => "Greater than or equal to",
                OpCode::Equal => "Equal",
                OpCode::NotEqual => "Not equal",
                OpCode::And => "Bitwise And",
                OpCode::Or => "Bitwise Or",
                OpCode::Modulus => "Modulus",
                OpCode::Exponent => "Exponent",
                OpCode::In => "In",
                OpCode::Matches => "Matches",
                OpCode::Capture => "Capture",
                OpCode::CaptureMultiple => "CaptureMultiple",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnCode {
    Not,
    Plus,
    Minus,
}

impl std::fmt::Display for UnCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnCode::Not => "Not",
                UnCode::Plus => "Plus",
                UnCode::Minus => "Minus",
            }
        )
    }
}
