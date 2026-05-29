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
    Regex(String, String),
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
        is_filter: bool,
    },
    FilterItemProperty(String),
    Conditional {
        left: Box<Expression>,
        truthy: Box<Expression>,
        falsy: Box<Expression>,
    },
    MapTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    SortByTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    AnyTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    AllTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    FindTransform {
        subject: Box<Expression>,
        name: String,
        args: Option<Vec<Box<Expression>>>,
    },
    FindIndexTransform {
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
    Null,
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

/// Returns true if `expr` contains at least one `FilterItemProperty` node anywhere
/// in its subtree. Used at parse time to annotate `IndexOperation` nodes with
/// `is_filter`, so the evaluator can branch without a runtime tree walk.
pub fn contains_filter_property(expr: &Expression) -> bool {
    match &expr.expression {
        Expr::FilterItemProperty(_) => true,
        Expr::BinaryOperation { left, right, .. } => {
            contains_filter_property(left) || contains_filter_property(right)
        }
        Expr::UnaryOperation { right, .. } => contains_filter_property(right),
        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => {
            contains_filter_property(left)
                || contains_filter_property(truthy)
                || contains_filter_property(falsy)
        }
        // `is_filter` already encodes whether the index sub-tree contains any
        // FilterItemProperty, so we can short-circuit instead of re-walking it.
        // Known limitation: `arr[arr2[.x == 1]]` marks the *outer* IndexOperation
        // as is_filter=true because the inner node's is_filter leaks up through this
        // check.  In practice the evaluator then tries to filter `arr` using the
        // array result of `arr2[.x == 1]` as a predicate, which silently returns no
        // results.  This was the same behaviour before this refactor; a proper fix
        // would require distinguishing "filter property in the immediate index" from
        // "filter property nested inside a sub-index".
        Expr::IndexOperation {
            subject, is_filter, ..
        } => *is_filter || contains_filter_property(subject),
        Expr::DotOperation { subject, .. } => contains_filter_property(subject),
        Expr::Array(items) => items.iter().any(|item| contains_filter_property(item)),
        Expr::Object(items) => items.iter().any(|(_, e)| contains_filter_property(e)),
        Expr::Transform { subject, args, .. } => {
            contains_filter_property(subject)
                || args
                    .as_ref()
                    .map(|a| a.iter().any(|e| contains_filter_property(e)))
                    .unwrap_or(false)
        }
        Expr::MapTransform { subject, args, .. }
        | Expr::FilterTransform { subject, args, .. }
        | Expr::SortByTransform { subject, args, .. }
        | Expr::AnyTransform { subject, args, .. }
        | Expr::AllTransform { subject, args, .. }
        | Expr::FindTransform { subject, args, .. }
        | Expr::FindIndexTransform { subject, args, .. } => {
            contains_filter_property(subject)
                || args
                    .as_ref()
                    .map(|a| a.iter().any(|e| contains_filter_property(e)))
                    .unwrap_or(false)
        }
        Expr::ExpressionTransform {
            subject,
            expression,
            ..
        } => contains_filter_property(subject) || contains_filter_property(expression),
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            contains_filter_property(subject)
                || contains_filter_property(init)
                || contains_filter_property(expression)
        }
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Boolean(_)
        | Expr::Identifier(_)
        | Expr::Regex(_, _)
        | Expr::Now
        | Expr::NowUtc
        | Expr::Null => false,
    }
}
