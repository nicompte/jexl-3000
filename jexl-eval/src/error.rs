/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use jexl_parser::{ast::OpCode, ParseError, Token};

use serde_json::Value;

pub type Result<'a, T, E = EvaluationError<'a>> = std::result::Result<T, E>;
#[derive(Debug, thiserror::Error)]
pub enum EvaluationError<'a> {
    #[error("Parsing error: {0}")]
    ParseError(Box<ParseError<usize, Token<'a>, &'a str>>),
    #[error("Invalid binary operation, left: {left}, right: {right}, operation: {operation}")]
    InvalidBinaryOp {
        left: Value,
        right: Value,
        operation: OpCode,
    },
    #[error("Unknown transform: {0}")]
    UnknownTransform(String),
    #[error("Unknown filter: {0}")]
    UnknownFilter(String),
    #[error("Expected a boolean response for filter: {0}")]
    ExpectedBoolForFilter(String),
    #[error("Expected an array for {0}, got {1}")]
    ExpectedArray(String, String),
    #[error("Expected a string for {0}, got {1}")]
    ExpectedString(String, String),
    #[error("Expected an array or a string for {0}, got {1}")]
    ExpectedArrayOrString(String, String),
    #[error("Expected a number for {0}, got {1}")]
    ExpectedNumber(String, String),
    #[error("Duplicate object key: {0}")]
    DuplicateObjectKey(String),
    #[error("Identifier '{0}' is undefined")]
    UndefinedIdentifier(String),
    #[error("Invalid context provided")]
    InvalidContext,
    #[error("Invalid index type")]
    InvalidIndexType,
    #[error("Index out of range {0}")]
    IndexOutOfRange(usize),
    #[error("Invalid json: {0}")]
    JSONError(#[from] serde_json::Error),
    #[error("Custom error: {0}")]
    CustomError(#[from] anyhow::Error),
    #[error("Invalid filter")]
    InvalidFilter,
    #[error("Invalid range")]
    InvalidRange,
    #[error("Filter should return a bool")]
    FilterShouldReturnBool,
    #[error("Invalid duration: {0}")]
    InvalidDuration(String),
    #[error("Failed transform: {0}")]
    FailedTransform(String),
    #[error("Failed to evaluate expression: {0}")]
    FailedEvaluation(String),

    #[error("UnavailableExpression")]
    UnavailableExpression,
}

impl<'a> From<ParseError<usize, Token<'a>, &'a str>> for EvaluationError<'a> {
    fn from(cause: ParseError<usize, Token<'a>, &'a str>) -> Self {
        EvaluationError::ParseError(Box::new(cause))
    }
}
