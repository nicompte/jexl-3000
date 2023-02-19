/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use jexl_parser::ast::{Location, OpCode};

use serde_json::Value;

pub type Result<T, E = EvaluationError> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum EvaluationError {
    //#[error("Parsing error: {0}")]
    //ParseError(Box<ParseError<usize, Token<'a>, &'a str>>),
    #[error("Invalid binary operation, left: {left}, right: {right}, operation: {operation} at {location:?}")]
    InvalidBinaryOp {
        location: Location,
        left: Value,
        right: Value,
        operation: OpCode,
    },
    #[error("Unknown transform: {1} at {0:?}")]
    UnknownTransform(Location, String, Vec<String>),
    #[error("Unknown filter: {1} at {0:?}")]
    UnknownFilter(Location, String, Vec<String>),
    #[error("Expected a boolean response for filter: {1} at {0:?}")]
    ExpectedBoolForFilter(Location, String),
    #[error("Invalid type, expected a {1:?}, got \"{2}\" at {0:?}")]
    InvalidType(Location, ExpectedType, String),
    #[error("Duplicate object key \"{1}\" at {0:?}")]
    DuplicateObjectKey(Location, String),
    #[error("Identifier \"{1}\" is undefined at {0:?}")]
    UndefinedIdentifier(Location, String),
    #[error("Invalid context provided")]
    InvalidContext,
    #[error("Invalid index type at {0:?}")]
    InvalidIndexType(Location),
    #[error("Index out of range \"{1}\" at {0:?}")]
    IndexOutOfRange(Location, usize),
    #[error("Invalid json: {0}")]
    JSONError(#[from] serde_json::Error),
    #[error("Invalid filter at {0:?}")]
    InvalidFilter(Location),
    #[error("Invalid range at {0:?}")]
    InvalidRange(Location),
    #[error("Filter should return a bool at {0:?}")]
    FilterShouldReturnBool(Location),
    #[error("Invalid duration \"{1}\" at {0:?}")]
    InvalidDuration(Location, String),
    #[error("Failed transform \"{1}\" at {0:?}")]
    FailedTransform(Location, String),
    #[error("Failed to evaluate expression \"{1}\" at {0:?}")]
    FailedEvaluation(Location, String),

    #[error("UnavailableExpression at {0:?}")]
    UnavailableExpression(Location),

    #[error("Invalid expression at {0:?}")]
    ExpectedValue(Location),
    /* FROM 3000 */
    #[error("Missing argument {1} at {0:?}")]
    MissingArgument(Location, usize),

    #[error("Invalid duration type at {0:?}")]
    InvalidDurationType(Location),

    #[error("Unsortable data type at {0:?}")]
    UnsortableType(Location),

    #[error("Failed to transform to integer at {0:?}")]
    FailedToInt(Location),

    #[error("Missing attribute {1} at {0:?}")]
    MissingAttribute(Location, String),

    #[error("Failed to parse date \"{1}\" with format \"{2}\" at {0:?}")]
    DateParseError(Location, String, String),

    #[error("Incorrect date format \"{1}\" at {0:?}")]
    DateFormatError(Location, String),

    #[error("Invalid regex \"{1}\" at {0:?}")]
    InvalidRegex(Location, String),

    #[error("Failed to capture \"{1}\" with regex \"{2}\" at {0:?}")]
    FailedCapture(Location, String, String),

    #[error("Failed reduce operation at {0:?}")]
    FailedReduce(Location),
}

/*impl<'a> From<ParseError<usize, Token<'a>, &'a str>> for EvaluationError<'a> {
    fn from(cause: ParseError<usize, Token<'a>, &'a str>) -> Self {
        EvaluationError::ParseError(Box::new(cause))
    }
}*/

#[derive(Debug)]
pub enum ExpectedType {
    String,
    Number,
    Boolean,
    Object,
    Array,
    ArrayOrString,
}
