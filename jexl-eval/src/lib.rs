/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! A JEXL evaluator written in Rust
//! This crate depends on a JEXL parser crate that handles all the parsing
//! and is a part of the same workspace.
//! JEXL is an expression language used by Mozilla, you can find more information here: https://github.com/mozilla/mozjexl
//!
//! # How to use
//! The access point for this crate is the `eval` functions of the Evaluator Struct
//! You can use the `eval` function directly to evaluate standalone statements
//!
//! For example:
//! ```rust
//! use jexl_eval::Evaluator;
//! use serde_json::json as value;
//! let evaluator = Evaluator::new();
//! assert_eq!(evaluator.eval("'Hello ' + 'World'").unwrap(), value!("Hello World"));
//! ```
//!
//! You can also run the statements against a context using the `eval_in_context` function
//! The context can be any type that implements the `serde::Serializable` trait
//! and the function will return errors if the statement doesn't match the context
//!
//! For example:
//! ```rust
//! use jexl_eval::Evaluator;
//! use serde_json::json as value;
//! let context = value!({"a": {"b": 2.0}});
//! let evaluator = Evaluator::new();
//! assert_eq!(evaluator.eval_in_context("a.b", &context).unwrap(), value!(2.0));
//! ```
//!
pub use jexl_parser::ast::Location;
use jexl_parser::{
    ast::{Expr, Expression, ExpressionTransform, OpCode, UnCode},
    Parser,
};
use regex::Regex;
use serde_json::{json as value, Value};
use time::{format_description, Date, Duration, OffsetDateTime, PrimitiveDateTime};

pub mod error;
use dashmap::DashMap;
use error::*;
use std::sync::Arc;

const EPSILON: f64 = 0.000001f64;

trait Truthy {
    fn is_truthy(&self) -> bool;

    fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }
}

impl Truthy for Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Number(f) => f.as_f64().unwrap() != 0.0,
            Value::String(s) => !s.is_empty(),
            // It would be better if these depended on the contents of the
            // object (empty array/object is falsey, non-empty is truthy, like
            // in Python) but this matches JS semantics. Is it worth changing?
            Value::Array(_) => true,
            Value::Object(_) => true,
        }
    }
}

impl Truthy for Result<Value> {
    fn is_truthy(&self) -> bool {
        match self {
            Ok(v) => v.is_truthy(),
            _ => false,
        }
    }
}

type Context = Value;

/// TransformFn represents an arbitrary transform function
/// Transform functions take an arbitrary number of `serde_json::Value`to represent their arguments
/// and return a `serde_json::Value`.
/// the transform function itself is responsible for checking if the format and number of
/// the arguments is correct
///
/// Returns a Result with an `anyhow::Error`. This allows consumers to return their own custom errors
/// in the closure, and use `.into` to convert it into an `anyhow::Error`. The error message will be perserved
pub type TransformFn<'a> =
    Arc<dyn Fn(Location, &[Value]) -> Result<Value, EvaluationError> + 'a + Send + Sync>;

#[derive(Default)]
pub struct Evaluator<'a> {
    transforms: DashMap<String, TransformFn<'a>, fxhash::FxBuildHasher>,
    parsed: DashMap<String, Expression, fxhash::FxBuildHasher>,
    regexes: DashMap<String, Regex, fxhash::FxBuildHasher>,
}

impl<'a> Evaluator<'a> {
    pub fn new() -> Self {
        Evaluator {
            transforms: DashMap::with_hasher(fxhash::FxBuildHasher::default()),
            parsed: DashMap::with_hasher(fxhash::FxBuildHasher::default()),
            regexes: DashMap::with_hasher(fxhash::FxBuildHasher::default()),
        }
    }

    /// Adds a custom transform function
    /// This is meant as a way to allow consumers to add their own custom functionality
    /// to the expression language.
    /// Note that the name added here has to match with
    /// the name that the transform will have when it's a part of the expression statement
    ///
    /// # Arguments:
    /// - `name`: The name of the transfrom
    /// - `transform`: The actual function. A closure the implements Fn(&[serde_json::Value]) -> Result<Value, anyhow::Error>
    ///
    /// # Example:
    ///
    /// ```rust
    /// use jexl_eval::Evaluator;
    /// use serde_json::{json as value, Value};
    ///
    /// let evaluator = Evaluator::new().with_transform("lower", |v: &[Value]| {
    ///    let s = v
    ///            .first()
    ///            .expect("Should have 1 argument!")
    ///            .as_str()
    ///            .expect("Should be a string!");
    ///       Ok(value!(s.to_lowercase()))
    ///  });
    ///
    /// assert_eq!(evaluator.eval("'JOHN DOe'|lower").unwrap(), value!("john doe"))
    /// ```
    pub fn with_transform<F>(self, name: &str, transform: F) -> Self
    where
        F: Fn(Location, &[Value]) -> Result<Value, EvaluationError> + 'a + Send + Sync,
    {
        self.transforms
            .insert(name.to_string(), Arc::new(transform));
        self
    }

    pub fn eval(&self, input: &str) -> Result<Value> {
        let context = value!({});
        self.eval_in_context(input, &context)
    }

    pub fn eval_in_context(&self, input: &str, context: &Value) -> Result<Value> {
        if !context.is_object() {
            return Err(EvaluationError::InvalidContext);
        }

        if let Some(tree) = self.parsed.get(input) {
            self.eval_ast(&tree, context)
        } else {
            // TODO: remove unwrap()
            let tree = Parser::parse(input).unwrap();
            self.parsed.insert(input.to_string(), tree.clone());
            self.eval_ast(&tree, context)
        }
    }

    fn eval_ast(&self, ast: &Expression, context: &Context) -> Result<Value> {
        let Expression {
            expression,
            location,
        } = &ast;
        let ast = expression;
        match ast {
            Expr::Number(n) => Ok(value!(n)),
            Expr::Boolean(b) => Ok(value!(b)),
            Expr::String(s) => Ok(value!(s)),
            Expr::Regex(s) => Ok(value!(s)),
            Expr::Array(xs) => xs.iter().map(|x| self.eval_ast(x, context)).collect(),

            Expr::Object(items) => {
                let mut map = serde_json::Map::with_capacity(items.len());
                for (key, expr) in items.iter() {
                    if map.contains_key(key) {
                        return Err(EvaluationError::DuplicateObjectKey(
                            location.clone(),
                            key.clone(),
                        ));
                    }
                    let value = self.eval_ast(expr, context)?;
                    map.insert(key.clone(), value);
                }
                Ok(Value::Object(map))
            }

            Expr::Identifier(inner) => match context.get(inner) {
                Some(v) => Ok(v.clone()),
                _ => Err(EvaluationError::UndefinedIdentifier(
                    location.clone(),
                    inner.clone(),
                )),
            },

            Expr::DotOperation { subject, ident } => {
                let subject = self.eval_ast(&subject, context)?;
                Ok(subject.get(ident).unwrap_or(&value!(null)).clone())
            }

            Expr::IndexOperation { subject, index } => {
                let subject = self.eval_ast(&subject, context)?;
                if let Expr::Filter { ident, op, right } = (*index.clone()).expression {
                    let subject_arr = subject
                        .as_array()
                        .ok_or(EvaluationError::InvalidFilter(location.clone()))?;
                    let right = self.eval_ast(&right, context)?;
                    let filtered = subject_arr
                        .iter()
                        .filter(|e| {
                            let left = e.get(&ident).unwrap_or(&value!(null));
                            // returns false if any members fail the op, could happen if array members are missing the identifier
                            self.apply_op(location.clone(), &op, left.clone(), right.clone())
                                .unwrap_or(value!(false))
                                .is_truthy()
                        })
                        .collect::<Vec<_>>();
                    return Ok(value!(filtered));
                }

                let index = self.eval_ast(&index, context)?;
                match index {
                    Value::String(inner) => {
                        Ok(subject.get(&inner).unwrap_or(&value!(null)).clone())
                    }
                    Value::Number(inner) => {
                        if let Some(array) = subject.as_array() {
                            Ok(array
                                .get(
                                    inner
                                        .as_f64()
                                        .ok_or_else(|| {
                                            EvaluationError::InvalidType(
                                                location.clone(),
                                                ExpectedType::String,
                                                inner.to_string(),
                                            )
                                        })?
                                        .floor() as usize,
                                )
                                .unwrap_or(&value!(null))
                                .clone())
                        } else if let Some(str) = subject.as_str() {
                            Ok(value!(str.chars().nth(inner.as_f64().ok_or_else(|| {
                                EvaluationError::InvalidType(
                                    location.clone(),
                                    ExpectedType::String,
                                    inner.to_string(),
                                )
                            })?
                                as usize)))
                        } else {
                            Ok(value!(null))
                        }
                    }
                    _ => Err(EvaluationError::InvalidIndexType(location.clone())),
                }
            }
            Expr::UnaryOperation { operation, right } => {
                let right = self.eval_ast(&right, context)?;
                match operation {
                    UnCode::Not => Ok(value!(!right.is_truthy())),
                    UnCode::Minus => Ok(value!(-right.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::String,
                            right.to_string(),
                        )
                    })?)),
                    UnCode::Plus => Ok(right),
                }
            }
            Expr::BinaryOperation {
                left,
                right,
                operation,
            } => self.eval_op(location.clone(), &operation, &left, &right, context),
            Expr::Transform {
                name,
                subject,
                args,
            } => {
                let subject = self.eval_ast(&subject, context)?;
                let mut args_arr = vec![subject];
                if let Some(args) = args {
                    for arg in args {
                        args_arr.push(self.eval_ast(&arg, context)?);
                    }
                }
                self.transforms.get(name).ok_or_else(|| {
                    EvaluationError::UnknownTransform(
                        location.clone(),
                        name.clone(),
                        self.transforms.iter().map(|el| el.key().clone()).collect(),
                    )
                })?(location.clone(), &args_arr)
                /* .context(EvaluationError::FailedTransform(
                    location.clone(),
                    name.to_string(),
                ))
                .map_err(|e| e.into())*/
            }

            Expr::Conditional {
                left,
                truthy,
                falsy,
            } => {
                if self.eval_ast(&left, context).is_truthy() {
                    self.eval_ast(&truthy, context)
                } else {
                    self.eval_ast(&falsy, context)
                }
            }

            Expr::Filter {
                ident: _,
                op: _,
                right: _,
            } => {
                // Filters shouldn't be evaluated individually
                // instead, they are evaluated as a part of an IndexOperation
                Err(EvaluationError::InvalidFilter(location.clone()))
            }
            Expr::MapTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(&subject, context)?;
                let mut args_arr = Vec::new();
                // args_arr.push(subject.clone());
                if let Some(args) = args {
                    for arg in args {
                        args_arr.push(self.eval_ast(&arg, context)?);
                    }
                }
                let array = subject.as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::Array,
                        subject.to_string(),
                    )
                })?;

                let f = {
                    // let transforms = self.transforms;
                    self.transforms.get(name).ok_or_else(|| {
                        EvaluationError::UnknownTransform(
                            location.clone(),
                            name.clone(),
                            self.transforms.iter().map(|el| el.key().clone()).collect(),
                        )
                    })?
                    // .clone()
                };
                let res = array.iter().map(|v| {
                    let mut data = args_arr.clone();
                    data.insert(0, v.clone());
                    f(location.clone(), &data)
                });
                let res = res.collect::<std::result::Result<Vec<Value>, _>>()?;

                Ok(serde_json::to_value(res)?)
            }
            Expr::ExpressionTransform {
                name,
                subject,
                expression,
                args,
            } => match name {
                ExpressionTransform::Map => {
                    let subject = self.eval_ast(&subject, context)?;
                    let array = subject.as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array
                        .iter()
                        .map(|v| self.eval_ast(&expression, &value!({ "this": v })));
                    let res =
                        serde_json::to_value(res.collect::<std::result::Result<Vec<Value>, _>>()?)?;
                    Ok(res)
                }
                ExpressionTransform::Apply => {
                    let subject = self.eval_ast(&subject, context)?;
                    let res = serde_json::to_value(
                        self.eval_ast(&expression, &value!({ "this": subject }))?,
                    )?;
                    Ok(res)
                }
                ExpressionTransform::SortBy => {
                    let subject = self.eval_ast(&subject, context)?;
                    let reverse = args == &Some(-1f64);

                    let array = subject.as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;

                    let mut array = array.clone();
                    array.sort_by(|a, b| {
                        if let (Ok(a_val), Ok(b_val)) = (
                            self.eval_ast(&expression, &value!({ "this": a })),
                            self.eval_ast(&expression, &value!({ "this": b })),
                        ) {
                            if let (Some(a), Some(b)) = (a_val.as_str(), b_val.as_str()) {
                                a.cmp(b)
                            } else if let (Some(a), Some(b)) = (a_val.as_i64(), b_val.as_i64()) {
                                a.cmp(&b)
                            } else if let (Some(a), Some(b)) = (a_val.as_f64(), b_val.as_f64()) {
                                a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal)
                            } else {
                                // can't sort
                                std::cmp::Ordering::Equal
                            }
                        } else {
                            // failed evaluation
                            std::cmp::Ordering::Equal
                        }
                    });
                    if reverse {
                        array.reverse();
                    }

                    Ok(value!(array))
                }
                ExpressionTransform::Filter => {
                    let subject = self.eval_ast(&subject, context)?;

                    let array = if let Some(array) = subject.as_array() {
                        array
                    } else {
                        let v: Vec<Value> = vec![];
                        return Ok(value!(v));
                    };

                    let res = array
                        .iter()
                        .try_fold::<_, _, Result<Vec<_>, EvaluationError>>(vec![], |mut acc, v| {
                            let value = self
                                .eval_ast(&expression, &value!({ "this": v }))
                                .map_err(|_| {
                                    EvaluationError::FailedEvaluation(
                                        location.clone(),
                                        "filter".to_string(),
                                    )
                                })?
                                .as_bool()
                                // TODO: remove unwrap, use ok_or
                                /* .ok_or(|| {
                                    EvaluationError::InvalidType(
                                        location.clone(),
                                        ExpectedType::Boolean,
                                        v.to_string(),
                                    )
                                })?;*/
                                .unwrap();
                            if value {
                                acc.push(v);
                            }
                            Ok(acc)
                        })
                        .map_err(|_| EvaluationError::FailedReduce(location.clone()))?;

                    Ok(serde_json::to_value(res)?)
                }
                ExpressionTransform::Any => {
                    let subject = self.eval_ast(&subject, context)?;
                    if subject == Value::Null {
                        return Ok(Value::Bool(false));
                    }
                    let array = subject.as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().any(|v| {
                        self.eval_ast(&expression, &value!({ "this": v }))
                            .unwrap_or(Value::Bool(false))
                            .is_truthy()
                    });
                    Ok(value!(res))
                }
                ExpressionTransform::All => {
                    let subject = self.eval_ast(&subject, context)?;
                    let array = subject.as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().all(|v| {
                        self.eval_ast(&expression, &value!({ "this": v }))
                            .unwrap_or(Value::Bool(false))
                            .is_truthy()
                    });
                    Ok(value!(res))
                }
                ExpressionTransform::Find => {
                    let subject = self.eval_ast(&subject, context)?;
                    let array = subject.as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().find(|v| {
                        self.eval_ast(&expression, &value!({ "this": v }))
                            .unwrap_or(Value::Bool(false))
                            .is_truthy()
                    });
                    Ok(value!(res.cloned().unwrap_or(Value::Null)))
                }
                ExpressionTransform::FindIndex => {
                    let subject = self.eval_ast(&subject, context)?;
                    let array = subject.as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location.clone(),
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().position(|v| {
                        self.eval_ast(&expression, &value!({ "this": v }))
                            .unwrap_or(Value::Bool(false))
                            .is_truthy()
                    });
                    Ok(value!(res.map(|v| v as i64).unwrap_or(-1)))
                }
            },
            Expr::ReduceExpression {
                subject,
                init,
                expression,
            } => {
                let init = self.eval_ast(&init, &value!({}))?;
                let subject = self.eval_ast(&subject, context)?;

                let array = subject.as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::Array,
                        subject.to_string(),
                    )
                })?;

                let res = array.iter().try_fold(init, |acc, v| {
                    self.eval_ast(&expression, &value!({ "this": v, "acc": acc }))
                })?;
                Ok(serde_json::to_value(res)?)
            }
            Expr::FilterTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(&subject, context)?;
                let mut args_arr = Vec::new();
                if let Some(args) = args {
                    for arg in args {
                        args_arr.push(self.eval_ast(&arg, context)?);
                    }
                }
                let array = if let Some(array) = subject.as_array() {
                    array
                } else {
                    let v: Vec<Value> = vec![];
                    return Ok(value!(v));
                };

                let f = {
                    self.transforms.get(name).ok_or_else(|| {
                        EvaluationError::UnknownTransform(
                            location.clone(),
                            name.clone(),
                            self.transforms.iter().map(|el| el.key().clone()).collect(),
                        )
                    })?
                };

                let res = array.iter().filter(|v| {
                    let mut data = args_arr.clone();
                    data.insert(0, <&serde_json::Value>::clone(v).clone());
                    f(location.clone(), &data)
                        .unwrap_or(value![false])
                        .as_bool()
                        .unwrap_or(false)
                });
                let res = res.collect::<Vec<_>>();

                Ok(serde_json::to_value(res)?)
            }
            Expr::Now => Ok(value!(OffsetDateTime::now_local()
                .unwrap_or_else(|_| OffsetDateTime::now_utc())
                .unix_timestamp())),
            Expr::NowUtc => Ok(value!(OffsetDateTime::now_utc().unix_timestamp())),
            Expr::Date { date, format } => {
                let date = self.eval_ast(&date, context)?;
                let date = date.as_str().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::String,
                        date.to_string(),
                    )
                })?;
                let format = self.eval_ast(&format, context)?;
                let format = format.as_str().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::String,
                        date.to_string(),
                    )
                })?;
                let format_description = format_description::parse(format).map_err(|_| {
                    EvaluationError::DateFormatError(location.clone(), date.to_string())
                })?;
                let date = Date::parse(date, &format_description).map_err(|_| {
                    EvaluationError::DateParseError(
                        location.clone(),
                        date.to_string(),
                        format.to_string(),
                    )
                })?;
                let date = date.midnight();
                Ok(value!(date.assume_utc().unix_timestamp()))
            }
            Expr::DateTime { datetime, format } => {
                let datetime = self.eval_ast(&datetime, context)?;
                let datetime = datetime.as_str().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::String,
                        datetime.to_string(),
                    )
                })?;
                let format = self.eval_ast(&format, context)?;
                let format = format.as_str().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::String,
                        format.to_string(),
                    )
                })?;
                let format_description = format_description::parse(format).map_err(|_| {
                    EvaluationError::DateFormatError(location.clone(), format.to_string())
                })?;
                let datetime =
                    PrimitiveDateTime::parse(datetime, &format_description).map_err(|_| {
                        EvaluationError::DateParseError(
                            location.clone(),
                            datetime.to_string(),
                            format.to_string(),
                        )
                    })?;

                Ok(value!(datetime.assume_utc().unix_timestamp()))
            }
            Expr::Duration {
                duration,
                duration_type,
            } => {
                let duration = self.eval_ast(&duration, context)?;
                let duration = duration.as_f64().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::Number,
                        duration.to_string(),
                    )
                })? as i64;
                let duration_type = self.eval_ast(&duration_type, context)?;
                let duration_type = duration_type.as_str().ok_or_else(|| {
                    EvaluationError::InvalidType(
                        location.clone(),
                        ExpectedType::String,
                        duration_type.to_string(),
                    )
                })?;

                let duration = match duration_type {
                    "years" | "year" => Duration::days(365 * duration),
                    "months" | "month" => Duration::weeks(4 * duration),
                    "weeks" | "week" => Duration::weeks(duration),
                    "days" | "day" => Duration::days(duration),
                    "hours" | "hour" => Duration::hours(duration),
                    "minutes" | "minute" => Duration::minutes(duration),
                    "seconds" | "second" => Duration::seconds(duration),
                    _ => {
                        return Err(EvaluationError::InvalidDuration(
                            location.clone(),
                            duration_type.to_string(),
                        ));
                    }
                };
                Ok(value!(duration.as_seconds_f64().round() as i64))
            }
        }
    }

    fn eval_op(
        &self,
        location: Location,
        operation: &OpCode,
        left: &Expression,
        right: &Expression,
        context: &Context,
    ) -> Result<Value> {
        let left = self.eval_ast(left, context);

        // We want to delay evaluating the right hand side in the cases of AND and OR.
        let eval_right = || self.eval_ast(right, context);
        Ok(match operation {
            OpCode::Or => {
                if left.is_truthy() {
                    left?
                } else {
                    eval_right()?
                }
            }
            OpCode::And => {
                if left.is_truthy() {
                    eval_right()?
                } else {
                    left?
                }
            }
            _ => self.apply_op(location, operation, left?, eval_right()?)?,
        })
    }

    fn apply_op(
        &self,
        location: Location,
        operation: &OpCode,
        left: Value,
        right: Value,
    ) -> Result<Value> {
        match (operation, left, right) {
            (OpCode::NotEqual, a, b) => {
                // Implement NotEquals as the inverse of Equals.
                let value = self.apply_op(location, &OpCode::Equal, a, b)?;
                let equality = value
                    .as_bool()
                    .unwrap_or_else(|| unreachable!("Equality always returns a bool"));
                Ok(value!(!equality))
            }
            (OpCode::And, a, b) => Ok(if a.is_truthy() { b } else { a }),
            (OpCode::Or, a, b) => Ok(if a.is_truthy() { a } else { b }),
            // TODO: boolean boolean
            (op, Value::Number(a), Value::Number(b)) => {
                let left = a.as_f64().unwrap();
                let right = b.as_f64().unwrap();
                Ok(match op {
                    OpCode::Add => value!(left + right),
                    OpCode::Subtract => value!(left - right),
                    OpCode::Multiply => value!(left * right),
                    OpCode::Divide => value!(left / right),
                    OpCode::FloorDivide => value!((left / right).floor()),
                    OpCode::Modulus => value!(left % right),
                    OpCode::Exponent => value!(left.powf(right)),
                    OpCode::Less => value!(left < right),
                    OpCode::Greater => value!(left > right),
                    OpCode::LessEqual => value!(left <= right),
                    OpCode::GreaterEqual => value!(left >= right),
                    OpCode::Equal => value!((left - right).abs() < EPSILON),
                    OpCode::NotEqual => value!((left - right).abs() >= EPSILON),
                    OpCode::In => value!(false),
                    OpCode::Matches => value!(false),
                    OpCode::Capture => value!(false),
                    OpCode::CaptureMultiple => value!(false),
                    OpCode::And | OpCode::Or => {
                        unreachable!("Covered by previous case in parent match")
                    }
                })
            }

            (op, Value::String(a), Value::String(b)) => match op {
                OpCode::Equal => Ok(value!(a == b)),

                OpCode::Add => Ok(value!(format!("{}{}", a, b))),
                OpCode::In => Ok(value!(b.contains(&a))),

                OpCode::Less => Ok(value!(a < b)),
                OpCode::Greater => Ok(value!(a > b)),
                OpCode::LessEqual => Ok(value!(a <= b)),
                OpCode::GreaterEqual => Ok(value!(a >= b)),
                OpCode::Matches => {
                    if let Some(regex) = self.regexes.get(&b) {
                        Ok(value!(regex.is_match(&a)))
                    } else {
                        let regex = Regex::new(&b)
                            .map_err(|_| EvaluationError::InvalidRegex(location, b.to_string()))?;
                        self.regexes.insert(b, regex.clone());
                        Ok(value!(regex.is_match(&a)))
                    }
                }
                OpCode::Capture => {
                    let regex = {
                        if let Some(regex) = self.regexes.get(&b) {
                            regex.clone()
                        } else {
                            let regex = Regex::new(&b)
                                .map_err(|_| EvaluationError::InvalidRegex(location, b.clone()))?;
                            self.regexes.insert(b.clone(), regex.clone());
                            regex
                        }
                    };
                    let captures = regex.captures(&a);
                    if captures.is_none() {
                        return Ok(value!([]));
                    }
                    let captures: Vec<_> = captures
                        // .ok_or(|| EvaluationError::FailedCapture(location, b, regex.to_string()))?
                        // DOTO: remove unwrap
                        .unwrap()
                        .iter()
                        .map(|e| {
                            e.ok_or(|| {
                                EvaluationError::FailedCapture(
                                    location,
                                    b.clone(),
                                    regex.to_string(),
                                )
                            })
                            .map(|res| res.as_str())
                        })
                        .skip(1)
                        .collect::<Result<_, _>>()
                        .map_err(|_| {
                            EvaluationError::FailedCapture(location, b.clone(), regex.to_string())
                        })?;
                    Ok(value!(captures))
                }
                OpCode::CaptureMultiple => {
                    let regex = {
                        if let Some(regex) = self.regexes.get(&b) {
                            regex.clone()
                        } else {
                            let regex = Regex::new(&b)
                                .map_err(|_| EvaluationError::InvalidRegex(location, b.clone()))?;
                            self.regexes.insert(b.clone(), regex.clone());
                            regex
                        }
                    };
                    let captures: Vec<_> = regex.captures_iter(&a).collect();

                    let captures: Vec<_> = captures
                        .iter()
                        .map(|e| {
                            e.iter()
                                .map(|res| {
                                    res.ok_or(|| {
                                        EvaluationError::FailedCapture(
                                            location,
                                            b.clone(),
                                            regex.to_string(),
                                        )
                                    })
                                    .map(|e| e.as_str())
                                })
                                .skip(1)
                                .collect::<Result<Vec<_>, _>>()
                        })
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|_| {
                            EvaluationError::FailedCapture(location, b.clone(), regex.to_string())
                        })?;
                    Ok(value!(captures))
                }
                _ => Err(EvaluationError::InvalidBinaryOp {
                    location,
                    operation: *operation,
                    left: value!(a),
                    right: value!(b),
                }),
            },
            (OpCode::In, left, Value::Array(v)) => Ok(value!(v.contains(&left))),
            (OpCode::Equal, a, b) => match (a, b) {
                // Number == Number is handled above
                // String == String is handled above
                (Value::Bool(a), Value::Bool(b)) => Ok(value!(a == b)),
                (Value::Null, Value::Null) => Ok(value!(true)),
                (Value::Array(a), Value::Array(b)) => Ok(value!(a == b)),
                (Value::Object(a), Value::Object(b)) => Ok(value!(a == b)),
                // If the types don't match, it's always false
                _ => Ok(value!(false)),
            },
            (operation, left, right) => Err(EvaluationError::InvalidBinaryOp {
                location,
                operation: *operation,
                left,
                right,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use serde_json::json as value;

    #[test]
    fn test_literal() {
        assert_eq!(Evaluator::new().eval("1").unwrap(), value!(1.0));
    }

    #[test]
    fn test_binary_expression_addition() {
        assert_eq!(Evaluator::new().eval("1 + 2").unwrap(), value!(3.0));
    }

    #[test]
    fn test_binary_expression_multiplication() {
        assert_eq!(Evaluator::new().eval("2 * 3").unwrap(), value!(6.0));
    }

    #[test]
    fn test_precedence() {
        assert_eq!(Evaluator::new().eval("2 + 3 * 4").unwrap(), value!(14.0));
    }

    #[test]
    fn test_parenthesis() {
        assert_eq!(Evaluator::new().eval("(2 + 3) * 4").unwrap(), value!(20.0));
    }

    #[test]
    fn test_string_concat() {
        assert_eq!(
            Evaluator::new().eval("'Hello ' + 'World'").unwrap(),
            value!("Hello World")
        );
    }

    #[test]
    fn test_true_comparison() {
        assert_eq!(Evaluator::new().eval("2 > 1").unwrap(), value!(true));
    }

    #[test]
    fn test_false_comparison() {
        assert_eq!(Evaluator::new().eval("2 <= 1").unwrap(), value!(false));
    }

    #[test]
    fn test_boolean_logic() {
        assert_eq!(
            Evaluator::new()
                .eval("'foo' && 6 >= 6 && 0 + 1 && true")
                .unwrap(),
            value!(true)
        );
    }

    #[test]
    fn test_identifier() {
        let context = value!({"a": 1.0});
        assert_eq!(
            Evaluator::new().eval_in_context("a", &context).unwrap(),
            value!(1.0)
        );
    }

    #[test]
    fn test_identifier_chain() {
        let context = value!({"a": {"b": 2.0}});
        assert_eq!(
            Evaluator::new().eval_in_context("a.b", &context).unwrap(),
            value!(2.0)
        );
    }

    #[test]
    fn test_context_filter_arrays() {
        let context = value!({
            "foo": {
                "bar": [
                    {"tek": "hello"},
                    {"tek": "baz"},
                    {"tok": "baz"},
                ]
            }
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("foo.bar[.tek == 'baz']", &context)
                .unwrap(),
            value!([{"tek": "baz"}])
        );
    }

    #[test]
    fn test_context_array_index() {
        let context = value!({
            "foo": {
                "bar": [
                    {"tek": "hello"},
                    {"tek": "baz"},
                    {"tok": "baz"},
                ]
            }
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("foo.bar[1].tek", &context)
                .unwrap(),
            value!("baz")
        );
    }

    #[test]
    fn test_object_expression_properties() {
        let context = value!({"foo": {"baz": {"bar": "tek"}}});
        assert_eq!(
            Evaluator::new()
                .eval_in_context("foo['ba' + 'z'].bar", &context)
                .unwrap(),
            value!("tek")
        );
    }

    #[test]
    fn test_divfloor() {
        assert_eq!(Evaluator::new().eval("7 // 2").unwrap(), value!(3.0));
    }

    #[test]
    fn test_empty_object_literal() {
        assert_eq!(Evaluator::new().eval("{}").unwrap(), value!({}));
    }

    #[test]
    fn test_object_literal_strings() {
        assert_eq!(
            Evaluator::new().eval("{'foo': {'bar': 'tek'}}").unwrap(),
            value!({"foo": {"bar": "tek"}})
        );
    }

    #[test]
    fn test_object_literal_identifiers() {
        assert_eq!(
            Evaluator::new().eval("{foo: {bar: 'tek'}}").unwrap(),
            value!({"foo": {"bar": "tek"}})
        );
    }

    #[test]
    fn test_object_literal_properties() {
        assert_eq!(
            Evaluator::new().eval("{foo: 'bar'}.foo").unwrap(),
            value!("bar")
        );
    }

    #[test]
    fn test_array_literal() {
        assert_eq!(
            Evaluator::new().eval("['foo', 1+2]").unwrap(),
            value!(["foo", 3.0])
        );
    }

    #[test]
    fn test_array_literal_indexing() {
        assert_eq!(Evaluator::new().eval("[1, 2, 3][1]").unwrap(), value!(2.0));
    }

    #[test]
    fn test_map() {
        let evaluator = Evaluator::new().with_transform("lowercase", |_: Location, v: &[Value]| {
            let s = v
                .get(0)
                .expect("missing value")
                .as_str()
                .expect("Should be a string!");
            Ok(value!(s.to_lowercase()))
        });
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | map(this | lowercase)"#)
                .unwrap(),
            value!(["test", "test"])
        );
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | mapTransform(lowercase)"#)
                .unwrap(),
            value!(["test", "test"])
        );
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | map({ id: this | lowercase })"#)
                .unwrap(),
            value!([{"id": "test"}, {"id": "test"}])
        );
    }

    #[test]
    fn test_filter() {
        let evaluator = Evaluator::new().with_transform("tests", |_: Location, v: &[Value]| {
            let s = v
                .get(0)
                .expect("missing value")
                .as_str()
                .expect("Should be a string!");
            Ok(value!(s == "test"))
        });
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | filter(this | tests)"#)
                .unwrap(),
            value!(["test"])
        );
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | filterTransform(tests)"#)
                .unwrap(),
            value!(["test"])
        );
    }

    #[test]
    fn test_reduce() {
        let evaluator = Evaluator::new();
        assert_eq!(
            evaluator
                .eval(r#"[1, 2, 3] | reduce(0, acc + this)"#)
                .unwrap(),
            value!(6f64)
        );
    }

    #[test]
    fn test_map_filter() {
        let evaluator = Evaluator::new()
            .with_transform("lower", |_: Location, v: &[Value]| {
                let s = v
                    .get(0)
                    .expect("missing value")
                    .as_str()
                    .expect("Should be a string!");
                Ok(value!(s.to_lowercase()))
            })
            .with_transform("tests", |_: Location, v: &[Value]| {
                let s = v
                    .get(0)
                    .expect("missing value")
                    .as_str()
                    .expect("Should be a string!");
                Ok(value![s == "test"])
            });
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | map(this | lower) | filter(this | tests)"#)
                .unwrap(),
            value!(["test", "test"])
        );
        assert_eq!(
            evaluator
                .eval(r#"["TEST", "test"] | filter(this | tests) | map(this | lower)"#)
                .unwrap(),
            value!(["test"])
        );
    }

    #[test]
    fn test_in_operator_string() {
        assert_eq!(
            Evaluator::new().eval("'bar' in 'foobartek'").unwrap(),
            value!(true)
        );
        assert_eq!(
            Evaluator::new().eval("'baz' in 'foobartek'").unwrap(),
            value!(false)
        );
    }

    #[test]
    fn test_in_operator_array() {
        assert_eq!(
            Evaluator::new()
                .eval("'bar' in ['foo', 'bar', 'tek']")
                .unwrap(),
            value!(true)
        );
        assert_eq!(
            Evaluator::new()
                .eval("'baz' in ['foo', 'bar', 'tek']")
                .unwrap(),
            value!(false)
        );
    }

    #[test]
    fn test_conditional_expression() {
        assert_eq!(
            Evaluator::new().eval("'foo' ? 1 : 2").unwrap(),
            value!(1f64)
        );
        assert_eq!(Evaluator::new().eval("'' ? 1 : 2").unwrap(), value!(2f64));
    }

    #[test]
    fn test_arbitrary_whitespace() {
        assert_eq!(
            Evaluator::new().eval("(\t2\n+\n3) *\n4\n\r\n").unwrap(),
            value!(20.0)
        );
    }

    #[test]
    fn test_non_integer() {
        assert_eq!(Evaluator::new().eval("1.5 * 3.0").unwrap(), value!(4.5));
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(
            Evaluator::new().eval("'hello world'").unwrap(),
            value!("hello world")
        );
        assert_eq!(
            Evaluator::new().eval("\"hello world\"").unwrap(),
            value!("hello world")
        );
    }

    #[test]
    fn test_string_escapes() {
        assert_eq!(Evaluator::new().eval("'a\\'b'").unwrap(), value!("a'b"));
        assert_eq!(Evaluator::new().eval("\"a\\\"b\"").unwrap(), value!("a\"b"));
    }

    #[test]
    // Test a very simple transform that applies to_lowercase to a string
    fn test_simple_transform() {
        let evaluator = Evaluator::new().with_transform("lower", |_: Location, v: &[Value]| {
            let s = v
                .get(0)
                .expect("There should be one argument!")
                .as_str()
                .expect("Should be a string!");
            Ok(value!(s.to_lowercase()))
        });
        assert_eq!(evaluator.eval("'T_T'|lower").unwrap(), value!("t_t"));
    }

    #[test]
    // Test returning an UnknownTransform error if a transform is unknown
    fn test_missing_transform() {
        let err = Evaluator::new()
            .with_transform("sqrt", |_: Location, _: &[Value]| unimplemented!())
            .eval("'hello'|world")
            .unwrap_err();
        if let EvaluationError::UnknownTransform(location, transform, transforms) = err {
            assert_eq!(location, (0, 13));
            assert_eq!(transform, "world");
            assert_eq!(transforms, vec![String::from("sqrt")]);
        } else {
            panic!("Should have thrown an unknown transform error")
        }
    }

    #[test]
    // Test returning an UndefinedIdentifier error if an identifier is unknown
    fn test_undefined_identifier() {
        let err = Evaluator::new().eval("not_defined").unwrap_err();
        if let EvaluationError::UndefinedIdentifier(location, id) = err {
            assert_eq!(location, (0, 11));
            assert_eq!(id, "not_defined")
        } else {
            panic!("Should have thrown an undefined identifier error")
        }
    }

    #[test]
    // Test returning an UndefinedIdentifier error if an identifier is unknown
    fn test_undefined_identifier_truthy_ops() {
        let err = Evaluator::new().eval("not_defined").unwrap_err();
        if let EvaluationError::UndefinedIdentifier(location, id) = err {
            assert_eq!(location, (0, 11));
            assert_eq!(id, "not_defined")
        } else {
            panic!("Should have thrown an undefined identifier error")
        }

        let evaluator = Evaluator::new();
        let context = value!({
            "NULL": null,
            "DEFINED": "string",
        });

        let test = |expr: &str, is_ok: bool, exp: Value| {
            let obs = evaluator.eval_in_context(&expr, &context);
            if !is_ok {
                assert!(obs.is_err());
                assert!(matches!(
                    obs.unwrap_err(),
                    EvaluationError::UndefinedIdentifier(_, _)
                ));
            } else {
                assert_eq!(obs.unwrap(), exp,);
            }
        };

        test("UNDEFINED", false, value!(null));
        test("UNDEFINED == 'string'", false, value!(null));
        test("'string' == UNDEFINED", false, value!(null));

        test("UNDEFINED ? 'WRONG' : 'RIGHT'", true, value!("RIGHT"));
        test("DEFINED ? UNDEFINED : 'WRONG'", false, value!(null));

        test("UNDEFINED || 'RIGHT'", true, value!("RIGHT"));
        test("'RIGHT' || UNDEFINED", true, value!("RIGHT"));

        test("'WRONG' && UNDEFINED", false, value!(null));
        test("UNDEFINED && 'WRONG'", false, value!(null));

        test("UNDEFINED && 'WRONG'", false, value!(null));

        test(
            "(UNDEFINED && UNDEFINED == 'string') || (DEFINED && DEFINED == 'string')",
            true,
            value!(true),
        );
    }

    #[test]
    fn test_add_multiple_transforms() {
        let evaluator = Evaluator::new()
            .with_transform("sqrt", |_: Location, v: &[Value]| {
                let num = v
                    .first()
                    .expect("There should be one argument!")
                    .as_f64()
                    .expect("Should be a valid number!");
                Ok(value!(num.sqrt() as u64))
            })
            .with_transform("square", |_: Location, v: &[Value]| {
                let num = v
                    .first()
                    .expect("There should be one argument!")
                    .as_f64()
                    .expect("Should be a valid number!");
                Ok(value!((num as u64).pow(2)))
            });

        assert_eq!(evaluator.eval("4|square").unwrap(), value!(16));
        assert_eq!(evaluator.eval("4|sqrt").unwrap(), value!(2));
        assert_eq!(evaluator.eval("4|square|sqrt").unwrap(), value!(4));
    }

    #[test]
    fn test_transform_with_argument() {
        let evaluator = Evaluator::new().with_transform("split", |_: Location, args: &[Value]| {
            let s = args
                .first()
                .expect("Should be a first argument!")
                .as_str()
                .expect("Should be a string!");
            let c = args
                .get(1)
                .expect("There should be a second argument!")
                .as_str()
                .expect("Should be a string");
            let res: Vec<&str> = s.split_terminator(c).collect();
            Ok(value!(res))
        });

        assert_eq!(
            evaluator.eval("'John Doe'|split(' ')").unwrap(),
            value!(vec!["John", "Doe"])
        );
    }

    /*#[test]
    fn test_custom_error_message() {
        let evaluator = Evaluator::new().with_transform("error", |_: Location, _: &[Value]| {
            Err(EvaluationError::InvalidContext)
        });
        let res = evaluator.eval("1234|error");
        assert!(res.is_err());
        if let EvaluationError::CustomError(e) = res.unwrap_err() {
            assert_eq!(e.to_string(), "Failed transform \"error\" at (0, 10)")
        } else {
            panic!("Should have returned a Custom error!")
        }
    }*/

    #[test]
    fn test_filter_collections_many_returned() {
        let evaluator = Evaluator::new();
        let context = value!({
            "foo": [
                {"bobo": 50, "fofo": 100},
                {"bobo": 60, "baz": 90},
                {"bobo": 10, "bar": 83},
                {"bobo": 20, "yam": 12},
            ]
        });
        let exp = "foo[.bobo >= 50]";
        assert_eq!(
            evaluator.eval_in_context(exp, &context).unwrap(),
            value!([{"bobo": 50, "fofo": 100}, {"bobo": 60, "baz": 90}])
        );
    }

    #[test]
    fn test_matches() {
        let evaluator = Evaluator::new();
        assert_eq!(evaluator.eval(r#" "test" ~ /te*/ "#).unwrap(), value!(true))
    }

    fn test_eval(input: String, output: Value) {
        test_eval_in_context(input, value!({}), output);
    }

    fn test_eval_in_context(input: String, context: Value, output: Value) {
        let evaluator = Evaluator::new();
        let res = evaluator
            .eval_in_context(input.as_str(), &context)
            .map_err(|e| println!("{:?}", e))
            .unwrap();
        assert_eq!(res, output);
    }

    #[test]
    fn test_binary_op_eq_ne() {
        let evaluator = Evaluator::new();
        let context = value!({
            "NULL": null,
            "STRING": "string",
            "BOOLEAN": true,
            "NUMBER": 42,
            "OBJECT": { "x": 1, "y": 2 },
            "ARRAY": [ "string" ]
        });

        let test = |l: &str, r: &str, exp: bool| {
            let expr = format!("{} == {}", l, r);
            assert_eq!(
                evaluator.eval_in_context(&expr, &context).unwrap(),
                value!(exp)
            );

            let expr = format!("{} != {}", l, r);
            assert_eq!(
                evaluator.eval_in_context(&expr, &context).unwrap(),
                value!(!exp)
            );
        };

        test("STRING", "'string'", true);
        test("NUMBER", "42", true);
        test("BOOLEAN", "true", true);
        test("OBJECT", "OBJECT", true);
        test("ARRAY", "[ 'string' ]", true);

        test("OBJECT", "{ 'x': 1, 'y': 2 }", false);

        test("STRING", "NULL", false);
        test("NUMBER", "NULL", false);
        test("BOOLEAN", "NULL", false);
        // test("NULL", "NULL", false);
        test("OBJECT", "NULL", false);
        test("ARRAY", "NULL", false);

        // test("STRING", "STRING", false);
        test("NUMBER", "STRING", false);
        test("BOOLEAN", "STRING", false);
        test("NULL", "STRING", false);
        test("OBJECT", "STRING", false);
        test("ARRAY", "STRING", false);

        test("STRING", "NUMBER", false);
        // test("NUMBER", "NUMBER", false);
        test("BOOLEAN", "NUMBER", false);
        test("NULL", "NUMBER", false);
        test("OBJECT", "NUMBER", false);
        test("ARRAY", "NUMBER", false);

        test("STRING", "BOOLEAN", false);
        test("NUMBER", "BOOLEAN", false);
        // test("BOOLEAN", "BOOLEAN", false);
        test("NULL", "BOOLEAN", false);
        test("OBJECT", "BOOLEAN", false);
        test("ARRAY", "BOOLEAN", false);

        test("STRING", "OBJECT", false);
        test("NUMBER", "OBJECT", false);
        test("BOOLEAN", "OBJECT", false);
        test("NULL", "OBJECT", false);
        // test("OBJECT", "OBJECT", false);
        test("ARRAY", "OBJECT", false);

        test("STRING", "ARRAY", false);
        test("NUMBER", "ARRAY", false);
        test("BOOLEAN", "ARRAY", false);
        test("NULL", "ARRAY", false);
        test("OBJECT", "ARRAY", false);
        // test("ARRAY", "ARRAY", false);
    }

    #[test]
    fn test_binary_op_string_gt_lt_gte_lte() {
        let evaluator = Evaluator::new();
        let context = value!({
            "A": "A string",
            "B": "B string",
        });

        let test = |l: &str, r: &str, is_gt: bool| {
            let expr = format!("{} > {}", l, r);
            assert_eq!(
                evaluator.eval_in_context(&expr, &context).unwrap(),
                value!(is_gt)
            );

            let expr = format!("{} <= {}", l, r);
            assert_eq!(
                evaluator.eval_in_context(&expr, &context).unwrap(),
                value!(!is_gt)
            );

            // we test equality in another test
            let expr = format!("{} == {}", l, r);
            let is_eq = evaluator
                .eval_in_context(&expr, &context)
                .unwrap()
                .as_bool()
                .unwrap();

            if is_eq {
                let expr = format!("{} >= {}", l, r);
                assert_eq!(
                    evaluator.eval_in_context(&expr, &context).unwrap(),
                    value!(true)
                );
            } else {
                let expr = format!("{} < {}", l, r);
                assert_eq!(
                    evaluator.eval_in_context(&expr, &context).unwrap(),
                    value!(!is_gt)
                );
            }
        };

        test("A", "B", false);
        test("B", "A", true);
        test("A", "A", false);
    }

    #[test]
    fn test_lazy_eval_binary_op_and_or() {
        let evaluator = Evaluator::new();
        // error is a missing transform
        let res = evaluator.eval("42 || 0|error");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), value!(42.0));

        let res = evaluator.eval("false || 0|error");
        assert!(res.is_err());

        let res = evaluator.eval("42 && 0|error");
        assert!(res.is_err());

        let res = evaluator.eval("false && 0|error");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), value!(false));
    }

    #[test]
    fn test_lazy_eval_trinary_op() {
        let evaluator = Evaluator::new();
        // error is a missing transform
        let res = evaluator.eval("true ? 42 : 0|error");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), value!(42.0));

        let res = evaluator.eval("true ? 0|error : 42");
        assert!(res.is_err());

        let res = evaluator.eval("true ? 0|error : 42");
        assert!(res.is_err());

        let res = evaluator.eval("false ? 0|error : 42");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), value!(42.0));
    }

    #[rstest]
    #[case(r#" "test" "#, value!("test"))]
    #[case(r#" 12 "#, value!(12f64))]
    #[case(r#" [12] "#, value!([12f64]))]
    #[case(r#" {test: 12} "#, value!({"test": 12f64}))]
    #[case(r#" {"test": 12} "#, value!({"test": 12f64}))]
    #[case(r#" {'test': 12} "#, value!({"test": 12f64}))]
    #[case(r#" {'test': 12} "#, value!({"test": 12f64}))]
    #[case(r#" true "#, value!(true))]
    #[case(r#" false "#, value!(false))]
    fn test_basic_values(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [1, 2] "#, value!([1f64, 2f64]))]
    #[case(r#" [[1, 2]] "#, value!([[1f64, 2f64]]))]
    #[case(r#" [[1, 2]][0] "#, value!([1f64, 2f64]))]
    #[case(r#" [[1, 2]][0][0] "#, value!(1f64))]
    fn test_arrays(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" 'test'[0] "#, value!('t'))]
    fn test_strings(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" !true "#, value!(false))]
    #[case(r#" !false "#, value!(true))]
    #[case(r#" !(true && false) "#, value!(true))]
    #[case(r#" !(2 > 3) "#, value!(true))]
    #[case(r#" -(2 + 3) "#, value!(-5f64))]
    #[case(r#" -1 "#, value!(-1f64))]
    #[case(r#" +1 "#, value!(1f64))]
    fn test_unary_operations(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" 1 + 1 "#, value!(2f64))]
    #[case(r#" 1 - 1 "#, value!(0f64))]
    #[case(r#" 1 / 1 "#, value!(1f64))]
    // #[case(r#" 12 // 5 "#, value!(2f64))]
    #[case(r#" 12 % 5 "#, value!(2f64))]
    #[case(r#" 12 ^ 5 "#, value!(248832f64))]
    #[case(r#" 1 * 1 "#, value!(1f64))]
    fn test_binary_operations(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" duration(1, "second") "#, value!(1))]
    #[case(r#" duration(1, "seconds") "#, value!(1))]
    #[case(r#" duration(1, "minute") "#, value!(60))]
    #[case(r#" duration(1, "minutes") "#, value!(60))]
    #[case(r#" duration(1, "hour") "#, value!(3600))]
    #[case(r#" duration(1, "hours") "#, value!(3600))]
    #[case(r#" duration(1, "day") "#, value!(86400i64))]
    #[case(r#" duration(1, "days") "#, value!(86400i64))]
    #[case(r#" duration(1, "week") "#, value!(604800i64))]
    #[case(r#" duration(1, "weeks") "#, value!(604800i64))]
    #[case(r#" duration(1, "month") "#, value!(2419200i64))]
    #[case(r#" duration(1, "months") "#, value!(2419200i64))]
    #[case(r#" duration(1, "year") "#, value!(31536000i64))]
    #[case(r#" duration(1, "years") "#, value!(31536000i64))]
    fn test_duration(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" date("29/03/1988", "[day]/[month]/[year]") + duration(600, "days") < date("1990-03-29", "[year]-[month]-[day]") "#, value!(true))]
    #[case(r#" date("29/03/1988", "[day]/[month]/[year]") > $now "#, value!(false))]
    fn test_date_and_duration(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" date("1988-03-29", "[year]-[month]-[day]") "#, value!(575596800i64))]
    #[case(r#" date("1988-03-29T00:00:00", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(575596800i64))]
    #[case(r#" datetime("1988-03-29T00:00:00", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(575596800i64))]
    fn test_date_and_datetime(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" "test" ~ /te*/ "#, value!(true))]
    #[case(r#" "2010-01-01T00:00:00" ~ /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/ "#, value!(true))]
    #[case(r#" ( "2010-01-01T00:00:00" @ /(\d{4})-(\d{2})-(\d{2})T\d{2}:\d{2}:\d{2}/ )"#, value!(["2010", "01", "01"]))]
    #[case(r#" ( "2010-01-01T00:00:00" @+ /(\d{4})-(\d{2})-(\d{2})T\d{2}:\d{2}:\d{2}/ )"#, value!([["2010", "01", "01"]]))]
    #[case(r#" ( "2010-01-01T00:00:00" @+ /(\d{4})-(\d{2})-(\d{2})T\d{2}:\d{2}:\d{2}/ )[0]"#, value!(["2010", "01", "01"]))]
    #[case(r#" ( "John, Mike, Bob" @+ /([a-zA-Z]+)(?:, )?/ ) | map(this[0])"#, value!(["John", "Mike", "Bob"]))]
    fn test_regexes(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [1, 2] | map(this + 1) "#, value!([2f64, 3f64]))]
    #[case(r#" {"a": 22, "b": 23} | apply(this.a + this.b) "#, value!(45f64))]
    fn test_map_2(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [{"a": [1, 5, 2]}, {"a": [3, 4]}] | sortBy(this.a[1]) "#, value!([{"a": [3f64, 4f64]}, {"a": [1f64, 5f64, 2f64]}]))]
    #[case(r#" [{"a": [1, 5, 2]}, {"a": [3, 4]}] | sortBy(this.a[1], -1) "#, value!([{"a": [1f64, 5f64, 2f64]}, {"a": [3f64, 4f64]}]))]
    #[case(r#" [{"a": {"b": "b"}}, {"a": {"b": "a"}}] | sortBy(this.a.b) "#, value!([{"a": {"b": "a"}}, {"a": {"b": "b"}}]))]
    #[case(r#" [{"a": {"b": "b"}}, {"a": {"b": "a"}}] | sortBy(this.a.b, -1) "#, value!([{"a": {"b": "b"}}, {"a": {"b": "a"}}]))]
    fn test_sort_by(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [{"a": 1}, {"a": 2}] | find(this.a == 1) "#, value!({"a": 1f64}))]
    #[case(r#" [{"a": 1}, {"a": 2}] | find(this.a == 3) "#, value!(null))]
    fn test_find(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [{"a": 1}, {"a": 2}] | findIndex(this.a == 1) "#, value!(0i64))]
    #[case(r#" [{"a": 1}, {"a": 2}] | findIndex(this.a == 3) "#, value!(-1i64))]
    fn test_find_index(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [{"a": 1}, {"a": 2}] | any(this.a == 1) "#, value!(true))]
    #[case(r#" [{"a": 1}, {"a": 2}] | any(this.a == 3) "#, value!(false))]
    fn test_any(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" [{"a": 1}, {"a": 2}] | all(this.a == 1) "#, value!(false))]
    #[case(r#" [{"a": 1}, {"a": 1}] | all(this.a == 1) "#, value!(true))]
    #[case(r#" [{"a": 1}, {"a": 2}] | all(this.a == 3) "#, value!(false))]
    fn test_all(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }
}
