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
// Support the pattern `mapTransform(lowercase)` and `filterTransform(name)`
// where the first argument is a bare identifier referring to a registered
// transform. In that case we should treat the identifier as the transform
// name (not a context variable) and invoke that transform per-item.
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
use regex_lite::Regex;
use serde_json::{json as value, Value};
use time::OffsetDateTime;

pub mod error;
use error::*;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::RwLock;

// Most transforms receive the subject plus zero or one extra arg. Use a
// small inline capacity of 2 (subject + 1) to reduce stack usage on 64-bit
// platforms while keeping common cases allocation-free.
type Args = SmallVec<[Value; 2]>;

const EPSILON: f64 = 0.000001f64;

fn sort_by_key<F>(array: Vec<Value>, mut key_fn: F, reverse: bool) -> Vec<Value>
where
    F: FnMut(&Value) -> Value,
{
    let mut keyed: Vec<(Value, Value)> = Vec::with_capacity(array.len());
    for v in array.into_iter() {
        let key = key_fn(&v);
        keyed.push((key, v));
    }

    keyed.sort_unstable_by(|(a_val, _), (b_val, _)| {
        if let (Some(a), Some(b)) = (a_val.as_str(), b_val.as_str()) {
            a.cmp(b)
        } else if let (Some(a), Some(b)) = (a_val.as_i64(), b_val.as_i64()) {
            a.cmp(&b)
        } else if let (Some(a), Some(b)) = (a_val.as_f64(), b_val.as_f64()) {
            a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal)
        } else {
            std::cmp::Ordering::Equal
        }
    });

    let mut out: Vec<Value> = keyed.into_iter().map(|(_, v)| v).collect();
    if reverse {
        out.reverse();
    }
    out
}

trait Truthy {
    fn is_truthy(&self) -> bool;
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

impl<'a> Truthy for Cow<'a, Value> {
    fn is_truthy(&self) -> bool {
        self.as_ref().is_truthy()
    }
}

impl<'a> Truthy for Result<Cow<'a, Value>> {
    fn is_truthy(&self) -> bool {
        match self {
            Ok(v) => v.is_truthy(),
            _ => false,
        }
    }
}

pub enum InternalContext<'a> {
    Base(&'a Value),
    Layered {
        base: &'a InternalContext<'a>,
        key: &'a str,
        value: &'a Value,
    },
}

impl<'a> InternalContext<'a> {
    pub fn get(&self, key: &str) -> Option<&Value> {
        match self {
            InternalContext::Base(val) => val.get(key),
            InternalContext::Layered {
                base,
                key: k,
                value,
            } => {
                if key == *k {
                    Some(*value)
                } else {
                    base.get(key)
                }
            }
        }
    }
}

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
    transforms: RwLock<HashMap<String, TransformFn<'a>, fxhash::FxBuildHasher>>,
    parsed: RwLock<HashMap<String, Expression, fxhash::FxBuildHasher>>,
    regexes: RwLock<HashMap<String, Arc<Regex>, fxhash::FxBuildHasher>>,
}

impl<'a> Evaluator<'a> {
    pub fn new() -> Self {
        Evaluator {
            transforms: RwLock::new(HashMap::with_hasher(fxhash::FxBuildHasher::default())),
            parsed: RwLock::new(HashMap::with_hasher(fxhash::FxBuildHasher::default())),
            regexes: RwLock::new(HashMap::with_hasher(fxhash::FxBuildHasher::default())),
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
    /// let evaluator = Evaluator::new().with_transform("lower", |_loc, v: &[Value]| {
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
        {
            let mut w = self.transforms.write().unwrap();
            w.insert(name.to_string(), Arc::new(transform));
        }
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

        let internal_context = InternalContext::Base(context);

        {
            let read = self.parsed.read().unwrap();
            if let Some(tree) = read.get(input) {
                return self
                    .eval_ast(tree, &internal_context)
                    .map(|cow| cow.into_owned());
            }
        }

        // not cached: parse and evaluate locally, then insert for future callers
        let tree = Parser::parse(input).map_err(|_| EvaluationError::ParseError)?;
        let result = self
            .eval_ast(&tree, &internal_context)
            .map(|c| c.into_owned());

        let mut write = self.parsed.write().unwrap();
        write.entry(input.to_string()).or_insert(tree);
        result
    }

    fn get_transform(&self, name: &str, location: Location) -> Result<TransformFn<'a>> {
        let map = self.transforms.read().unwrap();
        map.get(name).cloned().ok_or_else(|| {
            let known = map.keys().cloned().collect();
            EvaluationError::UnknownTransform(location, name.to_string(), known)
        })
    }

    /// Look up a compiled regex by pattern, compiling and caching it on first access.
    fn get_or_compile_regex(&self, pattern: &str, location: Location) -> Result<Arc<Regex>> {
        {
            let read = self.regexes.read().unwrap();
            if let Some(regex) = read.get(pattern) {
                return Ok(regex.clone());
            }
        }

        let regex = Regex::new(pattern)
            .map_err(|_| EvaluationError::InvalidRegex(location, pattern.to_string()))?;
        let arc_regex = Arc::new(regex);

        let mut write = self.regexes.write().unwrap();
        Ok(write
            .entry(pattern.to_string())
            .or_insert_with(|| arc_regex.clone())
            .clone())
    }

    fn eval_ast<'b>(
        &self,
        ast: &Expression,
        context: &'b InternalContext<'b>,
    ) -> Result<Cow<'b, Value>> {
        let Expression {
            expression,
            location,
        } = &ast;
        let location = *location;
        let ast = expression;
        match ast {
            Expr::Number(n) => Ok(Cow::Owned(value!(n))),
            Expr::Boolean(b) => Ok(Cow::Owned(value!(b))),
            Expr::String(s) => Ok(Cow::Owned(value!(s))),
            Expr::Null => Ok(Cow::Owned(value!(null))),
            Expr::Regex(pattern, flags) => {
                // Embed flags as an inline modifier so the regex string is self-contained.
                // e.g. /will/i  →  "(?i)will"
                let regex_str = if flags.is_empty() {
                    pattern.clone()
                } else {
                    format!("(?{}){}", flags, pattern)
                };
                Ok(Cow::Owned(value!(regex_str)))
            }
            Expr::Array(xs) => {
                let result: Result<Vec<Value>> = xs
                    .iter()
                    .map(|x| self.eval_ast(x, context).map(|cow| cow.into_owned()))
                    .collect();
                result.map(|vec| Cow::Owned(Value::Array(vec)))
            }

            Expr::Object(items) => {
                let mut map = serde_json::Map::with_capacity(items.len());
                for (key, expr) in items.iter() {
                    let value = self.eval_ast(expr, context)?.into_owned();
                    if map.insert(key.clone(), value).is_some() {
                        return Err(EvaluationError::DuplicateObjectKey(location, key.clone()));
                    }
                }
                Ok(Cow::Owned(Value::Object(map)))
            }

            Expr::Identifier(inner) => match context.get(inner) {
                Some(v) => Ok(Cow::Borrowed(v)),
                _ => Err(EvaluationError::UndefinedIdentifier(
                    location,
                    inner.clone(),
                )),
            },

            Expr::DotOperation { subject, ident } => {
                let subject = match self.eval_ast(subject, context) {
                    Ok(s) => s,
                    Err(EvaluationError::UndefinedIdentifier(_, _)) => Cow::Owned(value!(null)),
                    Err(e) => return Err(e),
                };

                match subject {
                    Cow::Owned(Value::Array(array)) => {
                        let item = match array.into_iter().next() {
                            Some(it) => it,
                            None => return Ok(Cow::Owned(value!(null))),
                        };
                        if let Value::Object(mut map) = item {
                            Ok(Cow::Owned(map.remove(ident).unwrap_or(value!(null))))
                        } else {
                            Ok(Cow::Owned(item.get(ident).unwrap_or(&value!(null)).clone()))
                        }
                    }
                    Cow::Borrowed(Value::Array(array)) => {
                        Ok(match array.first().and_then(|v| v.get(ident)) {
                            Some(val) => Cow::Borrowed(val),
                            None => Cow::Owned(value!(null)),
                        })
                    }
                    Cow::Owned(Value::Object(mut map)) => {
                        Ok(Cow::Owned(map.remove(ident).unwrap_or(value!(null))))
                    }
                    Cow::Borrowed(Value::Object(map)) => Ok(match map.get(ident) {
                        Some(val) => Cow::Borrowed(val),
                        None => Cow::Owned(value!(null)),
                    }),
                    _ => Ok(Cow::Owned(value!(null))),
                }
            }

            Expr::IndexOperation {
                subject,
                index,
                is_filter,
            } => {
                let subject = match self.eval_ast(subject, context) {
                    Ok(s) => s,
                    Err(EvaluationError::UndefinedIdentifier(_, _)) => Cow::Owned(value!(null)),
                    Err(e) => return Err(e),
                };

                // Filter operation when the index expression contains FilterItemProperty nodes
                if *is_filter {
                    // Filter operation: apply expression to each array element
                    match subject {
                        Cow::Owned(Value::Array(array)) => {
                            let mut result: Vec<Value> = Vec::new();
                            for item in array {
                                let filter_context = InternalContext::Layered {
                                    base: context,
                                    key: "__jexl_filter_item__",
                                    value: &item,
                                };
                                if self
                                    .eval_ast(index, &filter_context)
                                    .map(|val| val.as_ref().as_bool().unwrap_or(false))
                                    .unwrap_or(false)
                                {
                                    result.push(item);
                                }
                            }
                            return Ok(Cow::Owned(Value::Array(result)));
                        }
                        Cow::Borrowed(Value::Array(array)) => {
                            let mut result: Vec<Value> = Vec::new();
                            for item in array {
                                let filter_context = InternalContext::Layered {
                                    base: context,
                                    key: "__jexl_filter_item__",
                                    value: item,
                                };
                                if self
                                    .eval_ast(index, &filter_context)
                                    .map(|val| val.as_ref().as_bool().unwrap_or(false))
                                    .unwrap_or(false)
                                {
                                    result.push(item.clone());
                                }
                            }
                            return Ok(Cow::Owned(Value::Array(result)));
                        }
                        _ => return Err(EvaluationError::ExpectedArray(location)),
                    }
                }

                // Regular indexing (non-filter)
                let index = self.eval_ast(index, context)?;
                match index.as_ref() {
                    Value::String(inner) => match subject {
                        Cow::Owned(Value::Object(mut map)) => {
                            Ok(Cow::Owned(map.remove(inner).unwrap_or(value!(null))))
                        }
                        Cow::Borrowed(Value::Object(map)) => Ok(match map.get(inner) {
                            Some(val) => Cow::Borrowed(val),
                            None => Cow::Owned(value!(null)),
                        }),
                        _ => Ok(Cow::Owned(value!(null))),
                    },
                    Value::Number(inner) => {
                        let idx = inner
                            .as_f64()
                            .ok_or_else(|| {
                                EvaluationError::InvalidType(
                                    location,
                                    ExpectedType::Number,
                                    inner.to_string(),
                                )
                            })?
                            .floor() as usize;

                        match subject {
                            Cow::Owned(Value::Array(mut array)) => {
                                if idx < array.len() {
                                    Ok(Cow::Owned(array.swap_remove(idx)))
                                } else {
                                    Ok(Cow::Owned(value!(null)))
                                }
                            }
                            Cow::Borrowed(Value::Array(array)) => Ok(match array.get(idx) {
                                Some(val) => Cow::Borrowed(val),
                                None => Cow::Owned(value!(null)),
                            }),
                            Cow::Owned(Value::String(s)) => {
                                Ok(Cow::Owned(value!(s.chars().nth(idx))))
                            }
                            Cow::Borrowed(Value::String(s)) => {
                                Ok(Cow::Owned(value!(s.chars().nth(idx))))
                            }
                            _ => Ok(Cow::Owned(value!(null))),
                        }
                    }
                    _ => Err(EvaluationError::InvalidIndexType(location)),
                }
            }
            Expr::UnaryOperation { operation, right } => {
                let right = self.eval_ast(right, context)?;
                match operation {
                    UnCode::Not => Ok(Cow::Owned(value!(!right.is_truthy()))),
                    UnCode::Minus => Ok(Cow::Owned(value!(-right.as_ref().as_f64().ok_or_else(
                        || {
                            EvaluationError::InvalidType(
                                location,
                                ExpectedType::String,
                                right.to_string(),
                            )
                        }
                    )?))),
                    UnCode::Plus => Ok(right),
                }
            }
            Expr::BinaryOperation {
                left,
                right,
                operation,
            } => self.eval_op(location, operation, left, right, context),
            Expr::Transform {
                name,
                subject,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?.into_owned();
                let mut args_arr: Args = Args::new();
                args_arr.push(subject);
                if let Some(args) = args {
                    for arg in args {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }
                let f = self.get_transform(name, location)?;
                let result = f(location, &args_arr)?;
                Ok(Cow::Owned(result))
            }

            Expr::Conditional {
                left,
                truthy,
                falsy,
            } => {
                if self.eval_ast(left, context).is_truthy() {
                    self.eval_ast(truthy, context)
                } else {
                    self.eval_ast(falsy, context)
                }
            }

            Expr::FilterItemProperty(prop) => {
                // Access the special __jexl_filter_item__ from context
                let item = context
                    .get("__jexl_filter_item__")
                    .ok_or(EvaluationError::FilterItemPropertyOutsideFilter(location))?;
                if let Some(val) = item.get(prop) {
                    Ok(Cow::Borrowed(val))
                } else {
                    Ok(Cow::Owned(value!(null)))
                }
            }
            Expr::MapTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;
                let mut args_arr = Vec::new();
                if let Some(args) = args {
                    for arg in args {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let f = self.get_transform(name, location)?;

                match subject {
                    Cow::Owned(Value::Array(array)) => {
                        let res = if args_arr.is_empty() {
                            array
                                .into_iter()
                                .map(|v| f(location, std::slice::from_ref(&v)))
                                .collect::<std::result::Result<Vec<Value>, _>>()?
                        } else {
                            let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                            data.push(Value::Null);
                            data.extend(args_arr.iter().cloned());
                            let mut res = Vec::with_capacity(array.len());
                            for v in array {
                                data[0] = v;
                                res.push(f(location, &data)?);
                            }
                            res
                        };
                        Ok(Cow::Owned(Value::Array(res)))
                    }
                    Cow::Borrowed(Value::Array(array)) => {
                        let res = if args_arr.is_empty() {
                            array
                                .iter()
                                .map(|v| f(location, std::slice::from_ref(v)))
                                .collect::<std::result::Result<Vec<Value>, _>>()?
                        } else {
                            let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                            data.push(Value::Null);
                            data.extend(args_arr.iter().cloned());
                            let mut res = Vec::with_capacity(array.len());
                            for v in array.iter() {
                                data[0] = v.clone();
                                res.push(f(location, &data)?);
                            }
                            res
                        };
                        Ok(Cow::Owned(Value::Array(res)))
                    }
                    _ => Err(EvaluationError::InvalidType(
                        location,
                        ExpectedType::Array,
                        subject.to_string(),
                    )),
                }
            }
            Expr::ExpressionTransform {
                name,
                subject,
                expression,
                args,
            } => match name {
                ExpressionTransform::Map => {
                    let subject = self.eval_ast(subject, context)?;
                    let array = subject.as_ref().as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    // Support shorthand where the inner `expression` is a bare identifier
                    // (e.g. `map(lowercase)`) in case the parser emitted ExpressionTransform
                    if let Expr::Identifier(ref inner_name) = expression.expression {
                        // treat as MapTransform by looking up registered transform
                        let f = self.get_transform(inner_name, location)?;

                        let mut res: Vec<Value> = Vec::with_capacity(array.len());
                        let mut args_arr: Args = Args::with_capacity(1);
                        args_arr.push(Value::Null);
                        for v in array {
                            args_arr[0] = v.clone();
                            res.push(f(location, &args_arr).map_err(|_e| {
                                EvaluationError::FailedTransform(location, inner_name.clone())
                            })?);
                        }
                        return Ok(Cow::Owned(Value::Array(res)));
                    }

                    let mut res: Vec<Value> = Vec::with_capacity(array.len());
                    for (i, v) in array.iter().enumerate() {
                        let idx_val = value!(i as f64);
                        let idx_ctx = InternalContext::Layered {
                            base: context,
                            key: "index",
                            value: &idx_val,
                        };
                        let this_ctx = InternalContext::Layered {
                            base: &idx_ctx,
                            key: "this",
                            value: v,
                        };
                        res.push(self.eval_ast(expression, &this_ctx)?.into_owned());
                    }
                    Ok(Cow::Owned(Value::Array(res)))
                }
                ExpressionTransform::Apply => {
                    let subject = self.eval_ast(subject, context)?;
                    let this_ctx = InternalContext::Layered {
                        base: context,
                        key: "this",
                        value: subject.as_ref(),
                    };
                    let res = self.eval_ast(expression, &this_ctx)?.into_owned();
                    Ok(Cow::Owned(res))
                }
                ExpressionTransform::SortBy => {
                    let subject = self.eval_ast(subject, context)?;
                    let reverse = args == &Some(-1f64);

                    // If expression is a bare identifier, use registered transform per-item
                    if let Expr::Identifier(ref inner_name) = expression.expression {
                        let f = self.get_transform(inner_name, location)?;

                        let arr_vec: Vec<Value> = match subject {
                            Cow::Owned(Value::Array(array)) => array,
                            Cow::Borrowed(Value::Array(array)) => array.to_vec(),
                            _ => {
                                return Err(EvaluationError::InvalidType(
                                    location,
                                    ExpectedType::Array,
                                    subject.to_string(),
                                ))
                            }
                        };

                        let res = sort_by_key(
                            arr_vec,
                            |v| f(location, std::slice::from_ref(v)).unwrap_or(Value::Null),
                            reverse,
                        );

                        return Ok(Cow::Owned(value!(res)));
                    }

                    let arr_vec: Vec<Value> = match subject {
                        Cow::Owned(Value::Array(array)) => array,
                        Cow::Borrowed(Value::Array(array)) => array.to_vec(),
                        _ => {
                            return Err(EvaluationError::InvalidType(
                                location,
                                ExpectedType::Array,
                                subject.to_string(),
                            ))
                        }
                    };

                    let res = sort_by_key(
                        arr_vec,
                        |v| {
                            let this_ctx = InternalContext::Layered {
                                base: context,
                                key: "this",
                                value: v,
                            };
                            self.eval_ast(expression, &this_ctx)
                                .unwrap_or(Cow::Owned(Value::Null))
                                .into_owned()
                        },
                        reverse,
                    );

                    Ok(Cow::Owned(value!(res)))
                }
                ExpressionTransform::Filter => {
                    let subject = self.eval_ast(subject, context)?;

                    // If expression is a bare identifier, treat as FilterTransform shorthand
                    if let Expr::Identifier(ref inner_name) = expression.expression {
                        let f = self.get_transform(inner_name, location)?;

                        let mut res: Vec<Value> = Vec::new();
                        match subject {
                            Cow::Owned(Value::Array(array)) => {
                                for v in array {
                                    let keep = f(location, std::slice::from_ref(&v))
                                        .unwrap_or(value!(false))
                                        .as_bool()
                                        .unwrap_or(false);
                                    if keep {
                                        res.push(v);
                                    }
                                }
                            }
                            Cow::Borrowed(Value::Array(array)) => {
                                for v in array {
                                    let keep = f(location, std::slice::from_ref(v))
                                        .unwrap_or(value!(false))
                                        .as_bool()
                                        .unwrap_or(false);
                                    if keep {
                                        res.push(v.clone());
                                    }
                                }
                            }
                            _ => return Ok(Cow::Owned(value!([]))),
                        }

                        return Ok(Cow::Owned(Value::Array(res)));
                    }

                    let mut res: Vec<Value> = Vec::new();
                    match subject {
                        Cow::Owned(Value::Array(array)) => {
                            for (i, v) in array.into_iter().enumerate() {
                                let idx_val = value!(i as f64);
                                let idx_ctx = InternalContext::Layered {
                                    base: context,
                                    key: "index",
                                    value: &idx_val,
                                };
                                let this_ctx = InternalContext::Layered {
                                    base: &idx_ctx,
                                    key: "this",
                                    value: &v,
                                };
                                let keep = self
                                    .eval_ast(expression, &this_ctx)
                                    .map_err(|_| {
                                        EvaluationError::FailedEvaluation(
                                            location,
                                            "filter".to_string(),
                                        )
                                    })?
                                    .as_ref()
                                    .as_bool()
                                    .unwrap_or(false);
                                if keep {
                                    res.push(v);
                                }
                            }
                        }
                        Cow::Borrowed(Value::Array(array)) => {
                            for (i, v) in array.iter().enumerate() {
                                let idx_val = value!(i as f64);
                                let idx_ctx = InternalContext::Layered {
                                    base: context,
                                    key: "index",
                                    value: &idx_val,
                                };
                                let this_ctx = InternalContext::Layered {
                                    base: &idx_ctx,
                                    key: "this",
                                    value: v,
                                };
                                let keep = self
                                    .eval_ast(expression, &this_ctx)
                                    .map_err(|_| {
                                        EvaluationError::FailedEvaluation(
                                            location,
                                            "filter".to_string(),
                                        )
                                    })?
                                    .as_ref()
                                    .as_bool()
                                    .unwrap_or(false);
                                if keep {
                                    res.push(v.clone());
                                }
                            }
                        }
                        _ => return Ok(Cow::Owned(value!([]))),
                    }

                    Ok(Cow::Owned(Value::Array(res)))
                }
                ExpressionTransform::Any => {
                    let subject = self.eval_ast(subject, context)?;
                    if subject.as_ref() == &Value::Null {
                        return Ok(Cow::Owned(Value::Bool(false)));
                    }
                    let array = subject.as_ref().as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().enumerate().any(|(i, v)| {
                        let idx_val = value!(i as f64);
                        let idx_ctx = InternalContext::Layered {
                            base: context,
                            key: "index",
                            value: &idx_val,
                        };
                        let this_ctx = InternalContext::Layered {
                            base: &idx_ctx,
                            key: "this",
                            value: v,
                        };
                        self.eval_ast(expression, &this_ctx)
                            .unwrap_or(Cow::Owned(Value::Bool(false)))
                            .is_truthy()
                    });
                    Ok(Cow::Owned(value!(res)))
                }
                ExpressionTransform::All => {
                    let subject = self.eval_ast(subject, context)?;
                    let array = subject.as_ref().as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().enumerate().all(|(i, v)| {
                        let idx_val = value!(i as f64);
                        let idx_ctx = InternalContext::Layered {
                            base: context,
                            key: "index",
                            value: &idx_val,
                        };
                        let this_ctx = InternalContext::Layered {
                            base: &idx_ctx,
                            key: "this",
                            value: v,
                        };
                        self.eval_ast(expression, &this_ctx)
                            .unwrap_or(Cow::Owned(Value::Bool(false)))
                            .is_truthy()
                    });
                    Ok(Cow::Owned(value!(res)))
                }
                ExpressionTransform::Find => {
                    let subject = self.eval_ast(subject, context)?;
                    let array = subject.as_ref().as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().enumerate().find(|(i, v)| {
                        let idx_val = value!(*i as f64);
                        let idx_ctx = InternalContext::Layered {
                            base: context,
                            key: "index",
                            value: &idx_val,
                        };
                        let this_ctx = InternalContext::Layered {
                            base: &idx_ctx,
                            key: "this",
                            value: v,
                        };
                        self.eval_ast(expression, &this_ctx)
                            .unwrap_or(Cow::Owned(Value::Bool(false)))
                            .is_truthy()
                    });
                    Ok(Cow::Owned(value!(res
                        .map(|(_i, v)| v.clone())
                        .unwrap_or(Value::Null))))
                }
                ExpressionTransform::FindIndex => {
                    let subject = self.eval_ast(subject, context)?;
                    let array = subject.as_ref().as_array().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Array,
                            subject.to_string(),
                        )
                    })?;
                    let res = array.iter().enumerate().position(|(i, v)| {
                        let idx_val = value!(i as f64);
                        let idx_ctx = InternalContext::Layered {
                            base: context,
                            key: "index",
                            value: &idx_val,
                        };
                        let this_ctx = InternalContext::Layered {
                            base: &idx_ctx,
                            key: "this",
                            value: v,
                        };
                        self.eval_ast(expression, &this_ctx)
                            .unwrap_or(Cow::Owned(Value::Bool(false)))
                            .is_truthy()
                    });
                    Ok(Cow::Owned(value!(res.map(|v| v as i64).unwrap_or(-1))))
                }
            },
            Expr::SortByTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;

                // Build args array and detect optional direction flag as first numeric arg
                let mut args_arr: Args = Args::new();
                if let Some(arg_exprs) = args {
                    for arg in arg_exprs {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let mut reverse = false;
                if let Some(first) = args_arr.first() {
                    if let Some(n) = first.as_f64() {
                        if (n - -1.0).abs() < f64::EPSILON {
                            reverse = true;
                            args_arr.remove(0);
                        }
                    }
                }

                let f = self.get_transform(name, location)?;

                let arr_vec: Vec<Value> = match subject {
                    Cow::Owned(Value::Array(array)) => array,
                    Cow::Borrowed(Value::Array(array)) => array.to_vec(),
                    _ => {
                        return Err(EvaluationError::InvalidType(
                            location,
                            ExpectedType::Array,
                            subject.to_string(),
                        ))
                    }
                };

                let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                data.push(Value::Null);
                data.extend(args_arr.iter().cloned());

                let res = sort_by_key(
                    arr_vec,
                    |v| {
                        data[0] = v.clone();
                        f(location, &data).unwrap_or(Value::Null)
                    },
                    reverse,
                );

                Ok(Cow::Owned(value!(res)))
            }

            Expr::AnyTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;
                let mut args_arr: Args = Args::new();
                if let Some(arg_exprs) = args {
                    for arg in arg_exprs {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let f = self.get_transform(name, location)?;

                if subject.as_ref() == &Value::Null {
                    return Ok(Cow::Owned(Value::Bool(false)));
                }

                let array = subject.as_ref().as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, subject.to_string())
                })?;

                let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                data.push(Value::Null);
                data.extend(args_arr.iter().cloned());
                let res = array.iter().any(|v| {
                    data[0] = v.clone();
                    f(location, &data).unwrap_or(value!(false)).is_truthy()
                });

                Ok(Cow::Owned(value!(res)))
            }

            Expr::AllTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;
                let mut args_arr: Args = Args::new();
                if let Some(arg_exprs) = args {
                    for arg in arg_exprs {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let f = self.get_transform(name, location)?;

                let array = subject.as_ref().as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, subject.to_string())
                })?;

                let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                data.push(Value::Null);
                data.extend(args_arr.iter().cloned());
                let res = array.iter().all(|v| {
                    data[0] = v.clone();
                    f(location, &data).unwrap_or(value!(false)).is_truthy()
                });

                Ok(Cow::Owned(value!(res)))
            }

            Expr::FindTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;
                let mut args_arr: Args = Args::new();
                if let Some(arg_exprs) = args {
                    for arg in arg_exprs {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let f = self.get_transform(name, location)?;

                let array = subject.as_ref().as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, subject.to_string())
                })?;

                let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                data.push(Value::Null);
                data.extend(args_arr.iter().cloned());
                let res = array.iter().find(|v| {
                    data[0] = (*v).clone();
                    f(location, &data).unwrap_or(value!(false)).is_truthy()
                });

                Ok(Cow::Owned(value!(res.cloned().unwrap_or(Value::Null))))
            }

            Expr::FindIndexTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;
                let mut args_arr: Args = Args::new();
                if let Some(arg_exprs) = args {
                    for arg in arg_exprs {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let f = self.get_transform(name, location)?;

                let array = subject.as_ref().as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, subject.to_string())
                })?;

                let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                data.push(Value::Null);
                data.extend(args_arr.iter().cloned());
                let res = array.iter().position(|v| {
                    data[0] = v.clone();
                    f(location, &data).unwrap_or(value!(false)).is_truthy()
                });

                Ok(Cow::Owned(value!(res.map(|v| v as i64).unwrap_or(-1))))
            }
            Expr::ReduceExpression {
                subject,
                init,
                expression,
            } => {
                // initializing against current context since will probably just be a literal value,
                // and if it's an expression it can refer to other context variables but not "acc" or "this"
                // since those only get defined within the reduce expression itself
                let init = self.eval_ast(init, context)?.into_owned();
                let subject = self.eval_ast(subject, context)?;

                let array = subject.as_ref().as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, subject.to_string())
                })?;

                let res = array.iter().try_fold(init, |acc, v| {
                    let acc_ctx = InternalContext::Layered {
                        base: context,
                        key: "acc",
                        value: &acc,
                    };
                    let this_ctx = InternalContext::Layered {
                        base: &acc_ctx,
                        key: "this",
                        value: v,
                    };
                    self.eval_ast(expression, &this_ctx)
                        .map(|cow| cow.into_owned())
                })?;
                Ok(Cow::Owned(res))
            }
            Expr::FilterTransform {
                subject,
                name,
                args,
            } => {
                let subject = self.eval_ast(subject, context)?;
                let mut args_arr: Args = Args::new();
                if let Some(args) = args {
                    for arg in args {
                        args_arr.push(self.eval_ast(arg, context)?.into_owned());
                    }
                }

                let f = self.get_transform(name, location)?;

                match subject {
                    Cow::Owned(Value::Array(array)) => {
                        let res: Vec<Value> = if args_arr.is_empty() {
                            array
                                .into_iter()
                                .filter(|v| {
                                    f(location, std::slice::from_ref(v))
                                        .unwrap_or(value![false])
                                        .as_bool()
                                        .unwrap_or(false)
                                })
                                .collect()
                        } else {
                            let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                            data.push(Value::Null);
                            data.extend(args_arr.iter().cloned());
                            let mut res = Vec::new();
                            for v in array {
                                data[0] = v;
                                let keep = f(location, &data)
                                    .unwrap_or(value![false])
                                    .as_bool()
                                    .unwrap_or(false);
                                if keep {
                                    res.push(std::mem::replace(&mut data[0], Value::Null));
                                }
                            }
                            res
                        };
                        Ok(Cow::Owned(Value::Array(res)))
                    }
                    Cow::Borrowed(Value::Array(array)) => {
                        let res: Vec<Value> = if args_arr.is_empty() {
                            array
                                .iter()
                                .filter(|v| {
                                    f(location, std::slice::from_ref(*v))
                                        .unwrap_or(value![false])
                                        .as_bool()
                                        .unwrap_or(false)
                                })
                                .cloned()
                                .collect()
                        } else {
                            let mut data: Vec<Value> = Vec::with_capacity(1 + args_arr.len());
                            data.push(Value::Null);
                            data.extend(args_arr.iter().cloned());
                            let mut res = Vec::new();
                            for v in array.iter() {
                                data[0] = v.clone();
                                let keep = f(location, &data)
                                    .unwrap_or(value![false])
                                    .as_bool()
                                    .unwrap_or(false);
                                if keep {
                                    res.push(std::mem::replace(&mut data[0], Value::Null));
                                }
                            }
                            res
                        };
                        Ok(Cow::Owned(Value::Array(res)))
                    }
                    _ => Ok(Cow::Owned(value!([]))),
                }
            }
            Expr::Now => {
                #[cfg(not(target_arch = "wasm32"))]
                let ts = OffsetDateTime::now_local()
                    .unwrap_or_else(|_| OffsetDateTime::now_utc())
                    .unix_timestamp();
                #[cfg(target_arch = "wasm32")]
                let ts = OffsetDateTime::now_utc().unix_timestamp();
                Ok(Cow::Owned(value!(ts)))
            }
            Expr::NowUtc => Ok(Cow::Owned(value!(
                OffsetDateTime::now_utc().unix_timestamp()
            ))),
        }
    }

    fn eval_op<'b>(
        &self,
        location: Location,
        operation: &OpCode,
        left: &Expression,
        right: &Expression,
        context: &'b InternalContext<'b>,
    ) -> Result<Cow<'b, Value>> {
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
            _ => Cow::Owned(self.apply_op(
                location,
                operation,
                left?.as_ref(),
                eval_right()?.as_ref(),
            )?),
        })
    }

    fn apply_op(
        &self,
        location: Location,
        operation: &OpCode,
        left: &Value,
        right: &Value,
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
            (OpCode::And, _, _) => unreachable!("And is handled in eval_op"),
            (OpCode::Or, _, _) => unreachable!("Or is handled in eval_op"),
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
                OpCode::In => Ok(value!(b.contains(a))),

                OpCode::Less => Ok(value!(a < b)),
                OpCode::Greater => Ok(value!(a > b)),
                OpCode::LessEqual => Ok(value!(a <= b)),
                OpCode::GreaterEqual => Ok(value!(a >= b)),
                OpCode::Matches => {
                    let regex = self.get_or_compile_regex(b, location)?;
                    Ok(value!(regex.is_match(a)))
                }
                OpCode::Capture => {
                    let regex = self.get_or_compile_regex(b, location)?;
                    let captures = regex.captures(a);
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
                    let regex = self.get_or_compile_regex(b, location)?;
                    let captures: Vec<_> = regex.captures_iter(a).collect();

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
            (OpCode::In, left, Value::Array(v)) => Ok(value!(v.contains(left))),
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
                left: left.clone(),
                right: right.clone(),
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
    fn test_null_literal() {
        assert_eq!(Evaluator::new().eval("null").unwrap(), value!(null));
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
                .first()
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
                .eval(r#"["TEST", "test"] | map(lowercase)"#)
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
    fn test_expression_index_map_filter_find() {
        let evaluator = Evaluator::new();

        // map: add index to each item
        assert_eq!(
            evaluator.eval("[1, 2, 3] | map(this + index)").unwrap(),
            value!([1.0, 3.0, 5.0])
        );

        // filter: keep items at even indices
        assert_eq!(
            evaluator
                .eval("[10, 11, 12, 13] | filter(index % 2 == 0)")
                .unwrap(),
            value!([10.0, 12.0])
        );

        // find: return element where index == 2
        assert_eq!(
            evaluator.eval("[5, 6, 7] | find(index == 2)").unwrap(),
            value!(7.0)
        );

        // findIndex: index of element matching predicate
        assert_eq!(
            evaluator.eval("[5, 6, 7] | findIndex(index == 1)").unwrap(),
            value!(1)
        );
    }

    #[test]
    fn debug_map_parses() {
        let evaluator = Evaluator::new().with_transform("lowercase", |_: Location, v: &[Value]| {
            let s = v
                .first()
                .expect("missing value")
                .as_str()
                .expect("Should be a string!");
            Ok(value!(s.to_lowercase()))
        });
        let exps = vec![
            r#"["TEST", "test"] | map(this | lowercase)"#,
            r#"["TEST", "test"] | mapTransform(lowercase)"#,
            r#"["TEST", "test"] | map(lowercase)"#,
            r#"["TEST", "test"] | map({ id: this | lowercase })"#,
        ];
        for e in exps {
            match Parser::parse(e) {
                Ok(tree) => println!("AST - {} => {:?}", e, tree),
                Err(_) => println!("AST - {} => <parse error>", e),
            }
            match evaluator.eval(e) {
                Ok(v) => println!("OK - {} => {}", e, v),
                Err(err) => println!("ERR - {} => {:?}", e, err),
            }
        }
    }

    #[test]
    fn test_filter() {
        let evaluator = Evaluator::new().with_transform("tests", |_: Location, v: &[Value]| {
            let s = v
                .first()
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
                .eval(r#"["TEST", "test"] | filter(tests)"#)
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
                    .first()
                    .expect("missing value")
                    .as_str()
                    .expect("Should be a string!");
                Ok(value!(s.to_lowercase()))
            })
            .with_transform("tests", |_: Location, v: &[Value]| {
                let s = v
                    .first()
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
                .first()
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
            let obs = evaluator.eval_in_context(expr, &context);
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
    // TODO: -2^2 currently evaluates as (-2)^2 = 4 because unary minus binds
    // tighter than exponent in the current grammar. Mathematically it should be
    // -(2^2) = -4. Use -(2^2) explicitly until the grammar is fixed.
    #[case(r#" -(2^2) "#, value!(-4f64))]
    #[case(r#" -2^2 "#, value!(4f64))]
    fn test_unary_operations(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[rstest]
    #[case(r#" 1 + 1 "#, value!(2f64))]
    #[case(r#" 1 - 1 "#, value!(0f64))]
    #[case(r#" 1 / 1 "#, value!(1f64))]
    #[case(r#" 12 // 5 "#, value!(2f64))]
    #[case(r#" 12 % 5 "#, value!(2f64))]
    #[case(r#" 12 ^ 5 "#, value!(248832f64))]
    #[case(r#" 1 * 1 "#, value!(1f64))]
    fn test_binary_operations(#[case] input: String, #[case] output: Value) {
        test_eval(input, output)
    }

    #[test]
    fn test_now_returns_unix_timestamp() {
        let evaluator = Evaluator::new();
        let result = evaluator.eval("$now").unwrap();
        let ts = result
            .as_i64()
            .expect("$now should return an integer timestamp");
        // Must be after 2024-01-01 (unix 1704067200) and before year 2100 (unix 4102444800)
        assert!(
            ts > 1_704_067_200,
            "$now returned implausibly small value: {}",
            ts
        );
        assert!(
            ts < 4_102_444_800,
            "$now returned implausibly large value: {}",
            ts
        );
    }

    #[test]
    fn test_now_utc_returns_unix_timestamp() {
        let evaluator = Evaluator::new();
        let result = evaluator.eval("$now_utc").unwrap();
        let ts = result
            .as_i64()
            .expect("$now_utc should return an integer timestamp");
        // Must be after 2024-01-01 (unix 1704067200) and before year 2100 (unix 4102444800)
        assert!(
            ts > 1_704_067_200,
            "$now_utc returned implausibly small value: {}",
            ts
        );
        assert!(
            ts < 4_102_444_800,
            "$now_utc returned implausibly large value: {}",
            ts
        );
    }

    #[rstest]
    #[case(r#" "test" ~ /te*/ "#, value!(true))]
    #[case(r#" "2010-01-01T00:00:00" ~ /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/ "#, value!(true))]
    #[case(r#" ( "2010-01-01T00:00:00" @ /(\d{4})-(\d{2})-(\d{2})T\d{2}:\d{2}:\d{2}/ )"#, value!(["2010", "01", "01"]))]
    #[case(r#" ( "2010-01-01T00:00:00" @+ /(\d{4})-(\d{2})-(\d{2})T\d{2}:\d{2}:\d{2}/ )"#, value!([["2010", "01", "01"]]))]
    #[case(r#" ( "2010-01-01T00:00:00" @+ /(\d{4})-(\d{2})-(\d{2})T\d{2}:\d{2}:\d{2}/ )[0]"#, value!(["2010", "01", "01"]))]
    #[case(r#" ( "John, Mike, Bob" @+ /([a-zA-Z]+)(?:, )?/ ) | map(this[0])"#, value!(["John", "Mike", "Bob"]))]
    // case-insensitive flag /i
    #[case(r#" "Will" ~ /will/i "#, value!(true))]
    #[case(r#" "WILL" ~ /will/i "#, value!(true))]
    #[case(r#" "will" ~ /will/i "#, value!(true))]
    #[case(r#" "nope" ~ /will/i "#, value!(false))]
    #[case(r#" ( "Hello World" @ /(world)/i )"#, value!(["World"]))]
    #[case(r#" ( "Hello World" @+ /(world)/i )"#, value!([["World"]]))]
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

    // New filter expression tests
    #[test]
    fn test_filter_simple() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Malory", "last": "Archer", "age": 75},
                {"first": "Lana", "last": "Kane", "age": 33},
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.first == 'Sterling']", &context)
                .unwrap(),
            value!([{"first": "Sterling", "last": "Archer", "age": 36}])
        );
    }

    #[test]
    fn test_filter_complex_and() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Malory", "last": "Archer", "age": 75},
                {"first": "Lana", "last": "Kane", "age": 33},
                {"first": "Cyril", "last": "Figgis", "age": 45},
                {"first": "Cheryl", "last": "Tunt", "age": 28}
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age >= 30 && .age < 40]", &context)
                .unwrap(),
            value!([
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Lana", "last": "Kane", "age": 33}
            ])
        );
    }

    #[test]
    fn test_filter_with_context() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Malory", "last": "Archer", "age": 75},
                {"first": "Lana", "last": "Kane", "age": 33},
            ],
            "retireAge": 62
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age >= retireAge]", &context)
                .unwrap(),
            value!([{"first": "Malory", "last": "Archer", "age": 75}])
        );
    }

    #[test]
    fn test_filter_chained_access() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Cheryl", "last": "Tunt", "age": 28}
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.last == 'Tunt'].first", &context)
                .unwrap(),
            value!("Cheryl")
        );
    }

    #[test]
    fn test_filter_chained_filters() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Malory", "last": "Archer", "age": 75},
                {"first": "Lana", "last": "Kane", "age": 33},
                {"first": "Cyril", "last": "Figgis", "age": 45},
                {"first": "Cheryl", "last": "Tunt", "age": 28}
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age >= 30 && .age < 40][.age < 35]", &context)
                .unwrap(),
            value!([{"first": "Lana", "last": "Kane", "age": 33}])
        );
    }

    #[test]
    fn test_filter_no_matches() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Lana", "last": "Kane", "age": 33},
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age > 100]", &context)
                .unwrap(),
            value!([])
        );
    }

    #[test]
    fn test_filter_all_match() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Lana", "last": "Kane", "age": 33},
            ]
        });
        let result = Evaluator::new()
            .eval_in_context("employees[.age > 0]", &context)
            .unwrap();
        assert_eq!(
            result,
            value!([
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Lana", "last": "Kane", "age": 33}
            ])
        );
    }

    #[test]
    fn test_filter_nested_property() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "address": {"city": "New York"}},
                {"first": "Lana", "address": {"city": "Los Angeles"}},
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.address.city == 'New York']", &context)
                .unwrap(),
            value!([{"first": "Sterling", "address": {"city": "New York"}}])
        );
    }

    #[test]
    fn test_filter_empty_array() {
        let context = value!({"arr": []});
        assert_eq!(
            Evaluator::new()
                .eval_in_context("arr[.x > 0]", &context)
                .unwrap(),
            value!([])
        );
    }

    #[test]
    fn test_filter_with_expression() {
        let context = value!({
            "employees": [
                {"first": "Cheryl", "last": "Tunt", "age": 28},
                {"first": "Sterling", "last": "Archer", "age": 36},
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.last == 'Tu' + 'nt'].first", &context)
                .unwrap(),
            value!("Cheryl")
        );
    }

    #[test]
    fn test_filter_complex_boolean() {
        let context = value!({
            "employees": [
                {"name": "John", "age": 35},
                {"name": "Jane", "age": 25},
                {"name": "Bob", "age": 45},
            ]
        });
        assert_eq!(
            Evaluator::new()
                .eval_in_context(
                    "employees[.age > 30 && (.name == 'John' || .name == 'Jane')]",
                    &context
                )
                .unwrap(),
            value!([{"name": "John", "age": 35}])
        );
    }

    #[test]
    fn test_comprehensive_collection_filtering() {
        let context = value!({
            "employees": [
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Malory", "last": "Archer", "age": 75},
                {"first": "Lana", "last": "Kane", "age": 33},
                {"first": "Cyril", "last": "Figgis", "age": 45},
                {"first": "Cheryl", "last": "Tunt", "age": 28}
            ],
            "retireAge": 62
        });

        // Test all examples from the spec
        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.first == 'Sterling']", &context)
                .unwrap(),
            value!([{"first": "Sterling", "last": "Archer", "age": 36}])
        );

        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.last == 'Tu' + 'nt'].first", &context)
                .unwrap(),
            value!("Cheryl")
        );

        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age >= 30 && .age < 40]", &context)
                .unwrap(),
            value!([
                {"first": "Sterling", "last": "Archer", "age": 36},
                {"first": "Lana", "last": "Kane", "age": 33}
            ])
        );

        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age >= 30 && .age < 40][.age < 35]", &context)
                .unwrap(),
            value!([{"first": "Lana", "last": "Kane", "age": 33}])
        );

        assert_eq!(
            Evaluator::new()
                .eval_in_context("employees[.age >= retireAge].first", &context)
                .unwrap(),
            value!("Malory")
        );
    }

    #[test]
    fn test_filter_property_outside_filter_context() {
        let evaluator = Evaluator::new();
        let result = evaluator.eval(".age");
        assert!(result.is_err());
        if let Err(EvaluationError::FilterItemPropertyOutsideFilter(_)) = result {
            // Expected error
        } else {
            panic!("Expected FilterItemPropertyOutsideFilter error");
        }
    }

    #[test]
    fn test_filter_non_array() {
        let context = value!({"str": "hello"});
        let evaluator = Evaluator::new();
        let result = evaluator.eval_in_context("str[.x > 0]", &context);
        assert!(result.is_err());
        if let Err(EvaluationError::ExpectedArray(_)) = result {
            // Expected error
        } else {
            panic!("Expected ExpectedArray error");
        }
    }

    #[test]
    fn test_do_operation_on_arrays() {
        let evaluator = Evaluator::new();
        assert_eq!(
            evaluator
                .eval_in_context(
                    "arr.name",
                    &value!({"arr": [{"name": "Alice"}, {"name": "Bob"}]})
                )
                .unwrap(),
            value!("Alice")
        );
    }
}
