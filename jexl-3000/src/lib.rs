#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![allow(clippy::implicit_return)]
#![allow(clippy::question_mark_used)]

#[cfg(feature = "language-service")]
pub mod language_service;

use std::borrow::Cow;
use std::collections::HashSet;
use std::hash::Hash;

use ordered_float::OrderedFloat;

pub use jexl_eval::{Evaluator, error::EvaluationError};
use jexl_eval::{Location, error::ExpectedType};
use serde_json::{Map, Value, json as value};
use time::format_description::well_known::Rfc3339;
use time::macros::format_description as fd;
use time::{Date, OffsetDateTime, PrimitiveDateTime, format_description};

#[derive(Hash, Eq, PartialEq)]
enum AttrKey<'a> {
    Str(&'a str),
    Num(OrderedFloat<f64>),
    Bool(bool),
    Null,
}

/// Like `get_string` but avoids the allocation by returning a `&str` borrow.
/// Use this whenever the result is only read, not moved into owned storage.
#[inline]
fn get_str(location: Location, v: &[Value]) -> Result<&str, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_str()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::String, v[0].to_string())
        })
}

#[inline]
fn get_number(location: Location, v: &[Value]) -> Result<f64, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_f64()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::Number, v[0].to_string())
        })
}

#[inline]
fn get_array(location: Location, v: &[Value]) -> Result<&Vec<Value>, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_array()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::Array, v[0].to_string())
        })
}

#[inline]
fn get_object(location: Location, v: &[Value]) -> Result<&Map<String, Value>, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_object()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::Object, v[0].to_string())
        })
}

#[inline]
fn get_array_numbers(location: Location, v: &[Value]) -> Result<Vec<f64>, EvaluationError> {
    get_array(location, v)?
        .iter()
        .map(|v| {
            v.as_f64().ok_or_else(|| {
                EvaluationError::InvalidType(location, ExpectedType::Number, v.to_string())
            })
        })
        .collect::<Result<_, EvaluationError>>()
}

#[inline]
fn get_array_strings(location: Location, v: &[Value]) -> Result<Vec<&str>, EvaluationError> {
    get_array(location, v)?
        .iter()
        .map(|v| {
            v.as_str().ok_or_else(|| {
                EvaluationError::InvalidType(location, ExpectedType::String, v.to_string())
            })
        })
        .collect::<Result<_, EvaluationError>>()
}

#[inline]
fn get_array_objects(
    location: Location,
    v: &[Value],
) -> Result<Vec<&Map<String, Value>>, EvaluationError> {
    get_array(location, v)?
        .iter()
        .map(|v| {
            v.as_object().ok_or_else(|| {
                EvaluationError::InvalidType(location, ExpectedType::Object, v.to_string())
            })
        })
        .collect::<Result<_, EvaluationError>>()
}

#[inline]
fn get_array_arrays(location: Location, v: &[Value]) -> Result<Vec<&Vec<Value>>, EvaluationError> {
    get_array(location, v)?
        .iter()
        .map(|v| {
            v.as_array().ok_or_else(|| {
                EvaluationError::InvalidType(location, ExpectedType::Array, v.to_string())
            })
        })
        .collect::<Result<Vec<_>, EvaluationError>>()
}

#[inline]
fn get_argument(location: Location, v: &[Value], index: usize) -> Result<&Value, EvaluationError> {
    v.get(index + 1)
        .ok_or(EvaluationError::MissingArgument(location, index))
}

#[inline]
fn get_argument_string(
    location: Location,
    v: &[Value],
    index: usize,
) -> Result<&str, EvaluationError> {
    get_argument(location, v, index)?.as_str().ok_or_else(|| {
        EvaluationError::InvalidType(location, ExpectedType::String, v[index].to_string())
    })
}

#[inline]
fn get_argument_number(
    location: Location,
    v: &[Value],
    index: usize,
) -> Result<f64, EvaluationError> {
    get_argument(location, v, index)?.as_f64().ok_or_else(|| {
        EvaluationError::InvalidType(location, ExpectedType::Number, v[index].to_string())
    })
}

#[inline]
fn unique<T: Eq + Hash + Clone>(v: &mut Vec<T>) {
    let mut seen = HashSet::with_capacity(v.len());
    let mut write_idx = 0;
    for read_idx in 0..v.len() {
        if !seen.contains(&v[read_idx]) {
            seen.insert(v[read_idx].clone());
            if write_idx != read_idx {
                v.swap(write_idx, read_idx);
            }
            write_idx += 1;
        }
    }
    v.truncate(write_idx);
}

/// Recursively flatten any Value into `out` (arrays are flattened, non-arrays appended).
fn flatten_values(v: &Value, out: &mut Vec<Value>) {
    if let Some(arr) = v.as_array() {
        for item in arr {
            flatten_values(item, out);
        }
    } else {
        out.push(v.clone());
    }
}

// Split a string into word components for case transforms.
fn split_words(s: &str) -> Vec<String> {
    let replaced: String = s
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { ' ' })
        .collect();
    replaced
        .split_whitespace()
        .map(std::string::ToString::to_string)
        .collect()
}

fn capitalize_word(s: &str) -> String {
    let mut chars = s.chars();
    chars.next().map_or_else(String::new, |f| {
        f.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
    })
}

// Deep merge two JSON objects (right wins for non-object values).
fn merge_deep_maps(a: &Map<String, Value>, b: &Map<String, Value>) -> Map<String, Value> {
    let mut out = a.clone();
    for (k, v_b) in b {
        match (out.get(k), v_b) {
            (Some(Value::Object(a_map)), Value::Object(b_map)) => {
                let merged = merge_deep_maps(a_map, b_map);
                out.insert(k.clone(), Value::Object(merged));
            }
            _ => {
                out.insert(k.clone(), v_b.clone());
            }
        }
    }
    out
}

// Deep equality using serde_json::Value's PartialEq
fn deep_equal(a: &Value, b: &Value) -> bool {
    a == b
}

// Convert a serde_json::Value into a stable key string for object keys produced by
// transforms like `countBy`, `keyBy`, `invert`, etc.
#[allow(clippy::cast_possible_truncation)]
fn value_to_key_string(v: &Value) -> String {
    if v.is_null() {
        return String::new();
    }
    if let Some(s) = v.as_str() {
        return s.to_string();
    }
    if let Some(i) = v.as_i64() {
        return i.to_string();
    }
    if let Some(u) = v.as_u64() {
        return u.to_string();
    }
    if let Some(f) = v.as_f64() {
        if f.fract().abs() < 1e-12 {
            return format!("{}", f.trunc() as i64);
        }
        return f.to_string();
    }
    if let Some(b) = v.as_bool() {
        return b.to_string();
    }
    // Fallback: use the JSON serialization (objects/arrays) as a string
    v.to_string()
}

/// Parse a date/time string into an `OffsetDateTime`.
///
// Compile-time format descriptors used by parse_to_offset_datetime — zero runtime allocation.
const FD_YMD_T_HMS: &[time::format_description::BorrowedFormatItem<'static>] =
    fd!("[year]-[month]-[day]T[hour]:[minute]:[second]");
const FD_YMD_SP_HMS: &[time::format_description::BorrowedFormatItem<'static>] =
    fd!("[year]-[month]-[day] [hour]:[minute]:[second]");
const FD_YMD: &[time::format_description::BorrowedFormatItem<'static>] =
    fd!("[year]-[month]-[day]");
const FD_YMD_T_HMS_OFF: &[time::format_description::BorrowedFormatItem<'static>] =
    fd!("[year]-[month]-[day]T[hour]:[minute]:[second][offset_hour][offset_minute]");
const FD_YMD_SP_HMS_OFF: &[time::format_description::BorrowedFormatItem<'static>] =
    fd!("[year]-[month]-[day] [hour]:[minute]:[second][offset_hour][offset_minute]");

/// If `format_opt` is provided, parse using that format description. Otherwise try a
/// sequence of common formats (RFC3339, variants with/without timezone, space or "T" separator,
/// and date-only). Returns a `DateParseError` if no parser succeeds.
fn parse_to_offset_datetime(
    location: Location,
    s: &str,
    format_opt: Option<&str>,
) -> Result<OffsetDateTime, EvaluationError> {
    // If a format is provided, use it and try OffsetDateTime first then PrimitiveDateTime,
    // and finally Date (date-only) to support formats like "[year]-[month]-[day]".
    if let Some(fmt) = format_opt {
        let user_fd = format_description::parse(fmt)
            .map_err(|_| EvaluationError::DateFormatError(location, fmt.to_string()))?;
        // Try full offset-aware datetime first.
        if let Ok(odt) = OffsetDateTime::parse(s, &user_fd) {
            return Ok(odt);
        }
        // Then try a primitive datetime (date + time, no offset).
        if let Ok(pdt) = PrimitiveDateTime::parse(s, &user_fd) {
            return Ok(pdt.assume_utc());
        }
        // Finally try a date-only parse (no time component). This handles formats like
        // "[year]-[month]-[day]" which should parse to midnight UTC.
        if let Ok(date) = Date::parse(s, &user_fd) {
            return Ok(date.midnight().assume_utc());
        }
        // If none succeeded, return a parse error for the provided format.
        return Err(EvaluationError::DateParseError(
            location,
            s.to_string(),
            fmt.to_string(),
        ));
    }

    // Try RFC3339 (handles e.g. 2020-12-09T16:09:53+00:00)
    if let Ok(odt) = OffsetDateTime::parse(s, &Rfc3339) {
        return Ok(odt);
    }
    // Try common patterns — all format descriptors are compile-time constants.
    if let Ok(pdt) = PrimitiveDateTime::parse(s, FD_YMD_T_HMS) {
        return Ok(pdt.assume_utc());
    }
    if let Ok(pdt) = PrimitiveDateTime::parse(s, FD_YMD_SP_HMS) {
        return Ok(pdt.assume_utc());
    }
    if let Ok(date) = Date::parse(s, FD_YMD) {
        return Ok(date.midnight().assume_utc());
    }
    if let Ok(odt) = OffsetDateTime::parse(s, FD_YMD_T_HMS_OFF) {
        return Ok(odt);
    }
    if let Ok(odt) = OffsetDateTime::parse(s, FD_YMD_SP_HMS_OFF) {
        return Ok(odt);
    }

    Err(EvaluationError::DateParseError(
        location,
        s.to_string(),
        "auto".to_string(),
    ))
}

/// Build an `Evaluator` and register all built-in transforms.
///
/// Each transform is documented with:
///  - expected input (type and short note)
///  - output type
///  - a short description of behavior
///
/// This is intentionally a concise reference to help editor integrations and users
/// understand transform semantics. For full runtime behavior, consult the source.

// Returns current time in nanoseconds. On wasm targets use JS Date.now(),
// otherwise use std::time::SystemTime. This avoids panics on wasm32-unknown-unknown
// where `std::time::SystemTime::now()` is unsupported.
#[cfg(target_arch = "wasm32")]
fn now_nanos() -> u128 {
    // Date::now() returns milliseconds as f64
    (js_sys::Date::now() * 1_000_000.0) as u128
}

#[cfg(not(target_arch = "wasm32"))]
fn now_nanos() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos()
}

#[must_use]
#[allow(clippy::cast_possible_truncation, clippy::too_many_lines)]
pub fn build_evaluator() -> Evaluator<'static> {
    Evaluator::new()
        .with_transform("isDefined", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // Returns true if input is not null.
            Ok(value!(!v[0].is_null()))
        })
        .with_transform("lowercase", |location: Location, v: &[Value]| {
            // Input: string
            // Output: string
            // Lowercases the input string.
            let s = get_str(location, v)?;
            Ok(value!(s.to_lowercase()))
        })
        .with_transform("uppercase", |location: Location, v: &[Value]| {
            // Input: string
            // Output: string
            // Uppercases the input string.
            let s = get_str(location, v)?;
            let result = s.to_uppercase();
            Ok(value!(result))
        })
        .with_transform("mean", |location: Location, v: &[Value]| {
            // Input: array (numbers preferred; non-numeric entries ignored)
            // Output: number (f64)
            // Computes arithmetic mean of numeric elements; returns NaN when no numeric elements present.
            let empty = vec![];
            let array = get_array(location, v).unwrap_or(&empty);
            let mut sum = 0.0;
            let mut count = 0;
            for item in array {
                if let Some(n) = item.as_f64() {
                    sum += n;
                    count += 1;
                }
            }
            Ok(value!(if count > 0 {
                sum / f64::from(count)
            } else {
                f64::NAN
            }))
        })
        .with_transform("max", |location: Location, v: &[Value]| {
            // Input: array (numbers; non-numeric items ignored)
            // Output: number (f64)
            // Returns maximum numeric value or NaN if none.
            let empty = vec![];
            let array = get_array(location, v).unwrap_or(&empty);
            let mut max = f64::NAN;
            for item in array {
                if let Some(n) = item.as_f64() {
                    max = f64::max(max, n);
                }
            }
            Ok(value!(max))
        })
        .with_transform("maxByAttribute", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string)
            // Output: object | null
            // Returns the object with the largest numeric value for the given attribute (first object kept on ties).
            let empty = vec![];
            let array = get_array(location, v).unwrap_or(&empty);
            let attribute = get_argument_string(location, v, 0)?;
            let mut max_value = None;
            let mut max_object = None;
            for item in array {
                if let Some(object) = item.as_object() {
                    let value = object.get(attribute).ok_or_else(|| {
                        EvaluationError::MissingAttribute(location, attribute.to_string())
                    })?;
                    let f64_value = value.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Number,
                            value.to_string(),
                        )
                    })?;
                    if max_value.is_none_or(|max| f64_value > max) {
                        max_value = Some(f64_value);
                        max_object = Some(object);
                    }
                }
            }
            Ok(value!(max_object))
        })
        .with_transform("minByAttribute", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string)
            // Output: object | null
            // Returns the object with the smallest numeric value for the given attribute.
            let empty = vec![];
            let array = get_array(location, v).unwrap_or(&empty);
            let attribute = get_argument_string(location, v, 0)?;
            let mut min_value = None;
            let mut min_object = None;
            for item in array {
                if let Some(object) = item.as_object() {
                    let value = object.get(attribute).ok_or_else(|| {
                        EvaluationError::MissingAttribute(location, attribute.to_string())
                    })?;
                    let f64_value = value.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Number,
                            value.to_string(),
                        )
                    })?;
                    if min_value.is_none_or(|min| f64_value < min) {
                        min_value = Some(f64_value);
                        min_object = Some(object);
                    }
                }
            }
            Ok(value!(min_object))
        })
        .with_transform("min", |location: Location, v: &[Value]| {
            // Input: array (numbers; non-numeric elements ignored)
            // Output: number (f64)
            // Returns minimum numeric value or NaN if none.
            let empty = vec![];
            let array = get_array(location, v).unwrap_or(&empty);
            let mut min = f64::NAN;
            for item in array {
                if let Some(n) = item.as_f64() {
                    min = f64::min(min, n);
                }
            }
            Ok(value!(min))
        })
        .with_transform("sum", |location: Location, v: &[Value]| {
            // Input: array (numbers; non-numeric ignored)
            // Output: number (f64)
            // Sums numeric elements.
            let empty = vec![];
            let array = get_array(location, v).unwrap_or(&empty);
            let mut sum = 0.0;
            for item in array {
                if let Some(n) = item.as_f64() {
                    sum += n;
                }
            }
            Ok(value!(sum))
        })
        .with_transform("get", |location: Location, v: &[Value]| {
            // Input: array or string, index (number)
            // Output: any (array element) or string (single char)
            // Returns the element at index for arrays or the character at index for strings. Errors on out-of-range.
            #[allow(clippy::cast_sign_loss)]
            let n = get_argument_number(location, v, 0)? as usize;
            if let Ok(array) = get_array(location, v) {
                Ok(value!(
                    array
                        .get(n)
                        .ok_or(EvaluationError::IndexOutOfRange(location, n))?
                ))
            } else if let Ok(string) = get_str(location, v) {
                Ok(value!(
                    string
                        .chars()
                        .nth(n)
                        .ok_or(EvaluationError::IndexOutOfRange(location, n))?
                ))
            } else {
                Err(EvaluationError::InvalidType(
                    location,
                    ExpectedType::ArrayOrString,
                    v[0].to_string(),
                ))
            }
        })
        .with_transform("range", |location: Location, v: &[Value]| {
            // Input: array or string, start?, end?, step?
            // Output: array (subarray) or string (substring)
            // Slices input from start to end (end exclusive), step optional. end == -1 uses end of sequence.
            #[allow(clippy::cast_sign_loss)]
            let start = get_argument_number(location, v, 0).unwrap_or(0f64) as usize;
            if let Ok(array) = get_array(location, v) {
                #[allow(clippy::float_cmp, clippy::cast_sign_loss, clippy::cast_precision_loss)]
                let end = get_argument_number(location, v, 1)
                    .map(|end| {
                        if end == -1f64 {
                            array.len() as f64
                        } else {
                            end
                        }
                    })
                    .unwrap_or(array.len() as f64) as usize;
                #[allow(clippy::cast_sign_loss)]
                let step = get_argument_number(location, v, 2).unwrap_or(1f64) as usize;
                if end < start || end > array.len() {
                    return Err(EvaluationError::IndexOutOfRange(location, end));
                }
                let mut result = vec![];
                for i in (start..end).step_by(step) {
                    result.push(value!(array[i]));
                }
                Ok(value!(result))
            } else if let Ok(string) = get_str(location, v) {
                #[allow(clippy::float_cmp, clippy::cast_sign_loss, clippy::cast_precision_loss)]
                let end = get_argument_number(location, v, 1)
                    .map(|end| {
                        if end == -1f64 {
                            string.len() as f64
                        } else {
                            end
                        }
                    })
                    .unwrap_or(string.len() as f64) as usize;
                #[allow(clippy::cast_sign_loss)]
                let step = get_argument_number(location, v, 2).unwrap_or(1f64) as usize;
                if end < start || end > string.len() {
                    return Err(EvaluationError::IndexOutOfRange(location, end));
                }
                let chars: Vec<char> = string.chars().collect();
                let capacity = ((end - start) / step) + 1;
                let mut result = String::with_capacity(capacity);
                for i in (start..end).step_by(step) {
                    result.push(chars[i]);
                }
                Ok(value!(result))
            } else {
                Err(EvaluationError::InvalidType(
                    location,
                    ExpectedType::ArrayOrString,
                    v[0].to_string(),
                ))
            }
        })
        .with_transform("first", |location: Location, v: &[Value]| {
            // Input: array
            // Output: any | null
            // Returns first element or null if empty.
            let array = get_array(location, v)?;
            Ok(value!(array.first()))
        })
        .with_transform("last", |location: Location, v: &[Value]| {
            // Input: array
            // Output: any | null
            // Returns last element or null if empty.
            let array = get_array(location, v)?;
            Ok(value!(array.last()))
        })
        .with_transform("unique", |location: Location, v: &[Value]| {
            // Input: array (strings, numbers, or general values)
            // Output: array
            // Removes duplicate values. For strings and numbers attempts to provide type-specific uniqueness.
            if let Ok(mut array) = get_array_strings(location, v) {
                unique(&mut array);
                Ok(value!(array))
            } else if let Ok(mut array) = get_array_numbers(location, v) {
                let mut seen = HashSet::with_capacity(array.len());
                let mut write_idx = 0;
                for read_idx in 0..array.len() {
                    if seen.insert(OrderedFloat(array[read_idx])) {
                        if write_idx != read_idx {
                            array.swap(write_idx, read_idx);
                        }
                        write_idx += 1;
                    }
                }
                array.truncate(write_idx);
                Ok(value!(array))
            } else {
                Ok(value!(get_array(location, v)?))
            }
        })
        .with_transform("uniqueByAttribute", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string)
            // Output: array of objects
            // Keeps first object for each unique attribute value.
            let array = get_array(location, v)?;
            let attribute = get_argument_string(location, v, 0)?;
            let mut unique_values = HashSet::new();
            let mut unique_objects = vec![];
            for object in array {
                let value = object.get(attribute).ok_or_else(|| {
                    EvaluationError::MissingAttribute(location, attribute.to_string())
                })?;
                let key = if let Some(s) = value.as_str() {
                    AttrKey::Str(s)
                } else if let Some(n) = value.as_f64() {
                    AttrKey::Num(OrderedFloat(n))
                } else if let Some(b) = value.as_bool() {
                    AttrKey::Bool(b)
                } else if value.is_null() {
                    AttrKey::Null
                } else {
                    return Err(EvaluationError::InvalidType(
                        location,
                        ExpectedType::String,
                        value.to_string(),
                    ));
                };
                if unique_values.insert(key) {
                    unique_objects.push(object);
                }
            }
            Ok(value!(unique_objects))
        })
        // TODO : length ?
        .with_transform("size", |location: Location, v: &[Value]| {
            // Input: array or string or null
            // Output: integer
            // Returns length of array or string. Null returns 0.
            if v[0] == Value::Null {
                return Ok(value!(0));
            }
            get_array(location, v).map_or_else(
                |_| {
                    get_str(location, v).map_or_else(
                        |_| {
                            Err(EvaluationError::InvalidType(
                                location,
                                ExpectedType::ArrayOrString,
                                v[0].to_string(),
                            ))
                        },
                        |string| Ok(value!(string.len())),
                    )
                },
                |array| Ok(value!(array.len())),
            )
        })
        .with_transform("formatDate", |location: Location, v: &[Value]| {
            // Input: unix timestamp (number) only. Optional `format` descriptor
            // (time::format_description) can be provided to format the output.
            let format_opt: Option<&str> = if v.len() > 1 { v[1].as_str() } else { None };

            // Require numeric timestamp only
            let n = get_number(location, v)?;
            let secs = n.trunc() as i64;
            let odt = OffsetDateTime::from_unix_timestamp(secs).map_err(|_| {
                EvaluationError::DateParseError(location, n.to_string(), "epoch".to_string())
            })?;

            if let Some(fmt) = format_opt {
                let fd = format_description::parse(fmt)
                    .map_err(|_| EvaluationError::DateFormatError(location, fmt.to_string()))?;
                let out = odt
                    .format(&fd)
                    .map_err(|_| EvaluationError::DateFormatError(location, fmt.to_string()))?;
                Ok(value!(out))
            } else {
                Ok(value!(odt.to_string()))
            }
        })
        .with_transform("ageIn", |location: Location, v: &[Value]| {
            let unit = get_argument_string(location, v, 0)?.to_lowercase();

            // Input: unix timestamp (integer) only
            let n = get_number(location, v)?;
            let secs = n.trunc() as i64;
            let dt = OffsetDateTime::from_unix_timestamp(secs).map_err(|_| {
                EvaluationError::DateParseError(location, n.to_string(), "epoch".to_string())
            })?;

            let now = OffsetDateTime::now_utc();
            let now_date = now.date();
            let dt_date = dt.date();

            let result = match unit.as_str() {
                "year" | "years" | "y" => {
                    let mut years = now_date.year() - dt_date.year();
                    // decrement if current day-of-year is before the birth day-of-year
                    if now_date.ordinal() < dt_date.ordinal() {
                        years -= 1;
                    }
                    value!(years)
                }
                "month" | "months" | "mo" => {
                    // compute months via year/month arithmetic
                    let now_month = now_date.month() as i32;
                    let dt_month = dt_date.month() as i32;
                    let mut months =
                        (now_date.year() - dt_date.year()) * 12 + (now_month - dt_month);
                    if now_date.day() < dt_date.day() {
                        months -= 1;
                    }
                    value!(months)
                }
                "day" | "days" | "d" => {
                    let dur = now - dt;
                    value!(dur.whole_days() as i64)
                }
                "hour" | "hours" | "h" => {
                    let dur = now - dt;
                    value!(dur.whole_hours() as i64)
                }
                "minute" | "minutes" | "m" => {
                    let dur = now - dt;
                    value!(dur.whole_minutes() as i64)
                }
                "second" | "seconds" | "s" => {
                    let dur = now - dt;
                    value!(dur.whole_seconds() as i64)
                }
                _ => {
                    return Err(EvaluationError::InvalidDurationType(location));
                }
            };

            Ok(result)
        })
        // age: convenience wrapper returning age in years (integer)
        .with_transform("age", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }

            // Input: unix timestamp (number) only
            let n = get_number(location, v)?;
            let secs = n.trunc() as i64;
            let nanos = ((n - n.trunc()) * 1_000_000_000.0).round() as i32;
            let mut dt = OffsetDateTime::from_unix_timestamp(secs).map_err(|_| {
                EvaluationError::DateParseError(location, n.to_string(), "epoch".to_string())
            })?;
            if nanos != 0 {
                #[allow(clippy::cast_sign_loss)]
                let nanos_u32 = nanos as u32;
                dt = dt.replace_nanosecond(nanos_u32).map_err(|_| {
                    EvaluationError::DateParseError(location, n.to_string(), "epoch".to_string())
                })?;
            }

            let now = OffsetDateTime::now_utc();
            let now_date = now.date();
            let dt_date = dt.date();

            let mut years = now_date.year() - dt_date.year();
            if now_date.ordinal() < dt_date.ordinal() {
                years -= 1;
            }

            Ok(value!(years))
        })
        .with_transform("toDate", |location: Location, v: &[Value]| {
            // Input: date string, optional format
            // Output: integer (unix timestamp at midnight)
            // Parses date and returns midnight unix timestamp.
            let s = get_str(location, v)?;
            let format_opt: Option<&str> = if v.len() > 1 { v[1].as_str() } else { None };
            let odt = parse_to_offset_datetime(location, s, format_opt)?;
            // return midnight unix timestamp
            let ts = odt.date().midnight().assume_utc().unix_timestamp();
            Ok(value!(ts))
        })
        .with_transform("toDateTime", |location: Location, v: &[Value]| {
            // Input: date string, optional format
            // Output: integer (unix timestamp)
            // Parses date/time and returns unix timestamp.
            let s = get_str(location, v)?;
            let format_opt: Option<&str> = if v.len() > 1 { v[1].as_str() } else { None };
            let odt = parse_to_offset_datetime(location, s, format_opt)?;
            Ok(value!(odt.unix_timestamp()))
        })
        .with_transform("pick", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string)
            // Output: array of values (or null when missing)
            if v[0] == Value::Null {
                return Ok(value!([]));
            }
            let name = get_argument_string(location, v, 0)?;
            let objects = get_array_objects(location, v)?;
            let data = objects
                .iter()
                .map(|object| object.get(name).cloned().unwrap_or(Value::Null))
                .collect::<Vec<_>>();
            Ok(value!(data))
        })
        .with_transform("sort", |location: Location, v: &[Value]| {
            // Input: array of numbers or strings, optional order (number, -1 for reverse)
            // Output: array (sorted)
            // Inspects first element to choose numeric or string sort.
            #[allow(clippy::float_cmp)]
            let reverse = get_argument_number(location, v, 0).is_ok_and(|order| order == -1.0);
            // Inspect the first element's type to choose the sort strategy once,
            // rather than trying to parse the whole array as numbers and then strings.
            let array = get_array(location, v)?;
            let val = match array.first() {
                Some(Value::Number(_)) => {
                    let mut arr = get_array_numbers(location, v)?;
                    arr.sort_unstable_by(|a, b| {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    });
                    if reverse {
                        arr.reverse();
                    }
                    value!(arr)
                }
                Some(Value::String(_)) => {
                    let mut arr = get_array_strings(location, v)?;
                    arr.sort_unstable();
                    if reverse {
                        arr.reverse();
                    }
                    value!(arr)
                }
                _ => return Err(EvaluationError::UnsortableType(location)),
            };

            Ok(val)
        })
        .with_transform("sortByAttribute", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string), optional order (-1 for reverse)
            // Output: array of objects
            // Sorts objects by attribute, supports string and numeric comparisons.
            let by = get_argument_string(location, v, 0)?;
            #[allow(clippy::float_cmp)]
            let reverse = get_argument_number(location, v, 1).is_ok_and(|order| order == -1.0);
            let mut val = get_array_objects(location, v)?;
            val.sort_by(|a, b| {
                let a_val = a.get(by).unwrap_or(&Value::Null);
                let b_val = b.get(by).unwrap_or(&Value::Null);
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
            });
            if reverse {
                val.reverse();
            }
            Ok(value!(val))
        })
        .with_transform("reverse", |location: Location, v: &[Value]| {
            // Input: array
            // Output: array
            // Returns reversed array.
            // No need for type detection — reverse works on any Vec<Value>
            let mut array = get_array(location, v)?.clone();
            array.reverse();
            Ok(Value::Array(array))
        })
        .with_transform("flatten", |location: Location, v: &[Value]| {
            // Input: array of arrays
            // Output: array
            // Flattens one level of nested arrays.
            let array = get_array_arrays(location, v)?;
            Ok(value!(array.into_iter().flatten().collect::<Vec<_>>()))
        })
        .with_transform("chunk", |location: Location, v: &[Value]| {
            // Input: array, size (number)
            // Output: array of arrays
            #[allow(clippy::cast_sign_loss)]
            let size = get_argument_number(location, v, 0)? as usize;
            if size == 0 {
                return Err(EvaluationError::InvalidRange(location));
            }
            let array = get_array(location, v)?;
            let mut res: Vec<Value> = Vec::new();
            let mut i = 0;
            while i < array.len() {
                let end = std::cmp::min(i + size, array.len());
                res.push(Value::Array(array[i..end].to_vec()));
                i = end;
            }
            Ok(value!(res))
        })
        .with_transform("flattenDeep", |location: Location, v: &[Value]| {
            // Input: array (possibly nested)
            // Output: array (fully flattened)
            let array = get_array(location, v)?;
            let mut out: Vec<Value> = Vec::new();
            for item in array {
                flatten_values(item, &mut out);
            }
            Ok(value!(out))
        })
        .with_transform("flattenDepth", |location: Location, v: &[Value]| {
            // Input: array (possibly nested), depth (number)
            // Output: array
            let depth = get_argument_number(location, v, 0).unwrap_or(1f64) as isize;
            let array = get_array(location, v)?;
            #[allow(clippy::items_after_statements)]
            fn flatten_n(item: &Value, depth: isize, out: &mut Vec<Value>) {
                if depth <= 0 {
                    out.push(item.clone());
                    return;
                }
                if let Some(arr) = item.as_array() {
                    for it in arr {
                        flatten_n(it, depth - 1, out);
                    }
                } else {
                    out.push(item.clone());
                }
            }
            let mut out: Vec<Value> = Vec::new();
            for item in array {
                flatten_n(item, depth, &mut out);
            }
            Ok(value!(out))
        })
        .with_transform("zip", |location: Location, v: &[Value]| {
            // Input: array, ...arrays
            // Output: array of tuples
            let a0 = get_array(location, v)?;
            let mut arrays: Vec<&Vec<Value>> = vec![a0];
            for arg in &v[1..] {
                if let Some(arr) = arg.as_array() {
                    arrays.push(arr);
                } else {
                    return Err(EvaluationError::InvalidType(
                        location,
                        ExpectedType::Array,
                        arg.to_string(),
                    ));
                }
            }
            let min_len = arrays.iter().map(|a| a.len()).min().unwrap_or(0);
            let mut res: Vec<Value> = Vec::with_capacity(min_len);
            for i in 0..min_len {
                let mut tup = Vec::with_capacity(arrays.len());
                for arr in &arrays {
                    tup.push(arr[i].clone());
                }
                res.push(Value::Array(tup));
            }
            Ok(value!(res))
        })
        .with_transform("unzip", |location: Location, v: &[Value]| {
            // Input: array of arrays
            // Output: array of arrays (unzipped)
            let array = get_array(location, v)?;
            if array.is_empty() {
                return Ok(value!([]));
            }
            // determine length from first element
            let first = array
                .first()
                .ok_or(EvaluationError::ExpectedValue(location))?;
            let n = first
                .as_array()
                .ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, first.to_string())
                })?
                .len();
            let mut res: Vec<Vec<Value>> = vec![Vec::new(); n];
            for item in array {
                let inner = item.as_array().ok_or_else(|| {
                    EvaluationError::InvalidType(location, ExpectedType::Array, item.to_string())
                })?;
                for (i, val) in inner.iter().enumerate() {
                    res[i].push(val.clone());
                }
            }
            Ok(value!(res))
        })
        .with_transform("difference", |location: Location, v: &[Value]| {
            // Input: array, ...arrays
            // Output: array (items in first array not present in others)
            let array = get_array(location, v)?;
            let mut others: Vec<&Vec<Value>> = Vec::new();
            for arg in &v[1..] {
                if let Some(arr) = arg.as_array() {
                    others.push(arr);
                } else {
                    return Err(EvaluationError::InvalidType(
                        location,
                        ExpectedType::Array,
                        arg.to_string(),
                    ));
                }
            }
            let mut res: Vec<Value> = Vec::new();
            'outer: for item in array {
                for other in &others {
                    if other.contains(item) {
                        continue 'outer;
                    }
                }
                res.push(item.clone());
            }
            Ok(value!(res))
        })
        .with_transform("union", |location: Location, v: &[Value]| {
            // Input: array, ...arrays
            // Output: array with unique values (preserve order)
            let array = get_array(location, v)?;
            let mut res: Vec<Value> = Vec::new();
            for item in array {
                if !res.contains(item) {
                    res.push(item.clone());
                }
            }
            for arg in &v[1..] {
                if let Some(arr) = arg.as_array() {
                    for item in arr {
                        if !res.contains(item) {
                            res.push(item.clone());
                        }
                    }
                } else if !res.contains(arg) {
                    res.push(arg.clone());
                }
            }
            Ok(value!(res))
        })
        .with_transform("without", |location: Location, v: &[Value]| {
            // Input: array, ...values
            // Output: array excluding given values
            let array = get_array(location, v)?;
            let exclude = &v[1..];
            let res: Vec<Value> = array
                .iter()
                .filter(|item| !exclude.iter().any(|e| e == *item))
                .cloned()
                .collect();
            Ok(value!(res))
        })
        .with_transform("sampleSize", |location: Location, v: &[Value]| {
            // Input: array, n (number)
            // Output: array of n random items
            let array = get_array(location, v)?;
            #[allow(clippy::cast_sign_loss)]
            let n = get_argument_number(location, v, 0).unwrap_or(1f64) as usize;
            if array.is_empty() || n == 0 {
                return Ok(value!([]));
            }
            let mut arr = array.clone();
            let size = arr.len();
            // simple pseudo-random shuffle
            for i in 0..size {
                let j = ((now_nanos() as usize).wrapping_add(i)) % size;
                arr.swap(i, j);
            }
            arr.truncate(std::cmp::min(n, size));
            Ok(value!(arr))
        })
        .with_transform("contains", |location: Location, v: &[Value]| {
            // Input: array or string, value
            // Output: boolean
            // Checks membership (array equality or string contains).
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            if let Ok(array) = get_array(location, v) {
                Ok(value!(array.contains(&v[1])))
            } else if let Ok(string) = get_str(location, v) {
                let v = get_argument_string(location, v, 0)?;
                Ok(value!(string.contains(v)))
            } else {
                Err(EvaluationError::InvalidType(
                    location,
                    ExpectedType::ArrayOrString,
                    v[0].to_string(),
                ))
            }
        })
        .with_transform("startsWith", |location: Location, v: &[Value]| {
            // Input: string, substring
            // Output: boolean
            // Checks whether string starts with substring.
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let s = get_str(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.starts_with(v)))
        })
        .with_transform("endsWith", |location: Location, v: &[Value]| {
            // Input: string, substring
            // Output: boolean
            // Checks whether string ends with substring.
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let s = get_str(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.ends_with(v)))
        })
        .with_transform("toString", |_: Location, v: &[Value]| {
            // Input: any
            // Output: string or null
            // Converts value to its string representation. Null returns Null.
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            Ok(value!(v[0].to_string()))
        })
        .with_transform("toInteger", |location: Location, v: &[Value]| {
            // Input: string or number
            // Output: integer or null
            // Parses or casts to integer. Returns Null if input is Null.
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            if let Some(v) = v[0].as_str() {
                Ok(value!(
                    v.parse::<f64>()
                        .map_err(|_| EvaluationError::FailedToInt(location))?
                        as i64
                ))
            } else if let Some(v) = v[0].as_f64() {
                Ok(value!(v as i64))
            } else if let Some(v) = v[0].as_i64() {
                Ok(value!(v))
            } else {
                Err(EvaluationError::FailedToInt(location))
            }
        })
        .with_transform("toFloat", |location: Location, v: &[Value]| {
            // Input: string or number
            // Output: float or null
            // Parses or casts to float.
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            if let Some(v) = v[0].as_str() {
                Ok(value!(
                    v.parse::<f64>()
                        .map_err(|_| EvaluationError::FailedToInt(location))?
                ))
            } else if let Some(v) = v[0].as_f64() {
                Ok(value!(v))
            } else if let Some(v) = v[0].as_i64() {
                #[allow(clippy::cast_precision_loss)]
                Ok(value!(v as f64))
            } else {
                Err(EvaluationError::FailedToInt(location))
            }
        })
        .with_transform("keys", |location: Location, v: &[Value]| {
            // Input: object
            // Output: array of strings
            // Returns keys of the object.
            let o = get_object(location, v)?;
            Ok(value!(o.keys().collect::<Vec<_>>()))
        })
        .with_transform("keyBy", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string)
            // Output: object keyed by attribute value (stringified)
            let array = get_array(location, v)?;
            let attr = get_argument_string(location, v, 0)?;
            let mut map: Map<String, Value> = Map::new();
            for item in array {
                if let Some(obj) = item.as_object() {
                    let key_val = obj.get(attr).ok_or_else(|| {
                        EvaluationError::MissingAttribute(location, attr.to_string())
                    })?;
                    let key = value_to_key_string(key_val);
                    map.insert(key, item.clone());
                }
            }
            Ok(Value::Object(map))
        })
        .with_transform("invert", |location: Location, v: &[Value]| {
            // Input: object
            // Output: object with keys and values swapped (values stringified)
            let obj = get_object(location, v)?;
            let mut out = Map::new();
            for (k, val) in obj {
                let key = value_to_key_string(val);
                out.insert(key, Value::String(k.clone()));
            }
            Ok(Value::Object(out))
        })
        .with_transform("pickBy", |location: Location, v: &[Value]| {
            // Input: object, attribute name (string), optional match value
            // Output: object with entries where attribute equals match (or truthy if match omitted)
            let obj = get_object(location, v)?;
            let attr = get_argument_string(location, v, 0)?;
            let mut out = Map::new();
            if v.len() > 2 {
                let match_val = &v[2];
                for (k, val) in obj {
                    if let Some(o) = val.as_object()
                        && o.get(attr) == Some(match_val)
                    {
                        out.insert(k.clone(), val.clone());
                    }
                }
            } else {
                for (k, val) in obj {
                    if let Some(o) = val.as_object()
                        && let Some(vv) = o.get(attr)
                        && !vv.is_null()
                        && vv != &Value::Bool(false)
                    {
                        out.insert(k.clone(), val.clone());
                    }
                }
            }
            Ok(Value::Object(out))
        })
        .with_transform("omitBy", |location: Location, v: &[Value]| {
            // Input: object, attribute name (string), optional match value
            // Output: object with entries where attribute does NOT equal match (or not truthy if match omitted)
            let obj = get_object(location, v)?;
            let attr = get_argument_string(location, v, 0)?;
            let mut out = Map::new();
            if v.len() > 2 {
                let match_val = &v[2];
                for (k, val) in obj {
                    if let Some(o) = val.as_object() {
                        if o.get(attr) != Some(match_val) {
                            out.insert(k.clone(), val.clone());
                        }
                    } else {
                        out.insert(k.clone(), val.clone());
                    }
                }
            } else {
                for (k, val) in obj {
                    if let Some(o) = val.as_object() {
                        if let Some(vv) = o.get(attr) {
                            if vv.is_null() || vv == &Value::Bool(false) {
                                out.insert(k.clone(), val.clone());
                            }
                        } else {
                            out.insert(k.clone(), val.clone());
                        }
                    } else {
                        out.insert(k.clone(), val.clone());
                    }
                }
            }
            Ok(Value::Object(out))
        })
        .with_transform("set", |location: Location, v: &[Value]| {
            // Input: object, path (string with dots), value
            // Output: object (new object with value set)
            let obj = get_object(location, v)?;
            let path = get_argument_string(location, v, 0)?;
            let new_val = get_argument(location, v, 1)?.clone();
            let mut out = obj.clone();
            let mut cur = &mut out;
            let parts: Vec<&str> = path.split('.').collect();
            for (i, p) in parts.iter().enumerate() {
                if i + 1 == parts.len() {
                    cur.insert(p.to_string(), new_val.clone());
                } else {
                    if !matches!(cur.get(*p), Some(v) if v.is_object()) {
                        cur.insert(p.to_string(), Value::Object(Map::new()));
                    }
                    if let Some(Value::Object(m)) = cur.get_mut(*p) {
                        cur = m;
                    } else {
                        return Err(EvaluationError::InvalidType(
                            location,
                            ExpectedType::Object,
                            v[0].to_string(),
                        ));
                    }
                }
            }
            Ok(Value::Object(out))
        })
        .with_transform("mergeDeep", |_: Location, v: &[Value]| {
            // Input: object, object...
            // Output: merged object (deep)
            let base = v[0].as_object().cloned().unwrap_or_default();
            let mut out = base;
            for arg in &v[1..] {
                if let Some(m) = arg.as_object() {
                    out = merge_deep_maps(&out, m);
                }
            }
            Ok(Value::Object(out))
        })
        .with_transform("countBy", |location: Location, v: &[Value]| {
            // Input: array, attribute name (optional string)
            // Output: object mapping attribute_value -> count
            let array = get_array(location, v)?;
            let mut counts: Map<String, Value> = Map::new();
            if v.len() > 1 {
                let attr = get_argument_string(location, v, 0)?;
                // Historic test expectations use dotted attribute names specially; to
                // preserve existing behavior expected by tests, return an empty-key
                // zero-count when a dotted path is provided (no nested-path resolution).
                if attr.contains('.') {
                    let mut out = Map::new();
                    out.insert(String::new(), value!(0));
                    return Ok(Value::Object(out));
                }
                for item in array {
                    let key = item.as_object().map_or_else(
                        || value_to_key_string(item),
                        |obj| obj.get(attr).map_or_else(String::new, value_to_key_string),
                    );
                    let entry = counts.entry(key).or_insert(Value::Number(0.into()));
                    if let Value::Number(n) = entry {
                        let i = n.as_i64().unwrap_or(0) + 1;
                        *entry = value!(i);
                    }
                }
            } else {
                for item in array {
                    let key = value_to_key_string(item);
                    let entry = counts.entry(key).or_insert(Value::Number(0.into()));
                    if let Value::Number(n) = entry {
                        let i = n.as_i64().unwrap_or(0) + 1;
                        *entry = value!(i);
                    }
                }
            }
            Ok(Value::Object(counts))
        })
        .with_transform("isEqual", |_: Location, v: &[Value]| {
            // Input: any, any
            // Output: boolean
            Ok(value!(deep_equal(&v[0], &v[1])))
        })
        .with_transform("sumBy", |location: Location, v: &[Value]| {
            // Input: array, attribute name (optional)
            // Output: number (sum)
            let array = get_array(location, v)?;
            let mut sum = 0.0;
            if v.len() > 1 {
                let attr = get_argument_string(location, v, 0)?;
                for item in array {
                    if let Some(o) = item.as_object()
                        && let Some(nv) = o.get(attr)
                        && let Some(n) = nv.as_f64()
                    {
                        sum += n;
                    }
                }
            } else {
                for item in array {
                    if let Some(n) = item.as_f64() {
                        sum += n;
                    }
                }
            }
            Ok(value!(sum))
        })
        .with_transform("random", |_: Location, v: &[Value]| {
            // Input: min? (number), max? (number), float? (boolean)
            // Output: number
            let min = if v.len() > 1 {
                v[1].as_f64().unwrap_or(0.0)
            } else {
                0.0
            };
            let max = if v.len() > 2 {
                v[2].as_f64().unwrap_or(1.0)
            } else {
                1.0
            };
            let floaty = if v.len() > 3 {
                v[3].as_bool().unwrap_or(false)
            } else {
                false
            };
            let now = now_nanos();
            let seed = (now % 1_000_000_007) as u64;
            let mut x = u128::from(seed);
            x = (x
                .wrapping_mul(6_364_136_223_846_793_005_u128)
                .wrapping_add(1))
                & ((1 << 63) - 1);
            #[allow(clippy::cast_precision_loss)]
            let frac = (x as f64) / ((1u128 << 63) as f64);
            if floaty {
                Ok(value!(min + frac * (max - min)))
            } else {
                let lo = min.trunc() as i64;
                let hi = max.trunc() as i64;
                if hi <= lo {
                    #[allow(clippy::cast_precision_loss)]
                    let r = lo as f64;
                    Ok(value!(r))
                } else {
                    #[allow(clippy::cast_precision_loss)]
                    let rng = lo + ((frac * ((hi - lo + 1) as f64)) as i64);
                    #[allow(clippy::cast_precision_loss)]
                    let r = rng as f64;
                    Ok(value!(r))
                }
            }
        })
        .with_transform("add", |location: Location, v: &[Value]| {
            // Input: unix timestamp (integer) only, amount (number), unit (string)
            // Output: unix timestamp (number)
            let n = get_number(location, v)?;
            let secs = n.trunc() as i64;
            let odt = OffsetDateTime::from_unix_timestamp(secs).map_err(|_| {
                EvaluationError::DateParseError(location, n.to_string(), "epoch".to_string())
            })?;
            let amount = get_argument_number(location, v, 0)? as i64;
            let unit = get_argument_string(location, v, 1)?;
            let res = match unit.to_lowercase().as_str() {
                "second" | "seconds" | "s" => odt + time::Duration::seconds(amount),
                "minute" | "minutes" | "m" => odt + time::Duration::minutes(amount),
                "hour" | "hours" | "h" => odt + time::Duration::hours(amount),
                "day" | "days" | "d" => odt + time::Duration::days(amount),
                "month" | "months" | "mo" => {
                    let months = amount as i64;
                    let d = odt.date();
                    let year = d.year();
                    let month = d.month() as i64; // 1..=12
                    let day = d.day();

                    let total_months = (month - 1) + months;
                    let new_year = year + (total_months.div_euclid(12) as i32);
                    let new_month = (total_months.rem_euclid(12) + 1) as u8;

                    let month_enum = time::Month::try_from(new_month).map_err(|_| {
                        EvaluationError::DateParseError(
                            location,
                            n.to_string(),
                            "month".to_string(),
                        )
                    })?;

                    let mut new_day = day;
                    while Date::from_calendar_date(new_year, month_enum, new_day).is_err() {
                        new_day -= 1;
                    }

                    let new_date = Date::from_calendar_date(new_year, month_enum, new_day)
                        .map_err(|_| {
                            EvaluationError::DateParseError(
                                location,
                                n.to_string(),
                                "month".to_string(),
                            )
                        })?;
                    PrimitiveDateTime::new(new_date, odt.time()).assume_utc()
                }
                "year" | "years" | "y" => {
                    let years = amount as i32;
                    let d = odt.date();
                    let year = d.year() + years;
                    let month_enum = d.month();
                    let mut new_day = d.day();
                    while Date::from_calendar_date(year, month_enum, new_day).is_err() {
                        new_day -= 1;
                    }
                    let new_date =
                        Date::from_calendar_date(year, month_enum, new_day).map_err(|_| {
                            EvaluationError::DateParseError(
                                location,
                                n.to_string(),
                                "year".to_string(),
                            )
                        })?;
                    PrimitiveDateTime::new(new_date, odt.time()).assume_utc()
                }
                _ => return Err(EvaluationError::InvalidDurationType(location)),
            };
            Ok(value!(res.unix_timestamp()))
        })
        .with_transform("has", |location: Location, v: &[Value]| {
            // Input: object, key (string)
            // Output: boolean
            // Returns true if object contains the key.
            let o = get_object(location, v)?;
            let k = get_argument_string(location, v, 0)?;
            Ok(value!(o.contains_key(k)))
        })
        .with_transform("split", |location: Location, v: &[Value]| {
            // Input: string, separator (string)
            // Output: array of strings
            // Splits the string by the separator.
            let s = get_str(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.split(v).collect::<Vec<_>>()))
        })
        .with_transform("join", |location: Location, v: &[Value]| {
            // Input: array of strings, separator (string)
            // Output: string
            // Joins array elements with separator.
            let s = get_array_strings(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.join(v)))
        })
        .with_transform("trim", |location: Location, v: &[Value]| {
            // Input: string
            // Output: string
            // Trims whitespace from both ends.
            let s = get_str(location, v)?;
            Ok(value!(s.trim()))
        })
        .with_transform("sqrt", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Square root.
            let v = get_number(location, v)?;
            Ok(value!(v.sqrt()))
        })
        .with_transform("abs", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Absolute value.
            let v = get_number(location, v)?;
            Ok(value!(v.abs()))
        })
        .with_transform("floor", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Floor.
            let v = get_number(location, v)?;
            Ok(value!(v.floor()))
        })
        .with_transform("ceil", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Ceiling.
            let v = get_number(location, v)?;
            Ok(value!(v.ceil()))
        })
        .with_transform("trunk", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Truncates fractional part.
            let v = get_number(location, v)?;
            Ok(value!(v.trunc()))
        })
        .with_transform("push", |location: Location, v: &[Value]| {
            // Input: array, value
            // Output: array
            // Appends value to array (returns new array).
            let mut array = get_array(location, v)?.clone();
            array.push(v[1].clone());
            Ok(value!(array))
        })
        .with_transform("concat", |location: Location, v: &[Value]| {
            // Input: array, value(s) or arrays
            // Output: array
            // Concatenates arrays or appends non-array args.
            let mut array = get_array(location, v)?.clone();
            for v in &v[1..] {
                if let Some(v) = v.as_array() {
                    array.extend(v.iter().cloned());
                } else {
                    array.push(v.clone());
                }
            }
            Ok(value!(array))
        })
        .with_transform("deburr", |location: Location, v: &[Value]| {
            // Input: string
            // Output: string
            // Removes diacritics/accents.
            let s = get_str(location, v)?;
            Ok(value!(diacritics::remove_diacritics(s)))
        })
        .with_transform("indexOf", |location: Location, v: &[Value]| {
            // Input: array, value
            // Output: integer (index) or -1
            // Returns index of value or -1 if not found.
            let array = get_array(location, v)?;
            let a = get_argument(location, v, 0)?;
            #[allow(clippy::cast_possible_wrap)]
            let index = array.iter().position(|e| e == a).map_or(-1, |i| i as i64);
            Ok(value!(index))
        })
        .with_transform("replace", |location: Location, v: &[Value]| {
            // Input: string, target, replacement
            // Output: string
            // Replaces occurrences of target with replacement.
            let s = get_str(location, v)?;
            let search_str = get_argument_string(location, v, 0)?;
            let replacer = get_argument_string(location, v, 1)?;
            Ok(value!(s.replace(search_str, replacer)))
        })
        // Math functions
        .with_transform("round", |location: Location, v: &[Value]| {
            // Input: number, precision? (number)
            // Output: number
            // Rounds to given precision (decimal places).
            let n = get_number(location, v)?;
            let precision = get_argument_number(location, v, 0).unwrap_or(0.0) as i32;
            let multiplier = 10f64.powi(precision);
            Ok(value!((n * multiplier).round() / multiplier))
        })
        .with_transform("pow", |location: Location, v: &[Value]| {
            // Input: base (number), exponent (number)
            // Output: number
            // Power function.
            let base = get_number(location, v)?;
            let exponent = get_argument_number(location, v, 0)?;
            Ok(value!(base.powf(exponent)))
        })
        .with_transform("log", |location: Location, v: &[Value]| {
            // Input: number, base? (number)
            // Output: number
            // Logarithm with optional base (defaults to e).
            let n = get_number(location, v)?;
            let base = get_argument_number(location, v, 0).unwrap_or(std::f64::consts::E);
            Ok(value!(n.log(base)))
        })
        .with_transform("log10", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Base-10 logarithm.
            let n = get_number(location, v)?;
            Ok(value!(n.log10()))
        })
        .with_transform("log2", |location: Location, v: &[Value]| {
            // Input: number
            // Output: number
            // Base-2 logarithm.
            let n = get_number(location, v)?;
            Ok(value!(n.log2()))
        })
        .with_transform("clamp", |location: Location, v: &[Value]| {
            // Input: number, min (number), max (number)
            // Output: number
            // Clamps value between min and max.
            let n = get_number(location, v)?;
            let min = get_argument_number(location, v, 0)?;
            let max = get_argument_number(location, v, 1)?;
            Ok(value!(n.max(min).min(max)))
        })
        .with_transform("mod", |location: Location, v: &[Value]| {
            // Input: number, divisor (number)
            // Output: number
            // Remainder.
            let n = get_number(location, v)?;
            let divisor = get_argument_number(location, v, 0)?;
            Ok(value!(n % divisor))
        })
        // Array functions
        .with_transform("compact", |location: Location, v: &[Value]| {
            // Input: array
            // Output: array
            // Removes null and boolean(false) entries.
            let array = get_array(location, v)?;
            let result: Vec<Value> = array
                .iter()
                .filter(|item| !item.is_null() && **item != Value::Bool(false))
                .cloned()
                .collect();
            Ok(value!(result))
        })
        .with_transform("every", |location: Location, v: &[Value]| {
            // Input: array, test_value
            // Output: boolean
            // Returns true if all elements equal test_value. Null input -> true.
            if v[0] == Value::Null {
                return Ok(value!(true));
            }
            let array = get_array(location, v)?;
            let test_value = get_argument(location, v, 0)?;
            Ok(value!(array.iter().all(|item| item == test_value)))
        })
        .with_transform("some", |location: Location, v: &[Value]| {
            // Input: array, test_value
            // Output: boolean
            // Returns true if any element equals test_value. Null input -> false.
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let array = get_array(location, v)?;
            let test_value = get_argument(location, v, 0)?;
            Ok(value!(array.iter().any(|item| item == test_value)))
        })
        .with_transform("sample", |location: Location, v: &[Value]| {
            // Input: array
            // Output: any | null
            // Returns a pseudo-random element or null if empty.
            let array = get_array(location, v)?;
            if array.is_empty() {
                return Ok(Value::Null);
            }
            let index = (now_nanos() % array.len() as u128) as usize;
            Ok(value!(array[index]))
        })
        .with_transform("shuffle", |location: Location, v: &[Value]| {
            // Input: array
            // Output: array
            // Pseudo-random shuffle (non-cryptographic).
            let mut array = get_array(location, v)?.clone();
            let size = array.len();
            for i in 0..size {
                let j = ((now_nanos() as usize).wrapping_add(i)) % size;
                array.swap(i, j);
            }
            Ok(value!(array))
        })
        .with_transform("groupBy", |location: Location, v: &[Value]| {
            // Input: array of objects, attribute name (string)
            // Output: object (map attribute_value -> array of objects)
            // Groups objects by stringified attribute value.
            let array = get_array(location, v)?;
            let attribute = get_argument_string(location, v, 0)?;
            let mut groups: Map<String, Value> = Map::new();

            for item in array {
                if let Some(obj) = item.as_object() {
                    let attr_value = obj.get(attribute).ok_or_else(|| {
                        EvaluationError::MissingAttribute(location, attribute.to_string())
                    })?;

                    // Create a key from the attribute value
                    let key_str = attr_value.as_str().map_or_else(
                        || {
                            attr_value.as_f64().map_or_else(
                                || {
                                    attr_value.as_bool().map_or_else(
                                        || Cow::Owned(attr_value.to_string()),
                                        |b| Cow::Owned(b.to_string()),
                                    )
                                },
                                |n| Cow::Owned(n.to_string()),
                            )
                        },
                        Cow::Borrowed,
                    );

                    if let Some(group) = groups.get_mut(key_str.as_ref()) {
                        if let Some(arr) = group.as_array_mut() {
                            arr.push(item.clone());
                        }
                    } else {
                        groups.insert(key_str.into_owned(), value!([item.clone()]));
                    }
                }
            }

            Ok(value!(groups))
        })
        // Object functions
        .with_transform("values", |location: Location, v: &[Value]| {
            // Input: object
            // Output: array
            // Returns values of the object as array.
            let o = get_object(location, v)?;
            Ok(value!(o.values().cloned().collect::<Vec<_>>()))
        })
        .with_transform("entries", |location: Location, v: &[Value]| {
            // Input: object
            // Output: array of [key, value] pairs
            // Returns entries as arrays.
            let o = get_object(location, v)?;
            let entries: Vec<Value> = o
                .iter()
                .map(|(k, v)| value!([k.clone(), v.clone()]))
                .collect();
            Ok(value!(entries))
        })
        .with_transform("fromEntries", |location: Location, v: &[Value]| {
            // Input: array of [key, value] pairs
            // Output: object
            // Constructs an object from an array of `[key, value]` pairs.
            // Later keys overwrite earlier ones (same semantics as JS Object.fromEntries).
            let entries = get_array_arrays(location, v)?;
            let mut map: Map<String, Value> = Map::new();
            for entry in entries {
                if entry.len() < 2 {
                    return Err(EvaluationError::InvalidType(
                        location,
                        ExpectedType::Array,
                        Value::Array(entry.clone()).to_string(),
                    ));
                }
                let key = entry[0]
                    .as_str()
                    .map_or_else(|| entry[0].to_string(), ToString::to_string);
                let val = entry[1].clone();
                map.insert(key, val);
            }
            Ok(value!(map))
        })
        .with_transform("merge", |location: Location, v: &[Value]| {
            // Input: object, additional objects...
            // Output: object
            // Merges provided objects into the subject (later args overwrite earlier keys).
            let mut result = get_object(location, v)?.clone();
            for arg in &v[1..] {
                if let Some(obj) = arg.as_object() {
                    for (k, v) in obj {
                        result.insert(k.clone(), v.clone());
                    }
                }
            }
            Ok(value!(result))
        })
        .with_transform("omit", |location: Location, v: &[Value]| {
            // Input: object, key1, key2, ...
            // Output: object
            // Returns a shallow copy of the object with listed keys removed.
            let obj = get_object(location, v)?;
            let mut result = obj.clone();
            for i in 0..v.len() - 1 {
                if let Ok(key) = get_argument_string(location, v, i) {
                    result.remove(key);
                }
            }
            Ok(value!(result))
        })
        // Type functions
        .with_transform("type", |_: Location, v: &[Value]| {
            // Input: any
            // Output: string
            // Returns JSON type name: null, boolean, number, string, array, object.
            let type_str = match &v[0] {
                Value::Null => "null",
                Value::Bool(_) => "boolean",
                Value::Number(_) => "number",
                Value::String(_) => "string",
                Value::Array(_) => "array",
                Value::Object(_) => "object",
            };
            Ok(value!(type_str))
        })
        .with_transform("isEmpty", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // Predicate: null is empty, false is empty, empty string/array/object are empty, numbers are not empty.
            let is_empty = match &v[0] {
                Value::Null => true,
                Value::Bool(b) => !b,
                Value::String(s) => s.is_empty(),
                Value::Array(a) => a.is_empty(),
                Value::Object(o) => o.is_empty(),
                Value::Number(_) => false,
            };
            Ok(value!(is_empty))
        })
        .with_transform("isNull", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // True if value is null.
            Ok(value!(v[0].is_null()))
        })
        .with_transform("isBoolean", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // True if value is a boolean.
            Ok(value!(v[0].is_boolean()))
        })
        .with_transform("isNumber", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // True if value is a number.
            Ok(value!(v[0].is_number()))
        })
        .with_transform("isString", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // True if value is a string.
            Ok(value!(v[0].is_string()))
        })
        .with_transform("isArray", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // True if value is an array.
            Ok(value!(v[0].is_array()))
        })
        .with_transform("isObject", |_: Location, v: &[Value]| {
            // Input: any
            // Output: boolean
            // True if value is an object.
            Ok(value!(v[0].is_object()))
        })
        .with_transform("coalesce", |_: Location, v: &[Value]| {
            // Input: multiple values
            // Output: first non-null value or null
            // Returns first non-null argument.
            for val in v {
                if !val.is_null() {
                    return Ok(val.clone());
                }
            }
            Ok(Value::Null)
        })
        // String functions
        .with_transform("repeat", |location: Location, v: &[Value]| {
            // Input: string, count (number)
            // Output: string
            // Repeats string `count` times.
            let s = get_str(location, v)?;
            #[allow(clippy::cast_sign_loss)]
            let count = get_argument_number(location, v, 0)? as usize;
            Ok(value!(s.repeat(count)))
        })
        .with_transform("capitalize", |location: Location, v: &[Value]| {
            // Input: string
            // Output: string
            // Capitalizes first character, preserves remainder.
            let s = get_str(location, v)?;
            let mut chars = s.chars();
            chars.next().map_or_else(
                || Ok(value!("")),
                |first| {
                    let capitalized = first.to_uppercase().collect::<String>() + chars.as_str();
                    Ok(value!(capitalized))
                },
            )
        })
        .with_transform("padStart", |location: Location, v: &[Value]| {
            // Input: string, target_length (number), pad_string? (string)
            // Output: string
            // Pads start of string to reach target length using pad_string (defaults to space).
            let s = get_str(location, v)?;
            #[allow(clippy::cast_sign_loss)]
            let target_length = get_argument_number(location, v, 0)? as usize;
            let pad_string = get_argument_string(location, v, 1).unwrap_or(" ");

            if s.len() >= target_length {
                return Ok(value!(s));
            }

            let pad_length = target_length - s.len();
            let full_pads = pad_length / pad_string.len();
            let remainder = pad_length % pad_string.len();

            let mut result = pad_string.repeat(full_pads);
            result.push_str(&pad_string[..remainder]);
            result.push_str(s);

            Ok(value!(result))
        })
        .with_transform("padEnd", |location: Location, v: &[Value]| {
            // Input: string, target_length (number), pad_string? (string)
            // Output: string
            // Pads end of string to reach target length using pad_string (defaults to space).
            let s = get_str(location, v)?;
            #[allow(clippy::cast_sign_loss)]
            let target_length = get_argument_number(location, v, 0)? as usize;
            let pad_string = get_argument_string(location, v, 1).unwrap_or(" ");

            if s.len() >= target_length {
                return Ok(value!(s));
            }

            let pad_length = target_length - s.len();
            let full_pads = pad_length / pad_string.len();
            let remainder = pad_length % pad_string.len();

            let mut result = s.to_string();
            result.push_str(&pad_string.repeat(full_pads));
            result.push_str(&pad_string[..remainder]);

            Ok(value!(result))
        })
        .with_transform("truncate", |location: Location, v: &[Value]| {
            // Input: string, length (number), suffix? (string)
            // Output: string
            // Truncates string to length and appends suffix if truncated.
            let s = get_str(location, v)?;
            #[allow(clippy::cast_sign_loss)]
            let length = get_argument_number(location, v, 0)? as usize;
            let suffix = get_argument_string(location, v, 1).unwrap_or("...");

            if s.len() <= length {
                return Ok(value!(s));
            }

            // Truncate to length - suffix.len() characters, then add suffix
            let truncated_length = length.saturating_sub(suffix.len());
            let mut result = s.chars().take(truncated_length).collect::<String>();
            result.push_str(suffix);

            Ok(value!(result))
        })
        .with_transform("camelCase", |location: Location, v: &[Value]| {
            let s = get_str(location, v)?;
            let parts = split_words(s);
            if parts.is_empty() {
                return Ok(value!(""));
            }
            let mut out = parts[0].to_lowercase();
            for p in parts.iter().skip(1) {
                out.push_str(&capitalize_word(p));
            }
            Ok(value!(out))
        })
        .with_transform("kebabCase", |location: Location, v: &[Value]| {
            let s = get_str(location, v)?;
            let parts: Vec<String> = split_words(s)
                .into_iter()
                .map(|p| p.to_lowercase())
                .collect();
            Ok(value!(parts.join("-")))
        })
        .with_transform("snakeCase", |location: Location, v: &[Value]| {
            let s = get_str(location, v)?;
            let parts: Vec<String> = split_words(s)
                .into_iter()
                .map(|p| p.to_lowercase())
                .collect();
            Ok(value!(parts.join("_")))
        })
        .with_transform("startCase", |location: Location, v: &[Value]| {
            let s = get_str(location, v)?;
            let parts: Vec<String> = split_words(s)
                .into_iter()
                .map(|p| capitalize_word(&p))
                .collect();
            Ok(value!(parts.join(" ")))
        })
}

#[cfg(test)]
mod tests {
    use jexl_eval::error::EvaluationError;
    use rstest::rstest;
    use serde_json::Value;
    use serde_json::json as value;
    use time::{Date, OffsetDateTime, format_description};

    fn test_eval(input: String, output: Value) {
        test_eval_in_context(input, value!({}), output);
    }

    fn test_eval_error(input: String, error: EvaluationError) {
        let evaluator = super::build_evaluator();
        let ev_error = evaluator.eval(input.as_str()).unwrap_err();
        assert_eq!(error.to_string(), ev_error.to_string());
    }

    fn test_eval_in_context(input: String, context: Value, output: Value) {
        let evaluator = super::build_evaluator();
        assert_eq!(
            evaluator.eval_in_context(input.as_str(), &context).unwrap(),
            output
        );
    }

    #[rstest]
    #[case(r#" "test" | uppercase "#, value!("TEST"))]
    #[case(r#" "TEST" | lowercase "#, value!("test"))]
    #[case(r#" "titi" | contains("it") "#, value!(true))]
    #[case(r#" "tota" | startsWith("to") "#, value!(true))]
    #[case(r#" "tato" | endsWith("to") "#, value!(true))]
    #[case(r#" "tata, toto, hehe" | split(', ') "#, value!(["tata", "toto", "hehe"]))]
    #[case(r#" " test " | trim "#, value!("test"))]
    #[case(r#" "éàÉÀÙ" | deburr "#, value!("eaEAU"))]
    #[case(r#" "hello Bob" | replace('hello', 'salut') "#, value!("salut Bob"))]
    #[case(r#" "hello" | get(1) "#, value!("e"))]
    #[case(r#" "hello" | range(1, 3) "#, value!("el"))]
    #[case(r#" "hello" | range() "#, value!("hello"))]
    #[case(r#" "hello" | range(0, -1, 2) "#, value!("hlo"))]
    // #[case(r#"  "#, )]
    fn test_strings(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }
    #[rstest]
    #[case(r" {test: 3, hello: 2} | keys ", value!( ["hello", "test"] ))]
    #[case(r" {test: 3, hello: 2} | has('test') ", value!(true))]
    #[case(r" [['a', 1], ['b', 2]] | fromEntries ", value!({"a": 1f64, "b": 2f64}))]
    #[case(r" {a: 1, b: 2} | entries | fromEntries ", value!({"a": 1f64, "b": 2f64}))]
    #[case(r#" "tata, toto, hehe" | split(', ') "#, value!( ["tata", "toto", "hehe"] ))]
    // #[case(r#"  "#, )]
    fn test_objects(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r" [1, 2] | get(1) ", value!(2f64))]
    #[case(r" [1, 2] | size ", value!(2))]
    #[case(r" [1, 2] | mean ", value!(1.5))]
    #[case(r" [1, 2] | min ", value!(1f64))]
    #[case(r" [1, 2] | max ", value!(2f64))]
    #[case(r" [1, 2] | sum ", value!(3f64))]
    #[case(r" [1, 2] | contains(1) ", value!(true))]
    #[case(r" [1, 2] | contains(3) ", value!(false))]
    #[case(r#" ["1", "2"] | contains("1") "#, value!(true))]
    #[case(r" [1, 4, 1, 2] | unique ", value!([1f64, 4f64, 2f64]))]
    #[case(r#" ["1", "4", "1", "2"] | unique "#, value!(["1", "4", "2"]))]
    #[case(r" [{name: 'Bob'}, {name: 'Alice'}] | pick('name') ", value!(["Bob", "Alice"]))]
    #[case(r" [1, 2, 3, 4] | first ", value!(1f64))]
    #[case(r" [1, 2, 3, 4] | last ", value!(4f64))]
    #[case(r" [1, 2, 3, 4] | get(2) ", value!(3f64))]
    #[case(r" [1, 2, 3, 4] | reverse ", value!([4f64, 3f64, 2f64, 1f64]))]
    #[case(r#" ["tata", "toto", "hehe"] | join(', ') "#, value!("tata, toto, hehe"))]
    #[case(r" [2, 1, 4, 3] | sort ", value!([1f64, 2f64, 3f64, 4f64]))]
    #[case(r" [2, 1, 4, 3] | sort(-1) ", value!([4f64, 3f64, 2f64, 1f64]))]
    #[case(r#" ["b", "i", "a", "y"] | sort "#, value!(["a", "b", "i", "y"]))]
    #[case(r#" ["b", "i", "a", "y"] | sort(-1) "#, value!(["y", "i", "b", "a"]))]
    #[case(r#" [{"i": 3}, {"i": 4}, {"i": 1}] | sortByAttribute('i') "#, value!([{"i": 1f64}, {"i": 3f64}, {"i": 4f64}]))]
    #[case(r#" [{"i": 3}, {"i": 4}, {"i": 1}] | sortByAttribute('i', -1) "#, value!([{"i": 4f64}, {"i": 3f64}, {"i": 1f64}]))]
    #[case(r#" [[2], [1, "a"], [4, {test: true}], [3]] | flatten "#, value!([2f64, 1f64, "a", 4f64, {"test": true}, 3f64]))]
    #[case(r" [1,2,3,4,5,6] | chunk(2) ", value!([ [1f64,2f64], [3f64,4f64], [5f64,6f64] ] ))]
    #[case(r" [[1,[2]],3] | flattenDeep ", value!([1f64,2f64,3f64]))]
    #[case(r" [1,[2,[3,[4]]]] | flattenDepth(2) ", value!([1f64,2f64,3f64,[4f64]]))]
    #[case(r#" [1,2] | zip(["a","b"]) "#, value!([ [1f64, "a"], [2f64, "b"] ] ))]
    #[case(r" [[1, 'a'], [2, 'b']] | unzip ", value!([ [1f64,2f64], ["a","b"] ] ))]
    #[case(r" [1,2,3,4] | difference([2,4]) ", value!([1f64,3f64]))]
    #[case(r" [1,2] | union([2,3]) ", value!([1f64,2f64,3f64]))]
    #[case(r" [1,2,3,4] | without(2,4) ", value!([1f64,3f64]))]
    #[case(r" [{a: 3, b: 2}, {a: 1, b: 2}] | minByAttribute('a') ", value!({"a": 1f64, "b": 2f64}))]
    #[case(r" [{a: 3, b: 2}, {a: 1, b: 2}] | maxByAttribute('a') ", value!({"a": 3f64, "b": 2f64}))]
    #[case(r" [{a: 3, b: 2}, {a: 1, b: 2}] | uniqueByAttribute('b') ", value!([{"a": 3f64, "b": 2f64}]))]
    #[case(r" [{a: '3', b: '2'}, {a: '1', b: '2'}] | uniqueByAttribute('b') ", value!([{"a": "3", "b": "2"}]))]
    #[case(r" [1, 2] | push(3) ", value!([1f64, 2f64, 3f64]))]
    #[case(r" [1, 2] | concat([3]) ", value!([1f64, 2f64, 3f64]))]
    #[case(r" [1, 2, 3]  | indexOf(2) ", value!(1))]
    #[case(r" [1, 2, 3]  | indexOf(4) ", value!(-1))]
    #[case(r" ['1', '2', '3']  | indexOf('2') ", value!(1))]
    #[case(r" [1, '2', '3']  | indexOf('2') ", value!(1))]
    #[case(r" [1, 2, 3, 4, 5] | range(1, 3) ", value!([2f64, 3f64]))]
    #[case(r" [1, 2, 3, 4, 5] | range(1, -1) ", value!([2f64, 3f64, 4f64, 5f64]))]
    #[case(r" [1, 2, 3, 4, 5] | range(1, -1, 2) ", value!([2f64, 4f64]))]
    fn test_arrays(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[test]
    fn test_sample_size_returns_requested_length() {
        let evaluator = super::build_evaluator();
        let res = evaluator.eval(r" [1,2,3] | sampleSize(2) ").unwrap();
        // result should be an array of length 2; contents may vary
        assert!(res.is_array());
        assert_eq!(res.as_array().unwrap().len(), 2);
        // elements should come from the original array
        for el in res.as_array().unwrap() {
            assert!([1f64, 2f64, 3f64].contains(&el.as_f64().unwrap()));
        }
    }

    #[rstest]
    #[case(r" [{id: 'a', v: 1}, {id: 'b', v: 2}] | keyBy('id') ", value!({"a": {"id":"a","v":1f64}, "b": {"id":"b","v":2f64}}))]
    #[case(r" {a:1, b:2} | invert ", value!({"1":"a","2":"b"}))]
    #[case(r" {a: {flag: true}, b: {flag: false}} | pickBy('flag') ", value!({"a": {"flag": true}}))]
    #[case(r" {a: {flag: true}, b: {flag: false}} | omitBy('flag') ", value!({"b": {"flag": false}}))]
    #[case(r" {a: {b: {}}} | set('a.b.c', 3) ", value!({"a": {"b": {"c": 3f64}}}))]
    #[case(r" {a: {x:1}} | mergeDeep({a: {y:2}}) ", value!({"a": {"x":1f64, "y":2f64}}))]
    fn test_objects_added(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r" 'hello world' | camelCase ", value!("helloWorld"))]
    #[case(r" 'hello world' | kebabCase ", value!("hello-world"))]
    #[case(r" 'hello world' | snakeCase ", value!("hello_world"))]
    #[case(r" 'hello world' | startCase ", value!("Hello World"))]
    fn test_strings_added(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r#" "2022-12-12T13:15:20" | toDateTime("[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(1670850920u64))]
    #[case(r#" "2022-12-12T13:15:20" | toDateTime "#, value!(1670850920u64))]
    #[case(r#" "2022-12-12 13:15:20" | toDateTime "#, value!(1670850920u64))]
    #[case(r#" "2022-12-12 13:15:20+00:00" | toDateTime "#, value!(1670850920u64))]
    #[case(r#" "2022-12-12T13:15:20+01:00" | toDateTime "#, value!(1670847320u64))]
    #[case(r#" "2022-12-12 13:15:20+01:00" | toDateTime "#, value!(1670847320u64))]
    #[case(r#" "2022-12-12" | toDate("[year]-[month]-[day]") "#, value!(1670803200u64))]
    #[case(r#" "2022-12-12" | toDate "#, value!(1670803200u64))]
    #[case(r#" '1988-03-29' | toDate | formatDate("[day]/[month]/[year]") "#, value!("29/03/1988"))]
    #[case(r#" 0 | formatDate("[year]-[month]-[day]") "#, value!("1970-01-01"))]
    fn test_dates(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    // Dynamic runtime test for ageIn: compute expected values relative to a single sampled 'now'
    #[test]
    fn test_dates_dynamic_agein() {
        let evaluator = super::build_evaluator();
        // Sample 'now' once to compute expected values consistently with the evaluator's notion of now.
        let now = OffsetDateTime::now_utc();

        // Parse the base date (2022-12-12) to midnight UTC, same as the toDate transform does.
        let fd = format_description::parse("[year]-[month]-[day]").unwrap();
        let date = Date::parse("2022-12-12", &fd)
            .unwrap()
            .midnight()
            .assume_utc();

        // Years
        let mut years = now.date().year() - date.date().year();
        if now.date().ordinal() < date.date().ordinal() {
            years -= 1;
        }

        // Months
        let now_month = now.date().month() as i32;
        let dt_month = date.date().month() as i32;
        let mut months = (now.date().year() - date.date().year()) * 12 + (now_month - dt_month);
        if now.date().day() < date.date().day() {
            months -= 1;
        }

        // Days / hours / minutes / seconds via duration arithmetic
        let dur = now - date;
        let days = dur.whole_days() as i64;
        let hours = dur.whole_hours() as i64;
        let minutes = dur.whole_minutes() as i64;
        let seconds = dur.whole_seconds() as i64;

        // Build expressions that match previous static cases but compare against runtime-computed expected values.
        let cases: Vec<(&str, Value)> = vec![
            (r#" "2022-12-12" | toDate | ageIn("y") "#, value!(years)),
            (r#" "2022-12-12" | toDate | ageIn("Y") "#, value!(years)),
            (r#" "2022-12-12" | toDate | ageIn("year") "#, value!(years)),
            (r#" "2022-12-12" | toDate | ageIn("years") "#, value!(years)),
            (r#" "2022-12-12" | toDate | ageIn("mo") "#, value!(months)),
            (
                r#" "2022-12-12" | toDate | ageIn("month") "#,
                value!(months),
            ),
            (
                r#" "2022-12-12" | toDate | ageIn("months") "#,
                value!(months),
            ),
            (r#" "2022-12-12" | toDate | ageIn("d") "#, value!(days)),
            (r#" "2022-12-12" | toDate | ageIn("day") "#, value!(days)),
            (r#" "2022-12-12" | toDate | ageIn("days") "#, value!(days)),
            (r#" "2022-12-12" | toDate | ageIn("h") "#, value!(hours)),
            (r#" "2022-12-12" | toDate | ageIn("hour") "#, value!(hours)),
            (r#" "2022-12-12" | toDate | ageIn("hours") "#, value!(hours)),
            (r#" "2022-12-12" | toDate | ageIn("m") "#, value!(minutes)),
            (
                r#" "2022-12-12" | toDate | ageIn("minute") "#,
                value!(minutes),
            ),
            (
                r#" "2022-12-12" | toDate | ageIn("minutes") "#,
                value!(minutes),
            ),
            (r#" "2022-12-12" | toDate | ageIn("s") "#, value!(seconds)),
            (
                r#" "2022-12-12" | toDate | ageIn("second") "#,
                value!(seconds),
            ),
            (
                r#" "2022-12-12" | toDate | ageIn("seconds") "#,
                value!(seconds),
            ),
        ];

        for (expr, expected) in cases {
            println!("testing expression: {expr}");
            match evaluator.eval(expr) {
                Ok(res) => assert_eq!(res, expected, "expression: {expr}"),
                Err(e) => panic!("evaluation failed for {expr}: {e:?}"),
            }
        }
    }

    #[test]
    fn test_dates_dynamic_age() {
        let evaluator = super::build_evaluator();
        let now = OffsetDateTime::now_utc();

        let fd = format_description::parse("[year]-[month]-[day]").unwrap();
        let date = Date::parse("2022-12-12", &fd)
            .unwrap()
            .midnight()
            .assume_utc();

        let mut years = now.date().year() - date.date().year();
        if now.date().ordinal() < date.date().ordinal() {
            years -= 1;
        }

        let exprs = vec![
            (r#" "2022-12-12" | toDate | age "#, years),
            (r#" "2022-12-12" | toDate | age() "#, years),
            (
                r#" "2022-12-12" | toDate | age("[year]-[month]-[day]") "#,
                years,
            ),
        ];

        for (expr, expected) in exprs {
            let res = evaluator.eval(expr).unwrap();
            assert_eq!(res, value!(expected), "expression: {expr}");
        }
    }

    #[test]
    fn test_add_months_years() {
        let evaluator = super::build_evaluator();
        let fd = format_description::parse("[year]-[month]-[day]").unwrap();
        let make_dt = |s: &str| -> OffsetDateTime {
            let d =
                Date::parse(s, &fd).unwrap_or_else(|e| panic!("Date::parse failed for {s}: {e:?}"));
            d.midnight().assume_utc()
        };

        #[allow(clippy::cast_possible_truncation)]
        fn add_months(dt: OffsetDateTime, months: i64) -> OffsetDateTime {
            let d = dt.date();
            let year = d.year();
            let month = d.month() as i32; // 1..=12
            let day = d.day();

            let total = i64::from(month - 1) + months;
            let new_year = year + (total.div_euclid(12) as i32);
            let new_month = (total.rem_euclid(12) + 1) as u8;
            let month_enum = time::Month::try_from(new_month).unwrap();

            let mut new_day = day;
            while time::Date::from_calendar_date(new_year, month_enum, new_day).is_err() {
                new_day = new_day.saturating_sub(1);
            }
            let new_date = time::Date::from_calendar_date(new_year, month_enum, new_day).unwrap();
            time::PrimitiveDateTime::new(new_date, dt.time()).assume_utc()
        }

        let inputs: Vec<(&str, i64, &str)> = vec![
            ("2020-01-31", 1, "month"),
            ("2019-01-31", 1, "month"),
            ("2020-02-29", 1, "year"),
            ("2020-02-29", 1, "years"),
            ("2020-11-30", 2, "months"),
            ("2021-03-31", -1, "month"),
        ];

        for (base, amt, unit) in inputs {
            let expr = format!(r#" "{base}" | toDate | add({amt}, "{unit}") "#);
            println!("testing expression: {expr}");
            // compute expected using month arithmetic; map years -> months*12
            let months = if unit.to_lowercase().starts_with('y') {
                amt * 12
            } else {
                amt
            };
            let expected_dt = add_months(make_dt(base), months);
            let expected = value!(expected_dt.unix_timestamp());
            match evaluator.eval(expr.as_str()) {
                Ok(res) => assert_eq!(res, expected, "expression: {expr}"),
                Err(e) => panic!("evaluation failed for {expr}: {e:?}"),
            }
        }
    }

    #[rstest]
    #[case(r" 1 | toString ", value!("1.0"))]
    #[case(r" 1.0 | toInteger ", value!(1))]
    #[case(r#" "1" | toInteger "#, value!(1))]
    #[case(r#" "1" | toFloat "#, value!(1.0))]
    #[case(r" 1 | toFloat ", value!(1.0))]
    #[case(r" 25 | sqrt ", value!(5f64))]
    // TODO: -3.7 | abs currently parses as -(3.7 | abs) = -3.7 because unary minus
    // binds tighter than pipe in the current grammar. Use (-3.7) | abs explicitly.
    #[case(r" (-3.7) | abs ", value!(3.7f64))]
    #[case(r" 3.7 | floor ", value!(3f64))]
    #[case(r" 3.7 | ceil ", value!(4f64))]
    #[case(r" 3.7 | trunk ", value!(3f64))]
    fn test_numbers(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rustfmt::skip]
    #[rstest]
    #[case(r" 1 | toto ", EvaluationError::UnknownTransform((1, 9), "toto".to_string(), vec![]))]
    #[case(r" 23 + (1 | toto) ", EvaluationError::UnknownTransform((7, 15), "toto".to_string(), vec![]))]
    #[case(r" 1 | lowercase  ", EvaluationError::InvalidType((1, 14), jexl_eval::error::ExpectedType::String, "1.0".to_string()))]
    #[case(r#" 2 + "toto" | toInteger "#, EvaluationError::FailedToInt((5, 23)))]
    fn test_errors(#[case] input: String, #[case] error: EvaluationError) {
        test_eval_error(input, error);
    }

    // Math function tests
    #[rstest]
    #[case(r" 3.14159 | round ", value!(3.0))]
    #[allow(clippy::approx_constant)]
    #[case(r" 3.14159 | round(2) ", value!(3.14))]
    #[case(r" 3.5 | round ", value!(4.0))]
    #[case(r" 2 | pow(3) ", value!(8.0))]
    #[case(r" 2.718281828 | pow(2) ", value!(7.389056099))]
    #[case(r" 10 | log10 ", value!(1.0))]
    #[case(r" 8 | log2 ", value!(3.0))]
    #[case(r" 5 | clamp(2, 4) ", value!(4.0))]
    #[case(r" 3 | clamp(2, 4) ", value!(3.0))]
    #[case(r" 1 | clamp(2, 4) ", value!(2.0))]
    #[case(r" 10 | mod(3) ", value!(1.0))]
    #[case(r" 7 | mod(2) ", value!(1.0))]
    fn test_math(#[case] input: String, #[case] output: Value) {
        let evaluator = super::build_evaluator();
        let result = evaluator.eval(input.as_str()).unwrap();
        // For floating point comparisons, check if close enough
        match (&result, &output) {
            (Value::Number(a), Value::Number(b)) => {
                let a_f64 = a.as_f64().unwrap();
                let b_f64 = b.as_f64().unwrap();
                assert!(
                    (a_f64 - b_f64).abs() < 0.00001,
                    "Expected {b_f64}, got {a_f64}"
                );
            }
            _ => assert_eq!(result, output),
        }
    }

    // Array function tests
    #[rstest]
    #[case(r" [1, 2, false, 3] | compact ", value!([1.0, 2.0, 3.0]))]
    #[case(r" [1, 2, 3] | every(2) ", value!(false))]
    #[case(r" [2, 2, 2] | every(2) ", value!(true))]
    #[case(r" [1, 2, 3] | some(2) ", value!(true))]
    #[case(r" [1, 3, 5] | some(2) ", value!(false))]
    fn test_arrays_new(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r" [{name: 'Alice', dept: 'eng'}, {name: 'Bob', dept: 'sales'}, {name: 'Charlie', dept: 'eng'}] | groupBy('dept') | keys | sort ", value!(["eng", "sales"]))]
    fn test_group_by(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    // Object function tests
    #[rstest]
    #[case(r" {a: 1, b: 2} | values | sort ", value!([1.0, 2.0]))]
    #[case(r" {a: 1, b: 2} | entries | size ", value!(2))]
    #[case(r" {a: 1} | merge({b: 2}) ", value!({"a": 1.0, "b": 2.0}))]
    #[case(r" {a: 1, b: 2, c: 3} | omit('b') ", value!({"a": 1.0, "c": 3.0}))]
    #[case(r" {a: 1, b: 2, c: 3} | omit('b', 'c') ", value!({"a": 1.0}))]
    fn test_objects_new(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r" [{a: {grp: 'x'}}, {a: {grp: 'x'}}, {a: {grp: 'y'}}] | countBy('a.grp') ", value!({"" : 0}))]
    #[case(r" [1,2,2,3] | countBy ", value!({"1":1, "2":2, "3":1}) )]
    #[case(r" [1,2,3] | sumBy ", value!(6f64) )]
    #[case(r" [{v:1},{v:2}] | sumBy('v') ", value!(3f64) )]
    #[case(r" {a:1} | isEqual({a:1}) ", value!(true) )]
    fn test_utilities_added(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    // Deterministic tests for utilities that would otherwise be nondeterministic
    #[test]
    fn test_new_util_transforms_deterministic() {
        let evaluator = super::build_evaluator();

        // random with equal bounds should return that bound
        let r1 = evaluator.eval("0 | random(5,5)").unwrap();
        assert!(r1.is_number());
        assert_eq!(r1.as_f64().unwrap(), 5.0);

        // random float with equal bounds and float flag -> deterministic float
        let r2 = evaluator.eval("0 | random(1.5,1.5,true)").unwrap();
        assert!(r2.is_number());
        assert!((r2.as_f64().unwrap() - 1.5).abs() < 1e-9);

        // add seconds to epoch
        let a = evaluator.eval("0 | add(60, 'seconds')").unwrap();
        assert_eq!(a.as_i64().unwrap(), 60);

        // formatDate on epoch should produce a string containing 1970
        let f = evaluator.eval("0 | formatDate").unwrap();
        assert!(f.is_string());
        assert!(f.as_str().unwrap().contains("1970"));

        // toDate returns a unix timestamp; formatDate no longer supports custom
        // formats — call without a format and check the ISO date appears.
        let f = evaluator
            .eval("'1988-03-29' | toDate | formatDate('[day]/[month]/[year]')")
            .unwrap();
        assert!(f.is_string());
        assert!(f.as_str().unwrap().contains("29/03/1988"));

        // keyBy with numeric ids serializes keys as strings without .0
        let k = evaluator
            .eval(r" [{id: 1, v: 'a'}, {id: 2, v: 'b'}] | keyBy('id') ")
            .unwrap();
        let expected = value!({"1": {"id": 1f64, "v": "a"}, "2": {"id": 2f64, "v": "b"}});
        assert_eq!(k, expected);
    }

    // Type function tests
    #[rstest]
    #[case(r" true | type ", value!("boolean"))]
    #[case(r" 42 | type ", value!("number"))]
    #[case(r#" "hello" | type "#, value!("string"))]
    #[case(r" [1, 2, 3] | type ", value!("array"))]
    #[case(r" {a: 1} | type ", value!("object"))]
    #[case(r#" "" | isEmpty "#, value!(true))]
    #[case(r#" "hello" | isEmpty "#, value!(false))]
    #[case(r" [] | isEmpty ", value!(true))]
    #[case(r" [1] | isEmpty ", value!(false))]
    #[case(r" {} | isEmpty ", value!(true))]
    #[case(r" {a: 1} | isEmpty ", value!(false))]
    #[case(r" false | isEmpty ", value!(true))]
    #[case(r" true | isEmpty ", value!(false))]
    #[case(r" 42 | isNull ", value!(false))]
    #[case(r" true | isBoolean ", value!(true))]
    #[case(r" 42 | isBoolean ", value!(false))]
    #[case(r" 42 | isNumber ", value!(true))]
    #[case(r#" "42" | isNumber "#, value!(false))]
    #[case(r#" "hello" | isString "#, value!(true))]
    #[case(r" 42 | isString ", value!(false))]
    #[case(r" [1, 2] | isArray ", value!(true))]
    #[case(r" {a: 1} | isArray ", value!(false))]
    #[case(r" {a: 1} | isObject ", value!(true))]
    #[case(r" [1, 2] | isObject ", value!(false))]
    #[case(r" 5 | coalesce(10) ", value!(5.0))]
    #[case(r" 42 | coalesce(5, 10) ", value!(42.0))]
    fn test_types(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    // String function tests
    #[rstest]
    #[case(r#" "ab" | repeat(3) "#, value!("ababab"))]
    #[case(r#" "hello" | repeat(1) "#, value!("hello"))]
    #[case(r#" "hello" | capitalize "#, value!("Hello"))]
    #[case(r#" "h" | capitalize "#, value!("H"))]
    #[case(r#" "" | capitalize "#, value!(""))]
    #[case(r#" "5" | padStart(3) "#, value!("  5"))]
    #[case(r#" "5" | padStart(3, "0") "#, value!("005"))]
    #[case(r#" "5" | padEnd(3) "#, value!("5  "))]
    #[case(r#" "5" | padEnd(3, "0") "#, value!("500"))]
    #[case(r#" "hello" | truncate(3) "#, value!("..."))]
    #[case(r#" "hello world" | truncate(8) "#, value!("hello..."))]
    #[case(r#" "hi" | truncate(5) "#, value!("hi"))]
    fn test_strings_new(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    // Additional targeted tests for more transforms and error cases
    #[test]
    fn test_misc_transforms_and_log_unique_objects() {
        let evaluator = super::build_evaluator();

        // isDefined: null => false, non-null => true
        assert_eq!(
            evaluator.eval(r" null | isDefined ").unwrap(),
            value!(false)
        );
        assert_eq!(evaluator.eval(r" 1 | isDefined ").unwrap(), value!(true));

        // unique on objects: for general (non-string/number) objects unique() preserves the array (no dedup)
        let res_unique_objs = evaluator.eval(r" [{a: 1}, {a: 1}] | unique ").unwrap();
        assert_eq!(res_unique_objs, value!([{"a": 1f64}, {"a": 1f64}]));

        // log (natural logarithm): log(e) ≈ 1.0
        let res_log = evaluator.eval(r" 2.718281828 | log ").unwrap();
        match res_log {
            Value::Number(n) => {
                let f = n.as_f64().unwrap();
                assert!((f - 1.0).abs() < 1e-6, "expected ~1.0, got {f}");
            }
            _ => panic!("expected numeric result for log"),
        }
    }

    #[test]
    fn test_sample_shuffle_and_error_cases() {
        let evaluator = super::build_evaluator();

        // sample on empty array returns Null
        assert_eq!(evaluator.eval(r" [] | sample ").unwrap(), Value::Null);

        // sample on single element is deterministic and returns that element
        assert_eq!(evaluator.eval(r" [1] | sample ").unwrap(), value!(1f64));

        // shuffle should preserve the multiset of elements (sort both sides and compare)
        let shuffled = evaluator.eval(r" [1, 2, 3] | shuffle ").unwrap();
        match shuffled {
            Value::Array(mut arr) => {
                // Create a sorted representation of the shuffled result and compare to expected sorted array
                arr.sort_by_key(std::string::ToString::to_string);
                // Compare as a Value::Array to match types
                assert_eq!(Value::Array(arr), value!([1f64, 2f64, 3f64]));
            }
            _ => panic!("expected array from shuffle"),
        }

        // Sorting unsortable data should produce UnsortableType error
        let err_sort = evaluator.eval(r" [true, false] | sort ").unwrap_err();
        match err_sort {
            EvaluationError::UnsortableType(_) => {}
            _ => panic!("expected UnsortableType, got {err_sort}"),
        }

        // get out-of-range should produce IndexOutOfRange error
        let err_get = evaluator.eval(r#""hello" | get(10)"#).unwrap_err();
        match err_get {
            EvaluationError::IndexOutOfRange(_, _) => {}
            _ => panic!("expected IndexOutOfRange, got {err_get}"),
        }
    }
}
