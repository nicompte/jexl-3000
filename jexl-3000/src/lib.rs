use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Add;

use anyhow::{Context, Result};
use chrono::{Duration, NaiveDateTime};
pub use jexl_eval::{error::EvaluationError, Evaluator};
use serde_json::{json as value, Map, Value};

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Expected value")]
    ExpectedValue,

    #[error("Expected a string, got {0}")]
    ExpectedString(String),

    #[error("Expected an array or a string, got {0}")]
    ExpectedStringOrArray(String),

    #[error("Expected an array, got {0}")]
    ExpectedArray(String),

    #[error("Expected an number, got {0}")]
    ExpectedNumber(String),

    #[error("Expected an object, got {0}")]
    ExpectedObject(String),

    #[error("Missing argument {0}")]
    MissingArgument(usize),

    #[error("Invalid duration type")]
    InvalidDurationType,

    #[error("Unsortable data type")]
    UnsortableType,

    #[error("Failed to transform to integer")]
    FailedToInt,

    #[error("Missing attribute {0}")]
    MissingAttribute(String),
}

#[inline(always)]
fn get_string(v: &[Value]) -> Result<&str, Error> {
    v.first()
        .ok_or(Error::ExpectedValue)?
        .as_str()
        .ok_or_else(|| Error::ExpectedString(v[0].to_string()))
}

#[inline(always)]
fn get_number(v: &[Value]) -> Result<f64, Error> {
    v.first()
        .ok_or(Error::ExpectedValue)?
        .as_f64()
        .ok_or_else(|| Error::ExpectedNumber(v[0].to_string()))
}

#[inline(always)]
fn get_array(v: &[Value]) -> Result<&Vec<Value>, Error> {
    v.first()
        .ok_or(Error::ExpectedValue)?
        .as_array()
        .ok_or_else(|| Error::ExpectedArray(v[0].to_string()))
}

#[inline(always)]
fn get_object(v: &[Value]) -> Result<&Map<String, Value>, Error> {
    v.first()
        .ok_or(Error::ExpectedValue)?
        .as_object()
        .ok_or_else(|| Error::ExpectedObject(v[0].to_string()))
}

#[inline(always)]
fn get_array_numbers(v: &[Value]) -> Result<Vec<f64>, Error> {
    get_array(v)?
        .iter()
        .map(|v| {
            v.as_f64()
                .ok_or_else(|| Error::ExpectedNumber(v.to_string()))
        })
        .collect::<Result<_, Error>>()
}

#[inline(always)]
fn get_array_strings(v: &[Value]) -> Result<Vec<&str>, Error> {
    get_array(v)?
        .iter()
        .map(|v| {
            v.as_str()
                .ok_or_else(|| Error::ExpectedNumber(v.to_string()))
        })
        .collect::<Result<_, Error>>()
}

#[inline(always)]
fn get_array_objects(v: &[Value]) -> Result<Vec<&Map<String, Value>>, Error> {
    get_array(v)?
        .iter()
        .map(|v| {
            v.as_object()
                .ok_or_else(|| Error::ExpectedObject(v.to_string()))
        })
        .collect::<Result<_, Error>>()
}

#[inline(always)]
fn get_array_arrays(v: &[Value]) -> Result<Vec<&Vec<Value>>, Error> {
    get_array(v)?
        .iter()
        .map(|v| {
            v.as_array()
                .ok_or_else(|| Error::ExpectedArray(v.to_string()))
        })
        .collect::<Result<Vec<_>, Error>>()
}

#[inline(always)]
fn get_argument(v: &[Value], index: usize) -> Result<&Value, Error> {
    v.get(index + 1).ok_or(Error::MissingArgument(index))
}

#[inline(always)]
fn get_argument_string(v: &[Value], index: usize) -> Result<&str, Error> {
    get_argument(v, index)?
        .as_str()
        .ok_or_else(|| Error::ExpectedString(v[index].to_string()))
}

#[inline(always)]
fn get_argument_number(v: &[Value], index: usize) -> Result<f64, Error> {
    get_argument(v, index)?
        .as_f64()
        .ok_or_else(|| Error::ExpectedNumber(v[index].to_string()))
}

#[inline(always)]
fn get_argument_integer(v: &[Value], index: usize) -> Result<i64, Error> {
    get_argument_number(v, index).map(|n| n as i64)
}

#[inline(always)]
fn unique<T: Eq + Hash + Clone>(v: &mut Vec<T>) {
    let mut uniques = HashSet::new();
    v.retain(|e| uniques.insert(e.clone()));
}

#[inline(always)]
fn build_duration(duration: i64, duration_type: &str) -> Result<Duration, Error> {
    match duration_type {
        "years" => Ok(Duration::days(365 * duration)),
        "months" => Ok(Duration::weeks(4 * duration)),
        "weeks" => Ok(Duration::weeks(duration)),
        "days" => Ok(Duration::days(duration)),
        "hours" => Ok(Duration::hours(duration)),
        "minutes" => Ok(Duration::minutes(duration)),
        "seconds" => Ok(Duration::seconds(duration)),
        _ => Err(Error::InvalidDurationType),
    }
}

pub fn build_evaluator() -> Evaluator<'static> {
    Evaluator::new()
        .with_transform("isDefined", |v: &[Value]| Ok(value!(v[0].is_null())))
        .with_transform("lowercase", |v: &[Value]| {
            let s = get_string(v)?;
            Ok(value!(s.to_lowercase()))
        })
        .with_transform("uppercase", |v: &[Value]| {
            let s = get_string(v)?;
            Ok(value!(s.to_uppercase()))
        })
        .with_transform("mean", |v: &[Value]| {
            let numbers = get_array_numbers(v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().sum::<f64>() / numbers.len() as f64))
        })
        .with_transform("max", |v: &[Value]| {
            let numbers = get_array_numbers(v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().copied().fold(f64::NAN, f64::max)))
        })
        .with_transform("maxByAttribute", |v: &[Value]| {
            let objects = get_array_objects(v).unwrap_or_else(|_| vec![]);
            let attribute = get_argument_string(v, 0)?;
            let mut max_value = None;
            let mut max_object = None;
            for object in objects {
                let value = object
                    .get(attribute)
                    .ok_or_else(|| Error::MissingAttribute(attribute.to_string()))?;
                if let Some(max) = max_value {
                    if value.as_f64().ok_or_else(|| {
                        EvaluationError::ExpectedNumber(
                            "maxByAttribute".to_string(),
                            value.to_string(),
                        )
                    })? > max
                    {
                        max_value = Some(value.as_f64().ok_or_else(|| {
                            EvaluationError::ExpectedNumber(
                                "maxByAttribute".to_string(),
                                value.to_string(),
                            )
                        })?);
                        max_object = Some(object);
                    }
                } else {
                    max_value = Some(value.as_f64().ok_or_else(|| {
                        EvaluationError::ExpectedNumber(
                            "maxByAttribute".to_string(),
                            value.to_string(),
                        )
                    })?);
                    max_object = Some(object);
                }
            }
            Ok(value!(max_object))
        })
        .with_transform("minByAttribute", |v: &[Value]| {
            let objects = get_array_objects(v).unwrap_or_else(|_| vec![]);
            let attribute = get_argument_string(v, 0)?;
            let mut min_value = None;
            let mut min_object = None;
            for object in objects {
                let value = object
                    .get(attribute)
                    .ok_or_else(|| Error::MissingAttribute(attribute.to_string()))?;
                if let Some(min) = min_value {
                    if value.as_f64().ok_or_else(|| {
                        EvaluationError::ExpectedNumber(
                            "maxByAttribute".to_string(),
                            value.to_string(),
                        )
                    })? < min
                    {
                        min_value = Some(value.as_f64().ok_or_else(|| {
                            EvaluationError::ExpectedNumber(
                                "maxByAttribute".to_string(),
                                value.to_string(),
                            )
                        })?);
                        min_object = Some(object);
                    }
                } else {
                    min_value = Some(value.as_f64().ok_or_else(|| {
                        EvaluationError::ExpectedNumber(
                            "maxByAttribute".to_string(),
                            value.to_string(),
                        )
                    })?);
                    min_object = Some(object);
                }
            }
            Ok(value!(min_object))
        })
        .with_transform("min", |v: &[Value]| {
            let numbers = get_array_numbers(v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().copied().fold(f64::NAN, f64::min)))
        })
        .with_transform("sum", |v: &[Value]| {
            let numbers = get_array_numbers(v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().sum::<f64>()))
        })
        .with_transform("get", |v: &[Value]| {
            let n = get_argument_number(v, 0)? as usize;
            if let Ok(array) = get_array(v) {
                Ok(value!(array
                    .get(n)
                    .ok_or_else(|| { EvaluationError::IndexOutOfRange(n) })?))
            } else if let Ok(string) = get_string(v) {
                Ok(value!(string
                    .chars()
                    .nth(n)
                    .ok_or_else(|| { EvaluationError::IndexOutOfRange(n) })?))
            } else {
                Err(anyhow::Error::new(EvaluationError::ExpectedArrayOrString(
                    "get".to_string(),
                    v[0].to_string(),
                )))
            }
        })
        .with_transform("range", |v: &[Value]| {
            let start = get_argument_number(v, 0).unwrap_or(0f64) as usize;
            if let Ok(array) = get_array(v) {
                let end = get_argument_number(v, 1)
                    .map(|end| {
                        if end == -1f64 {
                            array.len() as f64
                        } else {
                            end
                        }
                    })
                    .unwrap_or(array.len() as f64) as usize;
                let step = get_argument_number(v, 2).unwrap_or(1f64) as usize;
                if end < start || end > array.len() as usize {
                    return Err(anyhow::Error::new(EvaluationError::IndexOutOfRange(end)));
                }
                let mut result = vec![];
                for i in (start..end).step_by(step) {
                    result.push(value!(array[i]));
                }
                Ok(value!(result))
            } else if let Ok(string) = get_string(v) {
                let end = get_argument_number(v, 1)
                    .map(|end| {
                        if end == -1f64 {
                            string.len() as f64
                        } else {
                            end
                        }
                    })
                    .unwrap_or(string.len() as f64) as usize;
                let step = get_argument_number(v, 2).unwrap_or(1f64) as usize;
                if end < start || end > string.len() as usize {
                    return Err(anyhow::Error::new(EvaluationError::IndexOutOfRange(end)));
                }
                let mut result = String::from("");
                for i in (start..end).step_by(step) {
                    // result.push(value!(string.chars().nth(i)));
                    result += &string.chars().nth(i).unwrap().to_string();
                }
                Ok(value!(result))
            } else {
                Err(anyhow::Error::new(EvaluationError::ExpectedArrayOrString(
                    "range".to_string(),
                    v[0].to_string(),
                )))
            }
        })
        .with_transform("first", |v: &[Value]| {
            let array = get_array(v)?;
            Ok(value!(array.first()))
        })
        .with_transform("last", |v: &[Value]| {
            let array = get_array(v)?;
            Ok(value!(array.last()))
        })
        .with_transform("unique", |v: &[Value]| {
            if let Ok(mut array) = get_array_strings(v) {
                unique(&mut array);
                Ok(value!(array))
            } else if let Ok(array) = get_array_numbers(v) {
                let mut str_array = array.iter().map(|n| n.to_string()).collect::<Vec<_>>();
                unique(&mut str_array);
                Ok(value!(str_array
                    .iter()
                    .map(|n| n.parse::<f64>().map_err(|_| {
                        EvaluationError::ExpectedNumber("unique".to_string(), n.to_string())
                    }))
                    .collect::<Result<Vec<_>, _>>()?))
            } else {
                Ok(value!(get_array(v)?))
            }
        })
        .with_transform("uniqueByAttribute", |v: &[Value]| {
            let array = get_array(v)?;
            let attribute = get_argument_string(v, 0)?;
            let mut unique_values = HashSet::new();
            let mut unique_objects = vec![];
            for object in array {
                let value = object
                    .get(attribute)
                    .ok_or_else(|| Error::MissingAttribute(attribute.to_string()))?;
                let value = if let Some(value) = value.as_str() {
                    value.to_string()
                } else {
                    value
                        .as_f64()
                        .ok_or_else(|| {
                            EvaluationError::ExpectedNumber(
                                "uniqueByAttribute".to_string(),
                                value.to_string(),
                            )
                        })?
                        .to_string()
                };
                if unique_values.insert(value.clone()) {
                    unique_objects.push(object);
                }
            }
            Ok(value!(unique_objects))
        })
        .with_transform("len", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(0));
            }
            if let Ok(array) = get_array(v) {
                Ok(value!(array.len()))
            } else if let Ok(string) = get_string(v) {
                Ok(value!(string.len()))
            } else {
                Err(anyhow::Error::new(Error::ExpectedStringOrArray(
                    v[0].to_string(),
                )))
            }
        })
        .with_transform("pick", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!([]));
            }
            let name = get_argument_string(v, 0)?;
            let objects = get_array_objects(v)?;
            let data = objects
                .iter()
                .map(|object| object.get(name).unwrap_or(&Value::Null))
                .collect::<Vec<_>>();
            Ok(value!(data))
        })
        .with_transform("isBefore", |v: &[Value]| {
            let date = get_number(v)?;
            let to_compare = get_argument_number(v, 0)?;
            Ok(value!(date < to_compare))
        })
        .with_transform("isAfter", |v: &[Value]| {
            let date = get_number(v)?;
            let to_compare = get_argument_number(v, 0)?;
            Ok(value!(date > to_compare))
        })
        .with_transform("isOlderThan", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let date = get_string(v)?;
            let to_compare = get_argument_integer(v, 0)?;
            let period_type = get_argument_string(v, 1)?;
            let format = get_argument_string(v, 2)?;

            let date = NaiveDateTime::parse_from_str(date, format)?;
            let utc_date = chrono::DateTime::<chrono::Utc>::from_utc(date, chrono::Utc);
            let duration = build_duration(to_compare, period_type)?;
            let now = chrono::Utc::now();

            Ok(value!(utc_date.add(duration) < now))
        })
        .with_transform("isYoungerThan", |v: &[Value]| {
            let date = get_string(v)?;
            let to_compare = get_argument_integer(v, 0)?;
            let period_type = get_argument_string(v, 1)?;
            let format = get_argument_string(v, 2)?;

            let date = NaiveDateTime::parse_from_str(date, format)?;
            let utc_date = chrono::DateTime::<chrono::Utc>::from_utc(date, chrono::Utc);
            let duration = build_duration(to_compare, period_type)?;
            let now = chrono::Utc::now();

            Ok(value!(utc_date.add(duration) > now))
        })
        .with_transform("toDate", |v: &[Value]| {
            let date = get_string(v)?;
            let format = get_argument_string(v, 0)?;
            Ok(value!(chrono::NaiveDate::parse_from_str(date, format)
                .context("failed to parse date")?
                .and_hms_opt(0, 0, 0)
                .context("failed to add datetime")?
                .timestamp()))
        })
        .with_transform("toDateTime", |v: &[Value]| {
            let date = get_string(v)?;
            let format = get_argument_string(v, 0)?;
            Ok(value!(chrono::NaiveDateTime::parse_from_str(date, format)
                .context("failed to parse date")?
                .timestamp()))
        })
        .with_transform("sort", |v: &[Value]| {
            let reverse = {
                if let Ok(order) = get_argument_number(v, 0) {
                    order == -1.0
                } else {
                    false
                }
            };
            let val = if let Ok(mut arr) = get_array_numbers(v) {
                arr.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                if reverse {
                    arr.reverse();
                }
                value!(arr)
            } else if let Ok(mut arr) = get_array_strings(v) {
                arr.sort_unstable();
                if reverse {
                    arr.reverse();
                }
                value!(arr)
            } else {
                return Err(Error::UnsortableType.into());
            };

            Ok(val)
        })
        .with_transform("sortByAttribute", |v: &[Value]| {
            let by = get_argument_string(v, 0)?;
            let reverse = {
                if let Ok(order) = get_argument_number(v, 1) {
                    order == -1.0
                } else {
                    false
                }
            };
            let mut val = get_array_objects(v)?;
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
        .with_transform("reverse", |v: &[Value]| {
            let val = if let Ok(mut arr) = get_array_numbers(v) {
                arr.reverse();
                value!(arr)
            } else if let Ok(mut arr) = get_array_objects(v) {
                arr.reverse();
                value!(arr)
            } else if let Ok(mut arr) = get_array_strings(v) {
                arr.reverse();
                value!(arr)
            } else {
                return Err(Error::UnsortableType.into());
            };

            Ok(val)
        })
        .with_transform("flatten", |v: &[Value]| {
            let array = get_array_arrays(v)?;
            Ok(value!(array.into_iter().flatten().collect::<Vec<_>>()))
        })
        .with_transform("contains", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(0));
            }
            if let Ok(array) = get_array(v) {
                Ok(value!(array.contains(&v[1])))
            } else if let Ok(string) = get_string(v) {
                let v = get_argument_string(v, 0)?;
                Ok(value!(string.contains(v)))
            } else {
                Err(anyhow::Error::new(Error::ExpectedStringOrArray(
                    v[0].to_string(),
                )))
            }
        })
        .with_transform("startsWith", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let s = get_string(v)?;
            let v = get_argument_string(v, 0)?;
            Ok(value!(s.starts_with(v)))
        })
        .with_transform("endsWith", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let s = get_string(v)?;
            let v = get_argument_string(v, 0)?;
            Ok(value!(s.ends_with(v)))
        })
        .with_transform("toString", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            Ok(value!(v[0].to_string()))
        })
        .with_transform("toInt", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            if let Some(v) = v[0].as_str() {
                Ok(value!(
                    v.parse::<f64>().context("failed to parse to f64")? as i64
                ))
            } else if let Some(v) = v[0].as_f64() {
                Ok(value!(v as i64))
            } else if let Some(v) = v[0].as_i64() {
                Ok(value!(v))
            } else {
                Err(Error::FailedToInt.into())
            }
        })
        .with_transform("toFloat", |v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            if let Some(v) = v[0].as_str() {
                Ok(value!(v
                    .parse::<f64>()
                    .context("failed to parse to f64")?))
            } else if let Some(v) = v[0].as_f64() {
                Ok(value!(v))
            } else if let Some(v) = v[0].as_i64() {
                Ok(value!(v as f64))
            } else {
                Err(Error::FailedToInt.into())
            }
        })
        .with_transform("keys", |v: &[Value]| {
            let o = get_object(v)?;
            Ok(value!(o.keys().map(|k| k.to_string()).collect::<Vec<_>>()))
        })
        .with_transform("has", |v: &[Value]| {
            let o = get_object(v)?;
            let k = get_argument_string(v, 0)?;
            Ok(value!(o.contains_key(k)))
        })
        .with_transform("split", |v: &[Value]| {
            let s = get_string(v)?;
            let v = get_argument_string(v, 0)?;
            Ok(value!(s
                .split(v)
                .map(|s| s.to_string())
                .collect::<Vec<_>>()))
        })
        .with_transform("join", |v: &[Value]| {
            let s = get_array_strings(v)?;
            let v = get_argument_string(v, 0)?;
            Ok(value!(s.join(v)))
        })
        .with_transform("trim", |v: &[Value]| {
            let s = get_string(v)?;
            Ok(value!(s.trim().to_string()))
        })
        .with_transform("sqrt", |v: &[Value]| {
            let v = get_number(v)?;
            Ok(value!(v.sqrt()))
        })
        .with_transform("abs", |v: &[Value]| {
            let v = get_number(v)?;
            Ok(value!(v.abs()))
        })
        .with_transform("floor", |v: &[Value]| {
            let v = get_number(v)?;
            Ok(value!(v.floor()))
        })
        .with_transform("ceil", |v: &[Value]| {
            let v = get_number(v)?;
            Ok(value!(v.ceil()))
        })
        .with_transform("trunk", |v: &[Value]| {
            let v = get_number(v)?;
            Ok(value!(v.trunc()))
        })
        .with_transform("push", |v: &[Value]| {
            let mut array = get_array(v)?.clone();
            array.push(v[1].clone());
            Ok(value!(array))
        })
        .with_transform("concat", |v: &[Value]| {
            let mut array = get_array(v)?.clone();
            for v in v[1..].iter() {
                if let Some(v) = v.as_array() {
                    array.extend(v.iter().cloned());
                } else {
                    array.push(v.clone());
                }
            }
            Ok(value!(array))
        })
        .with_transform("deburr", |v: &[Value]| {
            let s = get_string(v)?;
            Ok(value!(diacritics::remove_diacritics(s)))
        })
        .with_transform("indexOf", |v: &[Value]| {
            let array = get_array(v)?;
            let a = get_argument(v, 0)?;
            let index = array
                .iter()
                .position(|e| e == a)
                .map(|i| i as i64)
                .unwrap_or(-1);
            Ok(value!(index))
        })
        .with_transform("replace", |v: &[Value]| {
            let s = get_string(v)?;
            let replaced = get_argument_string(v, 0)?;
            let replacer = get_argument_string(v, 1)?;
            Ok(value!(s.replace(replaced, replacer)))
        })
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use rstest::rstest;
    use serde_json::json as value;
    use serde_json::Value;

    fn test_eval(input: String, output: Value) {
        test_eval_in_context(input, value!({}), output);
    }

    fn test_eval_error(input: String, error: String) {
        let evaluator = super::build_evaluator();

        let ev_error = evaluator.eval(input.as_str()).unwrap_err();
        let ev_error = anyhow::Chain::new(ev_error.source().unwrap())
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        assert_eq!(error, ev_error);
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
    #[case(r#" {test: 3, hello: 2} | keys "#, value!(["hello", "test"]))]
    #[case(r#" {test: 3, hello: 2} | has('test') "#, value!(true))]
    #[case(r#" "tata, toto, hehe" | split(', ') "#, value!(["tata", "toto", "hehe"]))]
    // #[case(r#"  "#, )]
    fn test_objects(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r#" [1, 2] | get(1) "#, value!(2f64))]
    #[case(r#" [1, 2] | len "#, value!(2))]
    #[case(r#" [1, 2] | mean "#, value!(1.5))]
    #[case(r#" [1, 2] | min "#, value!(1f64))]
    #[case(r#" [1, 2] | max "#, value!(2f64))]
    #[case(r#" [1, 2] | sum "#, value!(3f64))]
    #[case(r#" [1, 2] | contains(1) "#, value!(true))]
    #[case(r#" [1, 2] | contains(3) "#, value!(false))]
    #[case(r#" ["1", "2"] | contains("1") "#, value!(true))]
    #[case(r#" [1, 4, 1, 2] | unique "#, value!([1f64, 4f64, 2f64]))]
    #[case(r#" ["1", "4", "1", "2"] | unique "#, value!(["1", "4", "2"]))]
    #[case(r#" [{name: 'Bob'}, {name: 'Alice'}] | pick('name') "#, value!(["Bob", "Alice"]))]
    #[case(r#" [1, 2, 3, 4] | first "#, value!(1f64))]
    #[case(r#" [1, 2, 3, 4] | last "#, value!(4f64))]
    #[case(r#" [1, 2, 3, 4] | get(2) "#, value!(3f64))]
    #[case(r#" [1, 2, 3, 4] | reverse "#, value!([4f64, 3f64, 2f64, 1f64]))]
    #[case(r#" ["tata", "toto", "hehe"] | join(', ') "#, value!("tata, toto, hehe"))]
    #[case(r#" [2, 1, 4, 3] | sort "#, value!([1f64, 2f64, 3f64, 4f64]))]
    #[case(r#" [2, 1, 4, 3] | sort(-1) "#, value!([4f64, 3f64, 2f64, 1f64]))]
    #[case(r#" ["b", "i", "a", "y"] | sort "#, value!(["a", "b", "i", "y"]))]
    #[case(r#" ["b", "i", "a", "y"] | sort(-1) "#, value!(["y", "i", "b", "a"]))]
    #[case(r#" [{"i": 3}, {"i": 4}, {"i": 1}] | sortByAttribute('i') "#, value!([{"i": 1f64}, {"i": 3f64}, {"i": 4f64}]))]
    #[case(r#" [{"i": 3}, {"i": 4}, {"i": 1}] | sortByAttribute('i', -1) "#, value!([{"i": 4f64}, {"i": 3f64}, {"i": 1f64}]))]
    #[case(r#" [[2], [1, "a"], [4, {test: true}], [3]] | flatten "#, value!([2f64, 1f64, "a", 4f64, {"test": true}, 3f64]))]
    #[case(r#" [{a: 3, b: 2}, {a: 1, b: 2}] | minByAttribute('a') "#, value!({"a": 1f64, "b": 2f64}))]
    #[case(r#" [{a: 3, b: 2}, {a: 1, b: 2}] | maxByAttribute('a') "#, value!({"a": 3f64, "b": 2f64}))]
    #[case(r#" [{a: 3, b: 2}, {a: 1, b: 2}] | uniqueByAttribute('b') "#, value!([{"a": 3f64, "b": 2f64}]))]
    #[case(r#" [{a: '3', b: '2'}, {a: '1', b: '2'}] | uniqueByAttribute('b') "#, value!([{"a": "3", "b": "2"}]))]
    #[case(r#" [1, 2] | push(3) "#, value!([1f64, 2f64, 3f64]))]
    #[case(r#" [1, 2] | concat([3]) "#, value!([1f64, 2f64, 3f64]))]
    #[case(r#" [1, 2, 3]  | indexOf(2) "#, value!(1))]
    #[case(r#" [1, 2, 3]  | indexOf(4) "#, value!(-1))]
    #[case(r#" ['1', '2', '3']  | indexOf('2') "#, value!(1))]
    #[case(r#" [1, '2', '3']  | indexOf('2') "#, value!(1))]
    #[case(r#" [1, 2, 3, 4, 5] | range(1, 3) "#, value!([2f64, 3f64]))]
    #[case(r#" [1, 2, 3, 4, 5] | range(1, -1) "#, value!([2f64, 3f64, 4f64, 5f64]))]
    #[case(r#" [1, 2, 3, 4, 5] | range(1, -1, 2) "#, value!([2f64, 4f64]))]
    // #[case(r#"  "#, )]
    fn test_arrays(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r#" "2022-12-12T13:15:20" | toDateTime("%Y-%m-%dT%H:%M:%S") "#, value!(1670850920u64))]
    #[case(r#" "2022-12-12" | toDate("%Y-%m-%d") "#, value!(1670803200u64))]
    #[case(r#" "2032-12-12" | toDate("%Y-%m-%d") | isAfter($now)"#, value!(true))]
    #[case(r#" "2012-12-12" | toDate("%Y-%m-%d") | isBefore($now)"#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "days", "%Y-%m-%dT%H:%M:%S") "#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "weeks", "%Y-%m-%dT%H:%M:%S") "#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "months", "%Y-%m-%dT%H:%M:%S") "#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "years", "%Y-%m-%dT%H:%M:%S") "#, value!(true))]
    #[case(r#" "2021-01-01T00:00:00" | isYoungerThan(100, "days", "%Y-%m-%dT%H:%M:%S") "#, value!(false))]
    #[case(r#" "2019-01-01T00:00:00" | isYoungerThan(100, "weeks", "%Y-%m-%dT%H:%M:%S") "#, value!(false))]
    #[case(r#" "2010-01-01T00:00:00" | isYoungerThan(100, "months", "%Y-%m-%dT%H:%M:%S") "#, value!(false))]
    #[case(r#" "1900-01-01T00:00:00" | isYoungerThan(100, "years", "%Y-%m-%dT%H:%M:%S") "#, value!(false))]
    // #[case(r#"  "#, )]
    fn test_dates(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rstest]
    #[case(r#" 1 | toString "#, value!("1.0"))]
    #[case(r#" 1.0 | toInt "#, value!(1))]
    #[case(r#" "1" | toInt "#, value!(1))]
    #[case(r#" "1" | toFloat "#, value!(1.0))]
    #[case(r#" 1 | toFloat "#, value!(1.0))]
    #[case(r#" 25 | sqrt "#, value!(5f64))]
    #[case(r#" -3.7 | abs "#, value!(3.7f64))]
    #[case(r#" 3.7 | floor "#, value!(3f64))]
    #[case(r#" 3.7 | ceil "#, value!(4f64))]
    #[case(r#" 3.7 | trunk "#, value!(3f64))]
    // #[case(r#"  "#, )]
    fn test_numbers(#[case] input: String, #[case] output: Value) {
        test_eval(input, output);
    }

    #[rustfmt::skip]
    #[rstest]
    #[case(r#" 1 | lowercase "#, "Failed transform: lowercase\nExpected a string, got 1.0")]
    // #[case(r#"  "#, )]
    fn test_errors(#[case] input: String, #[case] error: String) {
        test_eval_error(input, error);
    }
}
