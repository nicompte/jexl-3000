use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Add;

pub use jexl_eval::{error::EvaluationError, Evaluator};
use jexl_eval::{error::ExpectedType, Location};
use serde_json::{json as value, Map, Value};
use time::{format_description, Date, Duration, OffsetDateTime, PrimitiveDateTime};

#[inline(always)]
fn get_string(location: Location, v: &[Value]) -> Result<String, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_str()
        .map(|s| s.to_owned())
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::String, v[0].to_string())
        })
}

#[inline(always)]
fn get_number(location: Location, v: &[Value]) -> Result<f64, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_f64()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::Number, v[0].to_string())
        })
}

#[inline(always)]
fn get_array(location: Location, v: &[Value]) -> Result<&Vec<Value>, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_array()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::Array, v[0].to_string())
        })
}

#[inline(always)]
fn get_object(location: Location, v: &[Value]) -> Result<&Map<String, Value>, EvaluationError> {
    v.first()
        .ok_or(EvaluationError::ExpectedValue(location))?
        .as_object()
        .ok_or_else(|| {
            EvaluationError::InvalidType(location, ExpectedType::Object, v[0].to_string())
        })
}

#[inline(always)]
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

#[inline(always)]
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

#[inline(always)]
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

#[inline(always)]
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

#[inline(always)]
fn get_argument(location: Location, v: &[Value], index: usize) -> Result<&Value, EvaluationError> {
    v.get(index + 1)
        .ok_or(EvaluationError::MissingArgument(location, index))
}

#[inline(always)]
fn get_argument_string(
    location: Location,
    v: &[Value],
    index: usize,
) -> Result<&str, EvaluationError> {
    get_argument(location, v, index)?.as_str().ok_or_else(|| {
        EvaluationError::InvalidType(location, ExpectedType::String, v[index].to_string())
    })
}

#[inline(always)]
fn get_argument_number(
    location: Location,
    v: &[Value],
    index: usize,
) -> Result<f64, EvaluationError> {
    get_argument(location, v, index)?.as_f64().ok_or_else(|| {
        EvaluationError::InvalidType(location, ExpectedType::Number, v[index].to_string())
    })
}

#[inline(always)]
fn get_argument_integer(
    location: Location,
    v: &[Value],
    index: usize,
) -> Result<i64, EvaluationError> {
    get_argument_number(location, v, index).map(|n| n as i64)
}

#[inline(always)]
fn unique<T: Eq + Hash + Clone>(v: &mut Vec<T>) {
    let mut uniques = HashSet::new();
    v.retain(|e| uniques.insert(e.clone()));
}

#[inline(always)]
fn build_duration(
    location: Location,
    duration: i64,
    duration_type: &str,
) -> Result<Duration, EvaluationError> {
    match duration_type {
        "years" => Ok(Duration::days(365 * duration)),
        "months" => Ok(Duration::weeks(4 * duration)),
        "weeks" => Ok(Duration::weeks(duration)),
        "days" => Ok(Duration::days(duration)),
        "hours" => Ok(Duration::hours(duration)),
        "minutes" => Ok(Duration::minutes(duration)),
        "seconds" => Ok(Duration::seconds(duration)),
        _ => Err(EvaluationError::InvalidDurationType(location)),
    }
}

pub fn build_evaluator() -> Evaluator<'static> {
    Evaluator::new()
        .with_transform("isDefined", |_: Location, v: &[Value]| {
            Ok(value!(!v[0].is_null()))
        })
        .with_transform("lowercase", |location: Location, v: &[Value]| {
            let s = get_string(location, v)?;
            Ok(value!(s.to_lowercase()))
        })
        .with_transform("uppercase", |location: Location, v: &[Value]| {
            let s = get_string(location, v)?;
            let result = s.to_uppercase();
            Ok(value!(result))
        })
        .with_transform("mean", |location: Location, v: &[Value]| {
            let numbers = get_array_numbers(location, v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().sum::<f64>() / numbers.len() as f64))
        })
        .with_transform("max", |location: Location, v: &[Value]| {
            let numbers = get_array_numbers(location, v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().copied().fold(f64::NAN, f64::max)))
        })
        .with_transform("maxByAttribute", |location: Location, v: &[Value]| {
            let objects = get_array_objects(location, v).unwrap_or_else(|_| vec![]);
            let attribute = get_argument_string(location, v, 0)?;
            let mut max_value = None;
            let mut max_object = None;
            for object in objects {
                let value = object.get(attribute).ok_or_else(|| {
                    EvaluationError::MissingAttribute(location, attribute.to_string())
                })?;
                if let Some(max) = max_value {
                    if value.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Number,
                            value.to_string(),
                        )
                    })? > max
                    {
                        max_value = Some(value.as_f64().ok_or_else(|| {
                            EvaluationError::InvalidType(
                                location,
                                ExpectedType::Number,
                                value.to_string(),
                            )
                        })?);
                        max_object = Some(object);
                    }
                } else {
                    max_value = Some(value.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Number,
                            value.to_string(),
                        )
                    })?);
                    max_object = Some(object);
                }
            }
            Ok(value!(max_object))
        })
        .with_transform("minByAttribute", |location: Location, v: &[Value]| {
            let objects = get_array_objects(location, v).unwrap_or_else(|_| vec![]);
            let attribute = get_argument_string(location, v, 0)?;
            let mut min_value = None;
            let mut min_object = None;
            for object in objects {
                let value = object.get(attribute).ok_or_else(|| {
                    EvaluationError::MissingAttribute(location, attribute.to_string())
                })?;
                if let Some(min) = min_value {
                    if value.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Number,
                            value.to_string(),
                        )
                    })? < min
                    {
                        min_value = Some(value.as_f64().ok_or_else(|| {
                            EvaluationError::InvalidType(
                                location,
                                ExpectedType::Number,
                                value.to_string(),
                            )
                        })?);
                        min_object = Some(object);
                    }
                } else {
                    min_value = Some(value.as_f64().ok_or_else(|| {
                        EvaluationError::InvalidType(
                            location,
                            ExpectedType::Number,
                            value.to_string(),
                        )
                    })?);
                    min_object = Some(object);
                }
            }
            Ok(value!(min_object))
        })
        .with_transform("min", |location: Location, v: &[Value]| {
            let numbers = get_array_numbers(location, v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().copied().fold(f64::NAN, f64::min)))
        })
        .with_transform("sum", |location: Location, v: &[Value]| {
            let numbers = get_array_numbers(location, v).unwrap_or_else(|_| vec![]);
            Ok(value!(numbers.iter().sum::<f64>()))
        })
        .with_transform("get", |location: Location, v: &[Value]| {
            let n = get_argument_number(location, v, 0)? as usize;
            if let Ok(array) = get_array(location, v) {
                Ok(value!(array.get(n).ok_or_else(|| {
                    EvaluationError::IndexOutOfRange(location, n)
                })?))
            } else if let Ok(string) = get_string(location, v) {
                Ok(value!(string.chars().nth(n).ok_or_else(|| {
                    EvaluationError::IndexOutOfRange(location, n)
                })?))
            } else {
                Err(EvaluationError::InvalidType(
                    location,
                    ExpectedType::ArrayOrString,
                    v[0].to_string(),
                ))
            }
        })
        .with_transform("range", |location: Location, v: &[Value]| {
            let start = get_argument_number(location, v, 0).unwrap_or(0f64) as usize;
            if let Ok(array) = get_array(location, v) {
                let end = get_argument_number(location, v, 1)
                    .map(|end| {
                        if end == -1f64 {
                            array.len() as f64
                        } else {
                            end
                        }
                    })
                    .unwrap_or(array.len() as f64) as usize;
                let step = get_argument_number(location, v, 2).unwrap_or(1f64) as usize;
                if end < start || end > array.len() {
                    return Err(EvaluationError::IndexOutOfRange(location, end));
                }
                let mut result = vec![];
                for i in (start..end).step_by(step) {
                    result.push(value!(array[i]));
                }
                Ok(value!(result))
            } else if let Ok(string) = get_string(location, v) {
                let end = get_argument_number(location, v, 1)
                    .map(|end| {
                        if end == -1f64 {
                            string.len() as f64
                        } else {
                            end
                        }
                    })
                    .unwrap_or(string.len() as f64) as usize;
                let step = get_argument_number(location, v, 2).unwrap_or(1f64) as usize;
                if end < start || end > string.len() {
                    return Err(EvaluationError::IndexOutOfRange(location, end));
                }
                let mut result = String::from("");
                for i in (start..end).step_by(step) {
                    result += &string.chars().nth(i).unwrap().to_string();
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
            let array = get_array(location, v)?;
            Ok(value!(array.first()))
        })
        .with_transform("last", |location: Location, v: &[Value]| {
            let array = get_array(location, v)?;
            Ok(value!(array.last()))
        })
        .with_transform("unique", |location: Location, v: &[Value]| {
            if let Ok(mut array) = get_array_strings(location, v) {
                unique(&mut array);
                Ok(value!(array))
            } else if let Ok(array) = get_array_numbers(location, v) {
                let mut str_array = array.iter().map(|n| n.to_string()).collect::<Vec<_>>();
                unique(&mut str_array);
                Ok(value!(str_array
                    .iter()
                    .map(|n| n.parse::<f64>().map_err(|_| {
                        EvaluationError::InvalidType(location, ExpectedType::Number, n.to_string())
                    }))
                    .collect::<Result<Vec<_>, _>>()?))
            } else {
                Ok(value!(get_array(location, v)?))
            }
        })
        .with_transform("uniqueByAttribute", |location: Location, v: &[Value]| {
            let array = get_array(location, v)?;
            let attribute = get_argument_string(location, v, 0)?;
            let mut unique_values = HashSet::new();
            let mut unique_objects = vec![];
            for object in array {
                let value = object.get(attribute).ok_or_else(|| {
                    EvaluationError::MissingAttribute(location, attribute.to_string())
                })?;
                let value = if let Some(value) = value.as_str() {
                    value.to_string()
                } else {
                    value
                        .as_f64()
                        .ok_or_else(|| {
                            EvaluationError::InvalidType(
                                location,
                                ExpectedType::Number,
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
        .with_transform("len", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(0));
            }
            if let Ok(array) = get_array(location, v) {
                Ok(value!(array.len()))
            } else if let Ok(string) = get_string(location, v) {
                Ok(value!(string.len()))
            } else {
                Err(EvaluationError::InvalidType(
                    location,
                    ExpectedType::ArrayOrString,
                    v[0].to_string(),
                ))
            }
        })
        .with_transform("pick", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!([]));
            }
            let name = get_argument_string(location, v, 0)?;
            let objects = get_array_objects(location, v)?;
            let data = objects
                .iter()
                .map(|object| object.get(name).unwrap_or(&Value::Null))
                .collect::<Vec<_>>();
            Ok(value!(data))
        })
        .with_transform("isBefore", |location: Location, v: &[Value]| {
            let date = get_number(location, v)?;
            let to_compare = get_argument_number(location, v, 0)?;
            Ok(value!(date < to_compare))
        })
        .with_transform("isAfter", |location: Location, v: &[Value]| {
            let date = get_number(location, v)?;
            let to_compare = get_argument_number(location, v, 0)?;
            Ok(value!(date > to_compare))
        })
        .with_transform("isOlderThan", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let date = get_string(location, v)?;
            let to_compare = get_argument_integer(location, v, 0)?;
            let period_type = get_argument_string(location, v, 1)?;
            let format = get_argument_string(location, v, 2)?;

            let format_descripion = format_description::parse(format)
                .map_err(|_| EvaluationError::DateFormatError(location, format.to_string()))?;
            let date = PrimitiveDateTime::parse(&date, &format_descripion)
                .map_err(|_| EvaluationError::DateParseError(location, date, format.to_string()))?;
            let duration = build_duration(location, to_compare, period_type)?;
            let now = OffsetDateTime::now_utc();

            Ok(value!(date.assume_utc().add(duration) < now))
        })
        .with_transform("isYoungerThan", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let date = get_string(location, v)?;
            let to_compare = get_argument_integer(location, v, 0)?;
            let period_type = get_argument_string(location, v, 1)?;
            let format = get_argument_string(location, v, 2)?;

            let format_descripion = format_description::parse(format)
                .map_err(|_| EvaluationError::DateFormatError(location, format.to_string()))?;
            let date = PrimitiveDateTime::parse(&date, &format_descripion)
                .map_err(|_| EvaluationError::DateParseError(location, date, format.to_string()))?;
            let duration = build_duration(location, to_compare, period_type)?;
            let now = OffsetDateTime::now_utc();

            Ok(value!(date.assume_utc().add(duration) > now))
        })
        .with_transform("toDate", |location: Location, v: &[Value]| {
            let date = get_string(location, v)?;
            let format = get_argument_string(location, v, 0)?;
            let format_descripion = format_description::parse(format)
                .map_err(|_| EvaluationError::DateFormatError(location, format.to_string()))?;
            let date = Date::parse(&date, &format_descripion)
                .map_err(|_| EvaluationError::DateParseError(location, date, format.to_string()))?;
            Ok(value!(date.midnight().assume_utc().unix_timestamp()))
        })
        .with_transform("toDateTime", |location: Location, v: &[Value]| {
            let date = get_string(location, v)?;
            let format = get_argument_string(location, v, 0)?;
            let format_descripion = format_description::parse(format)
                .map_err(|_| EvaluationError::DateFormatError(location, format.to_string()))?;
            let date = PrimitiveDateTime::parse(&date, &format_descripion)
                .map_err(|_| EvaluationError::DateParseError(location, date, format.to_string()))?;
            Ok(value!(date.assume_utc().unix_timestamp()))
        })
        .with_transform("sort", |location: Location, v: &[Value]| {
            let reverse = {
                if let Ok(order) = get_argument_number(location, v, 0) {
                    order == -1.0
                } else {
                    false
                }
            };
            let val = if let Ok(mut arr) = get_array_numbers(location, v) {
                arr.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                if reverse {
                    arr.reverse();
                }
                value!(arr)
            } else if let Ok(mut arr) = get_array_strings(location, v) {
                arr.sort_unstable();
                if reverse {
                    arr.reverse();
                }
                value!(arr)
            } else {
                return Err(EvaluationError::UnsortableType(location).into());
            };

            Ok(val)
        })
        .with_transform("sortByAttribute", |location: Location, v: &[Value]| {
            let by = get_argument_string(location, v, 0)?;
            let reverse = {
                if let Ok(order) = get_argument_number(location, v, 1) {
                    order == -1.0
                } else {
                    false
                }
            };
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
            let val = if let Ok(mut arr) = get_array_numbers(location, v) {
                arr.reverse();
                value!(arr)
            } else if let Ok(mut arr) = get_array_objects(location, v) {
                arr.reverse();
                value!(arr)
            } else if let Ok(mut arr) = get_array_strings(location, v) {
                arr.reverse();
                value!(arr)
            } else {
                return Err(EvaluationError::UnsortableType(location).into());
            };

            Ok(val)
        })
        .with_transform("flatten", |location: Location, v: &[Value]| {
            let array = get_array_arrays(location, v)?;
            Ok(value!(array.into_iter().flatten().collect::<Vec<_>>()))
        })
        .with_transform("contains", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            if let Ok(array) = get_array(location, v) {
                Ok(value!(array.contains(&v[1])))
            } else if let Ok(string) = get_string(location, v) {
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
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let s = get_string(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.starts_with(v)))
        })
        .with_transform("endsWith", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(value!(false));
            }
            let s = get_string(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.ends_with(v)))
        })
        .with_transform("toString", |_: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            Ok(value!(v[0].to_string()))
        })
        .with_transform("toInt", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            if let Some(v) = v[0].as_str() {
                Ok(value!(v
                    .parse::<f64>()
                    .map_err(|_| EvaluationError::FailedToInt(location))?
                    as i64))
            } else if let Some(v) = v[0].as_f64() {
                Ok(value!(v as i64))
            } else if let Some(v) = v[0].as_i64() {
                Ok(value!(v))
            } else {
                Err(EvaluationError::FailedToInt(location))
            }
        })
        .with_transform("toFloat", |location: Location, v: &[Value]| {
            if v[0] == Value::Null {
                return Ok(Value::Null);
            }
            if let Some(v) = v[0].as_str() {
                Ok(value!(v
                    .parse::<f64>()
                    .map_err(|_| EvaluationError::FailedToInt(location))?))
            } else if let Some(v) = v[0].as_f64() {
                Ok(value!(v))
            } else if let Some(v) = v[0].as_i64() {
                Ok(value!(v as f64))
            } else {
                Err(EvaluationError::FailedToInt(location))
            }
        })
        .with_transform("keys", |location: Location, v: &[Value]| {
            let o = get_object(location, v)?;
            Ok(value!(o.keys().map(|k| k.to_string()).collect::<Vec<_>>()))
        })
        .with_transform("has", |location: Location, v: &[Value]| {
            let o = get_object(location, v)?;
            let k = get_argument_string(location, v, 0)?;
            Ok(value!(o.contains_key(k)))
        })
        .with_transform("split", |location: Location, v: &[Value]| {
            let s = get_string(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s
                .split(v)
                .map(|s| s.to_string())
                .collect::<Vec<_>>()))
        })
        .with_transform("join", |location: Location, v: &[Value]| {
            let s = get_array_strings(location, v)?;
            let v = get_argument_string(location, v, 0)?;
            Ok(value!(s.join(v)))
        })
        .with_transform("trim", |location: Location, v: &[Value]| {
            let s = get_string(location, v)?;
            Ok(value!(s.trim().to_string()))
        })
        .with_transform("sqrt", |location: Location, v: &[Value]| {
            let v = get_number(location, v)?;
            Ok(value!(v.sqrt()))
        })
        .with_transform("abs", |location: Location, v: &[Value]| {
            let v = get_number(location, v)?;
            Ok(value!(v.abs()))
        })
        .with_transform("floor", |location: Location, v: &[Value]| {
            let v = get_number(location, v)?;
            Ok(value!(v.floor()))
        })
        .with_transform("ceil", |location: Location, v: &[Value]| {
            let v = get_number(location, v)?;
            Ok(value!(v.ceil()))
        })
        .with_transform("trunk", |location: Location, v: &[Value]| {
            let v = get_number(location, v)?;
            Ok(value!(v.trunc()))
        })
        .with_transform("push", |location: Location, v: &[Value]| {
            let mut array = get_array(location, v)?.clone();
            array.push(v[1].clone());
            Ok(value!(array))
        })
        .with_transform("concat", |location: Location, v: &[Value]| {
            let mut array = get_array(location, v)?.clone();
            for v in v[1..].iter() {
                if let Some(v) = v.as_array() {
                    array.extend(v.iter().cloned());
                } else {
                    array.push(v.clone());
                }
            }
            Ok(value!(array))
        })
        .with_transform("deburr", |location: Location, v: &[Value]| {
            let s = get_string(location, v)?;
            Ok(value!(diacritics::remove_diacritics(&s)))
        })
        .with_transform("indexOf", |location: Location, v: &[Value]| {
            let array = get_array(location, v)?;
            let a = get_argument(location, v, 0)?;
            let index = array
                .iter()
                .position(|e| e == a)
                .map(|i| i as i64)
                .unwrap_or(-1);
            Ok(value!(index))
        })
        .with_transform("replace", |location: Location, v: &[Value]| {
            let s = get_string(location, v)?;
            let replaced = get_argument_string(location, v, 0)?;
            let replacer = get_argument_string(location, v, 1)?;
            Ok(value!(s.replace(replaced, replacer)))
        })
}

#[cfg(test)]
mod tests {
    use jexl_eval::error::EvaluationError;
    use rstest::rstest;
    use serde_json::json as value;
    use serde_json::Value;

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
    #[case(r#" "2022-12-12T13:15:20" | toDateTime("[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(1670850920u64))]
    #[case(r#" "2022-12-12" | toDate("[year]-[month]-[day]") "#, value!(1670803200u64))]
    #[case(r#" "2032-12-12" | toDate("[year]-[month]-[day]") | isAfter($now)"#, value!(true))]
    #[case(r#" "2012-12-12" | toDate("[year]-[month]-[day]") | isBefore($now)"#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "days", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "weeks", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "months", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(true))]
    #[case(r#" "1900-01-01T00:00:00" | isOlderThan(10, "years", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(true))]
    #[case(r#" "2021-01-01T00:00:00" | isYoungerThan(100, "days", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(false))]
    #[case(r#" "2019-01-01T00:00:00" | isYoungerThan(100, "weeks", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(false))]
    #[case(r#" "2010-01-01T00:00:00" | isYoungerThan(100, "months", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(false))]
    #[case(r#" "1900-01-01T00:00:00" | isYoungerThan(100, "years", "[year]-[month]-[day]T[hour]:[minute]:[second]") "#, value!(false))]
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
    #[case(r#" 1 | toto "#, EvaluationError::UnknownTransform((1, 9), "toto".to_string(), vec![]))]
    #[case(r#" 23 + (1 | toto) "#, EvaluationError::UnknownTransform((7, 15), "toto".to_string(), vec![]))]
    #[case(r#" 1 | lowercase  "#, EvaluationError::InvalidType((1, 14), jexl_eval::error::ExpectedType::String, "1.0".to_string()))]
    #[case(r#" 2 + "toto" | toInt "#, EvaluationError::FailedToInt((5, 19)))]
    // #[case(r#"  "#, )]
    fn test_errors(#[case] input: String, #[case] error: EvaluationError) {
        test_eval_error(input, error);
    }
}
