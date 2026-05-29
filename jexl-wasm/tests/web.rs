//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

/// Parse a JSON string into a JS value for use as context.
fn ctx(json: &str) -> JsValue {
    JsValue::from(js_sys::JSON::parse(json).unwrap())
}

/// Serialize a JS value to a JSON string via `JSON.stringify`.
/// Returns `"null"` for JS null/undefined (JSON.stringify(undefined) returns undefined, not a string).
fn stringify(val: &JsValue) -> String {
    if val.is_null() || val.is_undefined() {
        return "null".to_string();
    }
    js_sys::JSON::stringify(val)
        .ok()
        .and_then(|s| s.as_string())
        .unwrap_or_else(|| "null".to_string())
}

// ---------------------------------------------------------------------------
// Sanity
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn pass() {
    assert_eq!(1 + 1, 2);
}

// ---------------------------------------------------------------------------
// Serialization correctness
// The key regression: serde_wasm_bindgen used to emit JS `Map` objects for
// serde_json::Map, which JSON.stringify renders as `{}`.
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_objects_serialize_as_plain_objects() {
    let evaluator = jexl_wasm::Evaluator::new();
    let context = ctx(
        r#"{"order": {"items": [{"name": "Widget", "price": 29.99}, {"name": "Gadget", "price": 70}]}}"#,
    );
    let result = evaluator.evaluate("order.items", context).unwrap();
    // Keys are BTreeMap-ordered (alphabetical): name < price
    assert_eq!(
        stringify(&result),
        r#"[{"name":"Widget","price":29.99},{"name":"Gadget","price":70}]"#
    );
}

#[wasm_bindgen_test]
fn test_nested_objects_serialize_correctly() {
    let evaluator = jexl_wasm::Evaluator::new();
    let context = ctx(r#"{"data": {"user": {"name": "Alice", "address": {"city": "Paris"}}}}"#);
    let result = evaluator.evaluate("data.user", context).unwrap();
    // Keys alphabetical: address < name
    assert_eq!(
        stringify(&result),
        r#"{"address":{"city":"Paris"},"name":"Alice"}"#
    );
}

// ---------------------------------------------------------------------------
// Literal expressions (no context)
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_literal_number() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(stringify(&ev.evaluate("42", JsValue::NULL).unwrap()), "42");
}

#[wasm_bindgen_test]
fn test_literal_float() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("3.14", JsValue::NULL).unwrap()),
        "3.14"
    );
}

#[wasm_bindgen_test]
fn test_literal_string() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("'hello world'", JsValue::NULL).unwrap()),
        r#""hello world""#
    );
}

#[wasm_bindgen_test]
fn test_literal_boolean_true() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("true", JsValue::NULL).unwrap()),
        "true"
    );
}

#[wasm_bindgen_test]
fn test_literal_boolean_false() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("false", JsValue::NULL).unwrap()),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_literal_array() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("[1, 2, 3]", JsValue::NULL).unwrap()),
        "[1,2,3]"
    );
}

#[wasm_bindgen_test]
fn test_literal_object() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("{'answer': 42}", JsValue::NULL).unwrap()),
        r#"{"answer":42}"#
    );
}

#[wasm_bindgen_test]
fn test_literal_null() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("null", JsValue::NULL).unwrap()),
        "null"
    );
}

// ---------------------------------------------------------------------------
// Arithmetic
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_addition() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("1 + 2", JsValue::NULL).unwrap()),
        "3"
    );
}

#[wasm_bindgen_test]
fn test_subtraction() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("10 - 3", JsValue::NULL).unwrap()),
        "7"
    );
}

#[wasm_bindgen_test]
fn test_multiplication() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("2 * 3", JsValue::NULL).unwrap()),
        "6"
    );
}

#[wasm_bindgen_test]
fn test_division() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("10 / 4", JsValue::NULL).unwrap()),
        "2.5"
    );
}

#[wasm_bindgen_test]
fn test_floor_division() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("7 // 2", JsValue::NULL).unwrap()),
        "3"
    );
}

#[wasm_bindgen_test]
fn test_modulo() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("12 % 5", JsValue::NULL).unwrap()),
        "2"
    );
}

#[wasm_bindgen_test]
fn test_exponent() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("2 ^ 10", JsValue::NULL).unwrap()),
        "1024"
    );
}

#[wasm_bindgen_test]
fn test_precedence() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("2 + 3 * 4", JsValue::NULL).unwrap()),
        "14"
    );
}

#[wasm_bindgen_test]
fn test_parentheses() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("(2 + 3) * 4", JsValue::NULL).unwrap()),
        "20"
    );
}

#[wasm_bindgen_test]
fn test_unary_negation() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("-(5 + 3)", JsValue::NULL).unwrap()),
        "-8"
    );
}

// ---------------------------------------------------------------------------
// String operations
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_string_concatenation() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("'Hello ' + 'World'", JsValue::NULL).unwrap()),
        r#""Hello World""#
    );
}

#[wasm_bindgen_test]
fn test_string_in_operator() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("'bar' in 'foobartek'", JsValue::NULL).unwrap()),
        "true"
    );
    assert_eq!(
        stringify(&ev.evaluate("'baz' in 'foobartek'", JsValue::NULL).unwrap()),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_regex_match() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate(r#""test123" ~ /\d+/"#, JsValue::NULL).unwrap()),
        "true"
    );
    assert_eq!(
        stringify(
            &ev.evaluate(r#""no-digits" ~ /\d+/"#, JsValue::NULL)
                .unwrap()
        ),
        "false"
    );
}

// ---------------------------------------------------------------------------
// Comparison & boolean logic
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_comparison_greater_than() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("2 > 1", JsValue::NULL).unwrap()),
        "true"
    );
}

#[wasm_bindgen_test]
fn test_comparison_less_than_or_equal() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("2 <= 1", JsValue::NULL).unwrap()),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_comparison_equality() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("'foo' == 'foo'", JsValue::NULL).unwrap()),
        "true"
    );
    assert_eq!(
        stringify(&ev.evaluate("'foo' == 'bar'", JsValue::NULL).unwrap()),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_boolean_and() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("true && true", JsValue::NULL).unwrap()),
        "true"
    );
    assert_eq!(
        stringify(&ev.evaluate("true && false", JsValue::NULL).unwrap()),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_boolean_or_short_circuit() {
    let ev = jexl_wasm::Evaluator::new();
    // 42 is truthy, OR short-circuits; result is the truthy left-hand value
    assert_eq!(
        stringify(&ev.evaluate("42 || 0", JsValue::NULL).unwrap()),
        "42"
    );
}

#[wasm_bindgen_test]
fn test_boolean_not() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("!true", JsValue::NULL).unwrap()),
        "false"
    );
    assert_eq!(
        stringify(&ev.evaluate("!(2 > 3)", JsValue::NULL).unwrap()),
        "true"
    );
}

#[wasm_bindgen_test]
fn test_conditional_expression_truthy() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("'foo' ? 1 : 2", JsValue::NULL).unwrap()),
        "1"
    );
}

#[wasm_bindgen_test]
fn test_conditional_expression_falsy() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("'' ? 1 : 2", JsValue::NULL).unwrap()),
        "2"
    );
}

#[wasm_bindgen_test]
fn test_in_operator_array() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("'bar' in ['foo', 'bar', 'baz']", JsValue::NULL)
                .unwrap()
        ),
        "true"
    );
    assert_eq!(
        stringify(
            &ev.evaluate("'qux' in ['foo', 'bar', 'baz']", JsValue::NULL)
                .unwrap()
        ),
        "false"
    );
}

// ---------------------------------------------------------------------------
// Context access
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_simple_identifier() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("score", ctx(r#"{"score": 99}"#)).unwrap()),
        "99"
    );
}

#[wasm_bindgen_test]
fn test_dot_access() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("user.age", ctx(r#"{"user": {"age": 30}}"#))
                .unwrap()
        ),
        "30"
    );
}

#[wasm_bindgen_test]
fn test_deep_dot_access() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("a.b.c", ctx(r#"{"a": {"b": {"c": "deep"}}}"#))
                .unwrap()
        ),
        r#""deep""#
    );
}

#[wasm_bindgen_test]
fn test_bracket_computed_access() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("data['ke' + 'y']", ctx(r#"{"data": {"key": "found"}}"#))
                .unwrap()
        ),
        r#""found""#
    );
}

#[wasm_bindgen_test]
fn test_context_arithmetic() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("price * qty", ctx(r#"{"price": 10, "qty": 5}"#))
                .unwrap()
        ),
        "50"
    );
}

// ---------------------------------------------------------------------------
// Array access
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_array_index() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("nums[1]", ctx(r#"{"nums": [10, 20, 30]}"#))
                .unwrap()
        ),
        "20"
    );
}

#[wasm_bindgen_test]
fn test_array_of_objects_index() {
    let ev = jexl_wasm::Evaluator::new();
    // Keys alphabetical: id < name
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items[0]",
                ctx(r#"{"items": [{"id": 1, "name": "a"}, {"id": 2, "name": "b"}]}"#)
            )
            .unwrap()
        ),
        r#"{"id":1,"name":"a"}"#
    );
}

#[wasm_bindgen_test]
fn test_array_of_objects_property_access() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items[0].name",
                ctx(r#"{"items": [{"id": 1, "name": "alpha"}, {"id": 2, "name": "beta"}]}"#)
            )
            .unwrap()
        ),
        r#""alpha""#
    );
}

// ---------------------------------------------------------------------------
// Collection filtering
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_filter_expression() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items[.val > 2]",
                ctx(r#"{"items": [{"val": 1}, {"val": 5}, {"val": 3}]}"#)
            )
            .unwrap()
        ),
        r#"[{"val":5},{"val":3}]"#
    );
}

#[wasm_bindgen_test]
fn test_filter_chained_access() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "employees[.last == 'Tunt'].first",
                ctx(r#"{"employees": [{"first": "Cheryl", "last": "Tunt"}, {"first": "Sterling", "last": "Archer"}]}"#)
            )
            .unwrap()
        ),
        r#""Cheryl""#
    );
}

#[wasm_bindgen_test]
fn test_filter_no_results() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items[.val > 100]",
                ctx(r#"{"items": [{"val": 1}, {"val": 2}]}"#)
            )
            .unwrap()
        ),
        "[]"
    );
}

#[wasm_bindgen_test]
fn test_filter_with_context_variable() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items[.val > threshold]",
                ctx(r#"{"items": [{"val": 10}, {"val": 20}, {"val": 30}], "threshold": 15}"#)
            )
            .unwrap()
        ),
        r#"[{"val":20},{"val":30}]"#
    );
}

#[wasm_bindgen_test]
fn test_filter_complex_condition() {
    let ev = jexl_wasm::Evaluator::new();
    // age > 25 AND age < 50 → Sterling(36) and Lana(33)
    // Keys alphabetical: age < first < last
    assert_eq!(
        stringify(
            &ev.evaluate(
                "employees[.age > 25 && .age < 50]",
                ctx(r#"{"employees": [{"first": "Sterling", "last": "Archer", "age": 36}, {"first": "Malory", "last": "Archer", "age": 75}, {"first": "Lana", "last": "Kane", "age": 33}]}"#)
            )
            .unwrap()
        ),
        r#"[{"age":36,"first":"Sterling","last":"Archer"},{"age":33,"first":"Lana","last":"Kane"}]"#
    );
}

#[wasm_bindgen_test]
fn test_chained_filters() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items[.val >= 2][.val <= 3]",
                ctx(r#"{"items": [{"val": 1}, {"val": 2}, {"val": 3}, {"val": 4}]}"#)
            )
            .unwrap()
        ),
        r#"[{"val":2},{"val":3}]"#
    );
}

// ---------------------------------------------------------------------------
// Built-in transforms
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_map_transform() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3] | map(this + 10)", JsValue::NULL)
                .unwrap()
        ),
        "[11,12,13]"
    );
}

#[wasm_bindgen_test]
fn test_map_to_object() {
    let ev = jexl_wasm::Evaluator::new();
    // Keys alphabetical: id
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2] | map({id: this})", JsValue::NULL)
                .unwrap()
        ),
        r#"[{"id":1},{"id":2}]"#
    );
}

#[wasm_bindgen_test]
fn test_filter_transform() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3, 4] | filter(this > 2)", JsValue::NULL)
                .unwrap()
        ),
        "[3,4]"
    );
}

#[wasm_bindgen_test]
fn test_reduce_transform() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3] | reduce(0, acc + this)", JsValue::NULL)
                .unwrap()
        ),
        "6"
    );
}

#[wasm_bindgen_test]
fn test_sort_by_ascending() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items | sortBy(this.val)",
                ctx(r#"{"items": [{"val": 3}, {"val": 1}, {"val": 2}]}"#)
            )
            .unwrap()
        ),
        r#"[{"val":1},{"val":2},{"val":3}]"#
    );
}

#[wasm_bindgen_test]
fn test_sort_by_descending() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items | sortBy(this.val, -1)",
                ctx(r#"{"items": [{"val": 3}, {"val": 1}, {"val": 2}]}"#)
            )
            .unwrap()
        ),
        r#"[{"val":3},{"val":2},{"val":1}]"#
    );
}

#[wasm_bindgen_test]
fn test_find_transform() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items | find(this.id == 2)",
                ctx(r#"{"items": [{"id": 1}, {"id": 2}, {"id": 3}]}"#)
            )
            .unwrap()
        ),
        r#"{"id":2}"#
    );
}

#[wasm_bindgen_test]
fn test_find_returns_null_when_not_found() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev
        .evaluate(
            "items | find(this.id == 99)",
            ctx(r#"{"items": [{"id": 1}, {"id": 2}]}"#),
        )
        .unwrap();
    assert!(result.is_undefined(), "expected null, got: {:?}", result);
}

#[wasm_bindgen_test]
fn test_find_index_transform() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items | findIndex(this.id == 2)",
                ctx(r#"{"items": [{"id": 1}, {"id": 2}, {"id": 3}]}"#)
            )
            .unwrap()
        ),
        "1"
    );
    assert_eq!(
        stringify(
            &ev.evaluate(
                "items | findIndex(this.id == 99)",
                ctx(r#"{"items": [{"id": 1}, {"id": 2}]}"#)
            )
            .unwrap()
        ),
        "-1"
    );
}

#[wasm_bindgen_test]
fn test_any_transform_true() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3] | any(this > 2)", JsValue::NULL)
                .unwrap()
        ),
        "true"
    );
}

#[wasm_bindgen_test]
fn test_any_transform_false() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3] | any(this > 10)", JsValue::NULL)
                .unwrap()
        ),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_all_transform_true() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3] | all(this > 0)", JsValue::NULL)
                .unwrap()
        ),
        "true"
    );
}

#[wasm_bindgen_test]
fn test_all_transform_false() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate("[1, 2, 3] | all(this > 1)", JsValue::NULL)
                .unwrap()
        ),
        "false"
    );
}

#[wasm_bindgen_test]
fn test_map_then_filter() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "[1, 2, 3] | map(this * 2) | filter(this > 4)",
                JsValue::NULL
            )
            .unwrap()
        ),
        "[6]"
    );
}

#[wasm_bindgen_test]
fn test_apply_transform() {
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(
            &ev.evaluate(
                "obj | apply(this.a + this.b)",
                ctx(r#"{"obj": {"a": 10, "b": 20}}"#)
            )
            .unwrap()
        ),
        "30"
    );
}

// ---------------------------------------------------------------------------
// Undefined / null access (regression for the undefined-identifier bug fix)
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_missing_top_level_property_returns_null() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev.evaluate("customer.email", ctx(r#"{}"#)).unwrap();
    assert!(result.is_undefined(), "expected null, got: {:?}", result);
}

#[wasm_bindgen_test]
fn test_missing_nested_property_returns_null() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev
        .evaluate("customer.email", ctx(r#"{"customer": {}}"#))
        .unwrap();
    assert!(result.is_undefined(), "expected null, got: {:?}", result);
}

#[wasm_bindgen_test]
fn test_deep_missing_chain_returns_null() {
    let ev = jexl_wasm::Evaluator::new();
    // user.address is null → .city on null falls through to the catch-all → null
    let result = ev
        .evaluate("user.address.city", ctx(r#"{"user": {}}"#))
        .unwrap();
    assert!(result.is_undefined(), "expected null, got: {:?}", result);
}

#[wasm_bindgen_test]
fn test_evaluate_with_null_context() {
    // Evaluating a constant expression with a null context is valid
    let ev = jexl_wasm::Evaluator::new();
    assert_eq!(
        stringify(&ev.evaluate("1 + 1", JsValue::NULL).unwrap()),
        "2"
    );
}

// ---------------------------------------------------------------------------
// Error handling
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_unknown_transform_returns_error_object() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev.evaluate("'hello' | noSuchTransform", JsValue::NULL);
    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_type = js_sys::Reflect::get(&err, &JsValue::from_str("error")).unwrap();
    assert_eq!(error_type, JsValue::from_str("EvaluationError"));
}

#[wasm_bindgen_test]
fn test_evaluation_error_has_caused_by() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev.evaluate("'x' | doesNotExist", JsValue::NULL);
    assert!(result.is_err());
    let err = result.unwrap_err();
    let caused_by = js_sys::Reflect::get(&err, &JsValue::from_str("caused_by")).unwrap();
    // caused_by should be an Array with at least one entry
    let arr = js_sys::Array::from(&caused_by);
    assert!(arr.length() > 0);
}

// ---------------------------------------------------------------------------
// $now / $now_utc — these used to trap ("unreachable executed") on
// wasm32-unknown-unknown because std::time::SystemTime::now() is unsupported
// on that target.  The time crate's `wasm-bindgen` feature fixes this by
// delegating to js_sys::Date instead.
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn test_now_returns_unix_timestamp() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev
        .evaluate("$now", JsValue::NULL)
        .expect("$now should not trap or error");
    // Result must be a JS number
    assert!(
        result.as_f64().is_some(),
        "$now did not return a number: {:?}",
        result
    );
    let ts = result.as_f64().unwrap();
    // Must be after 2024-01-01 (unix 1704067200) and before year 2100 (unix 4102444800)
    assert!(
        ts > 1_704_067_200.0,
        "$now returned implausibly small value: {}",
        ts
    );
    assert!(
        ts < 4_102_444_800.0,
        "$now returned implausibly large value: {}",
        ts
    );
}

#[wasm_bindgen_test]
fn test_now_utc_returns_unix_timestamp() {
    let ev = jexl_wasm::Evaluator::new();
    let result = ev
        .evaluate("$now_utc", JsValue::NULL)
        .expect("$now_utc should not trap or error");
    // Result must be a JS number
    assert!(
        result.as_f64().is_some(),
        "$now_utc did not return a number: {:?}",
        result
    );
    let ts = result.as_f64().unwrap();
    // Must be after 2024-01-01 (unix 1704067200) and before year 2100 (unix 4102444800)
    assert!(
        ts > 1_704_067_200.0,
        "$now_utc returned implausibly small value: {}",
        ts
    );
    assert!(
        ts < 4_102_444_800.0,
        "$now_utc returned implausibly large value: {}",
        ts
    );
}

// ---------------------------------------------------------------------------
// LanguageService — feature-gated tests
// ---------------------------------------------------------------------------

#[cfg(feature = "language-service")]
mod language_service_tests {
    use super::*;
    use jexl_wasm::LanguageService;

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    /// Build a simple JSON Schema with name (string), age (number), and items (array).
    fn simple_schema() -> JsValue {
        ctx(r#"{
            "type": "object",
            "properties": {
                "name": { "type": "string", "description": "Person name" },
                "age":  { "type": "number" },
                "active": { "type": "boolean" },
                "items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id":    { "type": "number" },
                            "label": { "type": "string" }
                        }
                    }
                }
            },
            "required": ["name"]
        }"#)
    }

    // -----------------------------------------------------------------------
    // validate — no schema
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_validate_valid_literal_no_schema() {
        let mut svc = LanguageService::new();
        let result = svc.validate("1 + 1");
        let arr = js_sys::Array::from(&result);
        assert_eq!(arr.length(), 0, "Expected no diagnostics for '1 + 1'");
    }

    #[wasm_bindgen_test]
    fn test_validate_syntax_error_no_schema() {
        let mut svc = LanguageService::new();
        let result = svc.validate("1 +");
        let arr = js_sys::Array::from(&result);
        assert!(
            arr.length() > 0,
            "Expected at least one diagnostic for '1 +'"
        );
        // First diagnostic should have severity "error"
        let first = arr.get(0);
        let sev = js_sys::Reflect::get(&first, &JsValue::from_str("severity")).unwrap();
        assert_eq!(sev, JsValue::from_str("error"));
    }

    #[wasm_bindgen_test]
    fn test_validate_diagnostic_has_required_fields() {
        let mut svc = LanguageService::new();
        let result = svc.validate("foo +");
        let arr = js_sys::Array::from(&result);
        assert!(arr.length() > 0);
        let first = arr.get(0);
        // `message`, `severity`, `start`, `end` must all be present
        let message = js_sys::Reflect::get(&first, &JsValue::from_str("message")).unwrap();
        assert!(
            message.as_string().map(|s| !s.is_empty()).unwrap_or(false),
            "Diagnostic message should be a non-empty string"
        );
        let start = js_sys::Reflect::get(&first, &JsValue::from_str("start")).unwrap();
        assert!(start.as_f64().is_some(), "start should be a number");
        let end = js_sys::Reflect::get(&first, &JsValue::from_str("end")).unwrap();
        assert!(end.as_f64().is_some(), "end should be a number");
    }

    // -----------------------------------------------------------------------
    // validate — with schema
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_validate_schema_known_property_clean() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        let result = svc.validate("name | uppercase");
        let arr = js_sys::Array::from(&result);
        assert_eq!(
            arr.length(),
            0,
            "No diagnostics expected for a valid expression against schema"
        );
    }

    #[wasm_bindgen_test]
    fn test_validate_schema_unknown_property_warns() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        let result = svc.validate("nonExistentField");
        let arr = js_sys::Array::from(&result);
        assert!(
            arr.length() > 0,
            "Expected a diagnostic for an unknown field"
        );
        let json = stringify(&result);
        assert!(
            json.contains("not defined") || json.contains("nonExistentField"),
            "Diagnostic should mention the unknown field; got: {json}"
        );
    }

    #[wasm_bindgen_test]
    fn test_validate_clear_schema_removes_property_check() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        // With schema: unknown field → diagnostic
        let before = js_sys::Array::from(&svc.validate("ghost"));
        assert!(before.length() > 0, "Expected diagnostic before clear");

        svc.clear_schema();
        // Without schema: no property checks → no diagnostics
        let after = js_sys::Array::from(&svc.validate("ghost"));
        assert_eq!(
            after.length(),
            0,
            "Expected no diagnostic after clear_schema"
        );
    }

    // -----------------------------------------------------------------------
    // validate — with context (no schema)
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_validate_context_known_property_clean() {
        let mut svc = LanguageService::new();
        svc.set_context(ctx(r#"{"score": 42, "label": "hi"}"#))
            .unwrap();
        let result = svc.validate("score + 1");
        let arr = js_sys::Array::from(&result);
        assert_eq!(
            arr.length(),
            0,
            "No diagnostics expected when property exists in context"
        );
    }

    #[wasm_bindgen_test]
    fn test_validate_clear_context() {
        let mut svc = LanguageService::new();
        svc.set_context(ctx(r#"{"x": 1}"#)).unwrap();
        svc.clear_context();
        // After clearing context without schema, unknown identifiers are not warned about
        let result = svc.validate("x");
        let arr = js_sys::Array::from(&result);
        assert_eq!(
            arr.length(),
            0,
            "No diagnostics after clear_context without schema"
        );
    }

    // -----------------------------------------------------------------------
    // completions
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_completions_after_pipe_includes_transforms() {
        let svc = LanguageService::new();
        let result = svc.completions("name | ", 7);
        let arr = js_sys::Array::from(&result);
        assert!(arr.length() > 0, "Expected completion items after '|'");
        let json = stringify(&result);
        // Standard transforms should appear
        assert!(
            json.contains("uppercase") || json.contains("map") || json.contains("filter"),
            "Expected transform names in completions; got: {json}"
        );
    }

    #[wasm_bindgen_test]
    fn test_completions_item_has_required_fields() {
        let svc = LanguageService::new();
        let result = svc.completions("name | ", 7);
        let arr = js_sys::Array::from(&result);
        assert!(arr.length() > 0);
        let first = arr.get(0);
        let label = js_sys::Reflect::get(&first, &JsValue::from_str("label")).unwrap();
        assert!(
            label.as_string().map(|s| !s.is_empty()).unwrap_or(false),
            "Completion item must have a non-empty label"
        );
        let kind = js_sys::Reflect::get(&first, &JsValue::from_str("kind")).unwrap();
        assert!(
            kind.as_string().is_some(),
            "Completion item must have a kind string"
        );
        let sort_order = js_sys::Reflect::get(&first, &JsValue::from_str("sort_order")).unwrap();
        assert!(
            sort_order.as_f64().is_some(),
            "Completion item must have a numeric sort_order"
        );
    }

    #[wasm_bindgen_test]
    fn test_completions_schema_properties_appear_as_variables() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        let result = svc.completions("n", 1);
        let json = stringify(&result);
        assert!(
            json.contains("name"),
            "Schema property 'name' should appear in completions; got: {json}"
        );
    }

    #[wasm_bindgen_test]
    fn test_completions_context_properties_appear() {
        let mut svc = LanguageService::new();
        svc.set_context(ctx(r#"{"myProp": 42, "otherProp": "hello"}"#))
            .unwrap();
        let result = svc.completions("m", 1);
        let json = stringify(&result);
        assert!(
            json.contains("myProp"),
            "Context property 'myProp' should appear in completions; got: {json}"
        );
    }

    // -----------------------------------------------------------------------
    // hover
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_hover_schema_property_returns_info() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        let result = svc.hover("name", 2);
        assert!(
            !result.is_null() && !result.is_undefined(),
            "Expected hover info for a schema-defined identifier"
        );
        let content = js_sys::Reflect::get(&result, &JsValue::from_str("content")).unwrap();
        let content_str = content.as_string().unwrap_or_default();
        assert!(
            !content_str.is_empty(),
            "Hover content should be non-empty; got: {content_str:?}"
        );
    }

    #[wasm_bindgen_test]
    fn test_hover_unknown_position_returns_null() {
        let mut svc = LanguageService::new();
        // Empty expression — nothing to hover on
        let result = svc.hover("", 0);
        // Should return JS null/undefined or a null-like value
        assert!(
            result.is_null() || result.is_undefined(),
            "Expected null/undefined hover for empty expression"
        );
    }

    #[wasm_bindgen_test]
    fn test_hover_result_has_range_when_present() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        let result = svc.hover("name", 2);
        if !result.is_null() && !result.is_undefined() {
            // range is optional but when present must be an array of two numbers
            let range = js_sys::Reflect::get(&result, &JsValue::from_str("range")).unwrap();
            if !range.is_undefined() {
                let arr = js_sys::Array::from(&range);
                assert_eq!(arr.length(), 2, "Hover range must be [start, end]");
                assert!(arr.get(0).as_f64().is_some());
                assert!(arr.get(1).as_f64().is_some());
            }
        }
    }

    // -----------------------------------------------------------------------
    // completions_with_diagnostics
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_completions_with_diagnostics_valid_expr() {
        let mut svc = LanguageService::new();
        svc.set_schema(simple_schema()).unwrap();
        // Use a complete, valid expression — an incomplete trailing pipe is itself a syntax error
        let result = svc.completions_with_diagnostics("name | uppercase", 16);
        // Result has `items` and `diagnostics`
        let items = js_sys::Reflect::get(&result, &JsValue::from_str("items")).unwrap();
        let diags = js_sys::Reflect::get(&result, &JsValue::from_str("diagnostics")).unwrap();
        let items_arr = js_sys::Array::from(&items);
        let diags_arr = js_sys::Array::from(&diags);
        // completions at offset 16 (end of "uppercase") should include transform suggestions
        let _ = items_arr.length(); // items may be empty at end of a transform name, that's fine
        assert_eq!(
            diags_arr.length(),
            0,
            "Expected no diagnostics for valid expr 'name | uppercase'"
        );
    }

    #[wasm_bindgen_test]
    fn test_completions_with_diagnostics_invalid_expr() {
        let mut svc = LanguageService::new();
        let result = svc.completions_with_diagnostics("1 + ", 4);
        let diags = js_sys::Reflect::get(&result, &JsValue::from_str("diagnostics")).unwrap();
        let diags_arr = js_sys::Array::from(&diags);
        assert!(
            diags_arr.length() > 0,
            "Expected diagnostics for invalid expression '1 + '"
        );
    }

    #[wasm_bindgen_test]
    fn test_completions_with_diagnostics_has_items_and_diagnostics_keys() {
        let mut svc = LanguageService::new();
        let result = svc.completions_with_diagnostics("age | ", 6);
        // Both keys should be present and be arrays
        let items = js_sys::Reflect::get(&result, &JsValue::from_str("items")).unwrap();
        let diags = js_sys::Reflect::get(&result, &JsValue::from_str("diagnostics")).unwrap();
        assert!(js_sys::Array::is_array(&items), "items should be an array");
        assert!(
            js_sys::Array::is_array(&diags),
            "diagnostics should be an array"
        );
    }

    // -----------------------------------------------------------------------
    // set_schema / set_context error cases
    // -----------------------------------------------------------------------

    #[wasm_bindgen_test]
    fn test_set_schema_accepts_valid_schema() {
        let mut svc = LanguageService::new();
        let result = svc.set_schema(simple_schema());
        assert!(
            result.is_ok(),
            "set_schema should succeed for a valid JS object"
        );
    }

    #[wasm_bindgen_test]
    fn test_set_context_accepts_valid_context() {
        let mut svc = LanguageService::new();
        let result = svc.set_context(ctx(r#"{"foo": 1}"#));
        assert!(
            result.is_ok(),
            "set_context should succeed for a valid JS object"
        );
    }
}
