use jexl_eval::Evaluator;
use serde_json::json;

#[test]
fn test_undefined_dot_property() {
    let evaluator = Evaluator::new();
    let context = json!({"customer": {}});
    let result = evaluator.eval_in_context("customer.email", &context);
    println!("{:?}", result);
    assert!(result.is_ok());
}
