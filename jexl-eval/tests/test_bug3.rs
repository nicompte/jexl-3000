use jexl_eval::Evaluator;
use serde_json::json;

#[test]
fn test_order_items() {
    let evaluator = Evaluator::new();
    let context = json!({
      "customer": {
        "name": "John Doe",
        "age": 30,
        "email": "john@example.com",
        "tags": [
          "vip",
          "verified"
        ]
      },
      "order": {
        "id": 123,
        "amount": 99.99,
        "items": [
          {
            "name": "Widget",
            "price": 29.99
          },
          {
            "name": "Gadget",
            "price": 70
          }
        ]
      }
    });
    let result = evaluator.eval_in_context("order.items", &context).unwrap();
    assert_eq!(
        result,
        json!([
          {
            "name": "Widget",
            "price": 29.99
          },
          {
            "name": "Gadget",
            "price": 70
          }
        ])
    );
}
