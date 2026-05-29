// Example demonstrating the new collection filtering functionality
use jexl_eval::Evaluator;
use serde_json::json;

fn main() {
    let evaluator = Evaluator::new();

    // Create a sample dataset
    let context = json!({
        "employees": [
            {"first": "Sterling", "last": "Archer", "age": 36, "department": "Field Ops"},
            {"first": "Malory", "last": "Archer", "age": 75, "department": "Management"},
            {"first": "Lana", "last": "Kane", "age": 33, "department": "Field Ops"},
            {"first": "Cyril", "last": "Figgis", "age": 45, "department": "Accounting"},
            {"first": "Cheryl", "last": "Tunt", "age": 28, "department": "HR"}
        ],
        "retireAge": 62
    });

    println!("=== Collection Filtering Examples ===\n");

    // Example 1: Simple property filter
    println!("1. Find employee named Sterling:");
    let result = evaluator
        .eval_in_context("employees[.first == 'Sterling']", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 2: Complex boolean expression
    println!("2. Find employees between 30 and 40:");
    let result = evaluator
        .eval_in_context("employees[.age >= 30 && .age < 40]", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 3: Using context variable
    println!("3. Find employees eligible for retirement (age >= retireAge):");
    let result = evaluator
        .eval_in_context("employees[.age >= retireAge]", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 4: Chained filters
    println!("4. Find Field Ops employees in their 30s:");
    let result = evaluator
        .eval_in_context("employees[.department == 'Field Ops'][.age >= 30]", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 5: Property access after filtering
    println!("5. Get first name of employee with last name 'Tunt':");
    let result = evaluator
        .eval_in_context("employees[.last == 'Tunt'].first", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 6: Expression in filter
    println!("6. Filter using computed string:");
    let result = evaluator
        .eval_in_context("employees[.last == 'Tu' + 'nt'].first", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 7: No matches
    println!("7. Filter with no matches:");
    let result = evaluator
        .eval_in_context("employees[.age > 100]", &context)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 8: Complex boolean logic
    println!("8. Complex filter: age > 30 AND (name is John OR Jane):");
    let context2 = json!({
        "people": [
            {"name": "John", "age": 35},
            {"name": "Jane", "age": 25},
            {"name": "Bob", "age": 45},
        ]
    });
    let result = evaluator
        .eval_in_context("people[.age > 30 && (.name == 'John' || .name == 'Jane')]", &context2)
        .unwrap();
    println!("   Result: {}\n", result);

    // Example 9: Nested property access
    println!("9. Filter by nested property:");
    let context3 = json!({
        "employees": [
            {"name": "Alice", "address": {"city": "New York"}},
            {"name": "Bob", "address": {"city": "Los Angeles"}},
        ]
    });
    let result = evaluator
        .eval_in_context("employees[.address.city == 'New York'].name", &context3)
        .unwrap();
    println!("   Result: {}\n", result);

    println!("=== All examples completed successfully! ===");
}
