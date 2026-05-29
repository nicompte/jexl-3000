# jexl-parser

A JEXL (JavaScript Expression Language) parser written in Rust using LALRPOP.

## Overview

`jexl-parser` is a Rust library that parses JEXL expressions into an Abstract Syntax Tree (AST). It provides a robust parser for the JEXL expression language with support for a wide range of operations including transforms, filters, conditionals, and more.

This crate is part of the [jexl-3000](https://github.com/mozilla/jexl-rs) project, which extends the original Mozilla JEXL implementation with additional features.

## Features

- **Comprehensive Expression Support**: Parse literals, identifiers, arrays, objects, and complex expressions
- **Binary Operations**: Arithmetic (`+`, `-`, `*`, `/`, `//`, `%`, `^`), comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`), and logical operators (`&&`, `||`)
- **Unary Operations**: Negation (`!`), plus (`+`), and minus (`-`)
- **Transforms**: Pipe expressions with optional arguments (`value | transform(arg1, arg2)`)
- **Collection Operations**: Map, filter, reduce, and other array transformations
- **Property Access**: Dot notation (`object.property`) and index access (`array[0]`)
- **Conditional Expressions**: Ternary operator (`condition ? truthy : falsy`) and if-else syntax
- **Regex Operations**: Pattern matching (`~`), capture (`@`), and multiple capture (`@+`)
- **Date/Time**: Date and datetime parsing with custom formats
- **Comments**: Support for `#` line comments

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
jexl-parser = "0.1.7"
```

## Usage

### Basic Parsing

```rust
use jexl_parser::Parser;

fn main() {
    // Parse a simple expression
    let result = Parser::parse("1 + 2").unwrap();
    println!("{:?}", result);

    // Parse a string literal
    let result = Parser::parse(r#""hello world""#).unwrap();
    println!("{:?}", result);
}
```

### Parsing Complex Expressions

```rust
use jexl_parser::Parser;

fn main() {
    // Array with transform
    let expr = "[1, 2, 3] | map(this * 2)";
    let result = Parser::parse(expr).unwrap();

    // Object property access
    let expr = "user.name";
    let result = Parser::parse(expr).unwrap();

    // Conditional expression
    let expr = "age >= 18 ? 'adult' : 'minor'";
    let result = Parser::parse(expr).unwrap();
}
```

### Working with the AST

The parser returns an `Expression` struct containing:

- `expression`: The parsed expression variant (`Expr` enum)
- `location`: Source location tuple `(start, end)`

```rust
use jexl_parser::{Parser, ast::{Expr, Expression}};

fn main() {
    let result = Parser::parse("1 + 2").unwrap();

    match result.expression {
        Expr::BinaryOperation { operation, left, right } => {
            println!("Binary operation: {:?}", operation);
            println!("Location: {:?}", result.location);
        }
        _ => {}
    }
}
```

## Expression Types

### Literals

- **Numbers**: `42`, `3.14`, `-10`, `.5`
- **Strings**: `"hello"`, `'world'` (with escape support)
- **Booleans**: `true`, `false`
- **Arrays**: `[1, 2, 3]`, `["a", "b"]`
- **Objects**: `{name: "John", age: 30}`, `{"key": "value"}`
- **Regex**: `/\w+/`, `/[0-9]+/`

### Operations

#### Binary Operations

```
1 + 2          # Addition
10 - 5         # Subtraction
3 * 4          # Multiplication
10 / 2         # Division
10 // 3        # Floor division
10 % 3         # Modulus
2 ^ 3          # Exponentiation
x == y         # Equality
x != y         # Inequality
x > y          # Greater than
x >= y         # Greater than or equal
x < y          # Less than
x <= y         # Less than or equal
x && y         # Logical AND
x || y         # Logical OR
x in [1,2,3]   # In operator
"hi" ~ /\w+/   # Regex match
"hi" @ /(\w+)/ # Regex capture
"hi" @+ /\w/   # Regex multiple captures
```

#### Unary Operations

```
!condition     # Logical NOT
-value         # Negation
+value         # Plus
```

### Transforms

Transforms allow you to pipe values through functions:

```
value | transform
value | transform(arg1, arg2)
users | filter(age > 18) | map(name)
```

### Expression Transforms

Special transforms that take expressions as arguments:

```
[1, 2, 3] | map(this * 2)
users | filter(this.age > 18)
items | sortBy(this.price)
numbers | any(this > 10)
numbers | all(this > 0)
items | find(this.id == 5)
items | findIndex(this.name == "John")
```

### Transform Variants

```
array | reduce(initialValue, accumulator + this)
```

### Property Access

```
object.property        # Dot notation
object["property"]     # Index notation
array[0]              # Array indexing
array[.item > 5]      # Filter syntax
```

### Conditionals

```
condition ? truthy : falsy
if condition { truthy } else { falsy }
```

### Date and Time

```
$now                  # Current timestamp
$now_utc              # Current timestamp UTC
```

### Comments

Comments start with `#` and continue to the end of the line:

```
# This is a comment
value + 1  # Inline comment
```

## AST Structure

The parser produces an AST with the following main types:

### `Expression`

```rust
pub struct Expression {
    pub expression: Expr,
    pub location: Location,  // (start_pos, end_pos)
}
```

### `Expr` Variants

- `Number(f64)`: Numeric literal
- `String(String)`: String literal
- `Boolean(bool)`: Boolean literal
- `Array(Vec<Box<Expression>>)`: Array literal
- `Object(Vec<(String, Box<Expression>)>)`: Object literal
- `Identifier(String)`: Variable reference
- `Regex(String)`: Regular expression
- `UnaryOperation`: Unary operator with operand
- `BinaryOperation`: Binary operator with left and right operands
- `Transform`: Transform operation with subject and optional arguments
- `DotOperation`: Property access
- `IndexOperation`: Array/object indexing
- `Conditional`: Ternary or if-else conditional
- `Filter`: Array filter with comparison
- `MapTransform`: Map with named transform
- `FilterTransform`: Filter with named transform
- `ExpressionTransform`: Map/filter/sortBy/any/all/find/findIndex with expression
- `ReduceExpression`: Reduce operation
- `Now`: Current timestamp
- `NowUtc`: Current UTC timestamp

### `OpCode` (Binary Operators)

- Arithmetic: `Add`, `Subtract`, `Multiply`, `Divide`, `FloorDivide`, `Modulus`, `Exponent`
- Comparison: `Equal`, `NotEqual`, `Less`, `LessEqual`, `Greater`, `GreaterEqual`
- Logical: `And`, `Or`
- Other: `In`, `Matches`, `Capture`, `CaptureMultiple`

### `UnCode` (Unary Operators)

- `Not`: Logical negation
- `Plus`: Unary plus
- `Minus`: Unary minus

## Error Handling

The parser returns `Result<Expression, ParseError>`. Parse errors include position information:

```rust
use jexl_parser::{Parser, ParseError};

match Parser::parse("invalid syntax @#$") {
    Ok(expr) => println!("Parsed: {:?}", expr),
    Err(e) => eprintln!("Parse error: {:?}", e),
}
```

## Examples

### Parsing a Transform Chain

```rust
use jexl_parser::Parser;

let expr = "users | filter(age > 18) | map(name) | sort";
let ast = Parser::parse(expr).unwrap();
```

### Parsing an Object Expression

```rust
use jexl_parser::Parser;

let expr = r#"{
    name: "John",
    age: 30,
    active: true
}"#;
let ast = Parser::parse(expr).unwrap();
```

### Parsing with Regex

```rust
use jexl_parser::Parser;

let expr = r#""hello123" @ /(\w+)(\d+)/"#;
let ast = Parser::parse(expr).unwrap();
```

## Development

### Building the Parser

The parser is generated from a LALRPOP grammar file (`src/parser.lalrpop`). When you build the crate, LALRPOP automatically generates the parser code.

```bash
cargo build
```

### Running Tests

```bash
cargo test
```

## License

This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

## Authors

- Mike Cooper <mythmon@gmail.com>
- The Sync Team <sync-team@mozilla.com>
- The Glean Team <glean-team@mozilla.com>

## Repository

https://github.com/mozilla/jexl-rs

## Related Crates

This parser is part of the jexl-3000 ecosystem:

- `jexl-eval`: Expression evaluator
- `jexl-3000`: High-level JEXL interface with transforms and caching
- `jexl-wasm`: WebAssembly bindings
- `jexl-node`: Node.js bindings
- `jexl-python`: Python bindings
