# jexl-3000

jexl-3000 is a fork from [github.com/mozilla/jexl-rs](github.com/mozilla/jexl-rs) that adds:
- a set of transforms
- parsing cache
- map/reduce/filter and similar transforms, that take an expression as input

It does not aim to be compatible with the jexl specification.

## Usage

### From rust

Add `jexl-3000` to your dependencies:

```toml
[dependencies]
jexl-3000 = "0.0.1"
```

Import and use:

```rust
use jexl_3000::build_evaluator;
use serde_json::json;

fn main() {
    let evaluator = build_evaluator;
    let context = json!({
        "users": [
            {"name": "Bob", "age": 22},
            {"name": "Alicia", "age": 23},
            {"name": "Peter": "age": 19}
        ]
    });
    let res = evaluator.eval_in_context("{meanAge: users | pick('age') | mean | toInt, youngest: users | sortByAttribute('age') | first}", &context);
    assert_eq!(res, json!({"meanAge": 21, "youngest": "Peter"}))
}
```

### From node

Add `jexl-3000` to your package.json:

```bash
npm i jexl-3000
```

Import and use:

```js
const Evaluator = require('jexl-3000').Evaluator;
const evaluator = new Evaluator();
const context = {
    users: [
        {name: 'Bob', age: 22},
        {name: 'Alicia', age: 23},
        {name: 'Peter': age: 19},
    ]
};
const res = evaluator.evaluate("{meanAge: users | pick('age') | mean | toInt, youngest: users | sortByAttribute('age') | first}", context);
```

### From wasm

TODO: add doc