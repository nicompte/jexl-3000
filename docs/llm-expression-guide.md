# jexl-3000 Expression Language

## Quick Reference

```
# Literals
42    3.14    true    false    null    'string'    "string"    /regex/
[1, 2, 3]    {key: value, "key2": value}

# Context access
identifier            obj.prop          obj[key]      arr[0]
users.name            # maps over array elements → ["Alice", "Bob"]

# Operators
+  -  *  /  //  %  ^      # arithmetic (// = floor divide, ^ = exponent)
==  !=  <  <=  >  >=      # comparison
&&  ||  !                  # logical (short-circuit)
in                         # 'x' in arr  or  'x' in 'foobar'
~                          # regex match → boolean
@                          # regex capture first match → [group1, group2, ...]
@+                         # regex capture all matches → [[g1, g2], ...]

# Conditional
cond ? a : b
if cond { a } else { b }

# Inline array filter  (.prop = property of current item)
array[.prop == value]
array[.active && .age > 18]

# Pipe / transform
value | transform
value | transform(arg1, arg2)

# Expression transforms  (this = current element, acc = accumulator in reduce)
array | map(this.x + 1)
array | filter(this.age >= 18)
array | sortBy(this.price)                  # ascending
array | sortBy(this.price, -1)              # descending
array | any(this.active == true)
array | all(this.done)
array | find(this.id == targetId)           # first match or null
array | findIndex(this.id == targetId)      # index or -1
array | reduce(0, acc + this)
value | apply(this.a + this.b)              # expression on a single value or whole array

# Named-transform shorthand (bare identifier → apply that transform to each element)
["HELLO"] | map(lowercase)                  # shorthand for map(this | lowercase)

# Special keywords
$now        # current local Unix timestamp
$now_utc    # current UTC Unix timestamp
```

---

## 1. Overview

jexl-3000 is an expression language that evaluates against a JSON **context** object.
An expression always returns a single JSON value (string, number, boolean, array, object, or null).

Key characteristics:

- Expressions are single-line or multi-line (whitespace is ignored)
- The pipe operator `|` chains transforms (left-to-right)
- Array/object literals can be constructed inline
- Comments use `#` (rest of line is ignored)
- Strings use single `'...'` or double `"..."` quotes
- All numbers are floats internally; `toInteger` truncates to integer when needed

---

## 2. Data Types & Literals

| Type    | Example                    |
| ------- | -------------------------- |
| Number  | `42`, `3.14`, `-7`         |
| String  | `'hello'`, `"world"`       |
| Boolean | `true`, `false`            |
| Null    | `null`                     |
| Array   | `[1, 2, 'three']`          |
| Object  | `{name: 'Alice', age: 30}` |
| Regex   | `/pattern/`                |

Object keys may be unquoted identifiers or quoted strings:

```
{foo: 1}     {"foo": 1}     {'foo': 1}
```

---

## 3. Operators

### Arithmetic

| Operator | Meaning      | Example  | Result |
| -------- | ------------ | -------- | ------ |
| `+`      | Add / Concat | `2 + 3`  | `5`    |
| `-`      | Subtract     | `5 - 2`  | `3`    |
| `*`      | Multiply     | `3 * 4`  | `12`   |
| `/`      | Divide       | `7 / 2`  | `3.5`  |
| `//`     | Floor divide | `7 // 2` | `3`    |
| `%`      | Modulus      | `7 % 3`  | `1`    |
| `^`      | Exponent     | `2 ^ 8`  | `256`  |

String concatenation uses `+`: `'Hello ' + 'World'` → `"Hello World"`

### Comparison

| Operator | Meaning          |
| -------- | ---------------- |
| `==`     | Equal            |
| `!=`     | Not equal        |
| `<`      | Less than        |
| `<=`     | Less or equal    |
| `>`      | Greater than     |
| `>=`     | Greater or equal |

### Logical

| Operator | Meaning | Short-circuits? |
| -------- | ------- | --------------- |
| `&&`     | And     | Yes             |
| `\|\|`   | Or      | Yes             |
| `!`      | Not     | N/A             |

### Membership

```
'bar' in 'foobartek'          # => true  (substring check)
'baz' in ['foo', 'bar']       # => false (array membership)
```

### Regex operators

```
'test' ~ /te*/                # => true  (match check, returns boolean)
'2024-01-15' @ /(\d{4})-(\d{2})-(\d{2})/   # => ["2024", "01", "15"]  (capture groups, first match)
'a,b,c' @+ /([a-z])/          # => [["a"], ["b"], ["c"]]  (capture groups, all matches)
```

### Operator Precedence (highest to lowest)

1. Unary: `!`, `-`, `+`
2. Exponent: `^`
3. Multiplicative: `*`, `/`, `//`, `%`
4. Additive: `+`, `-`
5. Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`, `in`, `~`, `@`, `@+`
6. Logical And: `&&`
7. Logical Or: `||`
8. Ternary / Conditional (lowest)

Use parentheses to override precedence: `(2 + 3) * 4` → `20`

---

## 4. Accessing the Context

The context is a flat or nested JSON object. Access values by name.

```
# Context: {"name": "Alice", "user": {"city": "Paris"}, "items": ["a", "b", "c"]}
name              # => "Alice"
user.city         # => "Paris"       (dot notation)
data[key]         # => dynamic key   (bracket notation)
items[0]          # => "a"           (array index)
```

When dot access is applied to an array, it maps over all elements:

```
# Context: {"users": [{"name": "Alice"}, {"name": "Bob"}]}
users.name        # => ["Alice", "Bob"]
```

---

## 5. Conditional Expressions

```
age >= 18 ? 'adult' : 'minor'
score > 90 ? 'A' : score > 75 ? 'B' : 'C'

if age >= 18 { 'adult' } else { 'minor' }   # alternative block syntax
```

Both forms are lazy: only the selected branch is evaluated.

---

## 6. Array Filters (inline bracket syntax)

Filter an array using `.property` (dot-prefixed) references inside `[...]`. The dot prefix refers to a **property of the current item**.

```
# Context: {"employees": [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]}

employees[.age >= 30]                          # => [{"name": "Alice", "age": 30}]
employees[.age >= 25 && .age < 30]             # => [{"name": "Bob", "age": 25}]
employees[.name == 'Alice'].age                # => 30  (chain property access after filter)
employees[.age >= 25][.name == 'Alice']        # => chained filters
employees[.age >= threshold]                   # reference context values
```

**Important**: `.property` syntax is ONLY valid inside `[...]` filter brackets.

---

## 7. The Pipe Operator and Transforms

The pipe `|` passes the left-hand value as the first argument to a transform function.
Pipes chain left-to-right.

```
value | transformName
value | transformName(arg1, arg2)
'Hello World' | lowercase | trim
users | sortByAttribute('age') | first
```

---

## 8. Expression Transforms (lambda-style)

These transforms take an **inline expression** where `this` refers to the current element.

### `map(expression)` — transform each element

```
[1, 2, 3] | map(this + 1)                          # => [2, 3, 4]
users | map({name: this.name, label: this.name | uppercase})
orders | map(this.price * this.quantity)

Note: inside element-level expression transforms (`map`, `filter`, `find`, `findIndex`, `any`, `all`, `sortBy`, etc.) a zero-based numeric variable `index` is available alongside `this`. Use `index` to refer to the current element's position in the array, for example:

```

["a", "b", "c"] | map(this + index) # => ["a0", "b1", "c2"]
[1,2,3] | filter(index % 2 == 0) # => [1, 3]

```

### Element-level expression variables

The `index` name is not a transformer — it's a contextual variable provided to inline element-level expressions. When writing expressions for transforms such as `map`, `filter`, `find`, `findIndex`, `any`, `all`, `sortBy` and similar, the following variables are available:

- `this`: the current element being evaluated.
- `index`: the zero-based numeric position of the current element within the array.
- `acc`: the accumulator value (available for `reduce`).

Examples:

```

["a", "b", "c"] | map(this + index) # => ["a0", "b1", "c2"]
[1,2,3] | filter(index % 2 == 0) # => [1, 3]

```

```

### `filter(expression)` — keep items where expression is truthy

```
[1, 2, 3, 4, 5] | filter(this > 3)                # => [4, 5]
users | filter(this.age >= 18)
users | filter(this.active == true && this.age > 21)
```

### `apply(expression)` — evaluate expression against a single value or whole array

`this` is the entire subject (not iterating). Works on objects, scalars, or a full array.

```
{a: 10, b: 20} | apply(this.a + this.b)            # => 30  (on an object)
[1, 2, 3] | apply(this | sum)                       # => 6   (on a whole array)
users | sortByAttribute('age') | first | apply({name: this.name, isAdult: this.age >= 18})
```

### `sortBy(expression)` — sort array by a computed key

```
products | sortBy(this.price)                       # ascending
products | sortBy(this.price, -1)                   # descending
```

### `any(expression)` — true if at least one element matches

```
[1, 2, 3] | any(this > 2)                          # => true
users | any(this.role == 'admin')
```

### `all(expression)` — true if all elements match

```
[2, 4, 6] | all(this % 2 == 0)                     # => true
orders | all(this.status == 'shipped')
```

### `find(expression)` — first matching element, or null

```
[{"id": 1}, {"id": 2}] | find(this.id == 2)        # => {"id": 2}
users | find(this.email == 'alice@example.com')     # => user object or null
```

### `findIndex(expression)` — index of first match, or -1

```
[{"id": 1}, {"id": 2}] | findIndex(this.id == 2)   # => 1
items | findIndex(this.sku == 'ABC123')             # => index or -1
```

### `reduce(initialValue, expression)` — fold array into a single value

`this` is the current element; `acc` is the accumulator.

```
[1, 2, 3, 4] | reduce(0, acc + this)               # => 10  (sum)
[1, 2, 3, 4] | reduce(1, acc * this)               # => 24  (product)
items | reduce([], this.active ? acc | push(this.name) : acc)
users | reduce(0, this.active ? acc + 1 : acc)      # count matching items
```

### Named-transform shorthand

When the argument to `map` or `filter` is a **bare identifier** (no `this`, no operators), it is treated as the name of a registered transform applied to each element.

```
# These two are equivalent:
["HELLO", "WORLD"] | map(lowercase)             # shorthand — bare identifier
["HELLO", "WORLD"] | map(this | lowercase)      # explicit — expression form

# Shorthand only works for plain transform names with no extra logic:
items | filter(isActive)                        # OK — bare identifier
items | filter(this.active == true)             # OK — expression
items | map(this.name | lowercase)              # OK — expression (not shorthand)
items | map(this | lowercase)                   # OK — expression (not shorthand)
```

---

## 9. Built-in Functions

### `$now` — current local time as Unix timestamp

```
$now
"2030-01-01" | toDate("[year]-[month]-[day]") > $now    # => true (date is in the future)
```

### `$now_utc` — current UTC time as Unix timestamp

```
$now_utc
"2030-01-01" | toDate("[year]-[month]-[day]") > $now_utc
```

---

## 10. Built-in Transforms (pipe syntax)

All transforms are invoked with `|`. Omit parentheses when there are no arguments.

### 10.1 String Transforms

| Transform                    | Description                                              | Example                                       |
| ---------------------------- | -------------------------------------------------------- | --------------------------------------------- |
| `lowercase`                  | Convert to lowercase                                     | `'Hello' \| lowercase` → `"hello"`            |
| `uppercase`                  | Convert to uppercase                                     | `'hello' \| uppercase` → `"HELLO"`            |
| `trim`                       | Remove leading/trailing whitespace                       | `' hi ' \| trim` → `"hi"`                     |
| `capitalize`                 | Uppercase first character only                           | `'hello' \| capitalize` → `"Hello"`           |
| `deburr`                     | Remove diacritics / accents                              | `'éàü' \| deburr` → `"eau"`                   |
| `contains(substring)`        | True if string contains substring                        | `'foobar' \| contains('oba')` → `true`        |
| `startsWith(prefix)`         | True if string starts with prefix                        | `'hello' \| startsWith('he')` → `true`        |
| `endsWith(suffix)`           | True if string ends with suffix                          | `'hello' \| endsWith('lo')` → `true`          |
| `split(separator)`           | Split string into array                                  | `'a,b,c' \| split(',')` → `["a","b","c"]`     |
| `replace(from, to)`          | Replace all occurrences                                  | `'hi Bob' \| replace('Bob', 'Alice')`         |
| `repeat(n)`                  | Repeat string n times                                    | `'ab' \| repeat(3)` → `"ababab"`              |
| `padStart(length, padChar?)` | Pad from left to reach target length (default pad `' '`) | `'5' \| padStart(3, '0')` → `"005"`           |
| `padEnd(length, padChar?)`   | Pad from right to reach target length                    | `'5' \| padEnd(3, '0')` → `"500"`             |
| `truncate(length, suffix?)`  | Truncate to length, appending suffix (default `"..."`)   | `'hello world' \| truncate(8)` → `"hello..."` |
| `get(index)`                 | Get character at index                                   | `'hello' \| get(1)` → `"e"`                   |
| `range(start?, end?, step?)` | Substring by index range                                 | `'hello' \| range(1, 3)` → `"el"`             |

Notes on `range`: `end` of `-1` means "to the end"; `step` defaults to 1; `range()` returns the full string.

### 10.2 Array Transforms

| Transform                    | Description                                            | Example                                         |
| ---------------------------- | ------------------------------------------------------ | ----------------------------------------------- |
| `size`                       | Array length (also works on strings; `0` for null)     | `[1,2,3] \| size` → `3`                         |
| `first`                      | First element                                          | `[1,2,3] \| first` → `1`                        |
| `last`                       | Last element                                           | `[1,2,3] \| last` → `3`                         |
| `get(index)`                 | Element at index                                       | `[10,20,30] \| get(2)` → `30`                   |
| `range(start?, end?, step?)` | Slice of array                                         | `[1,2,3,4,5] \| range(1, 3)` → `[2, 3]`         |
| `reverse`                    | Reverse array                                          | `[1,2,3] \| reverse` → `[3,2,1]`                |
| `flatten`                    | Flatten one level of nested arrays                     | `[[1,2],[3]] \| flatten` → `[1,2,3]`            |
| `unique`                     | Remove duplicate primitives                            | `[1,2,1,3] \| unique` → `[1,2,3]`               |
| `uniqueByAttribute(attr)`    | Remove objects with duplicate values of `attr`         | `items \| uniqueByAttribute('id')`              |
| `compact`                    | Remove null and false values                           | `[1, null, 2, false, 3] \| compact` → `[1,2,3]` |
| `push(value)`                | Append a value (returns new array)                     | `[1,2] \| push(3)` → `[1,2,3]`                  |
| `concat(array)`              | Concatenate with another array                         | `[1,2] \| concat([3,4])` → `[1,2,3,4]`          |
| `pick(attr)`                 | Extract one attribute from each object                 | `users \| pick('name')` → `["Alice","Bob"]`     |
| `join(separator)`            | Join array of strings                                  | `['a','b','c'] \| join(',')` → `"a,b,c"`        |
| `contains(value)`            | True if array contains value                           | `[1,2,3] \| contains(2)` → `true`               |
| `indexOf(value)`             | Index of first occurrence, or -1                       | `[1,2,3] \| indexOf(2)` → `1`                   |
| `sort`                       | Sort numbers or strings ascending                      | `[3,1,2] \| sort` → `[1,2,3]`                   |
| `sort(-1)`                   | Sort descending                                        | `[3,1,2] \| sort(-1)` → `[3,2,1]`               |
| `sortByAttribute(attr)`      | Sort objects by attribute ascending                    | `items \| sortByAttribute('name')`              |
| `sortByAttribute(attr, -1)`  | Sort objects by attribute descending                   | `items \| sortByAttribute('price', -1)`         |
| `sum`                        | Sum of numeric array                                   | `[1,2,3] \| sum` → `6`                          |
| `mean`                       | Average of numeric array                               | `[1,2,3] \| mean` → `2`                         |
| `min`                        | Minimum of numeric array                               | `[1,2,3] \| min` → `1`                          |
| `max`                        | Maximum of numeric array                               | `[1,2,3] \| max` → `3`                          |
| `minByAttribute(attr)`       | Object with minimum value of `attr`                    | `items \| minByAttribute('price')`              |
| `maxByAttribute(attr)`       | Object with maximum value of `attr`                    | `items \| maxByAttribute('score')`              |
| `groupBy(attr)`              | Group objects into a `{key: [items]}` object by `attr` | `items \| groupBy('category')`                  |
| `every(value)`               | True if all elements equal value                       | `[2,2,2] \| every(2)` → `true`                  |
| `some(value)`                | True if any element equals value                       | `[1,2,3] \| some(2)` → `true`                   |
| `sample`                     | Return a random element                                | `[1,2,3] \| sample`                             |
| `shuffle`                    | Return a shuffled copy                                 | `[1,2,3] \| shuffle`                            |

Notes on `range` for arrays: `range(1, -1)` means from index 1 to end; `range(0, -1, 2)` takes every other element.

### 10.2.1 New / Extended Array & Utility Transforms

| Transform               | Description                                           | Example                                            |
| ----------------------- | ----------------------------------------------------- | -------------------------------------------------- |
| `chunk(size)`           | Split array into chunks of given size                 | `[1,2,3,4,5] \| chunk(2)` → `[[1,2],[3,4],[5]]`    |
| `flattenDeep`           | Fully flatten nested arrays into a single array       | `[[1,[2]],3] \| flattenDeep` → `[1,2,3]`           |
| `flattenDepth(depth)`   | Flatten nested arrays up to `depth` levels            | `[[1,[2]],3] \| flattenDepth(1)` → `[1,[2],3]`     |
| `zip(...arrays)`        | Zip multiple arrays into an array of tuples           | `[1,2] \| zip(['a','b'])` → `[[1,'a'],[2,'b']]`    |
| `unzip(arrayOfArrays)`  | Reverse `zip` producing arrays of columns             | `[[1,'a'],[2,'b']] \| unzip` → `[[1,2],['a','b']]` |
| `difference(...arrays)` | Items in first array not present in subsequent arrays | `[1,2,3] \| difference([2,4])` → `[1,3]`           |
| `union(...arrays)`      | Unique values across arrays preserving order          | `[1,2] \| union([2,3])` → `[1,2,3]`                |
| `without(...values)`    | Return array excluding the provided values            | `[1,2,3] \| without(2)` → `[1,3]`                  |
| `sampleSize(n)`         | Return `n` random items from the array                | `[1,2,3,4] \| sampleSize(2)` → `[3,1]`             |

### 10.3 Object & Keyed Transforms (additions)

| Transform                   | Description                                                                                 | Example                                                 |
| --------------------------- | ------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| `keyBy(attr)`               | Build an object keyed by `attr` value from an array of objects                              | `users \| keyBy('id')` → `{ '42': {...}, '43': {...} }` |
| `invert`                    | Swap keys and values of an object (values stringified)                                      | `{a: '1', b: '2'} \| invert` → `{'1': 'a', '2': 'b'}`   |
| `pickBy(obj, attr, match?)` | Keep object entries whose nested `attr` matches `match` (or is truthy when `match` omitted) | `{k1: {active: true}} \| pickBy('active')`              |
| `omitBy(obj, attr, match?)` | Opposite of `pickBy`, remove entries matching attr/match                                    | `{k1: {active: false}} \| omitBy('active')`             |
| `set(path, value)`          | Set a dotted path in an object returning a new object                                       | `{a:{b:1}} \| set('a.c', 2)` → `{a:{b:1,c:2}}`          |
| `mergeDeep(obj...)`         | Deep-merge objects (right wins for scalar values)                                           | `{a:{x:1}} \| mergeDeep({a:{y:2}})` → `{a:{x:1,y:2}}`   |
| `countBy(attr?)`            | Count items grouped by attribute (or by item value when omitted)                            | `users \| countBy('role')` → `{admin:2,user:5}`         |

### 10.4 Misc / Comparison / Numeric Additions

| Transform                    | Description                                                                  | Example                              |
| ---------------------------- | ---------------------------------------------------------------------------- | ------------------------------------ |
| `isEqual(a, b)`              | Deep equality comparison between two values                                  | `obj1 \| isEqual(obj2)` → `true`     |
| `sumBy(attr?)`               | Sum numeric attribute values across an array (or numeric items when omitted) | `items \| sumBy('amount')` → `125.0` |
| `random(min?, max?, float?)` | Pseudo-random number generator; returns integer by default                   | `random(1,10)` → `7`                 |

### 10.5 Case transforms (strings)

| Transform   | Description                                                   | Example                                        |
| ----------- | ------------------------------------------------------------- | ---------------------------------------------- |
| `camelCase` | Convert a string to camelCase                                 | `'hello world' \| camelCase` → `'helloWorld'`  |
| `kebabCase` | Convert a string to kebab-case                                | `'hello world' \| kebabCase` → `'hello-world'` |
| `snakeCase` | Convert a string to snake_case                                | `'hello world' \| snakeCase` → `'hello_world'` |
| `startCase` | Convert a string to Start Case (space-separated, capitalized) | `'foo_bar' \| startCase` → `'Foo Bar'`         |

### 10.3 Object Transforms

| Transform        | Description                                | Example                                           |
| ---------------- | ------------------------------------------ | ------------------------------------------------- |
| `keys`           | Array of object keys                       | `{a:1, b:2} \| keys` → `["a","b"]`                |
| `values`         | Array of object values                     | `{a:1, b:2} \| values` → `[1,2]`                  |
| `entries`        | Array of `[key, value]` pairs              | `{a:1} \| entries` → `[ ["a",1] ]`                |
| `fromEntries`    | Construct object from `[key, value]` pairs | `[['a',1], ['b',2]] \| fromEntries` → `{a:1,b:2}` |
| `has(key)`       | True if object has key                     | `{a:1} \| has('a')` → `true`                      |
| `merge(object)`  | Merge another object (right side wins)     | `{a:1} \| merge({b:2})` → `{a:1, b:2}`            |
| `omit(key, ...)` | Return object without specified keys       | `{a:1,b:2,c:3} \| omit('b','c')` → `{a:1}`        |

### 10.4 Numeric Transforms

| Transform          | Description                             | Example                        |
| ------------------ | --------------------------------------- | ------------------------------ |
| `abs`              | Absolute value                          | `-3.7 \| abs` → `3.7`          |
| `floor`            | Round down to integer                   | `3.7 \| floor` → `3`           |
| `ceil`             | Round up to integer                     | `3.2 \| ceil` → `4`            |
| `trunk`            | Truncate decimal part                   | `3.9 \| trunk` → `3`           |
| `round`            | Round to nearest integer                | `3.5 \| round` → `4`           |
| `round(precision)` | Round to N decimal places               | `3.14159 \| round(2)` → `3.14` |
| `sqrt`             | Square root                             | `25 \| sqrt` → `5`             |
| `pow(exponent)`    | Raise to power                          | `2 \| pow(10)` → `1024`        |
| `log(base?)`       | Logarithm (natural log if base omitted) | `10 \| log(10)` → `1`          |
| `log10`            | Base-10 logarithm                       | `100 \| log10` → `2`           |
| `log2`             | Base-2 logarithm                        | `8 \| log2` → `3`              |
| `clamp(min, max)`  | Constrain number to range               | `5 \| clamp(1, 4)` → `4`       |
| `mod(divisor)`     | Modulo                                  | `10 \| mod(3)` → `1`           |

### 10.5 Type Conversion Transforms

| Transform   | Description                                         | Example                      |
| ----------- | --------------------------------------------------- | ---------------------------- |
| `toInteger` | Convert to integer (truncates float, parses string) | `'42' \| toInteger` → `42`   |
| `toFloat`   | Convert to float                                    | `'3.14' \| toFloat` → `3.14` |
| `toString`  | Convert to string                                   | `42 \| toString` → `"42.0"`  |

### 10.6 Type Inspection Transforms

| Transform                 | Description                                                                                       |
| ------------------------- | ------------------------------------------------------------------------------------------------- |
| `type`                    | Returns type name as string: `"null"`, `"boolean"`, `"number"`, `"string"`, `"array"`, `"object"` |
| `isNull`                  | True if value is null                                                                             |
| `isDefined`               | True if value is not null                                                                         |
| `isBoolean`               | True if value is boolean                                                                          |
| `isNumber`                | True if value is number                                                                           |
| `isString`                | True if value is string                                                                           |
| `isArray`                 | True if value is array                                                                            |
| `isObject`                | True if value is object                                                                           |
| `isEmpty`                 | True if null, `""`, `[]`, `{}`, or `false`                                                        |
| `coalesce(fallback, ...)` | Returns first non-null value from subject or arguments                                            |

```
nickname | coalesce(username, 'Anonymous')
# returns nickname if non-null, else username, else 'Anonymous'
```

### 10.7 Date Transforms

These transforms work with Unix timestamps (numbers) or date strings.

| Transform              | Description                                                                                                 |
| ---------------------- | ----------------------------------------------------------------------------------------------------------- |
| `toDate(format?)`      | Parse date string to Unix timestamp (format optional; common formats auto-detected)                         |
| `toDateTime(format?)`  | Parse datetime string to Unix timestamp (format optional; common formats auto-detected)                     |
| `age(format?)`         | Number of full years between subject date and now. Accepts Unix timestamp or date string (format optional). |
| `ageIn(unit, format?)` | Number of full units between subject date and now. Accepts Unix timestamp or date string (format optional). |

Date format strings follow the time-rs component syntax:

- `[year]` — 4-digit year
- `[month]` — 2-digit month
- `[day]` — 2-digit day
- `[hour]`, `[minute]`, `[second]`

Examples:

```
"2024-01-15" | toDate("[year]-[month]-[day]")
# => Unix timestamp (integer)

"2024-01-15T10:30:00" | toDateTime("[year]-[month]-[day]T[hour]:[minute]:[second]")
```

Duration units for `ageIn`: `"seconds"`, `"minutes"`, `"hours"`, `"days"`, `"weeks"`, `"months"`, `"years"`.

---

## 11. Null Behavior Reference

Different transforms handle null differently. Use this table to write null-safe expressions.

| Behavior         | Transforms / situations                                                         |
| ---------------- | ------------------------------------------------------------------------------- |
| Propagate `null` | `toString`, `toInteger`, `toFloat`, `age`, `ageIn` — null in → null out         |
| Return `0`       | `size` — null input returns 0                                                   |
| Return `[]`      | `pick` on null input, `filter`/`map`/`sort`/`sortBy`/`compact` on empty array   |
| Return `null`    | `find` when no match; `first`/`last` on empty array                             |
| Error on null    | Most string transforms (`lowercase`, `trim`, `split`, etc.) — guard with `\|\|` |

### Null-safe patterns

```
# Guard potentially undefined context keys
user.middleName || ''

# Coalesce chain
nickname | coalesce(username, 'Anonymous')

# Safe array operations — filter/map/sort always return [] on empty/null
items | filter(this.active) | size     # safe even if items is []

# Conditional null guard
user.address != null ? user.address.city : 'unknown'
```

---

## 12. Inline Object and Array Construction

You can construct new objects and arrays inline, referencing context values:

```
# Context: {"user": {"firstName": "Alice", "lastName": "Smith", "age": 28}}

{
  fullName: user.firstName + ' ' + user.lastName,
  isAdult: user.age >= 18
}
# => {"fullName": "Alice Smith", "isAdult": true}
```

With `map` or `apply`:

```
users | map({
  label: this.firstName + ' ' + this.lastName,
  age: this.age,
  senior: this.age >= 65
})
```

---

## 13. Chaining and Multi-line Expressions

Transforms chain left-to-right. Expressions can span multiple lines.
Use `#` for inline comments.

```
movies
  | filter(this.year >= 2000)           # only recent films
  | sortBy(this.rating, -1)             # best rated first
  | map({
      title: this.title,
      year: this.year,
      stars: this.rating | round(1)
    })
```

---

## 14. Complex Examples

### Example 1: Return the highest-rated item

```
# Context: {"products": [{"name": "A", "rating": 4.5}, {"name": "B", "rating": 4.9}]}

products | sortByAttribute('rating', -1) | first
# => {"name": "B", "rating": 4.9}

# Get just the name
products | sortByAttribute('rating', -1) | first | apply(this.name)
# => "B"
```

### Example 2: Filter, transform, and reshape

```
# Context: {"employees": [{"name": "Alice", "dept": "eng", "salary": 90000}, ...]}

employees
  | filter(this.dept == 'eng')
  | sortByAttribute('salary', -1)
  | map({name: this.name, salary: this.salary})
```

### Example 3: Compute statistics from an array

```
# Context: {"scores": [85, 92, 78, 95, 88]}

{
  mean: scores | mean | round(1),
  max: scores | max,
  min: scores | min,
  passing: scores | filter(this >= 80) | size
}
```

### Example 4: Reduce to build a custom result

```
# Context: {"items": [{"name": "a", "active": true}, {"name": "b", "active": false}, {"name": "c", "active": true}]}

items | reduce([], this.active ? acc | push(this.name) : acc)
# => ["a", "c"]
```

### Example 5: Regex capture and map

```
# Context: {"dates": ["2024-01-15", "2023-07-04"]}

dates | map(this @ /(\d{4})-(\d{2})-(\d{2})/)
# => [["2024","01","15"], ["2023","07","04"]]

# Get just the years
dates | map((this @ /(\d{4})-(\d{2})-(\d{2})/)[0])
# => ["2024", "2023"]
```

### Example 6: Group and aggregate

```
# Context: {"orders": [{"category": "food", "amount": 10}, {"category": "tech", "amount": 500}, {"category": "food", "amount": 25}]}

orders | groupBy('category')
# => {"food": [{...}, {...}], "tech": [{...}]}

# Total per category
{
  foodTotal: orders | filter(this.category == 'food') | pick('amount') | sum,
  techTotal: orders | filter(this.category == 'tech') | pick('amount') | sum
}
```

### Example 7: String manipulation pipeline

```
# Context: {"tags": ["  JavaScript  ", "  rust  ", "  WebAssembly  "]}

tags | map(this | trim | lowercase) | sort | join(', ')
# => "javascript, rust, webassembly"
```

### Example 8: Object reshaping with apply

```
# Context: {"movie": {"title": "Inception", "vote_average": 8.8, "revenue": 825532764}}

movie | apply({
  title: this.title,
  score: this.vote_average | round(1),
  revenue: '$' + (this.revenue | toString)
})
```

### Example 9: Complex pipeline combining many features

`apply` used mid-pipeline treats the array as a whole: `this` is the full array, not individual elements. This allows transforms like `range` and `size` to operate on the array itself before passing it on.

```
# Context: {"movies": [{title, language, vote_average, popularity, release_date, genre, revenue}, ...]}

{
  # Best rated movie as a summary
  best_rated: movies
    | sortByAttribute('vote_average', -1)
    | first
    | apply({title: this.title, score: this.vote_average}),

  # French movies — keep only the first half, extract titles
  # apply(this | range(...)) treats the filtered array as a whole (this = the array)
  french_titles: movies
    | filter(this.language == 'fr')
    | apply(this | range(0, (this | size) // 2))
    | pick('title'),

  # Count thrillers
  nb_thrillers: movies
    | filter(this.genre | contains('Thriller'))
    | size,

  # Titles not containing Latin characters
  non_latin: movies
    | reduce(
        [],
        !(this.title ~ /[a-zA-Z]/) ? acc | push(this.title) : acc
      )
}
```

---

## 15. Common Patterns and Idioms

### Null-safe fallback

```
user.nickname || user.name || 'Anonymous'
```

### Get first match from array

```
items | find(this.id == targetId)
```

### Check membership

```
'admin' in user.roles          # array membership
user.name | contains('Alice')  # substring check
```

### Building summaries over arrays

```
{
  total: orders | pick('amount') | sum,
  count: orders | size,
  average: orders | pick('amount') | mean | round(2),
  hasExpensive: orders | any(this.amount > 1000)
}
```

### Conditional field inclusion

```
{
  name: user.name,
  label: user.isPremium ? user.name + ' ★' : user.name
}
```

---

## 16. Error Behavior

- Accessing an undefined context key is an **error** (not null). Use `||` to guard: `user.middleName || ''`
- Using `.property` outside a filter bracket is an error.
- Applying a string transform to a non-string is an error (e.g. `42 | lowercase`).
- Applying `sortByAttribute` with a missing attribute key is an error.
- `toInteger` / `toFloat` on a non-numeric string is an error.
- Duplicate object keys in a literal are an error: `{a: 1, a: 2}` fails.
