// Documentation data for the jexl-3000 expression language.
// Each entry has a unique id (used for anchor links), a category, and rich docs.

export interface DocExample {
  /** Code snippet */
  code: string;
  /** Expected output (optional) */
  result?: string;
  /** Short description of the example */
  label?: string;
}

export interface DocEntry {
  /** Unique identifier, used as URL anchor */
  id: string;
  /** Display name */
  name: string;
  /** Category for grouping */
  category: DocCategory;
  /** Short one-line summary */
  summary: string;
  /** Longer description (supports basic markdown) */
  description: string;
  /** Expected input type(s), if applicable */
  inputType?: string;
  /** Output type, if applicable */
  outputType?: string;
  /** Syntax/signature */
  syntax?: string;
  /** Usage examples */
  examples: DocExample[];
}

export type DocCategory =
  | "syntax"
  | "operator"
  | "string-transform"
  | "number-transform"
  | "array-transform"
  | "object-transform"
  | "type-transform"
  | "date-transform"
  | "expression-transform"
  | "function"
  | "pattern";

export interface DocCategoryInfo {
  id: DocCategory;
  label: string;
  icon: string;
}

export const DOC_CATEGORIES: DocCategoryInfo[] = [
  { id: "syntax", label: "Syntax", icon: "📝" },
  { id: "operator", label: "Operators", icon: "⚙️" },
  { id: "expression-transform", label: "Expression Transforms", icon: "λ" },
  { id: "string-transform", label: "String Transforms", icon: "🔤" },
  { id: "number-transform", label: "Number Transforms", icon: "🔢" },
  { id: "array-transform", label: "Array Transforms", icon: "📋" },
  { id: "object-transform", label: "Object Transforms", icon: "🗂️" },
  { id: "type-transform", label: "Type Transforms", icon: "🏷️" },
  { id: "date-transform", label: "Date Transforms", icon: "📅" },
  { id: "function", label: "Built-in Functions", icon: "ƒ" },
  { id: "pattern", label: "Common Patterns", icon: "💡" },
];

// ── Documentation Entries ─────────────────────────────────────────────────────

export const DOC_ENTRIES: DocEntry[] = [
  // ══════════════════════════════════════════════════════════════════════════
  // SYNTAX
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "literals",
    name: "Literals",
    category: "syntax",
    summary:
      "Number, string, boolean, null, array, object, and regex literals.",
    description:
      "jexl-3000 supports numeric literals (integers and floats), single- or double-quoted strings, booleans (`true`/`false`), `null`, arrays, objects, and regex patterns.",
    examples: [
      { code: "42", result: "42" },
      { code: "'hello'", result: '"hello"' },
      { code: "[1, 2, 'three']", result: '[1, 2, "three"]' },
      {
        code: "{name: 'Alice', age: 30}",
        result: '{"name": "Alice", "age": 30}',
      },
      { code: "/pattern/", label: "Regex literal" },
    ],
  },
  {
    id: "context-access",
    name: "Context Access",
    category: "syntax",
    summary: "Access JSON context values via dot & bracket notation.",
    description:
      "The context is the JSON object the expression evaluates against. Use identifiers for top-level keys, dot notation for nested access, brackets for dynamic keys, and numeric indices for arrays.",
    syntax: "identifier\nobj.prop\nobj[key]\narr[0]",
    examples: [
      { code: "name", result: '"Alice"', label: 'Context: {"name": "Alice"}' },
      {
        code: "user.city",
        result: '"Paris"',
        label: 'Context: {"user": {"city": "Paris"}}',
      },
      { code: "data[key]", result: "1", label: "Dynamic key access" },
      { code: "items[0]", result: '"a"', label: "Array index" },
      {
        code: "users.name",
        result: '["Alice", "Bob"]',
        label: "Property access across arrays",
      },
    ],
  },
  {
    id: "pipe",
    name: "Pipe Operator",
    category: "syntax",
    summary: "Chain transforms with the | operator.",
    description:
      "The pipe `|` passes the left-hand value as input to a transform function on the right. Pipes chain left-to-right, and expressions can span multiple lines.",
    syntax: "value | transform\nvalue | transform(arg1, arg2)",
    examples: [
      { code: "'Hello' | lowercase", result: '"hello"' },
      { code: "'Hello World' | lowercase | trim", result: '"hello world"' },
      { code: "[3, 1, 2] | sort | first", result: "1" },
    ],
  },
  {
    id: "ternary",
    name: "Conditional / Ternary",
    category: "syntax",
    summary: "Conditional expressions using ternary or if/else syntax.",
    description:
      "Use `condition ? value_if_true : value_if_false` or `if condition { value_if_true } else { value_if_false }`. Both forms are lazy — only the selected branch is evaluated.",
    syntax: "cond ? a : b\nif cond { a } else { b }",
    examples: [
      {
        code: "age >= 18 ? 'adult' : 'minor'",
        result: '"adult"',
        label: "Ternary",
      },
      {
        code: "if age >= 18 { 'adult' } else { 'minor' }",
        result: '"adult"',
        label: "If/else block",
      },
      {
        code: "score > 90 ? 'A' : score > 75 ? 'B' : 'C'",
        label: "Nested ternary",
      },
    ],
  },
  {
    id: "array-filter",
    name: "Array Filters",
    category: "syntax",
    summary: "Filter arrays inline using [.property] bracket syntax.",
    description:
      "Filter an array inline using `.property` (dot-prefixed) references inside `[...]`. The dot prefix refers to a property of the current item. `.property` syntax is only valid inside filter brackets.",
    syntax: "array[.prop == value]\narray[.prop >= value && .prop2 < value2]",
    examples: [
      { code: "employees[.age >= 30]", label: "Filter by age" },
      { code: "employees[.name == 'Bob']", label: "Filter by name" },
      {
        code: "employees[.age >= 25 && .age < 30]",
        label: "Multiple conditions",
      },
      {
        code: "employees[.name == 'Alice'].age",
        result: "30",
        label: "Filter + property access",
      },
    ],
  },
  {
    id: "comments",
    name: "Comments",
    category: "syntax",
    summary: "Use # for inline comments.",
    description:
      "Comments start with `#` and continue to the end of the line. They are ignored during evaluation.",
    syntax: "# This is a comment",
    examples: [{ code: "age + 1 # increment age", result: "31" }],
  },
  {
    id: "inline-construction",
    name: "Inline Object/Array Construction",
    category: "syntax",
    summary: "Build objects and arrays inline referencing context values.",
    description:
      "Construct new objects and arrays inline, referencing context values. Object keys can be unquoted identifiers or quoted strings. Especially useful with `map` or `apply`.",
    examples: [
      {
        code: "{\n  fullName: user.firstName + ' ' + user.lastName,\n  isAdult: user.age >= 18\n}",
        label: "Inline object from context",
      },
      {
        code: "users | map({\n  label: this.firstName,\n  senior: this.age >= 65\n})",
        label: "Object construction inside map",
      },
    ],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // OPERATORS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "arithmetic",
    name: "Arithmetic Operators",
    category: "operator",
    summary:
      "Add, subtract, multiply, divide, floor divide, modulus, exponent.",
    description:
      "`+` (add/concat), `-` (subtract), `*` (multiply), `/` (divide), `//` (floor divide), `%` (modulus), `^` (exponent). `+` also performs string concatenation.",
    examples: [
      { code: "2 + 3", result: "5" },
      { code: "7 // 2", result: "3", label: "Floor division" },
      { code: "2 ^ 8", result: "256", label: "Exponent" },
      {
        code: "'Hello ' + 'World'",
        result: '"Hello World"',
        label: "String concat",
      },
    ],
  },
  {
    id: "comparison",
    name: "Comparison Operators",
    category: "operator",
    summary: "==, !=, <, <=, >, >= for comparing values.",
    description:
      "Standard comparison operators: `==` (equal), `!=` (not equal), `<` (less), `<=` (less or equal), `>` (greater), `>=` (greater or equal).",
    examples: [
      { code: "5 == 5", result: "true" },
      { code: "5 != 3", result: "true" },
      { code: "age >= 18", label: "Compare against context" },
    ],
  },
  {
    id: "logical",
    name: "Logical Operators",
    category: "operator",
    summary: "&&, ||, ! — with short-circuit evaluation.",
    description:
      "`&&` (and), `||` (or), `!` (not). `&&` and `||` short-circuit: the right side is only evaluated if needed. `||` is commonly used as a null/falsy fallback.",
    examples: [
      { code: "true && false", result: "false" },
      { code: "false || 'fallback'", result: '"fallback"' },
      { code: "!true", result: "false" },
      {
        code: "user.nickname || user.name || 'Anonymous'",
        label: "Null-safe fallback",
      },
    ],
  },
  {
    id: "membership",
    name: "Membership Operator",
    category: "operator",
    summary: "'in' operator for substring and array membership checks.",
    description:
      "The `in` operator checks for substring presence in strings and element membership in arrays.",
    examples: [
      {
        code: "'bar' in 'foobartek'",
        result: "true",
        label: "Substring check",
      },
      {
        code: "'baz' in ['foo', 'bar']",
        result: "false",
        label: "Array membership",
      },
    ],
  },
  {
    id: "regex-operators",
    name: "Regex Operators",
    category: "operator",
    summary: "~, @, @+ for regex matching and capture groups.",
    description:
      "`~` tests for a match (returns boolean). `@` extracts capture groups from the first match. `@+` extracts capture groups from all matches.",
    examples: [
      { code: "'test' ~ /te*/", result: "true", label: "Match check" },
      {
        code: "'2024-01-15' @ /(\\d{4})-(\\d{2})-(\\d{2})/",
        result: '["2024", "01", "15"]',
        label: "Capture groups",
      },
      {
        code: "'a,b,c' @+ /([a-z])/",
        result: '[["a"], ["b"], ["c"]]',
        label: "All matches",
      },
    ],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // EXPRESSION TRANSFORMS (lambda-style)
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "map",
    name: "map",
    category: "expression-transform",
    summary: "Transform each element of an array using an expression.",
    description:
      "Evaluates the expression for each item. `this` refers to the current item. Returns a new array with the transformed values.",
    inputType: "array",
    outputType: "array",
    syntax: "array | map(expression)",
    examples: [
      { code: "[1, 2, 3] | map(this + 1)", result: "[2, 3, 4]" },
      { code: "[1, 2, 3] | map(this * this)", result: "[1, 4, 9]" },
      {
        code: "users | map({name: this.name, label: this.name | uppercase})",
        label: "Build objects",
      },
      { code: '["HELLO", "WORLD"] | map(lowercase)', result: '["hello", "world"]', label: 'Shorthand map(transformer)' },
    ],
  },
  {
    id: "filter",
    name: "filter",
    category: "expression-transform",
    summary: "Keep only elements where the expression is truthy.",
    description:
      "Evaluates the expression for each item. `this` refers to the current item. Keeps items where the expression evaluates to a truthy value.",
    inputType: "array",
    outputType: "array",
    syntax: "array | filter(expression)",
    examples: [
      { code: "[1, 2, 3, 4, 5] | filter(this > 3)", result: "[4, 5]" },
      { code: "users | filter(this.age >= 18)", label: "Filter adults" },
      {
        code: "users | filter(this.active == true && this.age > 21)",
        label: "Multiple conditions",
      },
      { code: 'items | filter(isActive)', label: 'Shorthand filter(NAME)' },
    ],
  },
  {
    id: "reduce",
    name: "reduce",
    category: "expression-transform",
    summary: "Fold an array into a single value using an accumulator.",
    description:
      "Reduces an array to a single value. `this` is the current element, `acc` is the accumulator. The first argument is the initial accumulator value.",
    inputType: "array",
    outputType: "any",
    syntax: "array | reduce(initialValue, expression)",
    examples: [
      {
        code: "[1, 2, 3, 4] | reduce(0, acc + this)",
        result: "10",
        label: "Sum",
      },
      {
        code: "[1, 2, 3, 4] | reduce(1, acc * this)",
        result: "24",
        label: "Product",
      },
      {
        code: "items | reduce([], this.active ? acc | push(this.name) : acc)",
        label: "Conditional accumulation",
      },
    ],
  },
  {
    id: "apply",
    name: "apply",
    category: "expression-transform",
    summary: "Apply an expression to a single value (not iterating).",
    description:
      "`this` is the entire subject. Useful for transforming a single object or computing a derived value from it.",
    inputType: "any",
    outputType: "any",
    syntax: "value | apply(expression)",
    examples: [
      { code: "{a: 10, b: 20} | apply(this.a + this.b)", result: "30" },
      {
        code: "user | apply({name: this.name, isAdult: this.age >= 18})",
        label: "Reshape object",
      },
    ],
  },
  {
    id: "sortBy",
    name: "sortBy",
    category: "expression-transform",
    summary: "Sort array by a computed key expression.",
    description:
      "Sorts an array by the value returned by the expression. Ascending by default. Pass `-1` as the second argument for descending.",
    inputType: "array",
    outputType: "array",
    syntax: "array | sortBy(expression)\narray | sortBy(expression, -1)",
    examples: [
      { code: "products | sortBy(this.price)", label: "Ascending by price" },
      {
        code: "products | sortBy(this.price, -1)",
        label: "Descending by price",
      },
      { code: "users | sortBy(this.name)", label: "Alphabetical" },
    ],
  },
  {
    id: "any",
    name: "any",
    category: "expression-transform",
    summary: "True if at least one element matches the expression.",
    description:
      "Returns `true` if at least one element evaluates to truthy, `false` otherwise.",
    inputType: "array",
    outputType: "boolean",
    syntax: "array | any(expression)",
    examples: [
      { code: "[1, 2, 3] | any(this > 2)", result: "true" },
      { code: "users | any(this.role == 'admin')", label: "Check for admin" },
    ],
  },
  {
    id: "all",
    name: "all",
    category: "expression-transform",
    summary: "True if all elements match the expression.",
    description:
      "Returns `true` if every element evaluates to truthy, `false` otherwise.",
    inputType: "array",
    outputType: "boolean",
    syntax: "array | all(expression)",
    examples: [
      { code: "[2, 4, 6] | all(this % 2 == 0)", result: "true" },
      {
        code: "orders | all(this.status == 'shipped')",
        label: "Check all shipped",
      },
    ],
  },
  {
    id: "find",
    name: "find",
    category: "expression-transform",
    summary: "Return the first matching element, or null.",
    description:
      "Returns the first element for which the expression evaluates to truthy. Returns `null` if nothing matches.",
    inputType: "array",
    outputType: "any",
    syntax: "array | find(expression)",
    examples: [
      {
        code: '[{"id": 1}, {"id": 2}] | find(this.id == 2)',
        result: '{"id": 2}',
      },
      {
        code: "users | find(this.email == 'alice@example.com')",
        label: "Find by email",
      },
    ],
  },
  {
    id: "findIndex",
    name: "findIndex",
    category: "expression-transform",
    summary: "Return the index of the first match, or -1.",
    description:
      "Returns the 0-based index of the first element matching the expression. Returns `-1` if nothing matches.",
    inputType: "array",
    outputType: "number",
    syntax: "array | findIndex(expression)",
    examples: [
      { code: '[{"id": 1}, {"id": 2}] | findIndex(this.id == 2)', result: "1" },
    ],
  },
  {
    id: "index-var",
    name: "index",
    category: "syntax",
    summary: "Not a transformer — contextual variable available inside element-level expressions.",
    description:
      "`index` is a contextual variable (not a transformer). It is provided to inline element-level expressions used by transforms such as `map`, `filter`, `find`, `findIndex`, `any`, `all`, and `sortBy`. `index` contains the zero-based numeric position of the current element. Use it alongside `this` (the current element) and `acc` (the accumulator in `reduce`).",
    inputType: "number",
    outputType: "number",
    syntax: "index",
    examples: [
      { code: "[1, 2, 3] | map(this + index)", result: "[1, 3, 5]", label: "Add position to value" },
      { code: "[10,11,12,13] | filter(index % 2 == 0)", result: "[10, 12]", label: "Keep even indices" },
    ],
  },
  // ══════════════════════════════════════════════════════════════════════════
  // STRING TRANSFORMS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "lowercase",
    name: "lowercase",
    category: "string-transform",
    summary: "Convert string to lower case.",
    description:
      "Returns the string with all characters converted to lower case.",
    inputType: "string",
    outputType: "string",
    syntax: "string | lowercase",
    examples: [{ code: "'Hello' | lowercase", result: '"hello"' }],
  },
  {
    id: "uppercase",
    name: "uppercase",
    category: "string-transform",
    summary: "Convert string to upper case.",
    description:
      "Returns the string with all characters converted to upper case.",
    inputType: "string",
    outputType: "string",
    syntax: "string | uppercase",
    examples: [{ code: "'hello' | uppercase", result: '"HELLO"' }],
  },
  {
    id: "trim",
    name: "trim",
    category: "string-transform",
    summary: "Remove leading and trailing whitespace.",
    description:
      "Returns the string with leading and trailing whitespace removed.",
    inputType: "string",
    outputType: "string",
    syntax: "string | trim",
    examples: [{ code: "' hi ' | trim", result: '"hi"' }],
  },
  {
    id: "capitalize",
    name: "capitalize",
    category: "string-transform",
    summary: "Uppercase only the first character.",
    description:
      "Returns the string with the first character uppercased and the rest unchanged.",
    inputType: "string",
    outputType: "string",
    syntax: "string | capitalize",
    examples: [{ code: "'hello' | capitalize", result: '"Hello"' }],
  },
  {
    id: "deburr",
    name: "deburr",
    category: "string-transform",
    summary: "Remove diacritics/accents.",
    description: "Returns the string with diacritics (accent marks) removed.",
    inputType: "string",
    outputType: "string",
    syntax: "string | deburr",
    examples: [{ code: "'éàü' | deburr", result: '"eau"' }],
  },
  {
    id: "contains",
    name: "contains",
    category: "string-transform",
    summary: "Check if a string/array contains a value.",
    description:
      "For strings, checks if the substring is present. For arrays, checks if the value is an element. Returns a boolean.",
    inputType: "string | array",
    outputType: "boolean",
    syntax: "string | contains(substring)\narray | contains(value)",
    examples: [
      { code: "'foobar' | contains('oba')", result: "true" },
      { code: "[1, 2, 3] | contains(2)", result: "true" },
    ],
  },
  {
    id: "startsWith",
    name: "startsWith",
    category: "string-transform",
    summary: "Check if a string starts with a prefix.",
    description: "Returns `true` if the string starts with the given prefix.",
    inputType: "string",
    outputType: "boolean",
    syntax: "string | startsWith(prefix)",
    examples: [{ code: "'hello' | startsWith('he')", result: "true" }],
  },
  {
    id: "endsWith",
    name: "endsWith",
    category: "string-transform",
    summary: "Check if a string ends with a suffix.",
    description: "Returns `true` if the string ends with the given suffix.",
    inputType: "string",
    outputType: "boolean",
    syntax: "string | endsWith(suffix)",
    examples: [{ code: "'hello' | endsWith('lo')", result: "true" }],
  },
  {
    id: "split",
    name: "split",
    category: "string-transform",
    summary: "Split a string into an array.",
    description:
      "Splits the string by the separator and returns an array of substrings.",
    inputType: "string",
    outputType: "array",
    syntax: "string | split(separator)",
    examples: [{ code: "'a,b,c' | split(',')", result: '["a", "b", "c"]' }],
  },
  {
    id: "replace",
    name: "replace",
    category: "string-transform",
    summary: "Replace first occurrence of a substring.",
    description:
      "Replaces the first occurrence of the search string with the replacement.",
    inputType: "string",
    outputType: "string",
    syntax: "string | replace(search, replacement)",
    examples: [
      { code: "'hi Bob' | replace('Bob', 'Alice')", result: '"hi Alice"' },
    ],
  },
  {
    id: "repeat",
    name: "repeat",
    category: "string-transform",
    summary: "Repeat a string n times.",
    description: "Returns the string repeated `n` times.",
    inputType: "string",
    outputType: "string",
    syntax: "string | repeat(n)",
    examples: [{ code: "'ab' | repeat(3)", result: '"ababab"' }],
  },
  {
    id: "padStart",
    name: "padStart",
    category: "string-transform",
    summary: "Pad from the left to reach a target length.",
    description:
      "Pads the start of the string to reach the target length. Default pad character is a space.",
    inputType: "string",
    outputType: "string",
    syntax: "string | padStart(length, padChar?)",
    examples: [{ code: "'5' | padStart(3, '0')", result: '"005"' }],
  },
  {
    id: "padEnd",
    name: "padEnd",
    category: "string-transform",
    summary: "Pad from the right to reach a target length.",
    description:
      "Pads the end of the string to reach the target length. Default pad character is a space.",
    inputType: "string",
    outputType: "string",
    syntax: "string | padEnd(length, padChar?)",
    examples: [{ code: "'5' | padEnd(3, '0')", result: '"500"' }],
  },
  {
    id: "truncate",
    name: "truncate",
    category: "string-transform",
    summary: "Truncate to a maximum length.",
    description:
      'Truncates the string to the given length, appending the suffix (default `"..."`).',
    inputType: "string",
    outputType: "string",
    syntax: "string | truncate(length, suffix?)",
    examples: [{ code: "'hello world' | truncate(8)", result: '"hello..."' }],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // NUMBER TRANSFORMS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "abs",
    name: "abs",
    category: "number-transform",
    summary: "Absolute value.",
    description: "Returns the absolute value of the number.",
    inputType: "number",
    outputType: "number",
    syntax: "number | abs",
    examples: [{ code: "-3.7 | abs", result: "3.7" }],
  },
  {
    id: "floor",
    name: "floor",
    category: "number-transform",
    summary: "Round down to integer.",
    description: "Rounds the number down to the nearest integer.",
    inputType: "number",
    outputType: "number",
    syntax: "number | floor",
    examples: [{ code: "3.7 | floor", result: "3" }],
  },
  {
    id: "ceil",
    name: "ceil",
    category: "number-transform",
    summary: "Round up to integer.",
    description: "Rounds the number up to the nearest integer.",
    inputType: "number",
    outputType: "number",
    syntax: "number | ceil",
    examples: [{ code: "3.2 | ceil", result: "4" }],
  },
  {
    id: "trunk",
    name: "trunk",
    category: "number-transform",
    summary: "Truncate decimal part.",
    description:
      "Removes the decimal part of the number (truncates towards zero).",
    inputType: "number",
    outputType: "number",
    syntax: "number | trunk",
    examples: [{ code: "3.9 | trunk", result: "3" }],
  },
  {
    id: "round",
    name: "round",
    category: "number-transform",
    summary: "Round to nearest integer or N decimal places.",
    description:
      "Rounds to the nearest integer, or to a specified number of decimal places when a precision argument is provided.",
    inputType: "number",
    outputType: "number",
    syntax: "number | round\nnumber | round(precision)",
    examples: [
      { code: "3.5 | round", result: "4" },
      { code: "3.14159 | round(2)", result: "3.14" },
    ],
  },
  {
    id: "sqrt",
    name: "sqrt",
    category: "number-transform",
    summary: "Square root.",
    description: "Returns the square root of the number.",
    inputType: "number",
    outputType: "number",
    syntax: "number | sqrt",
    examples: [{ code: "25 | sqrt", result: "5" }],
  },
  {
    id: "pow",
    name: "pow",
    category: "number-transform",
    summary: "Raise to a power.",
    description: "Returns the number raised to the specified exponent.",
    inputType: "number",
    outputType: "number",
    syntax: "number | pow(exponent)",
    examples: [{ code: "2 | pow(10)", result: "1024" }],
  },
  {
    id: "log",
    name: "log",
    category: "number-transform",
    summary: "Logarithm (natural by default).",
    description:
      "Returns the logarithm of the number. Natural log if no base is specified.",
    inputType: "number",
    outputType: "number",
    syntax: "number | log\nnumber | log(base)",
    examples: [{ code: "10 | log(10)", result: "1" }],
  },
  {
    id: "log10",
    name: "log10",
    category: "number-transform",
    summary: "Base-10 logarithm.",
    description: "Returns the base-10 logarithm of the number.",
    inputType: "number",
    outputType: "number",
    syntax: "number | log10",
    examples: [{ code: "100 | log10", result: "2" }],
  },
  {
    id: "log2",
    name: "log2",
    category: "number-transform",
    summary: "Base-2 logarithm.",
    description: "Returns the base-2 logarithm of the number.",
    inputType: "number",
    outputType: "number",
    syntax: "number | log2",
    examples: [{ code: "8 | log2", result: "3" }],
  },
  {
    id: "clamp",
    name: "clamp",
    category: "number-transform",
    summary: "Constrain a number to a range.",
    description: "Returns the number clamped between `min` and `max`.",
    inputType: "number",
    outputType: "number",
    syntax: "number | clamp(min, max)",
    examples: [{ code: "5 | clamp(1, 4)", result: "4" }],
  },
  {
    id: "mod",
    name: "mod",
    category: "number-transform",
    summary: "Modulo (remainder).",
    description: "Returns the remainder of dividing the number by the divisor.",
    inputType: "number",
    outputType: "number",
    syntax: "number | mod(divisor)",
    examples: [{ code: "10 | mod(3)", result: "1" }],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // ARRAY TRANSFORMS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "size",
    name: "size",
    category: "array-transform",
    summary: "sizegth of array, string, or object.",
    description:
      "Returns the number of elements in an array, characters in a string, or keys in an object.",
    inputType: "array | string | object",
    outputType: "number",
    syntax: "value | size",
    examples: [
      { code: "[1, 2, 3] | size", result: "3" },
      { code: "'hello' | size", result: "5" },
    ],
  },
  {
    id: "first",
    name: "first",
    category: "array-transform",
    summary: "First element of an array.",
    description:
      "Returns the first element. Returns `null` for an empty array.",
    inputType: "array",
    outputType: "any",
    syntax: "array | first",
    examples: [{ code: "[1, 2, 3] | first", result: "1" }],
  },
  {
    id: "last",
    name: "last",
    category: "array-transform",
    summary: "Last element of an array.",
    description: "Returns the last element. Returns `null` for an empty array.",
    inputType: "array",
    outputType: "any",
    syntax: "array | last",
    examples: [{ code: "[1, 2, 3] | last", result: "3" }],
  },
  {
    id: "get",
    name: "get",
    category: "array-transform",
    summary: "Get element by index or key.",
    description:
      "Returns the element at the given index (for arrays) or key (for objects/strings).",
    inputType: "array | object | string",
    outputType: "any",
    syntax: "value | get(index)",
    examples: [
      { code: "[10, 20, 30] | get(2)", result: "30" },
      { code: "'hello' | get(1)", result: '"e"' },
    ],
  },
  {
    id: "range",
    name: "range",
    category: "array-transform",
    summary: "Slice an array or substring by index range.",
    description:
      "Returns a slice from `start` to `end` (exclusive). `end` of `-1` means to the end. `step` defaults to 1. Works on arrays and strings.",
    inputType: "array | string",
    outputType: "array | string",
    syntax: "value | range(start?, end?, step?)",
    examples: [
      { code: "[1, 2, 3, 4, 5] | range(1, 3)", result: "[2, 3]" },
      { code: "'hello' | range(1, 3)", result: '"el"' },
    ],
  },
  {
    id: "reverse",
    name: "reverse",
    category: "array-transform",
    summary: "Reverse an array.",
    description: "Returns a new array with elements in reverse order.",
    inputType: "array",
    outputType: "array",
    syntax: "array | reverse",
    examples: [{ code: "[1, 2, 3] | reverse", result: "[3, 2, 1]" }],
  },
  {
    id: "flatten",
    name: "flatten",
    category: "array-transform",
    summary: "Flatten one level of nested arrays.",
    description: "Flattens one level of array nesting.",
    inputType: "array",
    outputType: "array",
    syntax: "array | flatten",
    examples: [{ code: "[[1, 2], [3]] | flatten", result: "[1, 2, 3]" }],
  },
  {
    id: "unique",
    name: "unique",
    category: "array-transform",
    summary: "Remove duplicate primitives.",
    description: "Returns a new array with duplicate primitive values removed.",
    inputType: "array",
    outputType: "array",
    syntax: "array | unique",
    examples: [{ code: "[1, 2, 1, 3] | unique", result: "[1, 2, 3]" }],
  },
  {
    id: "uniqueByAttribute",
    name: "uniqueByAttribute",
    category: "array-transform",
    summary: "Remove objects with duplicate values of an attribute.",
    description:
      "Keeps the first object for each distinct value of the specified attribute.",
    inputType: "array",
    outputType: "array",
    syntax: "array | uniqueByAttribute(attr)",
    examples: [
      { code: "items | uniqueByAttribute('id')", label: "Deduplicate by id" },
    ],
  },
  {
    id: "compact",
    name: "compact",
    category: "array-transform",
    summary: "Remove null and false values.",
    description: "Returns a new array with `null` and `false` values removed.",
    inputType: "array",
    outputType: "array",
    syntax: "array | compact",
    examples: [
      { code: "[1, null, 2, false, 3] | compact", result: "[1, 2, 3]" },
    ],
  },
  {
    id: "push",
    name: "push",
    category: "array-transform",
    summary: "Append a value (returns new array).",
    description: "Returns a new array with the value appended at the end.",
    inputType: "array",
    outputType: "array",
    syntax: "array | push(value)",
    examples: [{ code: "[1, 2] | push(3)", result: "[1, 2, 3]" }],
  },
  {
    id: "concat",
    name: "concat",
    category: "array-transform",
    summary: "Concatenate with another array.",
    description:
      "Returns a new array with the elements of both arrays combined.",
    inputType: "array",
    outputType: "array",
    syntax: "array | concat(otherArray)",
    examples: [{ code: "[1, 2] | concat([3, 4])", result: "[1, 2, 3, 4]" }],
  },
  {
    id: "pick",
    name: "pick",
    category: "array-transform",
    summary: "Extract one attribute from each object.",
    description:
      "Returns an array of the specified attribute's value from each object in the array.",
    inputType: "array",
    outputType: "array",
    syntax: "array | pick(attr)",
    examples: [{ code: "users | pick('name')", result: '["Alice", "Bob"]' }],
  },
  {
    id: "join",
    name: "join",
    category: "array-transform",
    summary: "Join array elements into a string.",
    description:
      "Joins all array elements into a string separated by the given separator.",
    inputType: "array",
    outputType: "string",
    syntax: "array | join(separator)",
    examples: [{ code: "['a', 'b', 'c'] | join(',')", result: '"a,b,c"' }],
  },
  {
    id: "indexOf",
    name: "indexOf",
    category: "array-transform",
    summary: "Index of first occurrence, or -1.",
    description:
      "Returns the 0-based index of the first occurrence of the value, or `-1` if not found.",
    inputType: "array",
    outputType: "number",
    syntax: "array | indexOf(value)",
    examples: [{ code: "[1, 2, 3] | indexOf(2)", result: "1" }],
  },
  {
    id: "sort",
    name: "sort",
    category: "array-transform",
    summary: "Sort numbers or strings.",
    description:
      "Sorts primitives ascending by default. Pass `-1` for descending.",
    inputType: "array",
    outputType: "array",
    syntax: "array | sort\narray | sort(-1)",
    examples: [
      { code: "[3, 1, 2] | sort", result: "[1, 2, 3]" },
      { code: "[3, 1, 2] | sort(-1)", result: "[3, 2, 1]" },
    ],
  },
  {
    id: "sortByAttribute",
    name: "sortByAttribute",
    category: "array-transform",
    summary: "Sort objects by attribute.",
    description:
      "Sorts an array of objects by the given attribute. Ascending by default; pass `-1` for descending.",
    inputType: "array",
    outputType: "array",
    syntax: "array | sortByAttribute(attr)\narray | sortByAttribute(attr, -1)",
    examples: [
      { code: "items | sortByAttribute('name')", label: "Ascending" },
      { code: "items | sortByAttribute('price', -1)", label: "Descending" },
    ],
  },
  {
    id: "sum",
    name: "sum",
    category: "array-transform",
    summary: "Sum of numeric array.",
    description: "Returns the sum of all numbers in the array.",
    inputType: "array",
    outputType: "number",
    syntax: "array | sum",
    examples: [{ code: "[1, 2, 3] | sum", result: "6" }],
  },
  {
    id: "mean",
    name: "mean",
    category: "array-transform",
    summary: "Average of numeric array.",
    description: "Returns the arithmetic mean (average) of all numbers.",
    inputType: "array",
    outputType: "number",
    syntax: "array | mean",
    examples: [{ code: "[1, 2, 3] | mean", result: "2" }],
  },
  {
    id: "min",
    name: "min",
    category: "array-transform",
    summary: "Minimum of numeric array.",
    description: "Returns the smallest number in the array.",
    inputType: "array",
    outputType: "number",
    syntax: "array | min",
    examples: [{ code: "[1, 2, 3] | min", result: "1" }],
  },
  {
    id: "max",
    name: "max",
    category: "array-transform",
    summary: "Maximum of numeric array.",
    description: "Returns the largest number in the array.",
    inputType: "array",
    outputType: "number",
    syntax: "array | max",
    examples: [{ code: "[1, 2, 3] | max", result: "3" }],
  },
  {
    id: "minByAttribute",
    name: "minByAttribute",
    category: "array-transform",
    summary: "Object with minimum value for an attribute.",
    description:
      "Returns the object that has the minimum value for the specified attribute.",
    inputType: "array",
    outputType: "any",
    syntax: "array | minByAttribute(attr)",
    examples: [
      { code: "items | minByAttribute('price')", label: "Cheapest item" },
    ],
  },
  {
    id: "maxByAttribute",
    name: "maxByAttribute",
    category: "array-transform",
    summary: "Object with maximum value for an attribute.",
    description:
      "Returns the object that has the maximum value for the specified attribute.",
    inputType: "array",
    outputType: "any",
    syntax: "array | maxByAttribute(attr)",
    examples: [
      { code: "items | maxByAttribute('score')", label: "Highest scored" },
    ],
  },
  {
    id: "groupBy",
    name: "groupBy",
    category: "array-transform",
    summary: "Group objects into {key: [items]} by attribute.",
    description:
      "Groups array elements into an object keyed by the specified attribute's value.",
    inputType: "array",
    outputType: "object",
    syntax: "array | groupBy(attr)",
    examples: [
      { code: "orders | groupBy('category')", label: "Group by category" },
    ],
  },
  {
    id: "every",
    name: "every",
    category: "array-transform",
    summary: "True if all elements equal a value.",
    description:
      "Returns `true` if every element in the array equals the given value.",
    inputType: "array",
    outputType: "boolean",
    syntax: "array | every(value)",
    examples: [{ code: "[2, 2, 2] | every(2)", result: "true" }],
  },
  {
    id: "some",
    name: "some",
    category: "array-transform",
    summary: "True if any element equals a value.",
    description:
      "Returns `true` if at least one element equals the given value.",
    inputType: "array",
    outputType: "boolean",
    syntax: "array | some(value)",
    examples: [{ code: "[1, 2, 3] | some(2)", result: "true" }],
  },
  {
    id: "sample",
    name: "sample",
    category: "array-transform",
    summary: "Return a random element.",
    description: "Returns a randomly selected element from the array.",
    inputType: "array",
    outputType: "any",
    syntax: "array | sample",
    examples: [{ code: "[1, 2, 3] | sample", label: "Random element" }],
  },
  {
    id: "shuffle",
    name: "shuffle",
    category: "array-transform",
    summary: "Return a shuffled copy.",
    description: "Returns a new array with the elements in random order.",
    inputType: "array",
    outputType: "array",
    syntax: "array | shuffle",
    examples: [{ code: "[1, 2, 3] | shuffle", label: "Shuffled array" }],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // OBJECT TRANSFORMS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "keys",
    name: "keys",
    category: "object-transform",
    summary: "Array of object keys.",
    description:
      "Returns an array of the object's own enumerable property names.",
    inputType: "object",
    outputType: "array",
    syntax: "object | keys",
    examples: [{ code: "{a: 1, b: 2} | keys", result: '["a", "b"]' }],
  },
  {
    id: "values",
    name: "values",
    category: "object-transform",
    summary: "Array of object values.",
    description:
      "Returns an array of the object's own enumerable property values.",
    inputType: "object",
    outputType: "array",
    syntax: "object | values",
    examples: [{ code: "{a: 1, b: 2} | values", result: "[1, 2]" }],
  },
  {
    id: "entries",
    name: "entries",
    category: "object-transform",
    summary: "Array of [key, value] pairs.",
    description: "Returns an array of `[key, value]` pairs.",
    inputType: "object",
    outputType: "array",
    syntax: "object | entries",
    examples: [{ code: "{a: 1} | entries", result: '[["a", 1]]' }],
  },
  {
    id: "fromEntries",
    name: "fromEntries",
    category: "object-transform",
    summary: "Construct an object from [key, value] pairs.",
    description:
      "Builds an object from an array of `[key, value]` pairs. Later keys overwrite earlier ones.",
    inputType: "array",
    outputType: "object",
    syntax: "array | fromEntries",
    examples: [
      { code: "[['a', 1], ['b', 2]] | fromEntries", result: '{"a": 1, "b": 2}' },
      { code: "{a:1, b:2} | entries | fromEntries", result: '{"a": 1, "b": 2}' },
    ],
  },
  {
    id: "has",
    name: "has",
    category: "object-transform",
    summary: "Check if object has a key.",
    description: "Returns `true` if the object has the specified key.",
    inputType: "object",
    outputType: "boolean",
    syntax: "object | has(key)",
    examples: [{ code: "{a: 1} | has('a')", result: "true" }],
  },
  {
    id: "merge",
    name: "merge",
    category: "object-transform",
    summary: "Merge another object (right side wins).",
    description:
      "Returns a new object with properties from both objects. The argument's properties take precedence.",
    inputType: "object",
    outputType: "object",
    syntax: "object | merge(otherObject)",
    examples: [{ code: "{a: 1} | merge({b: 2})", result: '{"a": 1, "b": 2}' }],
  },
  {
    id: "omit",
    name: "omit",
    category: "object-transform",
    summary: "Return object without specified keys.",
    description: "Returns a new object with the specified keys removed.",
    inputType: "object",
    outputType: "object",
    syntax: "object | omit(key, ...)",
    examples: [
      { code: "{a: 1, b: 2, c: 3} | omit('b', 'c')", result: '{"a": 1}' },
    ],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // TYPE TRANSFORMS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "type",
    name: "type",
    category: "type-transform",
    summary: "Get the type name as a string.",
    description:
      'Returns the type of the value: `"null"`, `"boolean"`, `"number"`, `"string"`, `"array"`, or `"object"`.',
    inputType: "any",
    outputType: "string",
    syntax: "value | type",
    examples: [
      { code: "42 | type", result: '"number"' },
      { code: "'hello' | type", result: '"string"' },
    ],
  },
  {
    id: "isNull",
    name: "isNull",
    category: "type-transform",
    summary: "True if value is null.",
    description: "Returns `true` if the value is `null`.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isNull",
    examples: [{ code: "null | isNull", result: "true" }],
  },
  {
    id: "isDefined",
    name: "isDefined",
    category: "type-transform",
    summary: "True if value is not null.",
    description: "Returns `true` if the value is not `null`.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isDefined",
    examples: [{ code: "42 | isDefined", result: "true" }],
  },
  {
    id: "isBoolean",
    name: "isBoolean",
    category: "type-transform",
    summary: "True if value is boolean.",
    description: "Returns `true` if the value is a boolean.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isBoolean",
    examples: [{ code: "true | isBoolean", result: "true" }],
  },
  {
    id: "isNumber",
    name: "isNumber",
    category: "type-transform",
    summary: "True if value is a number.",
    description: "Returns `true` if the value is a number.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isNumber",
    examples: [{ code: "42 | isNumber", result: "true" }],
  },
  {
    id: "isString",
    name: "isString",
    category: "type-transform",
    summary: "True if value is a string.",
    description: "Returns `true` if the value is a string.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isString",
    examples: [{ code: "'hello' | isString", result: "true" }],
  },
  {
    id: "isArray",
    name: "isArray",
    category: "type-transform",
    summary: "True if value is an array.",
    description: "Returns `true` if the value is an array.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isArray",
    examples: [{ code: "[1, 2] | isArray", result: "true" }],
  },
  {
    id: "isObject",
    name: "isObject",
    category: "type-transform",
    summary: "True if value is an object.",
    description: "Returns `true` if the value is a plain object.",
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isObject",
    examples: [{ code: "{a: 1} | isObject", result: "true" }],
  },
  {
    id: "isEmpty",
    name: "isEmpty",
    category: "type-transform",
    summary: "True if null, empty string, empty array, or empty object.",
    description:
      'Returns `true` if the value is `null`, `""`, `[]`, `{}`, or `false`.',
    inputType: "any",
    outputType: "boolean",
    syntax: "value | isEmpty",
    examples: [
      { code: "'' | isEmpty", result: "true" },
      { code: "[] | isEmpty", result: "true" },
    ],
  },
  {
    id: "coalesce",
    name: "coalesce",
    category: "type-transform",
    summary: "First non-null value from subject or arguments.",
    description:
      "Returns the first non-null value: the subject itself if non-null, otherwise the first non-null argument.",
    inputType: "any",
    outputType: "any",
    syntax: "value | coalesce(fallback, ...)",
    examples: [
      {
        code: "nickname | coalesce(username, 'Anonymous')",
        label: "Fallback chain",
      },
    ],
  },
  {
    id: "toInteger",
    name: "toInteger",
    category: "type-transform",
    summary: "Convert to integer.",
    description:
      "Converts a value to an integer (truncates float, parses numeric string).",
    inputType: "any",
    outputType: "number",
    syntax: "value | toInteger",
    examples: [{ code: "'42' | toInteger", result: "42" }],
  },
  {
    id: "toFloat",
    name: "toFloat",
    category: "type-transform",
    summary: "Convert to float.",
    description: "Converts a value to a float.",
    inputType: "any",
    outputType: "number",
    syntax: "value | toFloat",
    examples: [{ code: "'3.14' | toFloat", result: "3.14" }],
  },
  {
    id: "toString",
    name: "toString",
    category: "type-transform",
    summary: "Convert to string.",
    description: "Converts a value to its string representation.",
    inputType: "any",
    outputType: "string",
    syntax: "value | toString",
    examples: [{ code: "42 | toString", result: '"42.0"' }],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // DATE TRANSFORMS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "toDate",
    name: "toDate",
    category: "date-transform",
    summary: "Parse date string to Unix timestamp.",
    description:
      "Parses a date string and returns a Unix timestamp (seconds). The optional `format` parameter lets you specify a time-rs format; if omitted the parser tries common ISO/RFC3339 and other common datetime formats.",
    inputType: "string",
    outputType: "number",
    syntax: "string | toDate(format?)",
    examples: [
      {
        code: '"2020-12-09T16:09:53+00:00" | toDate',
        label: "ISO with timezone (no format needed)",
      },
      { code: '"2020-12-09T16:09:53" | toDate', label: "ISO without timezone" },
      {
        code: '"2020-12-09 16:09:53" | toDate',
        label: "Space-separated datetime",
      },
      {
        code: '"2024-01-15" | toDate("[year]-[month]-[day]")',
        label: "Explicit format",
      },
    ],
  },
  {
    id: "toDateTime",
    name: "toDateTime",
    category: "date-transform",
    summary: "Parse datetime string to Unix timestamp.",
    description:
      "Parses a datetime string and returns a Unix timestamp (seconds). The `format` argument is optional — when omitted the parser attempts common ISO/RFC3339 and other datetime formats.",
    inputType: "string",
    outputType: "number",
    syntax: "string | toDateTime(format?)",
    examples: [
      {
        code: '"2020-12-09T16:09:53+00:00" | toDateTime',
        label: "ISO with timezone (no format needed)",
      },
      {
        code: '"2020-12-09T16:09:53" | toDateTime',
        label: "ISO without timezone",
      },
      {
        code: '"2024-01-15T10:30:00" | toDateTime("[year]-[month]-[day]T[hour]:[minute]:[second]")',
        label: "Explicit format",
      },
    ],
  },
  {
    id: "age",
    name: "age",
    category: "date-transform",
    summary: "Compute age in full years relative to now.",
    description:
      "Returns the number of full years between the subject date and the current time. Accepts a Unix timestamp (number) or a date string; an optional `format` argument may be provided for non-ISO dates.",
    inputType: "string",
    outputType: "number",
    syntax: "date | age(format?)",
    examples: [
      {
        code: '"1990-06-15" | toDate | age',
        label: "Compute age from a formatted date (result depends on current date)",
      },
      {
        code: '"1990-06-15" | age("[year]-[month]-[day]")',
        label: "Parse string and compute age in years",
      },
    ],
  },
  {
    id: "ageIn",
    name: "ageIn",
    category: "date-transform",
    summary: "Compute age in the specified unit relative to now.",
    description:
      "Returns the number of units between the subject date and the current time. Accepts a Unix timestamp (number) or a date string; an optional `format` argument may be provided for non-ISO dates.",
    inputType: "string",
    outputType: "number",
    syntax: "date | ageIn(unit, format?)",
    examples: [
      {
        code: '"1990-06-15" | toDate | ageIn("Y")',
        label: "Compute age from a formatted date (result depends on current date)",
      },
      {
        code: '"1990-06-15" | ageIn("months", "[year]-[month]-[day]")',
        label: "Parse string and compute age in years",
      },
    ],
  },
  // ══════════════════════════════════════════════════════════════════════════
  // NOW and NOW_UTC VARIABLES
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "now-var",
    name: "$now",
    category: "function",
    summary: "Current time as Unix timestamp.",
    description:
      "`$now` is a special keyword (not a function call) that returns the current local time as a Unix timestamp in seconds.",
    outputType: "number",
    syntax: "$now",
    examples: [
      { code: "$now", label: "Current timestamp" },
      {
        code: 'date("2030-01-01", "[year]-[month]-[day]") > $now',
        result: "true",
        label: "Future date check",
      },
    ],
  },
  {
    id: "now-utc-var",
    name: "$now_utc",
    category: "function",
    summary: "Current UTC time as Unix timestamp.",
    description:
      "`$now_utc` is a special keyword (not a function call) that returns the current UTC time as a Unix timestamp in seconds.",
    outputType: "number",
    syntax: "$now_utc",
    examples: [
      { code: "$now_utc", label: "Current UTC timestamp" },
      {
        code: 'date("2030-01-01", "[year]-[month]-[day]") > $now_utc',
        result: "true",
        label: "Future date check (UTC)",
      },
    ],
  },

  // ══════════════════════════════════════════════════════════════════════════
  // COMMON PATTERNS
  // ══════════════════════════════════════════════════════════════════════════
  {
    id: "null-safe",
    name: "Null-safe access",
    category: "pattern",
    summary: "Use || as a fallback for null/falsy values.",
    description:
      "`||` short-circuits, so use it to provide fallback values when a property might be null or falsy.",
    examples: [
      {
        code: "user.nickname || user.name || 'Anonymous'",
        label: "Fallback chain",
      },
    ],
  },
  {
    id: "summary-pattern",
    name: "Building summaries",
    category: "pattern",
    summary: "Aggregate statistics from arrays into an object.",
    description:
      "Combine multiple array operations into a single result object to produce dashboards or summaries.",
    examples: [
      {
        code: "{\n  total: orders | pick('amount') | sum,\n  count: orders | size,\n  average: orders | pick('amount') | mean | round(2),\n  hasExpensive: orders | any(this.amount > 1000)\n}",
        label: "Order summary",
      },
    ],
  },
  {
    id: "chain-pattern",
    name: "Multi-step pipelines",
    category: "pattern",
    summary: "Chain filter, sort, and map for complex data transformations.",
    description:
      "Combine multiple transforms in a pipeline to filter, sort, and reshape data in one expression.",
    examples: [
      {
        code: "employees\n  | filter(this.dept == 'eng')\n  | sortByAttribute('salary', -1)\n  | map({name: this.name, salary: this.salary})",
        label: "Filter → sort → reshape",
      },
    ],
  },
];

/** Lookup a documentation entry by its id. */
export function getDocEntryById(id: string): DocEntry | undefined {
  return DOC_ENTRIES.find((e) => e.id === id);
}

/** Lookup a documentation entry by transformer/function name. */
export function getDocEntryByName(name: string): DocEntry | undefined {
  return DOC_ENTRIES.find((e) => e.name === name || e.id === name);
}
