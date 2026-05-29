use super::types::PrimitiveType;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Describes a single argument accepted by a transformer.
#[derive(Debug, Clone)]
pub struct TransformerArg {
    pub name: &'static str,
    pub types: &'static [PrimitiveType],
    pub required: bool,
    pub description: &'static str,
}

/// Describes how a transformer's output type relates to its input.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputTypeKind {
    /// The output type is fixed (e.g. `join` → String, `length` → Number).
    Fixed,
    /// The output is the element type of the input array (e.g. `first`, `last`, `find`).
    ElementOfInput,
    /// The output is an array preserving the same element type (e.g. `reverse`, `unique`, `filter`, `sort`).
    ArrayPreservingElements,
    /// The output type is determined by the lambda and cannot be statically inferred.
    /// Used for `map`, `reduce`, etc.
    LambdaDetermined,
}

/// Metadata about a transformer: accepted input/output types, description, arguments.
#[derive(Debug, Clone)]
pub struct TransformerDescriptor {
    pub input_types: &'static [PrimitiveType],
    pub output_types: &'static [PrimitiveType],
    pub description: &'static str,
    pub args: Vec<TransformerArg>,
    pub doc_id: Option<&'static str>,
    /// How the output type relates to the input type — enables schema-aware chaining.
    pub output_type_kind: OutputTypeKind,
}

/// Metadata about a built-in variable (e.g. `$now`).
#[derive(Debug, Clone)]
pub struct BuiltinVariableDescriptor {
    pub output_type: PrimitiveType,
    pub description: &'static str,
    pub doc_id: Option<&'static str>,
}

/// Metadata about a built-in function (e.g. `date()`).
#[derive(Debug, Clone)]
pub struct BuiltinFunctionDescriptor {
    pub output_type: PrimitiveType,
    pub description: &'static str,
    pub syntax: &'static str,
    pub args: Vec<TransformerArg>,
    pub doc_id: Option<&'static str>,
}

// Convenience aliases for type arrays used in the registry.
use PrimitiveType::{Any, Array, Boolean, Date, Datetime, Integer, Number, Object, String};

static T_STRING: &[PrimitiveType] = &[String];
static T_NUMBER: &[PrimitiveType] = &[Number];
static T_BOOLEAN: &[PrimitiveType] = &[Boolean];
static T_ARRAY: &[PrimitiveType] = &[Array];
static T_OBJECT: &[PrimitiveType] = &[Object];
static T_ANY: &[PrimitiveType] = &[Any];
static T_INTEGER: &[PrimitiveType] = &[Integer];
static T_DATE: &[PrimitiveType] = &[Date];
static T_DATETIME: &[PrimitiveType] = &[Datetime];
static T_STRING_NUMBER: &[PrimitiveType] = &[String, Number];
static _T_STRING_ARRAY: &[PrimitiveType] = &[String, Array];
static T_STRING_ARRAY_OBJECT: &[PrimitiveType] = &[String, Array, Object];
static T_ARRAY_OBJECT: &[PrimitiveType] = &[Array, Object];
static T_ARRAY_STRING: &[PrimitiveType] = &[Array, String];

/// All built-in transformer descriptors, keyed by name.
pub static TRANSFORMER_REGISTRY: LazyLock<HashMap<&'static str, TransformerDescriptor>> =
    LazyLock::new(|| {
        let mut m = HashMap::new();

        // ── String transformers ──────────────────────────────────────────
        m.insert(
            "lowercase",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Convert string to lower case",
                args: vec![],
                doc_id: Some("lowercase"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "uppercase",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Convert string to upper case",
                args: vec![],
                doc_id: Some("uppercase"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "trim",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Remove leading and trailing whitespace",
                args: vec![],
                doc_id: Some("trim"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "capitalize",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Capitalize the first letter of a string",
                args: vec![],
                doc_id: Some("capitalize"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "deburr",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Remove diacritics from a string",
                args: vec![],
                doc_id: Some("deburr"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "split",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_ARRAY,
                description: "Split a string into an array by a separator",
                args: vec![TransformerArg {
                    name: "separator",
                    types: T_STRING,
                    required: true,
                    description: "The separator string",
                }],
                doc_id: Some("split"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "replace",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Replace a substring in a string",
                args: vec![
                    TransformerArg {
                        name: "search",
                        types: T_STRING,
                        required: true,
                        description: "The substring to search for",
                    },
                    TransformerArg {
                        name: "replacement",
                        types: T_STRING,
                        required: true,
                        description: "The replacement string",
                    },
                ],
                doc_id: Some("replace"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "startsWith",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_BOOLEAN,
                description: "Check if a string starts with a given prefix",
                args: vec![TransformerArg {
                    name: "prefix",
                    types: T_STRING,
                    required: true,
                    description: "The prefix to check for",
                }],
                doc_id: Some("startsWith"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "endsWith",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_BOOLEAN,
                description: "Check if a string ends with a given suffix",
                args: vec![TransformerArg {
                    name: "suffix",
                    types: T_STRING,
                    required: true,
                    description: "The suffix to check for",
                }],
                doc_id: Some("endsWith"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "repeat",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Repeat a string a given number of times",
                args: vec![TransformerArg {
                    name: "count",
                    types: T_NUMBER,
                    required: true,
                    description: "The number of times to repeat",
                }],
                doc_id: Some("repeat"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "padStart",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Pad the start of a string to a target length",
                args: vec![
                    TransformerArg {
                        name: "length",
                        types: T_NUMBER,
                        required: true,
                        description: "Target length",
                    },
                    TransformerArg {
                        name: "char",
                        types: T_STRING,
                        required: false,
                        description: "Padding character (default: space)",
                    },
                ],
                doc_id: Some("padStart"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "padEnd",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Pad the end of a string to a target length",
                args: vec![
                    TransformerArg {
                        name: "length",
                        types: T_NUMBER,
                        required: true,
                        description: "Target length",
                    },
                    TransformerArg {
                        name: "char",
                        types: T_STRING,
                        required: false,
                        description: "Padding character (default: space)",
                    },
                ],
                doc_id: Some("padEnd"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "truncate",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Truncate a string to a maximum length",
                args: vec![
                    TransformerArg {
                        name: "length",
                        types: T_NUMBER,
                        required: true,
                        description: "Maximum length",
                    },
                    TransformerArg {
                        name: "omission",
                        types: T_STRING,
                        required: false,
                        description: "Omission string (default: '...')",
                    },
                ],
                doc_id: Some("truncate"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "camelCase",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Convert a string to camelCase",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "kebabCase",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Convert a string to kebab-case",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "snakeCase",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Convert a string to snake_case",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "startCase",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_STRING,
                description: "Convert a string to Start Case (space-separated, capitalized)",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Number transformers ──────────────────────────────────────────
        m.insert(
            "round",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Round a number to a given number of decimal places",
                args: vec![TransformerArg {
                    name: "decimals",
                    types: T_NUMBER,
                    required: false,
                    description: "Number of decimal places (default: 0)",
                }],
                doc_id: Some("round"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "floor",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Round a number down to the nearest integer",
                args: vec![],
                doc_id: Some("floor"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "ceil",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Round a number up to the nearest integer",
                args: vec![],
                doc_id: Some("ceil"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "trunk",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Truncate the decimal part of a number",
                args: vec![],
                doc_id: Some("trunk"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "abs",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Get the absolute value of a number",
                args: vec![],
                doc_id: Some("abs"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "sqrt",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Get the square root of a number",
                args: vec![],
                doc_id: Some("sqrt"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "pow",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Raise a number to a power",
                args: vec![TransformerArg {
                    name: "exponent",
                    types: T_NUMBER,
                    required: true,
                    description: "The exponent",
                }],
                doc_id: Some("pow"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "log",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Get the natural logarithm of a number",
                args: vec![TransformerArg {
                    name: "base",
                    types: T_NUMBER,
                    required: false,
                    description: "Logarithm base (default: e)",
                }],
                doc_id: Some("log"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "log10",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Get the base-10 logarithm of a number",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "log2",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Get the base-2 logarithm of a number",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "clamp",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Clamp a number between a minimum and maximum value",
                args: vec![
                    TransformerArg {
                        name: "min",
                        types: T_NUMBER,
                        required: true,
                        description: "Minimum value",
                    },
                    TransformerArg {
                        name: "max",
                        types: T_NUMBER,
                        required: true,
                        description: "Maximum value",
                    },
                ],
                doc_id: Some("clamp"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "mod",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Get the remainder of dividing a number by another",
                args: vec![TransformerArg {
                    name: "divisor",
                    types: T_NUMBER,
                    required: true,
                    description: "The divisor",
                }],
                doc_id: Some("mod"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Type conversion transformers ─────────────────────────────────
        m.insert(
            "toString",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_STRING,
                description: "Convert a value to a string",
                args: vec![],
                doc_id: Some("toString"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "toInteger",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_NUMBER,
                description: "Convert a value to an integer",
                args: vec![],
                doc_id: Some("toInteger"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "toFloat",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_NUMBER,
                description: "Convert a value to a float",
                args: vec![],
                doc_id: Some("toFloat"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Date transformers ────────────────────────────────────────────
        m.insert(
            "toDate",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_DATE,
                description: "Convert a string to a date",
                args: vec![TransformerArg {
                    name: "format",
                    types: T_STRING,
                    required: false,
                    description: "Date format string",
                }],
                doc_id: Some("toDate"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "toDateTime",
            TransformerDescriptor {
                input_types: T_STRING,
                output_types: T_DATETIME,
                description: "Convert a string to a datetime",
                args: vec![TransformerArg {
                    name: "format",
                    types: T_STRING,
                    required: false,
                    description: "Datetime format string",
                }],
                doc_id: Some("toDateTime"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "age",
            TransformerDescriptor {
                input_types: T_STRING_NUMBER,
                output_types: T_INTEGER,
                description: "Compute the number of full years between the subject date and now",
                args: vec![TransformerArg {
                    name: "format",
                    types: T_STRING,
                    required: false,
                    description: "Optional format string for parsing non-ISO dates",
                }],
                doc_id: Some("age"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "formatDate",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_STRING,
                description: "Format a Unix timestamp as a date string",
                args: vec![TransformerArg {
                    name: "format",
                    types: T_STRING,
                    required: true,
                    description: "Format string",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "ageIn",
            TransformerDescriptor {
                input_types: T_STRING_NUMBER,
                output_types: T_NUMBER,
                description: "Compute age in a given unit (days, months, years, etc.)",
                args: vec![
                    TransformerArg {
                        name: "unit",
                        types: T_STRING,
                        required: true,
                        description: "Unit: days, months, years, etc.",
                    },
                    TransformerArg {
                        name: "format",
                        types: T_STRING,
                        required: false,
                        description: "Optional format string",
                    },
                ],
                doc_id: Some("ageIn"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "add",
            TransformerDescriptor {
                input_types: T_NUMBER,
                output_types: T_NUMBER,
                description: "Add a duration to a Unix timestamp",
                args: vec![
                    TransformerArg {
                        name: "amount",
                        types: T_NUMBER,
                        required: true,
                        description: "Amount to add",
                    },
                    TransformerArg {
                        name: "unit",
                        types: T_STRING,
                        required: true,
                        description: "Unit: seconds, minutes, hours, days, etc.",
                    },
                ],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Array transformers ───────────────────────────────────────────
        m.insert(
            "first",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Get the first element of an array",
                args: vec![],
                doc_id: Some("first"),
                output_type_kind: OutputTypeKind::ElementOfInput,
            },
        );
        m.insert(
            "last",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Get the last element of an array",
                args: vec![],
                doc_id: Some("last"),
                output_type_kind: OutputTypeKind::ElementOfInput,
            },
        );
        m.insert(
            "reverse",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Reverse the elements of an array",
                args: vec![],
                doc_id: Some("reverse"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "sort",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Sort an array of primitives",
                args: vec![TransformerArg {
                    name: "order",
                    types: T_NUMBER,
                    required: false,
                    description: "Sort order: omit or 1 for ascending (default), -1 for descending",
                }],
                doc_id: Some("sort"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "sortByAttribute",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Sort an array of objects by a given attribute",
                args: vec![
                    TransformerArg {
                        name: "attribute",
                        types: T_STRING,
                        required: true,
                        description: "The attribute name to sort by",
                    },
                    TransformerArg {
                        name: "order",
                        types: T_NUMBER,
                        required: false,
                        description: "Sort order: omit or 1 for ascending (default), -1 for descending",
                    },
                ],
                doc_id: Some("sortByAttribute"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "unique",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Remove duplicate values from an array",
                args: vec![],
                doc_id: Some("unique"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "uniqueByAttribute",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Remove duplicates from an array of objects by attribute",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "The attribute to deduplicate by",
                }],
                doc_id: Some("uniqueByAttribute"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "flatten",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Flatten a nested array one level deep",
                args: vec![],
                doc_id: Some("flatten"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "flattenDeep",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Recursively flatten nested arrays into a single array",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "flattenDepth",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Flatten nested arrays up to a specified depth",
                args: vec![TransformerArg {
                    name: "depth",
                    types: T_NUMBER,
                    required: false,
                    description: "Depth to flatten (default: 1)",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "chunk",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Split an array into chunks of the given size",
                args: vec![TransformerArg {
                    name: "size",
                    types: T_NUMBER,
                    required: true,
                    description: "Chunk size",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "zip",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Zip multiple arrays into an array of tuples",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "unzip",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Unzip an array of tuples into arrays of columns",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "difference",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Return items present in the first array but not in the others",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "union",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Return the union of multiple arrays, preserving order",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "without",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Return array excluding the provided values",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "sampleSize",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Return N pseudo-random elements from the array",
                args: vec![TransformerArg {
                    name: "n",
                    types: T_NUMBER,
                    required: false,
                    description: "Number of items (default: 1)",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "compact",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Remove falsy values from an array",
                args: vec![],
                doc_id: Some("compact"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "concat",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Concatenate two arrays, or concatenate a value to an array",
                args: vec![TransformerArg {
                    name: "other",
                    types: T_ARRAY,
                    required: true,
                    description: "The array (or value) to concatenate",
                }],
                doc_id: Some("concat"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "push",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Append a value to the end of an array",
                args: vec![TransformerArg {
                    name: "value",
                    types: T_ANY,
                    required: true,
                    description: "The value to append",
                }],
                doc_id: Some("push"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "join",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_STRING,
                description: "Join array elements into a string, optionally with a separator",
                args: vec![TransformerArg {
                    name: "separator",
                    types: T_STRING,
                    required: false,
                    description: "Separator string (default: ',')",
                }],
                doc_id: Some("join"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "pick",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Extract the value of a named attribute from each object in an array",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "Name of the attribute to extract from each object",
                }],
                doc_id: Some("pick"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );
        m.insert(
            "indexOf",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_NUMBER,
                description: "Find the index of a value in an array, or -1 if not found",
                args: vec![TransformerArg {
                    name: "value",
                    types: T_ANY,
                    required: true,
                    description: "The value to search for",
                }],
                doc_id: Some("indexOf"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "some",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_BOOLEAN,
                description: "Check if at least one element satisfies a condition",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "Attribute name to test for truthiness",
                }],
                doc_id: Some("some"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "every",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_BOOLEAN,
                description: "Check if all elements satisfy a condition",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "Attribute name to test for truthiness",
                }],
                doc_id: Some("every"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "groupBy",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_OBJECT,
                description: "Group array elements into an object keyed by an attribute value",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "The attribute to group by",
                }],
                doc_id: Some("groupBy"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "max",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_NUMBER,
                description: "Get the maximum value from an array of numbers",
                args: vec![],
                doc_id: Some("max"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "min",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_NUMBER,
                description: "Get the minimum value from an array of numbers",
                args: vec![],
                doc_id: Some("min"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "sum",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_NUMBER,
                description: "Get the sum of all numbers in an array",
                args: vec![],
                doc_id: Some("sum"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "sumBy",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_NUMBER,
                description: "Sum numeric values optionally by attribute name",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: false,
                    description: "Attribute name to sum",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "mean",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_NUMBER,
                description: "Get the mean (average) of all numbers in an array",
                args: vec![],
                doc_id: Some("mean"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "maxByAttribute",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Get the object with the maximum value for a given attribute",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "The attribute to compare",
                }],
                doc_id: Some("maxByAttribute"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "minByAttribute",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Get the object with the minimum value for a given attribute",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "The attribute to compare",
                }],
                doc_id: Some("minByAttribute"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "sample",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Get a random element from an array",
                args: vec![],
                doc_id: Some("sample"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "shuffle",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Shuffle the elements of an array randomly",
                args: vec![],
                doc_id: Some("shuffle"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Mixed / utility transformers ─────────────────────────────────
        m.insert(
            "size",
            TransformerDescriptor {
                input_types: T_STRING_ARRAY_OBJECT,
                output_types: T_NUMBER,
                description: "Get the length of a string, number of array elements, or object keys",
                args: vec![],
                doc_id: Some("size"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "get",
            TransformerDescriptor {
                input_types: T_ARRAY_OBJECT,
                output_types: T_ANY,
                description: "Get a value by index or key",
                args: vec![TransformerArg {
                    name: "key",
                    types: T_ANY,
                    required: true,
                    description: "The index or key to retrieve",
                }],
                doc_id: Some("get"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "range",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_ARRAY,
                description: "Generate an array of numbers from start to end (exclusive)",
                args: vec![
                    TransformerArg {
                        name: "start",
                        types: T_NUMBER,
                        required: true,
                        description: "Start value (inclusive)",
                    },
                    TransformerArg {
                        name: "end",
                        types: T_NUMBER,
                        required: false,
                        description: "End value (exclusive)",
                    },
                    TransformerArg {
                        name: "step",
                        types: T_NUMBER,
                        required: false,
                        description: "Step size (default: 1)",
                    },
                ],
                doc_id: Some("range"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "contains",
            TransformerDescriptor {
                input_types: T_ARRAY_STRING,
                output_types: T_BOOLEAN,
                description: "Check if an array contains a value, or a string contains a substring",
                args: vec![TransformerArg {
                    name: "value",
                    types: T_ANY,
                    required: true,
                    description: "The value or substring to search for",
                }],
                doc_id: Some("contains"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Object transformers ──────────────────────────────────────────
        m.insert(
            "keys",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_ARRAY,
                description: "Get the keys of an object as an array",
                args: vec![],
                doc_id: Some("keys"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "keyBy",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_OBJECT,
                description: "Key an array of objects by the given attribute (stringified)",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: true,
                    description: "Attribute name to key by",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "invert",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_OBJECT,
                description: "Invert keys and values of an object (values are stringified)",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert("pickBy", TransformerDescriptor {
            input_types: T_OBJECT, output_types: T_OBJECT,
            description: "Keep object entries whose nested attribute matches a value or is truthy",
            args: vec![
                TransformerArg { name: "attribute", types: T_STRING, required: true, description: "Attribute name to test" },
                TransformerArg { name: "match", types: T_ANY, required: false, description: "Optional value to match against" },
            ],
        doc_id: None,
        output_type_kind: OutputTypeKind::Fixed,
        });
        m.insert("omitBy", TransformerDescriptor {
            input_types: T_OBJECT, output_types: T_OBJECT,
            description: "Remove object entries whose nested attribute matches a value or is truthy",
            args: vec![
                TransformerArg { name: "attribute", types: T_STRING, required: true, description: "Attribute name to test" },
                TransformerArg { name: "match", types: T_ANY, required: false, description: "Optional value to match against" },
            ],
        doc_id: None,
        output_type_kind: OutputTypeKind::Fixed,
        });
        m.insert(
            "set",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_OBJECT,
                description: "Return a new object with a dotted path set to a value",
                args: vec![
                    TransformerArg {
                        name: "path",
                        types: T_STRING,
                        required: true,
                        description: "Dotted path to set",
                    },
                    TransformerArg {
                        name: "value",
                        types: T_ANY,
                        required: true,
                        description: "Value to set",
                    },
                ],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "mergeDeep",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_OBJECT,
                description: "Deep-merge multiple objects (right-most wins for scalars)",
                args: vec![],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "countBy",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_OBJECT,
                description: "Count items grouped by attribute or value",
                args: vec![TransformerArg {
                    name: "attribute",
                    types: T_STRING,
                    required: false,
                    description: "Attribute name to count by",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "values",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_ARRAY,
                description: "Get the values of an object as an array",
                args: vec![],
                doc_id: Some("values"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "entries",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_ARRAY,
                description: "Get the entries (key-value pairs) of an object as an array",
                args: vec![],
                doc_id: Some("entries"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "fromEntries",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_OBJECT,
                description: "Construct an object from an array of [key, value] pairs",
                args: vec![],
                doc_id: Some("fromEntries"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "has",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_BOOLEAN,
                description: "Check if an object has a given key",
                args: vec![TransformerArg {
                    name: "key",
                    types: T_STRING,
                    required: true,
                    description: "The key to check for",
                }],
                doc_id: Some("has"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert("merge", TransformerDescriptor {
            input_types: T_OBJECT, output_types: T_OBJECT,
            description: "Merge two objects together, with the second object's keys taking precedence",
            args: vec![TransformerArg { name: "other", types: T_OBJECT, required: true, description: "The object to merge in" }],
        doc_id: None,
        output_type_kind: OutputTypeKind::Fixed,
        });
        m.insert(
            "omit",
            TransformerDescriptor {
                input_types: T_OBJECT,
                output_types: T_OBJECT,
                description: "Create a new object omitting specified keys",
                args: vec![TransformerArg {
                    name: "keys",
                    types: T_ARRAY,
                    required: true,
                    description: "Array of key names to omit",
                }],
                doc_id: Some("omit"),
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );

        // ── Type / utility transformers ──────────────────────────────────
        m.insert(
            "type",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_STRING,
                description: "Get the type of a value as a string",
                args: vec![],
                doc_id: Some("type"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert("isEmpty", TransformerDescriptor {
            input_types: T_ANY, output_types: T_BOOLEAN,
            description: "Check if a value is empty (null, empty string, empty array, or empty object)",
            args: vec![],
        doc_id: None,
        output_type_kind: OutputTypeKind::Fixed,
        });
        m.insert(
            "isNull",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is null",
                args: vec![],
                doc_id: Some("isNull"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isBoolean",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is a boolean",
                args: vec![],
                doc_id: Some("isBoolean"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isNumber",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is a number",
                args: vec![],
                doc_id: Some("isNumber"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isString",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is a string",
                args: vec![],
                doc_id: Some("isString"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isArray",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is an array",
                args: vec![],
                doc_id: Some("isArray"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isObject",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is a plain object",
                args: vec![],
                doc_id: Some("isObject"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isDefined",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Check if a value is not null",
                args: vec![],
                doc_id: Some("isDefined"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "coalesce",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_ANY,
                description: "Return the first non-null value from a list of arguments",
                args: vec![TransformerArg {
                    name: "fallback",
                    types: T_ANY,
                    required: true,
                    description: "Fallback value if the input is null",
                }],
                doc_id: Some("coalesce"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "isEqual",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_BOOLEAN,
                description: "Deep equality check between two values",
                args: vec![TransformerArg {
                    name: "other",
                    types: T_ANY,
                    required: true,
                    description: "Value to compare against",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "random",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_NUMBER,
                description: "Generate a pseudo-random number",
                args: vec![
                    TransformerArg {
                        name: "min",
                        types: T_NUMBER,
                        required: false,
                        description: "Minimum (default 0)",
                    },
                    TransformerArg {
                        name: "max",
                        types: T_NUMBER,
                        required: false,
                        description: "Maximum (default 1)",
                    },
                    TransformerArg {
                        name: "float",
                        types: T_BOOLEAN,
                        required: false,
                        description: "Return float when true",
                    },
                ],
                doc_id: None,
                output_type_kind: OutputTypeKind::Fixed,
            },
        );

        // ── Higher-order / lambda transformers ───────────────────────────
        m.insert(
            "map",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Transform each element of an array using a lambda expression",
                args: vec![TransformerArg {
                    name: "lambda",
                    types: T_ANY,
                    required: true,
                    description: "Lambda expression (use `this` for the element)",
                }],
                doc_id: Some("map"),
                output_type_kind: OutputTypeKind::LambdaDetermined,
            },
        );
        m.insert(
            "filter",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Keep only elements satisfying a lambda predicate",
                args: vec![TransformerArg {
                    name: "lambda",
                    types: T_ANY,
                    required: true,
                    description: "Lambda predicate (use `this` for the element)",
                }],
                doc_id: Some("filter"),
                output_type_kind: OutputTypeKind::LambdaDetermined,
            },
        );
        m.insert(
            "reduce",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Reduce an array to a single value using a lambda accumulator",
                args: vec![
                    TransformerArg {
                        name: "lambda",
                        types: T_ANY,
                        required: true,
                        description: "Lambda (use `this` and `$acc`)",
                    },
                    TransformerArg {
                        name: "initialValue",
                        types: T_ANY,
                        required: false,
                        description: "Initial accumulator value",
                    },
                ],
                doc_id: Some("reduce"),
                output_type_kind: OutputTypeKind::LambdaDetermined,
            },
        );
        m.insert(
            "apply",
            TransformerDescriptor {
                input_types: T_ANY,
                output_types: T_ANY,
                description: "Apply a lambda expression to a value, binding `this` to the input",
                args: vec![TransformerArg {
                    name: "lambda",
                    types: T_ANY,
                    required: true,
                    description: "Lambda expression to apply",
                }],
                doc_id: Some("apply"),
                output_type_kind: OutputTypeKind::Fixed,
            },
        );
        m.insert(
            "sortBy",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Sort an array using a lambda key extractor",
                args: vec![
                    TransformerArg {
                        name: "lambda",
                        types: T_ANY,
                        required: true,
                        description: "Lambda returning the sort key (use `this`)",
                    },
                    TransformerArg {
                        name: "order",
                        types: T_NUMBER,
                        required: false,
                        description: "Sort order: omit or 1 for ascending (default), -1 for descending",
                    },
                ],
                doc_id: Some("sortBy"),
                output_type_kind: OutputTypeKind::LambdaDetermined,
            },
        );
        m.insert(
            "find",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ANY,
                description: "Find the first element satisfying a lambda predicate, or null",
                args: vec![TransformerArg {
                    name: "lambda",
                    types: T_ANY,
                    required: true,
                    description: "Lambda predicate (use `this`)",
                }],
                doc_id: Some("find"),
                output_type_kind: OutputTypeKind::ElementOfInput,
            },
        );
        m.insert("findIndex", TransformerDescriptor {
            input_types: T_ARRAY, output_types: T_NUMBER,
            description: "Find the index of the first element satisfying a lambda predicate, or -1",
            args: vec![TransformerArg { name: "lambda", types: T_ANY, required: true, description: "Lambda predicate (use `this`)" }],
        doc_id: None,
        output_type_kind: OutputTypeKind::LambdaDetermined,
        });
        m.insert(
            "any",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_BOOLEAN,
                description: "Check if at least one element satisfies a lambda predicate",
                args: vec![TransformerArg {
                    name: "lambda",
                    types: T_ANY,
                    required: true,
                    description: "Lambda predicate (use `this`)",
                }],
                doc_id: Some("any"),
                output_type_kind: OutputTypeKind::LambdaDetermined,
            },
        );
        m.insert(
            "all",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_BOOLEAN,
                description: "Check if all elements satisfy a lambda predicate",
                args: vec![TransformerArg {
                    name: "lambda",
                    types: T_ANY,
                    required: true,
                    description: "Lambda predicate (use `this`)",
                }],
                doc_id: Some("all"),
                output_type_kind: OutputTypeKind::LambdaDetermined,
            },
        );
        m.insert(
            "filterTransform",
            TransformerDescriptor {
                input_types: T_ARRAY,
                output_types: T_ARRAY,
                description: "Keep only elements for which a named transformer returns truthy",
                args: vec![TransformerArg {
                    name: "transformer",
                    types: T_STRING,
                    required: true,
                    description: "Name of the transformer to use as predicate",
                }],
                doc_id: None,
                output_type_kind: OutputTypeKind::ArrayPreservingElements,
            },
        );

        m
    });

/// All built-in variable descriptors.
pub static BUILTIN_VARIABLES: LazyLock<HashMap<&'static str, BuiltinVariableDescriptor>> =
    LazyLock::new(|| {
        let mut m = HashMap::new();
        m.insert(
            "$now",
            BuiltinVariableDescriptor {
                output_type: Integer,
                description: "Current local time as a Unix timestamp in seconds",
                doc_id: None,
            },
        );
        m.insert(
            "$now_utc",
            BuiltinVariableDescriptor {
                output_type: Integer,
                description: "Current UTC time as a Unix timestamp in seconds",
                doc_id: None,
            },
        );
        m.insert("index", BuiltinVariableDescriptor {
            output_type: Number,
            description: "Zero-based position of the current element inside map/filter/find/sortBy/any/all lambdas",
        doc_id: None,
        });
        m.insert(
            "this",
            BuiltinVariableDescriptor {
                output_type: Any,
                description: "Current element in map/filter/find/sortBy/any/all lambdas",
                doc_id: None,
            },
        );
        m.insert(
            "acc",
            BuiltinVariableDescriptor {
                output_type: Any,
                description: "Accumulator value in reduce() lambdas",
                doc_id: None,
            },
        );
        m
    });

/// All built-in function descriptors (not transformers, called as `fn(args)`).
pub static BUILTIN_FUNCTIONS: LazyLock<HashMap<&'static str, BuiltinFunctionDescriptor>> =
    LazyLock::new(|| {
        let mut m = HashMap::new();
        m.insert(
            "date",
            BuiltinFunctionDescriptor {
                output_type: Number,
                description: "Parse a date string and return a Unix timestamp (seconds)",
                syntax: "date(dateString, format?)",
                args: vec![
                    TransformerArg {
                        name: "dateString",
                        types: T_STRING,
                        required: true,
                        description: "Date string to parse",
                    },
                    TransformerArg {
                        name: "format",
                        types: T_STRING,
                        required: false,
                        description: "Optional format pattern",
                    },
                ],
                doc_id: None,
            },
        );
        m.insert(
            "datetime",
            BuiltinFunctionDescriptor {
                output_type: Number,
                description: "Parse a datetime string and return a Unix timestamp (seconds)",
                syntax: "datetime(datetimeString, format?)",
                args: vec![
                    TransformerArg {
                        name: "datetimeString",
                        types: T_STRING,
                        required: true,
                        description: "Datetime string to parse",
                    },
                    TransformerArg {
                        name: "format",
                        types: T_STRING,
                        required: false,
                        description: "Optional format pattern",
                    },
                ],
                doc_id: None,
            },
        );
        m.insert(
            "duration",
            BuiltinFunctionDescriptor {
                output_type: Integer,
                description: "Convert an amount and unit into a number of seconds",
                syntax: "duration(amount, unit)",
                args: vec![
                    TransformerArg {
                        name: "amount",
                        types: T_NUMBER,
                        required: true,
                        description: "Numeric amount",
                    },
                    TransformerArg {
                        name: "unit",
                        types: T_STRING,
                        required: true,
                        description: "seconds | minutes | hours | days | weeks | months | years",
                    },
                ],
                doc_id: None,
            },
        );
        m
    });

/// Keywords available in the language.
pub static KEYWORDS: &[&str] = &["true", "false", "null"];

/// Special built-in variable names available in all contexts.
pub static BUILTIN_VARIABLE_NAMES: &[&str] = &["$now", "$now_utc", "acc", "this", "index"];
