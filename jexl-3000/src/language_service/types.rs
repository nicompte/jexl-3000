use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

/// Primitive types used for type inference and filtering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PrimitiveType {
    String,
    Number,
    Integer,
    Boolean,
    Array,
    Object,
    Null,
    Any,
    Date,
    Datetime,
}

impl PrimitiveType {
    /// Check whether `self` is compatible with `other` for type-filtering purposes.
    /// `Any` is compatible with everything.
    #[must_use] 
    pub fn compatible_with(self, other: Self) -> bool {
        self == Self::Any || other == Self::Any || self == other
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => write!(f, "string"),
            Self::Number => write!(f, "number"),
            Self::Integer => write!(f, "integer"),
            Self::Boolean => write!(f, "boolean"),
            Self::Array => write!(f, "array"),
            Self::Object => write!(f, "object"),
            Self::Null => write!(f, "null"),
            Self::Any => write!(f, "any"),
            Self::Date => write!(f, "date"),
            Self::Datetime => write!(f, "datetime"),
        }
    }
}

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}

/// A diagnostic message attached to a byte-offset range in the expression.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnostic {
    pub message: String,
    pub severity: DiagnosticSeverity,
    /// Start byte offset in the expression.
    pub start: usize,
    /// End byte offset in the expression.
    pub end: usize,
    /// Suggested code actions (quick fixes) for this diagnostic.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub code_actions: Vec<CodeAction>,
}

/// Kind of completion item, used for UI categorization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CompletionKind {
    Function,
    Property,
    Variable,
    Keyword,
}

/// A single completion suggestion.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insert_text: Option<String>,
    pub sort_order: u32,
    /// When true, `insert_text` contains snippet syntax (e.g. `${1:placeholder}`).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_snippet: bool,
}

/// A combined result of completion and inline validation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompletionResult {
    pub items: Vec<CompletionItem>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Information about an element at a specific position, suitable for hover display.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HoverInfo {
    /// Markdown-formatted content.
    pub content: String,
    /// Byte-offset range of the hovered token.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<(usize, usize)>,
    /// Optional doc entry ID for linking to the documentation panel.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub doc_id: Option<String>,
}

/// Information about a single parameter in a signature.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParameterInfo {
    /// Display label for this parameter (e.g. `separator: string`).
    pub label: String,
    /// Optional documentation for this parameter.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
}

/// Signature help information for a transform at the cursor position.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignatureHelp {
    /// The full signature label (e.g. `replace(pattern: string, replacement: string)`).
    pub label: String,
    /// Optional description of the transform.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
    /// The parameters in this signature.
    pub parameters: Vec<ParameterInfo>,
    /// The index of the currently active parameter (0-based).
    pub active_parameter: usize,
}

/// A suggested code action attached to a diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CodeAction {
    /// Human-readable title (e.g. `Did you mean 'uppercase'?`).
    pub title: String,
    /// The replacement text.
    pub replacement: String,
    /// Start byte offset in the expression.
    pub start: usize,
    /// End byte offset in the expression.
    pub end: usize,
}

/// An indexed entry for a single dot-path in the schema.
#[derive(Debug, Clone)]
pub struct SchemaEntry {
    /// The raw JSON Schema node at this path.
    pub schema_node: Value,
    /// Whether the property is listed in the parent's `required` array.
    pub required: bool,
    /// Whether the schema allows null (via `nullable: true` or `type` includes `"null"`).
    pub nullable: bool,
    /// Direct child property names (for `type: "object"`).
    pub properties: Vec<String>,
    /// Child schemas keyed by property name.
    pub property_schemas: HashMap<String, Value>,
    /// For `type: "array"`, the `items` schema.
    pub array_item_schema: Option<Value>,
    /// Enum values from `"enum": [...]` in the schema.
    pub enum_values: Vec<Value>,
}

/// A flattened schema index mapping dot-paths to their metadata.
pub type SchemaIndex = HashMap<String, SchemaEntry>;

/// Infer `PrimitiveType` from a JSON Schema `type` field value.
#[must_use] 
pub fn schema_type_to_primitive(type_val: &Value) -> Vec<PrimitiveType> {
    match type_val {
        Value::String(s) => match s.as_str() {
            "string" => vec![PrimitiveType::String],
            "number" => vec![PrimitiveType::Number],
            "integer" => vec![PrimitiveType::Integer],
            "boolean" => vec![PrimitiveType::Boolean],
            "array" => vec![PrimitiveType::Array],
            "object" => vec![PrimitiveType::Object],
            "null" => vec![PrimitiveType::Null],
            _ => vec![PrimitiveType::Any],
        },
        Value::Array(arr) => arr
            .iter()
            .flat_map(schema_type_to_primitive)
            .collect(),
        _ => vec![PrimitiveType::Any],
    }
}

/// Infer `PrimitiveType` from a runtime `serde_json::Value`.
#[must_use] 
pub const fn value_to_primitive_type(val: &Value) -> PrimitiveType {
    match val {
        Value::String(_) => PrimitiveType::String,
        Value::Number(_) => PrimitiveType::Number,
        Value::Bool(_) => PrimitiveType::Boolean,
        Value::Array(_) => PrimitiveType::Array,
        Value::Object(_) => PrimitiveType::Object,
        Value::Null => PrimitiveType::Null,
    }
}
