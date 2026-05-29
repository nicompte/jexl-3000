use super::registry::{
    BUILTIN_FUNCTIONS, BUILTIN_VARIABLE_NAMES, BUILTIN_VARIABLES, KEYWORDS, OutputTypeKind,
    TRANSFORMER_REGISTRY,
};
use super::schema::{normalize_path, resolve_array_item_properties, resolve_path, types_for_path};
use super::types::{
    CompletionItem, CompletionKind, PrimitiveType, SchemaIndex, schema_type_to_primitive,
    value_to_primitive_type,
};
use serde_json::Value;

/// The result of analyzing the cursor context in an expression string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnalysisResult {
    /// Cursor is after a `.` — completing a property on `object`.
    PropertyAccess { object: String, partial: String },
    /// Cursor is after `|` — completing a transformer name.
    Transformer {
        partial: String,
        base_expression: String,
    },
    /// Cursor is inside a lambda expression (e.g. `map(this.___)`).
    TransformerProperty {
        transformer: String,
        base_expression: String,
        partial: String,
    },
    /// Cursor is on a bare identifier.
    Identifier { partial: String },
    /// Cursor is in a function argument position.
    FunctionArg { function: String },
    /// Cursor is inside a filter predicate: `items[.partial`
    FilterPredicate { array_path: String, partial: String },
    /// Cursor is after a comparison/arithmetic operator: `field == partial`
    OperatorRhs {
        operator: String,
        partial: String,
        lhs_expression: String,
    },
    /// Cursor is completing a property on `acc` inside `reduce()`: `acc.partial`
    AccumulatorProperty { partial: String },
    /// Cannot determine context.
    Generic,
}

/// Analyze the cursor context at a given byte offset in the expression.
///
/// Uses regex-like prefix analysis (same approach as the TS `analyzeContext`).
#[must_use]
pub fn analyze_context(expr: &str, offset: usize) -> AnalysisResult {
    let prefix = &expr[..offset.min(expr.len())];
    let trimmed = prefix.trim_end();

    // Check for `this.partial` inside a lambda (e.g., `map(this.na`)
    if let Some(result) = check_transformer_property(trimmed) {
        return result;
    }

    // Check for `acc.partial` inside a reduce lambda
    if let Some(result) = check_accumulator_property(trimmed) {
        return result;
    }

    // Check for filter predicate: `items[.partial`
    if let Some(result) = check_filter_predicate(trimmed) {
        return result;
    }

    // Check for property access: `something.partial`
    if let Some(result) = check_property_access(trimmed) {
        return result;
    }

    // Check for transformer: `something | partial`
    if let Some(result) = check_transformer(trimmed) {
        return result;
    }

    // Check for function argument: `funcname(`
    if let Some(result) = check_function_arg(trimmed) {
        return result;
    }

    // Check for operator RHS: `field == partial`
    if let Some(result) = check_operator_rhs(trimmed) {
        return result;
    }

    // Fall back to identifier completion
    let partial = extract_trailing_identifier(trimmed);
    if !partial.is_empty() {
        return AnalysisResult::Identifier {
            partial: partial.to_string(),
        };
    }

    AnalysisResult::Generic
}

fn check_transformer_property(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `transformer(this.partial` or `transformer(this.`
    // Look for `word(this.` pattern using simple manual matching instead of pulling in regex crate
    if let Some(this_dot_pos) = prefix.rfind("this.") {
        let after_this_dot = &prefix[this_dot_pos + 5..];
        // Check that after `this.` is only an identifier partial (or empty)
        let partial = extract_trailing_identifier(after_this_dot);
        if partial.len() == after_this_dot.len() {
            // Now find the enclosing transformer name
            let before = &prefix[..this_dot_pos];
            if let Some(transformer) = find_enclosing_transformer(before) {
                // Find the base expression (what's piped to the transformer)
                let base = find_base_expression_before_transformer(before, &transformer);
                return Some(AnalysisResult::TransformerProperty {
                    transformer,
                    base_expression: base,
                    partial: partial.to_string(),
                });
            }
        }
    }
    None
}

fn check_accumulator_property(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `acc.partial` inside a reduce lambda
    if let Some(acc_dot_pos) = prefix.rfind("acc.") {
        let after_acc_dot = &prefix[acc_dot_pos + 4..];
        let partial = extract_trailing_identifier(after_acc_dot);
        if partial.len() == after_acc_dot.len() {
            // Check that we're inside a reduce( by looking for an unclosed paren
            let before = &prefix[..acc_dot_pos];
            if is_inside_reduce(before) {
                return Some(AnalysisResult::AccumulatorProperty {
                    partial: partial.to_string(),
                });
            }
        }
    }
    None
}

/// Check if the cursor is inside an unclosed `reduce(` call.
fn is_inside_reduce(before: &str) -> bool {
    let mut paren_depth = 0i32;
    for (i, c) in before.char_indices().rev() {
        match c {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    let before_paren = before[..i].trim_end();
                    let name = extract_trailing_identifier(before_paren);
                    return name == "reduce";
                }
            }
            _ => {}
        }
    }
    false
}

fn check_property_access(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `something.partial` — last char sequence after a dot
    if let Some(dot_pos) = prefix.rfind('.') {
        let after_dot = &prefix[dot_pos + 1..];
        let partial = extract_trailing_identifier(after_dot);
        // Ensure everything after the dot is the partial (no operators in between)
        if partial.len() == after_dot.len() {
            let before_dot = prefix[..dot_pos].trim_end();
            if !before_dot.is_empty() {
                // Build the object path from before the dot
                let object = extract_trailing_expression(before_dot);
                if !object.is_empty() {
                    return Some(AnalysisResult::PropertyAccess {
                        object,
                        partial: partial.to_string(),
                    });
                }
            }
        }
    }
    None
}

fn check_transformer(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `something | partial`
    if let Some(pipe_pos) = prefix.rfind('|') {
        let after_pipe = prefix[pipe_pos + 1..].trim_start();
        let partial = extract_trailing_identifier(after_pipe);
        // Make sure everything after the pipe is just the partial
        if partial.len() == after_pipe.len() {
            // Don't match `||` (logical OR)
            if pipe_pos > 0 && prefix.as_bytes().get(pipe_pos - 1) == Some(&b'|') {
                return None;
            }
            let before_pipe = prefix[..pipe_pos].trim_end();
            let base_expression = extract_trailing_expression(before_pipe);
            return Some(AnalysisResult::Transformer {
                partial: partial.to_string(),
                base_expression,
            });
        }
    }
    None
}

fn check_function_arg(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `funcname(` at the end
    if let Some(before_paren) = prefix.strip_suffix('(').map(str::trim_end) {
        let func = extract_trailing_identifier(before_paren);
        if !func.is_empty() && BUILTIN_FUNCTIONS.contains_key(func) {
            return Some(AnalysisResult::FunctionArg {
                function: func.to_string(),
            });
        }
    }
    None
}

fn check_filter_predicate(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `array_path[...stuff .partial` — unclosed bracket with a `.` property
    // E.g. "order.items[.na", "items[.price > 100 && .na"
    // Find the last unclosed `[` in prefix
    let mut bracket_depth = 0i32;
    let mut last_open_bracket = None;
    for (i, c) in prefix.char_indices() {
        match c {
            '[' => {
                bracket_depth += 1;
                last_open_bracket = Some(i);
            }
            ']' => {
                bracket_depth -= 1;
                last_open_bracket = None;
            }
            _ => {}
        }
    }

    // Only proceed if there's an unclosed `[`
    let bracket_pos = last_open_bracket?;
    if bracket_depth <= 0 {
        return None;
    }

    // Inside the bracket, find the last `.partial` pattern
    let inside_bracket = &prefix[bracket_pos + 1..];
    let last_dot = inside_bracket.rfind('.')?;
    let after_dot = &inside_bracket[last_dot + 1..];

    // Everything after the last dot should be a partial identifier (or empty)
    let partial = extract_trailing_identifier(after_dot);
    if partial.len() != after_dot.len() {
        return None;
    }

    // Extract the array path before the `[`
    let before_bracket = prefix[..bracket_pos].trim_end();
    let array_path = extract_trailing_expression(before_bracket);
    if array_path.is_empty() {
        return None;
    }

    Some(AnalysisResult::FilterPredicate {
        array_path: normalize_path(&array_path),
        partial: partial.to_string(),
    })
}

fn check_operator_rhs(prefix: &str) -> Option<AnalysisResult> {
    // Pattern: `... op partial` where op is ==, !=, <=, >=, <, >, &&, ||
    // Look for a comparison/logical operator followed by optional whitespace and a partial
    let bytes = prefix.as_bytes();
    let len = bytes.len();

    // Extract trailing partial identifier
    let partial = extract_trailing_identifier(prefix);
    let before_partial = prefix[..len - partial.len()].trim_end();

    if before_partial.is_empty() {
        return None;
    }

    // Check if what's left ends with an operator
    let operators = &["==", "!=", "<=", ">=", "&&", "||", "<", ">"];
    for op in operators {
        if let Some(before_op) = before_partial.strip_suffix(op).map(str::trim_end) {
            let lhs = extract_trailing_expression(before_op);
            return Some(AnalysisResult::OperatorRhs {
                operator: op.to_string(),
                partial: partial.to_string(),
                lhs_expression: lhs,
            });
        }
    }

    None
}

/// Extract a trailing identifier (alphanumeric + _ + $) from the end of a string.
fn extract_trailing_identifier(s: &str) -> &str {
    let bytes = s.as_bytes();
    let mut i = bytes.len();
    while i > 0 {
        let c = bytes[i - 1];
        if c.is_ascii_alphanumeric() || c == b'_' || c == b'$' {
            i -= 1;
        } else {
            break;
        }
    }
    &s[i..]
}

/// Extract a trailing expression (identifier chain with dots) from the end of a string.
fn extract_trailing_expression(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut i = bytes.len();
    let mut bracket_depth = 0i32;
    let mut paren_depth = 0i32;
    while i > 0 {
        let c = bytes[i - 1];
        if c == b']' {
            bracket_depth += 1;
            i -= 1;
        } else if c == b'[' && bracket_depth > 0 {
            bracket_depth -= 1;
            i -= 1;
        } else if bracket_depth > 0 {
            // Inside brackets, consume anything
            i -= 1;
        } else if c == b')' {
            paren_depth += 1;
            i -= 1;
        } else if c == b'(' && paren_depth > 0 {
            paren_depth -= 1;
            i -= 1;
        } else if paren_depth > 0 {
            // Inside parens (e.g., transform args), consume anything
            i -= 1;
        } else if (c == b'"' || c == b'\'') && bracket_depth == 0 && paren_depth == 0 {
            // End of a string literal — walk backwards to the matching opening quote.
            let quote = c;
            i -= 1;
            while i > 0 {
                i -= 1;
                if bytes[i] == quote {
                    // Found the opening quote; stop here (i points to it).
                    break;
                }
            }
        } else if c.is_ascii_alphanumeric() || c == b'_' || c == b'$' || c == b'.' {
            i -= 1;
        } else if c == b'|' {
            // Include single `|` (pipe / transform), but NOT `||` (logical OR)
            if i >= 2 && bytes[i - 2] == b'|' {
                break;
            }
            i -= 1;
        } else if c == b' ' || c == b'\t' {
            // Include whitespace only if it's between parts of a pipe chain.
            // Peek backwards past the whitespace to see if a pipe or identifier continues.
            let mut j = i - 1;
            while j > 0 && (bytes[j - 1] == b' ' || bytes[j - 1] == b'\t') {
                j -= 1;
            }
            if j > 0
                && (bytes[j - 1] == b'|'
                    || bytes[j - 1].is_ascii_alphanumeric()
                    || bytes[j - 1] == b'_'
                    || bytes[j - 1] == b')'
                    || bytes[j - 1] == b']')
            {
                i -= 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    s[i..].to_string()
}

/// Find the transformer name that encloses the current position.
/// Looks for `transformerName(` pattern before the current position.
fn find_enclosing_transformer(before: &str) -> Option<String> {
    let trimmed = before.trim_end();
    // Walk back to find `name(`
    if let Some(paren_pos) = trimmed.rfind('(') {
        let before_paren = trimmed[..paren_pos].trim_end();
        let name = extract_trailing_identifier(before_paren);
        if !name.is_empty() {
            return Some(name.to_string());
        }
    }
    None
}

/// Find the base expression piped to a transformer.
/// E.g., in `items | map(this.`, returns `"items"`.
fn find_base_expression_before_transformer(before: &str, _transformer: &str) -> String {
    // Look for `| transformerName(` and extract what's before the pipe
    if let Some(pipe_pos) = before.rfind('|') {
        let before_pipe = before[..pipe_pos].trim_end();
        return extract_trailing_expression(before_pipe);
    }
    String::new()
}

/// Generate completion items for the given expression at the given offset.
#[must_use]
pub fn completions(
    expr: &str,
    offset: usize,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let analysis = analyze_context(expr, offset);
    let is_acc_property = matches!(analysis, AnalysisResult::AccumulatorProperty { .. });

    let prefix = &expr[..offset.min(expr.len())];
    let mut result = match analysis {
        AnalysisResult::PropertyAccess { object, partial } => {
            complete_property_access(&object, &partial, schema_index, context)
        }
        AnalysisResult::Transformer {
            partial,
            base_expression,
        } => complete_transformer(&partial, &base_expression, schema_index, context),
        AnalysisResult::TransformerProperty {
            base_expression,
            partial,
            ..
        } => complete_transformer_property(&base_expression, &partial, schema_index, context),
        AnalysisResult::Identifier { partial } => {
            complete_identifier(&partial, schema_index, context)
        }
        AnalysisResult::FilterPredicate {
            array_path,
            partial,
        } => complete_filter_predicate(&array_path, &partial, schema_index, context),
        AnalysisResult::OperatorRhs {
            partial,
            lhs_expression,
            ..
        } => complete_operator_rhs(&partial, &lhs_expression, schema_index, context),
        AnalysisResult::AccumulatorProperty { partial } => {
            complete_accumulator_property(prefix, &partial)
        }
        AnalysisResult::FunctionArg { .. } | AnalysisResult::Generic => {
            complete_generic(schema_index, context)
        }
    };

    // Inject lambda variables when inside a lambda context
    // (but not for AccumulatorProperty — user is already past variable-level completion)
    if !is_acc_property {
        inject_lambda_variables(prefix, &mut result);
    }

    result
}

/// Complete properties after a `.`
fn complete_property_access(
    object: &str,
    partial: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Try schema first
    if let Some(index) = schema_index {
        if let Some(entry) = resolve_path(object, index) {
            // Object properties
            for prop in &entry.properties {
                if !partial.is_empty() && !prop.starts_with(partial) {
                    continue;
                }
                let detail = entry
                    .property_schemas
                    .get(prop)
                    .and_then(|s| s.get("type"))
                    .map(format_type);
                items.push(CompletionItem {
                    label: prop.clone(),
                    kind: CompletionKind::Property,
                    detail,
                    insert_text: None,
                    sort_order: 0,
                    is_snippet: false,
                });
            }
            // Array item properties
            if let Some(item_schema) = &entry.array_item_schema
                && let Some(props) = item_schema.get("properties").and_then(|p| p.as_object())
            {
                for (prop, prop_schema) in props {
                    if !partial.is_empty() && !prop.starts_with(partial) {
                        continue;
                    }
                    let detail = prop_schema.get("type").map(format_type);
                    items.push(CompletionItem {
                        label: prop.clone(),
                        kind: CompletionKind::Property,
                        detail,
                        insert_text: None,
                        sort_order: 0,
                        is_snippet: false,
                    });
                }
            }
        }

        // If no results from direct path, try transform output type resolution.
        // E.g. `items | first.` → resolve `first` as ElementOfInput on `items`.
        if items.is_empty() {
            items = resolve_transform_output_properties(object, partial, index);
        }
    }

    // Fallback: context introspection
    if items.is_empty()
        && let Some(ctx) = context
    {
        let val = navigate_context(ctx, object);
        if let Some(obj) = val.and_then(|v| v.as_object()) {
            for key in obj.keys() {
                if !partial.is_empty() && !key.starts_with(partial) {
                    continue;
                }
                let detail = obj.get(key).map(|v| value_to_primitive_type(v).to_string());
                items.push(CompletionItem {
                    label: key.clone(),
                    kind: CompletionKind::Property,
                    detail,
                    insert_text: None,
                    sort_order: 0,
                    is_snippet: false,
                });
            }
        }
    }

    items
}

/// Complete properties inside a filter predicate `array[.partial`
fn complete_filter_predicate(
    array_path: &str,
    partial: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Use resolve_array_item_properties to get item fields
    if let Some(index) = schema_index {
        let props = resolve_array_item_properties(array_path, index);
        for (prop_name, type_str) in props {
            if !partial.is_empty() && !prop_name.starts_with(partial) {
                continue;
            }
            items.push(CompletionItem {
                label: format!(".{prop_name}"),
                kind: CompletionKind::Property,
                detail: type_str,
                insert_text: Some(prop_name),
                sort_order: 0,
                is_snippet: false,
            });
        }
    }

    // Fallback to context introspection
    if items.is_empty()
        && let Some(ctx) = context
        && let Some(arr) = navigate_context(ctx, array_path).and_then(|v| v.as_array())
        && let Some(first) = arr.first().and_then(|v| v.as_object())
    {
        for key in first.keys() {
            if !partial.is_empty() && !key.starts_with(partial) {
                continue;
            }
            let detail = first
                .get(key)
                .map(|v| value_to_primitive_type(v).to_string());
            items.push(CompletionItem {
                label: format!(".{key}"),
                kind: CompletionKind::Property,
                detail,
                insert_text: Some(key.clone()),
                sort_order: 0,
                is_snippet: false,
            });
        }
    }

    items
}

/// Complete the RHS of a comparison operator.
/// Returns identifier completions plus any enum values from the LHS schema.
fn complete_operator_rhs(
    partial: &str,
    lhs_expression: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let mut items = complete_identifier(partial, schema_index, context);

    // If LHS has enum values, inject them as high-priority completions
    if !lhs_expression.is_empty()
        && let Some(index) = schema_index
    {
        let normalized = normalize_path(lhs_expression);
        if let Some(entry) = resolve_path(&normalized, index) {
            for val in &entry.enum_values {
                let label = match val {
                    Value::String(s) => format!("\"{s}\""),
                    other => other.to_string(),
                };
                if !partial.is_empty() && !label.starts_with(partial) {
                    continue;
                }
                items.push(CompletionItem {
                    label: label.clone(),
                    kind: CompletionKind::Variable,
                    detail: Some("enum value".to_string()),
                    insert_text: Some(label),
                    sort_order: 0,
                    is_snippet: false,
                });
            }
        }
    }

    items
}

/// Complete transformer names after `|`
fn complete_transformer(
    partial: &str,
    base_expression: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let registry = &*TRANSFORMER_REGISTRY;

    // Try to infer the LHS type for filtering
    let lhs_types = infer_lhs_types(base_expression, schema_index, context);

    for (name, descriptor) in registry {
        if !partial.is_empty() && !name.starts_with(partial) {
            continue;
        }

        // Filter by input type compatibility
        if let Some(ref types) = lhs_types
            && !descriptor.input_types.contains(&PrimitiveType::Any)
        {
            let compatible = types.iter().any(|t| {
                *t == PrimitiveType::Any
                    || descriptor
                        .input_types
                        .iter()
                        .any(|it| it.compatible_with(*t))
            });
            if !compatible {
                continue;
            }
        }

        let detail = Some(format_transformer_signature(name, descriptor));
        let (insert_text, is_snippet) = build_transform_snippet(name, descriptor);
        items.push(CompletionItem {
            label: name.to_string(),
            kind: CompletionKind::Function,
            detail,
            insert_text,
            sort_order: 10,
            is_snippet,
        });
    }

    items.sort_by(|a, b| a.label.cmp(&b.label));
    items
}

/// Complete `this.xxx` properties inside lambda transforms.
fn complete_transformer_property(
    base_expression: &str,
    partial: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // The base expression is the array being transformed.
    // We need the array item schema.
    if let Some(index) = schema_index {
        // Try `base.__arrayItem__` or just `base` with items
        let array_item_path = format!("{base_expression}.__arrayItem__");
        let entry = resolve_path(&array_item_path, index).or_else(|| {
            resolve_path(base_expression, index).filter(|e| e.array_item_schema.is_some())
        });

        if let Some(entry) = entry {
            // If we resolved to the array itself, use its item schema properties
            if entry.array_item_schema.is_some() && entry.properties.is_empty() {
                if let Some(item_schema) = &entry.array_item_schema
                    && let Some(props) = item_schema.get("properties").and_then(|p| p.as_object())
                {
                    for (prop, prop_schema) in props {
                        if !partial.is_empty() && !prop.starts_with(partial) {
                            continue;
                        }
                        let detail = prop_schema.get("type").map(format_type);
                        items.push(CompletionItem {
                            label: prop.clone(),
                            kind: CompletionKind::Property,
                            detail,
                            insert_text: None,
                            sort_order: 0,
                            is_snippet: false,
                        });
                    }
                }
            } else {
                for prop in &entry.properties {
                    if !partial.is_empty() && !prop.starts_with(partial) {
                        continue;
                    }
                    let detail = entry
                        .property_schemas
                        .get(prop)
                        .and_then(|s| s.get("type"))
                        .map(format_type);
                    items.push(CompletionItem {
                        label: prop.clone(),
                        kind: CompletionKind::Property,
                        detail,
                        insert_text: None,
                        sort_order: 0,
                        is_snippet: false,
                    });
                }
            }
        }
    }

    // Fallback: context
    if items.is_empty()
        && let Some(ctx) = context
    {
        let val = navigate_context(ctx, base_expression);
        if let Some(arr) = val.and_then(|v| v.as_array())
            && let Some(first) = arr.first().and_then(|v| v.as_object())
        {
            for key in first.keys() {
                if !partial.is_empty() && !key.starts_with(partial) {
                    continue;
                }
                items.push(CompletionItem {
                    label: key.clone(),
                    kind: CompletionKind::Property,
                    detail: None,
                    insert_text: None,
                    sort_order: 0,
                    is_snippet: false,
                });
            }
        }
    }

    items
}

/// Complete properties on `acc.` inside a `reduce()` lambda.
///
/// Attempts to find the reduce init value by parsing the prefix and extracting
/// the init expression literal. If the init is an object literal, suggests its keys.
fn complete_accumulator_property(prefix: &str, partial: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Try to extract the reduce init value from the prefix.
    // Pattern: `reduce(INIT_EXPR, ... acc.`
    // We look for `reduce(` then try to parse the init expression.
    if let Some(init_value) = extract_reduce_init_from_prefix(prefix)
        && let Some(obj) = init_value.as_object()
    {
        for (key, val) in obj {
            if !partial.is_empty() && !key.starts_with(partial) {
                continue;
            }
            let detail = Some(super::types::value_to_primitive_type(val).to_string());
            items.push(CompletionItem {
                label: key.clone(),
                kind: CompletionKind::Property,
                detail,
                insert_text: None,
                sort_order: 0,
                is_snippet: false,
            });
        }
    }

    items
}

/// Try to extract the reduce init value from the expression prefix.
///
/// Looks for the pattern `reduce(INIT, body_with_acc.` and attempts to parse
/// `INIT` as a JSON literal.
fn extract_reduce_init_from_prefix(prefix: &str) -> Option<serde_json::Value> {
    // Find the `reduce(` that contains the cursor
    let mut paren_depth = 0i32;
    let bytes = prefix.as_bytes();
    let len = bytes.len();
    let mut i = len;

    while i > 0 {
        i -= 1;
        match bytes[i] {
            b')' => paren_depth += 1,
            b'(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    // Found the reduce's opening paren
                    let before = &prefix[..i];
                    let name = extract_trailing_identifier(before.trim_end());
                    if name == "reduce" {
                        // Everything after `reduce(` up to cursor is args
                        let args_str = &prefix[i + 1..];
                        return parse_reduce_init_arg(args_str);
                    }
                    return None;
                }
            }
            _ => {}
        }
    }
    None
}

/// Parse the init argument from a reduce args substring.
/// The args look like: `INIT_EXPR, body_expr_with_acc.partial`
/// We need to extract `INIT_EXPR` (the part before the first top-level comma).
fn parse_reduce_init_arg(args: &str) -> Option<serde_json::Value> {
    // Find the first top-level comma (not inside braces/parens/brackets)
    let mut depth = 0i32;
    for (i, c) in args.char_indices() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' => depth -= 1,
            ',' if depth == 0 => {
                let init_str = args[..i].trim();
                // Try to parse as JSON (handles numbers, strings, objects, arrays)
                if let Ok(val) = serde_json::from_str(init_str) {
                    return Some(val);
                }
                // Try as a simple number
                if let Ok(n) = init_str.parse::<f64>() {
                    return Some(serde_json::Value::Number(
                        serde_json::Number::from_f64(n)
                            .unwrap_or_else(|| serde_json::Number::from(0)),
                    ));
                }
                return None;
            }
            _ => {}
        }
    }
    None
}

/// Complete bare identifiers — schema root properties, built-in variables, keywords.
fn complete_identifier(
    partial: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Schema root properties
    if let Some(index) = schema_index
        && let Some(root) = index.get("")
    {
        for prop in &root.properties {
            if !partial.is_empty() && !prop.starts_with(partial) {
                continue;
            }
            let detail = root
                .property_schemas
                .get(prop)
                .and_then(|s| s.get("type"))
                .map(format_type);
            items.push(CompletionItem {
                label: prop.clone(),
                kind: CompletionKind::Variable,
                detail,
                insert_text: None,
                sort_order: 0,
                is_snippet: false,
            });
        }
    }

    // Context root keys as fallback
    if let Some(ctx) = context
        && let Some(obj) = ctx.as_object()
    {
        for key in obj.keys() {
            if !partial.is_empty() && !key.starts_with(partial) {
                continue;
            }
            // Skip if already from schema
            if items.iter().any(|i| i.label == *key) {
                continue;
            }
            items.push(CompletionItem {
                label: key.clone(),
                kind: CompletionKind::Variable,
                detail: None,
                insert_text: None,
                sort_order: 1,
                is_snippet: false,
            });
        }
    }

    // Built-in variables
    for name in BUILTIN_VARIABLE_NAMES {
        if !partial.is_empty() && !name.starts_with(partial) {
            continue;
        }
        let detail = BUILTIN_VARIABLES
            .get(name)
            .map(|d| d.description.to_string());
        items.push(CompletionItem {
            label: name.to_string(),
            kind: CompletionKind::Variable,
            detail,
            insert_text: None,
            sort_order: 5,
            is_snippet: false,
        });
    }

    // Keywords
    for kw in KEYWORDS {
        if !partial.is_empty() && !kw.starts_with(partial) {
            continue;
        }
        items.push(CompletionItem {
            label: kw.to_string(),
            kind: CompletionKind::Keyword,
            detail: None,
            insert_text: None,
            sort_order: 20,
            is_snippet: false,
        });
    }

    // Built-in functions
    for (name, descriptor) in BUILTIN_FUNCTIONS.iter() {
        if !partial.is_empty() && !name.starts_with(partial) {
            continue;
        }
        items.push(CompletionItem {
            label: name.to_string(),
            kind: CompletionKind::Function,
            detail: Some(descriptor.description.to_string()),
            insert_text: Some(format!("{name}(")),
            sort_order: 10,
            is_snippet: false,
        });
    }

    items
}

/// Generic completions when we can't determine context.
fn complete_generic(
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Vec<CompletionItem> {
    complete_identifier("", schema_index, context)
}

/// Infer the types of the left-hand side of a pipe expression.
///
/// Handles pipe chains by walking from left to right through `|`-separated
/// segments, resolving each transform's output type from the registry.
/// E.g. `name | split(',')` → resolve `name` as string, then `split` → array.
fn infer_lhs_types(
    base_expression: &str,
    schema_index: Option<&SchemaIndex>,
    context: Option<&Value>,
) -> Option<Vec<PrimitiveType>> {
    let registry = &*TRANSFORMER_REGISTRY;

    // Check if this is a pipe chain — resolve through each segment
    if let Some((base, last_transform)) = split_last_pipe(base_expression) {
        let transform_clean = last_transform
            .split('(')
            .next()
            .unwrap_or(last_transform)
            .trim();

        if let Some(descriptor) = registry.get(transform_clean) {
            match descriptor.output_type_kind {
                OutputTypeKind::Fixed => {
                    let out: Vec<PrimitiveType> = descriptor.output_types.to_vec();
                    if !out.contains(&PrimitiveType::Any) {
                        return Some(out);
                    }
                }
                OutputTypeKind::ElementOfInput => {
                    // Resolve the base's array element type from the schema
                    if let Some(index) = schema_index {
                        let base_trimmed = base.trim();
                        if let Some(item_types) =
                            resolve_chain_element_type(base_trimmed, index, context)
                        {
                            return Some(item_types);
                        }
                    }
                    // Fall back to static output types
                    let out: Vec<PrimitiveType> = descriptor.output_types.to_vec();
                    if !out.contains(&PrimitiveType::Any) {
                        return Some(out);
                    }
                }
                OutputTypeKind::ArrayPreservingElements => {
                    // Output is still an array — but try to preserve knowledge
                    return infer_lhs_types(base.trim(), schema_index, context);
                }
                OutputTypeKind::LambdaDetermined => {
                    // Cannot statically infer; use declared output types
                    let out: Vec<PrimitiveType> = descriptor.output_types.to_vec();
                    if !out.contains(&PrimitiveType::Any) {
                        return Some(out);
                    }
                }
            }
        }
    }

    // 1. Check if base is a known transformer → use its output type
    if let Some(descriptor) = registry.get(base_expression) {
        let out: Vec<PrimitiveType> = descriptor.output_types.to_vec();
        if out.contains(&PrimitiveType::Any) {
            return None;
        }
        return Some(out);
    }

    // 2. Number literal check
    if base_expression.parse::<f64>().is_ok() {
        return Some(vec![PrimitiveType::Number]);
    }

    // 2b. String literal check (e.g. `"2002-01-01"` or `'hello'`)
    if base_expression.len() >= 2
        && ((base_expression.starts_with('"') && base_expression.ends_with('"'))
            || (base_expression.starts_with('\'') && base_expression.ends_with('\'')))
    {
        return Some(vec![PrimitiveType::String]);
    }

    // 2c. Boolean literal check
    if base_expression == "true" || base_expression == "false" {
        return Some(vec![PrimitiveType::Boolean]);
    }

    // 3. Schema index lookup — skip empty path to avoid treating the root schema
    // as the type of an unrecognised literal expression.
    if !base_expression.is_empty()
        && let Some(index) = schema_index
        && let Some(types) = types_for_path(base_expression, index)
        && !types.contains(&PrimitiveType::Any)
    {
        return Some(types);
    }

    // 4. Runtime context evaluation
    if let Some(ctx) = context {
        let val = navigate_context(ctx, base_expression);
        if let Some(v) = val {
            return Some(vec![value_to_primitive_type(v)]);
        }
    }

    None
}

/// Resolve the element type of an array expression for `ElementOfInput` transforms.
/// Walks through pipe chains to find the underlying array path in the schema.
fn resolve_chain_element_type(
    base_expression: &str,
    index: &SchemaIndex,
    context: Option<&Value>,
) -> Option<Vec<PrimitiveType>> {
    let registry = &*TRANSFORMER_REGISTRY;

    // If base itself is a pipe chain, resolve through it
    if let Some((inner_base, inner_transform)) = split_last_pipe(base_expression) {
        let transform_clean = inner_transform
            .split('(')
            .next()
            .unwrap_or(inner_transform)
            .trim();
        if let Some(desc) = registry.get(transform_clean) {
            match desc.output_type_kind {
                OutputTypeKind::ArrayPreservingElements | OutputTypeKind::LambdaDetermined => {
                    // These keep the array — recurse to find the underlying array
                    return resolve_chain_element_type(inner_base.trim(), index, context);
                }
                _ => return None,
            }
        }
    }

    // Base is a simple path — look up its __arrayItem__ schema
    let item_path = format!("{base_expression}.__arrayItem__");
    if let Some(types) = types_for_path(&item_path, index) {
        if !types.contains(&PrimitiveType::Any) {
            return Some(types);
        }
    }

    // Try with array item resolution through resolve_path
    if let Some(entry) = resolve_path(base_expression, index) {
        if let Some(item_schema) = &entry.array_item_schema {
            if let Some(type_val) = item_schema.get("type") {
                let types = schema_type_to_primitive(type_val);
                if !types.contains(&PrimitiveType::Any) {
                    return Some(types);
                }
            }
        }
    }

    None
}

/// Navigate a JSON context by a dot-separated path.
fn navigate_context<'a>(context: &'a Value, path: &str) -> Option<&'a Value> {
    let mut current = context;
    for segment in path.split('.') {
        if segment.is_empty() {
            continue;
        }
        current = current.get(segment)?;
    }
    Some(current)
}

/// Format a JSON Schema type value as a human-readable string.
fn format_type(type_val: &Value) -> String {
    let types = schema_type_to_primitive(type_val);
    types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ")
}

/// Format a transformer signature for the detail field.
fn format_transformer_signature(
    name: &str,
    descriptor: &super::registry::TransformerDescriptor,
) -> String {
    let input = descriptor
        .input_types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ");

    if descriptor.args.is_empty() {
        format!("({input}) {name} — {}", descriptor.description)
    } else {
        let args: Vec<String> = descriptor
            .args
            .iter()
            .map(|a| {
                if a.required {
                    a.name.to_string()
                } else {
                    format!("{}?", a.name)
                }
            })
            .collect();
        format!(
            "({input}) {name}({}) — {}",
            args.join(", "),
            descriptor.description
        )
    }
}

/// Build a snippet `insert_text` for a transform with arguments.
///
/// Returns `(Some(snippet), true)` when the transform has required args,
/// `(None, false)` when it has no args at all.
fn build_transform_snippet(
    name: &str,
    descriptor: &super::registry::TransformerDescriptor,
) -> (Option<String>, bool) {
    if descriptor.args.is_empty() {
        return (None, false);
    }

    let mut parts = Vec::new();
    for (i, arg) in descriptor.args.iter().enumerate() {
        if arg.required {
            parts.push(format!("${{{}:{}}}", i + 1, arg.name));
        } else {
            // Optional args are not included in the snippet — user can add them manually.
            break;
        }
    }

    if parts.is_empty() {
        // All args are optional — just add parens with cursor inside
        return (Some(format!("{name}(${{1}})")), true);
    }

    (Some(format!("{name}({})", parts.join(", "))), true)
}

/// Resolve properties available on a transform output expression.
///
/// When user types `items | first.` or `items | reverse | first.`, this
/// function parses the pipe chain, checks the last transform's `OutputTypeKind`,
/// and if it's `ElementOfInput`, resolves the array's item properties.
fn resolve_transform_output_properties(
    object: &str,
    partial: &str,
    schema_index: &SchemaIndex,
) -> Vec<CompletionItem> {
    let inner = || -> Option<Vec<CompletionItem>> {
        let registry = &*TRANSFORMER_REGISTRY;

        // Strip balanced outer parentheses: `(movies | first)` → `movies | first`
        let stripped = strip_balanced_parens(object);

        // Parse: `base_expr | transform` (find the last pipe)
        let (base_expr, transform_name) = split_last_pipe(stripped)?;

        // Strip any arguments from the transform name (e.g., `sort('desc')` → `sort`)
        let transform_clean = transform_name
            .split('(')
            .next()
            .unwrap_or(transform_name)
            .trim();

        let descriptor = registry.get(transform_clean)?;

        match descriptor.output_type_kind {
            OutputTypeKind::ElementOfInput => {
                // The output is an element of the input array.
                // Resolve the base expression's array item properties.
                let base_trimmed = base_expr.trim();
                // The base itself might end with a transform chain — try to find the
                // underlying array path (limit to 1 level of recursion).
                let array_path = resolve_underlying_array_path(base_trimmed, schema_index);
                let path = array_path.as_deref().unwrap_or(base_trimmed);

                let props = resolve_array_item_properties(path, schema_index);
                let mut items = Vec::new();
                for (prop_name, type_str) in props {
                    if !partial.is_empty() && !prop_name.starts_with(partial) {
                        continue;
                    }
                    items.push(CompletionItem {
                        label: prop_name,
                        kind: CompletionKind::Property,
                        detail: type_str,
                        insert_text: None,
                        sort_order: 0,
                        is_snippet: false,
                    });
                }
                Some(items)
            }
            OutputTypeKind::ArrayPreservingElements => {
                // Output is an array with same element type — no direct `.` completions
                // (user would need to chain with `first`, `[0]`, etc.)
                None
            }
            _ => None,
        }
    };
    inner().unwrap_or_default()
}

/// Split an expression at the last `|` pipe into (base, `transform_name`).
fn split_last_pipe(expr: &str) -> Option<(&str, &str)> {
    // Find the last `|` that's not `||` (logical OR)
    let bytes = expr.as_bytes();
    let mut i = bytes.len();
    while i > 0 {
        i -= 1;
        if bytes[i] == b'|' {
            // Check it's not `||`
            if i > 0 && bytes[i - 1] == b'|' {
                i -= 1; // skip
                continue;
            }
            if i + 1 < bytes.len() && bytes[i + 1] == b'|' {
                continue;
            }
            let base = &expr[..i];
            let transform = expr[i + 1..].trim();
            if !transform.is_empty() && !base.is_empty() {
                return Some((base, transform));
            }
        }
    }
    None
}

/// Strip balanced outer parentheses from an expression.
///   `(movies | first)` → `movies | first`
///   `((x | y))`        → `x | y`
///   `movies | first`   → `movies | first`  (no-op)
fn strip_balanced_parens(s: &str) -> &str {
    let mut s = s.trim();
    while s.starts_with('(') && s.ends_with(')') {
        // Verify the outer parens are actually balanced with each other
        // (not `(a) + (b)` which starts with `(` and ends with `)` but aren't paired)
        let inner = &s[1..s.len() - 1];
        let mut depth = 0i32;
        let mut balanced = true;
        for b in inner.bytes() {
            match b {
                b'(' => depth += 1,
                b')' => {
                    depth -= 1;
                    if depth < 0 {
                        balanced = false;
                        break;
                    }
                }
                _ => {}
            }
        }
        if balanced && depth == 0 {
            s = inner.trim();
        } else {
            break;
        }
    }
    s
}

/// Try to resolve the underlying array path from a potentially-chained expression.
/// E.g. `items | filter(this.x > 0)` → `items`, `items | sort` → `items`
fn resolve_underlying_array_path(expr: &str, schema_index: &SchemaIndex) -> Option<String> {
    let trimmed = expr.trim();

    // Direct path — resolve directly
    if resolve_path(trimmed, schema_index).is_some() {
        return Some(trimmed.to_string());
    }

    // Has a pipe — check if the transform preserves array type
    if let Some((base, transform_name)) = split_last_pipe(trimmed) {
        let registry = &*TRANSFORMER_REGISTRY;
        let transform_clean = transform_name.split('(').next()?.trim();
        if let Some(desc) = registry.get(transform_clean) {
            match desc.output_type_kind {
                OutputTypeKind::ArrayPreservingElements | OutputTypeKind::LambdaDetermined => {
                    // Filter/sort/reverse preserve the array — try the base
                    let base_trimmed = base.trim();
                    if resolve_path(base_trimmed, schema_index).is_some() {
                        return Some(base_trimmed.to_string());
                    }
                }
                _ => {}
            }
        }
    }

    None
}

fn inject_lambda_variables(prefix: &str, items: &mut Vec<CompletionItem>) {
    // Detect lambda context by looking for unclosed `reduce(`, `map(`, `filter(`, etc.
    let lambda_transforms = [
        "reduce",
        "map",
        "filter",
        "some",
        "all",
        "sort",
        "flatMap",
        "find",
        "findIndex",
        "apply",
    ];
    let mut paren_depth = 0i32;

    // Walk backwards to find unclosed parens
    for (i, c) in prefix.char_indices().rev() {
        match c {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    // Found an unclosed `(`. Check what precedes it.
                    let before = prefix[..i].trim_end();
                    let name = extract_trailing_identifier(before);
                    if lambda_transforms.contains(&name) {
                        // Inject `index` for iterating transforms (not apply).
                        if name != "apply" && !items.iter().any(|i| i.label == "index") {
                            items.push(CompletionItem {
                                label: "index".to_string(),
                                kind: CompletionKind::Variable,
                                detail: Some("Current iteration index".to_string()),
                                insert_text: None,
                                sort_order: 2,
                                is_snippet: false,
                            });
                        }
                        // Inject `acc` only for reduce.
                        if name == "reduce" && !items.iter().any(|i| i.label == "acc") {
                            items.push(CompletionItem {
                                label: "acc".to_string(),
                                kind: CompletionKind::Variable,
                                detail: Some("Accumulator value in reduce".to_string()),
                                insert_text: None,
                                sort_order: 2,
                                is_snippet: false,
                            });
                        }
                        return;
                    }
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_analyze_property_access() {
        let result = analyze_context("customer.na", 11);
        assert!(
            matches!(result, AnalysisResult::PropertyAccess { ref object, ref partial } if object == "customer" && partial == "na")
        );
    }

    #[test]
    fn test_analyze_transformer() {
        let result = analyze_context("name | upper", 12);
        assert!(
            matches!(result, AnalysisResult::Transformer { ref partial, ref base_expression } if partial == "upper" && base_expression == "name")
        );
    }

    #[test]
    fn test_analyze_transformer_empty() {
        let result = analyze_context("name | ", 7);
        assert!(
            matches!(result, AnalysisResult::Transformer { ref partial, .. } if partial.is_empty())
        );
    }

    #[test]
    fn test_analyze_identifier() {
        let result = analyze_context("cust", 4);
        assert!(matches!(result, AnalysisResult::Identifier { ref partial } if partial == "cust"));
    }

    #[test]
    fn test_property_completions_from_schema() {
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": {
                    "type": "object",
                    "properties": {
                        "name": { "type": "string" },
                        "age": { "type": "number" }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("customer.", 9, Some(&index), None);
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "age"));
    }

    #[test]
    fn test_transformer_completions_filtered() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("name | ", 7, Some(&index), None);
        // Should include string transformers like uppercase
        assert!(items.iter().any(|i| i.label == "uppercase"));
        // Should NOT include array-only transformers like first
        assert!(!items.iter().any(|i| i.label == "first"));
    }

    #[test]
    fn test_identifier_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" },
                "age": { "type": "number" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("n", 1, Some(&index), None);
        assert!(items.iter().any(|i| i.label == "name"));
    }

    #[test]
    fn test_context_fallback() {
        let context = json!({
            "customer": {
                "name": "John",
                "email": "john@example.com"
            }
        });
        let items = completions("customer.", 9, None, Some(&context));
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "email"));
    }

    // --- Phase 1: Filter predicate tests ---

    #[test]
    fn test_analyze_filter_predicate() {
        let result = analyze_context("items[.na", 9);
        assert!(
            matches!(result, AnalysisResult::FilterPredicate { ref array_path, ref partial } if array_path == "items" && partial == "na"),
            "Expected FilterPredicate, got {result:?}"
        );
    }

    #[test]
    fn test_analyze_filter_predicate_empty_partial() {
        let result = analyze_context("items[.", 7);
        assert!(
            matches!(result, AnalysisResult::FilterPredicate { ref array_path, ref partial } if array_path == "items" && partial.is_empty()),
            "Expected FilterPredicate, got {result:?}"
        );
    }

    #[test]
    fn test_analyze_filter_predicate_nested() {
        let result = analyze_context("order.items[.pri", 16);
        assert!(
            matches!(result, AnalysisResult::FilterPredicate { ref array_path, ref partial } if array_path == "order.items" && partial == "pri"),
            "Expected FilterPredicate, got {result:?}"
        );
    }

    #[test]
    fn test_filter_predicate_completions_from_schema() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": { "type": "string" },
                            "price": { "type": "number" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("items[.", 7, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == ".name"),
            "Should have .name, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == ".price"),
            "Should have .price, got: {items:?}"
        );
    }

    #[test]
    fn test_filter_predicate_completions_from_context() {
        let context = json!({
            "items": [
                { "name": "Widget", "price": 10 },
                { "name": "Gadget", "price": 20 }
            ]
        });
        let items = completions("items[.", 7, None, Some(&context));
        assert!(
            items.iter().any(|i| i.label == ".name"),
            "Should have .name, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == ".price"),
            "Should have .price, got: {items:?}"
        );
    }

    // --- Phase 1: Operator RHS tests ---

    #[test]
    fn test_analyze_operator_rhs() {
        let result = analyze_context("age == ", 7);
        assert!(
            matches!(result, AnalysisResult::OperatorRhs { ref operator, ref partial, ref lhs_expression } if operator == "==" && partial.is_empty() && lhs_expression == "age"),
            "Expected OperatorRhs, got {result:?}"
        );
    }

    #[test]
    fn test_analyze_operator_rhs_with_partial() {
        let result = analyze_context("status != act", 13);
        assert!(
            matches!(result, AnalysisResult::OperatorRhs { ref operator, ref partial, ref lhs_expression } if operator == "!=" && partial == "act" && lhs_expression == "status"),
            "Expected OperatorRhs, got {result:?}"
        );
    }

    #[test]
    fn test_operator_rhs_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "status": { "type": "string" },
                "count": { "type": "number" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("status == ", 10, Some(&index), None);
        // Should return identifier-like completions
        assert!(
            items.iter().any(|i| i.label == "status"),
            "Should have 'status', got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "count"),
            "Should have 'count', got: {items:?}"
        );
    }

    #[test]
    fn test_enum_value_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "status": {
                    "type": "string",
                    "enum": ["active", "inactive", "pending"]
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("status == ", 10, Some(&index), None);
        // Should include enum values as completions
        assert!(
            items.iter().any(|i| i.label == "\"active\""),
            "Should have '\"active\"' enum value, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "\"inactive\""),
            "Should have '\"inactive\"' enum value, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "\"pending\""),
            "Should have '\"pending\"' enum value, got: {items:?}"
        );
        // Enum values should have high priority (sort_order 0)
        let enum_item = items.iter().find(|i| i.label == "\"active\"").unwrap();
        assert_eq!(enum_item.sort_order, 0);
    }

    #[test]
    fn test_enum_value_completions_numbers() {
        let schema = json!({
            "type": "object",
            "properties": {
                "priority": {
                    "type": "number",
                    "enum": [1, 2, 3]
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("priority == ", 12, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "1"),
            "Should have '1' enum value, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "2"),
            "Should have '2' enum value, got: {items:?}"
        );
    }

    // --- Phase 1: Lambda variable injection tests ---

    #[test]
    fn test_lambda_index_in_map() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": { "type": "array", "items": { "type": "object" } }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("items | map(", 12, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "index"),
            "Should have 'index' in map, got: {items:?}"
        );
    }

    #[test]
    fn test_lambda_acc_in_reduce() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": { "type": "array", "items": { "type": "object" } }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("items | reduce(", 15, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "acc"),
            "Should have 'acc' in reduce, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "index"),
            "Should have 'index' in reduce"
        );
    }

    #[test]
    fn test_lambda_index_with_partial() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": { "type": "array", "items": { "type": "object" } }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("items | map(ind", 15, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "index"),
            "Should have 'index' when typing 'ind' in map"
        );
    }

    // --- Phase 1: Array index normalization tests ---

    #[test]
    fn test_property_access_with_array_index() {
        let schema = json!({
            "type": "object",
            "properties": {
                "orders": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "status": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // orders[0]. should resolve to array item properties
        let items = completions("orders[0].", 10, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "id"),
            "Should have 'id' after array index, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "status"),
            "Should have 'status' after array index, got: {items:?}"
        );
    }

    #[test]
    fn test_extract_trailing_expression_with_brackets() {
        assert_eq!(extract_trailing_expression("orders[0]"), "orders[0]");
        assert_eq!(
            extract_trailing_expression("a.orders[0].name"),
            "a.orders[0].name"
        );
        assert_eq!(extract_trailing_expression("x + items[0]"), "items[0]");
    }

    // --- Phase 2: Enum value completion tests ---

    #[test]
    fn test_enum_completions_in_operator_rhs() {
        let schema = json!({
            "type": "object",
            "properties": {
                "status": {
                    "type": "string",
                    "enum": ["active", "inactive", "pending"]
                },
                "count": { "type": "number" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("status == ", 10, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "\"active\""),
            "Should have '\"active\"', got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "\"inactive\""),
            "Should have '\"inactive\"'"
        );
        assert!(
            items.iter().any(|i| i.label == "\"pending\""),
            "Should have '\"pending\"'"
        );
        // Should also have identifier completions
        assert!(
            items.iter().any(|i| i.label == "status"),
            "Should still have identifier completions"
        );
    }

    #[test]
    fn test_enum_completions_numeric() {
        let schema = json!({
            "type": "object",
            "properties": {
                "priority": {
                    "type": "number",
                    "enum": [1, 2, 3]
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("priority == ", 12, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "1"),
            "Should have '1', got: {items:?}"
        );
        assert!(items.iter().any(|i| i.label == "2"), "Should have '2'");
    }

    #[test]
    fn test_operator_rhs_lhs_expression() {
        let result = analyze_context("order.status == ", 16);
        assert!(
            matches!(result, AnalysisResult::OperatorRhs { ref lhs_expression, .. } if lhs_expression == "order.status"),
            "Expected LHS 'order.status', got {result:?}"
        );
    }

    // --- Phase: acc.property completions ---

    #[test]
    fn test_analyze_acc_property() {
        let result = analyze_context("orders | reduce({\"total\": 0}, acc.", 34);
        assert!(
            matches!(result, AnalysisResult::AccumulatorProperty { ref partial } if partial.is_empty()),
            "Expected AccumulatorProperty, got {result:?}"
        );
    }

    #[test]
    fn test_analyze_acc_property_with_partial() {
        let result = analyze_context("orders | reduce({\"total\": 0}, acc.to", 36);
        assert!(
            matches!(result, AnalysisResult::AccumulatorProperty { ref partial } if partial == "to"),
            "Expected AccumulatorProperty with partial 'to', got {result:?}"
        );
    }

    #[test]
    fn test_acc_property_completions_with_object_init() {
        let items = completions(
            "orders | reduce({\"total\": 0, \"count\": 0}, acc.",
            47,
            None,
            None,
        );
        assert!(
            items.iter().any(|i| i.label == "total"),
            "Should have 'total', got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "count"),
            "Should have 'count', got: {items:?}"
        );
    }

    #[test]
    fn test_acc_property_completions_with_number_init() {
        let items = completions("orders | reduce(0, acc.", 23, None, None);
        // Numeric init has no properties — should return empty
        assert!(
            items.is_empty(),
            "Should have no completions for numeric acc, got: {items:?}"
        );
    }

    // --- Phase: Transform output type completions ---

    #[test]
    fn test_first_transform_element_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "orders": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "name": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // `orders | first.` should resolve to the array item properties
        let items = completions("orders | first.", 15, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "id"),
            "Should have 'id' after first, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "name"),
            "Should have 'name' after first, got: {items:?}"
        );
    }

    #[test]
    fn test_last_transform_element_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "orders": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "status": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("orders | last.", 14, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "id"),
            "Should have 'id' after last, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "status"),
            "Should have 'status' after last, got: {items:?}"
        );
    }

    #[test]
    fn test_chained_filter_first_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "orders": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "name": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // `orders | sort | first.` — sort preserves elements, first extracts one
        let items = completions("orders | sort | first.", 22, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "id"),
            "Should have 'id' after sort | first, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "name"),
            "Should have 'name' after sort | first, got: {items:?}"
        );
    }

    #[test]
    fn test_split_last_pipe() {
        assert_eq!(
            split_last_pipe("orders | first"),
            Some(("orders ", "first"))
        );
        assert_eq!(
            split_last_pipe("orders | sort | first"),
            Some(("orders | sort ", "first"))
        );
        assert_eq!(split_last_pipe("orders"), None);
        // Should not split on `||` (logical OR)
        assert_eq!(split_last_pipe("a || b"), None);
    }

    #[test]
    fn test_parenthesized_transform_completions() {
        let schema = json!({
            "type": "object",
            "properties": {
                "movies": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "title": { "type": "string" },
                            "genre": { "type": "array", "items": { "type": "string" } }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // `(movies | first).` should offer movie item properties
        let expr = "(movies | first).";
        let items = completions(expr, expr.len(), Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "title"),
            "Should have 'title', got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "genre"),
            "Should have 'genre', got: {items:?}"
        );
    }

    #[test]
    fn test_strip_balanced_parens() {
        assert_eq!(strip_balanced_parens("(movies | first)"), "movies | first");
        assert_eq!(strip_balanced_parens("((x))"), "x");
        assert_eq!(strip_balanced_parens("movies | first"), "movies | first");
        // `(a) + (b)` — outer parens are NOT balanced with each other
        assert_eq!(strip_balanced_parens("(a) + (b)"), "(a) + (b)");
    }

    // --- Lambda variable injection: find, findIndex, apply ---

    #[test]
    fn test_lambda_this_in_find() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": { "type": "array", "items": { "type": "object" } }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("items | find(", 13, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "index"),
            "Should have 'index' in find, got: {items:?}"
        );
    }

    #[test]
    fn test_lambda_this_in_find_index() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": { "type": "array", "items": { "type": "object" } }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("items | findIndex(", 18, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "index"),
            "Should have 'index' in findIndex, got: {items:?}"
        );
    }

    #[test]
    fn test_lambda_apply_no_extra_index() {
        // apply should not inject an extra `index` variable via the lambda injection path
        // (index may still appear as a built-in variable from complete_identifier)
        let items = completions("{a: 1} | apply(", 15, None, None);
        let index_count = items.iter().filter(|i| i.label == "index").count();
        assert!(
            index_count <= 1,
            "Should have at most 1 'index' (from built-ins), got {index_count}: {items:?}"
        );
    }

    // --- Snippet completions ---

    #[test]
    fn test_transform_snippet_with_required_args() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("name | rep", 10, Some(&index), None);
        let replace_item = items.iter().find(|i| i.label == "replace");
        assert!(
            replace_item.is_some(),
            "Should have 'replace', got: {items:?}"
        );
        let replace_item = replace_item.unwrap();
        assert!(replace_item.is_snippet, "replace should be a snippet");
        assert!(
            replace_item
                .insert_text
                .as_deref()
                .unwrap()
                .contains("${1:"),
            "Should have placeholder, got: {:?}",
            replace_item.insert_text
        );
    }

    #[test]
    fn test_transform_snippet_no_args() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let items = completions("name | upper", 12, Some(&index), None);
        let upper = items.iter().find(|i| i.label == "uppercase");
        assert!(upper.is_some(), "Should have 'uppercase', got: {items:?}");
        let upper = upper.unwrap();
        assert!(!upper.is_snippet, "uppercase should NOT be a snippet");
    }

    // --- Type-aware chain completions ---

    #[test]
    fn test_chain_completion_split_then_array_transforms() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // After split, we have an array — should get array transforms
        let items = completions("name | split(',') | ", 20, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "first"),
            "Should have 'first' after split, got: {items:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "last"),
            "Should have 'last' after split, got: {items:?}"
        );
        // Should NOT have string-only transforms
        assert!(
            !items.iter().any(|i| i.label == "uppercase"),
            "Should NOT have 'uppercase' after split, got: {items:?}"
        );
    }

    #[test]
    fn test_chain_completion_string_to_string() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // After uppercase (string → string), should still get string transforms
        let items = completions("name | uppercase | ", 19, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "trim"),
            "Should have 'trim' after uppercase, got: {items:?}"
        );
        assert!(
            !items.iter().any(|i| i.label == "first"),
            "Should NOT have 'first' after uppercase, got: {items:?}"
        );
    }

    #[test]
    fn test_chain_completion_element_of_input() {
        let schema = json!({
            "type": "object",
            "properties": {
                "names": {
                    "type": "array",
                    "items": { "type": "string" }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // After first (element of string array), should get string transforms
        let items = completions("names | first | ", 16, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "uppercase"),
            "Should have 'uppercase' after first on string array, got: {items:?}"
        );
        assert!(
            !items.iter().any(|i| i.label == "join"),
            "Should NOT have 'join' (array-only) after first, got: {items:?}"
        );
    }

    #[test]
    fn test_chain_completion_sort_then_first() {
        let schema = json!({
            "type": "object",
            "properties": {
                "names": {
                    "type": "array",
                    "items": { "type": "string" }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // sort preserves array, first extracts element → string
        let items = completions("names | sort | first | ", 23, Some(&index), None);
        assert!(
            items.iter().any(|i| i.label == "uppercase"),
            "Should have 'uppercase' after sort | first on string array, got: {items:?}"
        );
    }

    #[test]
    fn test_string_literal_lhs_transformer_completion() {
        let schema = json!({
            "type": "object",
            "properties": {
                "birthDate": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);

        // No schema — all matching transforms should be returned
        let expr = r#""2002-01-01" | to"#;
        let offset = expr.len();
        let items = completions(expr, offset, None, None);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            items.iter().any(|i| i.label == "toDate"),
            "No schema: should have 'toDate', got: {labels:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "toDateTime"),
            "No schema: should have 'toDateTime', got: {labels:?}"
        );

        // With schema — should still recognise the string literal and return String transforms
        let items = completions(expr, offset, Some(&index), None);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            items.iter().any(|i| i.label == "toDate"),
            "With schema: should have 'toDate' for string literal LHS, got: {labels:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "toDateTime"),
            "With schema: should have 'toDateTime' for string literal LHS, got: {labels:?}"
        );
        // Array transforms should NOT appear (they don't start with "to" AND are wrong type)
        assert!(
            !items.iter().any(|i| i.label == "first"),
            "With schema: should NOT have 'first' for string literal LHS, got: {labels:?}"
        );

        // With no partial — verify that String-typed transforms appear and Array-only ones don't
        let expr_no_partial = r#""2002-01-01" | "#;
        let items = completions(expr_no_partial, expr_no_partial.len(), Some(&index), None);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            items.iter().any(|i| i.label == "uppercase"),
            "No partial: should have 'uppercase' for string literal LHS, got: {labels:?}"
        );
        assert!(
            items.iter().any(|i| i.label == "toDate"),
            "No partial: should have 'toDate' for string literal LHS, got: {labels:?}"
        );
        assert!(
            !items.iter().any(|i| i.label == "first"),
            "No partial: should NOT have 'first' (array-only) for string literal LHS, got: {labels:?}"
        );
    }
}
