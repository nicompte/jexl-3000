use super::ast_walk::{LambdaKind, NodeAtOffset, find_enclosing_lambda, find_node_at_offset};
use super::registry::{BUILTIN_FUNCTIONS, BUILTIN_VARIABLES, TRANSFORMER_REGISTRY};
use super::schema::{resolve_array_item_properties, resolve_path, resolve_path_segments};
use super::types::{HoverInfo, SchemaIndex, schema_type_to_primitive, value_to_primitive_type};
use jexl_parser::Parser;
use jexl_parser::ast::Expression;
use serde_json::Value;
use std::fmt::Write as _;

/// Get hover information for the element at a given byte offset in the expression.
///
/// Returns `None` if the offset doesn't correspond to a meaningful token, or if
/// no schema/registry information is available for it.
#[must_use]
pub fn hover(expr: &str, offset: usize, schema_index: Option<&SchemaIndex>) -> Option<HoverInfo> {
    let ast = Parser::parse(expr).ok()?;
    hover_ast(&ast, offset, schema_index)
}

/// Get hover information using a pre-parsed AST.
pub fn hover_ast(
    ast: &Expression,
    offset: usize,
    schema_index: Option<&SchemaIndex>,
) -> Option<HoverInfo> {
    let node = find_node_at_offset(ast, offset)?;

    match &node {
        NodeAtOffset::Identifier { name, location } => {
            // Check built-in variables — with lambda-context-aware enrichment
            if let Some(builtin) = BUILTIN_VARIABLES.get(name) {
                let lambda_note = lambda_context_note(name);

                // Try to enrich `this`, `acc`, `index` with schema-resolved type info
                let enriched = enrich_lambda_variable_hover(name, ast, offset, schema_index);

                if let Some(enriched_content) = enriched {
                    return Some(HoverInfo {
                        content: enriched_content,
                        range: Some(*location),
                        doc_id: None,
                    });
                }

                let mut content = format!(
                    "**`{name}`** — *{}*\n\n{}",
                    builtin.output_type, builtin.description
                );
                if let Some(note) = lambda_note {
                    let _ = write!(content, "\n\n{note}");
                }
                return Some(HoverInfo {
                    content,
                    range: Some(*location),
                    doc_id: None,
                });
            }

            // Check built-in functions
            if let Some(func) = BUILTIN_FUNCTIONS.get(name) {
                return Some(HoverInfo {
                    content: format_function_hover(name, func),
                    range: Some(*location),
                    doc_id: func.doc_id.map(|id| id.to_string()),
                });
            }

            // Check schema
            if let Some(index) = schema_index
                && let Some(entry) = resolve_path(name, index)
            {
                return Some(HoverInfo {
                    content: format_schema_hover(
                        name,
                        &entry.schema_node,
                        entry.required,
                        entry.nullable,
                    ),
                    range: Some(*location),
                    doc_id: None,
                });
            }

            None
        }

        NodeAtOffset::DotProperty {
            ident,
            full_path,
            location,
        } => {
            // Direct schema path resolution (e.g. `customer.name`)
            if let Some(index) = schema_index
                && let Some(entry) = resolve_path(full_path, index)
            {
                let mut content = String::new();
                // Add type breadcrumb for multi-segment paths
                let segments = resolve_path_segments(full_path, index);
                if segments.len() > 1 {
                    let breadcrumb = segments
                        .iter()
                        .map(|(name, typ)| format!("`{name}` (*{typ}*)"))
                        .collect::<Vec<_>>()
                        .join(" → ");
                    let _ = write!(content, "{breadcrumb}\n\n---\n\n");
                }
                content.push_str(&format_schema_hover(
                    ident,
                    &entry.schema_node,
                    entry.required,
                    entry.nullable,
                ));
                return Some(HoverInfo {
                    content,
                    range: Some(*location),
                    doc_id: None,
                });
            }

            // Lambda `this.property` resolution — resolve through enclosing
            // lambda's array item schema (e.g. `movies | map(this.genre)`)
            if full_path.starts_with("this.")
                && let Some(index) = schema_index
            {
                let prop_suffix = &full_path["this.".len()..];
                if let Some(lambda_ctx) = find_enclosing_lambda(ast, offset) {
                    let item_path =
                        format!("{}.__arrayItem__.{}", lambda_ctx.subject_path, prop_suffix);
                    if let Some(entry) = resolve_path(&item_path, index) {
                        return Some(HoverInfo {
                            content: format_schema_hover(
                                ident,
                                &entry.schema_node,
                                entry.required,
                                entry.nullable,
                            ),
                            range: Some(*location),
                            doc_id: None,
                        });
                    }
                }
            }

            // Lambda `acc.property` resolution — resolve acc init keys
            if full_path.starts_with("acc.")
                && let Some(lambda_ctx) = find_enclosing_lambda(ast, offset)
                && lambda_ctx.kind == LambdaKind::Reduce
                && let Some(init_val) = &lambda_ctx.reduce_init
                && let Some(obj) = init_val.as_object()
            {
                let prop_suffix = &full_path["acc.".len()..];
                let root_key = prop_suffix.split('.').next().unwrap_or(prop_suffix);
                if let Some(val) = obj.get(root_key) {
                    let type_str = value_to_primitive_type(val).to_string();
                    return Some(HoverInfo {
                        content: format!(
                            "**`{ident}`**: *{type_str}*\n\n_accumulator property from_ `reduce()` _init value_"
                        ),
                        range: Some(*location),
                        doc_id: None,
                    });
                }
            }

            None
        }

        NodeAtOffset::Transform { name, location } => {
            if let Some(descriptor) = TRANSFORMER_REGISTRY.get(name) {
                return Some(HoverInfo {
                    content: format_transformer_hover(name, descriptor),
                    range: Some(*location),
                    doc_id: descriptor.doc_id.map(|id| id.to_string()),
                });
            }
            None
        }

        NodeAtOffset::FilterProperty {
            name,
            location,
            array_context,
        } => {
            // Try to resolve the filter property from the enclosing array's item schema
            if let (Some(array_path), Some(index)) = (&array_context, schema_index) {
                let props = resolve_array_item_properties(array_path, index);
                if let Some((_, type_str)) = props.iter().find(|(n, _)| n == name) {
                    let type_display = type_str.as_deref().unwrap_or("unknown");
                    return Some(HoverInfo {
                        content: format!(
                            "**`.{name}`**: *{type_display}*\n\n_filter property of_ `{array_path}`"
                        ),
                        range: Some(*location),
                        doc_id: None,
                    });
                }
            }
            Some(HoverInfo {
                content: format!("**`.{name}`** — filter property"),
                range: Some(*location),
                doc_id: None,
            })
        }

        NodeAtOffset::Regex {
            pattern,
            flags,
            location,
        } => {
            let flags_display = if flags.is_empty() {
                String::new()
            } else {
                format!(" (flags: `{flags}`)")
            };
            let content = format!(
                "**Regex** `/{pattern}/{flags}`{flags_display}\n\n\
                 Operators:\n\n\
                 - `~` match → *boolean*\n\
                 - `@` capture groups → *array*\n\
                 - `@+` capture all matches → *array*"
            );
            Some(HoverInfo {
                content,
                range: Some(*location),
                doc_id: Some("regex".to_string()),
            })
        }

        NodeAtOffset::Other { .. } => None,
    }
}

/// Attempt to build an enriched hover for `this`, `acc`, or `index` by resolving
/// the enclosing lambda context and looking up the actual type from the schema.
fn enrich_lambda_variable_hover(
    name: &str,
    ast: &Expression,
    offset: usize,
    schema_index: Option<&SchemaIndex>,
) -> Option<String> {
    let lambda_ctx = find_enclosing_lambda(ast, offset)?;

    match name {
        "this" => {
            let index = schema_index?;
            let item_path = format!("{}.__arrayItem__", lambda_ctx.subject_path);
            let entry = resolve_path(&item_path, index).or_else(|| {
                resolve_path(&lambda_ctx.subject_path, index)
                    .filter(|e| e.array_item_schema.is_some())
            })?;

            let mut parts = Vec::new();

            // Determine element type
            let type_str = if entry.properties.is_empty() {
                entry
                    .schema_node
                    .get("type")
                    .map_or_else(|| "any".to_string(), format_type_display)
            } else {
                "object".to_string()
            };

            parts.push(format!(
                "**`this`** — *{type_str}*\n\nCurrent element of `{}`",
                lambda_ctx.subject_path
            ));

            // Show properties table if the element is an object
            if !entry.properties.is_empty() {
                let req_set: Vec<&str> = entry
                    .schema_node
                    .get("required")
                    .and_then(|r| r.as_array())
                    .map(|arr| arr.iter().filter_map(|v| v.as_str()).collect())
                    .unwrap_or_default();

                parts.push("\n**Properties:**".to_string());
                parts.push("| Name | Type |".to_string());
                parts.push("|------|------|".to_string());
                for prop in &entry.properties {
                    let t = entry
                        .property_schemas
                        .get(prop)
                        .and_then(|s| s.get("type"))
                        .map_or_else(|| "any".into(), format_type_display);
                    let req = if req_set.contains(&prop.as_str()) {
                        " ✓"
                    } else {
                        ""
                    };
                    parts.push(format!("| `{prop}` | {t}{req} |"));
                }
            }

            parts.push(lambda_context_note("this")?);

            Some(parts.join("\n\n"))
        }
        "acc" => {
            if lambda_ctx.kind != LambdaKind::Reduce {
                return None;
            }

            let mut parts = Vec::new();

            if let Some(init_val) = &lambda_ctx.reduce_init {
                let type_str = value_to_primitive_type(init_val).to_string();

                parts.push(format!(
                    "**`acc`** — *{type_str}*\n\nAccumulator in `reduce()` over `{}`",
                    lambda_ctx.subject_path
                ));

                // If the init is an object literal, show its keys
                if let Some(obj) = init_val.as_object()
                    && !obj.is_empty()
                {
                    parts.push("\n**Properties (from init):**".to_string());
                    parts.push("| Name | Type |".to_string());
                    parts.push("|------|------|".to_string());
                    for (key, val) in obj {
                        let t = value_to_primitive_type(val).to_string();
                        parts.push(format!("| `{key}` | {t} |"));
                    }
                }

                parts.push(format!("_Initial value:_ `{init_val}`"));
            } else {
                parts.push(format!(
                    "**`acc`** — *any*\n\nAccumulator in `reduce()` over `{}`",
                    lambda_ctx.subject_path
                ));
            }

            parts.push(lambda_context_note("acc")?);

            Some(parts.join("\n\n"))
        }
        "index" => {
            let mut parts = Vec::new();

            parts.push(format!(
                "**`index`** — *number*\n\nZero-based iteration index over `{}`",
                lambda_ctx.subject_path
            ));

            parts.push(lambda_context_note("index")?);

            Some(parts.join("\n\n"))
        }
        _ => None,
    }
}

/// Return a contextual note for lambda-scoped variables.
fn lambda_context_note(name: &str) -> Option<String> {
    match name {
        "this" => Some(
            "_Available inside_ `map()`, `filter()`, `find()`, `findIndex()`, `some()`, `all()`, `sort()`, `flatMap()`, `reduce()`".to_string(),
        ),
        "index" => Some(
            "_Available inside_ `map()`, `filter()`, `find()`, `findIndex()`, `some()`, `all()`, `sort()`, `flatMap()`, `reduce()` — _the current iteration index_".to_string(),
        ),
        "acc" => Some(
            "_Available inside_ `reduce()` — _the accumulator value_".to_string(),
        ),
        _ => None,
    }
}

/// Format hover content for a schema property.
fn format_schema_hover(name: &str, schema_node: &Value, required: bool, nullable: bool) -> String {
    let mut parts = Vec::new();

    // Header with type
    let type_str = schema_node
        .get("type")
        .map_or_else(|| "unknown".to_string(), format_type_display);

    parts.push(format!("**`{name}`**: *{type_str}*"));

    // Badges
    let mut badges = Vec::new();
    if required {
        badges.push("required");
    } else {
        badges.push("optional");
    }
    if nullable {
        badges.push("nullable");
    }
    if !badges.is_empty() {
        parts.push(format!("_{}_", badges.join(" · ")));
    }

    // Description
    if let Some(desc) = schema_node.get("description").and_then(|d| d.as_str()) {
        parts.push(desc.to_string());
    }

    // Enum values
    if let Some(enum_vals) = schema_node.get("enum").and_then(|e| e.as_array()) {
        let vals: Vec<String> = enum_vals
            .iter()
            .map(|v| {
                v.as_str()
                    .map_or_else(|| format!("`{v}`"), |s| format!("`\"{s}\"`"))
            })
            .collect();
        parts.push(format!("**Allowed values:** {}", vals.join(", ")));
    }

    // Object properties table
    if let Some(props) = schema_node.get("properties").and_then(|p| p.as_object())
        && !props.is_empty()
    {
        let req_set: Vec<&str> = schema_node
            .get("required")
            .and_then(|r| r.as_array())
            .map(|arr| arr.iter().filter_map(|v| v.as_str()).collect())
            .unwrap_or_default();

        parts.push("\n**Properties:**".to_string());
        parts.push("| Name | Type | Required |".to_string());
        parts.push("|------|------|----------|".to_string());
        for (key, val) in props {
            let t = val
                .get("type")
                .map_or_else(|| "any".into(), format_type_display);
            let req = if req_set.contains(&key.as_str()) {
                "✓"
            } else {
                ""
            };
            parts.push(format!("| `{key}` | {t} | {req} |"));
        }
    }

    // Array items
    if let Some(items) = schema_node.get("items") {
        let item_type = items
            .get("type")
            .map_or_else(|| "any".into(), format_type_display);
        parts.push(format!("\n**Array items:** *{item_type}*"));
    }

    parts.join("\n\n")
}

/// Format hover content for a transformer.
fn format_transformer_hover(
    name: &str,
    descriptor: &super::registry::TransformerDescriptor,
) -> String {
    let input = descriptor
        .input_types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ");
    let output = descriptor
        .output_types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ");

    let mut parts = Vec::new();

    // Signature
    if descriptor.args.is_empty() {
        parts.push(format!("**`{name}`** : *({input}) → {output}*"));
    } else {
        let args_str: Vec<String> = descriptor
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
        parts.push(format!(
            "**`{name}({})`** : *({input}) → {output}*",
            args_str.join(", ")
        ));
    }

    // Description
    parts.push(descriptor.description.to_string());

    // Arguments list
    if !descriptor.args.is_empty() {
        parts.push("\n**Arguments:**".to_string());
        for arg in &descriptor.args {
            let type_str = arg
                .types
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(" | ");
            let req = if arg.required { " *(required)*" } else { "" };
            parts.push(format!(
                "- `{}` *{}*{} — {}",
                arg.name, type_str, req, arg.description
            ));
        }
    }

    parts.join("\n\n")
}

/// Format hover content for a built-in function.
fn format_function_hover(_name: &str, func: &super::registry::BuiltinFunctionDescriptor) -> String {
    let mut parts = Vec::new();

    parts.push(format!("**`{}`** → *{}*", func.syntax, func.output_type));
    parts.push(func.description.to_string());

    if !func.args.is_empty() {
        parts.push("\n**Arguments:**".to_string());
        for arg in &func.args {
            let type_str = arg
                .types
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(" | ");
            let req = if arg.required { " *(required)*" } else { "" };
            parts.push(format!(
                "- `{}` *{}*{} — {}",
                arg.name, type_str, req, arg.description
            ));
        }
    }

    parts.join("\n\n")
}

/// Format a JSON Schema type value for display.
fn format_type_display(type_val: &Value) -> String {
    let types = schema_type_to_primitive(type_val);
    types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(" | ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn make_index() -> SchemaIndex {
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": {
                    "type": "object",
                    "description": "The customer record",
                    "properties": {
                        "name": { "type": "string", "description": "Customer name" },
                        "age": { "type": "number" }
                    },
                    "required": ["name"]
                }
            },
            "required": ["customer"]
        });
        super::super::schema::build_schema_index(&schema)
    }

    #[test]
    fn test_hover_identifier() {
        let index = make_index();
        let info = hover("customer", 4, Some(&index));
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("customer"));
        assert!(info.content.contains("object"));
    }

    #[test]
    fn test_hover_property() {
        let index = make_index();
        let info = hover("customer.name", 10, Some(&index));
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("name"));
        assert!(info.content.contains("string"));
    }

    #[test]
    fn test_hover_transform() {
        let info = hover("name | uppercase", 10, None);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("uppercase"));
        assert!(info.content.contains("upper case"));
    }

    #[test]
    fn test_hover_builtin_variable() {
        let info = hover("$now", 2, None);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("$now"));
        assert!(info.content.contains("timestamp"));
    }

    #[test]
    fn test_hover_no_match() {
        let info = hover("1 + 2", 2, None);
        // The `+` operator position — might not have hover
        // This is fine to return None
        assert!(info.is_none() || !info.unwrap().content.is_empty());
    }

    // --- Phase 3: Filter property hover with array context ---

    #[test]
    fn test_hover_filter_property_with_schema() {
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
        let info = hover("items[.name == 'x']", 8, Some(&index));
        assert!(
            info.is_some(),
            "Should have hover for filter property .name"
        );
        let info = info.unwrap();
        assert!(
            info.content.contains("string"),
            "Should show type, got: {}",
            info.content
        );
        assert!(
            info.content.contains("items"),
            "Should reference array path, got: {}",
            info.content
        );
    }

    // --- Phase 3: Lambda-aware hover ---

    #[test]
    fn test_hover_this_lambda_note() {
        let info = hover("this", 2, None);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(
            info.content.contains("map()"),
            "Should mention map(), got: {}",
            info.content
        );
        assert!(info.content.contains("filter()"), "Should mention filter()");
    }

    #[test]
    fn test_hover_index_lambda_note() {
        let info = hover("index", 3, None);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(
            info.content.contains("iteration index"),
            "Should mention iteration, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_acc_lambda_note() {
        let info = hover("acc", 2, None);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(
            info.content.contains("reduce()"),
            "Should mention reduce(), got: {}",
            info.content
        );
    }

    // --- Enhanced lambda variable hover with schema resolution ---

    fn make_array_schema() -> SchemaIndex {
        let schema = json!({
            "type": "object",
            "properties": {
                "orders": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "name": { "type": "string" },
                            "price": { "type": "number" }
                        },
                        "required": ["id", "name"]
                    }
                }
            }
        });
        super::super::schema::build_schema_index(&schema)
    }

    #[test]
    fn test_hover_this_inside_map_shows_element_type() {
        let index = make_array_schema();
        // Expression: orders | map(this.name)
        // Hovering over "this" (at offset 15 inside the map body)
        let expr = "orders | map(this.name)";
        let this_offset = 13; // start of "this"
        let info = hover(expr, this_offset, Some(&index));
        assert!(info.is_some(), "Should have hover for this inside map");
        let info = info.unwrap();
        assert!(
            info.content.contains("object"),
            "Should show element type as object, got: {}",
            info.content
        );
        assert!(
            info.content.contains("orders"),
            "Should reference subject array 'orders', got: {}",
            info.content
        );
        assert!(
            info.content.contains("id"),
            "Should list property 'id', got: {}",
            info.content
        );
        assert!(
            info.content.contains("name"),
            "Should list property 'name', got: {}",
            info.content
        );
        assert!(
            info.content.contains("price"),
            "Should list property 'price', got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_acc_inside_reduce_shows_init_type() {
        let index = make_array_schema();
        // Expression: orders | reduce(0, acc + this.price)
        let expr = "orders | reduce(0, acc + this.price)";
        let acc_offset = 19; // start of "acc"
        let info = hover(expr, acc_offset, Some(&index));
        assert!(info.is_some(), "Should have hover for acc inside reduce");
        let info = info.unwrap();
        assert!(
            info.content.contains("number"),
            "Should show init type as number, got: {}",
            info.content
        );
        assert!(
            info.content.contains("orders"),
            "Should reference subject array, got: {}",
            info.content
        );
        assert!(
            info.content.contains("`0`"),
            "Should show init value, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_acc_inside_reduce_with_object_init() {
        let index = make_array_schema();
        let expr = "orders | reduce({\"total\": 0, \"count\": 0}, acc.total + this.price)";
        // Find offset of "acc" after the comma + space
        let acc_pos = expr.find("acc.total").unwrap();
        let info = hover(expr, acc_pos, Some(&index));
        assert!(
            info.is_some(),
            "Should have hover for acc inside reduce with object init"
        );
        let info = info.unwrap();
        assert!(
            info.content.contains("object"),
            "Should show init type as object, got: {}",
            info.content
        );
        assert!(
            info.content.contains("total"),
            "Should list 'total' property from init, got: {}",
            info.content
        );
        assert!(
            info.content.contains("count"),
            "Should list 'count' property from init, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_index_inside_map_shows_context() {
        let index = make_array_schema();
        // Use index in an expression context (ExpressionTransform, not MapTransform shorthand)
        let expr = "orders | map(this.price + index)";
        let idx_offset = expr.find("index").unwrap(); // start of "index"
        let info = hover(expr, idx_offset, Some(&index));
        assert!(info.is_some(), "Should have hover for index inside map");
        let info = info.unwrap();
        assert!(
            info.content.contains("number"),
            "Should show number type, got: {}",
            info.content
        );
        assert!(
            info.content.contains("orders"),
            "Should reference subject array, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_property_after_transform() {
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
        // Hover over `genre` in `(movies | first).genre`
        let expr = "(movies | first).genre";
        let genre_offset = expr.find("genre").unwrap();
        let info = hover(expr, genre_offset, Some(&index));
        assert!(
            info.is_some(),
            "Should have hover for genre after transform, got None"
        );
        let info = info.unwrap();
        assert!(
            info.content.contains("genre"),
            "Should mention 'genre', got: {}",
            info.content
        );
        assert!(
            info.content.contains("array"),
            "Should show array type, got: {}",
            info.content
        );
    }

    // --- Lambda this.property hover ---

    #[test]
    fn test_hover_this_dot_property_in_map() {
        let schema = json!({
            "type": "object",
            "properties": {
                "movies": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "genre": { "type": "string", "description": "Movie genre" },
                            "title": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        // Hover over `genre` in `movies | map(this.genre)`
        let expr = "movies | map(this.genre)";
        let genre_offset = expr.rfind("genre").unwrap();
        let info = hover(expr, genre_offset, Some(&index));
        assert!(info.is_some(), "Should have hover for this.genre in lambda");
        let info = info.unwrap();
        assert!(
            info.content.contains("string"),
            "Should show string type, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_this_dot_property_in_filter() {
        let schema = json!({
            "type": "object",
            "properties": {
                "users": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "active": { "type": "boolean" },
                            "name": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let expr = "users | filter(this.active)";
        let active_offset = expr.rfind("active").unwrap();
        let info = hover(expr, active_offset, Some(&index));
        assert!(
            info.is_some(),
            "Should have hover for this.active in filter"
        );
        let info = info.unwrap();
        assert!(
            info.content.contains("boolean"),
            "Should show boolean type, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_this_dot_unknown_property() {
        let schema = json!({
            "type": "object",
            "properties": {
                "movies": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "title": { "type": "string" }
                        }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let expr = "movies | map(this.bogus)";
        let bogus_offset = expr.rfind("bogus").unwrap();
        let info = hover(expr, bogus_offset, Some(&index));
        // Unknown property — should return None (no false positives)
        assert!(
            info.is_none(),
            "Should NOT have hover for undefined this.bogus, got: {:?}",
            info
        );
    }

    // --- Breadcrumb tests ---

    #[test]
    fn test_hover_breadcrumb_multi_segment() {
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": {
                    "type": "object",
                    "properties": {
                        "name": { "type": "string" }
                    }
                }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let expr = "customer.name";
        let offset = expr.rfind("name").unwrap();
        let info = hover(expr, offset, Some(&index)).unwrap();
        assert!(
            info.content.contains("→"),
            "Multi-segment hover should include breadcrumb arrow, got: {}",
            info.content
        );
        assert!(
            info.content.contains("customer"),
            "Breadcrumb should include parent"
        );
    }

    #[test]
    fn test_hover_no_breadcrumb_single_segment() {
        let schema = json!({
            "type": "object",
            "properties": {
                "status": { "type": "string" }
            }
        });
        let index = super::super::schema::build_schema_index(&schema);
        let expr = "status";
        let info = hover(expr, 2, Some(&index)).unwrap();
        assert!(
            !info.content.contains("→"),
            "Single-segment hover should NOT have breadcrumb, got: {}",
            info.content
        );
    }

    // --- Lambda transform keyword hover ---

    #[test]
    fn test_hover_map_keyword() {
        // "items | map(this.name)" — hover on "map"
        let expr = "items | map(this.name)";
        let offset = expr.find("map").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for map keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("map"),
            "Should mention map, got: {}",
            info.content
        );
        assert!(
            info.content.contains("Transform each element"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_filter_keyword() {
        let expr = "items | filter(this.active)";
        let offset = expr.find("filter").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for filter keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("filter"),
            "Should mention filter, got: {}",
            info.content
        );
        assert!(
            info.content.contains("predicate"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_reduce_keyword() {
        let expr = "items | reduce(0, acc + this.val)";
        let offset = expr.find("reduce").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for reduce keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("reduce"),
            "Should mention reduce, got: {}",
            info.content
        );
        assert!(
            info.content.contains("accumulator"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_apply_keyword() {
        let expr = "value | apply(this * 2)";
        let offset = expr.find("apply").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for apply keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("apply"),
            "Should mention apply, got: {}",
            info.content
        );
        assert!(
            info.content.contains("lambda"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_find_keyword() {
        let expr = "items | find(this.active)";
        let offset = expr.find("find").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for find keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("find"),
            "Should mention find, got: {}",
            info.content
        );
        assert!(
            info.content.contains("first element"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_any_keyword() {
        let expr = "items | any(this.active)";
        let offset = expr.find("any").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for any keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("any"),
            "Should mention any, got: {}",
            info.content
        );
        assert!(
            info.content.contains("at least one"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_all_keyword() {
        let expr = "items | all(this.active)";
        let offset = expr.find("all").unwrap() + 1;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for all keyword");
        let info = info.unwrap();
        assert!(
            info.content.contains("all"),
            "Should mention all, got: {}",
            info.content
        );
        assert!(
            info.content.contains("all elements"),
            "Should include description, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_regex_literal() {
        let expr = r#""hello" ~ /hel/i"#;
        // offset on the regex literal
        let offset = expr.find("/hel/").unwrap() + 2;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for regex literal");
        let info = info.unwrap();
        assert!(
            info.content.contains("Regex"),
            "Should mention Regex, got: {}",
            info.content
        );
        assert!(
            info.content.contains("hel"),
            "Should contain the pattern, got: {}",
            info.content
        );
        assert!(
            info.content.contains("`~`"),
            "Should describe match operator, got: {}",
            info.content
        );
        assert_eq!(info.doc_id.as_deref(), Some("regex"));
    }

    #[test]
    fn test_hover_regex_with_flags() {
        let expr = r#""test" @ /pattern/gi"#;
        let offset = expr.find("/pattern/").unwrap() + 3;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for regex with flags");
        let info = info.unwrap();
        assert!(
            info.content.contains("gi"),
            "Should display flags, got: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_regex_no_flags() {
        let expr = r#""test" ~ /abc/"#;
        let offset = expr.find("/abc/").unwrap() + 2;
        let info = hover(expr, offset, None);
        assert!(info.is_some(), "Should have hover for regex without flags");
        let info = info.unwrap();
        assert!(
            info.content.contains("`/abc/`"),
            "Should show pattern, got: {}",
            info.content
        );
    }
}
