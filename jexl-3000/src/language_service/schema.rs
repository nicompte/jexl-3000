use super::types::{PrimitiveType, SchemaEntry, SchemaIndex, schema_type_to_primitive};
use serde_json::Value;
use std::collections::HashMap;

/// Build a flat index mapping dot-separated paths to their schema metadata.
///
/// Given a JSON Schema like:
/// ```json
/// {
///   "type": "object",
///   "properties": {
///     "customer": {
///       "type": "object",
///       "properties": {
///         "name": { "type": "string" },
///         "orders": {
///           "type": "array",
///           "items": { "type": "object", "properties": { "id": { "type": "number" } } }
///         }
///       }
///     }
///   }
/// }
/// ```
///
/// Produces entries for `""` (root), `"customer"`, `"customer.name"`,
/// `"customer.orders"`, `"customer.orders.__arrayItem__"`,
/// `"customer.orders.__arrayItem__.id"`, etc.
///
/// Automatically resolves `$ref` pointers and merges `allOf`/`oneOf`/`anyOf`.
#[must_use] 
pub fn build_schema_index(schema: &Value) -> SchemaIndex {
    let mut resolved = schema.clone();
    resolve_refs(&mut resolved);
    let mut index = HashMap::new();
    traverse(&resolved, &[], &[], &mut index);
    index
}

/// Recursively resolve all `$ref` pointers in a JSON Schema by inlining the referenced
/// definitions. Supports `#/definitions/Foo` and `#/$defs/Foo` pointer formats.
/// After resolution, `$ref` keys are removed and the referenced schema is merged in.
fn resolve_refs(schema: &mut Value) {
    // Extract top-level definitions for lookup
    let definitions = extract_definitions(schema);
    if !definitions.is_empty() {
        resolve_refs_inner(schema, &definitions, 0);
    }
}

fn extract_definitions(schema: &Value) -> HashMap<String, Value> {
    let mut defs = HashMap::new();
    for key in &["definitions", "$defs"] {
        if let Some(obj) = schema.get(*key).and_then(|v| v.as_object()) {
            for (name, def_schema) in obj {
                defs.insert(name.clone(), def_schema.clone());
            }
        }
    }
    defs
}

const MAX_REF_DEPTH: u32 = 20;

fn resolve_refs_inner(node: &mut Value, definitions: &HashMap<String, Value>, depth: u32) {
    if depth > MAX_REF_DEPTH {
        return;
    }

    match node {
        Value::Object(obj) => {
            // If this node has a `$ref`, replace it with the referenced definition
            if let Some(ref_val) = obj.get("$ref").and_then(|v| v.as_str()).map(String::from)
                && let Some(resolved) = resolve_ref_pointer(&ref_val, definitions) {
                    let mut resolved = resolved.clone();
                    // Merge any sibling properties (e.g., description override) onto the resolved schema
                    if let Value::Object(resolved_obj) = &mut resolved {
                        for (k, v) in obj.iter() {
                            if k != "$ref" {
                                resolved_obj.entry(k.clone()).or_insert_with(|| v.clone());
                            }
                        }
                    }
                    *node = resolved;
                    // Recurse to resolve nested refs in the inlined definition
                    resolve_refs_inner(node, definitions, depth + 1);
                    return;
                }

            // Recurse into all object values
            for value in obj.values_mut() {
                resolve_refs_inner(value, definitions, depth);
            }
        }
        Value::Array(arr) => {
            for item in arr.iter_mut() {
                resolve_refs_inner(item, definitions, depth);
            }
        }
        _ => {}
    }
}

/// Resolve a JSON Pointer `$ref` value (e.g., `#/definitions/Address`) to its definition.
fn resolve_ref_pointer<'a>(pointer: &str, definitions: &'a HashMap<String, Value>) -> Option<&'a Value> {
    let stripped = pointer.strip_prefix('#')?;
    let parts: Vec<&str> = stripped.split('/').filter(|s| !s.is_empty()).collect();
    if parts.len() == 2 && (parts[0] == "definitions" || parts[0] == "$defs") {
        return definitions.get(parts[1]);
    }
    None
}

fn traverse(
    node: &Value,
    path: &[&str],
    parent_required: &[&str],
    index: &mut SchemaIndex,
) {
    // First, merge allOf/oneOf/anyOf sub-schemas into an effective node
    let effective = merge_composition(node);
    let node = &effective;

    let path_key = path.join(".");

    let properties = node
        .get("properties")
        .and_then(|p| p.as_object())
        .map(|obj| obj.keys().cloned().collect::<Vec<_>>())
        .unwrap_or_default();

    let property_schemas: HashMap<String, Value> = node
        .get("properties")
        .and_then(|p| p.as_object())
        .map(|obj| {
            obj.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect()
        })
        .unwrap_or_default();

    let array_item_schema = node.get("items").cloned();

    let required = if path.is_empty() {
        // Root is always considered "required"
        true
    } else {
        let prop_name = path.last().copied().unwrap_or("");
        parent_required.contains(&prop_name)
    };

    let nullable = is_nullable(node);

    let enum_values = node
        .get("enum")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    let entry = SchemaEntry {
        schema_node: node.clone(),
        required,
        nullable,
        properties,
        property_schemas,
        array_item_schema: array_item_schema.clone(),
        enum_values,
    };

    index.insert(path_key, entry);

    // Recurse into object properties
    let child_required: Vec<&str> = node
        .get("required")
        .and_then(|r| r.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str())
                .collect()
        })
        .unwrap_or_default();

    if let Some(props) = node.get("properties").and_then(|p| p.as_object()) {
        for (key, child_schema) in props {
            let mut child_path: Vec<&str> = path.to_vec();
            // We need the key to live long enough — it's from the map so it's fine
            child_path.push(key.as_str());
            traverse(child_schema, &child_path, &child_required, index);
        }
    }

    // Recurse into array items
    if let Some(items) = &array_item_schema {
        let mut items_path: Vec<&str> = path.to_vec();
        items_path.push("__arrayItem__");
        traverse(items, &items_path, &[], index);
    }
}

/// Merge `allOf`, `oneOf`, and `anyOf` sub-schemas into a single effective schema node.
///
/// - `allOf` → all sub-schemas' properties are merged (intersection semantics)
/// - `oneOf` / `anyOf` → all sub-schemas' properties are merged as a union
///
/// The result is a single object with merged `properties`, `required`, `items`, `type`, and `enum`.
/// If there are no composition keywords, returns a clone of the input.
fn merge_composition(node: &Value) -> Value {
    let has_allof = node.get("allOf").and_then(|v| v.as_array()).is_some();
    let has_oneof = node.get("oneOf").and_then(|v| v.as_array()).is_some();
    let has_anyof = node.get("anyOf").and_then(|v| v.as_array()).is_some();

    if !has_allof && !has_oneof && !has_anyof {
        return node.clone();
    }

    let mut merged = node.clone();

    // Collect all sub-schemas from composition keywords
    let sub_schemas: Vec<&Value> = ["allOf", "oneOf", "anyOf"]
        .iter()
        .filter_map(|kw| node.get(*kw))
        .filter_map(|v| v.as_array())
        .flatten()
        .collect();

    for sub in &sub_schemas {
        // Recursively merge sub-schemas that themselves have composition
        let effective_sub = merge_composition(sub);
        merge_into(&mut merged, &effective_sub);
    }

    // Remove composition keywords from the merged result
    if let Value::Object(obj) = &mut merged {
        obj.remove("allOf");
        obj.remove("oneOf");
        obj.remove("anyOf");
    }

    merged
}

/// Merge properties, required, items, type, and enum from `source` into `target`.
fn merge_into(target: &mut Value, source: &Value) {
    let Some(target_obj) = target.as_object_mut() else { return };
    let Some(source_obj) = source.as_object() else { return };

    // Merge `properties`
    if let Some(src_props) = source_obj.get("properties").and_then(|p| p.as_object()) {
        let tgt_props = target_obj
            .entry("properties")
            .or_insert_with(|| Value::Object(serde_json::Map::new()));
        if let Some(tgt_map) = tgt_props.as_object_mut() {
            for (k, v) in src_props {
                tgt_map.entry(k.clone()).or_insert_with(|| v.clone());
            }
        }
    }

    // Merge `required` arrays
    if let Some(src_req) = source_obj.get("required").and_then(|r| r.as_array()) {
        let tgt_req = target_obj
            .entry("required")
            .or_insert_with(|| Value::Array(Vec::new()));
        if let Some(tgt_arr) = tgt_req.as_array_mut() {
            for item in src_req {
                if !tgt_arr.contains(item) {
                    tgt_arr.push(item.clone());
                }
            }
        }
    }

    // Merge `items` (first one wins — no merging of conflicting array items)
    if source_obj.contains_key("items") && !target_obj.contains_key("items") {
        target_obj.insert("items".to_string(), source_obj["items"].clone());
    }

    // Merge `type` — if source has a type and target doesn't, take it
    if source_obj.contains_key("type") && !target_obj.contains_key("type") {
        target_obj.insert("type".to_string(), source_obj["type"].clone());
    }

    // Merge `enum` values as union
    if let Some(src_enum) = source_obj.get("enum").and_then(|e| e.as_array()) {
        let tgt_enum = target_obj
            .entry("enum")
            .or_insert_with(|| Value::Array(Vec::new()));
        if let Some(tgt_arr) = tgt_enum.as_array_mut() {
            for item in src_enum {
                if !tgt_arr.contains(item) {
                    tgt_arr.push(item.clone());
                }
            }
        }
    }
}

/// Check if a schema node is nullable.
fn is_nullable(node: &Value) -> bool {
    // Check explicit `nullable: true`
    if node.get("nullable").and_then(serde_json::Value::as_bool) == Some(true) {
        return true;
    }
    // Check if `type` includes "null"
    if let Some(type_val) = node.get("type") {
        let types = schema_type_to_primitive(type_val);
        if types.contains(&PrimitiveType::Null) {
            return true;
        }
    }
    false
}

/// Look up a dot-separated property path in the schema index.
/// Handles array traversal: `orders.id` is resolved as `orders.__arrayItem__.id`.
/// Also strips `[N]` array indexing from path segments.
#[must_use] 
pub fn resolve_path<'a>(path: &str, index: &'a SchemaIndex) -> Option<&'a SchemaEntry> {
    let normalized = normalize_path(path);
    let path = normalized.as_str();

    // Direct lookup
    if let Some(entry) = index.get(path) {
        return Some(entry);
    }

    // Try inserting __arrayItem__ at each segment boundary for array traversal.
    // E.g. `orders.id` → `orders.__arrayItem__.id`
    let segments: Vec<&str> = path.split('.').collect();
    if segments.len() < 2 {
        return None;
    }

    // Try progressively inserting __arrayItem__ after each segment
    for i in 1..segments.len() {
        let mut attempt: Vec<&str> = Vec::with_capacity(segments.len() + 1);
        attempt.extend_from_slice(&segments[..i]);
        attempt.push("__arrayItem__");
        attempt.extend_from_slice(&segments[i..]);
        let key = attempt.join(".");
        if let Some(entry) = index.get(&key) {
            return Some(entry);
        }
    }

    None
}

/// Resolve each prefix segment of a dot-path, returning the segment name and
/// its resolved type string.  Used for building type breadcrumbs in hover.
///
/// For example, `order.items.price` might produce:
/// `[("order", "object"), ("items", "array"), ("price", "number")]`
#[must_use]
pub fn resolve_path_segments(path: &str, index: &SchemaIndex) -> Vec<(String, String)> {
    let normalized = normalize_path(path);
    let segments: Vec<&str> = normalized.split('.').collect();
    let mut result = Vec::with_capacity(segments.len());

    for i in 0..segments.len() {
        let prefix = segments[..=i].join(".");
        let type_str = if let Some(entry) = resolve_path(&prefix, index) {
            types_for_path(&prefix, index)
                .map(|types| {
                    types
                        .iter()
                        .map(std::string::ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" | ")
                })
                .unwrap_or_else(|| format_type_from_node(&entry.schema_node))
        } else {
            "unknown".to_string()
        };
        result.push((segments[i].to_string(), type_str));
    }

    result
}

/// Extract a simple type string from a JSON Schema node.
fn format_type_from_node(schema_node: &serde_json::Value) -> String {
    schema_node
        .get("type")
        .and_then(|t| t.as_str())
        .unwrap_or("unknown")
        .to_string()
}

/// Normalize a property path by stripping `[...]` indexing expressions.
/// E.g. `items[0].name` → `items.name`, `a[1].b[2].c` → `a.b.c`
#[must_use] 
pub fn normalize_path(path: &str) -> String {
    let mut result = String::with_capacity(path.len());
    let mut in_bracket = false;
    for c in path.chars() {
        match c {
            '[' => in_bracket = true,
            ']' => in_bracket = false,
            _ if !in_bracket => result.push(c),
            _ => {}
        }
    }
    result
}

/// Get the array item properties for a given array path.
/// Returns the property names and their type strings from the items schema.
#[must_use] 
pub fn resolve_array_item_properties(
    array_path: &str,
    index: &SchemaIndex,
) -> Vec<(String, Option<String>)> {
    let item_path = format!("{array_path}.__arrayItem__");
    if let Some(entry) = resolve_path(&item_path, index) {
        return entry
            .properties
            .iter()
            .map(|prop| {
                let detail = entry
                    .property_schemas
                    .get(prop)
                    .and_then(|s| s.get("type"))
                    .and_then(|t| t.as_str())
                    .map(std::string::ToString::to_string);
                (prop.clone(), detail)
            })
            .collect();
    }
    // Fallback: look at the array entry's item_schema directly
    if let Some(entry) = resolve_path(array_path, index)
        && let Some(item_schema) = &entry.array_item_schema
            && let Some(props) = item_schema.get("properties").and_then(|p| p.as_object()) {
                return props
                    .iter()
                    .map(|(k, v)| {
                        let detail = v.get("type").and_then(|t| t.as_str()).map(std::string::ToString::to_string);
                        (k.clone(), detail)
                    })
                    .collect();
            }
    Vec::new()
}

/// Get the types for a dot-path from the schema index.
#[must_use] 
pub fn types_for_path(path: &str, index: &SchemaIndex) -> Option<Vec<PrimitiveType>> {
    resolve_path(path, index).map(|entry| {
        entry
            .schema_node
            .get("type").map_or_else(|| vec![PrimitiveType::Any], schema_type_to_primitive)
    })
}

/// Update a subtree of the schema index at the given dot-separated path without
/// rebuilding the entire index. Removes all existing entries under `prefix` and
/// re-traverses from `new_schema`.
///
/// For example, `update_schema_subtree(index, "customer.address", new_address_schema)`
/// removes `customer.address`, `customer.address.street`, etc. and re-indexes
/// `new_address_schema` at that prefix.
pub fn update_schema_subtree(index: &mut SchemaIndex, prefix: &str, new_schema: &Value) {
    // 1. Resolve $ref / composition in the new sub-schema
    let mut resolved = new_schema.clone();
    let definitions = extract_definitions(&resolved);
    if !definitions.is_empty() {
        resolve_refs_inner(&mut resolved, &definitions, 0);
    }

    // 2. Remove all entries whose key equals or is a child of `prefix`
    let prefix_dot = format!("{prefix}.");
    index.retain(|k, _| k != prefix && !k.starts_with(&prefix_dot));

    // 3. Determine parent required list (collect into owned strings to release borrow)
    let parent_path = prefix.rsplit_once('.').map_or("", |(p, _)| p);
    let parent_required_owned: Vec<String> = index
        .get(parent_path)
        .and_then(|entry| {
            entry
                .schema_node
                .get("required")
                .and_then(|r| r.as_array())
        })
        .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
        .unwrap_or_default();

    // 4. Re-traverse from this prefix
    let parent_required: Vec<&str> = parent_required_owned.iter().map(std::string::String::as_str).collect();
    let segments: Vec<&str> = prefix.split('.').collect();
    traverse(&resolved, &segments, &parent_required, index);

    // 5. Update parent entry's properties and property_schemas
    if let Some(leaf_name) = prefix.rsplit_once('.').map(|(_, leaf)| leaf)
        && let Some(parent_entry) = index.get_mut(parent_path) {
            if !parent_entry.properties.contains(&leaf_name.to_string()) {
                parent_entry.properties.push(leaf_name.to_string());
            }
            parent_entry
                .property_schemas
                .insert(leaf_name.to_string(), resolved);
        }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_basic_schema_index() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string", "description": "The name" },
                "age": { "type": "number" }
            },
            "required": ["name"]
        });

        let index = build_schema_index(&schema);

        // Root entry
        assert!(index.contains_key(""));
        assert_eq!(index[""].properties.len(), 2);
        assert!(index[""].properties.contains(&"name".to_string()));
        assert!(index[""].properties.contains(&"age".to_string()));

        // name entry
        let name = &index["name"];
        assert!(name.required);
        assert!(!name.nullable);

        // age entry
        let age = &index["age"];
        assert!(!age.required);
    }

    #[test]
    fn test_nested_schema_index() {
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": {
                    "type": "object",
                    "properties": {
                        "address": {
                            "type": "object",
                            "properties": {
                                "street": { "type": "string" }
                            }
                        }
                    }
                }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("customer"));
        assert!(index.contains_key("customer.address"));
        assert!(index.contains_key("customer.address.street"));
    }

    #[test]
    fn test_array_items_schema() {
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

        let index = build_schema_index(&schema);
        assert!(index.contains_key("orders"));
        assert!(index.contains_key("orders.__arrayItem__"));
        assert!(index.contains_key("orders.__arrayItem__.id"));

        // resolve_path should find orders.id via __arrayItem__
        let entry = resolve_path("orders.id", &index);
        assert!(entry.is_some());
    }

    #[test]
    fn test_nullable() {
        let schema = json!({
            "type": "object",
            "properties": {
                "a": { "type": "string", "nullable": true },
                "b": { "type": ["string", "null"] }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index["a"].nullable);
        assert!(index["b"].nullable);
    }

    // --- Phase 2: $ref resolution tests ---

    #[test]
    fn test_ref_resolution() {
        let schema = json!({
            "type": "object",
            "definitions": {
                "Address": {
                    "type": "object",
                    "properties": {
                        "street": { "type": "string" },
                        "city": { "type": "string" }
                    }
                }
            },
            "properties": {
                "home": { "$ref": "#/definitions/Address" },
                "name": { "type": "string" }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("home"), "Should have 'home' entry");
        assert!(index.contains_key("home.street"), "Should have 'home.street'");
        assert!(index.contains_key("home.city"), "Should have 'home.city'");
        assert!(index.contains_key("name"), "Should have 'name'");
    }

    #[test]
    fn test_ref_resolution_defs() {
        let schema = json!({
            "type": "object",
            "$defs": {
                "Item": {
                    "type": "object",
                    "properties": {
                        "id": { "type": "number" }
                    }
                }
            },
            "properties": {
                "items": {
                    "type": "array",
                    "items": { "$ref": "#/$defs/Item" }
                }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("items.__arrayItem__.id"), "Should resolve $ref in array items");
    }

    #[test]
    fn test_ref_with_sibling_override() {
        let schema = json!({
            "type": "object",
            "definitions": {
                "Base": {
                    "type": "object",
                    "properties": {
                        "id": { "type": "number" }
                    }
                }
            },
            "properties": {
                "thing": {
                    "$ref": "#/definitions/Base",
                    "description": "Override description"
                }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("thing.id"), "Should resolve $ref and keep sibling props");
    }

    // --- Phase 2: allOf/oneOf/anyOf tests ---

    #[test]
    fn test_allof_merging() {
        let schema = json!({
            "type": "object",
            "properties": {
                "person": {
                    "allOf": [
                        {
                            "type": "object",
                            "properties": {
                                "name": { "type": "string" }
                            },
                            "required": ["name"]
                        },
                        {
                            "properties": {
                                "age": { "type": "number" }
                            }
                        }
                    ]
                }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("person"), "Should have 'person' entry");
        assert!(index.contains_key("person.name"), "allOf should merge 'name' property");
        assert!(index.contains_key("person.age"), "allOf should merge 'age' property");
    }

    #[test]
    fn test_oneof_union() {
        let schema = json!({
            "type": "object",
            "properties": {
                "shape": {
                    "oneOf": [
                        {
                            "type": "object",
                            "properties": {
                                "radius": { "type": "number" }
                            }
                        },
                        {
                            "type": "object",
                            "properties": {
                                "width": { "type": "number" },
                                "height": { "type": "number" }
                            }
                        }
                    ]
                }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("shape.radius"), "oneOf should union 'radius'");
        assert!(index.contains_key("shape.width"), "oneOf should union 'width'");
        assert!(index.contains_key("shape.height"), "oneOf should union 'height'");
    }

    #[test]
    fn test_anyof_with_base_properties() {
        let schema = json!({
            "type": "object",
            "properties": {
                "item": {
                    "type": "object",
                    "properties": {
                        "id": { "type": "number" }
                    },
                    "anyOf": [
                        { "properties": { "color": { "type": "string" } } },
                        { "properties": { "size": { "type": "number" } } }
                    ]
                }
            }
        });

        let index = build_schema_index(&schema);
        assert!(index.contains_key("item.id"), "Should keep base property 'id'");
        assert!(index.contains_key("item.color"), "anyOf should add 'color'");
        assert!(index.contains_key("item.size"), "anyOf should add 'size'");
    }

    // --- Phase 2: Enum values tests ---

    #[test]
    fn test_enum_values_indexed() {
        let schema = json!({
            "type": "object",
            "properties": {
                "status": {
                    "type": "string",
                    "enum": ["active", "inactive", "pending"]
                }
            }
        });

        let index = build_schema_index(&schema);
        let entry = &index["status"];
        assert_eq!(entry.enum_values.len(), 3);
        assert!(entry.enum_values.contains(&json!("active")));
        assert!(entry.enum_values.contains(&json!("inactive")));
        assert!(entry.enum_values.contains(&json!("pending")));
    }

    #[test]
    fn test_enum_values_from_oneof() {
        let schema = json!({
            "type": "object",
            "properties": {
                "color": {
                    "oneOf": [
                        { "enum": ["red", "green"] },
                        { "enum": ["blue"] }
                    ]
                }
            }
        });

        let index = build_schema_index(&schema);
        let entry = &index["color"];
        assert_eq!(entry.enum_values.len(), 3, "Should have union of all enum values, got: {:?}", entry.enum_values);
    }
}
